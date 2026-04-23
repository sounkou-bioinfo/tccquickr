# tccq_backend.R - fresh backend protocol
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_backend <- function(name, compile, capabilities = list()) {
  structure(
    list(
      name = name,
      compile = compile,
      capabilities = do.call(tccq_backend_capabilities, capabilities)
    ),
    class = "tccq_backend"
  )
}

#' `tccq` backend factories
#'
#' Backend objects control how emitted C is handled by [tccq_compile()].
#'
#' - `tccq_backend_source()` returns generated C source without compiling it.
#' - `tccq_backend_tinycc()` compiles and loads the emitted C in memory through
#'   `Rtinycc`.
#' - `tccq_backend_shlib()` compiles the emitted C as a shared library through
#'   `R CMD SHLIB` and loads it with `dyn.load()`. This lives in the same
#'   general deployment space as [`callme`](https://github.com/coolbutuseless/callme).
#'
#' Backends declare capabilities internally, and `tccq_compile()` validates the
#' selected backend against the current target, compile context, and explicit
#' boundary APIs before compiling.
#'
#' @return A `tccq_backend` object suitable for the `backend =` argument of
#'   [tccq_compile()].
#' @examples
#' tccq_backend_source()$name
#' tccq_backend_tinycc()$name
#' tccq_backend_shlib()$name
#' @name tccq_backends
NULL

#' @rdname tccq_backends
#' @export
tccq_backend_source <- function() {
  tccq_backend(
    name = "source",
    capabilities = list(c = TRUE, compile = FALSE, r_api = TRUE, emits_source = TRUE),
    compile = function(module, target, ctx = list()) {
      src <- target$emit(module, ctx)
      list(
        backend = "source",
        source = src,
        compiled = NULL,
        callable = NULL,
        module = module
      )
    }
  )
}

#' @rdname tccq_backends
#' @export
tccq_backend_tinycc <- function() {
  tccq_backend(
    name = "tinycc",
    capabilities = list(
      c = TRUE,
      compile = TRUE,
      r_api = TRUE,
      in_memory = TRUE,
      cli = FALSE,
      headers = TRUE,
      include_paths = TRUE,
      library_paths = TRUE,
      libraries = TRUE,
      options = TRUE,
      boundary_apis = "r_eval"
    ),
    compile = function(module, target, ctx = list()) {
      tccq_require_namespace("Rtinycc")

      src <- target$emit(module, ctx)
      spec <- target$entry_spec(module, ctx)

      ffi <- Rtinycc::tcc_ffi()
      ffi <- Rtinycc::tcc_source(ffi, src)

      headers <- ctx$headers %||% character()
      if (length(headers)) {
        for (header in headers) {
          ffi <- Rtinycc::tcc_header(ffi, header)
        }
      }

      include_paths <- ctx$include_paths %||% character()
      if (length(include_paths)) {
        for (path in include_paths) {
          ffi <- Rtinycc::tcc_include(ffi, path)
        }
      }

      libraries <- ctx$libraries %||% character()
      if (length(libraries)) {
        for (lib in libraries) {
          ffi <- Rtinycc::tcc_library(ffi, lib)
        }
      }

      library_paths <- ctx$library_paths %||% character()
      if (length(library_paths)) {
        for (path in library_paths) {
          ffi <- Rtinycc::tcc_library_path(ffi, path)
        }
      }

      options <- ctx$options %||% character()
      if (length(options)) {
        ffi <- Rtinycc::tcc_options(ffi, options)
      }

      bindings <- stats::setNames(list(spec), module$entry)
      ffi <- do.call(Rtinycc::tcc_bind, c(list(ffi), bindings))
      compiled <- Rtinycc::tcc_compile(ffi)
      callable <- compiled[[module$entry]]

      list(
        backend = "tinycc",
        source = src,
        compiled = compiled,
        callable = callable,
        module = module
      )
    }
  )
}

#' @rdname tccq_backends
#' @export
tccq_backend_shlib <- function() {
  tccq_backend(
    name = "shlib",
    capabilities = list(
      c = TRUE,
      compile = TRUE,
      r_api = TRUE,
      in_memory = FALSE,
      cli = TRUE,
      shared_library = TRUE,
      system_compiler = TRUE,
      headers = TRUE,
      include_paths = TRUE,
      library_paths = TRUE,
      libraries = TRUE,
      options = TRUE,
      boundary_apis = "r_eval"
    ),
    compile = function(module, target, ctx = list()) {
      tccq_backend_compile_shlib(module, target, ctx)
    }
  )
}

tccq_backend_compile_shlib <- function(module, target, ctx = list()) {
  src <- target$emit(module, ctx)
  src <- tccq_shlib_source_with_headers(src, ctx$headers %||% character())

  build_dir <- tccq_shlib_build_dir(module)
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  stem <- file.path(build_dir, module$entry)
  c_file <- paste0(stem, ".c")
  dll_file <- paste0(stem, .Platform$dynlib.ext)
  makevars_file <- file.path(build_dir, "Makevars")

  writeLines(src, c_file, useBytes = TRUE)
  makevars <- tccq_shlib_makevars(ctx)
  writeLines(makevars, makevars_file, useBytes = TRUE)

  r_bin <- file.path(R.home(component = "bin"), "R")
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(build_dir)

  log <- suppressWarnings(system2(
    command = r_bin,
    args = c("CMD", "SHLIB", basename(c_file)),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(log, "status") %||% 0L

  if (!identical(as.integer(status), 0L) || !file.exists(dll_file)) {
    tccq_abort(
      "R CMD SHLIB failed for entry '", module$entry, "'",
      if (length(log)) paste0("\n", paste(log, collapse = "\n")) else ""
    )
  }

  compiled <- dyn.load(dll_file)
  symbol <- getNativeSymbolInfo(module$entry, PACKAGE = compiled)
  callable <- local({
    .symbol <- symbol
    .dll <- compiled
    function(...) {
      base::.Call(.symbol, ...)
    }
  })

  list(
    backend = "shlib",
    source = src,
    compiled = compiled,
    callable = callable,
    module = module,
    dll_path = dll_file,
    build_dir = build_dir,
    build_log = log
  )
}

tccq_shlib_build_dir <- function(module) {
  tempfile(pattern = paste0("tccq_", tccq_c_ident(module$entry), "_"), tmpdir = tempdir())
}

tccq_shlib_source_with_headers <- function(src, headers = character()) {
  headers <- tccq_unique(tccq_nonempty_flags(headers))
  if (!length(headers)) {
    return(src)
  }
  paste(c(headers, "", src), collapse = "\n")
}

tccq_shlib_makevars <- function(ctx = list()) {
  ctx <- ctx %||% list()
  option_flags <- tccq_shlib_partition_options(ctx$options %||% character())

  include_paths <- tccq_nonempty_flags(ctx$include_paths)
  library_paths <- tccq_nonempty_flags(ctx$library_paths)
  libraries <- tccq_nonempty_flags(ctx$libraries)

  cppflags <- tccq_unique(c(
    option_flags$cppflags,
    if (length(include_paths)) paste0("-I", include_paths) else character()
  ))
  libs <- tccq_unique(c(
    option_flags$libs,
    if (length(library_paths)) paste0("-L", library_paths) else character(),
    if (length(libraries)) vapply(libraries, tccq_shlib_library_flag, character(1)) else character()
  ))
  cflags <- tccq_unique(c("-fPIC", option_flags$cflags))

  Filter(nzchar, c(
    if (length(cppflags)) paste0("PKG_CPPFLAGS=", paste(cppflags, collapse = " ")),
    if (length(cflags)) paste0("PKG_CFLAGS=", paste(cflags, collapse = " ")),
    if (length(libs)) paste0("PKG_LIBS=", paste(libs, collapse = " "))
  ))
}

tccq_shlib_partition_options <- function(options = character()) {
  options <- tccq_nonempty_flags(options)
  if (!length(options)) {
    return(list(cflags = character(), cppflags = character(), libs = character()))
  }

  cpp_idx <- grepl("^-(I|D|U)", options)
  lib_idx <- grepl("^-(L|l|Wl,)", options)

  list(
    cflags = options[!(cpp_idx | lib_idx)],
    cppflags = options[cpp_idx],
    libs = options[lib_idx]
  )
}

tccq_shlib_library_flag <- function(lib) {
  if (!nzchar(lib)) {
    return(lib)
  }
  if (grepl("^-l", lib) || grepl("^-L", lib) || grepl("^/", lib)) {
    return(lib)
  }
  paste0("-l", lib)
}
