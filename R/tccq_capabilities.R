# tccq_capabilities.R - backend/target capability checks
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_target_capabilities <- function(...) {
  utils::modifyList(
    list(
      c = FALSE,
      r_api = FALSE,
      boundary_apis = character(),
      source_artifact = NULL
    ),
    list(...)
  )
}

tccq_backend_capabilities <- function(...) {
  utils::modifyList(
    list(
      c = FALSE,
      compile = FALSE,
      r_api = FALSE,
      emits_source = FALSE,
      in_memory = FALSE,
      cli = FALSE,
      shared_library = FALSE,
      system_compiler = FALSE,
      headers = FALSE,
      include_paths = FALSE,
      library_paths = FALSE,
      libraries = FALSE,
      options = FALSE,
      boundary_apis = character()
    ),
    list(...)
  )
}

tccq_target_capability <- function(target, name, default = NULL) {
  target$capabilities[[name]] %||% default
}

tccq_backend_capability <- function(backend, name, default = NULL) {
  backend$capabilities[[name]] %||% default
}

tccq_nonempty_flags <- function(x) {
  x <- x %||% character()
  x[nzchar(x)]
}

tccq_compile_requirements <- function(module, target, ctx = list()) {
  boundaries <- if (!is.null(module$kernel)) tccq_boundary_nodes(module$kernel) else list()

  list(
    c = isTRUE(tccq_target_capability(target, "c", FALSE)),
    r_api = isTRUE(tccq_target_capability(target, "r_api", FALSE)),
    headers = length(tccq_nonempty_flags(ctx$headers)) > 0L,
    include_paths = length(tccq_nonempty_flags(ctx$include_paths)) > 0L,
    library_paths = length(tccq_nonempty_flags(ctx$library_paths)) > 0L,
    libraries = length(tccq_nonempty_flags(ctx$libraries)) > 0L,
    options = length(tccq_nonempty_flags(ctx$options)) > 0L,
    boundary_apis = tccq_unique(vapply(boundaries, `[[`, character(1), "api"))
  )
}

tccq_validate_backend_target_ctx <- function(backend, target, module, ctx = list()) {
  tccq_assert(inherits(backend, "tccq_backend"), "expected tccq_backend")
  tccq_assert(inherits(target, "tccq_target"), "expected tccq_target")

  req <- tccq_compile_requirements(module, target, ctx)

  if (isTRUE(req$c) && !isTRUE(tccq_backend_capability(backend, "c", FALSE))) {
    tccq_abort("backend '", backend$name, "' does not support C targets")
  }
  if (isTRUE(req$r_api) && !isTRUE(tccq_backend_capability(backend, "r_api", FALSE))) {
    tccq_abort("backend '", backend$name, "' does not support the R C API target")
  }

  if (!isTRUE(tccq_backend_capability(backend, "compile", FALSE))) {
    return(invisible(TRUE))
  }

  for (nm in c("headers", "include_paths", "library_paths", "libraries", "options")) {
    if (isTRUE(req[[nm]]) && !isTRUE(tccq_backend_capability(backend, nm, FALSE))) {
      tccq_abort("backend '", backend$name, "' does not support compile context field '", nm, "'")
    }
  }

  supported_apis <- tccq_backend_capability(backend, "boundary_apis", character())
  missing_apis <- setdiff(req$boundary_apis, supported_apis)
  if (length(missing_apis)) {
    tccq_abort(
      "backend '", backend$name, "' does not support boundary APIs: ",
      paste(missing_apis, collapse = ", ")
    )
  }

  invisible(TRUE)
}
