# tccq_jit.R - runtime specialization and per-signature caching
# SPDX-License-Identifier: GPL-3.0-or-later

#' Runtime-specialized JIT dispatcher for declared-typed `tccq` functions
#'
#' `tccq_jit()` wraps a declared `tccq` function with a per-call dispatch
#' cache keyed by observed argument signatures. Each distinct signature is compiled
#' at most once (optionally with exact shape guards) and then reused on
#' subsequent calls.
#'
#' This is intentionally a thin layer over `tccq_compile()` and is aimed at
#' low-latency interactive workflows where many calls share the same runtime
#' shape, e.g. fixed-size vectors/matrices.
#'
#' @param fn R function with a leading `declare(type(...))` annotation.
#' @param fallback One of "hard" or "auto". In "hard" mode unsupported calls
#'   are rejected. In "auto" mode unsupported calls may lower to explicit
#'   `r_eval` boundary nodes.
#' @param backend Backend object. Use `tccq_backend_source()`,
#'   `tccq_backend_tinycc()`, or `tccq_backend_shlib()`.
#' @param target Target object. Defaults to C + R C API emission.
#' @param extlibs Optional list of `tccq_external_library()` descriptors.
#' @param debug Print generated C source before each (first-time) signature
#'   compilation.
#' @param exact If `TRUE`, each signature is specialized on exact observed shape
#'   (`length` for rank-1 and `nrow`/`ncol` for rank-2) and corresponding
#'   argument checks are emitted into C entry setup.
#' @return A callable R function with signature-cache semantics. Repeated calls
#'   with the same observed signature reuse the compiled callable.
#' @export
#'
#' @examples
#' f <- function(x) {
#'   declare(type(x = double(NA)))
#'   x + 1
#' }
#' jf <- tccq_jit(f, exact = TRUE)
#' jf(c(1, 2, 3))
#'
#' @seealso [tccq_compile()], [tccq_backend_tinycc()], [tccq_backend_shlib()]
#'
tccq_jit <- function(
  fn,
  fallback = c("hard", "auto"),
  backend = tccq_backend_tinycc(),
  target = tccq_target_c_rapi(),
  extlibs = list(),
  debug = FALSE,
  exact = TRUE
) {
  fallback <- match.arg(fallback)
  base_module <- tccq_frontend(fn)
  base_module <- tccq_module_with(
    base_module,
    fallback = fallback
  )

  cache <- new.env(parent = emptyenv())
  compile_count <- 0L
  specializations <- list()

  dispatcher <- local({
    fmls <- base_module$formal_names

    function() {
      arg_values <- as.list(environment())[fmls]
      key <- tccq_jit_signature_key(base_module, arg_values, exact = exact)
      entry <- cache[[key]]

      if (is.null(entry)) {
        spec <- if (isTRUE(exact)) {
          tccq_jit_make_specialization(base_module, arg_values)
        } else {
          NULL
        }

        module <- tccq_module_with(
          base_module,
          specialization = spec,
          fallback = fallback
        )

        callable <- tccq_compile(
          module,
          mode = "compile",
          fallback = fallback,
          backend = backend,
          target = target,
          extlibs = extlibs,
          debug = debug
        )

        if (!is.function(callable)) {
          tccq_abort("tccq_jit could not obtain a callable for signature key: ", key)
        }

        compile_count <<- compile_count + 1L
        specializations[[key]] <<- spec

        entry <- list(
          result = attr(callable, "tccq") %||% list(),
          callable = callable,
          specialization = spec,
          key = key
        )
        cache[[key]] <- entry
      }

      do.call(entry$callable, arg_values)
    }
  })

  formals(dispatcher) <- formals(fn)
  class(dispatcher) <- c("tccq_jit_function", class(dispatcher))
  attr(dispatcher, "tccq") <- list(
    base = base_module,
    backend = backend$name,
    target = target$name,
    exact = exact,
    cache = cache
  )
  attr(dispatcher, "tccq_jit_count") <- function() compile_count
  attr(dispatcher, "tccq_jit_specializations") <- specializations

  dispatcher
}

tccq_jit_signature_key <- function(module, arg_values, exact = TRUE) {
  fmls <- module$formal_names
  out <- vapply(fmls, function(nm) {
    val <- arg_values[[nm]]
    type <- module$types[[nm]]
    key <- paste0("arg=", nm)

    if (is.null(val)) {
      return(paste0(key, ":missing"))
    }

    kind <- tccq_jit_value_shape_type(val, type)
    key <- paste0(key, ":", kind$type)

    if (exact && !is.null(kind$len)) {
      key <- paste0(key, ":len=", kind$len)
    }
    if (is.numeric(kind$len) && !is.na(kind$len)) {
      key <- paste0(key, ":rank=", type$rank)
    }

    if (isTRUE(exact) && !is.null(kind$dim)) {
      key <- paste0(key, ":dim=", paste(kind$dim, collapse = "x"))
    }

    key
  }, character(1))
  paste(out, collapse = "|")
}

tccq_jit_make_specialization <- function(module, arg_values) {
  out <- list()
  for (nm in module$formal_names) {
    val <- arg_values[[nm]]
    type <- module$types[[nm]]
    if (is.null(val)) {
      next
    }

    shape <- tccq_jit_value_shape_type(val, type)
    sp <- list(rank = as.integer(type$rank))

    if (!is.null(shape$len)) {
      sp$len <- as.integer(shape$len)
    }
    if (!is.null(shape$dim)) {
      sp$nrow <- as.integer(shape$dim[[1L]])
      sp$ncol <- as.integer(shape$dim[[2L]])
    }

    out[[nm]] <- sp
  }
  out
}

tccq_jit_value_shape_type <- function(value, declared_type = NULL) {
  out <- list(type = typeof(value), len = NULL, dim = NULL)

  out$len <- as.integer(length(value))

  d <- dim(value)
  if (is.null(d)) {
    return(out)
  }

  if (length(d) == 2L) {
    out$dim <- as.integer(d)
  }

  out
}
