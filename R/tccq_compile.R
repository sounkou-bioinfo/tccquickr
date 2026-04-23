# tccq_compile.R - public fresh compiler entry point
# SPDX-License-Identifier: GPL-3.0-or-later

#' Fresh experimental R-to-C compiler scaffold
#'
#' This is the package's current compiler path. The current milestone supports declared
#' scalar and vector arithmetic, fused `sum()` reductions, local bindings,
#' indexed reads, contiguous slices, and local indexed/range writes.
#'
#' Current statement semantics are intentionally strict: rebinding a local name
#' is rejected, direct mutation of formal arguments is rejected, and indexed or
#' range assignment currently requires a scalar right-hand side.
#'
#' @param fn R function with a leading declare(type(...)) annotation.
#' @param mode One of "compile", "code", or "ir".
#' @param fallback One of "auto" or "hard". In "auto" mode unsupported
#'   calls may lower to explicit `r_eval` boundary nodes. In "hard" mode they
#'   are rejected.
#' @param backend Backend object. Use `tccq_backend_source()`,
#'   `tccq_backend_tinycc()`, or `tccq_backend_shlib()`. Defaults to TinyCC via
#'   `Rtinycc`.
#' @param target Target object. Defaults to C + R C API emission.
#' @param extlibs Optional list of tccq_external_library descriptors.
#' @param debug Print generated C source before compiling.
#' @return A compiled callable, C source string, or module IR depending on mode.
#' @export
tccq_compile <- function(
  fn,
  mode = c("compile", "code", "ir"),
  fallback = c("auto", "hard"),
  backend = tccq_backend_tinycc(),
  target = tccq_target_c_rapi(),
  extlibs = list(),
  debug = FALSE
) {
  mode <- match.arg(mode)
  fallback <- match.arg(fallback)

  module <- tccq_frontend(fn)
  module <- tccq_module_with(module, fallback = fallback)
  module <- tccq_lower_module(module)
  module <- tccq_run_passes(module)

  if (identical(mode, "ir")) {
    return(module)
  }

  ctx <- tccq_merge_contexts(tccq_context_from_extlibs(extlibs), module$boundary_context)

  if (identical(mode, "compile")) {
    tccq_validate_backend_target_ctx(backend, target, module, ctx)
  }

  src <- target$emit(module, ctx)

  if (identical(mode, "code")) {
    return(src)
  }

  if (isTRUE(debug)) {
    message("tccq generated C source:\n", src)
  }

  result <- backend$compile(module, target, ctx)
  callable <- result$callable

  if (is.null(callable)) {
    return(result)
  }

  attr(callable, "tccq") <- result
  callable
}
