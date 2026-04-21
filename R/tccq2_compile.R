# tccq2_compile.R - public fresh compiler entry point
# SPDX-License-Identifier: GPL-3.0-or-later

#' Fresh experimental R-to-C compiler scaffold
#'
#' This is a fresh-start compiler path. It intentionally does not call the old
#' tcc_quick lowerer or code generator. Milestone 1 supports declared scalar and
#' vector arithmetic plus sum() reductions.
#'
#' @param fn R function with a leading declare(type(...)) annotation.
#' @param mode One of "compile", "code", or "ir".
#' @param backend Backend object. Defaults to TinyCC via Rtinycc.
#' @param target Target object. Defaults to C + R C API emission.
#' @param extlibs Optional list of tccq2_external_library descriptors.
#' @param debug Print generated C source before compiling.
#' @return A compiled callable, C source string, or module IR depending on mode.
#' @export
tccq2_compile <- function(
  fn,
  mode = c("compile", "code", "ir"),
  backend = tccq2_backend_tinycc(),
  target = tccq2_target_c_rapi(),
  extlibs = list(),
  debug = FALSE
) {
  mode <- match.arg(mode)

  module <- tccq2_frontend(fn)
  module <- tccq2_lower_module(module)
  module <- tccq2_run_passes(module)

  if (identical(mode, "ir")) {
    return(module)
  }

  ctx <- tccq2_context_from_extlibs(extlibs)
  src <- target$emit(module, ctx)

  if (identical(mode, "code")) {
    return(src)
  }

  if (isTRUE(debug)) {
    message("tccq2 generated C source:\n", src)
  }

  result <- backend$compile(module, target, ctx)
  callable <- result$callable

  if (is.null(callable)) {
    return(result)
  }

  attr(callable, "tccq2") <- result
  callable
}
