#' tccquickr: typed experimental R transformation core
#'
#' `tccquickr` is being rebuilt around declared R programs, shape-aware types,
#' classed diagnostics, and contract-checked compiler passes. The backend is
#' deliberately absent in this reset.
#'
#' @keywords internal
"_PACKAGE"

.onLoad <- function(...) {
  S7::methods_register()
}
