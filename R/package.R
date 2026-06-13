#' tccquickr: typed experimental R transformation core
#'
#' `tccquickr` is being rebuilt around declared R programs, shape-aware types,
#' classed diagnostics, contract-checked compiler passes, and generic backend
#' planning. Rtinycc is modeled as one backend descriptor, not as the compiler
#' architecture.
#'
#' @keywords internal
"_PACKAGE"

.onLoad <- function(...) {
  tccq_register_traits()
  tccq_register_backend_traits()
}
