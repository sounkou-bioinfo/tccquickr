# tccq_views.R - explicit view IR helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_ir_view1 <- function(base, start, stop, type = tccq_type_vector(base$type$mode, length = NA_integer_)) {
  if (base$type$rank != 1L) {
    tccq_abort("view1 currently requires a vector base")
  }
  tccq_node(
    "view1",
    x = base,
    base = base,
    start = start,
    stop = stop,
    type = type,
    effect = "pure",
    barrier = FALSE
  )
}

tccq_is_view_node <- function(node) {
  is.list(node) && !is.null(node$tag) && node$tag %in% c("view1", "slice_range")
}
