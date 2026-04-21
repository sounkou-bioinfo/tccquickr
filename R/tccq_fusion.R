# tccq_fusion.R - explicit fusion rewrites and legality barrier helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_kernel_has_boundary <- function(node) {
  found <- FALSE
  tccq_ir_walk(node, function(n) {
    if (identical(n$tag, "boundary") || identical(n$effect %||% "pure", "boundary")) {
      found <<- TRUE
    }
  })
  found
}

tccq_fuse_kernel <- function(kernel) {
  if (!is.list(kernel) || is.null(kernel$tag)) {
    tccq_abort("expected kernel node for fusion")
  }

  if (identical(kernel$tag, "fold") && identical(kernel$elem$tag, "materialize")) {
    producer <- kernel$elem$producer
    if (!tccq_kernel_has_boundary(producer)) {
      kernel$elem <- producer
      if (!identical(kernel$domain$vars, producer$domain$vars)) {
        kernel$domain <- producer$domain
      }
    }
  }

  kernel
}
