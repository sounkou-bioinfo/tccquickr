# tccq_fusion.R - explicit fusion rewrites and legality barrier helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_kernel_has_boundary <- function(node) {
  found <- FALSE
  tccq_ir_walk(node, function(n) {
    if (isTRUE(n$barrier) || identical(n$tag, "boundary_call") || identical(n$effect %||% "pure", "boundary")) {
      found <<- TRUE
    }
  })
  found
}

tccq_kernel_domains_equivalent <- function(a, b, shape_facts = NULL) {
  a_id <- a$shape_domain %||% NULL
  b_id <- b$shape_domain %||% NULL

  if (!is.null(a_id) && !is.null(b_id)) {
    return(tccq_shape_domains_equivalent(a_id, b_id, shape_facts = shape_facts))
  }

  identical(a$vars %||% character(), b$vars %||% character())
}

tccq_fuse_kernel <- function(kernel, module = NULL) {
  if (!is.list(kernel) || is.null(kernel$tag)) {
    tccq_abort("expected kernel node for fusion")
  }

  if (identical(kernel$tag, "fold") && identical(kernel$elem$tag, "materialize")) {
    producer <- kernel$elem$producer
    if (!tccq_kernel_has_boundary(producer)) {
      kernel$elem <- producer
      if (!tccq_kernel_domains_equivalent(kernel$domain, producer$domain, shape_facts = module$shape_facts %||% NULL) ||
          !identical(kernel$domain$shape_domain %||% NULL, producer$domain$shape_domain %||% NULL) ||
          !identical(kernel$domain$vars %||% character(), producer$domain$vars %||% character())) {
        kernel$domain <- producer$domain
      }
    }
  }

  kernel
}
