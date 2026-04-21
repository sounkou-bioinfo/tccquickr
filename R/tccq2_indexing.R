# tccq2_indexing.R - assignment and indexing helpers for the fresh compiler
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_is_assignment_call <- function(expr) {
  is.call(expr) && tccq2_call_head_name(expr) %in% c("<-", "=")
}

tccq2_is_subscript_call <- function(expr) {
  is.call(expr) && identical(tccq2_call_head_name(expr), "[")
}

tccq2_is_colon_call <- function(expr) {
  is.call(expr) && identical(tccq2_call_head_name(expr), ":")
}

tccq2_assignment_lhs <- function(expr) {
  tccq2_assert(tccq2_is_assignment_call(expr), "expected assignment call")
  expr[[2L]]
}

tccq2_assignment_rhs <- function(expr) {
  tccq2_assert(tccq2_is_assignment_call(expr), "expected assignment call")
  expr[[3L]]
}

tccq2_lhs_symbol_name <- function(lhs) {
  if (!is.symbol(lhs)) {
    tccq2_abort("expected assignment LHS symbol, got ", deparse(lhs, nlines = 1L))
  }
  as.character(lhs)
}

tccq2_subscript_parts <- function(expr) {
  if (!tccq2_is_subscript_call(expr)) {
    tccq2_abort("expected subscript call, got ", deparse(expr, nlines = 1L))
  }

  args <- as.list(expr[-1L])
  if (length(args) != 2L) {
    tccq2_abort("fresh compiler supports one-dimensional x[i] only")
  }

  list(base = args[[1L]], index = args[[2L]])
}

tccq2_subscript_base_name <- function(expr) {
  parts <- tccq2_subscript_parts(expr)
  if (!is.symbol(parts$base)) {
    tccq2_abort("indexed assignment LHS must be a direct symbol, e.g. y[i] <- v")
  }
  as.character(parts$base)
}

tccq2_is_scalar_rhs_for_assignment <- function(node) {
  is.list(node) && !is.null(node$type) && identical(as.integer(node$type$rank), 0L)
}

tccq2_is_vector_rhs_for_assignment <- function(node) {
  is.list(node) && !is.null(node$type) && identical(as.integer(node$type$rank), 1L)
}
