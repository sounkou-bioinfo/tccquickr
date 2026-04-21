# tccq2_ir.R - expression IR and tiny traversal helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_node <- function(tag, ..., type = NULL, effect = "pure") {
  structure(
    c(list(tag = tag, type = type, effect = effect), list(...)),
    class = c(paste0("tccq2_", tag), "tccq2_node")
  )
}

tccq2_ir_const <- function(value, type) {
  tccq2_node("const", value = value, type = type)
}

tccq2_ir_var <- function(name, type) {
  tccq2_node("var", name = name, type = type)
}

tccq2_ir_unary <- function(op, x, type = x$type) {
  tccq2_node("unary", op = op, x = x, type = type)
}

tccq2_ir_binary <- function(op, lhs, rhs, type) {
  tccq2_node("binary", op = op, lhs = lhs, rhs = rhs, type = type)
}

tccq2_ir_call1 <- function(fun, x, type = x$type) {
  tccq2_node("call1", fun = fun, x = x, type = type)
}

tccq2_ir_reduce <- function(op, x, type = tccq2_type_scalar("double")) {
  tccq2_node("reduce", op = op, x = x, type = type)
}

tccq2_ir_boundary <- function(kind, reason, input, type, effect = "boundary") {
  tccq2_node("boundary", kind = kind, reason = reason, input = input, type = type, effect = effect)
}

tccq2_ir_len <- function(x) {
  tccq2_node("len", x = x, type = tccq2_type_scalar("integer"))
}

tccq2_ir_kernel_scalar <- function(expr) {
  tccq2_node("kernel_scalar", expr = expr, type = expr$type)
}

tccq2_ir_kernel_materialize <- function(expr) {
  tccq2_node("kernel_materialize", expr = expr, type = expr$type)
}

tccq2_ir_kernel_fold <- function(op, expr, type = tccq2_type_scalar("double")) {
  tccq2_node("kernel_fold", op = op, expr = expr, type = type)
}

tccq2_ir_walk <- function(node, f) {
  if (!is.list(node) || is.null(node$tag)) {
    return(invisible(NULL))
  }
  f(node)
  for (child in node) {
    if (is.list(child) && !is.null(child$tag)) {
      tccq2_ir_walk(child, f)
    }
  }
  invisible(NULL)
}

tccq2_ir_has_tag <- function(node, tag) {
  found <- FALSE
  tccq2_ir_walk(node, function(n) {
    if (identical(n$tag, tag)) {
      found <<- TRUE
    }
  })
  found
}

tccq2_ir_vars <- function(node) {
  vars <- character()
  tccq2_ir_walk(node, function(n) {
    if (identical(n$tag, "var")) {
      vars <<- c(vars, n$name)
    }
  })
  tccq2_unique(vars)
}

tccq2_ir_vector_vars <- function(node) {
  vars <- character()
  tccq2_ir_walk(node, function(n) {
    if (identical(n$tag, "var") && n$type$rank > 0L) {
      vars <<- c(vars, n$name)
    }
  })
  tccq2_unique(vars)
}

tccq2_ir_validate <- function(node) {
  tccq2_assert(is.list(node), "IR node must be a list")
  tccq2_assert(!is.null(node$tag), "IR node missing tag")
  tccq2_assert(!is.null(node$type), "IR node '", node$tag, "' missing type")
  invisible(TRUE)
}
