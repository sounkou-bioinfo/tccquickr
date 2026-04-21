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

# Program and statement IR ----------------------------------------------------

tccq2_ir_program <- function(stmts, result) {
  tccq2_node(
    "program",
    stmts = stmts,
    result = result,
    type = result$type,
    effect = if (any(vapply(stmts, function(s) !identical(s$effect, "pure"), logical(1)))) {
      "write"
    } else {
      result$effect %||% "pure"
    }
  )
}

tccq2_ir_bind <- function(name, value) {
  tccq2_node(
    "bind",
    name = as.character(name),
    value = value,
    type = value$type,
    effect = value$effect %||% "pure"
  )
}

tccq2_ir_store_index <- function(name, index, value, target_type) {
  tccq2_node(
    "store_index",
    name = as.character(name),
    index = index,
    value = value,
    target_type = target_type,
    type = target_type,
    effect = "write"
  )
}

tccq2_ir_store_range <- function(name, start, stop, value, target_type) {
  tccq2_node(
    "store_range",
    name = as.character(name),
    start = start,
    stop = stop,
    value = value,
    target_type = target_type,
    type = target_type,
    effect = "write"
  )
}

# Indexing expression IR -----------------------------------------------------

tccq2_ir_index <- function(x, index) {
  if (x$type$rank != 1L) {
    tccq2_abort("x[i] currently requires a vector input")
  }
  tccq2_node(
    "index",
    x = x,
    index = index,
    type = tccq2_type_scalar(x$type$mode),
    effect = "pure"
  )
}

tccq2_ir_slice_range <- function(x, start, stop) {
  if (x$type$rank != 1L) {
    tccq2_abort("x[lo:hi] currently requires a vector input")
  }
  tccq2_node(
    "slice_range",
    x = x,
    start = start,
    stop = stop,
    type = tccq2_type_vector(x$type$mode, length = NA_integer_),
    effect = "pure"
  )
}

# Program kernel wrapper ------------------------------------------------------

tccq2_ir_kernel_program <- function(stmts, result_kernel) {
  tccq2_node(
    "kernel_program",
    stmts = stmts,
    result_kernel = result_kernel,
    type = result_kernel$type,
    effect = if (any(vapply(stmts, function(s) !identical(s$effect, "pure"), logical(1)))) {
      "write"
    } else {
      result_kernel$effect %||% "pure"
    }
  )
}

# Program helpers -------------------------------------------------------------

tccq2_ir_program_locals <- function(node) {
  locals <- list()
  tccq2_ir_walk(node, function(n) {
    if (identical(n$tag, "bind")) {
      locals[[n$name]] <<- n$type
    }
  })
  locals
}

tccq2_ir_program_mutated_names <- function(node) {
  names <- character()
  tccq2_ir_walk(node, function(n) {
    if (n$tag %in% c("store_index", "store_range")) {
      names <<- c(names, n$name)
    }
  })
  tccq2_unique(names)
}

# Recursive walker ------------------------------------------------------------

tccq2_ir_walk <- function(node, f) {
  visit <- function(x) {
    if (is.list(x) && !is.null(x$tag)) {
      f(x)
      for (child in x) {
        visit(child)
      }
    } else if (is.list(x)) {
      for (child in x) {
        visit(child)
      }
    }
    invisible(NULL)
  }

  visit(node)
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

  tccq2_ir_walk(node, function(n) {
    tccq2_assert(!is.null(n$tag), "IR child missing tag")
    tccq2_assert(!is.null(n$type), "IR node '", n$tag, "' missing type")
  })

  invisible(TRUE)
}
