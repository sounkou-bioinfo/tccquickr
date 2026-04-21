# tccq_ir.R - expression IR and tiny traversal helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_node <- function(tag, ..., type = NULL, effect = "pure", barrier = FALSE) {
  structure(
    c(list(tag = tag, type = type, effect = effect, barrier = barrier), list(...)),
    class = c(paste0("tccq_", tag), "tccq_node")
  )
}

tccq_ir_const <- function(value, type) {
  tccq_node("const", value = value, type = type)
}

tccq_ir_var <- function(name, type) {
  tccq_node("var", name = name, type = type)
}

tccq_ir_unary <- function(op, x, type = x$type) {
  tccq_node("unary", op = op, x = x, type = type)
}

tccq_ir_binary <- function(op, lhs, rhs, type) {
  tccq_node("binary", op = op, lhs = lhs, rhs = rhs, type = type)
}

tccq_ir_call1 <- function(fun, x, type = x$type) {
  tccq_node("call1", fun = fun, x = x, type = type)
}

tccq_ir_reduce <- function(op, x, type = tccq_type_scalar("double")) {
  tccq_node("reduce", op = op, x = x, type = type)
}

tccq_ir_boundary <- function(kind, reason, input, type, effect = "boundary") {
  tccq_ir_boundary_call(
    api = kind,
    name = reason,
    args = list(input),
    type = type,
    effect = effect,
    barrier = TRUE,
    metadata = list(reason = reason)
  )
}

tccq_ir_len <- function(x) {
  tccq_node("len", x = x, type = tccq_type_scalar("integer"))
}

# Program and statement IR ----------------------------------------------------

tccq_ir_program <- function(stmts, result) {
  tccq_node(
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

tccq_ir_bind <- function(name, value) {
  tccq_node(
    "bind",
    name = as.character(name),
    value = value,
    type = value$type,
    effect = value$effect %||% "pure"
  )
}

tccq_ir_store_index <- function(name, index, value, target_type) {
  tccq_node(
    "store_index",
    name = as.character(name),
    index = index,
    value = value,
    target_type = target_type,
    type = target_type,
    effect = "write"
  )
}

tccq_ir_store_range <- function(name, start, stop, value, target_type) {
  tccq_node(
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

tccq_ir_index <- function(x, index) {
  if (x$type$rank != 1L) {
    tccq_abort("x[i] currently requires a vector input")
  }
  tccq_node(
    "index",
    x = x,
    index = index,
    type = tccq_type_scalar(x$type$mode),
    effect = "pure"
  )
}

tccq_ir_slice_range <- function(x, start, stop) {
  if (x$type$rank != 1L) {
    tccq_abort("x[lo:hi] currently requires a vector input")
  }
  tccq_ir_view1(x, start, stop, type = tccq_type_vector(x$type$mode, length = NA_integer_))
}

# Program kernel wrapper ------------------------------------------------------

tccq_ir_kernel_program <- function(stmts, result_kernel) {
  tccq_node(
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

tccq_ir_program_locals <- function(node) {
  locals <- list()
  tccq_ir_walk(node, function(n) {
    if (identical(n$tag, "bind")) {
      locals[[n$name]] <<- n$type
    }
  })
  locals
}

tccq_ir_program_mutated_names <- function(node) {
  names <- character()
  tccq_ir_walk(node, function(n) {
    if (n$tag %in% c("store_index", "store_range")) {
      names <<- c(names, n$name)
    }
  })
  tccq_unique(names)
}

# Recursive walker ------------------------------------------------------------

tccq_ir_walk <- function(node, f) {
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

tccq_ir_has_tag <- function(node, tag) {
  found <- FALSE
  tccq_ir_walk(node, function(n) {
    if (identical(n$tag, tag)) {
      found <<- TRUE
    }
  })
  found
}

tccq_ir_vars <- function(node) {
  vars <- character()
  tccq_ir_walk(node, function(n) {
    if (identical(n$tag, "var")) {
      vars <<- c(vars, n$name)
    }
  })
  tccq_unique(vars)
}

tccq_ir_vector_vars <- function(node) {
  vars <- character()
  tccq_ir_walk(node, function(n) {
    if (identical(n$tag, "var") && n$type$rank > 0L) {
      vars <<- c(vars, n$name)
    }
  })
  tccq_unique(vars)
}

tccq_ir_validate <- function(node) {
  tccq_assert(is.list(node), "IR node must be a list")
  tccq_assert(!is.null(node$tag), "IR node missing tag")
  tccq_assert(!is.null(node$type), "IR node '", node$tag, "' missing type")

  tccq_ir_walk(node, function(n) {
    tccq_assert(!is.null(n$tag), "IR child missing tag")
    tccq_assert(!is.null(n$type), "IR node '", n$tag, "' missing type")
    tccq_assert(!is.null(n$barrier), "IR node '", n$tag, "' missing barrier flag")
  })

  invisible(TRUE)
}
