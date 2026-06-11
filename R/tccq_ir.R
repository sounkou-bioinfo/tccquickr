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

# Conditional select. `na` controls condition handling:
#   "error"     - scalar if(): an NA condition is a runtime error (R's if()).
#   "propagate" - ifelse(): an NA condition yields NA of the result type.
# For scalar if() the node is rank 0; for ifelse() it follows the test's rank.
tccq_ir_cond <- function(cond, yes, no, type = yes$type, na = "error") {
  tccq_node("cond", cond = cond, yes = yes, no = no, na = na, type = type)
}

# Vectorized conditional: ifelse(test, yes, no), NA-propagating.
tccq_ir_ifelse <- function(test, yes, no, type) {
  tccq_ir_cond(test, yes, no, type = type, na = "propagate")
}

# Integer positional switch: switch(index, case1, ..., caseN). `index` is a
# scalar integer (1-based); `cases` is a list of same-typed scalar expressions.
tccq_ir_switch <- function(index, cases, type) {
  tccq_node("switch_select", index = index, cases = cases, type = type)
}

tccq_ir_reduce <- function(op, x, type = tccq_type_scalar("double"), surface = op) {
  tccq_node("reduce", op = op, x = x, surface = surface, type = type)
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
  tccq_node("len", x = x, type = tccq_type_scalar("xlen"))
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

tccq_ir_store_index <- function(name, index, value, target_type, access = NULL) {
  tccq_node(
    "store_index",
    name = as.character(name),
    index = index,
    value = value,
    access = access,
    target_type = target_type,
    type = target_type,
    effect = "write"
  )
}

tccq_ir_store_range <- function(name, start, stop, value, target_type, access = NULL) {
  tccq_node(
    "store_range",
    name = as.character(name),
    start = start,
    stop = stop,
    value = value,
    access = access,
    target_type = target_type,
    type = target_type,
    effect = "write"
  )
}

tccq_ir_store_subscript_index <- function(index) {
  list(kind = "index", index = index)
}

tccq_ir_store_subscript_range <- function(start, stop) {
  list(kind = "range", start = start, stop = stop)
}

tccq_ir_store_subscript_all <- function() {
  list(kind = "all")
}

tccq_ir_subscript_index <- function(index) {
  list(kind = "index", index = index)
}

tccq_ir_subscript_range <- function(start, stop) {
  list(kind = "range", start = start, stop = stop)
}

tccq_ir_subscript_all <- function() {
  list(kind = "all")
}

tccq_ir_store_access <- function(name, subscripts, value, target_type, access = NULL) {
  if (!length(subscripts) && is.null(access)) {
    tccq_abort("assignment target requires at least one subscript")
  }
  tccq_node(
    "store_access",
    name = as.character(name),
    subscripts = subscripts,
    value = value,
    access = access,
    target_type = target_type,
    type = target_type,
    effect = "write"
  )
}

tccq_ir_store_index2 <- function(name, row, col, value, target_type) {
  if (row$type$rank != 0L || col$type$rank != 0L) {
    tccq_abort("matrix row/column indices must be scalar")
  }
  tccq_node(
    "store_index2",
    name = as.character(name),
    row = row,
    col = col,
    value = value,
    target_type = target_type,
    type = target_type,
    effect = "write"
  )
}

tccq_ir_for_loop <- function(var, start, stop, body) {
  tccq_node(
    "for_loop",
    var = as.character(var),
    start = start,
    stop = stop,
    body = body,
    type = tccq_type_scalar("xlen"),
    effect = "write",
    barrier = TRUE
  )
}

# Indexing expression IR -----------------------------------------------------

tccq_ir_index <- function(x, index) {
  if (x$type$rank != 1L) {
    tccq_abort("x[i] currently requires a vector input")
  }
  if (!index$type$rank %in% c(0L, 1L)) {
    tccq_abort("x[i] currently supports scalar or vector integer/logical subscripts only")
  }
  tccq_node(
    "index",
    x = x,
    index = index,
    type = if (index$type$rank == 0L) tccq_type_scalar(x$type$mode) else tccq_type_vector(x$type$mode),
    effect = "pure"
  )
}

tccq_ir_slice_range <- function(x, start, stop) {
  if (x$type$rank != 1L) {
    tccq_abort("x[lo:hi] currently requires a vector input")
  }
  tccq_ir_view1(x, start, stop, type = tccq_type_vector(x$type$mode, length = NA_integer_))
}

tccq_ir_index2 <- function(x, row, col) {
  if (x$type$rank != 2L) {
    tccq_abort("x[i, j] currently requires a matrix input")
  }
  if (row$type$rank != 0L || col$type$rank != 0L) {
    tccq_abort("matrix row/column indices must be scalar")
  }
  tccq_node(
    "index2",
    x = x,
    row = row,
    col = col,
    type = tccq_type_scalar(x$type$mode),
    effect = "pure"
  )
}

tccq_ir_matrix_view <- function(x, subscripts) {
  if (x$type$rank != 2L) {
    tccq_abort("matrix view currently requires a rank-2 input")
  }
  if (length(subscripts) != 2L) {
    tccq_abort("matrix view requires exactly two subscripts")
  }
  kinds <- vapply(subscripts, `[[`, character(1), "kind")
  index_ranks <- vapply(subscripts, function(sub) {
    if (identical(sub$kind, "index")) sub$index$type$rank else 0L
  }, integer(1))

  if (identical(kinds, c("all", "index")) && identical(index_ranks[[2L]], 0L)) {
    out_type <- tccq_type_vector(x$type$mode)
  } else if (identical(kinds, c("index", "all")) && identical(index_ranks[[1L]], 0L)) {
    out_type <- tccq_type_vector(x$type$mode)
  } else {
    tccq_abort("matrix extraction currently supports scalar x[i, j], x[, j], or x[i, ]")
  }

  tccq_node(
    "matrix_view",
    x = x,
    subscripts = subscripts,
    type = out_type,
    effect = "pure"
  )
}

tccq_ir_matrix_fill <- function(value, nrow, ncol) {
  if (value$type$rank != 0L || nrow$type$rank != 0L || ncol$type$rank != 0L) {
    tccq_abort("matrix() currently requires scalar data, nrow, and ncol")
  }
  if (identical(value$type$mode, "xlen")) {
    tccq_abort("matrix() data currently cannot be a length()/xlen scalar; coerce it to integer or double first")
  }
  tccq_node(
    "matrix_fill",
    value = value,
    nrow = nrow,
    ncol = ncol,
    type = tccq_type_matrix(value$type$mode, nrow = NA_integer_, ncol = NA_integer_),
    effect = "pure"
  )
}

tccq_ir_vector_fill <- function(value, length) {
  if (value$type$rank != 0L || length$type$rank != 0L) {
    tccq_abort("vector fill currently requires scalar data and length")
  }
  if (identical(value$type$mode, "xlen")) {
    tccq_abort("vector fill data currently cannot be a length()/xlen scalar; coerce it to integer or double first")
  }
  tccq_node(
    "vector_fill",
    value = value,
    length = length,
    type = tccq_type_vector(value$type$mode),
    effect = "pure"
  )
}

tccq_ir_arg_reduce <- function(op, x, type = tccq_type_scalar("integer")) {
  tccq_node("arg_reduce", op = op, x = x, type = type)
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
    } else if (identical(n$tag, "for_loop")) {
      locals[[n$var]] <<- n$type
    }
  })
  locals
}

tccq_ir_program_mutated_names <- function(node) {
  names <- character()
  tccq_ir_walk(node, function(n) {
    if (n$tag %in% c("store_index", "store_range", "store_index2", "store_access")) {
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
      for (nm in names(x)) {
        if (identical(nm, "normalized_access")) {
          next
        }
        visit(x[[nm]])
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

# Immediate child IR nodes of a node (one level): every field, and every element
# of a list field, that is itself an IR node. Lets generic analyses recurse over
# any node without a hand-written per-tag case (the same trick tccq_ir_walk uses).
tccq_ir_child_nodes <- function(node) {
  out <- list()
  for (nm in names(node)) {
    if (nm %in% c("tag", "type", "effect", "barrier", "normalized_access")) {
      next
    }
    v <- node[[nm]]
    if (is.list(v) && !is.null(v$tag)) {
      out[[length(out) + 1L]] <- v
    } else if (is.list(v)) {
      for (el in v) {
        if (is.list(el) && !is.null(el$tag)) {
          out[[length(out) + 1L]] <- el
        }
      }
    }
  }
  out
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
