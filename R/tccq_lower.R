# tccq_lower.R - lower R expressions to tccq IR
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_lower_module <- function(module) {
  tccq_module_validate(module)
  body_exprs <- module$body_exprs %||% list(module$expr)
  ir <- tccq_lower_program(body_exprs, module$types, fallback = module$fallback %||% "hard")
  tccq_module_with(module, ir = ir)
}

tccq_lower_program <- function(exprs, env, fallback = "hard") {
  if (!length(exprs)) {
    tccq_abort("cannot lower empty program")
  }

  stmts <- list()
  current_env <- env
  local_names <- character()

  if (length(exprs) > 1L) {
    for (i in seq_len(length(exprs) - 1L)) {
      lowered <- tccq_lower_stmt(exprs[[i]], current_env, local_names, fallback = fallback)
      stmts[[length(stmts) + 1L]] <- lowered$stmt
      current_env <- lowered$env
      local_names <- lowered$local_names
    }
  }

  result <- tccq_lower_expr(exprs[[length(exprs)]], current_env, fallback = fallback)
  tccq_ir_program(stmts, result)
}

tccq_exprs_from_body <- function(expr) {
  exprs <- if (tccq_is_call_to(expr, "{")) {
    as.list(expr[-1L])
  } else {
    list(expr)
  }
  exprs[!vapply(exprs, is.null, logical(1))]
}

tccq_is_missing_subscript_arg <- function(expr) {
  is.symbol(expr) && !nzchar(as.character(expr))
}

tccq_lower_store_subscript <- function(expr, env, fallback = "hard") {
  if (tccq_is_missing_subscript_arg(expr)) {
    return(tccq_ir_store_subscript_all())
  }

  if (is.call(expr) && identical(tccq_call_head_name(expr), ":")) {
    args <- as.list(expr[-1L])
    if (length(args) != 2L) {
      tccq_abort("range subscript expects lo:hi")
    }
    start <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    stop <- tccq_lower_expr(args[[2L]], env, fallback = fallback)
    if (start$type$rank != 0L || stop$type$rank != 0L) {
      tccq_abort("range subscript bounds must be scalar")
    }
    return(tccq_ir_store_subscript_range(start, stop))
  }

  index <- tccq_lower_expr(expr, env, fallback = fallback)
  if (index$type$rank != 0L) {
    tccq_abort("assignment indices must be scalar or lo:hi ranges")
  }
  tccq_ir_store_subscript_index(index)
}

tccq_lower_store_subscript_from_access_step <- function(step) {
  if (identical(step$kind, "slice")) {
    return(tccq_ir_store_subscript_range(step$start, step$stop))
  }
  if (identical(step$kind, "index")) {
    return(tccq_ir_store_subscript_index(step$index))
  }
  tccq_abort("unsupported indexed assignment access kind: ", step$kind)
}

tccq_lower_access_subscript <- function(expr, env, fallback = "hard") {
  if (tccq_is_missing_subscript_arg(expr)) {
    return(tccq_ir_subscript_all())
  }

  if (is.call(expr) && identical(tccq_call_head_name(expr), ":")) {
    args <- as.list(expr[-1L])
    if (length(args) != 2L) {
      tccq_abort("range subscript expects lo:hi")
    }
    start <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    stop <- tccq_lower_expr(args[[2L]], env, fallback = fallback)
    if (start$type$rank != 0L || stop$type$rank != 0L) {
      tccq_abort("range subscript bounds must be scalar")
    }
    return(tccq_ir_subscript_range(start, stop))
  }

  index <- tccq_lower_expr(expr, env, fallback = fallback)
  if (!index$type$rank %in% c(0L, 1L)) {
    tccq_abort("subscript expressions must be scalar or vector")
  }
  tccq_ir_subscript_index(index)
}

tccq_lower_local_write_error <- function(name, target_type) {
  label <- if (target_type$rank == 1L) "vector" else if (target_type$rank == 2L) "matrix" else "array"
  tccq_abort(
    "indexed assignment currently requires a local ", label, " binding. ",
    "Write y <- ", name, "; y[...] <- value; y instead of mutating formal '", name, "' directly."
  )
}

tccq_lower_loop_sequence <- function(expr, env, fallback = "hard") {
  lower_bound <- function(x) {
    out <- tccq_lower_expr(x, env, fallback = fallback)
    if (out$type$rank != 0L) {
      tccq_abort("for-loop bounds must be scalar")
    }
    out
  }

  if (is.call(expr) && identical(tccq_call_head_name(expr), ":")) {
    args <- as.list(expr[-1L])
    if (length(args) != 2L) {
      tccq_abort("for-loop ':' sequence expects start:stop")
    }
    return(list(start = lower_bound(args[[1L]]), stop = lower_bound(args[[2L]])))
  }

  if (is.call(expr) && tccq_call_head_name(expr) %in% c("seq", "seq.int")) {
    args <- as.list(expr[-1L])
    arg_names <- names(args) %||% rep("", length(args))
    arg_names[is.na(arg_names)] <- ""
    if (length(args) != 2L || any(nzchar(arg_names) & !arg_names %in% c("from", "to"))) {
      tccq_abort("for-loop seq() currently supports exactly seq(from, to)")
    }
    return(list(start = lower_bound(args[[1L]]), stop = lower_bound(args[[2L]])))
  }

  tccq_abort("for-loop sequence currently supports start:stop or seq(start, stop)")
}

tccq_lower_for_stmt <- function(expr, env, local_names, fallback = "hard") {
  args <- as.list(expr[-1L])
  if (length(args) != 3L) {
    tccq_abort("for-loop lowering expected for (var in seq) body")
  }
  var <- tccq_symbol_name(args[[1L]], what = "for-loop variable")
  if (var %in% names(env) || var %in% local_names) {
    tccq_abort("for-loop variable shadows an existing tccq name: ", var)
  }

  seq <- tccq_lower_loop_sequence(args[[2L]], env, fallback = fallback)
  body_env <- env
  body_env[[var]] <- tccq_type_scalar("xlen")
  body_local_names <- c(local_names, var)
  body_exprs <- tccq_exprs_from_body(args[[3L]])
  body_stmts <- list()
  for (body_expr in body_exprs) {
    lowered <- tccq_lower_stmt(body_expr, body_env, body_local_names, fallback = fallback)
    body_stmts[[length(body_stmts) + 1L]] <- lowered$stmt
    body_env <- lowered$env
    body_local_names <- lowered$local_names
  }

  list(
    stmt = tccq_ir_for_loop(var, seq$start, seq$stop, body_stmts),
    env = env,
    local_names = local_names
  )
}

tccq_lower_stmt <- function(expr, env, local_names, fallback = "hard") {
  if (tccq_is_call_to(expr, "for")) {
    return(tccq_lower_for_stmt(expr, env, local_names, fallback = fallback))
  }

  if (!tccq_is_assignment_call(expr)) {
    tccq_abort(
      "only assignment statements are allowed before the final expression; got: ",
      deparse(expr, nlines = 1L)
    )
  }

  lhs <- tccq_assignment_lhs(expr)
  rhs <- tccq_assignment_rhs(expr)

  if (is.symbol(lhs)) {
    name <- as.character(lhs)
    if (name %in% names(env) && !name %in% local_names) {
      tccq_abort("rebinding formal arguments is not yet supported in tccq: ", name)
    }
    if (name %in% local_names) {
      tccq_abort(
        "rebinding a local name is not yet supported in tccq: ", name,
        ". Use a fresh local name instead."
      )
    }
    value <- tccq_lower_expr(rhs, env, fallback = fallback)
    env[[name]] <- value$type
    local_names <- tccq_unique(c(local_names, name))

    return(list(
      stmt = tccq_ir_bind(name, value),
      env = env,
      local_names = local_names
    ))
  }

  if (tccq_is_subscript_call(lhs)) {
    lhs_args <- as.list(lhs[-1L])
    base_ast <- lhs_args[[1L]]

    if (is.symbol(base_ast) && nzchar(as.character(base_ast))) {
      name <- as.character(base_ast)
      if (!name %in% names(env)) {
        tccq_abort("unknown variable in indexed assignment: ", name)
      }
      target_type <- env[[name]]
      if (!name %in% local_names) {
        tccq_lower_local_write_error(name, target_type)
      }
      if (target_type$rank < 1L) {
        tccq_abort("indexed assignment target must be an array-like value: ", name)
      }

      sub_asts <- lhs_args[-1L]
      if (length(sub_asts) != target_type$rank) {
        tccq_abort(
          "indexed assignment rank mismatch for ", name, ": got ", length(sub_asts),
          " subscript(s), expected ", target_type$rank
        )
      }
      subscripts <- lapply(sub_asts, tccq_lower_store_subscript, env = env, fallback = fallback)

      value <- tccq_lower_expr(rhs, env, fallback = fallback)
      if (!value$type$rank %in% c(0L, 1L)) {
        tccq_abort("indexed assignment currently supports scalar or vector RHS values")
      }
      stmt <- tccq_ir_store_access(name, subscripts, value, target_type)
      return(list(stmt = stmt, env = env, local_names = local_names))
    }

    nested_all <- length(lhs_args) == 2L && tccq_is_missing_subscript_arg(lhs_args[[2L]])
    access <- tccq_lower_expr(if (nested_all) base_ast else lhs, env, fallback = fallback)
    plan <- tccq_ir_access_plan(access)
    if (is.null(plan) || is.null(plan$base_name) || !length(plan$steps)) {
      tccq_abort("indexed assignment LHS must be an access rooted at a direct symbol")
    }

    name <- plan$base_name
    if (!name %in% names(env)) {
      tccq_abort("unknown variable in indexed assignment: ", name)
    }

    target_type <- env[[name]]
    if (!name %in% local_names) {
      tccq_lower_local_write_error(name, target_type)
    }
    if (target_type$rank != 1L) {
      tccq_abort("nested indexed assignment is currently supported for one-dimensional vectors only: ", name)
    }

    value <- tccq_lower_expr(rhs, env, fallback = fallback)
    if (!tccq_is_scalar_rhs_for_assignment(value)) {
      tccq_abort("tccq currently supports scalar RHS assignment only")
    }

    final_step <- plan$steps[[length(plan$steps)]]
    nested_access <- if (length(plan$steps) > 1L) access else NULL
    subscripts <- if (is.null(nested_access)) {
      list(tccq_lower_store_subscript_from_access_step(final_step))
    } else {
      # For nested one-dimensional writes, `access` is the full normalized
      # target chain including the final write step.  Do not duplicate that
      # final step in `subscripts`, or boundary-backed indices would be
      # evaluated once during access setup and again as dead subscript setup.
      list()
    }
    stmt <- tccq_ir_store_access(
      name,
      subscripts,
      value,
      target_type,
      access = nested_access
    )

    return(list(stmt = stmt, env = env, local_names = local_names))
  }

  tccq_abort("unsupported assignment LHS: ", deparse(lhs, nlines = 1L))
}

tccq_lower_expr <- function(expr, env, fallback = "hard") {
  if (is.integer(expr) && length(expr) == 1L) {
    return(tccq_ir_const(as.integer(expr), tccq_type_scalar("integer")))
  }

  if (is.numeric(expr) && length(expr) == 1L) {
    return(tccq_ir_const(as.double(expr), tccq_type_scalar("double")))
  }

  if (is.logical(expr) && length(expr) == 1L) {
    val <- if (is.na(expr)) NA_integer_ else as.integer(expr)
    return(tccq_ir_const(val, tccq_type_scalar("logical")))
  }

  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (!nm %in% names(env)) {
      tccq_abort("unknown symbol in tccq: ", nm)
    }
    return(tccq_ir_var(nm, env[[nm]]))
  }

  if (!is.call(expr)) {
    tccq_abort("unsupported expression type: ", typeof(expr))
  }

  head <- tccq_call_head_name(expr)
  args <- as.list(expr[-1L])

  if (identical(head, "(")) {
    return(tccq_lower_expr(args[[1L]], env, fallback = fallback))
  }

  if (head %in% c("+", "-")) {
    if (length(args) == 1L) {
      x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
      if (identical(head, "+")) {
        return(x)
      }
      out_mode <- if (identical(x$type$mode, "logical")) "integer" else x$type$mode
      return(tccq_ir_unary("-", x, type = tccq_type(mode = out_mode, rank = x$type$rank, dims = x$type$dims)))
    }
    return(tccq_lower_binary(head, args, env, fallback = fallback))
  }

  if (identical(head, "!")) {
    if (length(args) != 1L) {
      tccq_abort("! expects exactly one argument")
    }
    x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    tccq_type_result_mode_logic(x$type, op = "!")
    return(tccq_ir_unary("!", x, type = tccq_type(mode = "logical", rank = x$type$rank, dims = x$type$dims)))
  }

  if (head %in% c("*", "/", "^", "%/%", "<", "<=", ">", ">=", "==", "!=", "&", "|")) {
    return(tccq_lower_binary(head, args, env, fallback = fallback))
  }

  if (identical(head, "identity")) {
    if (length(args) != 1L) {
      tccq_abort("identity() expects exactly one argument")
    }
    return(tccq_lower_expr(args[[1L]], env, fallback = fallback))
  }

  if (head %in% c("sin", "cos", "tan", "exp", "log", "sqrt", "abs")) {
    if (length(args) != 1L) {
      tccq_abort(head, " expects exactly one argument")
    }
    x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    out_type <- tccq_type(mode = "double", rank = x$type$rank, dims = x$type$dims)
    return(tccq_ir_call1(head, x, type = out_type))
  }

  if (head %in% c("double", "integer", "logical")) {
    if (length(args) != 1L) {
      tccq_abort(head, "() vector allocation currently expects exactly one length argument")
    }
    n <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    value <- switch(
      head,
      double = tccq_ir_const(0.0, tccq_type_scalar("double")),
      integer = tccq_ir_const(0L, tccq_type_scalar("integer")),
      logical = tccq_ir_const(0L, tccq_type_scalar("logical"))
    )
    return(tccq_ir_vector_fill(value, n))
  }

  if (identical(head, "matrix")) {
    arg_names <- names(args) %||% rep("", length(args))
    arg_names[is.na(arg_names)] <- ""

    formals <- c("data", "nrow", "ncol", "byrow", "dimnames")
    matched <- stats::setNames(vector("list", length(formals)), formals)

    for (i in seq_along(args)) {
      nm <- arg_names[[i]]
      if (!nzchar(nm)) {
        next
      }
      if (!nm %in% formals) {
        tccq_abort("matrix() currently supports data, nrow, and ncol only")
      }
      if (!is.null(matched[[nm]])) {
        tccq_abort("matrix() argument supplied multiple times: ", nm)
      }
      matched[[nm]] <- args[[i]]
    }

    next_formal <- 1L
    for (i in seq_along(args)) {
      if (nzchar(arg_names[[i]])) {
        next
      }
      while (next_formal <= length(formals) && !is.null(matched[[formals[[next_formal]]]])) {
        next_formal <- next_formal + 1L
      }
      if (next_formal > length(formals)) {
        tccq_abort("matrix() received too many positional arguments")
      }
      matched[[formals[[next_formal]]]] <- args[[i]]
      next_formal <- next_formal + 1L
    }

    if (!is.null(matched$byrow) || !is.null(matched$dimnames)) {
      tccq_abort("matrix() currently supports data, nrow, and ncol only")
    }

    data_ast <- matched$data
    nrow_ast <- matched$nrow
    ncol_ast <- matched$ncol
    if (is.null(data_ast) || is.null(nrow_ast) || is.null(ncol_ast)) {
      tccq_abort("matrix() currently requires data, nrow, and ncol")
    }

    value <- tccq_lower_expr(data_ast, env, fallback = fallback)
    nrow <- tccq_lower_expr(nrow_ast, env, fallback = fallback)
    ncol <- tccq_lower_expr(ncol_ast, env, fallback = fallback)
    return(tccq_ir_matrix_fill(value, nrow, ncol))
  }

  if (head %in% tccq_supported_reducer_names()) {
    return(tccq_lower_reducer_call(head, args, env, fallback = fallback))
  }

  if (identical(head, "which.max")) {
    if (length(args) != 1L) {
      tccq_abort("which.max() currently expects exactly one argument")
    }
    x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    if (!tccq_type_is_numeric(x$type)) {
      tccq_abort("which.max() requires numeric/logical input")
    }
    return(tccq_ir_arg_reduce("which.max", x, type = tccq_type_scalar("integer")))
  }

  if (identical(head, "Reduce")) {
    return(tccq_lower_reduce_family(args, env, fallback = fallback))
  }

  if (identical(head, "length")) {
    if (length(args) != 1L) {
      tccq_abort("length() expects exactly one argument")
    }
    x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    return(tccq_ir_len(x))
  }

  if (identical(head, "[")) {
    if (length(args) == 3L) {
      x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
      if (x$type$rank != 2L) {
        tccq_abort("subscript x[i, j] requires a matrix input")
      }
      row_sub <- tccq_lower_access_subscript(args[[2L]], env, fallback = fallback)
      col_sub <- tccq_lower_access_subscript(args[[3L]], env, fallback = fallback)
      if (identical(row_sub$kind, "index") && identical(col_sub$kind, "index") &&
          row_sub$index$type$rank == 0L && col_sub$index$type$rank == 0L) {
        row <- row_sub$index
        col <- col_sub$index
        return(tccq_ir_index2(x, row, col))
      }
      return(tccq_ir_matrix_view(x, list(row_sub, col_sub)))
    }

    if (length(args) != 2L) {
      tccq_abort("tccq currently supports one-dimensional x[i] or scalar matrix x[i, j] only")
    }

    x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
    if (x$type$rank != 1L) {
      tccq_abort("subscript x[i] requires a vector input")
    }

    idx_ast <- args[[2L]]
    if (tccq_is_colon_call(idx_ast)) {
      range_args <- as.list(idx_ast[-1L])
      if (length(range_args) != 2L) {
        tccq_abort("range slicing requires lo:hi")
      }
      start <- tccq_lower_expr(range_args[[1L]], env, fallback = fallback)
      stop <- tccq_lower_expr(range_args[[2L]], env, fallback = fallback)
      return(tccq_ir_slice_range(x, start, stop))
    }

    index <- tccq_lower_expr(idx_ast, env, fallback = fallback)
    return(tccq_ir_index(x, index))
  }

  if (identical(head, ":")) {
    tccq_abort("':' is currently only supported inside x[lo:hi]")
  }

  tccq_lower_boundary_or_abort(expr, head, args, env, fallback = fallback)
}

tccq_lower_boundary_or_abort <- function(expr, head, args, env, fallback = "hard") {
  fallback <- match.arg(fallback, c("auto", "hard"))
  if (identical(fallback, "hard")) {
    tccq_abort(
      "unsupported call in tccq: ", head,
      ". Add a lowerer case or route it through an explicit boundary node."
    )
  }

  lowered_args <- lapply(args, tccq_lower_expr, env = env, fallback = fallback)
  out_type <- if (length(lowered_args) == 1L) lowered_args[[1L]]$type else tccq_type_scalar("logical")
  tccq_ir_boundary_r_eval(
    call_expr = expr,
    args = lowered_args,
    type = out_type
  )
}

tccq_lower_binary <- function(op, args, env, fallback = "hard") {
  if (length(args) != 2L) {
    tccq_abort("operator ", op, " expects exactly two arguments")
  }
  lhs <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
  rhs <- tccq_lower_expr(args[[2L]], env, fallback = fallback)

  out_mode <- if (op %in% c("+", "-", "*", "/", "^", "%/%")) {
    tccq_type_result_mode_arith(lhs$type, rhs$type, op = op)
  } else if (op %in% c("<", "<=", ">", ">=", "==", "!=")) {
    tccq_type_result_mode_compare(lhs$type, rhs$type, op = op)
  } else if (op %in% c("&", "|")) {
    tccq_type_result_mode_logic(lhs$type, rhs$type, op = op)
  } else {
    tccq_abort("unsupported binary operator in tccq: ", op)
  }

  out_type <- tccq_type_broadcast(lhs$type, rhs$type, mode = out_mode)
  tccq_ir_binary(op, lhs, rhs, type = out_type)
}
