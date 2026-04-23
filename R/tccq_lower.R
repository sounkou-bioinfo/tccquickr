# tccq_lower.R - lower R expressions to fresh expression IR
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

tccq_lower_stmt <- function(expr, env, local_names, fallback = "hard") {
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
    parts <- tccq_subscript_parts(lhs)
    name <- tccq_subscript_base_name(lhs)

    if (!name %in% names(env)) {
      tccq_abort("unknown variable in indexed assignment: ", name)
    }
    if (!name %in% local_names) {
      tccq_abort(
        "indexed assignment currently requires a local vector binding. ",
        "Write y <- ", name, "; y[i] <- value; y instead of mutating formal '", name, "' directly."
      )
    }

    target_type <- env[[name]]
    if (target_type$rank != 1L) {
      tccq_abort("indexed assignment target must be a vector: ", name)
    }

    value <- tccq_lower_expr(rhs, env, fallback = fallback)
    if (!tccq_is_scalar_rhs_for_assignment(value)) {
      tccq_abort("fresh compiler currently supports scalar RHS assignment only")
    }

    if (tccq_is_colon_call(parts$index)) {
      range_args <- as.list(parts$index[-1L])
      if (length(range_args) != 2L) {
        tccq_abort("range assignment requires lo:hi")
      }
      start <- tccq_lower_expr(range_args[[1L]], env, fallback = fallback)
      stop <- tccq_lower_expr(range_args[[2L]], env, fallback = fallback)
      stmt <- tccq_ir_store_range(name, start, stop, value, target_type)
    } else {
      index <- tccq_lower_expr(parts$index, env, fallback = fallback)
      stmt <- tccq_ir_store_index(name, index, value, target_type)
    }

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
      tccq_abort("unknown symbol in fresh compiler: ", nm)
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
      return(tccq_ir_unary("-", x, type = x$type))
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

  if (head %in% c("*", "/", "^", "<", "<=", ">", ">=", "==", "!=", "&", "|")) {
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

  if (head %in% tccq_supported_reducer_names()) {
    return(tccq_lower_reducer_call(head, args, env, fallback = fallback))
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
    if (length(args) != 2L) {
      tccq_abort("fresh compiler supports one-dimensional x[i] only")
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
      "unsupported call in fresh compiler: ", head,
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

  out_mode <- if (op %in% c("+", "-", "*", "/", "^")) {
    tccq_type_result_mode_arith(lhs$type, rhs$type, op = op)
  } else if (op %in% c("<", "<=", ">", ">=", "==", "!=")) {
    tccq_type_result_mode_compare(lhs$type, rhs$type, op = op)
  } else if (op %in% c("&", "|")) {
    tccq_type_result_mode_logic(lhs$type, rhs$type, op = op)
  } else {
    tccq_abort("unsupported binary operator in fresh compiler: ", op)
  }

  out_type <- tccq_type_broadcast(lhs$type, rhs$type, mode = out_mode)
  tccq_ir_binary(op, lhs, rhs, type = out_type)
}
