# tccq2_lower.R - lower R expressions to fresh expression IR
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_lower_module <- function(module) {
  tccq2_module_validate(module)
  body_exprs <- module$body_exprs %||% list(module$expr)
  ir <- tccq2_lower_program(body_exprs, module$types)
  tccq2_module_with(module, ir = ir)
}

tccq2_lower_program <- function(exprs, env) {
  if (!length(exprs)) {
    tccq2_abort("cannot lower empty program")
  }

  stmts <- list()
  current_env <- env
  local_names <- character()

  if (length(exprs) > 1L) {
    for (i in seq_len(length(exprs) - 1L)) {
      lowered <- tccq2_lower_stmt(exprs[[i]], current_env, local_names)
      stmts[[length(stmts) + 1L]] <- lowered$stmt
      current_env <- lowered$env
      local_names <- lowered$local_names
    }
  }

  result <- tccq2_lower_expr(exprs[[length(exprs)]], current_env)
  tccq2_ir_program(stmts, result)
}

tccq2_lower_stmt <- function(expr, env, local_names) {
  if (!tccq2_is_assignment_call(expr)) {
    tccq2_abort(
      "only assignment statements are allowed before the final expression; got: ",
      deparse(expr, nlines = 1L)
    )
  }

  lhs <- tccq2_assignment_lhs(expr)
  rhs <- tccq2_assignment_rhs(expr)

  if (is.symbol(lhs)) {
    name <- as.character(lhs)
    if (name %in% names(env) && !name %in% local_names) {
      tccq2_abort("rebinding formal arguments is not yet supported in tccq2: ", name)
    }
    value <- tccq2_lower_expr(rhs, env)
    env[[name]] <- value$type
    local_names <- tccq2_unique(c(local_names, name))

    return(list(
      stmt = tccq2_ir_bind(name, value),
      env = env,
      local_names = local_names
    ))
  }

  if (tccq2_is_subscript_call(lhs)) {
    parts <- tccq2_subscript_parts(lhs)
    name <- tccq2_subscript_base_name(lhs)

    if (!name %in% names(env)) {
      tccq2_abort("unknown variable in indexed assignment: ", name)
    }
    if (!name %in% local_names) {
      tccq2_abort(
        "indexed assignment currently requires a local vector binding. ",
        "Write y <- ", name, "; y[i] <- value; y instead of mutating formal '", name, "' directly."
      )
    }

    target_type <- env[[name]]
    if (target_type$rank != 1L) {
      tccq2_abort("indexed assignment target must be a vector: ", name)
    }

    value <- tccq2_lower_expr(rhs, env)
    if (!tccq2_is_scalar_rhs_for_assignment(value)) {
      tccq2_abort("fresh compiler currently supports scalar RHS assignment only")
    }

    if (tccq2_is_colon_call(parts$index)) {
      range_args <- as.list(parts$index[-1L])
      if (length(range_args) != 2L) {
        tccq2_abort("range assignment requires lo:hi")
      }
      start <- tccq2_lower_expr(range_args[[1L]], env)
      stop <- tccq2_lower_expr(range_args[[2L]], env)
      stmt <- tccq2_ir_store_range(name, start, stop, value, target_type)
    } else {
      index <- tccq2_lower_expr(parts$index, env)
      stmt <- tccq2_ir_store_index(name, index, value, target_type)
    }

    return(list(stmt = stmt, env = env, local_names = local_names))
  }

  tccq2_abort("unsupported assignment LHS: ", deparse(lhs, nlines = 1L))
}

tccq2_lower_expr <- function(expr, env) {
  if (is.integer(expr) && length(expr) == 1L) {
    return(tccq2_ir_const(as.integer(expr), tccq2_type_scalar("integer")))
  }

  if (is.numeric(expr) && length(expr) == 1L) {
    return(tccq2_ir_const(as.double(expr), tccq2_type_scalar("double")))
  }

  if (is.logical(expr) && length(expr) == 1L) {
    return(tccq2_ir_const(isTRUE(expr), tccq2_type_scalar("logical")))
  }

  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (!nm %in% names(env)) {
      tccq2_abort("unknown symbol in fresh compiler: ", nm)
    }
    return(tccq2_ir_var(nm, env[[nm]]))
  }

  if (!is.call(expr)) {
    tccq2_abort("unsupported expression type: ", typeof(expr))
  }

  head <- tccq2_call_head_name(expr)
  args <- as.list(expr[-1L])

  if (identical(head, "(")) {
    return(tccq2_lower_expr(args[[1L]], env))
  }

  if (head %in% c("+", "-")) {
    if (length(args) == 1L) {
      x <- tccq2_lower_expr(args[[1L]], env)
      if (identical(head, "+")) {
        return(x)
      }
      return(tccq2_ir_unary("-", x, type = x$type))
    }
    return(tccq2_lower_binary(head, args, env))
  }

  if (head %in% c("*", "/", "^")) {
    return(tccq2_lower_binary(head, args, env))
  }

  if (head %in% c("sin", "cos", "tan", "exp", "log", "sqrt", "abs")) {
    if (length(args) != 1L) {
      tccq2_abort(head, " expects exactly one argument")
    }
    x <- tccq2_lower_expr(args[[1L]], env)
    out_type <- tccq2_type(mode = "double", rank = x$type$rank, dims = x$type$dims)
    return(tccq2_ir_call1(head, x, type = out_type))
  }

  if (identical(head, "sum")) {
    if (length(args) != 1L) {
      tccq2_abort("fresh compiler currently supports sum(x) with one argument")
    }
    x <- tccq2_lower_expr(args[[1L]], env)
    if (!tccq2_type_is_numeric(x$type)) {
      tccq2_abort("sum() requires numeric/logical input")
    }
    return(tccq2_ir_reduce("sum", x, type = tccq2_type_scalar("double")))
  }

  if (identical(head, "length")) {
    if (length(args) != 1L) {
      tccq2_abort("length() expects exactly one argument")
    }
    x <- tccq2_lower_expr(args[[1L]], env)
    return(tccq2_ir_len(x))
  }

  if (identical(head, "[")) {
    if (length(args) != 2L) {
      tccq2_abort("fresh compiler supports one-dimensional x[i] only")
    }

    x <- tccq2_lower_expr(args[[1L]], env)
    if (x$type$rank != 1L) {
      tccq2_abort("subscript x[i] requires a vector input")
    }

    idx_ast <- args[[2L]]
    if (tccq2_is_colon_call(idx_ast)) {
      range_args <- as.list(idx_ast[-1L])
      if (length(range_args) != 2L) {
        tccq2_abort("range slicing requires lo:hi")
      }
      start <- tccq2_lower_expr(range_args[[1L]], env)
      stop <- tccq2_lower_expr(range_args[[2L]], env)
      return(tccq2_ir_slice_range(x, start, stop))
    }

    index <- tccq2_lower_expr(idx_ast, env)
    return(tccq2_ir_index(x, index))
  }

  if (identical(head, ":")) {
    tccq2_abort("':' is currently only supported inside x[lo:hi]")
  }

  tccq2_abort(
    "unsupported call in fresh compiler: ", head,
    ". Add a lowerer case or route it through an explicit boundary node."
  )
}

tccq2_lower_binary <- function(op, args, env) {
  if (length(args) != 2L) {
    tccq2_abort("operator ", op, " expects exactly two arguments")
  }
  lhs <- tccq2_lower_expr(args[[1L]], env)
  rhs <- tccq2_lower_expr(args[[2L]], env)
  out_mode <- tccq2_type_result_mode_arith(lhs$type, rhs$type, op = op)
  out_type <- tccq2_type_broadcast(lhs$type, rhs$type, mode = out_mode)
  tccq2_ir_binary(op, lhs, rhs, type = out_type)
}
