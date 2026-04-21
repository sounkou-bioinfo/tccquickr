# tccq2_lower.R - lower R expressions to fresh expression IR
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_lower_module <- function(module) {
  tccq2_module_validate(module)
  ir <- tccq2_lower_expr(module$expr, module$types)
  tccq2_module_with(module, ir = ir)
}

tccq2_lower_expr <- function(expr, env) {
  if (is.numeric(expr) && length(expr) == 1L) {
    return(tccq2_ir_const(as.double(expr), tccq2_type_scalar("double")))
  }

  if (is.integer(expr) && length(expr) == 1L) {
    return(tccq2_ir_const(as.integer(expr), tccq2_type_scalar("integer")))
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
