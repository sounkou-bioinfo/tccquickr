# tccq2_frontend.R - parse declare()-annotated function into a module shell
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_frontend <- function(fn, entry = "tccq2_entry") {
  if (!is.function(fn)) {
    tccq2_abort("fn must be a function")
  }

  parsed <- tccq2_parse_function_body(fn)
  formal_names <- names(formals(fn))
  if (is.null(formal_names)) {
    formal_names <- character()
  }

  if (!length(formal_names)) {
    tccq2_abort("fresh compiler expects at least one formal argument")
  }

  missing <- setdiff(formal_names, names(parsed$types))
  if (length(missing)) {
    tccq2_abort(
      "fresh compiler requires declare(type(...)) annotations for every formal; missing: ",
      paste(missing, collapse = ", ")
    )
  }

  tccq2_module(
    entry = entry,
    formal_names = formal_names,
    types = parsed$types[formal_names],
    expr = parsed$expr,
    body_exprs = parsed$body_exprs
  )
}

tccq2_parse_function_body <- function(fn) {
  body_expr <- body(fn)
  exprs <- if (tccq2_is_call_to(body_expr, "{")) {
    as.list(body_expr[-1L])
  } else {
    list(body_expr)
  }

  types <- list()
  consumed <- 0L

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    if (!tccq2_is_call_to(expr, "declare")) {
      break
    }
    consumed <- consumed + 1L
    types <- modifyList(types, tccq2_parse_declare_call(expr))
  }

  remaining <- if (consumed < length(exprs)) {
    exprs[(consumed + 1L):length(exprs)]
  } else {
    list()
  }
  remaining <- remaining[!vapply(remaining, is.null, logical(1))]

  if (!length(remaining)) {
    tccq2_abort("function body must contain at least one expression after declare(...)")
  }

  list(
    types = types,
    expr = remaining[[length(remaining)]],
    body_exprs = remaining
  )
}

tccq2_parse_declare_call <- function(expr) {
  args <- as.list(expr[-1L])
  out <- list()

  for (arg in args) {
    if (tccq2_is_call_to(arg, "type")) {
      out <- modifyList(out, tccq2_parse_type_decl(arg))
    } else {
      tccq2_abort(
        "fresh compiler currently supports declare(type(x = double(NA), ...)) only"
      )
    }
  }

  out
}

tccq2_parse_type_decl <- function(expr) {
  args <- as.list(expr[-1L])
  nms <- names(args)
  if (is.null(nms) || any(!nzchar(nms))) {
    tccq2_abort("type(...) entries must be named, e.g. type(x = double(NA))")
  }

  out <- list()
  for (i in seq_along(args)) {
    nm <- nms[[i]]
    out[[nm]] <- tccq2_parse_type_call(args[[i]])
  }
  out
}
