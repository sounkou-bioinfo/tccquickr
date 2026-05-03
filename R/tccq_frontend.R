# tccq_frontend.R - parse declare()-annotated function into a module shell
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_frontend <- function(fn, entry = "tccq_entry") {
  if (!is.function(fn)) {
    tccq_abort("fn must be a function")
  }

  parsed <- tccq_parse_function_body(fn)
  formal_names <- names(formals(fn))
  if (is.null(formal_names)) {
    formal_names <- character()
  }

  if (!length(formal_names)) {
    tccq_abort("tccq expects at least one formal argument")
  }

  missing <- setdiff(formal_names, names(parsed$types))
  if (length(missing)) {
    tccq_abort(
      "tccq requires declare(type(...)) annotations for every formal; missing: ",
      paste(missing, collapse = ", ")
    )
  }

  tccq_module(
    entry = entry,
    formal_names = formal_names,
    types = parsed$types[formal_names],
    expr = parsed$expr,
    body_exprs = parsed$body_exprs
  )
}

tccq_parse_function_body <- function(fn) {
  body_expr <- body(fn)
  exprs <- if (tccq_is_call_to(body_expr, "{")) {
    as.list(body_expr[-1L])
  } else {
    list(body_expr)
  }

  types <- list()
  consumed <- 0L

  for (i in seq_along(exprs)) {
    expr <- exprs[[i]]
    if (!tccq_is_call_to(expr, "declare")) {
      break
    }
    consumed <- consumed + 1L
    types <- utils::modifyList(types, tccq_parse_declare_call(expr))
  }

  remaining <- if (consumed < length(exprs)) {
    exprs[(consumed + 1L):length(exprs)]
  } else {
    list()
  }
  remaining <- remaining[!vapply(remaining, is.null, logical(1))]

  if (!length(remaining)) {
    tccq_abort("function body must contain at least one expression after declare(...)")
  }

  list(
    types = types,
    expr = remaining[[length(remaining)]],
    body_exprs = remaining
  )
}

tccq_parse_declare_call <- function(expr) {
  args <- as.list(expr[-1L])
  out <- list()

  for (arg in args) {
    decls <- if (tccq_is_call_to(arg, "{")) {
      as.list(arg[-1L])
    } else {
      list(arg)
    }
    decls <- decls[!vapply(decls, is.null, logical(1))]

    for (decl in decls) {
      if (tccq_is_call_to(decl, "type")) {
        out <- utils::modifyList(out, tccq_parse_type_decl(decl))
      } else {
        tccq_abort(
          "tccq currently supports declare(type(x = double(NA), ...)) or declare({ type(...) }) only"
        )
      }
    }
  }

  out
}

tccq_parse_type_decl <- function(expr) {
  args <- as.list(expr[-1L])
  nms <- names(args)
  if (is.null(nms) || any(!nzchar(nms))) {
    tccq_abort("type(...) entries must be named, e.g. type(x = double(NA))")
  }

  out <- list()
  for (i in seq_along(args)) {
    nm <- nms[[i]]
    out[[nm]] <- tccq_parse_type_call(args[[i]])
  }
  out
}
