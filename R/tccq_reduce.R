# tccq_reduce.R - reducer registry and lowering helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_reducer_spec <- function(name, accepts, result_type, reduce_fun_aliases = character()) {
  list(
    name = name,
    accepts = accepts,
    result_type = result_type,
    reduce_fun_aliases = reduce_fun_aliases
  )
}

tccq_reducer_specs <- function() {
  list(
    sum = tccq_reducer_spec(
      name = "sum",
      accepts = function(type) tccq_type_is_numeric(type),
      result_type = function(type) tccq_type_scalar("double"),
      reduce_fun_aliases = "+"
    ),
    prod = tccq_reducer_spec(
      name = "prod",
      accepts = function(type) tccq_type_is_numeric(type),
      result_type = function(type) tccq_type_scalar("double"),
      reduce_fun_aliases = "*"
    ),
    min = tccq_reducer_spec(
      name = "min",
      accepts = function(type) tccq_type_is_numeric(type),
      result_type = function(type) tccq_type_scalar("double"),
      reduce_fun_aliases = "min"
    ),
    max = tccq_reducer_spec(
      name = "max",
      accepts = function(type) tccq_type_is_numeric(type),
      result_type = function(type) tccq_type_scalar("double"),
      reduce_fun_aliases = "max"
    ),
    mean = tccq_reducer_spec(
      name = "mean",
      accepts = function(type) tccq_type_is_numeric(type),
      result_type = function(type) tccq_type_scalar("double"),
      reduce_fun_aliases = character()
    ),
    any = tccq_reducer_spec(
      name = "any",
      accepts = function(type) tccq_type_is_logical(type),
      result_type = function(type) tccq_type_scalar("logical"),
      reduce_fun_aliases = "|"
    ),
    all = tccq_reducer_spec(
      name = "all",
      accepts = function(type) tccq_type_is_logical(type),
      result_type = function(type) tccq_type_scalar("logical"),
      reduce_fun_aliases = "&"
    )
  )
}

tccq_supported_reducer_names <- function() {
  names(tccq_reducer_specs())
}

tccq_reducer_spec_get <- function(name) {
  spec <- tccq_reducer_specs()[[name]]
  if (is.null(spec)) {
    tccq_abort("unsupported reducer in fresh compiler: ", name)
  }
  spec
}

tccq_reducer_validate_input <- function(name, type) {
  spec <- tccq_reducer_spec_get(name)
  if (!isTRUE(spec$accepts(type))) {
    if (name %in% c("any", "all")) {
      tccq_abort(name, "() requires logical input")
    }
    tccq_abort(name, "() requires numeric/logical input")
  }
  invisible(TRUE)
}

tccq_reducer_result_type <- function(name, type) {
  tccq_reducer_spec_get(name)$result_type(type)
}

tccq_reduce_fun_name <- function(fun) {
  if (is.symbol(fun)) {
    nm <- as.character(fun)
  } else if (is.character(fun) && length(fun) == 1L && !is.na(fun)) {
    nm <- as.character(fun)
  } else {
    tccq_abort("Reduce() currently requires a simple reducer symbol or string")
  }

  alias_map <- list(
    `+` = "sum",
    `*` = "prod",
    `&` = "all",
    `|` = "any",
    min = "min",
    max = "max"
  )
  reducer <- alias_map[[nm]]
  if (!is.null(reducer)) {
    return(reducer)
  }

  tccq_abort(
    "Reduce() currently supports only recognized reducer functions/operators; got: ",
    nm
  )
}

tccq_lower_reducer_call <- function(name, args, env, fallback = "hard") {
  if (length(args) != 1L) {
    tccq_abort(name, "() currently expects exactly one argument")
  }
  x <- tccq_lower_expr(args[[1L]], env, fallback = fallback)
  tccq_reducer_validate_input(name, x$type)
  tccq_ir_reduce(name, x, type = tccq_reducer_result_type(name, x$type))
}

tccq_lower_reduce_family <- function(args, env, fallback = "hard") {
  if (length(args) != 2L) {
    tccq_abort("Reduce() currently supports exactly Reduce(FUN, x)")
  }

  reducer <- tccq_reduce_fun_name(args[[1L]])
  x <- tccq_lower_expr(args[[2L]], env, fallback = fallback)
  tccq_reducer_validate_input(reducer, x$type)
  tccq_ir_reduce(
    reducer,
    x,
    type = tccq_reducer_result_type(reducer, x$type),
    surface = "Reduce"
  )
}
