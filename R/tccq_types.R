# tccq_types.R - tiny typed shape system for the fresh compiler
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_type <- function(mode, rank = 0L, dims = NULL) {
  mode <- as.character(mode)
  if (identical(mode, "numeric")) {
    mode <- "double"
  }
  if (!mode %in% c("double", "integer", "logical", "raw")) {
    tccq_abort("unsupported mode: ", mode)
  }

  rank <- as.integer(rank)
  if (rank < 0L) {
    tccq_abort("rank must be >= 0")
  }

  if (is.null(dims)) {
    dims <- rep(NA_integer_, rank)
  }
  if (length(dims) != rank) {
    tccq_abort("dims length must match rank")
  }

  structure(
    list(mode = mode, rank = rank, dims = dims),
    class = "tccq_type"
  )
}

tccq_type_scalar <- function(mode = "double") {
  tccq_type(mode = mode, rank = 0L, dims = integer())
}

tccq_type_vector <- function(mode = "double", length = NA_integer_) {
  tccq_type(mode = mode, rank = 1L, dims = length)
}

tccq_type_matrix <- function(mode = "double", nrow = NA_integer_, ncol = NA_integer_) {
  tccq_type(mode = mode, rank = 2L, dims = c(nrow, ncol))
}

tccq_type_is_scalar <- function(type) {
  is.list(type) && identical(as.integer(type$rank), 0L)
}

tccq_type_is_vector <- function(type) {
  is.list(type) && identical(as.integer(type$rank), 1L)
}

tccq_type_is_matrix <- function(type) {
  is.list(type) && identical(as.integer(type$rank), 2L)
}

tccq_type_is_numeric <- function(type) {
  type$mode %in% c("double", "integer", "logical")
}

tccq_type_to_string <- function(type) {
  if (type$rank == 0L) {
    return(type$mode)
  }
  paste0(type$mode, "[", paste(type$dims, collapse = ","), "]")
}

tccq_type_result_mode_arith <- function(a, b, op = NULL) {
  if (!tccq_type_is_numeric(a) || !tccq_type_is_numeric(b)) {
    tccq_abort("arithmetic requires numeric/logical operands")
  }
  if (identical(op, "/") || identical(op, "^") || a$mode == "double" || b$mode == "double") {
    return("double")
  }
  if (a$mode == "integer" || b$mode == "integer") {
    return("integer")
  }
  "logical"
}

tccq_type_broadcast <- function(a, b, mode = NULL) {
  if (a$rank == b$rank) {
    rank <- a$rank
    dims <- a$dims
  } else if (a$rank == 0L) {
    rank <- b$rank
    dims <- b$dims
  } else if (b$rank == 0L) {
    rank <- a$rank
    dims <- a$dims
  } else {
    tccq_abort(
      "cannot broadcast ranks ", a$rank, " and ", b$rank,
      "; fresh milestone only supports scalar-vector broadcasting"
    )
  }

  tccq_type(mode = mode %||% tccq_type_result_mode_arith(a, b), rank = rank, dims = dims)
}

tccq_sexptype_for_mode <- function(mode) {
  switch(
    mode,
    double = "REALSXP",
    integer = "INTSXP",
    logical = "LGLSXP",
    raw = "RAWSXP",
    tccq_abort("unsupported R mode for SEXP: ", mode)
  )
}

tccq_c_scalar_type_for_mode <- function(mode) {
  switch(
    mode,
    double = "double",
    integer = "int",
    logical = "int",
    raw = "unsigned char",
    tccq_abort("unsupported C scalar mode: ", mode)
  )
}

tccq_c_const_literal <- function(value, mode) {
  switch(
    mode,
    double = {
      if (is.nan(value)) "R_NaN" else if (is.infinite(value)) {
        if (value > 0) "R_PosInf" else "R_NegInf"
      } else {
        sprintf("%.17g", as.double(value))
      }
    },
    integer = paste0(as.integer(value)),
    logical = if (isTRUE(value)) "1" else "0",
    raw = paste0(as.integer(value)),
    tccq_abort("unsupported literal mode: ", mode)
  )
}

tccq_parse_type_call <- function(expr) {
  head <- tccq_call_head_name(expr)
  if (is.null(head)) {
    tccq_abort("type annotation must be a call such as double(NA)")
  }
  if (identical(head, "numeric")) {
    head <- "double"
  }
  if (!head %in% c("double", "integer", "logical", "raw")) {
    tccq_abort("unsupported declared type: ", head)
  }

  dims_expr <- as.list(expr[-1L])
  rank <- length(dims_expr)
  dims <- rep(NA_integer_, rank)

  for (i in seq_along(dims_expr)) {
    d <- dims_expr[[i]]
    if (is.numeric(d) && length(d) == 1L && !is.na(d)) {
      dims[[i]] <- as.integer(d)
    } else {
      dims[[i]] <- NA_integer_
    }
  }

  tccq_type(mode = head, rank = rank, dims = dims)
}
