# tccq_conformance.R - R-as-oracle conformance harness (Porffor discipline)
# SPDX-License-Identifier: GPL-3.0-or-later
#
# Generates programs over the in-subset grammar, compiles each with tccq, runs
# the identical program in the R interpreter, and diffs the results. R is the
# ground-truth oracle. The pass count is a tracked number CI gates on. Because
# the generator only emits constructs that the frontend accepts as a pure-C
# kernel, every case must match R: passed == total, always.

# Program construction --------------------------------------------------------

tccq_cf_decl_call <- function(types) {
  call("declare", as.call(c(as.name("type"), stats::setNames(types, names(types)))))
}

tccq_cf_declared_fn <- function(params, types, expr) {
  formals <- as.pairlist(stats::setNames(vector("list", length(params)), params))
  body <- as.call(c(as.name("{"), list(tccq_cf_decl_call(types), expr)))
  eval(call("function", formals, body), envir = baseenv())
}

tccq_cf_ref_fn <- function(params, expr) {
  formals <- as.pairlist(stats::setNames(vector("list", length(params)), params))
  body <- as.call(c(as.name("{"), list(expr)))
  eval(call("function", formals, body), envir = baseenv())
}

# Generators (only core constructs) -------------------------------------------

tccq_cf_num_leaf <- function() {
  sample(list(quote(x), quote(y), 1, -1, 2), size = 1L)[[1L]]
}

tccq_cf_num_expr <- function(depth) {
  if (depth <= 0L || stats::runif(1) < 0.35) {
    return(tccq_cf_num_leaf())
  }
  choice <- sample(c("binary", "unary", "call1"), size = 1L)
  if (identical(choice, "binary")) {
    op <- sample(c("+", "-", "*", "/"), size = 1L)
    return(as.call(list(as.name(op), tccq_cf_num_expr(depth - 1L), tccq_cf_num_expr(depth - 1L))))
  }
  if (identical(choice, "unary")) {
    return(as.call(list(as.name("-"), tccq_cf_num_expr(depth - 1L))))
  }
  fun <- sample(c("sin", "cos", "abs", "exp", "sqrt"), size = 1L)
  as.call(list(as.name(fun), tccq_cf_num_expr(depth - 1L)))
}

tccq_cf_logical_expr <- function(depth) {
  if (depth <= 0L || stats::runif(1) < 0.45) {
    op <- sample(c("<", "<=", ">", ">=", "==", "!="), size = 1L)
    return(as.call(list(as.name(op), tccq_cf_num_expr(1L), tccq_cf_num_expr(1L))))
  }
  if (identical(sample(c("binary", "not"), size = 1L), "not")) {
    return(as.call(list(as.name("!"), tccq_cf_logical_expr(depth - 1L))))
  }
  op <- sample(c("&", "|"), size = 1L)
  as.call(list(as.name(op), tccq_cf_logical_expr(depth - 1L), tccq_cf_logical_expr(depth - 1L)))
}

# One random in-subset program: an elementwise vector kernel, a numeric reducer,
# or a logical reducer.
tccq_cf_gen_case <- function() {
  kind <- sample(c("elementwise", "num_reduce", "logical_reduce"), size = 1L)
  if (identical(kind, "elementwise")) {
    return(tccq_cf_num_expr(3L))
  }
  if (identical(kind, "num_reduce")) {
    red <- sample(c("sum", "prod", "min", "max", "mean"), size = 1L)
    return(as.call(list(as.name(red), tccq_cf_num_expr(2L))))
  }
  red <- sample(c("any", "all"), size = 1L)
  as.call(list(as.name(red), tccq_cf_logical_expr(2L)))
}

tccq_cf_input_sets <- function(n_sets) {
  out <- vector("list", n_sets)
  for (i in seq_len(n_sets)) {
    n <- sample(1:8, size = 1L)
    out[[i]] <- list(
      x = stats::runif(n, -2, 2),
      y = stats::runif(n, -2, 2)
    )
  }
  out
}

# Comparison: numeric tolerance, with NA/NaN treated as matching positions.
tccq_cf_agrees <- function(a, b, tol = 1e-8) {
  if (length(a) != length(b)) {
    return(FALSE)
  }
  na_a <- is.na(a); na_b <- is.na(b)
  if (!identical(na_a, na_b)) {
    return(FALSE)
  }
  ok <- !na_a
  if (!any(ok)) {
    return(TRUE)
  }
  isTRUE(all.equal(a[ok], b[ok], tolerance = tol))
}

# Harness ---------------------------------------------------------------------

#' Run the R-as-oracle conformance suite
#'
#' Generates `n_cases` in-subset programs, compiles each with `tccq_compile`,
#' evaluates the same program in R, and diffs the results across several random
#' inputs. R is the oracle.
#'
#' @param n_cases Number of generated programs.
#' @param seed Random seed (the suite is deterministic for a given seed).
#' @param backend Backend used to compile cases.
#' @return A list with `total`, `passed`, and a per-case `results` data frame.
#' @keywords internal
#' @noRd
tccq_conformance_run <- function(n_cases = 40L, seed = 20260611L,
                                 backend = tccq_backend_tinycc()) {
  set.seed(seed)
  params <- c("x", "y")
  types <- c(x = quote(double(NA)), y = quote(double(NA)))

  rows <- vector("list", n_cases)
  for (i in seq_len(n_cases)) {
    expr <- tccq_cf_gen_case()
    inputs <- tccq_cf_input_sets(4L)
    status <- "pass"
    detail <- ""

    res <- tryCatch({
      fn <- tccq_cf_declared_fn(params, types, expr)
      ref <- tccq_cf_ref_fn(params, expr)
      compiled <- tccq_compile(fn, backend = backend)
      for (inp in inputs) {
        got <- do.call(compiled, inp)
        want <- do.call(ref, inp)
        if (!tccq_cf_agrees(got, want)) {
          status <- "fail"
          detail <- paste0("mismatch on n=", length(inp$x))
          break
        }
      }
      TRUE
    }, error = function(e) {
      status <<- "error"
      detail <<- conditionMessage(e)
      FALSE
    })

    rows[[i]] <- data.frame(
      case = i,
      expr = paste(deparse(expr), collapse = " "),
      status = status,
      detail = detail,
      stringsAsFactors = FALSE
    )
  }

  results <- do.call(rbind, rows)
  list(
    total = n_cases,
    passed = sum(results$status == "pass"),
    results = results
  )
}
