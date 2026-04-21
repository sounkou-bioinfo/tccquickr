# Generated-code evidence tests for tcc_quick
#
# These tests do two things:
#   1. keep an explicit corpus of hand-picked kernels we care about
#   2. generate additional valid R-subset functions and require that
#      R semantics == generated C semantics for the same generated source
#
# This is not a formal proof, but it is a reproducible witness framework:
# generated R code -> lowered IR -> generated C -> compiled callable ->
# equality against the original generated R function.

library(tccquickr)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

make_vec_fun <- function(expr, arg_names = c("x", "y"), mode = "double") {
  formals_pairlist <- as.pairlist(stats::setNames(rep(list(quote(expr = )), length(arg_names)), arg_names))
  type_calls <- lapply(arg_names, function(nm) {
    str2lang(sprintf("type(%s = %s(NA))", nm, mode))
  })
  declare_call <- as.call(c(list(as.name("declare")), type_calls))
  body_expr <- as.call(c(list(as.name("{"), declare_call), list(expr)))
  out <- eval(call("function", formals_pairlist, body_expr), envir = parent.frame())
  environment(out) <- .GlobalEnv
  out
}

make_reduce_fun <- function(reducer, expr) {
  make_vec_fun(call(reducer, expr), arg_names = c("x", "y"), mode = "double")
}

render_case_label <- function(expr) {
  paste(deparse(expr, width.cutoff = 500L), collapse = " ")
}

run_native_equivalence <- function(fn, args, label) {
  src <- tcc_quick(fn, fallback = "hard", mode = "code")
  fast <- tcc_quick(fn, fallback = "hard")

  expect_true(is.character(src), info = label)
  expect_true(grepl("SEXP tcc_quick_entry", src, fixed = TRUE), info = label)
  expect_false(grepl("Rf_eval\\(", src), info = label)

  expected <- do.call(fn, args)
  actual <- do.call(fast, args)
  expect_equal(actual, expected, tolerance = 1e-11, info = label)
}

set.seed(20260421)
xx <- runif(64, min = 0.25, max = 2.5)
yy <- runif(64, min = 0.25, max = 2.5)

# ---------------------------------------------------------------------------
# Explicit corpus: small named kernels we want to keep stable over time
# ---------------------------------------------------------------------------

explicit_cases <- list(
  vec_add = quote(x + y),
  vec_affine = quote((x * 2.0) - (y / 3.0)),
  vec_trig = quote(sin(x) + cos(y)),
  vec_minmax = quote(pmax(x, y) - pmin(x, y)),
  vec_rev = quote(rev(x + y)),
  reduce_sum = call("sum", quote((x * y) + x)),
  reduce_mean = call("mean", quote(sin(x) + y)),
  reduce_prod = call("prod", quote((x / y) + 1.0))
)

for (nm in names(explicit_cases)) {
  expr <- explicit_cases[[nm]]
  fn <- make_vec_fun(expr)
  run_native_equivalence(fn, list(xx, yy), paste0("explicit:", nm))
}

# ---------------------------------------------------------------------------
# Generated expression family: deterministic expression synthesis
# ---------------------------------------------------------------------------

build_unary <- function(fun, sym) {
  if (identical(fun, "id")) return(sym)
  call(fun, sym)
}

unary_left <- c("id", "sin", "cos", "abs")
unary_right <- c("id", "sin", "cos")
binary_ops <- c("+", "-", "*")

generated_exprs <- list()
for (ul in unary_left) {
  for (ur in unary_right) {
    for (op in binary_ops) {
      lhs <- build_unary(ul, quote(x))
      rhs <- build_unary(ur, quote(y))
      generated_exprs[[length(generated_exprs) + 1L]] <- call(op, lhs, rhs)
    }
  }
}

# Keep the set explicit and deterministic, but still synthesized.
for (i in seq_len(min(18L, length(generated_exprs)))) {
  expr <- generated_exprs[[i]]
  fn <- make_vec_fun(expr)
  label <- paste0("generated-expr:", i, ":", render_case_label(expr))
  run_native_equivalence(fn, list(xx, yy), label)
}

# ---------------------------------------------------------------------------
# Generated loop kernels from integer parameters embedded in generated R code
# ---------------------------------------------------------------------------

make_window_kernel <- function(k, scale) {
  stopifnot(k >= 1L)
  eval(bquote(
    function(x) {
      declare(type(x = double(NA)))
      n <- length(x)
      out <- double(n - .(k) + 1L)
      for (i in seq_len(n - .(k) + 1L)) {
        acc <- 0.0
        for (j in seq_len(.(k))) {
          acc <- acc + x[i + j - 1L] * (.(scale) * j)
        }
        out[i] <- acc
      }
      out
    }
  ), envir = .GlobalEnv)
}

loop_inputs <- list(runif(40, 0.1, 1.5), runif(25, 0.1, 2.0))
loop_specs <- list(
  list(k = 2L, scale = 0.5),
  list(k = 3L, scale = 1.0),
  list(k = 5L, scale = 1.5)
)

for (spec in loop_specs) {
  fn <- make_window_kernel(spec$k, spec$scale)
  src <- tcc_quick(fn, fallback = "hard", mode = "code")
  fast <- tcc_quick(fn, fallback = "hard")
  expect_false(grepl("Rf_eval\\(", src), info = paste0("window-k", spec$k))
  for (x in loop_inputs) {
    expect_equal(
      fast(x),
      fn(x),
      tolerance = 1e-11,
      info = paste0("window-k", spec$k, "-n", length(x))
    )
  }
}

# ---------------------------------------------------------------------------
# Generated alpha-equivalent functions: same body, distinct identifiers
# ---------------------------------------------------------------------------

alpha_a <- function(a, b) {
  declare(type(a = double(NA)), type(b = double(NA)))
  tmp <- sin(a) + b
  out <- tmp * (a - b)
  out
}

alpha_b <- function(left, right) {
  declare(type(left = double(NA)), type(right = double(NA)))
  scratch <- sin(left) + right
  dest <- scratch * (left - right)
  dest
}

fast_alpha_a <- tcc_quick(alpha_a, fallback = "hard")
fast_alpha_b <- tcc_quick(alpha_b, fallback = "hard")

expect_equal(fast_alpha_a(xx, yy), alpha_a(xx, yy), tolerance = 1e-11)
expect_equal(fast_alpha_b(xx, yy), alpha_b(xx, yy), tolerance = 1e-11)
expect_equal(fast_alpha_a(xx, yy), fast_alpha_b(xx, yy), tolerance = 1e-11)
