# test_tccq_generated_validation.R

make_decl_call <- function(types) {
  call("declare", as.call(c(as.name("type"), stats::setNames(types, names(types)))))
}

make_declared_fn <- function(params, types, expr) {
  formals <- as.pairlist(stats::setNames(vector("list", length(params)), params))
  body <- as.call(c(as.name("{"), list(make_decl_call(types), expr)))
  eval(call("function", formals, body), envir = baseenv())
}

make_ref_fn <- function(params, expr) {
  formals <- as.pairlist(stats::setNames(vector("list", length(params)), params))
  body <- as.call(c(as.name("{"), list(expr)))
  eval(call("function", formals, body), envir = baseenv())
}

rand_numeric_leaf <- function() {
  sample(list(quote(x), quote(y), 1, -1, 2), size = 1L)[[1L]]
}

gen_numeric_expr <- function(depth) {
  if (depth <= 0L || stats::runif(1) < 0.35) {
    return(rand_numeric_leaf())
  }

  choice <- sample(c("binary", "unary", "call1"), size = 1L)
  if (identical(choice, "binary")) {
    op <- sample(c("+", "-", "*", "/"), size = 1L)
    return(as.call(list(as.name(op), gen_numeric_expr(depth - 1L), gen_numeric_expr(depth - 1L))))
  }
  if (identical(choice, "unary")) {
    return(as.call(list(as.name("-"), gen_numeric_expr(depth - 1L))))
  }
  fun <- sample(c("sin", "cos", "abs"), size = 1L)
  as.call(list(as.name(fun), gen_numeric_expr(depth - 1L)))
}

gen_logical_expr <- function(depth) {
  if (depth <= 0L || stats::runif(1) < 0.45) {
    op <- sample(c("<", "<=", ">", ">=", "==", "!="), size = 1L)
    return(as.call(list(as.name(op), gen_numeric_expr(1L), gen_numeric_expr(1L))))
  }

  choice <- sample(c("binary", "not"), size = 1L)
  if (identical(choice, "not")) {
    return(as.call(list(as.name("!"), gen_logical_expr(depth - 1L))))
  }
  op <- sample(c("&", "|"), size = 1L)
  as.call(list(as.name(op), gen_logical_expr(depth - 1L), gen_logical_expr(depth - 1L)))
}

make_input_sets <- function(n_sets = 5L) {
  out <- vector("list", n_sets)
  for (i in seq_len(n_sets)) {
    n <- sample(1:8, size = 1L)
    x <- stats::runif(n, min = -2, max = 2)
    y <- stats::runif(n, min = -2, max = 2)
    if (stats::runif(1) < 0.4) {
      x[[sample.int(n, 1L)]] <- sample(c(NA_real_, NaN), size = 1L)
    }
    if (stats::runif(1) < 0.4) {
      y[[sample.int(n, 1L)]] <- sample(c(NA_real_, NaN), size = 1L)
    }
    out[[i]] <- list(x = x, y = y)
  }
  out
}

set.seed(20260423)
input_sets <- make_input_sets(6L)
backends <- list(
  tinycc = tccq_backend_tinycc(),
  shlib = tccq_backend_shlib()
)

numeric_reducers <- c("sum", "prod", "min", "max", "mean")
logical_reducers <- c("any", "all")
reduce_aliases <- list(
  sum = as.name("+"),
  prod = as.name("*"),
  any = as.name("|"),
  all = as.name("&")
)

cases <- list()
for (i in seq_len(4L)) {
  reducer <- sample(numeric_reducers, size = 1L)
  expr <- as.call(list(as.name(reducer), gen_numeric_expr(2L)))
  cases[[length(cases) + 1L]] <- list(
    name = paste0("numeric_", reducer, "_", i),
    params = c("x", "y"),
    types = list(x = quote(double(NA)), y = quote(double(NA))),
    expr = expr
  )
}
for (i in seq_len(3L)) {
  reducer <- sample(logical_reducers, size = 1L)
  expr <- as.call(list(as.name(reducer), gen_logical_expr(2L)))
  cases[[length(cases) + 1L]] <- list(
    name = paste0("logical_", reducer, "_", i),
    params = c("x", "y"),
    types = list(x = quote(double(NA)), y = quote(double(NA))),
    expr = expr
  )
}
for (nm in names(reduce_aliases)) {
  expr <- if (nm %in% c("sum", "prod")) {
    as.call(list(as.name("Reduce"), reduce_aliases[[nm]], gen_numeric_expr(2L)))
  } else {
    as.call(list(as.name("Reduce"), reduce_aliases[[nm]], gen_logical_expr(2L)))
  }
  cases[[length(cases) + 1L]] <- list(
    name = paste0("reduce_", nm),
    params = c("x", "y"),
    types = list(x = quote(double(NA)), y = quote(double(NA))),
    expr = expr
  )
}

for (case in cases) {
  compiled <- lapply(backends, function(backend) {
    tccq_compile(
      make_declared_fn(case$params, case$types, case$expr),
      backend = backend
    )
  })
  ref <- make_ref_fn(case$params, case$expr)

  for (inputs in input_sets) {
    args <- inputs[case$params]
    expected <- do.call(ref, args)
    for (nm in names(compiled)) {
      got <- do.call(compiled[[nm]], args)
      if (is.logical(expected)) {
        expect_equal(got, expected)
      } else {
        expect_equal(got, expected, tolerance = 1e-10)
      }
    }
    expect_equal(
      do.call(compiled[["tinycc"]], args),
      do.call(compiled[["shlib"]], args),
      tolerance = 1e-10
    )
  }
}
