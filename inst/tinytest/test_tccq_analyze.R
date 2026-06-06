# test_tccq_analyze.R

f <- function(x, y = 1) {
  declare(type(x = double(NA), y = double(NA)))
  x + y
}

a <- tccq_analyze(f)

expect_true(inherits(a, "tccq_analysis"))
expect_true(a$ast$has_declare)
expect_true("declare" %in% a$ast$call_names)
expect_true("+" %in% a$ast$call_names)
expect_true("x" %in% a$ast$symbols)
expect_true("y" %in% a$ast$symbols)
expect_true(a$compiler$available)
expect_true(length(a$recommendations) >= 0L)

if (isTRUE(a$compiler$available)) {
  expect_true(length(a$compiler$opcodes) >= 1L)
}

g <- function(x) {
  sum(x)
}

a_g <- tccq_analyze(g)
expect_true("add declare(type(...)) for typed compilation and stronger optimizer visibility" %in% a_g$recommendations)
expect_true("sum" %in% a_g$ast$call_names)

h <- function(x) {
  eval(x)
}

a_h <- tccq_analyze(h)
expect_true("eval" %in% a_h$ast$dynamic_calls)
