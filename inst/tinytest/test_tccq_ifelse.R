# Vectorized ifelse(): elementwise select with NA propagation (unlike scalar if).

# Vector test, scalar branches (broadcast), differential vs R incl. NA in test.
f <- function(x) {
  declare(type(x = double(NA)))
  ifelse(x > 0, 1, -1)
}
cf <- tccq_compile(f)
x <- c(-2, 0, 3, NA, 1.5, -0.0)
expect_identical(cf(x), f(x))

# Vector test and vector branches.
g <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  ifelse(x > y, x, y)
}
cg <- tccq_compile(g)
a <- c(1, 5, NA, 3, 2)
b <- c(4, 2, 7, NA, 2)
expect_identical(cg(a, b), g(a, b))

# Integer result type is preserved.
gi <- function(x) {
  declare(type(x = integer(NA)))
  ifelse(x > 0L, x, 0L)
}
cgi <- tccq_compile(gi)
xi <- c(-1L, 2L, NA_integer_, 5L)
expect_identical(cgi(xi), gi(xi))

# Scalar ifelse propagates NA (does NOT error, unlike scalar if()).
h <- function(t) {
  declare(type(t = double()))
  ifelse(t > 0, 10, 20)
}
ch <- tccq_compile(h)
# Deliberate, documented deviation: R's ifelse() returns a *logical* NA in the
# degenerate all-NA case (it initialises the result from `test` and never
# overwrites that position). A statically typed compiler returns NA of the result
# type instead; we assert NA-ness, not R's runtime-dependent mode quirk.
expect_true(is.na(ch(NA_real_)))
expect_identical(ch(3), h(3))               # 10
expect_identical(ch(-3), h(-3))             # 20

# Larger random differential check.
set.seed(7)
k <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  ifelse(x > y, x * 2, y - 1)
}
ck <- tccq_compile(k)
for (i in 1:20) {
  n <- sample(1:8, 1)
  xx <- stats::runif(n, -3, 3)
  yy <- stats::runif(n, -3, 3)
  expect_equal(ck(xx, yy), k(xx, yy), info = i)
}

# Same-type-branch contract (mismatch is rejected).
expect_error(
  tccq_compile(function(x) { declare(type(x = double(NA))); ifelse(x > 0, 1L, 2.5) }, mode = "code"),
  "same type"
)
