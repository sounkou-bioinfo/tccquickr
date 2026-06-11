# Scalar if/else expression: differential against R, plus the rejected edges.

# Double branches, differential vs R across random inputs.
f <- function(x, y) {
  declare(type(x = double(), y = double()))
  if (x > y) x * 2 else y + 1
}
cf <- tccq_compile(f)
expect_equal(cf(3, 1), 6)
expect_equal(cf(1, 3), 4)
expect_equal(cf(2, 2), 3)
set.seed(11)
for (i in 1:25) {
  a <- stats::runif(1, -5, 5)
  b <- stats::runif(1, -5, 5)
  expect_equal(cf(a, b), f(a, b), info = paste(a, b))
}

# Integer branches keep integer type.
g <- function(n) {
  declare(type(n = integer()))
  if (n > 0L) n else -n
}
cg <- tccq_compile(g)
expect_identical(cg(5L), 5L)
expect_identical(cg(-3L), 3L)

# An NA condition errors exactly as R's if() does.
expect_error(cf(NA_real_, 1), "missing value where TRUE/FALSE needed")

# Nested if/else.
h <- function(x) {
  declare(type(x = double()))
  if (x > 0) 1 else if (x < 0) -1 else 0
}
ch <- tccq_compile(h)
expect_equal(ch(5), 1)
expect_equal(ch(-5), -1)
expect_equal(ch(0), 0)

# Rejected edges (current scalar-only, same-type, both-branch contract).
expect_error(
  tccq_compile(function(x) { declare(type(x = double(NA))); if (x[1] > 0) x else x }, mode = "code"),
  "scalar"
)
expect_error(
  tccq_compile(function(x) { declare(type(x = double())); if (x) x else 1 }, mode = "code"),
  "logical"
)
expect_error(
  tccq_compile(function(x) { declare(type(x = double())); if (x > 0) x }, mode = "code"),
  "both branches"
)
expect_error(
  tccq_compile(function(n) { declare(type(n = integer())); if (n > 0L) n else 1.5 }, mode = "code"),
  "same type"
)
