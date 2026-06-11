# Integer positional switch(): nested guarded select, differential against R.

f <- function(k, x, y) {
  declare(type(k = integer(), x = double(), y = double()))
  switch(k, x + y, x - y, x * y)
}
cf <- tccq_compile(f)
for (k in 1:3) {
  expect_equal(cf(k, 6, 2), f(k, 6, 2), info = k)
}

# Out-of-range index: a typed numeric return can't be R's NULL, so we error.
expect_error(cf(4L, 6, 2), "switch: index out of range")
expect_error(cf(0L, 6, 2), "switch: index out of range")
expect_error(cf(NA_integer_, 6, 2), "switch: index out of range")

# Integer cases keep integer type.
g <- function(k) {
  declare(type(k = integer()))
  switch(k, 10L, 20L, 30L)
}
cg <- tccq_compile(g)
expect_identical(cg(2L), 20L)

# A single case.
h <- function(k, x) {
  declare(type(k = integer(), x = double()))
  switch(k, x * 2)
}
ch <- tccq_compile(h)
expect_equal(ch(1L, 5), 10)
expect_error(ch(2L, 5), "out of range")

# Rejected edges.
expect_error(
  tccq_compile(function(k, x) { declare(type(k = double(), x = double())); switch(k, x, x) }, mode = "code"),
  "scalar integer"
)
expect_error(
  tccq_compile(function(k, x) { declare(type(k = integer(), x = double(NA))); switch(k, x, x) }, mode = "code"),
  "scalar"
)
expect_error(
  tccq_compile(function(k, x) { declare(type(k = integer(), x = double())); switch(k, x, 1L) }, mode = "code"),
  "same type"
)
