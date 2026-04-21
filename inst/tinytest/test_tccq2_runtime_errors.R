# test_tccq2_runtime_errors.R

index_read_fn <- function(x, i) {
  declare(type(x = double(NA)), type(i = integer()))
  x[i] + 1
}
compiled_index_read <- tccq2_compile(index_read_fn)
expect_error(compiled_index_read(c(1, 2, 3), 0L), pattern = "index out of bounds")
expect_error(compiled_index_read(c(1, 2, 3), 4L), pattern = "index out of bounds")

length_mismatch_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum(x + y)
}
compiled_length_mismatch <- tccq2_compile(length_mismatch_fn)
expect_error(
  compiled_length_mismatch(c(1, 2, 3), c(1, 2)),
  pattern = "vector length mismatch"
)

slice_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  x[lo:hi]
}
compiled_slice <- tccq2_compile(slice_fn)
expect_error(compiled_slice(c(1, 2, 3), 3L, 2L), pattern = "decreasing slices")

range_assign_runtime_fn <- function(x, lo, hi, v) {
  declare(
    type(x = double(NA)),
    type(lo = integer()),
    type(hi = integer()),
    type(v = double())
  )
  y <- x
  y[lo:hi] <- v
  y
}
compiled_range_assign <- tccq2_compile(range_assign_runtime_fn)
expect_error(
  compiled_range_assign(c(1, 2, 3), 3L, 2L, 9),
  pattern = "decreasing ranges"
)

integer_scalar_fn <- function(i) {
  declare(type(i = integer()))
  i + 1L
}
compiled_integer_scalar <- tccq2_compile(integer_scalar_fn)
expect_error(compiled_integer_scalar(1.5), pattern = "wrong R type")
expect_error(compiled_integer_scalar(integer()), pattern = "scalar argument")
