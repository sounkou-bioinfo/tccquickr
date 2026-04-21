# test_tccq2_assign_slice.R

assignment_block_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  sum(y)
}

assignment_block_src <- tccq2_compile(assignment_block_fn, mode = "code")
expect_true(grepl("for \\(R_xlen_t i", assignment_block_src))
expect_true(grepl("acc \\+=", assignment_block_src))
expect_true(grepl("loc_y", assignment_block_src, fixed = TRUE))

scalar_index_read_fn <- function(x, i) {
  declare(type(x = double(NA)), type(i = integer()))
  x[i] + 1
}

scalar_index_read_src <- tccq2_compile(scalar_index_read_fn, mode = "code")
expect_true(grepl("tccq2_checked_index1", scalar_index_read_src, fixed = TRUE))
expect_true(grepl("index out of bounds", scalar_index_read_src, fixed = TRUE))

range_slice_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  x[lo:hi]
}

range_slice_src <- tccq2_compile(range_slice_fn, mode = "code")
expect_true(grepl("decreasing slices", range_slice_src, fixed = TRUE))
expect_true(grepl("Rf_allocVector", range_slice_src, fixed = TRUE))

indexed_assignment_fn <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

indexed_assignment_src <- tccq2_compile(indexed_assignment_fn, mode = "code")
expect_true(grepl("loc_y", indexed_assignment_src, fixed = TRUE))
expect_true(grepl("p_y\\[j_y\\]", indexed_assignment_src))

range_assignment_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x
  y[2:3] <- v
  y
}

range_assignment_src <- tccq2_compile(range_assignment_fn, mode = "code")
expect_true(grepl("n_rng_y", range_assignment_src, fixed = TRUE))
expect_true(grepl("indexed assignment", range_assignment_src, fixed = TRUE))

direct_formal_mutation_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  x[1] <- v
  x
}

expect_error(
  tccq2_compile(direct_formal_mutation_fn, mode = "code"),
  pattern = "requires a local vector binding"
)

if (requireNamespace("Rtinycc", quietly = TRUE)) {
  compiled_indexed_assignment <- tccq2_compile(indexed_assignment_fn, backend = tccquickr:::tccq2_backend_tinycc())
  expect_equal(compiled_indexed_assignment(c(1, 2, 3), 2L, 10), c(1, 10, 3))

  slice_sum_fn <- function(x, lo, hi) {
    declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
    sum(x[lo:hi])
  }

  compiled_slice_sum <- tccq2_compile(slice_sum_fn, backend = tccquickr:::tccq2_backend_tinycc())
  expect_equal(compiled_slice_sum(c(1, 2, 3, 4), 2L, 4L), 9)
}
