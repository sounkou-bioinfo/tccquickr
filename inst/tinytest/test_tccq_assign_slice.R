# test_tccq_assign_slice.R

assignment_block_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  sum(y)
}

assignment_block_mod <- tccq_compile(assignment_block_fn, mode = "ir")
expect_equal(assignment_block_mod$storage_plan$bindings$y$kind, "owned")
expect_identical(assignment_block_mod$storage_plan$result$strategy, "box_scalar")

scalar_index_read_fn <- function(x, i) {
  declare(type(x = double(NA)), type(i = integer()))
  x[i] + 1
}

compiled_index_read <- tccq_compile(scalar_index_read_fn)
expect_equal(compiled_index_read(c(1, 2, 3), 2L), 3)

range_slice_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  x[lo:hi]
}

range_slice_mod <- tccq_compile(range_slice_fn, mode = "ir")
expect_identical(range_slice_mod$storage_plan$result$strategy, "materialize_view")
expect_equal(tccq_compile(range_slice_fn)(c(1, 2, 3, 4), 2L, 4L), c(2, 3, 4))

indexed_assignment_fn <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

indexed_assignment_mod <- tccq_compile(indexed_assignment_fn, mode = "ir")
expect_equal(indexed_assignment_mod$storage_plan$bindings$y$kind, "alias")
expect_true(isTRUE(indexed_assignment_mod$storage_plan$bindings$y$materialize_on_write))
expect_identical(indexed_assignment_mod$storage_plan$result$strategy, "copy_on_write_return_local")

range_assignment_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x
  y[2:3] <- v
  y
}

range_assignment_mod <- tccq_compile(range_assignment_fn, mode = "ir")
expect_equal(range_assignment_mod$storage_plan$bindings$y$kind, "alias")
expect_true(isTRUE(range_assignment_mod$storage_plan$bindings$y$materialize_on_write))

direct_formal_mutation_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  x[1] <- v
  x
}

expect_error(
  tccq_compile(direct_formal_mutation_fn, mode = "code"),
  pattern = "requires a local vector binding"
)

direct_formal_range_mutation_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  x[2:3] <- v
  x
}

expect_error(
  tccq_compile(direct_formal_range_mutation_fn, mode = "code"),
  pattern = "requires a local vector binding"
)

rebind_local_name_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  y <- y + 2
  sum(y)
}

expect_error(
  tccq_compile(rebind_local_name_fn, mode = "code"),
  pattern = "rebinding a local name is not yet supported"
)

compiled_indexed_assignment <- tccq_compile(indexed_assignment_fn, backend = tccquickr:::tccq_backend_tinycc())
expect_equal(compiled_indexed_assignment(c(1, 2, 3), 2L, 10), c(1, 10, 3))

slice_sum_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  sum(x[lo:hi])
}

compiled_slice_sum <- tccq_compile(slice_sum_fn, backend = tccquickr:::tccq_backend_tinycc())
expect_equal(compiled_slice_sum(c(1, 2, 3, 4), 2L, 4L), 9)
