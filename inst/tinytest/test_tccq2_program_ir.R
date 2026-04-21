# test_tccq2_program_ir.R

binding_then_reduce_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  sum(y)
}

binding_then_store_fn <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  sum(y)
}

range_store_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x
  y[2:3] <- v
  y
}

slice_expr_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  x[lo:hi]
}

mod_bind <- tccq2_compile(binding_then_reduce_fn, mode = "ir")
expect_equal(mod_bind$ir$tag, "program")
expect_equal(length(mod_bind$ir$stmts), 1L)
expect_equal(mod_bind$ir$stmts[[1L]]$tag, "bind")
expect_equal(mod_bind$ir$result$tag, "reduce")
expect_equal(mod_bind$kernel$tag, "kernel_program")
expect_equal(mod_bind$kernel$result_kernel$tag, "fold")

mod_store <- tccq2_compile(binding_then_store_fn, mode = "ir")
expect_equal(mod_store$ir$tag, "program")
expect_equal(vapply(mod_store$ir$stmts, `[[`, character(1), "tag"), c("bind", "store_index"))
expect_equal(mod_store$ir$stmts[[2L]]$effect, "write")
expect_equal(mod_store$kernel$tag, "kernel_program")
expect_equal(mod_store$kernel$result_kernel$tag, "fold")

mod_range <- tccq2_compile(range_store_fn, mode = "ir")
expect_equal(vapply(mod_range$ir$stmts, `[[`, character(1), "tag"), c("bind", "store_range"))
expect_equal(mod_range$kernel$tag, "kernel_program")
expect_equal(mod_range$kernel$result_kernel$tag, "materialize")

mod_slice <- tccq2_compile(slice_expr_fn, mode = "ir")
expect_equal(mod_slice$ir$tag, "program")
expect_equal(mod_slice$ir$result$tag, "slice_range")
expect_equal(mod_slice$kernel$tag, "materialize")

if (requireNamespace("Rtinycc", quietly = TRUE)) {
  compiled_reduce_after_store <- tccq2_compile(binding_then_store_fn)
  expect_equal(compiled_reduce_after_store(c(1, 2, 3), 2L, 10), 14)

  compiled_range_store <- tccq2_compile(range_store_fn)
  expect_equal(compiled_range_store(c(1, 2, 3, 4), 9), c(1, 9, 9, 4))
}
