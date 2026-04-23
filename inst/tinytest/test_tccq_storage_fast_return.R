# test_tccq_storage_fast_return.R

patch_then_return <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

mod <- tccq_compile(patch_then_return, mode = "ir")
expect_identical(mod$storage_plan$result$strategy, "copy_on_write_return_local")
expect_true(isTRUE(mod$storage_plan$bindings$y$materialize_on_write))
expect_false(isTRUE(mod$storage_plan$bindings$y$materialize_on_return))

compiled_patch_then_return <- tccq_compile(patch_then_return)
expect_equal(compiled_patch_then_return(c(1, 2, 3), 2L, 10), c(1, 10, 3))
