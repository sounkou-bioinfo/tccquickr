# test_tccq_views_storage.R

slice_view_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  y
}

mod_view <- tccq_compile(slice_view_fn, mode = "ir")
expect_equal(mod_view$ir$stmts[[1L]]$tag, "bind")
expect_equal(mod_view$ir$stmts[[1L]]$value$tag, "view1")
expect_equal(mod_view$storage_plan$bindings$y$kind, "view")
expect_identical(mod_view$storage_plan$bindings$y$source, "x")
expect_true(isTRUE(mod_view$storage_plan$bindings$y$materialize_on_return))
expect_identical(mod_view$storage_plan$result$strategy, "copy_on_return")
expect_equal(tccq_compile(slice_view_fn)(c(1, 2, 3, 4), 2L, 4L), c(2, 3, 4))

slice_sum_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  sum(y)
}

mod_slice_sum <- tccq_compile(slice_sum_fn, mode = "ir")
expect_equal(mod_slice_sum$storage_plan$bindings$y$kind, "view")
expect_identical(mod_slice_sum$storage_plan$result$strategy, "box_scalar")
expect_equal(tccq_compile(slice_sum_fn)(c(1, 2, 3, 4), 2L, 4L), 9)

alias_then_patch_fn <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

mod_alias <- tccq_compile(alias_then_patch_fn, mode = "ir")
expect_equal(mod_alias$storage_plan$bindings$y$kind, "alias")
expect_equal(mod_alias$storage_plan$bindings$y$source, "x")
expect_true(isTRUE(mod_alias$storage_plan$bindings$y$materialize_on_write))
expect_false(isTRUE(mod_alias$storage_plan$bindings$y$materialize_on_return))
expect_identical(mod_alias$storage_plan$result$strategy, "copy_on_write_return_local")
expect_equal(tccq_compile(alias_then_patch_fn)(c(1, 2, 3), 2L, 10), c(1, 10, 3))

alias_then_return_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  y
}

mod_alias_return <- tccq_compile(alias_then_return_fn, mode = "ir")
expect_equal(mod_alias_return$storage_plan$bindings$y$kind, "alias")
expect_true(isTRUE(mod_alias_return$storage_plan$bindings$y$materialize_on_return))
expect_identical(mod_alias_return$storage_plan$result$strategy, "copy_on_return")
