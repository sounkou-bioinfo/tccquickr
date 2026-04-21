# test_tccq_views_storage.R

slice_view_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  y
}

mod_view <- tccq_compile(slice_view_fn, mode = "ir")
expect_equal(mod_view$ir$stmts[[1L]]$tag, "bind")
expect_equal(mod_view$ir$stmts[[1L]]$value$tag, "view1")
expect_true("y" %in% mod_view$storage_plan$views)
expect_equal(mod_view$storage_plan$aliases$y$kind, "view")

compiled_view <- tccq_compile(slice_view_fn)
expect_equal(compiled_view(c(1, 2, 3, 4), 2L, 4L), c(2, 3, 4))

slice_sum_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  sum(y)
}

src_slice_sum <- tccq_compile(slice_sum_fn, mode = "code")
expect_equal(sum(gregexpr("Rf_allocVector", src_slice_sum, fixed = TRUE)[[1L]] > 0L), 1L)
expect_equal(tccq_compile(slice_sum_fn)(c(1, 2, 3, 4), 2L, 4L), 9)

alias_then_patch_fn <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

mod_alias <- tccq_compile(alias_then_patch_fn, mode = "ir")
expect_equal(mod_alias$storage_plan$aliases$y$kind, "alias")
expect_equal(mod_alias$storage_plan$aliases$y$source, "x")
expect_false(isTRUE(mod_alias$storage_plan$direct_return))

src_alias_patch <- tccq_compile(alias_then_patch_fn, mode = "code")
expect_equal(sum(gregexpr("Rf_allocVector", src_alias_patch, fixed = TRUE)[[1L]] > 0L), 1L)
expect_equal(tccq_compile(alias_then_patch_fn)(c(1, 2, 3), 2L, 10), c(1, 10, 3))

expect_true(grepl("if \\(!own_y\\)", src_alias_patch))
expect_equal(length(gregexpr("if \\(!own_y\\)", src_alias_patch)[[1L]]), 2L)

alias_then_return_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  y
}

src_alias_return <- tccq_compile(alias_then_return_fn, mode = "code")
expect_equal(sum(gregexpr("Rf_allocVector", src_alias_return, fixed = TRUE)[[1L]] > 0L), 1L)
