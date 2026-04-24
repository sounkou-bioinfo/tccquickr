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
expect_identical(mod_view$storage_plan$bindings$y$domain_id, mod_view$shape_facts$by_name$y)
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
expect_identical(mod_alias$storage_plan$bindings$y$domain_id, mod_alias$shape_facts$by_name$x)
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
expect_identical(mod_alias_return$storage_plan$bindings$y$domain_id, mod_alias_return$shape_facts$by_name$x)
expect_true(isTRUE(mod_alias_return$storage_plan$bindings$y$materialize_on_return))
expect_identical(mod_alias_return$storage_plan$result$strategy, "copy_on_return")

owned_view_then_patch_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  z <- y[1:2]
  y[1] <- v
  z
}

mod_owned_view_then_patch <- tccq_compile(owned_view_then_patch_fn, mode = "ir")
owned_barrier <- mod_owned_view_then_patch$storage_plan$write_barriers[["3"]]
expect_true("z" %in% owned_barrier$materialize_views)
expect_equal(tccq_compile(owned_view_then_patch_fn)(as.double(c(10, 20, 30)), 99), c(10, 20))
expect_equal(
  tccq_compile(owned_view_then_patch_fn, backend = tccq_backend_shlib())(as.double(c(10, 20, 30)), 99),
  c(10, 20)
)

alias_view_then_patch_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x
  z <- y[1:2]
  y[1] <- v
  z
}

mod_alias_view_then_patch <- tccq_compile(alias_view_then_patch_fn, mode = "ir")
alias_barrier <- mod_alias_view_then_patch$storage_plan$write_barriers[["3"]]
expect_false("z" %in% alias_barrier$materialize_views)
expect_equal(tccq_compile(alias_view_then_patch_fn)(as.double(c(10, 20, 30)), 99), c(10, 20))

view_after_cow_then_patch_fn <- function(x, v, w) {
  declare(type(x = double(NA)), type(v = double()), type(w = double()))
  y <- x
  y[1] <- v
  z <- y[1:2]
  y[2] <- w
  z
}

mod_view_after_cow_then_patch <- tccq_compile(view_after_cow_then_patch_fn, mode = "ir")
second_barrier <- mod_view_after_cow_then_patch$storage_plan$write_barriers[["4"]]
expect_true("z" %in% second_barrier$materialize_views)
expect_equal(tccq_compile(view_after_cow_then_patch_fn)(as.double(c(10, 20, 30)), 99, 77), c(99, 20))

owned_alias_then_patch_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  z <- y
  y[1] <- v
  z
}

mod_owned_alias_then_patch <- tccq_compile(owned_alias_then_patch_fn, mode = "ir")
owned_alias_barrier <- mod_owned_alias_then_patch$storage_plan$write_barriers[["3"]]
expect_true("z" %in% owned_alias_barrier$materialize_views)
expect_equal(tccq_compile(owned_alias_then_patch_fn)(as.double(c(10, 20, 30)), 99), c(10, 20, 30))

alias_of_view_then_patch_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  z <- y[1:2]
  w <- z
  y[1] <- v
  w
}

mod_alias_of_view_then_patch <- tccq_compile(alias_of_view_then_patch_fn, mode = "ir")
alias_of_view_barrier <- mod_alias_of_view_then_patch$storage_plan$write_barriers[["4"]]
expect_true("w" %in% alias_of_view_barrier$materialize_views)
expect_false("z" %in% alias_of_view_barrier$materialize_views)
expect_equal(tccq_compile(alias_of_view_then_patch_fn)(as.double(c(10, 20, 30)), 99), c(10, 20))

nested_stale_borrow_chain_fn <- function(x, v, q) {
  declare(type(x = double(NA)), type(v = double()), type(q = double()))
  y <- x + 0
  z <- y
  w <- z[1:3]
  z[1] <- v
  u <- w[1:2]
  y[2] <- q
  u
}

mod_nested_stale_borrow_chain <- tccq_compile(nested_stale_borrow_chain_fn, mode = "ir")
nested_borrow_barrier <- mod_nested_stale_borrow_chain$storage_plan$write_barriers[["6"]]
expect_true("u" %in% nested_borrow_barrier$materialize_views)
expect_equal(
  tccq_compile(nested_stale_borrow_chain_fn)(as.double(c(10, 20, 30, 40)), 99, 77),
  c(10, 20)
)
expect_equal(
  tccq_compile(nested_stale_borrow_chain_fn, backend = tccq_backend_shlib())(as.double(c(10, 20, 30, 40)), 99, 77),
  c(10, 20)
)

nested_direct_view_over_alias_fn <- function(x) {
  declare(type(x = double(NA)))
  a <- x
  y <- a[2:5][2:3]
  y
}

expect_equal(tccq_compile(nested_direct_view_over_alias_fn)(as.double(1:6)), c(3, 4))
expect_equal(
  tccq_compile(nested_direct_view_over_alias_fn, backend = tccq_backend_shlib())(as.double(1:6)),
  c(3, 4)
)

nested_direct_view_over_view_fn <- function(x) {
  declare(type(x = double(NA)))
  a <- x[1:5]
  y <- a[2:3][1:2]
  y
}

expect_equal(tccq_compile(nested_direct_view_over_view_fn)(as.double(1:6)), c(2, 3))
expect_equal(
  tccq_compile(nested_direct_view_over_view_fn, backend = tccq_backend_shlib())(as.double(1:6)),
  c(2, 3)
)

view_rejects_vector_bounds_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  y
}

expect_error(tccq_compile(view_rejects_vector_bounds_fn)(as.double(1:5), 1:2, 3:4), "runtime length")
expect_error(
  tccq_compile(view_rejects_vector_bounds_fn, backend = tccq_backend_shlib())(as.double(1:5), 1:2, 3:4),
  "runtime length"
)

snapshot_view_rejects_vector_bounds_fn <- function(x, lo, hi, v) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()), type(v = double()))
  y <- x + 0
  z <- y[lo:hi]
  y[1] <- v
  z
}

expect_error(
  tccq_compile(snapshot_view_rejects_vector_bounds_fn)(as.double(1:5), 1:2, 3:4, 99),
  "runtime length"
)
expect_error(
  tccq_compile(snapshot_view_rejects_vector_bounds_fn, backend = tccq_backend_shlib())(as.double(1:5), 1:2, 3:4, 99),
  "runtime length"
)

materialized_snapshot_alias_fn <- function(x, v, w) {
  declare(type(x = double(NA)), type(v = double()), type(w = double()))
  y <- x + 0
  z <- y[1:3]
  y[1] <- v
  u <- z
  z[1] <- w
  u
}

mod_materialized_snapshot_alias <- tccq_compile(materialized_snapshot_alias_fn, mode = "ir")
snapshot_first_barrier <- mod_materialized_snapshot_alias$storage_plan$write_barriers[["3"]]
snapshot_second_barrier <- mod_materialized_snapshot_alias$storage_plan$write_barriers[["5"]]
expect_true("z" %in% snapshot_first_barrier$materialize_views)
expect_true("u" %in% snapshot_second_barrier$materialize_views)
expect_equal(
  tccq_compile(materialized_snapshot_alias_fn)(as.double(c(10, 20, 30, 40)), 99, 77),
  c(10, 20, 30)
)
expect_equal(
  tccq_compile(materialized_snapshot_alias_fn, backend = tccq_backend_shlib())(as.double(c(10, 20, 30, 40)), 99, 77),
  c(10, 20, 30)
)

self_materialized_alias_then_old_source_write_fn <- function(x, v, q, r) {
  declare(type(x = double(NA), v = double(), q = double(), r = double()))
  a <- x + 0
  y <- a
  y[1] <- v
  u <- y
  a[2] <- q
  t <- y[1]
  y[2] <- r
  u
}

mod_self_materialized_alias <- tccq_compile(self_materialized_alias_then_old_source_write_fn, mode = "ir")
self_materialized_barrier <- mod_self_materialized_alias$storage_plan$write_barriers[["7"]]
expect_true("u" %in% self_materialized_barrier$materialize_views)
expect_equal(
  tccq_compile(self_materialized_alias_then_old_source_write_fn)(as.double(c(10, 20, 30)), 99, 77, 55),
  c(99, 20, 30)
)
expect_equal(
  tccq_compile(self_materialized_alias_then_old_source_write_fn, backend = tccq_backend_shlib())(as.double(c(10, 20, 30)), 99, 77, 55),
  c(99, 20, 30)
)

range_store_rhs_view_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 0
  z <- y[1:2]
  y[1:2] <- z[1]
  y
}

mod_range_store_rhs_view <- tccq_compile(range_store_rhs_view_fn, mode = "ir")
range_barrier <- mod_range_store_rhs_view$storage_plan$write_barriers[["3"]]
expect_true("z" %in% range_barrier$materialize_views)
expect_equal(tccq_compile(range_store_rhs_view_fn)(as.double(c(10, 20, 30))), c(10, 10, 30))

range_store_composite_rhs_snapshot_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 0
  z <- y[1:2]
  y[1:2] <- y[1] + z[1]
  y
}

mod_range_store_composite_rhs_snapshot <- tccq_compile(range_store_composite_rhs_snapshot_fn, mode = "ir")
composite_range_barrier <- mod_range_store_composite_rhs_snapshot$storage_plan$write_barriers[["3"]]
expect_true("z" %in% composite_range_barrier$materialize_views)
expect_equal(tccq_compile(range_store_composite_rhs_snapshot_fn)(as.double(c(10, 20, 30))), c(20, 20, 30))
expect_equal(
  tccq_compile(range_store_composite_rhs_snapshot_fn, backend = tccq_backend_shlib())(as.double(c(10, 20, 30))),
  c(20, 20, 30)
)
