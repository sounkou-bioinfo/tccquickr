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
expect_equal(compiled_index_read(c(1, 2, 3), NA_integer_), NA_real_)

scalar_index_bind_read_fn <- function(x, i) {
  declare(type(x = double(NA)), type(i = integer()))
  y <- x[i]
  y
}

expect_equal(tccq_compile(scalar_index_bind_read_fn)(c(1, 2, 3), NA_integer_), NA_real_)

nested_index_read_na_fn <- function(x, lo, hi, i) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()), type(i = integer()))
  x[lo:hi][i]
}

expect_equal(tccq_compile(nested_index_read_na_fn)(as.double(1:5), 2L, 4L, NA_integer_), NA_real_)

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

compiled_indexed_assignment <- tccq_compile(indexed_assignment_fn, backend = tccq_backend_tinycc())
expect_equal(compiled_indexed_assignment(c(1, 2, 3), 2L, 10), c(1, 10, 3))

nested_index_assignment_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[2:4][2] <- v
  y
}

nested_index_assignment_mod <- tccq_compile(nested_index_assignment_fn, mode = "ir")
expect_identical(nested_index_assignment_mod$ir$stmts[[2L]]$tag, "store_access")
expect_identical(length(nested_index_assignment_mod$ir$stmts[[2L]]$subscripts), 0L)
expect_false(is.null(nested_index_assignment_mod$ir$stmts[[2L]]$access))
expect_equal(tccq_compile(nested_index_assignment_fn)(as.double(1:5), 99), c(1, 2, 99, 4, 5))
expect_equal(
  tccq_compile(nested_index_assignment_fn, backend = tccq_backend_shlib())(as.double(1:5), 99),
  c(1, 2, 99, 4, 5)
)

nested_range_assignment_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[2:5][2:3] <- v
  y
}

nested_range_assignment_mod <- tccq_compile(nested_range_assignment_fn, mode = "ir")
expect_identical(nested_range_assignment_mod$ir$stmts[[2L]]$tag, "store_access")
expect_identical(length(nested_range_assignment_mod$ir$stmts[[2L]]$subscripts), 0L)
expect_false(is.null(nested_range_assignment_mod$ir$stmts[[2L]]$access))
expect_equal(tccq_compile(nested_range_assignment_fn)(as.double(1:6), 99), c(1, 2, 99, 99, 5, 6))
expect_equal(
  tccq_compile(nested_range_assignment_fn, backend = tccq_backend_shlib())(as.double(1:6), 99),
  c(1, 2, 99, 99, 5, 6)
)

nested_index_assignment_na_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[2:5][NA_integer_] <- v
  y
}

expect_equal(tccq_compile(nested_index_assignment_na_fn)(as.double(1:6), 99), as.double(1:6))

nested_all_assignment_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[2:4][] <- v
  y
}

expect_equal(tccq_compile(nested_all_assignment_fn)(as.double(1:6), 99), c(1, 99, 99, 99, 5, 6))

assign("tccq_test_assign_order_log", character(), envir = .GlobalEnv)
assign(
  "tccq_test_assign_order_i",
  function(i) {
    assign("tccq_test_assign_order_log", c(get("tccq_test_assign_order_log", envir = .GlobalEnv), "i"), envir = .GlobalEnv)
    i
  },
  envir = .GlobalEnv
)
assign(
  "tccq_test_assign_order_v",
  function(v) {
    assign("tccq_test_assign_order_log", c(get("tccq_test_assign_order_log", envir = .GlobalEnv), "v"), envir = .GlobalEnv)
    v
  },
  envir = .GlobalEnv
)

assignment_eval_order_fn <- function(i, x, v) {
  declare(type(i = integer()), type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[tccq_test_assign_order_i(i)] <- tccq_test_assign_order_v(v)
  y
}

assign("tccq_test_assign_order_log", character(), envir = .GlobalEnv)
expect_equal(
  tccq_compile(assignment_eval_order_fn, fallback = "auto")(2L, as.double(1:4), 99),
  c(1, 99, 3, 4)
)
expect_identical(get("tccq_test_assign_order_log", envir = .GlobalEnv), c("v", "i"))
assign("tccq_test_assign_order_log", character(), envir = .GlobalEnv)
expect_equal(
  tccq_compile(assignment_eval_order_fn, fallback = "auto", backend = tccq_backend_shlib())(2L, as.double(1:4), 99),
  c(1, 99, 3, 4)
)
expect_identical(get("tccq_test_assign_order_log", envir = .GlobalEnv), c("v", "i"))

nested_all_eval_order_fn <- function(lo, hi, x, v) {
  declare(type(lo = integer()), type(hi = integer()), type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[tccq_test_assign_order_i(lo):hi][] <- tccq_test_assign_order_v(v)
  y
}

assign("tccq_test_assign_order_log", character(), envir = .GlobalEnv)
expect_equal(
  tccq_compile(nested_all_eval_order_fn, fallback = "auto")(2L, 4L, as.double(1:6), 99),
  c(1, 99, 99, 99, 5, 6)
)
expect_identical(get("tccq_test_assign_order_log", envir = .GlobalEnv), c("v", "i"))

assign("tccq_test_store_access_bump_count", 0L, envir = .GlobalEnv)
assign(
  "tccq_test_store_access_bump",
  function(i) {
    assign(
      "tccq_test_store_access_bump_count",
      get("tccq_test_store_access_bump_count", envir = .GlobalEnv) + 1L,
      envir = .GlobalEnv
    )
    i
  },
  envir = .GlobalEnv
)

nested_boundary_final_index_once_fn <- function(i, x, v) {
  declare(type(i = integer()), type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[2:5][tccq_test_store_access_bump(i)] <- v
  y
}

assign("tccq_test_store_access_bump_count", 0L, envir = .GlobalEnv)
expect_equal(
  tccq_compile(nested_boundary_final_index_once_fn, fallback = "auto")(2L, as.double(1:6), 99),
  c(1, 2, 99, 4, 5, 6)
)
expect_identical(get("tccq_test_store_access_bump_count", envir = .GlobalEnv), 1L)
assign("tccq_test_store_access_bump_count", 0L, envir = .GlobalEnv)
expect_equal(
  tccq_compile(nested_boundary_final_index_once_fn, fallback = "auto", backend = tccq_backend_shlib())(2L, as.double(1:6), 99),
  c(1, 2, 99, 4, 5, 6)
)
expect_identical(get("tccq_test_store_access_bump_count", envir = .GlobalEnv), 1L)

nested_boundary_final_range_once_fn <- function(lo, hi, x, v) {
  declare(type(lo = integer()), type(hi = integer()), type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[2:6][tccq_test_store_access_bump(lo):hi] <- v
  y
}

assign("tccq_test_store_access_bump_count", 0L, envir = .GlobalEnv)
expect_equal(
  tccq_compile(nested_boundary_final_range_once_fn, fallback = "auto")(2L, 3L, as.double(1:7), 99),
  c(1, 2, 99, 99, 5, 6, 7)
)
expect_identical(get("tccq_test_store_access_bump_count", envir = .GlobalEnv), 1L)
assign("tccq_test_store_access_bump_count", 0L, envir = .GlobalEnv)
expect_equal(
  tccq_compile(nested_boundary_final_range_once_fn, fallback = "auto", backend = tccq_backend_shlib())(2L, 3L, as.double(1:7), 99),
  c(1, 2, 99, 99, 5, 6, 7)
)
expect_identical(get("tccq_test_store_access_bump_count", envir = .GlobalEnv), 1L)

nested_assignment_materializes_stale_view_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  z <- y[2:4]
  y[2:4][2] <- v
  z
}

nested_assignment_materializes_stale_view_mod <- tccq_compile(
  nested_assignment_materializes_stale_view_fn,
  mode = "ir"
)
nested_assignment_barrier <- nested_assignment_materializes_stale_view_mod$storage_plan$write_barriers[["3"]]
expect_true("z" %in% nested_assignment_barrier$materialize_views)
expect_equal(
  tccq_compile(nested_assignment_materializes_stale_view_fn)(as.double(1:5), 99),
  c(2, 3, 4)
)

nested_assignment_oob_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[2:4][4] <- v
  y
}

expect_error(
  tccq_compile(nested_assignment_oob_fn)(as.double(1:5), 99),
  pattern = "index out of bounds"
)

nested_assignment_reuse_blocker_fn <- function(idx, x, v) {
  declare(type(idx = integer(NA)), type(x = double(NA)), type(v = double()))
  a <- idx + 0L
  y <- x + 0
  b <- a + 1L
  y[a[1]:a[2]][1] <- v
  y
}

nested_assignment_reuse_blocker_mod <- tccq_compile(nested_assignment_reuse_blocker_fn, mode = "ir")
reuse_b <- nested_assignment_reuse_blocker_mod$alloc_plan$reuse$bindings$b
expect_false(!is.null(reuse_b) && identical(reuse_b$name, "a"))
expect_equal(
  tccq_compile(nested_assignment_reuse_blocker_fn)(c(2L, 4L), as.double(1:6), 99),
  c(1, 99, 3, 4, 5, 6)
)

direct_formal_nested_mutation_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  x[2:4][2] <- v
  x
}

expect_error(
  tccq_compile(direct_formal_nested_mutation_fn, mode = "code"),
  pattern = "requires a local vector binding"
)

slice_sum_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  sum(x[lo:hi])
}

compiled_slice_sum <- tccq_compile(slice_sum_fn, backend = tccq_backend_tinycc())
expect_equal(compiled_slice_sum(c(1, 2, 3, 4), 2L, 4L), 9)
