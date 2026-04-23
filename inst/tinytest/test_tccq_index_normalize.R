# test_tccq_index_normalize.R

nested_slice_fn <- function(x) {
  declare(type(x = double(NA)))
  x[2:4][1:2]
}

nested_slice_mod <- tccq_compile(nested_slice_fn, mode = "ir")
expect_equal(nested_slice_mod$ir$result$tag, "view1")
expect_identical(nested_slice_mod$ir$result$normalized_access$base_name, "x")
expect_equal(length(nested_slice_mod$ir$result$normalized_access$steps), 2L)
expect_equal(vapply(nested_slice_mod$ir$result$normalized_access$steps, `[[`, character(1), "kind"), c("slice", "slice"))
expect_equal(tccq_compile(nested_slice_fn)(c(1, 2, 3, 4, 5)), c(2, 3))

nested_slice_sum_fn <- function(x) {
  declare(type(x = double(NA)))
  sum(x[2:4][1:2])
}

nested_slice_sum_mod <- tccq_compile(nested_slice_sum_fn, mode = "ir")
expect_identical(nested_slice_sum_mod$kernel$tag, "kernel_program")
expect_identical(nested_slice_sum_mod$kernel$result_kernel$tag, "fold")
expect_equal(tccq_compile(nested_slice_sum_fn)(c(1, 2, 3, 4, 5)), 5)

nested_index_fn <- function(x) {
  declare(type(x = double(NA)))
  x[2:4][2]
}

nested_index_mod <- tccq_compile(nested_index_fn, mode = "ir")
expect_equal(nested_index_mod$ir$result$tag, "index")
expect_identical(nested_index_mod$ir$result$normalized_access$base_name, "x")
expect_equal(vapply(nested_index_mod$ir$result$normalized_access$steps, `[[`, character(1), "kind"), c("slice", "index"))
expect_equal(tccq_compile(nested_index_fn)(c(1, 2, 3, 4, 5)), 3)

nested_index_plus_fn <- function(x) {
  declare(type(x = double(NA)))
  x[2:4][2] + 1
}

nested_index_plus_mod <- tccq_compile(nested_index_plus_fn, mode = "ir")
expect_true(length(nested_index_plus_mod$ir$stmts) >= 1L)
expect_true(any(vapply(nested_index_plus_mod$ir$stmts, function(s) identical(s$tag, "bind") && startsWith(s$name, "tccq_idx_tmp_"), logical(1))))
expect_equal(tccq_compile(nested_index_plus_fn)(c(1, 2, 3, 4, 5)), 4)

nested_slice_sum_plus_fn <- function(x) {
  declare(type(x = double(NA)))
  sum(x[2:4][1:2] + 1)
}

expect_equal(tccq_compile(nested_slice_sum_plus_fn)(c(1, 2, 3, 4, 5)), 7)

nested_index_oob_fn <- function(x) {
  declare(type(x = double(NA)))
  x[2:4][4]
}

expect_error(
  tccq_compile(nested_index_oob_fn)(c(1, 2, 3, 4, 5)),
  pattern = "index out of bounds"
)

nested_view_bind_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x[2:5]
  z <- y[2:3]
  z
}

nested_view_bind_mod <- tccq_compile(nested_view_bind_fn, mode = "ir")
expect_equal(nested_view_bind_mod$storage_plan$bindings$y$kind, "view")
expect_identical(nested_view_bind_mod$storage_plan$bindings$y$source, "x")
expect_equal(nested_view_bind_mod$storage_plan$bindings$z$kind, "view")
expect_identical(nested_view_bind_mod$storage_plan$bindings$z$source, "x")
expect_equal(tccq_compile(nested_view_bind_fn)(c(1, 2, 3, 4, 5, 6)), c(3, 4))

alias_view_bind_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  z <- y[2:3]
  z
}

alias_view_bind_mod <- tccq_compile(alias_view_bind_fn, mode = "ir")
expect_equal(alias_view_bind_mod$storage_plan$bindings$y$kind, "alias")
expect_identical(alias_view_bind_mod$storage_plan$bindings$y$source, "x")
expect_equal(alias_view_bind_mod$storage_plan$bindings$z$kind, "view")
expect_identical(alias_view_bind_mod$storage_plan$bindings$z$source, "x")
expect_equal(tccq_compile(alias_view_bind_fn)(c(1, 2, 3, 4)), c(2, 3))

nested_index_store_rhs_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  y[1] <- x[2:4][2]
  y
}

expect_equal(tccq_compile(nested_index_store_rhs_fn)(c(1, 2, 3, 4, 5)), c(3, 2, 3, 4, 5))

nested_access_index_arg_fn <- function(x, y) {
  declare(type(x = double(NA), y = integer(NA)))
  x[y[2:4][2]]
}

expect_equal(tccq_compile(nested_access_index_arg_fn)(c(10, 20, 30, 40, 50), c(1L, 2L, 3L, 4L, 5L)), 30)

nested_access_slice_start_fn <- function(x, y) {
  declare(type(x = double(NA), y = integer(NA)))
  x[y[2:4][2]:4L]
}

expect_equal(
  tccq_compile(nested_access_slice_start_fn)(c(10, 20, 30, 40, 50), c(1L, 2L, 3L, 4L, 5L)),
  c(30, 40)
)

nested_access_chain_index_fn <- function(x, y) {
  declare(type(x = double(NA), y = integer(NA)))
  x[y[2:4][2]:4L][2]
}

expect_equal(
  tccq_compile(nested_access_chain_index_fn)(c(10, 20, 30, 40, 50), c(1L, 2L, 3L, 4L, 5L)),
  40
)

nested_access_chain_slice_fn <- function(x, y) {
  declare(type(x = double(NA), y = integer(NA)))
  x[y[2:4][2]:5L][1:2]
}

expect_equal(
  tccq_compile(nested_access_chain_slice_fn)(c(10, 20, 30, 40, 50, 60), c(1L, 2L, 3L, 4L, 5L)),
  c(30, 40)
)

temp_name_collision_local_fn <- function(x) {
  declare(type(x = double(NA)))
  tccq_idx_tmp_1 <- 1
  x[2:4][2] + 1
}

expect_equal(tccq_compile(temp_name_collision_local_fn)(c(1, 2, 3, 4, 5)), 4)

temp_name_collision_formal_fn <- function(x, tccq_idx_tmp_1) {
  declare(type(x = double(NA)), type(tccq_idx_tmp_1 = double()))
  x[2:4][2] + 1
}

expect_equal(tccq_compile(temp_name_collision_formal_fn)(c(1, 2, 3, 4, 5), 0), 4)

temp_name_collision_c_ident_local_fn <- function(x) {
  declare(type(x = double(NA)))
  `tccq.idx.tmp.1` <- 1
  x[2:4][2] + 1
}

expect_equal(tccq_compile(temp_name_collision_c_ident_local_fn)(c(1, 2, 3, 4, 5)), 4)

composite_base_slice_fn <- function(x) {
  declare(type(x = double(NA)))
  (x + 1)[1:2]
}

expect_error(
  tccq_compile(composite_base_slice_fn, mode = "code"),
  pattern = "Bind the composite vector expression first"
)

mutated_alias_view_source_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x
  y[1] <- v
  z <- y[2:3]
  z
}

mutated_alias_view_source_mod <- tccq_compile(mutated_alias_view_source_fn, mode = "ir")
expect_identical(mutated_alias_view_source_mod$storage_plan$bindings$z$source, "y")
expect_equal(tccq_compile(mutated_alias_view_source_fn)(c(1, 2, 3, 4), 99), c(2, 3))
