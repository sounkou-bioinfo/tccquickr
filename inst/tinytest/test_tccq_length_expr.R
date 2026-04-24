# test_tccq_length_expr.R

length_slice_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  length(x[lo:hi])
}

expect_equal(tccq_compile(length_slice_fn)(as.double(1:5), 2L, 4L), 3L)
expect_equal(
  tccq_compile(length_slice_fn, backend = tccq_backend_shlib())(as.double(1:5), 2L, 4L),
  3L
)

length_nested_slice_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x[2:5][2:3])
}

mod_length_nested_slice <- tccq_compile(length_nested_slice_fn, mode = "ir")
expect_true(any(vapply(
  mod_length_nested_slice$ir$stmts,
  function(s) identical(s$tag, "bind") && startsWith(s$name, "tccq_"),
  logical(1)
)))
expect_equal(tccq_compile(length_nested_slice_fn)(as.double(1:6)), 2L)

length_composite_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  length(x + y)
}

expect_equal(tccq_compile(length_composite_fn)(as.double(1:4), as.double(11:14)), 4L)
expect_equal(
  tccq_compile(length_composite_fn, backend = tccq_backend_shlib())(as.double(1:4), as.double(11:14)),
  4L
)
expect_error(
  tccq_compile(length_composite_fn)(as.double(1:4), as.double(11:13)),
  pattern = "vector length mismatch"
)

length_slice_plus_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  length(x[lo:hi]) + 1L
}

expect_equal(tccq_compile(length_slice_plus_fn)(as.double(1:5), 2L, 4L), 4L)

length_slice_oob_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x[2:6])
}

expect_error(
  tccq_compile(length_slice_oob_fn)(as.double(1:5)),
  pattern = "index out of bounds"
)

length_scalar_index_expr_fn <- function(x) {
  declare(type(x = double(NA)))
  1L + length(x[2])
}

expect_equal(tccq_compile(length_scalar_index_expr_fn)(as.double(1:5)), 2L)
expect_equal(
  tccq_compile(length_scalar_index_expr_fn, backend = tccq_backend_shlib())(as.double(1:5)),
  2L
)

length_long_vector_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x)
}

length_direct_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x)
}

short_len <- tccq_compile(length_direct_fn)(as.double(1:5))
expect_true(is.integer(short_len))
expect_equal(short_len, 5L)

x_long <- as.double(1:3000000000)
long_len <- tccq_compile(length_long_vector_fn)(x_long)
long_len_shlib <- tccq_compile(length_long_vector_fn, backend = tccq_backend_shlib())(x_long)
expect_true(is.double(long_len))
expect_true(is.double(long_len_shlib))
expect_equal(long_len, 3e9)
expect_equal(long_len_shlib, 3e9)

length_long_plus_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x) + 1L
}

length_long_bind_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- length(x)
  y
}

length_long_bind_plus_fn <- function(x) {
  declare(type(x = double(NA)))
  z <- length(x)
  z + 1L
}

expect_equal(tccq_compile(length_long_plus_fn)(x_long), 3e9 + 1)
expect_equal(tccq_compile(length_long_plus_fn, backend = tccq_backend_shlib())(x_long), 3e9 + 1)
expect_equal(tccq_compile(length_long_bind_fn)(x_long), 3e9)
expect_equal(tccq_compile(length_long_bind_fn, backend = tccq_backend_shlib())(x_long), 3e9)
expect_equal(tccq_compile(length_long_bind_plus_fn)(x_long), 3e9 + 1)
expect_equal(tccq_compile(length_long_bind_plus_fn, backend = tccq_backend_shlib())(x_long), 3e9 + 1)

length_negative_long_fn <- function(x) {
  declare(type(x = double(NA)))
  1L - length(x)
}

expect_equal(tccq_compile(length_negative_long_fn)(x_long), 1 - 3e9)
expect_equal(tccq_compile(length_negative_long_fn, backend = tccq_backend_shlib())(x_long), 1 - 3e9)

length_vector_arith_fn <- function(x, y) {
  declare(type(x = double(NA), y = integer(NA)))
  length(x) + y
}

expect_equal(tccq_compile(length_vector_arith_fn)(as.double(1:5), 1:3), c(6, 7, 8))
expect_equal(
  tccq_compile(length_vector_arith_fn, backend = tccq_backend_shlib())(as.double(1:5), 1:3),
  c(6, 7, 8)
)

length_division_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x) / 2L
}

expect_equal(tccq_compile(length_division_fn)(as.double(1:5)), 2.5)
expect_equal(tccq_compile(length_division_fn, backend = tccq_backend_shlib())(as.double(1:5)), 2.5)

length_vector_division_fn <- function(x, y) {
  declare(type(x = double(NA)), type(y = integer(NA)))
  length(x) / y
}

expect_equal(tccq_compile(length_vector_division_fn)(as.double(1:5), c(2L, 4L)), c(2.5, 1.25))
expect_equal(
  tccq_compile(length_vector_division_fn, backend = tccq_backend_shlib())(as.double(1:5), c(2L, 4L)),
  c(2.5, 1.25)
)

length_boundary_fn <- function(x) {
  declare(type(x = double(NA)))
  length(rev(x))
}

expect_equal(tccq_compile(length_boundary_fn, fallback = "auto")(as.double(1:5)), 5L)
expect_equal(
  tccq_compile(length_boundary_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  5L
)

length_scalar_index_oob_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x[6])
}

expect_error(tccq_compile(length_scalar_index_oob_fn)(as.double(1:5)), "index out of bounds")
expect_error(
  tccq_compile(length_scalar_index_oob_fn, backend = tccq_backend_shlib())(as.double(1:5)),
  "index out of bounds"
)

assign("tccq_test_boom", function(z) stop("boom"), envir = .GlobalEnv)
length_boundary_scalar_fn <- function(x) {
  declare(type(x = double()))
  length(tccq_test_boom(x))
}

expect_error(tccq_compile(length_boundary_scalar_fn, fallback = "auto")(1), "boom")
expect_error(
  tccq_compile(length_boundary_scalar_fn, fallback = "auto", backend = tccq_backend_shlib())(1),
  "boom"
)

assign("tccq_test_dup2", function(z) c(z, z), envir = .GlobalEnv)
length_boundary_runtime_vector_fn <- function(x) {
  declare(type(x = double()))
  length(tccq_test_dup2(x))
}

expect_equal(tccq_compile(length_boundary_runtime_vector_fn, fallback = "auto")(1), 2L)
expect_equal(
  tccq_compile(length_boundary_runtime_vector_fn, fallback = "auto", backend = tccq_backend_shlib())(1),
  2L
)

length_boundary_runtime_vector_hoisted_fn <- function(x) {
  declare(type(x = double()))
  1L + length(tccq_test_dup2(x))
}

expect_equal(tccq_compile(length_boundary_runtime_vector_hoisted_fn, fallback = "auto")(1), 3L)
expect_equal(
  tccq_compile(length_boundary_runtime_vector_hoisted_fn, fallback = "auto", backend = tccq_backend_shlib())(1),
  3L
)

length_product_long_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  length(x) * length(y)
}

n_product <- 3037000500
x_product <- as.double(1:n_product)
y_product <- as.double(1:n_product)
expect_equal(tccq_compile(length_product_long_fn)(x_product, y_product), n_product * n_product)
expect_equal(
  tccq_compile(length_product_long_fn, backend = tccq_backend_shlib())(x_product, y_product),
  n_product * n_product
)

length_alias_in_slice_bounds_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  length(x[y[1]:y[2]])
}

expect_equal(tccq_compile(length_alias_in_slice_bounds_fn)(as.double(c(2, 4, 6, 8, 10))), 3L)
expect_equal(
  tccq_compile(length_alias_in_slice_bounds_fn, backend = tccq_backend_shlib())(as.double(c(2, 4, 6, 8, 10))),
  3L
)

length_boundary_uses_alias_arg_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  length(rev(y))
}

expect_equal(tccq_compile(length_boundary_uses_alias_arg_fn, fallback = "auto")(as.double(1:5)), 5L)
expect_equal(
  tccq_compile(length_boundary_uses_alias_arg_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  5L
)

length_composite_scalar_boundary_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x + tccq_test_boom(1))
}

expect_error(tccq_compile(length_composite_scalar_boundary_fn, fallback = "auto")(as.double(1:5)), "boom")
expect_error(
  tccq_compile(length_composite_scalar_boundary_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  "boom"
)

length_cache_name_collision_fn <- function(x) {
  declare(type(x = double(NA)))
  x_cache <- x
  length(x[x_cache[1]:x[1]])
}

expect_equal(tccq_compile(length_cache_name_collision_fn)(as.double(c(2, 4, 6, 8))), 1L)
expect_equal(
  tccq_compile(length_cache_name_collision_fn, backend = tccq_backend_shlib())(as.double(c(2, 4, 6, 8))),
  1L
)

assign("tccq_test_id", function(z) z, envir = .GlobalEnv)
length_bound_scalar_expr_preserves_runtime_length_fn <- function(x) {
  declare(type(x = double()))
  y <- x + 1L
  length(y)
}

expect_equal(tccq_compile(length_bound_scalar_expr_preserves_runtime_length_fn)(c(10, 20)), 2L)
expect_equal(
  tccq_compile(length_bound_scalar_expr_preserves_runtime_length_fn, backend = tccq_backend_shlib())(c(10, 20)),
  2L
)

length_xlen_boundary_bind_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- length(x)
  z <- tccq_test_id(y)
  z
}

expect_equal(tccq_compile(length_xlen_boundary_bind_fn, fallback = "auto")(as.double(1:5)), 5L)
expect_equal(
  tccq_compile(length_xlen_boundary_bind_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  5L
)
expect_equal(tccq_compile(length_xlen_boundary_bind_fn, fallback = "auto")(x_long), 3e9)
expect_equal(
  tccq_compile(length_xlen_boundary_bind_fn, fallback = "auto", backend = tccq_backend_shlib())(x_long),
  3e9
)

assign("tccq_test_pair_xlen", function(z) c(5, 6), envir = .GlobalEnv)
length_xlen_boundary_reject_vector_value_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- tccq_test_pair_xlen(length(x))
  y
}

expect_error(
  tccq_compile(length_xlen_boundary_reject_vector_value_fn, fallback = "auto")(as.double(1:2)),
  "runtime length"
)
expect_error(
  tccq_compile(length_xlen_boundary_reject_vector_value_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:2)),
  "runtime length"
)

length_xlen_boundary_reject_vector_arith_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- tccq_test_pair_xlen(length(x))
  y + 1L
}

expect_error(
  tccq_compile(length_xlen_boundary_reject_vector_arith_fn, fallback = "auto")(as.double(1:2)),
  "runtime length"
)
expect_error(
  tccq_compile(length_xlen_boundary_reject_vector_arith_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:2)),
  "runtime length"
)

length_scalar_formal_value_rejects_vector_fn <- function(x) {
  declare(type(x = double()))
  x + 1L
}

expect_error(tccq_compile(length_scalar_formal_value_rejects_vector_fn)(c(10, 20)), "runtime length")
expect_error(
  tccq_compile(length_scalar_formal_value_rejects_vector_fn, backend = tccq_backend_shlib())(c(10, 20)),
  "runtime length"
)

assign("tccq_test_half", function(z) z + 0.5, envir = .GlobalEnv)
length_xlen_boundary_reject_fractional_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- length(x)
  z <- tccq_test_half(y)
  z
}

expect_error(
  tccq_compile(length_xlen_boundary_reject_fractional_fn, fallback = "auto")(as.double(1:5)),
  "integer-like finite length"
)
expect_error(
  tccq_compile(length_xlen_boundary_reject_fractional_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  "integer-like finite length"
)

assign("tccq_test_badlen", function(z) Inf, envir = .GlobalEnv)
length_xlen_boundary_reject_infinite_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- length(x)
  z <- tccq_test_badlen(y)
  z
}

expect_error(
  tccq_compile(length_xlen_boundary_reject_infinite_fn, fallback = "auto")(as.double(1:5)),
  "integer-like finite length"
)
expect_error(
  tccq_compile(length_xlen_boundary_reject_infinite_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  "integer-like finite length"
)

length_bound_boundary_runtime_vector_fn <- function(x) {
  declare(type(x = double()))
  y <- tccq_test_dup2(x)
  length(y)
}

expect_equal(tccq_compile(length_bound_boundary_runtime_vector_fn, fallback = "auto")(1), 2L)
expect_equal(
  tccq_compile(length_bound_boundary_runtime_vector_fn, fallback = "auto", backend = tccq_backend_shlib())(1),
  2L
)

length_boundary_call1_runtime_vector_fn <- function(x) {
  declare(type(x = double()))
  length(abs(tccq_test_dup2(x)))
}

expect_equal(tccq_compile(length_boundary_call1_runtime_vector_fn, fallback = "auto")(1), 2L)
expect_equal(
  tccq_compile(length_boundary_call1_runtime_vector_fn, fallback = "auto", backend = tccq_backend_shlib())(1),
  2L
)

length_nested_boundary_scalar_fn <- function(x) {
  declare(type(x = double()))
  length(tccq_test_id(x) + 1L)
}

expect_equal(tccq_compile(length_nested_boundary_scalar_fn, fallback = "auto")(1), 1L)
expect_equal(
  tccq_compile(length_nested_boundary_scalar_fn, fallback = "auto", backend = tccq_backend_shlib())(1),
  1L
)

length_nested_boundary_call1_fn <- function(x) {
  declare(type(x = double()))
  length(abs(tccq_test_id(x)))
}

expect_equal(tccq_compile(length_nested_boundary_call1_fn, fallback = "auto")(1), 1L)
expect_equal(
  tccq_compile(length_nested_boundary_call1_fn, fallback = "auto", backend = tccq_backend_shlib())(1),
  1L
)

length_does_not_force_altrep_fn <- function(x) {
  declare(type(x = double(NA)))
  length(x)
}

altrep_x <- as.double(1:10)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_x))), fixed = TRUE)))
tccq_compile(length_does_not_force_altrep_fn)(altrep_x)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_x))), fixed = TRUE)))
altrep_y <- as.double(1:10)
tccq_compile(length_does_not_force_altrep_fn, backend = tccq_backend_shlib())(altrep_y)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_y))), fixed = TRUE)))

length_alias_does_not_force_altrep_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  length(y)
}

altrep_alias_x <- as.double(1:10)
tccq_compile(length_alias_does_not_force_altrep_fn)(altrep_alias_x)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_alias_x))), fixed = TRUE)))
altrep_alias_y <- as.double(1:10)
tccq_compile(length_alias_does_not_force_altrep_fn, backend = tccq_backend_shlib())(altrep_alias_y)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_alias_y))), fixed = TRUE)))

length_view_does_not_force_altrep_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x[2:5]
  length(y)
}

altrep_view_x <- as.double(1:10)
tccq_compile(length_view_does_not_force_altrep_fn)(altrep_view_x)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_view_x))), fixed = TRUE)))
altrep_view_y <- as.double(1:10)
tccq_compile(length_view_does_not_force_altrep_fn, backend = tccq_backend_shlib())(altrep_view_y)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_view_y))), fixed = TRUE)))
