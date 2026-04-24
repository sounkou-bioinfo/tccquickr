# test_tccq_allocation_reuse.R

reuse_pointwise_result_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  y * 2
}

reuse_pointwise_result_mod <- tccq_compile(reuse_pointwise_result_fn, mode = "ir")
expect_identical(
  reuse_pointwise_result_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
expect_identical(reuse_pointwise_result_mod$alloc_plan$reuse$result_buffer$name, "y")

reuse_pointwise_result_code <- tccq_compile(reuse_pointwise_result_fn, mode = "code")
expect_true(grepl("SEXP out = loc_y;", reuse_pointwise_result_code, fixed = TRUE))
expect_true(grepl("p_out[i] = (double)", reuse_pointwise_result_code, fixed = TRUE))
expect_equal(tccq_compile(reuse_pointwise_result_fn)(as.double(1:3)), (as.double(1:3) + 1) * 2)
expect_equal(
  tccq_compile(reuse_pointwise_result_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)

reuse_pointwise_call_result_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  sin(y)
}

reuse_pointwise_call_result_mod <- tccq_compile(reuse_pointwise_call_result_fn, mode = "ir")
expect_identical(
  reuse_pointwise_call_result_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
expect_equal(
  tccq_compile(reuse_pointwise_call_result_fn)(as.double(1:3)),
  sin(as.double(1:3) + 1),
  tolerance = 1e-10
)
expect_equal(
  tccq_compile(reuse_pointwise_call_result_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  sin(as.double(1:3) + 1),
  tolerance = 1e-10
)

expect_equal(tccq_compile(reuse_pointwise_result_fn)(numeric()), numeric())
expect_equal(tccq_compile(reuse_pointwise_result_fn, backend = tccq_backend_shlib())(numeric()), numeric())

reuse_after_write_result_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x + 0
  y[1] <- v
  y * 2
}

reuse_after_write_result_mod <- tccq_compile(reuse_after_write_result_fn, mode = "ir")
expect_identical(
  reuse_after_write_result_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
expect_equal(tccq_compile(reuse_after_write_result_fn)(as.double(1:3), 10), c(20, 4, 6))
expect_equal(
  tccq_compile(reuse_after_write_result_fn, backend = tccq_backend_shlib())(as.double(1:3), 10),
  c(20, 4, 6)
)

reuse_captured_scalar_result_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- y[1]
  y + s
}

reuse_captured_scalar_result_mod <- tccq_compile(reuse_captured_scalar_result_fn, mode = "ir")
expect_identical(
  reuse_captured_scalar_result_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
expect_equal(tccq_compile(reuse_captured_scalar_result_fn)(as.double(1:3)), (as.double(1:3) + 1) + 2)
expect_equal(
  tccq_compile(reuse_captured_scalar_result_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  (as.double(1:3) + 1) + 2
)

reuse_indexed_read_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  y + y[1]
}

reuse_indexed_read_not_safe_mod <- tccq_compile(reuse_indexed_read_not_safe_fn, mode = "ir")
expect_null(reuse_indexed_read_not_safe_mod$alloc_plan$reuse$result_buffer)
expect_equal(tccq_compile(reuse_indexed_read_not_safe_fn)(as.double(1:3)), (as.double(1:3) + 1) + 2)
expect_equal(
  tccq_compile(reuse_indexed_read_not_safe_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  (as.double(1:3) + 1) + 2
)

reuse_alias_read_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  z <- y
  z * 2
}

reuse_alias_read_not_safe_mod <- tccq_compile(reuse_alias_read_not_safe_fn, mode = "ir")
expect_null(reuse_alias_read_not_safe_mod$alloc_plan$reuse$result_buffer)
expect_equal(tccq_compile(reuse_alias_read_not_safe_fn)(as.double(1:3)), (as.double(1:3) + 1) * 2)
expect_equal(
  tccq_compile(reuse_alias_read_not_safe_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
