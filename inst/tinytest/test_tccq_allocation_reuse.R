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
reuse_pointwise_result_compiled <- tccq_compile(reuse_pointwise_result_fn)
invisible(gc())
reuse_pointwise_result_gc <- reuse_pointwise_result_compiled(as.double(1:1000))
invisible(gc())
expect_equal(reuse_pointwise_result_gc, (as.double(1:1000) + 1) * 2)

reuse_integer_pointwise_result_fn <- function(x) {
  declare(type(x = integer(NA)))
  y <- x + 1L
  y * 2L
}

reuse_integer_pointwise_result_mod <- tccq_compile(reuse_integer_pointwise_result_fn, mode = "ir")
expect_identical(
  reuse_integer_pointwise_result_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
expect_identical(reuse_integer_pointwise_result_mod$alloc_plan$reuse$result_buffer$mode, "integer")
expect_equal(tccq_compile(reuse_integer_pointwise_result_fn)(1:3), c(4L, 6L, 8L))

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

reuse_bind_buffer_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  z <- y * 2
  z
}

reuse_bind_buffer_mod <- tccq_compile(reuse_bind_buffer_fn, mode = "ir")
expect_identical(
  reuse_bind_buffer_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
expect_identical(reuse_bind_buffer_mod$alloc_plan$reuse$bindings$z$source, "y")
reuse_bind_buffer_code <- tccq_compile(reuse_bind_buffer_fn, mode = "code")
expect_true(grepl("SEXP loc_z = loc_y;", reuse_bind_buffer_code, fixed = TRUE))
expect_equal(tccq_compile(reuse_bind_buffer_fn)(as.double(1:3)), (as.double(1:3) + 1) * 2)
expect_equal(
  tccq_compile(reuse_bind_buffer_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
reuse_bind_buffer_compiled <- tccq_compile(reuse_bind_buffer_fn)
invisible(gc())
reuse_bind_buffer_gc <- reuse_bind_buffer_compiled(as.double(1:1000))
invisible(gc())
expect_equal(reuse_bind_buffer_gc, (as.double(1:1000) + 1) * 2)

reuse_bind_then_result_buffer_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  z <- y * 2
  z + 3
}

reuse_bind_then_result_buffer_mod <- tccq_compile(reuse_bind_then_result_buffer_fn, mode = "ir")
expect_identical(
  reuse_bind_then_result_buffer_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
expect_identical(
  reuse_bind_then_result_buffer_mod$alloc_plan$reuse$result_buffer$name,
  "z"
)
expect_equal(tccq_compile(reuse_bind_then_result_buffer_fn)(as.double(1:3)), ((as.double(1:3) + 1) * 2) + 3)
expect_equal(
  tccq_compile(reuse_bind_then_result_buffer_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  ((as.double(1:3) + 1) * 2) + 3
)

reuse_bind_captured_scalar_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- y[1]
  z <- y + s
  z
}

reuse_bind_captured_scalar_mod <- tccq_compile(reuse_bind_captured_scalar_fn, mode = "ir")
expect_identical(
  reuse_bind_captured_scalar_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
expect_equal(tccq_compile(reuse_bind_captured_scalar_fn)(as.double(1:3)), (as.double(1:3) + 1) + 2)
expect_equal(
  tccq_compile(reuse_bind_captured_scalar_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  (as.double(1:3) + 1) + 2
)

assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
assign(
  "tccq_alloc_reuse_capture_vec",
  function(z) {
    assign("tccq_alloc_reuse_leaked", z, envir = .GlobalEnv)
    z
  },
  envir = .GlobalEnv
)
assign("tccq_alloc_reuse_id_scalar", function(z) z, envir = .GlobalEnv)

reuse_result_scalar_boundary_no_escape_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- tccq_alloc_reuse_id_scalar(length(y))
  y * 2
}

reuse_result_scalar_boundary_no_escape_mod <- tccq_compile(
  reuse_result_scalar_boundary_no_escape_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_result_scalar_boundary_no_escape_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
expect_equal(
  tccq_compile(reuse_result_scalar_boundary_no_escape_fn, fallback = "auto")(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)

reuse_bind_scalar_boundary_no_escape_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- tccq_alloc_reuse_id_scalar(length(y))
  z <- y * 2
  z
}

reuse_bind_scalar_boundary_no_escape_mod <- tccq_compile(
  reuse_bind_scalar_boundary_no_escape_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_bind_scalar_boundary_no_escape_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
expect_equal(
  tccq_compile(reuse_bind_scalar_boundary_no_escape_fn, fallback = "auto")(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)

reuse_result_boundary_escape_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- length(tccq_alloc_reuse_capture_vec(y))
  y * 2
}

reuse_result_boundary_escape_not_safe_mod <- tccq_compile(
  reuse_result_boundary_escape_not_safe_fn,
  mode = "ir",
  fallback = "auto"
)
expect_null(reuse_result_boundary_escape_not_safe_mod$alloc_plan$reuse$result_buffer)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_result_boundary_escape_not_safe_fn, fallback = "auto")(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(
    reuse_result_boundary_escape_not_safe_fn,
    fallback = "auto",
    backend = tccq_backend_shlib()
  )(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)

reuse_result_after_escape_copy_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- length(tccq_alloc_reuse_capture_vec(y))
  y[1] <- 99
  y * 2
}

reuse_result_after_escape_copy_mod <- tccq_compile(
  reuse_result_after_escape_copy_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_result_after_escape_copy_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_result_after_escape_copy_fn, fallback = "auto")(as.double(1:3)),
  c(198, 6, 8)
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)

reuse_result_alias_after_cow_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  s <- length(tccq_alloc_reuse_capture_vec(y))
  y[1] <- 99
  y * 2
}

reuse_result_alias_after_cow_mod <- tccq_compile(
  reuse_result_alias_after_cow_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_result_alias_after_cow_mod$alloc_plan$reuse$result_buffer$strategy,
  "reuse_owned_local_result"
)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_result_alias_after_cow_fn, fallback = "auto")(as.double(1:3)),
  c(198, 4, 6)
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3))

reuse_bind_boundary_escape_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- length(tccq_alloc_reuse_capture_vec(y))
  z <- y * 2
  z
}

reuse_bind_boundary_escape_not_safe_mod <- tccq_compile(
  reuse_bind_boundary_escape_not_safe_fn,
  mode = "ir",
  fallback = "auto"
)
expect_null(reuse_bind_boundary_escape_not_safe_mod$alloc_plan$reuse$bindings$z)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_bind_boundary_escape_not_safe_fn, fallback = "auto")(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(
    reuse_bind_boundary_escape_not_safe_fn,
    fallback = "auto",
    backend = tccq_backend_shlib()
  )(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)

reuse_bind_after_escape_copy_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  s <- length(tccq_alloc_reuse_capture_vec(y))
  y[1] <- 99
  z <- y * 2
  z
}

reuse_bind_after_escape_copy_mod <- tccq_compile(
  reuse_bind_after_escape_copy_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_bind_after_escape_copy_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_bind_after_escape_copy_fn, fallback = "auto")(as.double(1:3)),
  c(198, 6, 8)
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)

reuse_bind_view_after_cow_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  v <- y[1:2]
  s <- length(tccq_alloc_reuse_capture_vec(v))
  v[1] <- 99
  z <- v * 2
  z
}

reuse_bind_view_after_cow_mod <- tccq_compile(
  reuse_bind_view_after_cow_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_bind_view_after_cow_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_bind_view_after_cow_fn, fallback = "auto")(as.double(1:3)),
  c(198, 6)
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), c(2, 3))

reuse_bind_stale_alias_after_cow_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  a <- y
  s <- length(tccq_alloc_reuse_capture_vec(y))
  y[1] <- 99
  z <- y * 2
  a
}

reuse_bind_stale_alias_after_cow_mod <- tccq_compile(
  reuse_bind_stale_alias_after_cow_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_bind_stale_alias_after_cow_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_bind_stale_alias_after_cow_fn, fallback = "auto")(as.double(1:3)),
  as.double(2:4)
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(2:4))

reuse_bind_alias_boundary_escape_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  a <- y
  s <- length(tccq_alloc_reuse_capture_vec(a))
  z <- y * 2
  z
}

reuse_bind_alias_boundary_escape_not_safe_mod <- tccq_compile(
  reuse_bind_alias_boundary_escape_not_safe_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_bind_alias_boundary_escape_not_safe_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_bind_alias_boundary_escape_not_safe_fn, fallback = "auto")(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(
    reuse_bind_alias_boundary_escape_not_safe_fn,
    fallback = "auto",
    backend = tccq_backend_shlib()
  )(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), as.double(1:3) + 1)

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

reuse_bind_future_write_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  z <- y * 2
  y[1] <- 100
  z
}

reuse_bind_future_write_not_safe_mod <- tccq_compile(reuse_bind_future_write_not_safe_fn, mode = "ir")
expect_null(reuse_bind_future_write_not_safe_mod$alloc_plan$reuse$bindings$z)
expect_equal(tccq_compile(reuse_bind_future_write_not_safe_fn)(as.double(1:3)), (as.double(1:3) + 1) * 2)
expect_equal(
  tccq_compile(reuse_bind_future_write_not_safe_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)

reuse_bind_live_view_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  v <- y[1:2]
  z <- y * 2
  v
}

reuse_bind_live_view_not_safe_mod <- tccq_compile(reuse_bind_live_view_not_safe_fn, mode = "ir")
expect_null(reuse_bind_live_view_not_safe_mod$alloc_plan$reuse$bindings$z)
expect_equal(tccq_compile(reuse_bind_live_view_not_safe_fn)(as.double(1:3)), c(2, 3))
expect_equal(
  tccq_compile(reuse_bind_live_view_not_safe_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  c(2, 3)
)

reuse_bind_escaped_view_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  v <- y[1:2]
  s <- length(tccq_alloc_reuse_capture_vec(v))
  z <- y * 2
  z
}

reuse_bind_escaped_view_not_safe_mod <- tccq_compile(
  reuse_bind_escaped_view_not_safe_fn,
  mode = "ir",
  fallback = "auto"
)
expect_identical(
  reuse_bind_escaped_view_not_safe_mod$alloc_plan$reuse$bindings$z$strategy,
  "reuse_owned_local_bind"
)
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(reuse_bind_escaped_view_not_safe_fn, fallback = "auto")(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), c(2, 3))
assign("tccq_alloc_reuse_leaked", NULL, envir = .GlobalEnv)
expect_equal(
  tccq_compile(
    reuse_bind_escaped_view_not_safe_fn,
    fallback = "auto",
    backend = tccq_backend_shlib()
  )(as.double(1:3)),
  (as.double(1:3) + 1) * 2
)
expect_equal(get("tccq_alloc_reuse_leaked", envir = .GlobalEnv), c(2, 3))

reuse_bind_live_alias_not_safe_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  a <- y
  z <- y * 2
  a
}

reuse_bind_live_alias_not_safe_mod <- tccq_compile(reuse_bind_live_alias_not_safe_fn, mode = "ir")
expect_null(reuse_bind_live_alias_not_safe_mod$alloc_plan$reuse$bindings$z)
expect_equal(tccq_compile(reuse_bind_live_alias_not_safe_fn)(as.double(1:3)), as.double(1:3) + 1)
expect_equal(
  tccq_compile(reuse_bind_live_alias_not_safe_fn, backend = tccq_backend_shlib())(as.double(1:3)),
  as.double(1:3) + 1
)
