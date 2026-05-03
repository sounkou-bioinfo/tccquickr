# test_tccq_matrix.R

matrix_fill_return_fn <- function(nx, ny) {
  declare(type(nx = integer()), type(ny = integer()))
  m <- matrix(0, nx, ny)
  m
}

matrix_fill_return_mod <- tccq_compile(matrix_fill_return_fn, mode = "ir")
expect_identical(matrix_fill_return_mod$storage_plan$bindings$m$type$rank, 2L)
mat0 <- tccq_compile(matrix_fill_return_fn)(3L, 4L)
expect_equal(dim(mat0), c(3L, 4L))
expect_equal(mat0, matrix(0, 3L, 4L))
expect_equal(
  tccq_compile(matrix_fill_return_fn, backend = tccq_backend_shlib())(3L, 4L),
  matrix(0, 3L, 4L)
)

matrix_bare_expr_fn <- function(nx, ny) {
  declare(type(nx = integer()), type(ny = integer()))
  matrix(0, nx, ny)
}

expect_equal(tccq_compile(matrix_bare_expr_fn)(2L, 3L), matrix(0, 2L, 3L))

matrix_named_args_fn <- function(nx, ny) {
  declare(type(nx = integer()), type(ny = integer()))
  matrix(0, ncol = ny, nrow = nx)
}

expect_equal(tccq_compile(matrix_named_args_fn)(2L, 3L), matrix(0, nrow = 2L, ncol = 3L))

matrix_ambiguous_args_fn <- function(dummy) {
  declare(type(dummy = integer()))
  matrix(0, 2L, 3L, nrow = 4L)
}

expect_error(
  tccq_compile(matrix_ambiguous_args_fn, mode = "code"),
  pattern = "currently supports data, nrow, and ncol only"
)

matrix_len_hoist_value_fn <- function(x, nx, ny) {
  declare(type(x = double(NA)), type(nx = integer()), type(ny = integer()))
  matrix(sqrt(length(x + 0)), nx, ny)
}

expect_equal(tccq_compile(matrix_len_hoist_value_fn)(as.double(1:5), 2L, 3L), matrix(sqrt(5), 2L, 3L))

matrix_xlen_fill_rejected_fn <- function(x, nx, ny) {
  declare(type(x = double(NA)), type(nx = integer()), type(ny = integer()))
  matrix(length(x), nx, ny)
}

expect_error(
  tccq_compile(matrix_xlen_fill_rejected_fn, mode = "code"),
  pattern = "xlen"
)

matrix_len_hoist_dim_fn <- function(x, ny) {
  declare(type(x = double(NA)), type(ny = integer()))
  matrix(0, length(x + 0), ny)
}

expect_equal(tccq_compile(matrix_len_hoist_dim_fn)(as.double(1:5), 3L), matrix(0, 5L, 3L))

matrix_index2_hoist_fn <- function(v, x) {
  declare(type(v = double(NA)), type(x = double(NA, NA)))
  x[length(v + 0), 1L]
}

expect_equal(tccq_compile(matrix_index2_hoist_fn)(as.double(1:2), matrix(as.double(1:6), 2L, 3L)), 2)
expect_equal(
  tccq_compile(matrix_index2_hoist_fn, backend = tccq_backend_shlib())(as.double(1:2), matrix(as.double(1:6), 2L, 3L)),
  2
)

matrix_non_scalar_read_index_fn <- function(x, i) {
  declare(type(x = double(NA, NA)), type(i = integer(NA)))
  x[i, 1L]
}

expect_error(
  tccq_compile(matrix_non_scalar_read_index_fn, mode = "code"),
  pattern = "matrix extraction currently supports"
)

matrix_non_scalar_store_index_fn <- function(x, i, v) {
  declare(type(x = double(NA, NA)), type(i = integer(NA)), type(v = double()))
  y <- x
  y[i, 1L] <- v
  y
}

expect_error(
  tccq_compile(matrix_non_scalar_store_index_fn, mode = "code"),
  pattern = "assignment indices must be scalar"
)

matrix_pointwise_expr_fn <- function(x) {
  declare(type(x = double(NA, NA)))
  x + 1
}

x_point <- matrix(as.double(1:6), 2L, 3L)
expect_equal(tccq_compile(matrix_pointwise_expr_fn)(x_point), x_point + 1)
expect_equal(
  tccq_compile(matrix_pointwise_expr_fn, backend = tccq_backend_shlib())(x_point),
  x_point + 1
)

matrix_pointwise_bind_fn <- function(x) {
  declare(type(x = double(NA, NA)))
  y <- x + 1
  y
}

expect_equal(tccq_compile(matrix_pointwise_bind_fn)(x_point), x_point + 1)

matrix_conformability_fn <- function(x, y) {
  declare(type(x = double(NA, NA)), type(y = double(NA, NA)))
  x + y
}

expect_equal(tccq_compile(matrix_conformability_fn)(x_point, x_point), x_point + x_point)
expect_error(
  tccq_compile(matrix_conformability_fn)(matrix(as.double(1:12), 2L, 6L), matrix(as.double(1:12), 3L, 4L)),
  pattern = "matrix dimension mismatch"
)

matrix_scalar_store_read_fn <- function(nx, ny, i, j, v) {
  declare(
    type(nx = integer()), type(ny = integer()),
    type(i = integer()), type(j = integer()), type(v = double())
  )
  m <- matrix(0, nx, ny)
  m[i, j] <- v
  m[i, j]
}

matrix_scalar_store_read_mod <- tccq_compile(matrix_scalar_store_read_fn, mode = "ir")
expect_identical(matrix_scalar_store_read_mod$ir$stmts[[2L]]$tag, "store_access")
expect_identical(
  vapply(matrix_scalar_store_read_mod$ir$stmts[[2L]]$subscripts, `[[`, character(1), "kind"),
  c("index", "index")
)
expect_equal(tccq_compile(matrix_scalar_store_read_fn)(3L, 4L, 2L, 3L, 99), 99)
expect_equal(
  tccq_compile(matrix_scalar_store_read_fn, backend = tccq_backend_shlib())(3L, 4L, 2L, 3L, 99),
  99
)

matrix_na_read_fn <- function(x, i, j) {
  declare(type(x = double(NA, NA)), type(i = integer()), type(j = integer()))
  x[i, j]
}

expect_equal(tccq_compile(matrix_na_read_fn)(matrix(as.double(1:4), 2L, 2L), NA_integer_, 1L), NA_real_)
expect_equal(tccq_compile(matrix_na_read_fn)(matrix(as.double(1:4), 2L, 2L), 1L, NA_integer_), NA_real_)

matrix_na_scalar_store_fn <- function(x, i, j, v) {
  declare(type(x = double(NA, NA)), type(i = integer()), type(j = integer()), type(v = double()))
  y <- x
  y[i, j] <- v
  y
}

na_store_x <- matrix(as.double(1:4), 2L, 2L)
expect_equal(tccq_compile(matrix_na_scalar_store_fn)(na_store_x, NA_integer_, 1L, 99), na_store_x)
expect_equal(tccq_compile(matrix_na_scalar_store_fn)(na_store_x, 1L, NA_integer_, 99), na_store_x)
expect_equal(
  tccq_compile(matrix_na_scalar_store_fn, backend = tccq_backend_shlib())(na_store_x, NA_integer_, 1L, 99),
  na_store_x
)

matrix_na_row_store_fn <- function(x, i, v) {
  declare(type(x = double(NA, NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i, ] <- v
  y
}

matrix_na_col_store_fn <- function(x, j, v) {
  declare(type(x = double(NA, NA)), type(j = integer()), type(v = double()))
  y <- x
  y[, j] <- v
  y
}

expect_equal(tccq_compile(matrix_na_row_store_fn)(na_store_x, NA_integer_, 99), na_store_x)
expect_equal(tccq_compile(matrix_na_col_store_fn)(na_store_x, NA_integer_, 99), na_store_x)

assign("tccq_test_matrix_order_log", character(), envir = .GlobalEnv)
assign(
  "tccq_test_matrix_order_r",
  function(i) {
    assign("tccq_test_matrix_order_log", c(get("tccq_test_matrix_order_log", envir = .GlobalEnv), "r"), envir = .GlobalEnv)
    i
  },
  envir = .GlobalEnv
)
assign(
  "tccq_test_matrix_order_c",
  function(j) {
    assign("tccq_test_matrix_order_log", c(get("tccq_test_matrix_order_log", envir = .GlobalEnv), "c"), envir = .GlobalEnv)
    j
  },
  envir = .GlobalEnv
)
assign(
  "tccq_test_matrix_order_v",
  function(v) {
    assign("tccq_test_matrix_order_log", c(get("tccq_test_matrix_order_log", envir = .GlobalEnv), "v"), envir = .GlobalEnv)
    v
  },
  envir = .GlobalEnv
)

matrix_assignment_eval_order_fn <- function(nx, ny, i, j, v) {
  declare(
    type(nx = integer()), type(ny = integer()),
    type(i = integer()), type(j = integer()), type(v = double())
  )
  m <- matrix(0, nx, ny)
  m[tccq_test_matrix_order_r(i), tccq_test_matrix_order_c(j)] <- tccq_test_matrix_order_v(v)
  m
}

assign("tccq_test_matrix_order_log", character(), envir = .GlobalEnv)
expect_equal(
  tccq_compile(matrix_assignment_eval_order_fn, fallback = "auto")(2L, 3L, 2L, 2L, 99),
  { z <- matrix(0, 2L, 3L); z[2L, 2L] <- 99; z }
)
expect_identical(get("tccq_test_matrix_order_log", envir = .GlobalEnv), c("v", "r", "c"))
assign("tccq_test_matrix_order_log", character(), envir = .GlobalEnv)
expect_equal(
  tccq_compile(matrix_assignment_eval_order_fn, fallback = "auto", backend = tccq_backend_shlib())(2L, 3L, 2L, 2L, 99),
  { z <- matrix(0, 2L, 3L); z[2L, 2L] <- 99; z }
)
expect_identical(get("tccq_test_matrix_order_log", envir = .GlobalEnv), c("v", "r", "c"))

matrix_column_major_store_fn <- function(nx, ny, i, j, v) {
  declare(
    type(nx = integer()), type(ny = integer()),
    type(i = integer()), type(j = integer()), type(v = double())
  )
  m <- matrix(0, nx, ny)
  m[i, j] <- v
  m
}

mat1 <- tccq_compile(matrix_column_major_store_fn)(3L, 4L, 2L, 3L, 99)
expect_equal(mat1, { z <- matrix(0, 3L, 4L); z[2L, 3L] <- 99; z })

matrix_row_assignment_fn <- function(nx, ny, v) {
  declare(type(nx = integer()), type(ny = integer()), type(v = double()))
  m <- matrix(0, nx, ny)
  m[2L, ] <- v
  m
}

expect_equal(tccq_compile(matrix_row_assignment_fn)(3L, 4L, 7), { z <- matrix(0, 3L, 4L); z[2L, ] <- 7; z })
expect_equal(
  tccq_compile(matrix_row_assignment_fn, backend = tccq_backend_shlib())(3L, 4L, 7),
  { z <- matrix(0, 3L, 4L); z[2L, ] <- 7; z }
)

matrix_col_assignment_fn <- function(nx, ny, v) {
  declare(type(nx = integer()), type(ny = integer()), type(v = double()))
  m <- matrix(0, nx, ny)
  m[, 3L] <- v
  m
}

expect_equal(tccq_compile(matrix_col_assignment_fn)(3L, 4L, 8), { z <- matrix(0, 3L, 4L); z[, 3L] <- 8; z })

matrix_rect_assignment_fn <- function(nx, ny, v) {
  declare(type(nx = integer()), type(ny = integer()), type(v = double()))
  m <- matrix(0, nx, ny)
  m[1L:2L, 2L:3L] <- v
  m
}

expect_equal(tccq_compile(matrix_rect_assignment_fn)(3L, 4L, 9), { z <- matrix(0, 3L, 4L); z[1L:2L, 2L:3L] <- 9; z })

matrix_full_assignment_fn <- function(nx, ny, v) {
  declare(type(nx = integer()), type(ny = integer()), type(v = double()))
  m <- matrix(0, nx, ny)
  m[, ] <- v
  m
}

matrix_full_assignment_mod <- tccq_compile(matrix_full_assignment_fn, mode = "ir")
expect_identical(
  vapply(matrix_full_assignment_mod$ir$stmts[[2L]]$subscripts, `[[`, character(1), "kind"),
  c("all", "all")
)
expect_equal(tccq_compile(matrix_full_assignment_fn)(2L, 3L, 4), matrix(4, 2L, 3L))

matrix_alias_cow_fn <- function(x, i, j, v) {
  declare(type(x = double(NA, NA)), type(i = integer()), type(j = integer()), type(v = double()))
  y <- x
  y[i, j] <- v
  y
}

matrix_alias_cow_mod <- tccq_compile(matrix_alias_cow_fn, mode = "ir")
expect_identical(matrix_alias_cow_mod$storage_plan$bindings$y$kind, "alias")
expect_true(isTRUE(matrix_alias_cow_mod$storage_plan$bindings$y$materialize_on_write))
xmat <- matrix(as.double(1:9), 3L, 3L)
expect_equal(tccq_compile(matrix_alias_cow_fn)(xmat, 2L, 2L, 99), { z <- xmat; z[2L, 2L] <- 99; z })
expect_equal(xmat, matrix(as.double(1:9), 3L, 3L))
expect_equal(
  tccq_compile(matrix_alias_cow_fn, backend = tccq_backend_shlib())(xmat, 2L, 2L, 99),
  { z <- xmat; z[2L, 2L] <- 99; z }
)

matrix_direct_formal_mutation_fn <- function(x, v) {
  declare(type(x = double(NA, NA)), type(v = double()))
  x[1, 1] <- v
  x
}

expect_error(
  tccq_compile(matrix_direct_formal_mutation_fn, mode = "code"),
  pattern = "requires a local matrix binding"
)

matrix_direct_formal_slice_mutation_fn <- function(x, v) {
  declare(type(x = double(NA, NA)), type(v = double()))
  x[1, ] <- v
  x
}

expect_error(
  tccq_compile(matrix_direct_formal_slice_mutation_fn, mode = "code"),
  pattern = "requires a local matrix binding"
)

matrix_store_oob_fn <- function(nx, ny) {
  declare(type(nx = integer()), type(ny = integer()))
  m <- matrix(0, nx, ny)
  m[nx + 1L, 1L] <- 1
  m
}

expect_error(
  tccq_compile(matrix_store_oob_fn)(3L, 4L),
  pattern = "index out of bounds"
)

integer_floor_division_fn <- function(nx) {
  declare(type(nx = integer()))
  nx %/% 2L
}

expect_equal(tccq_compile(integer_floor_division_fn)(5L), 2L)
expect_equal(tccq_compile(integer_floor_division_fn)(-3L), -2L)
expect_equal(tccq_compile(integer_floor_division_fn)(NA_integer_), NA_integer_)

integer_floor_division_zero_fn <- function(nx) {
  declare(type(nx = integer()))
  nx %/% 0L
}

expect_equal(tccq_compile(integer_floor_division_zero_fn)(5L), NA_integer_)

logical_floor_division_fn <- function(a, b) {
  declare(type(a = logical()), type(b = logical()))
  a %/% b
}

logical_floor_division_vector_fn <- function(a, b) {
  declare(type(a = logical(NA)), type(b = logical(NA)))
  a %/% b
}

logical_idiv <- tccq_compile(logical_floor_division_fn)(TRUE, TRUE)
expect_true(is.integer(logical_idiv))
expect_identical(logical_idiv, 1L)
logical_idiv_shlib <- tccq_compile(logical_floor_division_fn, backend = tccq_backend_shlib())(TRUE, TRUE)
expect_true(is.integer(logical_idiv_shlib))
expect_identical(logical_idiv_shlib, 1L)
logical_idiv_vec <- tccq_compile(logical_floor_division_vector_fn)(c(TRUE, FALSE), c(TRUE, TRUE))
expect_true(is.integer(logical_idiv_vec))
expect_identical(logical_idiv_vec, c(1L, 0L))
logical_idiv_vec_shlib <- tccq_compile(logical_floor_division_vector_fn, backend = tccq_backend_shlib())(c(TRUE, FALSE), c(TRUE, TRUE))
expect_true(is.integer(logical_idiv_vec_shlib))
expect_identical(logical_idiv_vec_shlib, c(1L, 0L))
