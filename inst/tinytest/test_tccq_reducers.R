# test_tccq_reducers.R

prod_kernel <- function(x) {
  declare(type(x = double(NA)))
  prod(x + 1)
}
prod_mod <- tccq_compile(prod_kernel, mode = "ir")
expect_identical(prod_mod$ir$result$tag, "reduce")
expect_identical(prod_mod$ir$result$op, "prod")
expect_equal(tccq_compile(prod_kernel)(c(1, 2, 3)), prod(c(1, 2, 3) + 1), tolerance = 1e-10)

mean_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  mean((x + y) * y)
}
mean_mod <- tccq_compile(mean_kernel, mode = "ir")
expect_identical(mean_mod$kernel$tag, "fold")
expect_identical(mean_mod$kernel$op, "mean")
expect_equal(
  tccq_compile(mean_kernel)(c(1, 2, 3), c(2, 3, 4)),
  mean((c(1, 2, 3) + c(2, 3, 4)) * c(2, 3, 4)),
  tolerance = 1e-10
)

min_slice_kernel <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  min(x[lo:hi])
}
expect_equal(tccq_compile(min_slice_kernel)(c(3, 1, 5, 2), 2L, 4L), 1)

all_slice_logic_kernel <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  all(x[lo:hi] > 0)
}
expect_equal(tccq_compile(all_slice_logic_kernel)(c(1, 2, -3, -4), 1L, 2L), TRUE)
expect_equal(tccq_compile(all_slice_logic_kernel)(c(1, 2, -3, -4), 1L, 3L), FALSE)

reduce_slice_logic_kernel <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  Reduce(`&`, x[lo:hi] > 0)
}
expect_equal(tccq_compile(reduce_slice_logic_kernel)(c(1, 2, -3, -4), 1L, 2L), Reduce(`&`, c(1, 2) > 0))

slice_cmp_vec_kernel <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  x[lo:hi] > 0
}
expect_equal(tccq_compile(slice_cmp_vec_kernel)(c(1, -2, 3, -4), 2L, 3L), c(FALSE, TRUE))

slice_zip_vec_kernel <- function(x, y, lo, hi) {
  declare(type(x = double(NA), y = double(NA)), type(lo = integer()), type(hi = integer()))
  x[lo:hi] + y[lo:hi]
}
expect_equal(
  tccq_compile(slice_zip_vec_kernel)(c(1, 2, 3, 4), c(10, 20, 30, 40), 2L, 4L),
  c(22, 33, 44),
  tolerance = 1e-10
)

max_kernel <- function(x) {
  declare(type(x = double(NA)))
  max(x)
}
expect_equal(tccq_compile(max_kernel)(c(3, 1, 5, 2)), 5)

all_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  all((x > 0) & (y > 0))
}
all_mod <- tccq_compile(all_kernel, mode = "ir")
expect_identical(all_mod$ir$result$op, "all")
expect_identical(all_mod$ir$result$x$tag, "binary")
expect_equal(tccq_compile(all_kernel)(c(1, 2, 3), c(4, 5, 6)), all((c(1, 2, 3) > 0) & (c(4, 5, 6) > 0)))
expect_equal(tccq_compile(all_kernel)(c(1, -2, 3), c(4, 5, 6)), all((c(1, -2, 3) > 0) & (c(4, 5, 6) > 0)))

any_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  any((x > y) | !(y > 0))
}
any_mod <- tccq_compile(any_kernel, mode = "ir")
expect_identical(any_mod$ir$result$op, "any")
expect_equal(
  tccq_compile(any_kernel)(c(1, 2, 3), c(2, 1, 3)),
  any((c(1, 2, 3) > c(2, 1, 3)) | !(c(2, 1, 3) > 0))
)

reduce_plus_kernel <- function(x) {
  declare(type(x = double(NA)))
  Reduce(`+`, x)
}
reduce_plus_mod <- tccq_compile(reduce_plus_kernel, mode = "ir")
expect_identical(reduce_plus_mod$ir$result$op, "sum")
expect_equal(tccq_compile(reduce_plus_kernel)(c(1, 2, 3)), Reduce(`+`, c(1, 2, 3)))

reduce_prod_kernel <- function(x) {
  declare(type(x = double(NA)))
  Reduce(`*`, x + 1)
}
reduce_prod_mod <- tccq_compile(reduce_prod_kernel, mode = "ir")
expect_identical(reduce_prod_mod$ir$result$op, "prod")
expect_equal(tccq_compile(reduce_prod_kernel)(c(1, 2, 3)), Reduce(`*`, c(1, 2, 3) + 1), tolerance = 1e-10)

reduce_all_kernel <- function(x) {
  declare(type(x = logical(NA)))
  Reduce(`&`, x)
}
reduce_all_mod <- tccq_compile(reduce_all_kernel, mode = "ir")
expect_identical(reduce_all_mod$ir$result$op, "all")
expect_equal(tccq_compile(reduce_all_kernel)(c(TRUE, TRUE, FALSE)), Reduce(`&`, c(TRUE, TRUE, FALSE)))

reduce_any_kernel <- function(x) {
  declare(type(x = logical(NA)))
  Reduce(`|`, x)
}
reduce_any_mod <- tccq_compile(reduce_any_kernel, mode = "ir")
expect_identical(reduce_any_mod$ir$result$op, "any")
expect_equal(tccq_compile(reduce_any_kernel)(c(FALSE, FALSE, TRUE)), Reduce(`|`, c(FALSE, FALSE, TRUE)))

reduce_empty_kernel <- function(x) {
  declare(type(x = double(NA)))
  Reduce(`+`, x)
}
expect_identical(tccq_compile(reduce_empty_kernel)(numeric()), NULL)

reduce_mean_kernel <- function(x) {
  declare(type(x = double(NA)))
  Reduce(mean, x)
}
expect_error(
  tccq_compile(reduce_mean_kernel, mode = "ir"),
  pattern = "recognized reducer"
)

sum_empty_kernel <- function(x) {
  declare(type(x = double(NA)))
  sum(x)
}
expect_equal(tccq_compile(sum_empty_kernel)(numeric()), 0)

prod_empty_kernel <- function(x) {
  declare(type(x = double(NA)))
  prod(x)
}
expect_equal(tccq_compile(prod_empty_kernel)(numeric()), 1)

mean_empty_kernel <- function(x) {
  declare(type(x = double(NA)))
  mean(x)
}
expect_true(is.nan(tccq_compile(mean_empty_kernel)(numeric())))

min_empty_kernel <- function(x) {
  declare(type(x = double(NA)))
  min(x)
}
expect_true(identical(tccq_compile(min_empty_kernel)(numeric()), Inf))

max_empty_kernel <- function(x) {
  declare(type(x = double(NA)))
  max(x)
}
expect_true(identical(tccq_compile(max_empty_kernel)(numeric()), -Inf))

sum_int_na_kernel <- function(x) {
  declare(type(x = integer(NA)))
  sum(x)
}
expect_true(is.na(tccq_compile(sum_int_na_kernel)(c(1L, NA_integer_))))

min_na_kernel <- function(x) {
  declare(type(x = double(NA)))
  min(x)
}
expect_true(is.na(tccq_compile(min_na_kernel)(c(2, NA_real_))))
expect_true(is.nan(tccq_compile(min_na_kernel)(c(2, NaN))))
expect_true(is.nan(tccq_compile(max_kernel)(c(2, NaN))))

sum_nan_kernel <- function(x) {
  declare(type(x = double(NA)))
  sum(x)
}
expect_true(is.nan(tccq_compile(sum_nan_kernel)(c(1, NaN))))

prod_nan_kernel <- function(x) {
  declare(type(x = double(NA)))
  prod(x)
}
expect_true(is.nan(tccq_compile(prod_nan_kernel)(c(1, NaN))))

mean_nan_kernel <- function(x) {
  declare(type(x = double(NA)))
  mean(x)
}
expect_true(is.nan(tccq_compile(mean_nan_kernel)(c(1, NaN))))

reduce_nan_kernel <- function(x) {
  declare(type(x = double(NA)))
  Reduce(`+`, x)
}
expect_true(is.nan(tccq_compile(reduce_nan_kernel)(c(1, NaN))))

compare_na_kernel <- function(x) {
  declare(type(x = double(NA)))
  any(x > 0)
}
expect_true(is.na(tccq_compile(compare_na_kernel)(c(NA_real_, -1))))

any_empty_kernel <- function(x) {
  declare(type(x = logical(NA)))
  any(x)
}
expect_identical(tccq_compile(any_empty_kernel)(logical()), FALSE)

all_empty_kernel <- function(x) {
  declare(type(x = logical(NA)))
  all(x)
}
expect_identical(tccq_compile(all_empty_kernel)(logical()), TRUE)

any_numeric_kernel <- function(x) {
  declare(type(x = double(NA)))
  any(x)
}
expect_error(
  tccq_compile(any_numeric_kernel, mode = "ir"),
  pattern = "any\\(\\) requires logical input"
)

bad_reduce_kernel <- function(x) {
  declare(type(x = double(NA)))
  Reduce(`-`, x)
}
expect_error(
  tccq_compile(bad_reduce_kernel, mode = "ir"),
  pattern = "recognized reducer"
)
