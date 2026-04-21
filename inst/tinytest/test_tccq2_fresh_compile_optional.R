# test_tccq2_fresh_compile_optional.R

if (requireNamespace("Rtinycc", quietly = TRUE)) {
  fresh_sum_kernel <- function(x, y) {
    declare(type(x = double(NA), y = double(NA)))
    sum((sin(x) + y) * y)
  }

  fresh_vec_kernel <- function(x, y) {
    declare(type(x = double(NA), y = double(NA)))
    sin(x) + y * y
  }

  ref_sum <- function(x, y) {
    sum((sin(x) + y) * y)
  }

  ref_vec <- function(x, y) {
    sin(x) + y * y
  }

  x <- as.double(seq(-2, 2, length.out = 25))
  y <- as.double(seq(1, 3, length.out = 25))

  cf_sum <- tccquickr:::tccq2_compile(fresh_sum_kernel)
  expect_equal(cf_sum(x, y), ref_sum(x, y), tolerance = 1e-10)

  cf_vec <- tccquickr:::tccq2_compile(fresh_vec_kernel)
  expect_equal(cf_vec(x, y), ref_vec(x, y), tolerance = 1e-10)
}
