# test_tccq_shlib_backend.R

shlib_sum_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

shlib_vec_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sin(x) + y * y
}

shlib_assign_fn <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

shlib_fallback_fn <- function(x) {
  declare(type(x = double()))
  identity(x)
}

shlib_backend <- tccquickr:::tccq_backend_shlib()
expect_identical(shlib_backend$name, "shlib")
expect_identical(shlib_backend$capabilities$shared_library, TRUE)
expect_identical(shlib_backend$capabilities$system_compiler, TRUE)
expect_identical(shlib_backend$capabilities$cli, TRUE)

x <- as.double(seq(-2, 2, length.out = 25))
y <- as.double(seq(1, 3, length.out = 25))

compiled_sum <- tccq_compile(shlib_sum_fn, backend = shlib_backend)
compiled_vec <- tccq_compile(shlib_vec_fn, backend = shlib_backend)
compiled_assign <- tccq_compile(shlib_assign_fn, backend = shlib_backend)
compiled_fallback <- tccq_compile(shlib_fallback_fn, backend = shlib_backend, fallback = "auto")

expect_identical(attr(compiled_sum, "tccq")$backend, "shlib")
expect_true(file.exists(attr(compiled_sum, "tccq")$dll_path))
expect_true(file.exists(attr(compiled_vec, "tccq")$dll_path))

expect_equal(compiled_sum(x, y), sum((sin(x) + y) * y), tolerance = 1e-10)
expect_equal(compiled_vec(x, y), sin(x) + y * y, tolerance = 1e-10)
expect_equal(compiled_assign(c(1, 2, 3), 2L, 10), c(1, 10, 3))
expect_identical(compiled_fallback(42), 42)
