# test_tccq_boundary_eval.R

fallback_scalar_fn <- function(x) {
  declare(type(x = double()))
  identity(x)
}

fallback_scalar_ir <- tccq_compile(fallback_scalar_fn, mode = "ir", fallback = "auto")
expect_equal(fallback_scalar_ir$ir$result$tag, "boundary_call")
expect_identical(fallback_scalar_ir$ir$result$api, "r_eval")
expect_identical(fallback_scalar_ir$ir$result$type$mode, "double")

fallback_scalar_src <- tccq_compile(fallback_scalar_fn, mode = "code", fallback = "auto")
expect_true(grepl("Rf_eval", fallback_scalar_src, fixed = TRUE))
expect_identical(tccq_compile(fallback_scalar_fn, fallback = "auto")(42), 42)

fallback_vector_fn <- function(x) {
  declare(type(x = double(NA)))
  identity(x)
}

fallback_vector_ir <- tccq_compile(fallback_vector_fn, mode = "ir", fallback = "auto")
expect_equal(fallback_vector_ir$ir$result$tag, "boundary_call")
expect_identical(fallback_vector_ir$ir$result$type$rank, 1L)
expect_equal(tccq_compile(fallback_vector_fn, fallback = "auto")(c(1, 2, 3)), c(1, 2, 3))
