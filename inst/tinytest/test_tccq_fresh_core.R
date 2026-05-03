# test_tccq_fresh_core.R

fresh_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

fresh_vec_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sin(x) + y * y
}

sum_module <- tccquickr:::tccq_compile(fresh_sum_kernel, mode = "ir")
expect_equal(sum_module$ir$tag, "program")
expect_equal(sum_module$kernel$tag, "fold")
expect_equal(sum_module$kernel$op, "sum")
expect_true(!is.null(sum_module$alloc_plan))
expect_true(!is.null(sum_module$protect_plan))
expect_identical(sum_module$protect_plan$strategy, "dynamic_counter")
expect_true(sum_module$protect_plan$max_live_upper_bound >= 1L)
expect_false("n_protect" %in% names(sum_module$protect_plan))

vec_module <- tccquickr:::tccq_compile(fresh_vec_kernel, mode = "ir")
expect_equal(vec_module$kernel$tag, "materialize")
expect_equal(vec_module$kernel$producer$tag, "producer")

source_backend <- tccq_backend_source()
source_result <- tccquickr:::tccq_compile(
  fresh_sum_kernel,
  backend = source_backend
)
expect_equal(source_result$backend, "source")
expect_true(is.character(source_result$source))
expect_true(inherits(source_result$module, "tccq_module"))

float_unary_minus_call_kernel <- function(x) {
  declare(type(x = double(NA)))
  prod(-cos(x))
}

float_neg_code <- tccquickr:::tccq_compile(float_unary_minus_call_kernel, mode = "code")
expect_false(grepl("(-(cos", float_neg_code, fixed = TRUE))
expect_true(grepl("((-1.0) * (double)(cos", float_neg_code, fixed = TRUE))
expect_equal(
  tccquickr:::tccq_compile(float_unary_minus_call_kernel)(as.double(1:3)),
  prod(-cos(as.double(1:3))),
  tolerance = 1e-10
)
