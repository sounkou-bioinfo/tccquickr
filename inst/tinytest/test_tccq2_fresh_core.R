# test_tccq2_fresh_core.R

fresh_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

fresh_vec_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sin(x) + y * y
}

sum_module <- tccquickr:::tccq2_compile(fresh_sum_kernel, mode = "ir")
expect_equal(sum_module$ir$tag, "program")
expect_equal(sum_module$kernel$tag, "fold")
expect_equal(sum_module$kernel$op, "sum")
expect_true(!is.null(sum_module$alloc_plan))
expect_true(!is.null(sum_module$protect_plan))

vec_module <- tccquickr:::tccq2_compile(fresh_vec_kernel, mode = "ir")
expect_equal(vec_module$kernel$tag, "materialize")
expect_equal(vec_module$kernel$producer$tag, "producer")

source_backend <- tccquickr:::tccq2_backend_source()
source_result <- tccquickr:::tccq2_compile(
  fresh_sum_kernel,
  backend = source_backend
)
expect_equal(source_result$backend, "source")
expect_true(is.character(source_result$source))
expect_true(inherits(source_result$module, "tccq2_module"))
