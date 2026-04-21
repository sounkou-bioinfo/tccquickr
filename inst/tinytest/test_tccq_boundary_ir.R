# test_tccq_boundary_ir.R

unsupported_call_fn <- function(x) {
  declare(type(x = double()))
  foo(x)
}

expect_error(
  tccq_compile(unsupported_call_fn, mode = "ir", fallback = "hard"),
  pattern = "unsupported call in fresh compiler"
)

mod_auto <- tccq_compile(unsupported_call_fn, mode = "ir", fallback = "auto")
expect_equal(mod_auto$ir$tag, "program")
expect_equal(mod_auto$ir$result$tag, "boundary_call")
expect_identical(mod_auto$ir$result$api, "r_eval")
expect_identical(mod_auto$ir$result$barrier, TRUE)
expect_equal(mod_auto$kernel$tag, "scalar_kernel")
expect_identical(mod_auto$kernel$expr$tag, "boundary_call")
expect_identical(mod_auto$boundary_context$headers, character())
expect_identical(mod_auto$boundary_context$libraries, character())
expect_identical(mod_auto$boundary_context$options, character())
