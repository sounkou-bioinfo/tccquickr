# test_tccq_boundary_ir.R

unsupported_call_fn <- function(x) {
  declare(type(x = double()))
  tccq_test_boundary_foo(x)
}

expect_error(
  tccq_compile(unsupported_call_fn, mode = "ir", fallback = "hard"),
  pattern = "unsupported call in tccq"
)
expect_error(
  tccq_compile(unsupported_call_fn, mode = "ir"),
  pattern = "unsupported call in tccq"
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
expect_identical(length(mod_auto$boundary_diagnostics), 1L)
expect_identical(mod_auto$boundary_diagnostics[[1L]]$api, "r_eval")
expect_identical(mod_auto$boundary_diagnostics[[1L]]$original_call, "tccq_test_boundary_foo(x)")

assign("tccq_test_boundary_foo", function(x) x, envir = .GlobalEnv)
compiled_auto <- tccq_compile(unsupported_call_fn, fallback = "auto")
expect_identical(length(attr(compiled_auto, "tccq")$boundaries), 1L)
rm("tccq_test_boundary_foo", envir = .GlobalEnv)
