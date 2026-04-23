# test_tccq_fusion_barriers.R

fresh_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

mod0 <- tccquickr:::tccq_frontend(fresh_sum_kernel)
mod0 <- tccquickr:::tccq_lower_module(mod0)
mod_before <- tccquickr:::tccq_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq_pass_validate_ir(),
    tccquickr:::tccq_pass_effects(),
    tccquickr:::tccq_pass_index_normalize(),
    tccquickr:::tccq_pass_kernelize()
  )
)
mod_after <- tccquickr:::tccq_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq_pass_validate_ir(),
    tccquickr:::tccq_pass_effects(),
    tccquickr:::tccq_pass_index_normalize(),
    tccquickr:::tccq_pass_kernelize(),
    tccquickr:::tccq_pass_fusion()
  )
)

expect_equal(mod_before$kernel$tag, "fold")
expect_equal(mod_before$kernel$elem$tag, "materialize")
expect_equal(mod_after$kernel$tag, "fold")
expect_equal(mod_after$kernel$elem$tag, "producer")
expect_equal(mod_after$kernel$domain$vars, mod_after$kernel$elem$domain$vars)

x <- as.double(seq(-2, 2, length.out = 12))
y <- as.double(seq(1, 3, length.out = 12))
ref <- sum((sin(x) + y) * y)

compiled_after <- tccquickr:::tccq_compile(fresh_sum_kernel)
expect_equal(compiled_after(x, y), ref, tolerance = 1e-10)

scalar_kernel <- tccquickr:::tccq_ir_scalar_kernel(
  tccquickr:::tccq_ir_const(1, tccquickr:::tccq_type_scalar("integer"))
)
expect_identical(tccquickr:::tccq_fuse_kernel(scalar_kernel), scalar_kernel)

boundary_ir <- tccquickr:::tccq_ir_boundary_r_eval(
  call_expr = quote(foo(x)),
  args = list(tccquickr:::tccq_ir_var("x", tccquickr:::tccq_type_vector("double"))),
  type = tccquickr:::tccq_type_scalar("logical")
)
barrier_module <- tccquickr:::tccq_module(
  entry = "tccq_entry",
  formal_names = c("x"),
  types = list(x = tccquickr:::tccq_type_vector("double")),
  expr = quote(x),
  ir = boundary_ir,
  fallback = "auto"
)
barrier_out <- tccquickr:::tccq_run_passes(barrier_module)
expect_equal(barrier_out$kernel$tag, "scalar_kernel")
expect_identical(barrier_out$boundary_context$headers, character())
