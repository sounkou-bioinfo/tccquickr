# test_tccq2_fusion_barriers.R

fresh_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

mod0 <- tccquickr:::tccq2_frontend(fresh_sum_kernel)
mod0 <- tccquickr:::tccq2_lower_module(mod0)
mod_before <- tccquickr:::tccq2_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq2_pass_validate_ir(),
    tccquickr:::tccq2_pass_effects(),
    tccquickr:::tccq2_pass_kernelize()
  )
)
mod_after <- tccquickr:::tccq2_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq2_pass_validate_ir(),
    tccquickr:::tccq2_pass_effects(),
    tccquickr:::tccq2_pass_kernelize(),
    tccquickr:::tccq2_pass_fusion()
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

compiled_after <- tccquickr:::tccq2_compile(fresh_sum_kernel)
expect_equal(compiled_after(x, y), ref, tolerance = 1e-10)

scalar_kernel <- tccquickr:::tccq2_ir_scalar_kernel(
  tccquickr:::tccq2_ir_const(1, tccquickr:::tccq2_type_scalar("integer"))
)
expect_identical(tccquickr:::tccq2_fuse_kernel(scalar_kernel), scalar_kernel)

boundary_ir <- tccquickr:::tccq2_node(
  "boundary",
  kind = "rf_call",
  reason = "unsupported R call",
  input = tccquickr:::tccq2_ir_var("x", tccquickr:::tccq2_type_vector("double")),
  type = tccquickr:::tccq2_type_vector("double"),
  effect = "boundary"
)
barrier_module <- tccquickr:::tccq2_module(
  entry = "tccq2_entry",
  formal_names = c("x"),
  types = list(x = tccquickr:::tccq2_type_vector("double")),
  expr = quote(x),
  ir = boundary_ir
)
expect_error(
  tccquickr:::tccq2_run_passes(barrier_module),
  pattern = "boundary nodes are explicit legality barriers"
)
