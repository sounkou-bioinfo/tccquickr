# test_tccq2_fusion_barriers.R

fresh_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

mod0 <- tccquickr:::tccq2_frontend(fresh_sum_kernel)
mod0 <- tccquickr:::tccq2_lower_module(mod0)
mod1 <- tccquickr:::tccq2_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq2_pass_validate_ir(),
    tccquickr:::tccq2_pass_effects(),
    tccquickr:::tccq2_pass_kernelize()
  )
)
expect_equal(mod1$kernel$tag, "fold")
expect_equal(mod1$kernel$elem$tag, "materialize")
expect_equal(mod1$kernel$elem$producer$tag, "producer")

mod2 <- tccquickr:::tccq2_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq2_pass_validate_ir(),
    tccquickr:::tccq2_pass_effects(),
    tccquickr:::tccq2_pass_kernelize(),
    tccquickr:::tccq2_pass_fusion()
  )
)
expect_equal(mod2$kernel$tag, "fold")
expect_equal(mod2$kernel$elem$tag, "producer")
expect_equal(mod2$kernel$elem$elem$tag, "binary")

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
