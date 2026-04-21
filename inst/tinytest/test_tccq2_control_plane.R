# test_tccq2_control_plane.R

simple_sum_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum(x + y)
}

mod0 <- tccquickr:::tccq2_frontend(simple_sum_fn)
mod0 <- tccquickr:::tccq2_lower_module(mod0)

bad_pass <- tccquickr:::tccq2_pass(
  name = "bad",
  requires = "nonexistent",
  run = function(module, facts) module
)
expect_error(
  tccquickr:::tccq2_run_passes(mod0, passes = list(bad_pass)),
  pattern = "missing required facts"
)

bad_effect_module <- tccquickr:::tccq2_module_with(
  mod0,
  ir = tccquickr:::tccq2_node(
    "const",
    value = 1,
    type = tccquickr:::tccq2_type_scalar("integer"),
    effect = "unknown"
  )
)
expect_error(
  tccquickr:::tccq2_pass_effects()$run(bad_effect_module, list(ir_valid = TRUE)),
  pattern = "unsupported effect"
)

extlib <- tccquickr:::tccq2_external_library(
  name = "libm",
  headers = c("#include <math.h>", "#include <math.h>"),
  libraries = c("m", "m"),
  symbols = list(sin = "double(double)")
)
ctx <- tccquickr:::tccq2_context_from_extlibs(list(extlib))
expect_equal(ctx$headers, "#include <math.h>")
expect_equal(ctx$libraries, "m")
expect_true("sin" %in% names(ctx$external_symbols))

source_backend <- tccquickr:::tccq2_backend_source()
source_result <- tccq2_compile(simple_sum_fn, backend = source_backend, extlibs = list(extlib))
expect_equal(source_result$backend, "source")
expect_true(grepl("#include <R.h>", source_result$source, fixed = TRUE))

expect_error(
  tccquickr:::tccq2_module_validate(
    tccquickr:::tccq2_module(
      entry = 1,
      formal_names = c("x"),
      types = list(x = tccquickr:::tccq2_type_vector("double")),
      expr = quote(x)
    )
  ),
  pattern = "module entry must be character"
)

expect_error(
  tccquickr:::tccq2_module_validate(
    tccquickr:::tccq2_module(
      entry = "e",
      formal_names = c("x", "y"),
      types = list(x = tccquickr:::tccq2_type_vector("double")),
      expr = quote(x + y)
    )
  ),
  pattern = "formal/type mismatch"
)

expect_error(
  tccquickr:::tccq2_module_validate(
    tccquickr:::tccq2_module(
      entry = "e",
      formal_names = c("x"),
      types = list(x = tccquickr:::tccq2_type_vector("double")),
      expr = quote(x),
      body_exprs = list()
    )
  ),
  pattern = "at least one body expression"
)

matrix_decl_fn <- function(x) {
  declare(type(x = double(NA, NA)))
  x
}
expect_error(
  tccq2_compile(matrix_decl_fn, mode = "code"),
  pattern = "supports scalar/vector only"
)
