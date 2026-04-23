# test_tccq_control_plane.R

simple_sum_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum(x + y)
}

mod0 <- tccquickr:::tccq_frontend(simple_sum_fn)
mod0 <- tccquickr:::tccq_lower_module(mod0)

bad_pass <- tccquickr:::tccq_pass(
  name = "bad",
  requires = "nonexistent",
  run = function(module, facts) module
)
expect_error(
  tccquickr:::tccq_run_passes(mod0, passes = list(bad_pass)),
  pattern = "missing required facts"
)

bad_effect_module <- tccquickr:::tccq_module_with(
  mod0,
  ir = tccquickr:::tccq_node(
    "const",
    value = 1,
    type = tccquickr:::tccq_type_scalar("integer"),
    effect = "unknown"
  )
)
expect_error(
  tccquickr:::tccq_pass_effects()$run(bad_effect_module, list(ir_valid = TRUE)),
  pattern = "unsupported effect"
)

extlib <- tccquickr:::tccq_external_library(
  name = "libm",
  headers = c("#include <math.h>", "#include <math.h>"),
  libraries = c("m", "m"),
  symbols = list(sin = "double(double)")
)
ctx <- tccquickr:::tccq_context_from_extlibs(list(extlib))
expect_equal(ctx$headers, "#include <math.h>")
expect_equal(ctx$libraries, "m")
expect_true("sin" %in% names(ctx$external_symbols))

source_backend <- tccquickr:::tccq_backend_source()
source_result <- tccq_compile(simple_sum_fn, backend = source_backend, extlibs = list(extlib))
expect_equal(source_result$backend, "source")
expect_true(is.character(source_result$source))
expect_true(nzchar(source_result$source))

expect_error(
  tccquickr:::tccq_module_validate(
    tccquickr:::tccq_module(
      entry = 1,
      formal_names = c("x"),
      types = list(x = tccquickr:::tccq_type_vector("double")),
      expr = quote(x)
    )
  ),
  pattern = "module entry must be character"
)

expect_error(
  tccquickr:::tccq_module_validate(
    tccquickr:::tccq_module(
      entry = "e",
      formal_names = c("x", "y"),
      types = list(x = tccquickr:::tccq_type_vector("double")),
      expr = quote(x + y)
    )
  ),
  pattern = "formal/type mismatch"
)

expect_error(
  tccquickr:::tccq_module_validate(
    tccquickr:::tccq_module(
      entry = "e",
      formal_names = c("x"),
      types = list(x = tccquickr:::tccq_type_vector("double")),
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
  tccq_compile(matrix_decl_fn, mode = "code"),
  pattern = "supports scalar/vector only"
)

header_only_extlib <- tccquickr:::tccq_external_library(
  name = "demo_header",
  headers = "#include <demo.h>"
)
headerless_backend <- tccquickr:::tccq_backend(
  name = "headerless",
  capabilities = list(c = TRUE, compile = TRUE, r_api = TRUE),
  compile = function(module, target, ctx = list()) {
    list(backend = "headerless", source = "", compiled = NULL, callable = NULL, module = module)
  }
)
expect_error(
  tccq_compile(simple_sum_fn, backend = headerless_backend, extlibs = list(header_only_extlib)),
  pattern = "compile context field 'headers'"
)

boundaryless_backend <- tccquickr:::tccq_backend(
  name = "boundaryless",
  capabilities = list(
    c = TRUE,
    compile = TRUE,
    r_api = TRUE,
    headers = TRUE,
    include_paths = TRUE,
    library_paths = TRUE,
    libraries = TRUE,
    options = TRUE,
    boundary_apis = character()
  ),
  compile = function(module, target, ctx = list()) {
    list(backend = "boundaryless", source = "", compiled = NULL, callable = NULL, module = module)
  }
)

fallback_scalar_fn <- function(x) {
  declare(type(x = double()))
  identity(x)
}
expect_error(
  tccq_compile(fallback_scalar_fn, backend = boundaryless_backend, fallback = "auto"),
  pattern = "does not support boundary APIs"
)
