library(tinytest)
library(tccquickr)

runtime <- tccq_runtime_policy(
  mode = "checked",
  allow_interrupts = TRUE,
  check_interval = 1024L,
  emit_debug_sites = TRUE
)
context <- tccq_backend_context(
  mode = "jit",
  target = "c",
  runtime = runtime
)
backend <- tccq_rtinycc_backend()

expect_true(S7::S7_inherits(runtime, TccqRuntimePolicy))
expect_true(S7::S7_inherits(context, TccqBackendContext))
expect_true(S7::S7_inherits(backend, TccqBackendSpec))
expect_true(s7contract::has_trait(backend, TccqBackend))
expect_equal(backend@id, "rtinycc")
expect_equal(backend@family, "c")
expect_equal(backend@target, "c")
expect_true("jit" %in% backend@modes)
expect_true("buffer_bridge" %in% backend@capabilities)

bad_runtime <- tryCatch(
  TccqRuntimePolicy(
    mode = "interactive",
    allow_interrupts = TRUE,
    check_interval = 1024L,
    emit_debug_sites = FALSE,
    attrs = list()
  ),
  error = identity
)
expect_true(inherits(bad_runtime, "error"))

fortran_backend <- tccq_fortran_backend()
graph_backend <- tccq_anvil_graph_backend()
object_backend <- tccq_r_object_backend()
core_backends <- tccq_core_backends()

expect_equal(fortran_backend@family, "fortran")
expect_equal(graph_backend@family, "graph")
expect_equal(object_backend@family, "object")
expect_true("openmp" %in% fortran_backend@capabilities)
expect_true("stablehlo" %in% graph_backend@capabilities)
expect_true("object_mode" %in% object_backend@capabilities)
expect_true(length(core_backends) >= 4L)

span <- tccq_source_span(file = "README.Rmd", line = 1L, column = 1L)
site <- tccq_debug_site("dbg1", "value", source = span)
safepoint <- tccq_safepoint(
  "sp1",
  "loop_backedge",
  region_id = "r1",
  requires_rapi = FALSE
)

expect_true(S7::S7_inherits(span, TccqSourceSpan))
expect_true(S7::S7_inherits(site, TccqDebugSite))
expect_true(S7::S7_inherits(safepoint, TccqSafepoint))
expect_equal(site@source@file, "README.Rmd")
expect_false(safepoint@requires_rapi)

bad_source_span <- tryCatch(
  TccqSourceSpan(
    file = "",
    line = 10L,
    column = 1L,
    end_line = 9L,
    end_column = 1L,
    label = ""
  ),
  error = identity
)
expect_true(inherits(bad_source_span, "error"))

double_n <- tccq_type("double", tccq_shape("n"))
buffer_n <- tccq_type("buffer", tccq_shape("n"))
bridge <- tccq_bridge_plan(
  "b1",
  "sexp_to_buffer",
  from_space = "r",
  to_space = "host",
  from_type = double_n,
  to_type = buffer_n,
  effect = tccq_effect(reads = TRUE, allocates = TRUE)
)

expect_true(S7::S7_inherits(bridge, TccqBridgePlan))
expect_equal(bridge@kind, "sexp_to_buffer")
expect_equal(bridge@to_type@base, "buffer")

region <- tccq_region(
  "r1",
  "kernel",
  memory_space = "host",
  touches_rapi = FALSE
)
program <- tccq_program(
  "backend_probe",
  formals = list(x = tccq_binding("x", double_n)),
  regions = list(region)
)

plan <- tccq_backend_plan(
  "manual.plan",
  backend_id = backend@id,
  family = backend@family,
  mode = "jit",
  target = "c",
  capabilities = backend@capabilities,
  regions = list(region),
  bridges = list(bridge),
  safepoints = list(safepoint),
  debug_sites = list(site)
)

expect_true(S7::S7_inherits(plan, TccqBackendPlan))
expect_equal(plan@backend_id, "rtinycc")
expect_equal(plan@family, "c")
expect_equal(length(plan@bridges), 1L)
expect_equal(length(plan@safepoints), 1L)
expect_equal(length(plan@debug_sites), 1L)

bad_backend_plan <- tryCatch(
  TccqBackendPlan(
    id = "bad.plan",
    backend_id = backend@id,
    family = "c",
    mode = "jit",
    target = "c",
    capabilities = "jit",
    regions = list(region),
    bridges = list("not a bridge plan"),
    safepoints = list(safepoint),
    debug_sites = list(site),
    diagnostics = list(),
    attrs = list()
  ),
  error = identity
)
expect_true(inherits(bad_backend_plan, "error"))

bad_backend_spec <- tryCatch(
  TccqBackendSpec(
    id = "bad",
    family = "c",
    target = "c",
    driver = "bad-driver",
    modes = "jit",
    region_kinds = "kernel",
    memory_spaces = "host",
    capabilities = "imaginary_capability",
    uses_rapi = FALSE,
    attrs = list(),
    prepare = function(backend, program, context) {
      tccq_result(success = FALSE)
    }
  ),
  error = identity
)
expect_true(inherits(bad_backend_spec, "error"))

planned <- tccq_plan_backend(program, backend, context)
expect_false(planned@success)
expect_true(S7::S7_inherits(planned@value, TccqBackendPlan))
expect_equal(planned@value@backend_id, "rtinycc")
expect_equal(planned@value@family, "c")
expect_equal(planned@value@mode, "jit")
expect_true(any(vapply(
  planned@diagnostics,
  function(x) identical(x@code, "backend.lowering_absent"),
  logical(1)
)))

missing_expression <- tccq_expression_tree(program)
expect_false(missing_expression@success)
expect_true(any(vapply(
  missing_expression@diagnostics,
  function(x) identical(x@code, "expression.missing_result"),
  logical(1)
)))

missing_capability <- tccq_plan_backend(
  program,
  backend,
  tccq_backend_context(
    mode = "jit",
    target = "c",
    required_capabilities = "device_memory"
  )
)
expect_false(missing_capability@success)
expect_true(any(vapply(
  missing_capability@diagnostics,
  function(x) identical(x@code, "backend.missing_capability"),
  logical(1)
)))

unsupported_mode <- tccq_plan_backend(
  program,
  backend,
  tccq_backend_context(mode = "shared_library", target = "c")
)
expect_false(unsupported_mode@success)
expect_true(any(vapply(
  unsupported_mode@diagnostics,
  function(x) identical(x@code, "backend.unsupported_mode"),
  logical(1)
)))

bad_bridge <- tryCatch(
  tccq_bridge_plan("bad", "sexp_to_buffer", "r", "gpu"),
  error = identity
)
expect_true(inherits(bad_bridge, "tccq_error"))

compile_probe <- function(x) {
  declare(type(x = double(n)))
  sqrt(x)
}
compiled <- tccq_compile(compile_probe, strict = FALSE)
expect_false(compiled@success)
expect_true(S7::S7_inherits(compiled@value, TccqBackendPlanSet))
expect_true(length(compiled@value@plans) >= 4L)
expect_true(any(vapply(
  compiled@diagnostics,
  function(x) identical(x@code, "backend.lowering_absent"),
  logical(1)
)))

vector_add <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  x + y
}

vector_program <- tccq_analyze(vector_add)
expect_true(vector_program@success)
expect_true(vector_program@value@attrs$lowered)

expression_result <- tccq_expression_tree(vector_program@value)
expect_true(expression_result@success)
expect_true(S7::S7_inherits(expression_result@value, TccqExpression))
expect_equal(expression_result@value@kind, "operation")
expect_equal(expression_result@value@op, "+")
expect_true(S7::S7_inherits(expression_result@value@resolved_op, TccqResolvedOp))

c_source_plan <- tccq_plan_backend(
  vector_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(c_source_plan@success)
expect_equal(c_source_plan@value@attrs$source_language, "c")
expect_true(grepl("double \\*", c_source_plan@value@attrs$source))
expect_true(grepl("input_0001", c_source_plan@value@attrs$source, fixed = TRUE))
expect_true(S7::S7_inherits(c_source_plan@value@attrs$expression, TccqExpression))
expect_true(S7::S7_inherits(c_source_plan@value@attrs$storage_plan, TccqStoragePlan))
expect_true(S7::S7_inherits(c_source_plan@value@attrs$function_interface, TccqBackendFunctionInterface))
expect_equal(c_source_plan@value@attrs$function_interface@kind, "map")
expect_true(c_source_plan@value@attrs$function_interface@needs_length)
expect_equal(c_source_plan@value@attrs$function_interface@parameter_value_ids, c("formal_0001", "formal_0002"))
expect_equal(length(c_source_plan@value@bridges), 3L)

negation_program <- function(x) {
  declare(type(x = double(n)))
  -x
}
negation_program <- tccq_analyze(negation_program)
expect_true(negation_program@success)
expect_true(negation_program@value@attrs$lowered)
negation_c_plan <- tccq_plan_backend(
  negation_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(negation_c_plan@success)
expect_true(grepl("(-input_0001[index_0001])", negation_c_plan@value@attrs$source, fixed = TRUE))
negation_fortran_plan <- tccq_plan_backend(
  negation_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(negation_fortran_plan@success)
expect_true(grepl("(-input_0001(index_0001))", negation_fortran_plan@value@attrs$source, fixed = TRUE))

square <- function(x) x
square_registry <- tccq_op_registry_add(
  tccq_default_op_registry(),
  tccq_op_impl(
    "square",
    target = "pure_c",
    region_kind = "kernel",
    render = function(operands, context) sprintf("(%s * %s)", operands[[1L]], operands[[1L]]),
    elementwise = tccq_elementwise_spec(
      "square",
      1L,
      result_type = function(input_types) input_types[[1L]]
    )
  )
)
custom_elementwise <- function(x) {
  declare(type(x = double(n)))
  square(x)
}
custom_elementwise_program <- tccq_analyze(custom_elementwise, registry = square_registry)
expect_true(custom_elementwise_program@success)
expect_true(custom_elementwise_program@value@attrs$lowered)
custom_elementwise_c_plan <- tccq_plan_backend(
  custom_elementwise_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(custom_elementwise_c_plan@success)
expect_true(grepl(
  "(input_0001[index_0001] * input_0001[index_0001])",
  custom_elementwise_c_plan@value@attrs$source,
  fixed = TRUE
))
custom_elementwise_fortran_plan <- tccq_plan_backend(
  custom_elementwise_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(custom_elementwise_fortran_plan@success)
expect_true(grepl(
  "(input_0001(index_0001) * input_0001(index_0001))",
  custom_elementwise_fortran_plan@value@attrs$source,
  fixed = TRUE
))

unhandled_values <- vector_program@value@values
unhandled_result <- unhandled_values[[vector_program@value@result]]
unhandled_values[[vector_program@value@result]] <- tccq_value(
  unhandled_result@id,
  "cos",
  inputs = unhandled_result@inputs,
  type = unhandled_result@type,
  effect = unhandled_result@effect,
  attrs = unhandled_result@attrs
)
unhandled_program <- tccq_program(
  "unhandled_expression",
  formals = vector_program@value@formals,
  values = unhandled_values,
  regions = vector_program@value@regions,
  result = vector_program@value@result,
  storage_plan = vector_program@value@storage_plan,
  attrs = vector_program@value@attrs
)
unhandled_source_plan <- tccq_plan_backend(
  unhandled_program,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_false(unhandled_source_plan@success)
expect_true(any(vapply(
  unhandled_source_plan@diagnostics,
  function(x) identical(x@code, "expression.operation_mismatch"),
  logical(1)
)))

fortran_source_plan <- tccq_plan_backend(
  vector_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(fortran_source_plan@success)
expect_equal(fortran_source_plan@value@attrs$source_language, "fortran")
expect_true(grepl("subroutine", fortran_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("iso_c_binding", fortran_source_plan@value@attrs$source, fixed = TRUE))
expect_true(S7::S7_inherits(fortran_source_plan@value@attrs$expression, TccqExpression))
expect_true(S7::S7_inherits(fortran_source_plan@value@attrs$function_interface, TccqBackendFunctionInterface))
expect_equal(fortran_source_plan@value@attrs$function_interface@kind, "map")
expect_equal(fortran_source_plan@value@attrs$function_interface@source_language, "fortran")
expect_equal(length(fortran_source_plan@value@bridges), 3L)

bound_chain <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  shifted <- sqrt(x)
  weighted <- exp(shifted) * y
  weighted + y
}

bound_program <- tccq_analyze(bound_chain)
expect_true(bound_program@success)
expect_true(bound_program@value@attrs$lowered)

bound_c_source_plan <- tccq_plan_backend(
  bound_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(bound_c_source_plan@success)
expect_true(grepl("exp(sqrt(input_0001[index_0001]))", bound_c_source_plan@value@attrs$source, fixed = TRUE))

bound_fortran_source_plan <- tccq_plan_backend(
  bound_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(bound_fortran_source_plan@success)
expect_true(grepl("exp(sqrt(input_0001(index_0001)))", bound_fortran_source_plan@value@attrs$source, fixed = TRUE))

if (requireNamespace("Rtinycc", quietly = TRUE)) {
  jit_plan <- tccq_plan_backend(
    vector_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(jit_plan@success)
  expect_equal(jit_plan@diagnostics, list())
  expect_equal(jit_plan@value@attrs$callable(c(1, 2), c(3, 4)), c(4, 6))

  negation_jit_plan <- tccq_plan_backend(
    negation_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(negation_jit_plan@success)
  expect_equal(negation_jit_plan@value@attrs$callable(c(2, -3, 5)), c(-2, 3, -5))

  custom_elementwise_jit_plan <- tccq_plan_backend(
    custom_elementwise_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(custom_elementwise_jit_plan@success)
  expect_equal(
    custom_elementwise_jit_plan@value@attrs$callable(c(2, 3, 5)),
    c(4, 9, 25)
  )
}

map_reduce <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  sum(exp(x) * y)
}

reduction_program <- tccq_analyze(map_reduce)
expect_true(reduction_program@success)
expect_true(reduction_program@value@attrs$lowered)

reduction_expression <- tccq_expression_tree(reduction_program@value)
expect_true(reduction_expression@success)
expect_equal(reduction_expression@value@op, "sum")
expect_equal(reduction_expression@value@attrs$lowering, "reduction")

reduction_c_source_plan <- tccq_plan_backend(
  reduction_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(reduction_c_source_plan@success)
expect_equal(reduction_c_source_plan@value@attrs$source_language, "c")
expect_true(grepl("double accumulator_0001 = 0.0;", reduction_c_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("for (int index_0001 = 0;", reduction_c_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("accumulator_0001 = accumulator_0001 + ", reduction_c_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("int length_0001", reduction_c_source_plan@value@attrs$source, fixed = TRUE))
expect_true(S7::S7_inherits(
  reduction_c_source_plan@value@attrs$function_interface,
  TccqBackendFunctionInterface
))
expect_equal(reduction_c_source_plan@value@attrs$function_interface@kind, "reduction")
expect_equal(reduction_c_source_plan@value@attrs$function_interface@accumulator_name, "accumulator_0001")
expect_equal(length(reduction_c_source_plan@value@bridges), 3L)

reduction_fortran_source_plan <- tccq_plan_backend(
  reduction_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(reduction_fortran_source_plan@success)
expect_equal(reduction_fortran_source_plan@value@attrs$source_language, "fortran")
expect_true(grepl("function", reduction_fortran_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("integer(c_int), value :: length_0001", reduction_fortran_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("output = 0.0_c_double", reduction_fortran_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("do index_0001 = 1, length_0001", reduction_fortran_source_plan@value@attrs$source, fixed = TRUE))
expect_true(grepl("output = output + ", reduction_fortran_source_plan@value@attrs$source, fixed = TRUE))
expect_equal(length(reduction_fortran_source_plan@value@bridges), 3L)

fold_add <- function(x) x
fold_add_registry <- tccq_op_registry_add(
  tccq_default_op_registry(),
  tccq_op_impl(
    "fold_add",
    target = "pure_c",
    region_kind = "kernel",
    effect = tccq_effect(reads = TRUE),
    reduction = tccq_reduction_spec(
      "fold_add",
      identity = function(type) tccq_literal_finite(0, type = type),
      combine = function(accumulator, value, context) sprintf("%s + %s", accumulator, value)
    )
  )
)
custom_reduce <- function(x) {
  declare(type(x = double(n)))
  fold_add(exp(x))
}
custom_reduction_program <- tccq_analyze(custom_reduce, registry = fold_add_registry)
expect_true(custom_reduction_program@success)
expect_true(custom_reduction_program@value@attrs$lowered)
custom_c_source_plan <- tccq_plan_backend(
  custom_reduction_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(custom_c_source_plan@success)
expect_true(grepl("accumulator_0001 = accumulator_0001 + ", custom_c_source_plan@value@attrs$source, fixed = TRUE))

if (requireNamespace("Rtinycc", quietly = TRUE)) {
  reduction_jit_plan <- tccq_plan_backend(
    reduction_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(reduction_jit_plan@success)
  x <- c(0, log(2), log(3))
  y <- c(5, 7, 11)
  expect_equal(
    reduction_jit_plan@value@attrs$callable(x, y),
    sum(exp(x) * y)
  )
  custom_reduction_jit_plan <- tccq_plan_backend(
    custom_reduction_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(custom_reduction_jit_plan@success)
  expect_equal(
    custom_reduction_jit_plan@value@attrs$callable(x),
    sum(exp(x))
  )
}
