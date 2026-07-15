library(tinytest)
library(tccquickr)

context <- tccq_backend_context(mode = "jit", target = "c")
backend <- tccq_rtinycc_backend()

backend_products <- function(plan_result) plan_result@value@products
backend_source <- function(plan_result) backend_products(plan_result)@attrs$source
backend_callable <- function(plan_result) backend_products(plan_result)@attrs$callable
backend_artifacts <- function(plan_result) backend_products(plan_result)@artifacts
backend_interface <- function(plan_result) backend_products(plan_result)@function_interface

can_build_shared_library <- function(language) {
  config_key <- switch(language, c = "CC", fortran = "FC", "")
  if (!nzchar(config_key)) {
    return(FALSE)
  }
  r_command <- file.path(R.home("bin"), "R")
  if (!file.exists(r_command)) {
    return(FALSE)
  }
  compiler <- tryCatch(
    suppressWarnings(system2(
      r_command,
      c("CMD", "config", config_key),
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(e) character()
  )
  status <- attr(compiler, "status")
  if (!is.null(status) && !identical(as.integer(status), 0L)) {
    return(FALSE)
  }
  compiler <- trimws(paste(compiler, collapse = " "))
  nzchar(compiler) && !compiler %in% c("false", "no") &&
    !grepl("not found|ERROR", compiler, ignore.case = TRUE)
}

# TinyCC's in-memory JIT can crash the whole process on platforms with strict
# W^X code-page policies (for example macOS on ARM64), and a segfault cannot
# be caught in-process. Probe a trivial compile-and-call in a subprocess and
# skip JIT coverage when the subprocess dies.
can_jit_with_rtinycc <- function() {
  if (!requireNamespace("Rtinycc", quietly = TRUE)) {
    return(FALSE)
  }
  probe_path <- tempfile("tccq_jit_probe_", fileext = ".R")
  writeLines(c(
    "ffi <- Rtinycc::tcc_ffi()",
    "ffi <- Rtinycc::tcc_bind(",
    "  ffi,",
    "  tccq_probe_add = list(args = list(\"f64\", \"f64\"), returns = \"f64\")",
    ")",
    "ffi <- Rtinycc::tcc_source(",
    "  ffi,",
    "  \"double tccq_probe_add(double a, double b) { return a + b; }\"",
    ")",
    "compiled <- Rtinycc::tcc_compile(ffi)",
    "stopifnot(identical(compiled[[\"tccq_probe_add\"]](1, 2), 3))",
    "cat(\"tccq-jit-ok\")"
  ), probe_path)
  on.exit(unlink(probe_path), add = TRUE)
  output <- tryCatch(
    suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"),
      c("--vanilla", probe_path),
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(e) character()
  )
  status <- attr(output, "status")
  (is.null(status) || identical(as.integer(status), 0L)) &&
    any(grepl("tccq-jit-ok", output, fixed = TRUE))
}

rtinycc_jit_available <- can_jit_with_rtinycc()

expect_true(S7::S7_inherits(context, TccqBackendContext))
expect_true(S7::S7_inherits(backend, TccqBackendSpec))
expect_true(s7contract::has_trait(backend, TccqBackend))
expect_equal(backend@id, "rtinycc")
expect_equal(backend@family, "c")
expect_equal(backend@target, "c")
expect_true("jit" %in% backend@modes)
expect_true("buffer_bridge" %in% backend@capabilities)

fortran_backend <- tccq_fortran_backend()
core_backends <- tccq_core_backends()

expect_equal(fortran_backend@family, "fortran")
expect_true("shared_library" %in% fortran_backend@capabilities)
expect_equal(names(core_backends), c("rtinycc", "c", "fortran"))
expect_equal(names(tccq_core_backends(include_rtinycc = FALSE)), c("c", "fortran"))

artifact <- tccq_backend_artifact(
  "artifact.source",
  "source",
  source_language = "c",
  attrs = list(text = "double probe(void) { return 1.0; }")
)
expect_true(S7::S7_inherits(artifact, TccqBackendArtifact))
expect_equal(artifact@kind, "source")

bad_artifact <- tryCatch(
  tccq_backend_artifact("artifact.bad", "shared_library", source_language = "c"),
  error = identity
)
expect_true(inherits(bad_artifact, "error"))

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
  bridges = list(bridge)
)

expect_true(S7::S7_inherits(plan, TccqBackendPlan))
expect_equal(plan@backend_id, "rtinycc")
expect_equal(plan@family, "c")
expect_equal(length(plan@bridges), 1L)

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
    products = NULL,
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
    required_capabilities = "shared_library"
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
expect_true(compiled@success)
expect_true(S7::S7_inherits(compiled@value, TccqBackendPlanSet))
expect_equal(length(compiled@value@plans), 3L)
compiled_plan_succeeded <- vapply(
  compiled@value@plans,
  function(plan) length(plan@diagnostics) == 0L,
  logical(1)
)
expect_true(any(compiled_plan_succeeded))

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
expect_true(S7::S7_inherits(expression_result@value@operation, TccqLoweredOperation))
expect_true(S7::S7_inherits(
  expression_result@value@operation@resolved_op,
  TccqResolvedOp
))
expect_identical(
  expression_result@value@effect,
  expression_result@value@operation@resolved_op@effect
)

c_source_plan <- tccq_plan_backend(
  vector_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(c_source_plan@success)
expect_equal(c_source_plan@value@attrs$source_language, "c")
c_products <- backend_products(c_source_plan)
c_artifacts <- backend_artifacts(c_source_plan)
c_interface <- backend_interface(c_source_plan)
c_source <- backend_source(c_source_plan)
expect_true(S7::S7_inherits(c_products, TccqBackendProducts))
expect_true(grepl("double \\*", c_source))
expect_true(grepl("input_0001", c_source, fixed = TRUE))
expect_true(S7::S7_inherits(c_products@body, TccqExpression))
expect_true(S7::S7_inherits(c_products@storage_plan, TccqStoragePlan))
expect_true(S7::S7_inherits(c_interface, TccqBackendFunctionInterface))
expect_true(S7::S7_inherits(c_artifacts$source, TccqBackendArtifact))
expect_equal(c_artifacts$source@kind, "source")
expect_equal(c_artifacts$source@attrs$text, c_source)
expect_equal(c_interface@kind, "loop_nest")
expect_equal(c_interface@abi, "c")
expect_equal(c_interface@result_placement, "return")
expect_equal(c_interface@result_name, "output")
expect_true(S7::S7_inherits(c_interface@domain, TccqDomain))
expect_equal(c_interface@domain@shape@rank, 1L)
expect_equal(vapply(c_interface@extents, function(binding) binding@symbol, character(1)), "n")
expect_equal(
  vapply(c_interface@extents, function(binding) binding@source_name, character(1)),
  "extent_n"
)
expect_equal(c_interface@index_names, "axis_0001")
expect_equal(c_interface@result_count_name, "result_count_0001")
expect_equal(
  vapply(c_interface@parameters, function(binding) binding@value_id, character(1)),
  c("formal_0001", "formal_0002")
)
expect_true(S7::S7_inherits(c_products@loop_nest, TccqLoopNest))
expect_equal(
  vapply(c_products@loop_nest@axes, function(axis) axis@role, character(1)),
  "map"
)
expect_equal(length(c_source_plan@value@bridges), 3L)
expect_equal(
  vapply(c_source_plan@value@bridges, function(bridge) bridge@kind, character(1)),
  c("sexp_to_buffer", "sexp_to_buffer", "buffer_to_sexp")
)

if (can_build_shared_library("c")) {
  c_shared_plan <- tccq_plan_backend(
    vector_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(c_shared_plan@success)
  expect_equal(c_shared_plan@value@mode, "shared_library")
  c_shared_products <- backend_products(c_shared_plan)
  c_shared_artifacts <- backend_artifacts(c_shared_plan)
  expect_true(file.exists(c_shared_products@attrs$source_path))
  expect_true(file.exists(c_shared_products@attrs$shared_library_path))
  expect_true(S7::S7_inherits(
    c_shared_artifacts$source,
    TccqBackendArtifact
  ))
  expect_true(S7::S7_inherits(
    c_shared_artifacts$shared_library,
    TccqBackendArtifact
  ))
  expect_equal(c_shared_artifacts$shared_library@kind, "shared_library")
  expect_true(S7::S7_inherits(
    c_shared_artifacts$native_callable,
    TccqBackendArtifact
  ))
  expect_equal(c_shared_artifacts$native_callable@kind, "native_callable")
  expect_equal(backend_callable(c_shared_plan)(c(1, 2), c(3, 4)), c(4, 6))
}

matrix_add <- function(x, y) {
  declare(type(x = double(n, p), y = double(n, p)))
  sqrt(x) + y
}

matrix_program <- tccq_analyze(matrix_add)
expect_true(matrix_program@success)
expect_true(matrix_program@value@attrs$lowered)
matrix_result <- matrix_program@value@values[[matrix_program@value@result]]
expect_equal(matrix_result@type@shape@rank, 2L)

matrix_c_source_plan <- tccq_plan_backend(
  matrix_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(matrix_c_source_plan@success)
matrix_c_interface <- backend_interface(matrix_c_source_plan)
expect_equal(matrix_c_interface@kind, "loop_nest")
expect_equal(matrix_c_interface@result_type@shape@rank, 2L)
expect_true(S7::S7_inherits(matrix_c_interface@domain, TccqDomain))
expect_equal(matrix_c_interface@domain@shape@rank, 2L)
expect_equal(
  vapply(matrix_c_interface@extents, function(binding) binding@symbol, character(1)),
  c("n", "p")
)
expect_equal(
  vapply(matrix_c_interface@extents, function(binding) binding@source_name, character(1)),
  c("extent_n", "extent_p")
)
expect_equal(matrix_c_interface@index_names, c("axis_0001", "axis_0002"))
expect_true(S7::S7_inherits(backend_products(matrix_c_source_plan)@body, TccqExpression))
expect_equal(backend_products(matrix_c_source_plan)@body@type@shape@rank, 2L)
expect_equal(
  vapply(matrix_c_source_plan@value@bridges, function(bridge) bridge@kind, character(1)),
  c("sexp_to_buffer", "sexp_to_buffer", "buffer_to_sexp")
)

matrix_fortran_source_plan <- tccq_plan_backend(
  matrix_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(matrix_fortran_source_plan@success)
matrix_fortran_interface <- backend_interface(matrix_fortran_source_plan)
expect_equal(matrix_fortran_interface@kind, "loop_nest")
expect_equal(matrix_fortran_interface@result_type@shape@rank, 2L)
expect_true(S7::S7_inherits(matrix_fortran_interface@domain, TccqDomain))
expect_equal(matrix_fortran_interface@domain@shape@rank, 2L)
expect_equal(
  vapply(matrix_fortran_interface@extents, function(binding) binding@source_name, character(1)),
  c("extent_n", "extent_p")
)

matrix_x <- matrix(c(1, 4, 9, 16), nrow = 2)
matrix_y <- matrix(c(10, 20, 30, 40), nrow = 2)
matrix_expected <- sqrt(matrix_x) + matrix_y

if (can_build_shared_library("c")) {
  matrix_c_shared_plan <- tccq_plan_backend(
    matrix_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(matrix_c_shared_plan@success)
  matrix_c_value <- backend_callable(matrix_c_shared_plan)(matrix_x, matrix_y)
  expect_equal(matrix_c_value, matrix_expected)
  expect_equal(dim(matrix_c_value), dim(matrix_x))
}

if (can_build_shared_library("fortran")) {
  matrix_fortran_shared_plan <- tccq_plan_backend(
    matrix_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(matrix_fortran_shared_plan@success)
  matrix_fortran_value <- backend_callable(matrix_fortran_shared_plan)(matrix_x, matrix_y)
  expect_equal(matrix_fortran_value, matrix_expected)
  expect_equal(dim(matrix_fortran_value), dim(matrix_x))
}

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
expect_true(grepl("(-input_0001[axis_0001])", backend_source(negation_c_plan), fixed = TRUE))
negation_fortran_plan <- tccq_plan_backend(
  negation_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(negation_fortran_plan@success)
expect_true(grepl("(-input_0001(axis_0001 + 1))", backend_source(negation_fortran_plan), fixed = TRUE))

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
  "(input_0001[axis_0001] * input_0001[axis_0001])",
  backend_source(custom_elementwise_c_plan),
  fixed = TRUE
))
custom_elementwise_fortran_plan <- tccq_plan_backend(
  custom_elementwise_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(custom_elementwise_fortran_plan@success)
expect_true(grepl(
  "(input_0001(axis_0001 + 1) * input_0001(axis_0001 + 1))",
  backend_source(custom_elementwise_fortran_plan),
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
  schedule = vector_program@value@schedule,
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
fortran_products <- backend_products(fortran_source_plan)
fortran_artifacts <- backend_artifacts(fortran_source_plan)
fortran_interface <- backend_interface(fortran_source_plan)
fortran_source <- backend_source(fortran_source_plan)
expect_true(grepl("subroutine", fortran_source, fixed = TRUE))
expect_true(grepl("iso_c_binding", fortran_source, fixed = TRUE))
expect_true(S7::S7_inherits(fortran_products@body, TccqExpression))
expect_true(S7::S7_inherits(fortran_interface, TccqBackendFunctionInterface))
expect_true(S7::S7_inherits(fortran_artifacts$source, TccqBackendArtifact))
expect_equal(fortran_interface@kind, "loop_nest")
expect_equal(fortran_interface@source_language, "fortran")
expect_equal(fortran_interface@abi, "fortran_bind_c")
expect_equal(fortran_interface@result_placement, "output_argument")
expect_equal(fortran_interface@result_name, "output")
expect_equal(length(fortran_source_plan@value@bridges), 3L)

if (can_build_shared_library("fortran")) {
  fortran_shared_plan <- tccq_plan_backend(
    vector_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(fortran_shared_plan@success)
  expect_equal(fortran_shared_plan@value@mode, "shared_library")
  fortran_shared_products <- backend_products(fortran_shared_plan)
  fortran_shared_artifacts <- backend_artifacts(fortran_shared_plan)
  expect_true(file.exists(fortran_shared_products@attrs$source_path))
  expect_true(file.exists(fortran_shared_products@attrs$shared_library_path))
  expect_true(S7::S7_inherits(
    fortran_shared_artifacts$shared_library,
    TccqBackendArtifact
  ))
  expect_equal(fortran_shared_artifacts$shared_library@source_language, "fortran")
  expect_true(S7::S7_inherits(
    fortran_shared_artifacts$native_callable,
    TccqBackendArtifact
  ))
  expect_equal(fortran_shared_artifacts$native_callable@kind, "native_callable")
  expect_equal(backend_callable(fortran_shared_plan)(c(1, 2), c(3, 4)), c(4, 6))
}

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
expect_true(grepl(
  "intermediate_0001[axis_0001] = sqrt(input_0001[axis_0001]);",
  backend_source(bound_c_source_plan),
  fixed = TRUE
))
expect_true(grepl(
  "exp(intermediate_0001[axis_0001])",
  backend_source(bound_c_source_plan),
  fixed = TRUE
))

bound_fortran_source_plan <- tccq_plan_backend(
  bound_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(bound_fortran_source_plan@success)
expect_true(grepl(
  "intermediate_0001(axis_0001 + 1) = sqrt(input_0001(axis_0001 + 1))",
  backend_source(bound_fortran_source_plan),
  fixed = TRUE
))
expect_true(grepl(
  "exp(intermediate_0001(axis_0001 + 1))",
  backend_source(bound_fortran_source_plan),
  fixed = TRUE
))

fused_local_chain <- function(x) {
  declare(type(x = double(n)))
  transformed <- exp(x)
  exp(transformed)
}

fused_local_program <- tccq_analyze(fused_local_chain, strict = TRUE)@value
fused_local_input <- c(-1, 0, 1)
fused_local_expected <- exp(exp(fused_local_input))
fused_local_c <- tccq_plan_backend(fused_local_program, tccq_c_backend())
fused_local_fortran <- tccq_plan_backend(
  fused_local_program,
  tccq_fortran_backend()
)
expect_true(fused_local_c@success)
expect_true(fused_local_fortran@success)
expect_false(grepl("intermediate_0001", backend_source(fused_local_c), fixed = TRUE))
expect_false(grepl("intermediate_0001", backend_source(fused_local_fortran), fixed = TRUE))
expect_true(grepl(
  "exp(exp(input_0001[axis_0001]))",
  backend_source(fused_local_c),
  fixed = TRUE
))
expect_true(grepl(
  "exp(exp(input_0001(axis_0001 + 1)))",
  backend_source(fused_local_fortran),
  fixed = TRUE
))

if (rtinycc_jit_available) {
  jit_plan <- tccq_plan_backend(
    vector_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(jit_plan@success)
  expect_equal(jit_plan@diagnostics, list())
  expect_equal(backend_callable(jit_plan)(c(1, 2), c(3, 4)), c(4, 6))
  expect_true(S7::S7_inherits(backend_artifacts(jit_plan)$jit_callable, TccqBackendArtifact))
  expect_equal(backend_artifacts(jit_plan)$jit_callable@kind, "jit_callable")

  negation_jit_plan <- tccq_plan_backend(
    negation_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(negation_jit_plan@success)
  expect_equal(backend_callable(negation_jit_plan)(c(2, -3, 5)), c(-2, 3, -5))

  custom_elementwise_jit_plan <- tccq_plan_backend(
    custom_elementwise_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(custom_elementwise_jit_plan@success)
  expect_equal(
    backend_callable(custom_elementwise_jit_plan)(c(2, 3, 5)),
    c(4, 9, 25)
  )

  matrix_jit_plan <- tccq_plan_backend(
    matrix_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(matrix_jit_plan@success)
  matrix_jit_value <- backend_callable(matrix_jit_plan)(matrix_x, matrix_y)
  expect_equal(matrix_jit_value, matrix_expected)
  expect_equal(dim(matrix_jit_value), dim(matrix_x))

  fused_local_jit <- tccq_plan_backend(
    fused_local_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(fused_local_jit@success)
  expect_equal(
    backend_callable(fused_local_jit)(fused_local_input),
    fused_local_expected
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
expect_true(S7::S7_inherits(reduction_expression@value@operation, TccqLoweredOperation))
expect_equal(reduction_expression@value@operation@family, "reduction")

reduction_c_source_plan <- tccq_plan_backend(
  reduction_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(reduction_c_source_plan@success)
expect_equal(reduction_c_source_plan@value@attrs$source_language, "c")
reduction_c_source <- backend_source(reduction_c_source_plan)
reduction_c_interface <- backend_interface(reduction_c_source_plan)
expect_true(grepl("double accumulator_0001 = 0.0;", reduction_c_source, fixed = TRUE))
expect_true(grepl("for (int axis_0001 = 0; axis_0001 < extent_n;", reduction_c_source, fixed = TRUE))
expect_true(grepl("accumulator_0001 = accumulator_0001 + ", reduction_c_source, fixed = TRUE))
expect_true(grepl("int extent_n", reduction_c_source, fixed = TRUE))
expect_true(S7::S7_inherits(
  reduction_c_interface,
  TccqBackendFunctionInterface
))
expect_equal(reduction_c_interface@kind, "loop_nest")
expect_equal(reduction_c_interface@result_placement, "return")
reduction_c_accumulator_id <- backend_products(
  reduction_c_source_plan
)@loop_nest@accumulator@value_id
expect_equal(
  vapply(reduction_c_interface@locals, function(binding) binding@source_name, character(1))[[match(
    reduction_c_accumulator_id,
    vapply(reduction_c_interface@locals, function(binding) binding@value_id, character(1))
  )]],
  "accumulator_0001"
)
expect_true(S7::S7_inherits(reduction_c_interface@domain, TccqDomain))
expect_equal(reduction_c_interface@domain@shape@rank, 1L)
expect_equal(
  vapply(reduction_c_interface@extents, function(binding) binding@source_name, character(1)),
  "extent_n"
)
expect_equal(reduction_c_interface@result_count_name, "")
expect_equal(length(reduction_c_interface@result_dims), 0L)
expect_equal(length(reduction_c_source_plan@value@bridges), 3L)
expect_equal(
  vapply(reduction_c_source_plan@value@bridges, function(bridge) bridge@kind, character(1)),
  c("sexp_to_buffer", "sexp_to_buffer", "scalar_to_sexp")
)

reduction_fortran_source_plan <- tccq_plan_backend(
  reduction_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(reduction_fortran_source_plan@success)
expect_equal(reduction_fortran_source_plan@value@attrs$source_language, "fortran")
reduction_fortran_source <- backend_source(reduction_fortran_source_plan)
reduction_fortran_interface <- backend_interface(reduction_fortran_source_plan)
expect_true(grepl("function", reduction_fortran_source, fixed = TRUE))
expect_true(grepl("integer(c_int), value :: extent_n", reduction_fortran_source, fixed = TRUE))
expect_true(grepl("accumulator_0001 = 0.0_c_double", reduction_fortran_source, fixed = TRUE))
expect_true(grepl("do axis_0001 = 0, extent_n - 1", reduction_fortran_source, fixed = TRUE))
expect_true(grepl("accumulator_0001 = accumulator_0001 + ", reduction_fortran_source, fixed = TRUE))
expect_true(grepl("output = accumulator_0001", reduction_fortran_source, fixed = TRUE))
expect_equal(reduction_fortran_interface@abi, "fortran_bind_c")
expect_equal(reduction_fortran_interface@result_placement, "return")
expect_equal(reduction_fortran_interface@result_name, "output")
expect_equal(length(reduction_fortran_source_plan@value@bridges), 3L)

matrix_reduce <- function(x, y) {
  declare(type(x = double(n, p), y = double(n, p)))
  sum(exp(x) * y)
}

matrix_reduction_program <- tccq_analyze(matrix_reduce)
expect_true(matrix_reduction_program@success)
expect_true(matrix_reduction_program@value@attrs$lowered)
matrix_reduction_result <- matrix_reduction_program@value@values[[matrix_reduction_program@value@result]]
expect_equal(matrix_reduction_result@type@shape@rank, 0L)
matrix_reduction_domain <- matrix_reduction_program@value@regions[[1L]]@fusion_groups[[1L]]@domain
expect_equal(matrix_reduction_domain@shape@rank, 2L)

matrix_reduction_c_source_plan <- tccq_plan_backend(
  matrix_reduction_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(matrix_reduction_c_source_plan@success)
matrix_reduction_c_interface <- backend_interface(matrix_reduction_c_source_plan)
expect_equal(matrix_reduction_c_interface@kind, "loop_nest")
expect_equal(matrix_reduction_c_interface@result_type@shape@rank, 0L)
expect_true(S7::S7_inherits(matrix_reduction_c_interface@domain, TccqDomain))
expect_equal(matrix_reduction_c_interface@domain@shape@rank, 2L)
expect_equal(
  vapply(
    matrix_reduction_c_interface@extents,
    function(binding) binding@source_name,
    character(1)
  ),
  c("extent_n", "extent_p")
)

matrix_reduction_fortran_source_plan <- tccq_plan_backend(
  matrix_reduction_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(matrix_reduction_fortran_source_plan@success)
matrix_reduction_fortran_interface <- backend_interface(matrix_reduction_fortran_source_plan)
expect_equal(matrix_reduction_fortran_interface@kind, "loop_nest")
expect_equal(matrix_reduction_fortran_interface@result_type@shape@rank, 0L)
expect_true(S7::S7_inherits(matrix_reduction_fortran_interface@domain, TccqDomain))
expect_equal(matrix_reduction_fortran_interface@domain@shape@rank, 2L)
expect_equal(
  vapply(
    matrix_reduction_fortran_interface@extents,
    function(binding) binding@source_name,
    character(1)
  ),
  c("extent_n", "extent_p")
)

matrix_reduction_expected <- sum(exp(matrix_x) * matrix_y)

column_axis_reduce <- function(x) {
  declare(type(x = double(n, p)))
  colSums(exp(x))
}

row_axis_reduce <- function(x) {
  declare(type(x = double(n, p)))
  rowSums(exp(x))
}

column_axis_program <- tccq_analyze(column_axis_reduce)
row_axis_program <- tccq_analyze(row_axis_reduce)
expect_true(column_axis_program@success)
expect_true(row_axis_program@success)

column_axis_c_source_plan <- tccq_plan_backend(
  column_axis_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(column_axis_c_source_plan@success)
column_axis_c_source <- backend_source(column_axis_c_source_plan)
column_axis_c_interface <- backend_interface(column_axis_c_source_plan)
expect_equal(column_axis_c_interface@kind, "loop_nest")
expect_equal(column_axis_c_interface@domain@shape@rank, 2L)
expect_equal(
  vapply(column_axis_c_interface@extents, function(binding) binding@source_name, character(1)),
  c("extent_n", "extent_p")
)
expect_equal(column_axis_c_interface@result_count_name, "result_count_0001")
column_axis_accumulator_id <- backend_products(
  column_axis_c_source_plan
)@loop_nest@accumulator@value_id
expect_equal(
  vapply(column_axis_c_interface@locals, function(binding) binding@source_name, character(1))[[match(
    column_axis_accumulator_id,
    vapply(column_axis_c_interface@locals, function(binding) binding@value_id, character(1))
  )]],
  "accumulator_0001"
)
expect_equal(
  vapply(column_axis_c_interface@result_dims, function(dim) dim@label, character(1)),
  "p"
)
expect_true(grepl("for (int axis_0002 = 0; axis_0002 < extent_p;", column_axis_c_source, fixed = TRUE))
expect_true(grepl("for (int axis_0001 = 0; axis_0001 < extent_n;", column_axis_c_source, fixed = TRUE))
expect_true(grepl(
  "input_0001[axis_0001 + axis_0002 * extent_n]",
  column_axis_c_source,
  fixed = TRUE
))
expect_true(grepl("output[axis_0002] = accumulator_0001;", column_axis_c_source, fixed = TRUE))

row_axis_fortran_source_plan <- tccq_plan_backend(
  row_axis_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(row_axis_fortran_source_plan@success)
row_axis_fortran_source <- backend_source(row_axis_fortran_source_plan)
row_axis_fortran_interface <- backend_interface(row_axis_fortran_source_plan)
expect_equal(row_axis_fortran_interface@kind, "loop_nest")
expect_equal(row_axis_fortran_interface@result_placement, "output_argument")
expect_equal(row_axis_fortran_interface@result_count_name, "result_count_0001")
expect_equal(
  vapply(row_axis_fortran_interface@result_dims, function(dim) dim@label, character(1)),
  "n"
)
expect_true(grepl("do axis_0001 = 0, extent_n - 1", row_axis_fortran_source, fixed = TRUE))
expect_true(grepl("do axis_0002 = 0, extent_p - 1", row_axis_fortran_source, fixed = TRUE))
expect_true(grepl(
  "input_0001(axis_0001 + axis_0002 * extent_n + 1)",
  row_axis_fortran_source,
  fixed = TRUE
))
expect_true(grepl("output(axis_0001 + 1) = accumulator_0001", row_axis_fortran_source, fixed = TRUE))

column_axis_expected <- colSums(exp(matrix_x))
row_axis_expected <- rowSums(exp(matrix_x))

if (can_build_shared_library("c")) {
  fused_local_c_shared <- tccq_plan_backend(
    fused_local_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(fused_local_c_shared@success)
  expect_equal(
    backend_callable(fused_local_c_shared)(fused_local_input),
    fused_local_expected
  )

  reduction_c_shared_plan <- tccq_plan_backend(
    reduction_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(reduction_c_shared_plan@success)
  expect_equal(backend_callable(reduction_c_shared_plan)(c(0, log(2)), c(5, 7)), 19)

  matrix_reduction_c_shared_plan <- tccq_plan_backend(
    matrix_reduction_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(matrix_reduction_c_shared_plan@success)
  expect_equal(
    backend_callable(matrix_reduction_c_shared_plan)(matrix_x, matrix_y),
    matrix_reduction_expected
  )

  column_axis_c_shared_plan <- tccq_plan_backend(
    column_axis_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  row_axis_c_shared_plan <- tccq_plan_backend(
    row_axis_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(column_axis_c_shared_plan@success)
  expect_true(row_axis_c_shared_plan@success)
  column_axis_c_value <- backend_callable(column_axis_c_shared_plan)(matrix_x)
  row_axis_c_value <- backend_callable(row_axis_c_shared_plan)(matrix_x)
  expect_equal(column_axis_c_value, column_axis_expected)
  expect_equal(row_axis_c_value, row_axis_expected)
  expect_true(is.null(dim(column_axis_c_value)))
  expect_true(is.null(dim(row_axis_c_value)))
}

if (can_build_shared_library("fortran")) {
  fused_local_fortran_shared <- tccq_plan_backend(
    fused_local_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(fused_local_fortran_shared@success)
  expect_equal(
    backend_callable(fused_local_fortran_shared)(fused_local_input),
    fused_local_expected
  )

  reduction_fortran_shared_plan <- tccq_plan_backend(
    reduction_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(reduction_fortran_shared_plan@success)
  expect_equal(backend_callable(reduction_fortran_shared_plan)(c(0, log(2)), c(5, 7)), 19)

  matrix_reduction_fortran_shared_plan <- tccq_plan_backend(
    matrix_reduction_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(matrix_reduction_fortran_shared_plan@success)
  expect_equal(
    backend_callable(matrix_reduction_fortran_shared_plan)(matrix_x, matrix_y),
    matrix_reduction_expected
  )

  column_axis_fortran_shared_plan <- tccq_plan_backend(
    column_axis_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  row_axis_fortran_shared_plan <- tccq_plan_backend(
    row_axis_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(column_axis_fortran_shared_plan@success)
  expect_true(row_axis_fortran_shared_plan@success)
  expect_equal(backend_callable(column_axis_fortran_shared_plan)(matrix_x), column_axis_expected)
  expect_equal(backend_callable(row_axis_fortran_shared_plan)(matrix_x), row_axis_expected)
}

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
expect_true(grepl("accumulator_0001 = accumulator_0001 + ", backend_source(custom_c_source_plan), fixed = TRUE))

if (rtinycc_jit_available) {
  reduction_jit_plan <- tccq_plan_backend(
    reduction_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(reduction_jit_plan@success)
  x <- c(0, log(2), log(3))
  y <- c(5, 7, 11)
  expect_equal(
    backend_callable(reduction_jit_plan)(x, y),
    sum(exp(x) * y)
  )
  matrix_reduction_jit_plan <- tccq_plan_backend(
    matrix_reduction_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(matrix_reduction_jit_plan@success)
  expect_equal(
    backend_callable(matrix_reduction_jit_plan)(matrix_x, matrix_y),
    matrix_reduction_expected
  )
  column_axis_jit_plan <- tccq_plan_backend(
    column_axis_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  row_axis_jit_plan <- tccq_plan_backend(
    row_axis_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(column_axis_jit_plan@success)
  expect_true(row_axis_jit_plan@success)
  expect_equal(backend_callable(column_axis_jit_plan)(matrix_x), column_axis_expected)
  expect_equal(backend_callable(row_axis_jit_plan)(matrix_x), row_axis_expected)
  custom_reduction_jit_plan <- tccq_plan_backend(
    custom_reduction_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(custom_reduction_jit_plan@success)
  expect_equal(
    backend_callable(custom_reduction_jit_plan)(x),
    sum(exp(x))
  )
}

# R's `if` remains a lazy branch in neutral IR. Both source families emit a
# statement-valued conditional, while the callable ABI carries the logical
# condition as a typed scalar rather than pretending every input is double.
branch_map <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  if (flag) x else -x
}
branch_program <- tccq_analyze(branch_map, strict = TRUE)@value
branch_expression <- tccq_expression_tree(branch_program)
branch_nests <- tccq_program_loop_nests(branch_program)

expect_true(branch_expression@success)
expect_equal(branch_expression@value@kind, "branch")
expect_true(S7::S7_inherits(branch_expression@value@branch, TccqBranch))
expect_true(branch_nests@success)
expect_true(S7::S7_inherits(branch_nests@value[[1L]]@body, TccqValueBlock))
expect_true(S7::S7_inherits(
  branch_nests@value[[1L]]@body@statements[[1L]],
  TccqConditional
))

branch_c_plan <- tccq_plan_backend(
  branch_program,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
branch_fortran_plan <- tccq_plan_backend(
  branch_program,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)

expect_true(branch_c_plan@success)
expect_true(branch_fortran_plan@success)
branch_c_interface <- backend_interface(branch_c_plan)
branch_fortran_interface <- backend_interface(branch_fortran_plan)
expect_equal(
  vapply(
    branch_c_interface@parameters,
    function(binding) binding@source_type@base,
    character(1)
  ),
  c("double", "logical")
)
expect_equal(branch_c_interface@result_type@base, "double")
expect_equal(branch_c_interface@result_placement, "output_argument")
expect_equal(branch_fortran_interface@parameters[[2L]]@source_type@base, "logical")
expect_true(S7::S7_inherits(branch_c_interface@error_channel, TccqBackendErrorChannel))
expect_equal(
  branch_c_interface@error_channel@diagnostics[[1L]]@code,
  "runtime.invalid_logical_condition"
)
expect_true(grepl("int input_0002", backend_source(branch_c_plan), fixed = TRUE))
expect_true(grepl("void tccq_", backend_source(branch_c_plan), fixed = TRUE))
expect_true(grepl("int *status_0001, double *output", backend_source(branch_c_plan), fixed = TRUE))
expect_true(grepl("condition_value = input_0002", backend_source(branch_c_plan), fixed = TRUE))
expect_true(grepl("integer(c_int), value :: input_0002", backend_source(branch_fortran_plan), fixed = TRUE))
expect_true(grepl("condition_value = input_0002", backend_source(branch_fortran_plan), fixed = TRUE))

logical_branch <- function(flag) {
  declare(type(flag = logical()))
  if (flag) TRUE else FALSE
}
logical_branch_program <- tccq_analyze(logical_branch, strict = TRUE)@value
logical_branch_c <- tccq_plan_backend(
  logical_branch_program,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
logical_branch_fortran <- tccq_plan_backend(
  logical_branch_program,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(logical_branch_c@success)
expect_true(logical_branch_fortran@success)
expect_equal(backend_interface(logical_branch_c)@result_type@base, "logical")
expect_true(grepl("int tccq_", backend_source(logical_branch_c), fixed = TRUE))
expect_true(grepl("integer(c_int) :: output", backend_source(logical_branch_fortran), fixed = TRUE))

scalar_less <- function(x, y) {
  declare(type(x = double(), y = double()))
  x < y
}
scalar_less_program <- tccq_analyze(scalar_less, strict = TRUE)@value
scalar_less_c <- tccq_plan_backend(
  scalar_less_program,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
scalar_less_fortran <- tccq_plan_backend(
  scalar_less_program,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(scalar_less_c@success)
expect_true(scalar_less_fortran@success)
expect_equal(backend_interface(scalar_less_c)@result_type@base, "logical")
expect_true(grepl("input_0001 < input_0002", backend_source(scalar_less_c), fixed = TRUE))
expect_true(grepl("input_0001 < input_0002", backend_source(scalar_less_fortran), fixed = TRUE))
expect_true(grepl("TCCQ_NA_LOGICAL", backend_source(scalar_less_c), fixed = TRUE))
expect_true(grepl("ieee_is_nan", backend_source(scalar_less_fortran), fixed = TRUE))

triangular_recurrence <- function(n) {
  declare(type(n = double()))
  iteration <- 0
  total <- 0
  while (iteration < n) {
    iteration <- iteration + 1
    total <- total + iteration
  }
  total
}
triangular_program <- tccq_analyze(triangular_recurrence, strict = TRUE)@value
triangular_c <- tccq_plan_backend(
  triangular_program,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
triangular_fortran <- tccq_plan_backend(
  triangular_program,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(triangular_c@success)
expect_true(triangular_fortran@success)
expect_equal(backend_interface(triangular_c)@kind, "structured")
expect_equal(backend_interface(triangular_fortran)@kind, "structured")
expect_true(S7::S7_inherits(backend_products(triangular_c)@body, TccqValueBlock))
expect_null(backend_products(triangular_c)@loop_nest)
expect_equal(length(backend_products(triangular_c)@loop_nests), 0L)
expect_true(grepl("while (", backend_source(triangular_c), fixed = TRUE))
expect_true(grepl("condition_value == TCCQ_NA_LOGICAL", backend_source(triangular_c), fixed = TRUE))
expect_true(grepl("condition_value == tccq_na_logical", backend_source(triangular_fortran), fixed = TRUE))
expect_true(grepl("if (condition_value == 0_c_int) exit", backend_source(triangular_fortran), fixed = TRUE))

conditional_recurrence <- function(n, pivot) {
  declare(type(n = double(), pivot = double()))
  iteration <- 0
  total <- 0
  while (iteration < n) {
    iteration <- iteration + 1
    if (iteration <= pivot) {
      total <- total + iteration
    } else {
      total <- total - iteration
    }
  }
  total
}
conditional_program <- tccq_analyze(conditional_recurrence, strict = TRUE)@value
conditional_c <- tccq_plan_backend(
  conditional_program,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
conditional_fortran <- tccq_plan_backend(
  conditional_program,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(conditional_c@success)
expect_true(conditional_fortran@success)
conditional_while <- Filter(
  function(statement) S7::S7_inherits(statement, TccqWhile),
  backend_products(conditional_c)@body@statements
)[[1L]]
expect_true(any(vapply(
  conditional_while@body@statements,
  S7::S7_inherits,
  logical(1),
  class = TccqIf
)))
expect_true(grepl("while (1)", backend_source(conditional_c), fixed = TRUE))
expect_true(grepl("if (condition_value != 0)", backend_source(conditional_c), fixed = TRUE))
expect_true(grepl("if (condition_value /= 0_c_int) then", backend_source(conditional_fortran), fixed = TRUE))

nonfinite_branch <- function(flag) {
  declare(type(flag = logical(), returns = double()))
  if (flag) NaN else Inf
}
nonfinite_branch_program <- tccq_analyze(nonfinite_branch, strict = TRUE)@value
for (backend in list(tccq_c_backend(), tccq_fortran_backend())) {
  nonfinite_plan <- tccq_plan_backend(nonfinite_branch_program, backend)
  expect_false(nonfinite_plan@success)
  expect_true(any(vapply(
    nonfinite_plan@diagnostics,
    function(diagnostic) identical(diagnostic@code, "backend.unsupported_literal_kind"),
    logical(1)
  )))
}

nested_branch <- function(x, flag, other) {
  declare(type(x = double(n), flag = logical(), other = logical()))
  if (flag) if (other) x else -x else if (other) -x else x
}
nested_branch_program <- tccq_analyze(nested_branch, strict = TRUE)@value
nested_branch_nests <- tccq_program_loop_nests(nested_branch_program)
expect_true(nested_branch_nests@success)
nested_branch_body <- nested_branch_nests@value[[1L]]@body
expect_true(S7::S7_inherits(nested_branch_body, TccqValueBlock))
nested_branch_statement <- nested_branch_body@statements[[1L]]
expect_true(S7::S7_inherits(nested_branch_statement, TccqConditional))
expect_true(S7::S7_inherits(
  nested_branch_statement@consequent@statements[[1L]],
  TccqConditional
))
expect_true(S7::S7_inherits(
  nested_branch_statement@alternative@statements[[1L]],
  TccqConditional
))

nested_branch_c <- tccq_plan_backend(nested_branch_program, tccq_c_backend())
nested_branch_fortran <- tccq_plan_backend(nested_branch_program, tccq_fortran_backend())
expect_true(nested_branch_c@success)
expect_true(nested_branch_fortran@success)
expect_true(grepl(
  "condition_value = input_0002",
  backend_source(nested_branch_c),
  fixed = TRUE
))
expect_true(grepl(
  "condition_value = input_0003",
  backend_source(nested_branch_c),
  fixed = TRUE
))
expect_true(grepl(
  "condition_value = input_0002",
  backend_source(nested_branch_fortran),
  fixed = TRUE
))
expect_true(grepl(
  "condition_value = input_0003",
  backend_source(nested_branch_fortran),
  fixed = TRUE
))

branch_condition <- function(x, flag, other) {
  declare(type(x = double(n), flag = logical(), other = logical()))
  if (if (flag) other else flag) x else -x
}
branch_condition_program <- tccq_analyze(branch_condition, strict = TRUE)@value
branch_condition_nests <- tccq_program_loop_nests(branch_condition_program)
expect_true(branch_condition_nests@success)
branch_condition_body <- branch_condition_nests@value[[1L]]@body
expect_true(S7::S7_inherits(branch_condition_body, TccqValueBlock))
expect_equal(length(branch_condition_body@locals), 1L)
expect_true(S7::S7_inherits(branch_condition_body@locals[[1L]], TccqWriteTarget))
expect_equal(branch_condition_body@locals[[1L]]@type@base, "logical")
expect_true(S7::S7_inherits(branch_condition_body@statements[[1L]], TccqConditional))
expect_true(S7::S7_inherits(branch_condition_body@statements[[2L]], TccqConditional))
expect_true(S7::S7_inherits(
  branch_condition_body@statements[[1L]]@consequent@statements[[1L]],
  TccqAssignment
))

branch_condition_c <- tccq_plan_backend(branch_condition_program, tccq_c_backend())
branch_condition_fortran <- tccq_plan_backend(
  branch_condition_program,
  tccq_fortran_backend()
)
expect_true(branch_condition_c@success)
expect_true(branch_condition_fortran@success)
expect_true(S7::S7_inherits(backend_products(branch_condition_c)@body, TccqValueBlock))
expect_equal(
  vapply(
    backend_interface(branch_condition_c)@locals,
    function(binding) binding@source_name,
    character(1)
  ),
  "local_0001"
)
expect_equal(
  backend_interface(branch_condition_c)@locals[[1L]]@source_type@base,
  "logical"
)
expect_true(grepl("int local_0001;", backend_source(branch_condition_c), fixed = TRUE))
expect_true(grepl("condition_value = local_0001", backend_source(branch_condition_c), fixed = TRUE))
expect_true(grepl(
  "integer(c_int) :: local_0001",
  backend_source(branch_condition_fortran),
  fixed = TRUE
))
expect_true(grepl(
  "condition_value = local_0001",
  backend_source(branch_condition_fortran),
  fixed = TRUE
))

# Conditional values nested under ordinary elementwise operations normalize to
# block-owned scalar storage without losing their full semantic array types.
conditional_scalar <- function(flag) {
  declare(type(flag = logical()))
  (if (flag) 2 else -2) + 1
}
conditional_composition <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  exp((if (flag) x else -x) + 1)
}
shared_conditional_value <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  selected <- if (flag) x else -x
  selected + selected
}
shared_conditional_subtrees <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  selected <- if (flag) x else -x
  exp(selected) + sqrt(selected * selected)
}
conditional_scalar_program <- tccq_analyze(conditional_scalar, strict = TRUE)@value
conditional_composition_program <- tccq_analyze(
  conditional_composition,
  strict = TRUE
)@value
shared_conditional_program <- tccq_analyze(
  shared_conditional_value,
  strict = TRUE
)@value
shared_conditional_subtrees_program <- tccq_analyze(
  shared_conditional_subtrees,
  strict = TRUE
)@value
conditional_scalar_nests <- tccq_program_loop_nests(conditional_scalar_program)
conditional_composition_nests <- tccq_program_loop_nests(
  conditional_composition_program
)
shared_conditional_nests <- tccq_program_loop_nests(shared_conditional_program)
shared_conditional_subtrees_nests <- tccq_program_loop_nests(
  shared_conditional_subtrees_program
)
expect_true(conditional_scalar_nests@success)
expect_true(conditional_composition_nests@success)
expect_true(shared_conditional_nests@success)
expect_true(shared_conditional_subtrees_nests@success)
expect_equal(length(shared_conditional_nests@value[[1L]]@body@locals), 0L)
expect_equal(length(shared_conditional_nests@value[[1L]]@body@statements), 1L)
expect_true(S7::S7_inherits(
  shared_conditional_nests@value[[1L]]@body@statements[[1L]],
  TccqConditional
))
expect_equal(
  sum(vapply(
    shared_conditional_subtrees_nests@value[[1L]]@body@statements,
    S7::S7_inherits,
    logical(1),
    class = TccqConditional
  )),
  1L
)

conditional_composition_body <- conditional_composition_nests@value[[1L]]@body
expect_true(S7::S7_inherits(conditional_composition_body, TccqValueBlock))
expect_true(conditional_composition_body@effect@may_error)
expect_equal(length(conditional_composition_body@locals), 2L)
expect_equal(
  vapply(
    conditional_composition_body@locals,
    function(target) target@type@shape@rank,
    integer(1)
  ),
  c(1L, 1L)
)
expect_equal(
  vapply(
    conditional_composition_body@locals,
    function(target) target@storage_type@shape@rank,
    integer(1)
  ),
  c(0L, 0L)
)
expect_true(S7::S7_inherits(
  conditional_composition_body@statements[[1L]],
  TccqConditional
))
expect_true(S7::S7_inherits(
  conditional_composition_body@statements[[2L]],
  TccqAssignment
))
expect_true(S7::S7_inherits(
  conditional_composition_body@statements[[3L]],
  TccqAssignment
))

conditional_composition_c <- tccq_plan_backend(
  conditional_composition_program,
  tccq_c_backend()
)
conditional_composition_fortran <- tccq_plan_backend(
  conditional_composition_program,
  tccq_fortran_backend()
)
expect_true(conditional_composition_c@success)
expect_true(conditional_composition_fortran@success)
expect_equal(
  vapply(
    backend_interface(conditional_composition_c)@locals,
    function(binding) binding@source_type@shape@rank,
    integer(1)
  ),
  c(0L, 0L)
)
expect_true(grepl("double local_0001;", backend_source(conditional_composition_c), fixed = TRUE))
expect_true(grepl("double local_0002;", backend_source(conditional_composition_c), fixed = TRUE))
expect_true(grepl("block", backend_source(conditional_composition_fortran), fixed = TRUE))
expect_true(grepl(
  "real(c_double) :: local_0002",
  backend_source(conditional_composition_fortran),
  fixed = TRUE
))

branch_reduction <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  if (flag) sum(x) else 0
}
branch_reduction_program <- tccq_analyze(branch_reduction, strict = TRUE)@value
branch_reduction_groups <- branch_reduction_program@regions[[1L]]@fusion_groups
branch_reduction_contract <- branch_reduction_groups[[length(branch_reduction_groups)]]@contract
expect_true(S7::S7_inherits(branch_reduction_contract@result_value, TccqBranch))
expect_equal(length(branch_reduction_contract@operations), 0L)
expect_null(branch_reduction_contract@result_operation)
branch_reduction_nests <- tccq_program_loop_nests(branch_reduction_program)
expect_true(branch_reduction_nests@success)
expect_equal(length(branch_reduction_nests@value), 2L)
branch_reduction_guard <- branch_reduction_nests@value[[1L]]@guards[[1L]]
expect_true(S7::S7_inherits(branch_reduction_guard, TccqLoopGuard))
expect_true(branch_reduction_guard@selected)
expect_equal(branch_reduction_guard@condition@reference@access@kind, "scalar")
expect_equal(length(branch_reduction_nests@value[[2L]]@guards), 0L)

branch_reduction_both <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  if (flag) sum(x) else sum(-x)
}
branch_reduction_both_program <- tccq_analyze(
  branch_reduction_both,
  strict = TRUE
)@value
branch_reduction_both_nests <- tccq_program_loop_nests(
  branch_reduction_both_program
)
expect_true(branch_reduction_both_nests@success)
expect_equal(length(branch_reduction_both_nests@value), 3L)
expect_equal(
  vapply(
    branch_reduction_both_nests@value[1:2],
    function(nest) nest@guards[[1L]]@selected,
    logical(1)
  ),
  c(TRUE, FALSE)
)

nested_branch_reduction <- function(x, primary, secondary) {
  declare(type(x = double(n), primary = logical(), secondary = logical()))
  if (primary) if (secondary) sum(x) else sum(-x) else 0
}
nested_branch_reduction_program <- tccq_analyze(
  nested_branch_reduction,
  strict = TRUE
)@value
nested_branch_reduction_nests <- tccq_program_loop_nests(
  nested_branch_reduction_program
)
expect_true(nested_branch_reduction_nests@success)
expect_equal(length(nested_branch_reduction_nests@value), 3L)
expect_equal(
  lapply(nested_branch_reduction_nests@value[1:2], function(nest) {
    vapply(nest@guards, function(guard) guard@selected, logical(1))
  }),
  list(c(TRUE, TRUE), c(TRUE, FALSE))
)

guarded_buffer_reduction <- function(x, flag) {
  declare(type(x = double(n, p), flag = logical()))
  if (flag) colSums(x) else -colSums(x)
}
guarded_buffer_program <- tccq_analyze(
  guarded_buffer_reduction,
  strict = TRUE
)@value
guarded_buffer_nests <- tccq_program_loop_nests(guarded_buffer_program)
expect_true(guarded_buffer_nests@success)
expect_equal(length(guarded_buffer_nests@value), 3L)
expect_equal(
  vapply(
    guarded_buffer_nests@value[1:2],
    function(nest) nest@guards[[1L]]@selected,
    logical(1)
  ),
  c(TRUE, FALSE)
)
expect_equal(
  vapply(
    guarded_buffer_nests@value[1:2],
    function(nest) nest@storage@type@shape@rank,
    integer(1)
  ),
  c(1L, 1L)
)
expect_equal(
  length(unique(vapply(
    guarded_buffer_nests@value[1:2],
    function(nest) nest@storage@allocation@id,
    character(1)
  ))),
  2L
)
guarded_buffer_c <- tccq_plan_backend(guarded_buffer_program, tccq_c_backend())
guarded_buffer_fortran <- tccq_plan_backend(
  guarded_buffer_program,
  tccq_fortran_backend()
)
expect_true(guarded_buffer_c@success)
expect_true(guarded_buffer_fortran@success)
expect_true(grepl(
  "double *intermediate_0001 = NULL;",
  backend_source(guarded_buffer_c),
  fixed = TRUE
))
expect_true(grepl(
  "condition_value = input_0002",
  backend_source(guarded_buffer_c),
  fixed = TRUE
))
expect_true(grepl(
  "intermediate_0001 = (double *)malloc",
  backend_source(guarded_buffer_c),
  fixed = TRUE
))
expect_true(grepl(
  "real(c_double), allocatable :: intermediate_0001(:)",
  backend_source(guarded_buffer_fortran),
  fixed = TRUE
))
expect_true(grepl(
  "condition_value = input_0002",
  backend_source(guarded_buffer_fortran),
  fixed = TRUE
))
expect_true(grepl(
  "allocate(intermediate_0001(extent_p))",
  backend_source(guarded_buffer_fortran),
  fixed = TRUE
))
expect_true(grepl(
  "if (allocated(intermediate_0001)) deallocate(intermediate_0001)",
  backend_source(guarded_buffer_fortran),
  fixed = TRUE
))

shared_guarded_buffer <- function(x, flag) {
  declare(type(x = double(n, p), flag = logical()))
  totals <- colSums(x)
  if (flag) totals else -totals
}
shared_guarded_buffer_program <- tccq_analyze(
  shared_guarded_buffer,
  strict = TRUE
)@value
shared_guarded_buffer_nests <- tccq_program_loop_nests(
  shared_guarded_buffer_program
)
expect_true(shared_guarded_buffer_nests@success)
expect_equal(length(shared_guarded_buffer_nests@value), 2L)
shared_guarded_binding <- shared_guarded_buffer_program@schedule@steps[[1L]]@binding
expect_true(S7::S7_inherits(shared_guarded_binding, TccqLocalBinding))
expect_equal(
  shared_guarded_buffer_nests@value[[1L]]@storage@value_id,
  shared_guarded_binding@value_id
)
expect_equal(length(shared_guarded_buffer_nests@value[[1L]]@guards), 0L)
shared_guarded_buffer_c <- tccq_plan_backend(
  shared_guarded_buffer_program,
  tccq_c_backend()
)
shared_guarded_buffer_fortran <- tccq_plan_backend(
  shared_guarded_buffer_program,
  tccq_fortran_backend()
)
expect_true(shared_guarded_buffer_c@success)
expect_true(shared_guarded_buffer_fortran@success)

definition_guarded_buffer <- function(x, primary, secondary) {
  declare(type(x = double(n, p), primary = logical(), secondary = logical()))
  totals <- if (primary) colSums(x) else -colSums(x)
  if (secondary) totals else -totals
}
definition_guarded_buffer_program <- tccq_analyze(
  definition_guarded_buffer,
  strict = TRUE
)@value
definition_guarded_binding <- definition_guarded_buffer_program@schedule@steps[[1L]]@binding
definition_guarded_buffer_nests <- tccq_program_loop_nests(
  definition_guarded_buffer_program
)
expect_true(definition_guarded_buffer_nests@success)
expect_equal(length(definition_guarded_buffer_nests@value), 4L)
expect_equal(
  vapply(
    definition_guarded_buffer_nests@value[1:2],
    function(nest) length(nest@guards),
    integer(1)
  ),
  c(1L, 1L)
)
expect_equal(
  vapply(
    definition_guarded_buffer_nests@value[1:2],
    function(nest) nest@guards[[1L]]@selected,
    logical(1)
  ),
  c(TRUE, FALSE)
)
expect_true(all(vapply(
  definition_guarded_buffer_nests@value[1:2],
  function(nest) identical(nest@guards[[1L]]@branch@id, definition_guarded_binding@value_id),
  logical(1)
)))
expect_equal(
  definition_guarded_buffer_nests@value[[3L]]@storage@value_id,
  definition_guarded_binding@value_id
)
expect_equal(length(definition_guarded_buffer_nests@value[[3L]]@guards), 0L)
expect_true(S7::S7_inherits(
  definition_guarded_buffer_nests@value[[3L]]@body,
  TccqValueBlock
))
definition_guarded_buffer_c <- tccq_plan_backend(
  definition_guarded_buffer_program,
  tccq_c_backend()
)
definition_guarded_buffer_fortran <- tccq_plan_backend(
  definition_guarded_buffer_program,
  tccq_fortran_backend()
)
expect_true(definition_guarded_buffer_c@success)
expect_true(definition_guarded_buffer_fortran@success)

unused_definition_buffer <- function(x) {
  declare(type(x = double(n, p)))
  discarded <- colSums(x)
  x
}
unused_definition_analysis <- tccq_analyze(
  unused_definition_buffer,
  strict = TRUE
)
expect_true(unused_definition_analysis@success)
unused_definition_program <- unused_definition_analysis@value
unused_definition_nests <- tccq_program_loop_nests(unused_definition_program)
expect_true(unused_definition_nests@success)
expect_equal(length(unused_definition_nests@value), 2L)
expect_equal(
  unused_definition_nests@value[[1L]]@storage@value_id,
  unused_definition_program@schedule@steps[[1L]]@binding@value_id
)
expect_true(tccq_plan_backend(
  unused_definition_program,
  tccq_c_backend()
)@success)
expect_true(tccq_plan_backend(
  unused_definition_program,
  tccq_fortran_backend()
)@success)

standalone_reduction <- function(x) {
  declare(type(x = double(n, p)))
  colSums(x)
  x
}
standalone_reduction_analysis <- tccq_analyze(
  standalone_reduction,
  strict = TRUE
)
expect_true(standalone_reduction_analysis@success)
standalone_reduction_program <- standalone_reduction_analysis@value
expect_equal(length(standalone_reduction_program@schedule@steps), 2L)
expect_null(standalone_reduction_program@schedule@steps[[1L]]@binding)
standalone_reduction_nests <- tccq_program_loop_nests(standalone_reduction_program)
expect_true(standalone_reduction_nests@success)
expect_equal(length(standalone_reduction_nests@value), 2L)
expect_equal(
  standalone_reduction_nests@value[[1L]]@storage@value_id,
  standalone_reduction_program@schedule@steps[[1L]]@value_id
)
expect_true(tccq_plan_backend(
  standalone_reduction_program,
  tccq_c_backend()
)@success)
expect_true(tccq_plan_backend(
  standalone_reduction_program,
  tccq_fortran_backend()
)@success)

alias_probe <- function(x) {
  declare(type(x = double(n)))
  first <- x
  second <- first
  second + 1
}
alias_analysis <- tccq_analyze(alias_probe, strict = TRUE)
alias_program <- alias_analysis@value
alias_schedule <- alias_program@schedule
expect_equal(length(alias_schedule@steps), 3L)
expect_equal(
  vapply(
    alias_schedule@steps[1:2],
    function(step) step@binding@name,
    character(1)
  ),
  c("first", "second")
)
expect_equal(
  vapply(
    alias_schedule@steps[1:2],
    function(step) step@binding@value_id,
    character(1)
  ),
  rep("formal_0001", 2L)
)
expect_true(S7::S7_inherits(
  alias_program@values[[alias_schedule@steps[[2L]]@value_id]],
  TccqBindingReference
))
expect_identical(
  alias_schedule@steps[[3L]]@uses[[1L]],
  alias_schedule@steps[[2L]]@binding
)
expect_true(tccq_plan_backend(alias_program, tccq_c_backend())@success)
expect_true(tccq_plan_backend(alias_program, tccq_fortran_backend())@success)
alias_input <- c(2, -3, 5)
alias_expected <- alias_input + 1

branch_reduction_c <- tccq_plan_backend(branch_reduction_program, tccq_c_backend())
branch_reduction_fortran <- tccq_plan_backend(
  branch_reduction_program,
  tccq_fortran_backend()
)
branch_reduction_both_c <- tccq_plan_backend(
  branch_reduction_both_program,
  tccq_c_backend()
)
branch_reduction_both_fortran <- tccq_plan_backend(
  branch_reduction_both_program,
  tccq_fortran_backend()
)
nested_branch_reduction_c <- tccq_plan_backend(
  nested_branch_reduction_program,
  tccq_c_backend()
)
nested_branch_reduction_fortran <- tccq_plan_backend(
  nested_branch_reduction_program,
  tccq_fortran_backend()
)
expect_true(branch_reduction_c@success)
expect_true(branch_reduction_fortran@success)
expect_true(branch_reduction_both_c@success)
expect_true(branch_reduction_both_fortran@success)
expect_true(nested_branch_reduction_c@success)
expect_true(nested_branch_reduction_fortran@success)

conditional_reduction_operand <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  sum(if (flag) x else -x)
}
conditional_axis_reduction <- function(x, flag) {
  declare(type(x = double(n, p), flag = logical()))
  colSums(if (flag) x else -x)
}
conditional_reduction_composition <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  sum(if (flag) x else -x) + 1
}
conditional_contraction <- function(x, weights, flag) {
  declare(type(x = double(n, p), weights = double(p), flag = logical()))
  (if (flag) x else -x) %*% weights
}
conditional_reduction_program <- tccq_analyze(
  conditional_reduction_operand,
  strict = TRUE
)@value
conditional_axis_reduction_program <- tccq_analyze(
  conditional_axis_reduction,
  strict = TRUE
)@value
conditional_reduction_composition_program <- tccq_analyze(
  conditional_reduction_composition,
  strict = TRUE
)@value
conditional_contraction_program <- tccq_analyze(
  conditional_contraction,
  strict = TRUE
)@value
conditional_reduction_nests <- tccq_program_loop_nests(
  conditional_reduction_program
)
conditional_axis_reduction_nests <- tccq_program_loop_nests(
  conditional_axis_reduction_program
)
conditional_reduction_composition_nests <- tccq_program_loop_nests(
  conditional_reduction_composition_program
)
conditional_contraction_nests <- tccq_program_loop_nests(
  conditional_contraction_program
)
expect_true(conditional_reduction_nests@success)
expect_true(conditional_axis_reduction_nests@success)
expect_true(conditional_reduction_composition_nests@success)
expect_true(conditional_contraction_nests@success)

conditional_reduction_body <- conditional_reduction_nests@value[[1L]]@body
expect_true(S7::S7_inherits(conditional_reduction_body, TccqValueBlock))
expect_equal(conditional_reduction_body@result@kind, "local")
expect_identical(
  conditional_reduction_body@locals[[1L]],
  conditional_reduction_body@result
)
expect_equal(conditional_reduction_body@result@type@shape@rank, 1L)
expect_equal(conditional_reduction_body@result@storage_type@shape@rank, 0L)
expect_equal(conditional_reduction_nests@value[[1L]]@reducer@name, "sum")
expect_true(S7::S7_inherits(
  conditional_axis_reduction_nests@value[[1L]]@body,
  TccqValueBlock
))
expect_equal(length(conditional_reduction_composition_nests@value), 2L)
expect_true(S7::S7_inherits(
  conditional_reduction_composition_nests@value[[1L]]@body,
  TccqValueBlock
))
expect_true(S7::S7_inherits(
  conditional_contraction_nests@value[[1L]]@body,
  TccqValueBlock
))

conditional_reduction_c <- tccq_plan_backend(
  conditional_reduction_program,
  tccq_c_backend()
)
conditional_reduction_fortran <- tccq_plan_backend(
  conditional_reduction_program,
  tccq_fortran_backend()
)
conditional_axis_reduction_c <- tccq_plan_backend(
  conditional_axis_reduction_program,
  tccq_c_backend()
)
conditional_axis_reduction_fortran <- tccq_plan_backend(
  conditional_axis_reduction_program,
  tccq_fortran_backend()
)
conditional_reduction_composition_c <- tccq_plan_backend(
  conditional_reduction_composition_program,
  tccq_c_backend()
)
conditional_reduction_composition_fortran <- tccq_plan_backend(
  conditional_reduction_composition_program,
  tccq_fortran_backend()
)
conditional_contraction_c <- tccq_plan_backend(
  conditional_contraction_program,
  tccq_c_backend()
)
conditional_contraction_fortran <- tccq_plan_backend(
  conditional_contraction_program,
  tccq_fortran_backend()
)
expect_true(conditional_reduction_c@success)
expect_true(conditional_reduction_fortran@success)
expect_true(conditional_axis_reduction_c@success)
expect_true(conditional_axis_reduction_fortran@success)
expect_true(conditional_reduction_composition_c@success)
expect_true(conditional_reduction_composition_fortran@success)
expect_true(conditional_contraction_c@success)
expect_true(conditional_contraction_fortran@success)
expect_equal(
  vapply(
    backend_interface(conditional_reduction_c)@locals,
    function(binding) binding@source_type@shape@rank,
    integer(1)
  ),
  c(0L, 0L)
)

branch_x <- c(1, -2, 3.5)
conditional_matrix <- matrix(c(1, 4, 9, 16, 25, 36), nrow = 2)
conditional_weights <- c(2, 3, 5)
conditional_axis_expected <- colSums(conditional_matrix)
conditional_contraction_expected <- drop(conditional_matrix %*% conditional_weights)
definition_primary_cases <- c(TRUE, TRUE, FALSE, FALSE)
definition_secondary_cases <- c(TRUE, FALSE, TRUE, FALSE)
definition_guarded_expected <- Map(
  function(primary, secondary) {
    totals <- if (primary) conditional_axis_expected else -conditional_axis_expected
    if (secondary) totals else -totals
  },
  definition_primary_cases,
  definition_secondary_cases
)
evaluate_definition_cases <- function(callable) {
  Map(
    function(primary, secondary) {
      callable(conditional_matrix, primary, secondary)
    },
    definition_primary_cases,
    definition_secondary_cases
  )
}
if (rtinycc_jit_available) {
  branch_jit_plan <- tccq_plan_backend(
    branch_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(branch_jit_plan@success)
  expect_equal(backend_interface(branch_jit_plan)@result_placement, "output_argument")
  expect_equal(backend_callable(branch_jit_plan)(branch_x, TRUE), branch_x)
  expect_equal(backend_callable(branch_jit_plan)(branch_x, FALSE), -branch_x)
  missing_condition <- tryCatch(
    backend_callable(branch_jit_plan)(branch_x, NA),
    error = identity
  )
  expect_true(inherits(missing_condition, "tccq_error"))
  expect_equal(tccq_condition_diagnostic(missing_condition)@code, "runtime.invalid_logical_condition")

  branch_reduction_jit <- tccq_plan_backend(
    branch_reduction_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  branch_reduction_both_jit <- tccq_plan_backend(
    branch_reduction_both_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  nested_branch_reduction_jit <- tccq_plan_backend(
    nested_branch_reduction_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(branch_reduction_jit@success)
  expect_true(branch_reduction_both_jit@success)
  expect_true(nested_branch_reduction_jit@success)
  expect_equal(backend_callable(branch_reduction_jit)(branch_x, TRUE), sum(branch_x))
  expect_equal(backend_callable(branch_reduction_jit)(branch_x, FALSE), 0)
  expect_equal(backend_callable(branch_reduction_both_jit)(branch_x, TRUE), sum(branch_x))
  expect_equal(backend_callable(branch_reduction_both_jit)(branch_x, FALSE), sum(-branch_x))
  expect_equal(
    backend_callable(nested_branch_reduction_jit)(branch_x, TRUE, FALSE),
    sum(-branch_x)
  )
  expect_equal(
    backend_callable(nested_branch_reduction_jit)(branch_x, FALSE, TRUE),
    0
  )
  guarded_buffer_jit <- tccq_plan_backend(
    guarded_buffer_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(guarded_buffer_jit@success)
  expect_equal(
    backend_callable(guarded_buffer_jit)(conditional_matrix, TRUE),
    colSums(conditional_matrix)
  )
  expect_equal(
    backend_callable(guarded_buffer_jit)(conditional_matrix, FALSE),
    -colSums(conditional_matrix)
  )
  shared_guarded_buffer_jit <- tccq_plan_backend(
    shared_guarded_buffer_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  definition_guarded_buffer_jit <- tccq_plan_backend(
    definition_guarded_buffer_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(shared_guarded_buffer_jit@success)
  expect_true(definition_guarded_buffer_jit@success)
  expect_equal(
    backend_callable(shared_guarded_buffer_jit)(conditional_matrix, TRUE),
    conditional_axis_expected
  )
  expect_equal(
    backend_callable(shared_guarded_buffer_jit)(conditional_matrix, FALSE),
    -conditional_axis_expected
  )
  expect_equal(
    evaluate_definition_cases(backend_callable(definition_guarded_buffer_jit)),
    definition_guarded_expected
  )
  unused_definition_jit <- tccq_plan_backend(
    unused_definition_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(unused_definition_jit@success)
  expect_equal(
    backend_callable(unused_definition_jit)(conditional_matrix),
    conditional_matrix
  )
  standalone_reduction_jit <- tccq_plan_backend(
    standalone_reduction_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(standalone_reduction_jit@success)
  expect_equal(
    backend_callable(standalone_reduction_jit)(conditional_matrix),
    conditional_matrix
  )
  alias_jit <- tccq_plan_backend(
    alias_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(alias_jit@success)
  expect_equal(backend_callable(alias_jit)(alias_input), alias_expected)

  nested_branch_jit <- tccq_plan_backend(
    nested_branch_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(nested_branch_jit@success)
  expect_equal(backend_callable(nested_branch_jit)(branch_x, TRUE, TRUE), branch_x)
  expect_equal(backend_callable(nested_branch_jit)(branch_x, TRUE, FALSE), -branch_x)
  expect_equal(backend_callable(nested_branch_jit)(branch_x, FALSE, TRUE), -branch_x)
  expect_equal(backend_callable(nested_branch_jit)(branch_x, FALSE, FALSE), branch_x)

  branch_condition_jit <- tccq_plan_backend(
    branch_condition_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(branch_condition_jit@success)
  expect_equal(backend_callable(branch_condition_jit)(branch_x, TRUE, TRUE), branch_x)
  expect_equal(backend_callable(branch_condition_jit)(branch_x, TRUE, FALSE), -branch_x)
  expect_equal(backend_callable(branch_condition_jit)(branch_x, FALSE, TRUE), -branch_x)
  expect_equal(backend_callable(branch_condition_jit)(branch_x, FALSE, FALSE), -branch_x)

  conditional_scalar_jit <- tccq_plan_backend(
    conditional_scalar_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  conditional_composition_jit <- tccq_plan_backend(
    conditional_composition_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(conditional_scalar_jit@success)
  expect_true(conditional_composition_jit@success)
  expect_equal(backend_callable(conditional_scalar_jit)(TRUE), 3)
  expect_equal(backend_callable(conditional_scalar_jit)(FALSE), -1)
  expect_equal(
    backend_callable(conditional_composition_jit)(branch_x, TRUE),
    exp(branch_x + 1)
  )
  expect_equal(
    backend_callable(conditional_composition_jit)(branch_x, FALSE),
    exp(-branch_x + 1)
  )

  conditional_reduction_jit <- tccq_plan_backend(
    conditional_reduction_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  conditional_axis_reduction_jit <- tccq_plan_backend(
    conditional_axis_reduction_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  conditional_reduction_composition_jit <- tccq_plan_backend(
    conditional_reduction_composition_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  conditional_contraction_jit <- tccq_plan_backend(
    conditional_contraction_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(conditional_reduction_jit@success)
  expect_true(conditional_axis_reduction_jit@success)
  expect_true(conditional_reduction_composition_jit@success)
  expect_true(conditional_contraction_jit@success)
  expect_equal(backend_callable(conditional_reduction_jit)(branch_x, TRUE), sum(branch_x))
  expect_equal(backend_callable(conditional_reduction_jit)(branch_x, FALSE), sum(-branch_x))
  expect_equal(
    backend_callable(conditional_axis_reduction_jit)(conditional_matrix, TRUE),
    conditional_axis_expected
  )
  expect_equal(
    backend_callable(conditional_axis_reduction_jit)(conditional_matrix, FALSE),
    -conditional_axis_expected
  )
  expect_equal(
    backend_callable(conditional_reduction_composition_jit)(branch_x, TRUE),
    sum(branch_x) + 1
  )
  expect_equal(
    backend_callable(conditional_contraction_jit)(
      conditional_matrix,
      conditional_weights,
      FALSE
    ),
    -conditional_contraction_expected
  )

  logical_branch_jit <- tccq_plan_backend(
    logical_branch_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(logical_branch_jit@success)
  expect_identical(backend_callable(logical_branch_jit)(TRUE), TRUE)
  expect_identical(backend_callable(logical_branch_jit)(FALSE), FALSE)

  scalar_less_jit <- tccq_plan_backend(
    scalar_less_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(scalar_less_jit@success)
  expect_identical(backend_callable(scalar_less_jit)(1, 2), TRUE)
  expect_identical(backend_callable(scalar_less_jit)(2, 1), FALSE)
  expect_true(is.na(backend_callable(scalar_less_jit)(NA_real_, 1)))
  expect_true(is.na(backend_callable(scalar_less_jit)(NaN, 1)))
}

if (can_build_shared_library("c")) {
  branch_reduction_c_shared <- tccq_plan_backend(
    branch_reduction_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  branch_reduction_both_c_shared <- tccq_plan_backend(
    branch_reduction_both_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  nested_branch_reduction_c_shared <- tccq_plan_backend(
    nested_branch_reduction_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(branch_reduction_c_shared@success)
  expect_true(branch_reduction_both_c_shared@success)
  expect_true(nested_branch_reduction_c_shared@success)
  expect_equal(backend_callable(branch_reduction_c_shared)(branch_x, FALSE), 0)
  expect_equal(
    backend_callable(branch_reduction_both_c_shared)(branch_x, FALSE),
    sum(-branch_x)
  )
  expect_equal(
    backend_callable(nested_branch_reduction_c_shared)(branch_x, TRUE, TRUE),
    sum(branch_x)
  )
  guarded_buffer_c_shared <- tccq_plan_backend(
    guarded_buffer_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(guarded_buffer_c_shared@success)
  expect_equal(
    backend_callable(guarded_buffer_c_shared)(conditional_matrix, TRUE),
    colSums(conditional_matrix)
  )
  expect_equal(
    backend_callable(guarded_buffer_c_shared)(conditional_matrix, FALSE),
    -colSums(conditional_matrix)
  )
  shared_guarded_buffer_c_shared <- tccq_plan_backend(
    shared_guarded_buffer_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  definition_guarded_buffer_c_shared <- tccq_plan_backend(
    definition_guarded_buffer_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(shared_guarded_buffer_c_shared@success)
  expect_true(definition_guarded_buffer_c_shared@success)
  expect_equal(
    backend_callable(shared_guarded_buffer_c_shared)(conditional_matrix, TRUE),
    conditional_axis_expected
  )
  expect_equal(
    backend_callable(shared_guarded_buffer_c_shared)(conditional_matrix, FALSE),
    -conditional_axis_expected
  )
  expect_equal(
    evaluate_definition_cases(backend_callable(definition_guarded_buffer_c_shared)),
    definition_guarded_expected
  )
  unused_definition_c_shared <- tccq_plan_backend(
    unused_definition_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(unused_definition_c_shared@success)
  expect_equal(
    backend_callable(unused_definition_c_shared)(conditional_matrix),
    conditional_matrix
  )
  standalone_reduction_c_shared <- tccq_plan_backend(
    standalone_reduction_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(standalone_reduction_c_shared@success)
  expect_equal(
    backend_callable(standalone_reduction_c_shared)(conditional_matrix),
    conditional_matrix
  )
  alias_c_shared <- tccq_plan_backend(
    alias_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(alias_c_shared@success)
  expect_equal(backend_callable(alias_c_shared)(alias_input), alias_expected)

  branch_condition_c_shared <- tccq_plan_backend(
    branch_condition_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(branch_condition_c_shared@success)
  expect_equal(backend_callable(branch_condition_c_shared)(branch_x, TRUE, TRUE), branch_x)
  expect_equal(backend_callable(branch_condition_c_shared)(branch_x, TRUE, FALSE), -branch_x)
  expect_equal(backend_callable(branch_condition_c_shared)(branch_x, FALSE, TRUE), -branch_x)
  expect_equal(backend_callable(branch_condition_c_shared)(branch_x, FALSE, FALSE), -branch_x)

  conditional_scalar_c_shared <- tccq_plan_backend(
    conditional_scalar_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  conditional_composition_c_shared <- tccq_plan_backend(
    conditional_composition_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(conditional_scalar_c_shared@success)
  expect_true(conditional_composition_c_shared@success)
  expect_equal(backend_callable(conditional_scalar_c_shared)(TRUE), 3)
  expect_equal(backend_callable(conditional_scalar_c_shared)(FALSE), -1)
  expect_equal(
    backend_callable(conditional_composition_c_shared)(branch_x, TRUE),
    exp(branch_x + 1)
  )
  expect_equal(
    backend_callable(conditional_composition_c_shared)(branch_x, FALSE),
    exp(-branch_x + 1)
  )

  conditional_reduction_c_shared <- tccq_plan_backend(
    conditional_reduction_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  conditional_axis_reduction_c_shared <- tccq_plan_backend(
    conditional_axis_reduction_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  conditional_reduction_composition_c_shared <- tccq_plan_backend(
    conditional_reduction_composition_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  conditional_contraction_c_shared <- tccq_plan_backend(
    conditional_contraction_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(conditional_reduction_c_shared@success)
  expect_true(conditional_axis_reduction_c_shared@success)
  expect_true(conditional_reduction_composition_c_shared@success)
  expect_true(conditional_contraction_c_shared@success)
  expect_equal(
    backend_callable(conditional_reduction_c_shared)(branch_x, TRUE),
    sum(branch_x)
  )
  expect_equal(
    backend_callable(conditional_axis_reduction_c_shared)(conditional_matrix, FALSE),
    -conditional_axis_expected
  )
  expect_equal(
    backend_callable(conditional_reduction_composition_c_shared)(branch_x, FALSE),
    sum(-branch_x) + 1
  )
  expect_equal(
    backend_callable(conditional_contraction_c_shared)(
      conditional_matrix,
      conditional_weights,
      TRUE
    ),
    conditional_contraction_expected
  )
}

if (can_build_shared_library("fortran")) {
  branch_fortran_shared <- tccq_plan_backend(
    branch_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(branch_fortran_shared@success)
  expect_equal(backend_callable(branch_fortran_shared)(branch_x, TRUE), branch_x)
  expect_equal(backend_callable(branch_fortran_shared)(branch_x, FALSE), -branch_x)

  branch_reduction_fortran_shared <- tccq_plan_backend(
    branch_reduction_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  branch_reduction_both_fortran_shared <- tccq_plan_backend(
    branch_reduction_both_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  nested_branch_reduction_fortran_shared <- tccq_plan_backend(
    nested_branch_reduction_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(branch_reduction_fortran_shared@success)
  expect_true(branch_reduction_both_fortran_shared@success)
  expect_true(nested_branch_reduction_fortran_shared@success)
  expect_equal(
    backend_callable(branch_reduction_fortran_shared)(branch_x, TRUE),
    sum(branch_x)
  )
  expect_equal(
    backend_callable(branch_reduction_both_fortran_shared)(branch_x, FALSE),
    sum(-branch_x)
  )
  expect_equal(
    backend_callable(nested_branch_reduction_fortran_shared)(branch_x, FALSE, FALSE),
    0
  )
  guarded_buffer_fortran_shared <- tccq_plan_backend(
    guarded_buffer_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(guarded_buffer_fortran_shared@success)
  expect_equal(
    backend_callable(guarded_buffer_fortran_shared)(conditional_matrix, TRUE),
    colSums(conditional_matrix)
  )
  expect_equal(
    backend_callable(guarded_buffer_fortran_shared)(conditional_matrix, FALSE),
    -colSums(conditional_matrix)
  )
  shared_guarded_buffer_fortran_shared <- tccq_plan_backend(
    shared_guarded_buffer_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  definition_guarded_buffer_fortran_shared <- tccq_plan_backend(
    definition_guarded_buffer_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(shared_guarded_buffer_fortran_shared@success)
  expect_true(definition_guarded_buffer_fortran_shared@success)
  expect_equal(
    backend_callable(shared_guarded_buffer_fortran_shared)(conditional_matrix, TRUE),
    conditional_axis_expected
  )
  expect_equal(
    backend_callable(shared_guarded_buffer_fortran_shared)(conditional_matrix, FALSE),
    -conditional_axis_expected
  )
  expect_equal(
    evaluate_definition_cases(backend_callable(definition_guarded_buffer_fortran_shared)),
    definition_guarded_expected
  )
  unused_definition_fortran_shared <- tccq_plan_backend(
    unused_definition_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(unused_definition_fortran_shared@success)
  expect_equal(
    backend_callable(unused_definition_fortran_shared)(conditional_matrix),
    conditional_matrix
  )
  standalone_reduction_fortran_shared <- tccq_plan_backend(
    standalone_reduction_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(standalone_reduction_fortran_shared@success)
  expect_equal(
    backend_callable(standalone_reduction_fortran_shared)(conditional_matrix),
    conditional_matrix
  )
  alias_fortran_shared <- tccq_plan_backend(
    alias_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(alias_fortran_shared@success)
  expect_equal(backend_callable(alias_fortran_shared)(alias_input), alias_expected)

  nested_branch_fortran_shared <- tccq_plan_backend(
    nested_branch_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(nested_branch_fortran_shared@success)
  expect_equal(backend_callable(nested_branch_fortran_shared)(branch_x, TRUE, TRUE), branch_x)
  expect_equal(backend_callable(nested_branch_fortran_shared)(branch_x, TRUE, FALSE), -branch_x)
  expect_equal(backend_callable(nested_branch_fortran_shared)(branch_x, FALSE, TRUE), -branch_x)
  expect_equal(backend_callable(nested_branch_fortran_shared)(branch_x, FALSE, FALSE), branch_x)

  branch_condition_fortran_shared <- tccq_plan_backend(
    branch_condition_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(branch_condition_fortran_shared@success)
  expect_equal(
    backend_callable(branch_condition_fortran_shared)(branch_x, TRUE, TRUE),
    branch_x
  )
  expect_equal(
    backend_callable(branch_condition_fortran_shared)(branch_x, TRUE, FALSE),
    -branch_x
  )
  expect_equal(
    backend_callable(branch_condition_fortran_shared)(branch_x, FALSE, TRUE),
    -branch_x
  )
  expect_equal(
    backend_callable(branch_condition_fortran_shared)(branch_x, FALSE, FALSE),
    -branch_x
  )

  conditional_scalar_fortran_shared <- tccq_plan_backend(
    conditional_scalar_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  conditional_composition_fortran_shared <- tccq_plan_backend(
    conditional_composition_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(conditional_scalar_fortran_shared@success)
  expect_true(conditional_composition_fortran_shared@success)
  expect_equal(backend_callable(conditional_scalar_fortran_shared)(TRUE), 3)
  expect_equal(backend_callable(conditional_scalar_fortran_shared)(FALSE), -1)
  expect_equal(
    backend_callable(conditional_composition_fortran_shared)(branch_x, TRUE),
    exp(branch_x + 1)
  )
  expect_equal(
    backend_callable(conditional_composition_fortran_shared)(branch_x, FALSE),
    exp(-branch_x + 1)
  )

  conditional_reduction_fortran_shared <- tccq_plan_backend(
    conditional_reduction_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  conditional_axis_reduction_fortran_shared <- tccq_plan_backend(
    conditional_axis_reduction_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  conditional_reduction_composition_fortran_shared <- tccq_plan_backend(
    conditional_reduction_composition_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  conditional_contraction_fortran_shared <- tccq_plan_backend(
    conditional_contraction_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(conditional_reduction_fortran_shared@success)
  expect_true(conditional_axis_reduction_fortran_shared@success)
  expect_true(conditional_reduction_composition_fortran_shared@success)
  expect_true(conditional_contraction_fortran_shared@success)
  expect_equal(
    backend_callable(conditional_reduction_fortran_shared)(branch_x, FALSE),
    sum(-branch_x)
  )
  expect_equal(
    backend_callable(conditional_axis_reduction_fortran_shared)(conditional_matrix, TRUE),
    conditional_axis_expected
  )
  expect_equal(
    backend_callable(conditional_reduction_composition_fortran_shared)(branch_x, TRUE),
    sum(branch_x) + 1
  )
  expect_equal(
    backend_callable(conditional_contraction_fortran_shared)(
      conditional_matrix,
      conditional_weights,
      FALSE
    ),
    -conditional_contraction_expected
  )

  logical_branch_fortran_shared <- tccq_plan_backend(
    logical_branch_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(logical_branch_fortran_shared@success)
  expect_identical(backend_callable(logical_branch_fortran_shared)(TRUE), TRUE)
  expect_identical(backend_callable(logical_branch_fortran_shared)(FALSE), FALSE)
}

matrix_vector <- function(x, w) {
  declare(type(x = double(n, p), w = double(p)))
  x %*% w
}

matrix_vector_program <- tccq_analyze(matrix_vector)
expect_true(matrix_vector_program@success)
expect_true(matrix_vector_program@value@attrs$lowered)
matrix_vector_nest <- tccq_program_loop_nest(matrix_vector_program@value)
expect_true(matrix_vector_nest@success)
expect_true(S7::S7_inherits(matrix_vector_nest@value, TccqLoopNest))
expect_equal(
  vapply(matrix_vector_nest@value@axes, function(axis) axis@role, character(1)),
  c("map", "reduce")
)
expect_equal(matrix_vector_nest@value@reducer@name, "sum")

matrix_vector_c_plan <- tccq_plan_backend(
  matrix_vector_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(matrix_vector_c_plan@success)
matrix_vector_c_source <- backend_source(matrix_vector_c_plan)
matrix_vector_c_interface <- backend_interface(matrix_vector_c_plan)
expect_equal(matrix_vector_c_interface@kind, "loop_nest")
expect_equal(
  vapply(matrix_vector_c_interface@extents, function(binding) binding@symbol, character(1)),
  c("n", "p")
)
expect_true(grepl(
  "(input_0001[axis_0001 + axis_0002 * extent_n] * input_0002[axis_0002])",
  matrix_vector_c_source,
  fixed = TRUE
))

tiled_stencil_1d <- function(x) {
  declare(type(x = double(n)))
  x[1:(n - 2L)] + x[2:(n - 1L)] + x[3:n]
}

stencil_program <- tccq_analyze(tiled_stencil_1d)
expect_true(stencil_program@success)
expect_true(stencil_program@value@attrs$lowered)
stencil_fusion <- stencil_program@value@regions[[1L]]@fusion_groups[[1L]]
expect_equal(stencil_fusion@kind, "stencil")
stencil_result_dim <- stencil_program@value@values[[stencil_program@value@result]]@type@shape@dims[[1L]]
expect_equal(stencil_result_dim@kind, "affine")
expect_equal(stencil_result_dim@label, "n")
expect_equal(stencil_result_dim@value, -2L)

stencil_c_plan <- tccq_plan_backend(
  stencil_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(stencil_c_plan@success)
stencil_c_source <- backend_source(stencil_c_plan)
expect_true(grepl("axis_0001 < (extent_n - 2)", stencil_c_source, fixed = TRUE))
expect_true(grepl("input_0001[(axis_0001 + 2)]", stencil_c_source, fixed = TRUE))

stencil_x <- c(1, 2, 4, 8, 16, 32, 64)
stencil_expected <- stencil_x[1:5] + stencil_x[2:6] + stencil_x[3:7]
matvec_x <- matrix(c(1, 4, 9, 16, 25, 36), nrow = 2)
matvec_w <- c(2, 3, 5)
matvec_expected <- drop(matvec_x %*% matvec_w)

if (can_build_shared_library("c")) {
  matrix_vector_c_shared_plan <- tccq_plan_backend(
    matrix_vector_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(matrix_vector_c_shared_plan@success)
  expect_equal(
    backend_callable(matrix_vector_c_shared_plan)(matvec_x, matvec_w),
    matvec_expected
  )
  mismatched_matvec <- tryCatch(
    backend_callable(matrix_vector_c_shared_plan)(matvec_x, c(1, 2)),
    error = identity
  )
  expect_true(inherits(mismatched_matvec, "error"))

  stencil_c_shared_plan <- tccq_plan_backend(
    stencil_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(stencil_c_shared_plan@success)
  expect_equal(backend_callable(stencil_c_shared_plan)(stencil_x), stencil_expected)
}

if (can_build_shared_library("fortran")) {
  matrix_vector_fortran_shared_plan <- tccq_plan_backend(
    matrix_vector_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(matrix_vector_fortran_shared_plan@success)
  expect_equal(
    backend_callable(matrix_vector_fortran_shared_plan)(matvec_x, matvec_w),
    matvec_expected
  )

  stencil_fortran_shared_plan <- tccq_plan_backend(
    stencil_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(stencil_fortran_shared_plan@success)
  expect_equal(backend_callable(stencil_fortran_shared_plan)(stencil_x), stencil_expected)
}

if (rtinycc_jit_available) {
  matrix_vector_jit_plan <- tccq_plan_backend(
    matrix_vector_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(matrix_vector_jit_plan@success)
  expect_equal(
    backend_callable(matrix_vector_jit_plan)(matvec_x, matvec_w),
    matvec_expected
  )
  jit_mismatched <- tryCatch(
    backend_callable(matrix_vector_jit_plan)(matvec_x, c(1, 2)),
    error = identity
  )
  expect_true(inherits(jit_mismatched, "tccq_error"))

  stencil_jit_plan <- tccq_plan_backend(
    stencil_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(stencil_jit_plan@success)
  expect_equal(backend_callable(stencil_jit_plan)(stencil_x), stencil_expected)
}

matvec_supported_compile <- tccq_compile(matrix_vector)
expect_true(matvec_supported_compile@success)

# Multi-nest composition: non-root scalar reductions become intermediate
# all-reduce nests feeding the final nest through named scalars.
nested_reduction <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  sum(x) + sum(y)
}
nested_reduction_program <- tccq_analyze(nested_reduction)
expect_true(nested_reduction_program@success)
nested_reduction_nests <- tccq_program_loop_nests(nested_reduction_program@value)
expect_true(nested_reduction_nests@success)
expect_equal(length(nested_reduction_nests@value), 3L)

normalize <- function(x) {
  declare(type(x = double(n)))
  s <- sum(x)
  x / s
}
normalize_program <- tccq_analyze(normalize)
expect_true(normalize_program@success)

normalize_groups <- normalize_program@value@regions[[1L]]@fusion_groups
expect_equal(length(normalize_groups), 2L)
expect_equal(
  vapply(normalize_groups, function(group) group@kind, character(1)),
  c("map_reduce", "map")
)
normalize_sum_reference <- normalize_program@value@values[[
  normalize_groups[[2L]]@contract@result_value@inputs[[2L]]
]]
expect_true(S7::S7_inherits(normalize_sum_reference, TccqBindingReference))
expect_equal(normalize_groups[[1L]]@outputs, normalize_sum_reference@binding@value_id)

normalize_nests <- tccq_program_loop_nests(normalize_program@value)
expect_true(normalize_nests@success)
expect_equal(length(normalize_nests@value), 2L)
normalize_intermediate <- normalize_nests@value[[1L]]
normalize_final <- normalize_nests@value[[2L]]
expect_true(S7::S7_inherits(normalize_intermediate@reducer, TccqReductionSpec))
expect_true(all(vapply(
  normalize_intermediate@axes,
  function(axis) identical(axis@role, "reduce"),
  logical(1)
)))
expect_true(S7::S7_inherits(normalize_intermediate@storage, TccqStorageSlot))
expect_equal(normalize_intermediate@storage@role, "temporary")
expect_true(normalize_intermediate@storage@materialized)
expect_true(S7::S7_inherits(normalize_intermediate@accumulator, TccqWriteTarget))
expect_null(normalize_final@reducer)

# The singular planner refuses multi-nest programs with a classed diagnostic.
normalize_single <- tccq_program_loop_nest(normalize_program@value)
expect_false(normalize_single@success)
expect_true(any(vapply(
  normalize_single@diagnostics,
  function(x) identical(x@code, "loop_nest.multi_nest_program"),
  logical(1)
)))

normalize_source_plan <- tccq_plan_backend(
  normalize_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(normalize_source_plan@success)
expect_equal(length(backend_products(normalize_source_plan)@loop_nests), 2L)
normalize_c_interface <- backend_interface(normalize_source_plan)
expect_equal(normalize_c_interface@allocations[[1L]]@source_name, "intermediate_0001")
expect_identical(
  normalize_c_interface@allocations[[1L]]@slots[[1L]],
  normalize_intermediate@storage
)
expect_true(grepl(
  "double intermediate_0001;",
  backend_source(normalize_source_plan),
  fixed = TRUE
))

normalize_fortran_plan <- tccq_plan_backend(
  normalize_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(normalize_fortran_plan@success)

if (rtinycc_jit_available) {
  normalize_jit <- tccq_plan_backend(
    normalize_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(normalize_jit@success)
  normalize_x <- c(1, 2, 3, 4)
  expect_equal(
    backend_callable(normalize_jit)(normalize_x),
    normalize_x / sum(normalize_x)
  )

  mean_square <- function(x) {
    declare(type(x = double(n)))
    s <- sum(x * x)
    s / 2
  }
  mean_square_jit <- tccq_plan_backend(
    tccq_analyze(mean_square, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(mean_square_jit@success)
  mean_square_x <- c(1, 2, 3)
  expect_equal(
    backend_callable(mean_square_jit)(mean_square_x),
    sum(mean_square_x * mean_square_x) / 2
  )

  center_scale <- function(x, y) {
    declare(type(x = double(n), y = double(n)))
    (x - sum(x)) / sum(y * y)
  }
  center_scale_jit <- tccq_plan_backend(
    tccq_analyze(center_scale, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(center_scale_jit@success)
  center_scale_x <- c(1, 2, 3)
  center_scale_y <- c(2, 1, 0.5)
  expect_equal(
    backend_callable(center_scale_jit)(center_scale_x, center_scale_y),
    (center_scale_x - sum(center_scale_x)) / sum(center_scale_y * center_scale_y)
  )
}

# Array-valued intermediates materialize as buffers: an axis reduction or
# contraction feeding later work becomes its own nest writing a temporary.
axis_reduction_feed <- function(x) {
  declare(type(x = double(n, p)))
  colSums(x) + 1
}
axis_reduction_feed_program <- tccq_analyze(axis_reduction_feed)
expect_true(axis_reduction_feed_program@success)
axis_feed_nests <- tccq_program_loop_nests(axis_reduction_feed_program@value)
expect_true(axis_feed_nests@success)
expect_equal(length(axis_feed_nests@value), 2L)
expect_true(S7::S7_inherits(axis_feed_nests@value[[1L]]@storage, TccqStorageSlot))
expect_equal(axis_feed_nests@value[[1L]]@storage@type@shape@rank, 1L)
expect_true(S7::S7_inherits(axis_feed_nests@value[[1L]]@output, TccqAccess))

axis_feed_groups <- axis_reduction_feed_program@value@regions[[1L]]@fusion_groups
expect_equal(
  vapply(axis_feed_groups, function(group) group@kind, character(1)),
  c("axis_reduce", "map")
)

axis_feed_buffer_slot <- axis_feed_nests@value[[1L]]@storage
expect_true(axis_feed_buffer_slot@materialized)
expect_true(S7::S7_inherits(axis_feed_buffer_slot@allocation, TccqStorageAllocation))

contraction_feed <- function(x, w, y) {
  declare(type(x = double(n, p), w = double(p), y = double(n)))
  (x %*% w) + y
}
contraction_feed_program <- tccq_analyze(contraction_feed)
expect_true(contraction_feed_program@success)
contraction_feed_nests <- tccq_program_loop_nests(contraction_feed_program@value)
expect_true(contraction_feed_nests@success)
expect_equal(length(contraction_feed_nests@value), 2L)
expect_true(S7::S7_inherits(
  contraction_feed_nests@value[[1L]]@storage,
  TccqStorageSlot
))
expect_equal(contraction_feed_nests@value[[1L]]@storage@type@shape@rank, 1L)

# A value consumed twice materializes once: cs feeds both the scalar
# reduction and the final map, so the program plans three nests, not four.
col_normalize <- function(x) {
  declare(type(x = double(n, p)))
  cs <- colSums(x)
  cs / sum(cs)
}
col_normalize_program <- tccq_analyze(col_normalize)
expect_true(col_normalize_program@success)
col_normalize_nests <- tccq_program_loop_nests(col_normalize_program@value)
expect_true(col_normalize_nests@success)
expect_equal(length(col_normalize_nests@value), 3L)

# Physical allocation identity, rather than a reuse hint, joins compatible
# materialized buffers whose typed lifetimes do not overlap.
reuse_storage <- function(x, y, z) {
  declare(type(x = double(n, n), y = double(n, n), z = double(n, n)))
  first <- x %*% y
  first_total <- sum(first)
  second <- y %*% z
  sum(second) + first_total
}
reuse_storage_program <- tccq_analyze(reuse_storage, strict = TRUE)
expect_true(reuse_storage_program@success)
reuse_storage_nests <- tccq_program_loop_nests(reuse_storage_program@value)
expect_true(reuse_storage_nests@success)
reuse_buffer_nests <- Filter(
  function(nest) nest@storage@type@shape@rank > 0L,
  reuse_storage_nests@value
)
expect_equal(length(reuse_buffer_nests), 2L)
expect_equal(
  length(unique(vapply(
    reuse_buffer_nests,
    function(nest) nest@storage@allocation@id,
    character(1)
  ))),
  1L
)

overlapping_storage <- function(x, y, z) {
  declare(type(x = double(n, n), y = double(n, n), z = double(n, n)))
  first <- x %*% y
  second <- y %*% z
  sum(first + second)
}
overlapping_storage_program <- tccq_analyze(overlapping_storage, strict = TRUE)
expect_true(overlapping_storage_program@success)
overlapping_storage_nests <- tccq_program_loop_nests(overlapping_storage_program@value)
expect_true(overlapping_storage_nests@success)
overlapping_buffer_nests <- Filter(
  function(nest) nest@storage@type@shape@rank > 0L,
  overlapping_storage_nests@value
)
expect_equal(length(overlapping_buffer_nests), 2L)
expect_equal(
  length(unique(vapply(
    overlapping_buffer_nests,
    function(nest) nest@storage@allocation@id,
    character(1)
  ))),
  2L
)

dependent_storage <- function(x, y, z) {
  declare(type(x = double(n, n), y = double(n, n), z = double(n, n)))
  first <- x %*% y
  second <- first %*% z
  sum(second)
}
dependent_storage_program <- tccq_analyze(dependent_storage, strict = TRUE)
expect_true(dependent_storage_program@success)
dependent_storage_nests <- tccq_program_loop_nests(dependent_storage_program@value)
expect_true(dependent_storage_nests@success)
dependent_buffer_nests <- Filter(
  function(nest) nest@storage@type@shape@rank > 0L,
  dependent_storage_nests@value
)
expect_equal(length(dependent_buffer_nests), 2L)
expect_equal(
  length(unique(vapply(
    dependent_buffer_nests,
    function(nest) nest@storage@allocation@id,
    character(1)
  ))),
  2L
)
expect_equal(
  dependent_buffer_nests[[1L]]@storage@lifetime@last_used_at,
  dependent_buffer_nests[[2L]]@storage@lifetime@defined_at
)

alias_storage <- function(x, y, z) {
  declare(type(x = double(n, n), y = double(n, n), z = double(n, n)))
  first <- x %*% y
  alias <- first
  second <- y %*% z
  sum(alias + second)
}
alias_storage_program <- tccq_analyze(alias_storage, strict = TRUE)
expect_true(alias_storage_program@success)
alias_storage_nests <- tccq_program_loop_nests(alias_storage_program@value)
expect_true(alias_storage_nests@success)
alias_buffer_nests <- Filter(
  function(nest) nest@storage@type@shape@rank > 0L,
  alias_storage_nests@value
)
expect_equal(length(alias_buffer_nests), 2L)
expect_equal(
  length(unique(vapply(
    alias_buffer_nests,
    function(nest) nest@storage@allocation@id,
    character(1)
  ))),
  2L
)

shape_mismatch_storage <- function(x, y, z) {
  declare(type(x = double(n, n), y = double(n, n), z = double(n, p)))
  first <- x %*% y
  first_total <- sum(first)
  second <- x %*% z
  sum(second) + first_total
}
shape_mismatch_program <- tccq_analyze(shape_mismatch_storage, strict = TRUE)
expect_true(shape_mismatch_program@success)
shape_mismatch_nests <- tccq_program_loop_nests(shape_mismatch_program@value)
expect_true(shape_mismatch_nests@success)
shape_mismatch_buffers <- Filter(
  function(nest) nest@storage@type@shape@rank > 0L,
  shape_mismatch_nests@value
)
expect_equal(length(shape_mismatch_buffers), 2L)
expect_false(identical(
  shape_mismatch_buffers[[1L]]@storage@type,
  shape_mismatch_buffers[[2L]]@storage@type
))
expect_equal(
  length(unique(vapply(
    shape_mismatch_buffers,
    function(nest) nest@storage@allocation@id,
    character(1)
  ))),
  2L
)

reuse_storage_c <- tccq_plan_backend(
  reuse_storage_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
reuse_storage_fortran <- tccq_plan_backend(
  reuse_storage_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(reuse_storage_c@success)
expect_true(reuse_storage_fortran@success)
expect_equal(
  sum(grepl("malloc", strsplit(backend_source(reuse_storage_c), "\n", fixed = TRUE)[[1L]], fixed = TRUE)),
  1L
)
expect_equal(
  sum(grepl(
    "real(c_double) :: intermediate_0001(",
    strsplit(backend_source(reuse_storage_fortran), "\n", fixed = TRUE)[[1L]],
    fixed = TRUE
  )),
  1L
)

reuse_x <- matrix(c(1, 2, 3, 4), nrow = 2)
reuse_y <- matrix(c(2, -1, 0.5, 3), nrow = 2)
reuse_z <- matrix(c(-2, 1, 4, 0.25), nrow = 2)
reuse_expected <- sum(reuse_x %*% reuse_y) + sum(reuse_y %*% reuse_z)

if (can_build_shared_library("c")) {
  reuse_storage_c_shared <- tccq_plan_backend(
    reuse_storage_program@value,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(reuse_storage_c_shared@success)
  expect_equal(
    backend_callable(reuse_storage_c_shared)(reuse_x, reuse_y, reuse_z),
    reuse_expected
  )
}

if (can_build_shared_library("fortran")) {
  reuse_storage_fortran_shared <- tccq_plan_backend(
    reuse_storage_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(reuse_storage_fortran_shared@success)
  expect_equal(
    backend_callable(reuse_storage_fortran_shared)(reuse_x, reuse_y, reuse_z),
    reuse_expected
  )
}

if (rtinycc_jit_available) {
  reuse_storage_jit <- tccq_plan_backend(
    reuse_storage_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(reuse_storage_jit@success)
  expect_equal(
    backend_callable(reuse_storage_jit)(reuse_x, reuse_y, reuse_z),
    reuse_expected
  )

  axis_feed_jit <- tccq_plan_backend(
    axis_reduction_feed_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(axis_feed_jit@success)
  axis_feed_x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  expect_equal(
    backend_callable(axis_feed_jit)(axis_feed_x),
    colSums(axis_feed_x) + 1
  )

  contraction_feed_jit <- tccq_plan_backend(
    contraction_feed_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(contraction_feed_jit@success)
  contraction_feed_x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  contraction_feed_w <- c(0.5, -1, 2)
  contraction_feed_y <- c(10, 20)
  expect_equal(
    backend_callable(contraction_feed_jit)(
      contraction_feed_x,
      contraction_feed_w,
      contraction_feed_y
    ),
    drop(contraction_feed_x %*% contraction_feed_w) + contraction_feed_y
  )

  col_normalize_jit <- tccq_plan_backend(
    col_normalize_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(col_normalize_jit@success)
  expect_equal(
    backend_callable(col_normalize_jit)(axis_feed_x),
    colSums(axis_feed_x) / sum(colSums(axis_feed_x))
  )

  sum_matvec <- function(x, w) {
    declare(type(x = double(n, p), w = double(p)))
    sum(x %*% w)
  }
  sum_matvec_jit <- tccq_plan_backend(
    tccq_analyze(sum_matvec, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(sum_matvec_jit@success)
  expect_equal(
    backend_callable(sum_matvec_jit)(contraction_feed_x, contraction_feed_w),
    sum(contraction_feed_x %*% contraction_feed_w)
  )
}

axis_feed_fortran <- tccq_plan_backend(
  axis_reduction_feed_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(axis_feed_fortran@success)

if (can_build_shared_library("fortran")) {
  fortran_multi_nest_x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  fortran_multi_nest_w <- c(0.5, -1, 2)
  fortran_multi_nest_y <- c(10, 20)

  axis_feed_fortran_shared <- tccq_plan_backend(
    axis_reduction_feed_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(axis_feed_fortran_shared@success)
  expect_equal(
    backend_callable(axis_feed_fortran_shared)(fortran_multi_nest_x),
    colSums(fortran_multi_nest_x) + 1
  )

  contraction_feed_fortran_shared <- tccq_plan_backend(
    contraction_feed_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(contraction_feed_fortran_shared@success)
  expect_equal(
    backend_callable(contraction_feed_fortran_shared)(
      fortran_multi_nest_x,
      fortran_multi_nest_w,
      fortran_multi_nest_y
    ),
    drop(fortran_multi_nest_x %*% fortran_multi_nest_w) + fortran_multi_nest_y
  )

  col_normalize_fortran_shared <- tccq_plan_backend(
    col_normalize_program@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(col_normalize_fortran_shared@success)
  expect_equal(
    backend_callable(col_normalize_fortran_shared)(fortran_multi_nest_x),
    colSums(fortran_multi_nest_x) / sum(colSums(fortran_multi_nest_x))
  )
}

# Declared dimension symbols are scalar values in the body: `n` reads the
# extent parameter the generated ABI already passes, widened to double.
col_means <- function(x) {
  declare(type(x = double(n, p)))
  colSums(x) / n
}
col_means_program <- tccq_analyze(col_means)
expect_true(col_means_program@success)
col_means_dim_values <- Filter(
  function(value) identical(value@op, "dim_symbol"),
  col_means_program@value@values
)
expect_equal(length(col_means_dim_values), 1L)
expect_equal(col_means_dim_values[[1L]]@attrs$symbol, "n")

col_means_c <- tccq_plan_backend(
  col_means_program@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_true(col_means_c@success)
col_means_fortran <- tccq_plan_backend(
  col_means_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(col_means_fortran@success)

if (rtinycc_jit_available) {
  col_means_jit <- tccq_plan_backend(
    col_means_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(col_means_jit@success)
  col_means_x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  expect_equal(backend_callable(col_means_jit)(col_means_x), colMeans(col_means_x))

  variance_like <- function(x) {
    declare(type(x = double(n)))
    sum(x * x) / (n - 1)
  }
  variance_like_jit <- tccq_plan_backend(
    tccq_analyze(variance_like, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(variance_like_jit@success)
  variance_like_x <- c(1, 2, 3, 4)
  expect_equal(
    backend_callable(variance_like_jit)(variance_like_x),
    sum(variance_like_x * variance_like_x) / (length(variance_like_x) - 1)
  )

  # Contraction buffer, axis-reduction buffer, dimension value, and an
  # elementwise chain composed in one program.
  composed_chain <- function(x, y) {
    declare(type(x = double(n, p), y = double(p, q)))
    m <- x %*% y
    sqrt(exp(colSums(m) / n))
  }
  composed_chain_jit <- tccq_plan_backend(
    tccq_analyze(composed_chain, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(composed_chain_jit@success)
  composed_chain_x <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
  composed_chain_y <- matrix(c(0.5, -1, 2, 1, 0, 1), nrow = 3)
  expect_equal(
    backend_callable(composed_chain_jit)(composed_chain_x, composed_chain_y),
    sqrt(exp(colSums(composed_chain_x %*% composed_chain_y) / nrow(composed_chain_x)))
  )
}

# Rank-mixed elementwise operands follow R's recycling rule: the shorter
# operand recycles over the host's column-major element order, GNU-R being
# the oracle for both alignments.
recycle_center <- function(x, mu) {
  declare(type(x = double(n, p), mu = double(p)))
  x - mu
}
recycle_center_program <- tccq_analyze(recycle_center)
expect_true(recycle_center_program@success)
recycle_center_nests <- tccq_program_loop_nests(recycle_center_program@value)
expect_true(recycle_center_nests@success)
recycle_center_access <-
  recycle_center_nests@value[[1L]]@body@inputs[[2L]]@reference@access
expect_equal(recycle_center_access@kind, "recycle")
expect_equal(recycle_center_access@consumer_shape@rank, 2L)
expect_equal(
  vapply(recycle_center_access@consumer_shape@dims, function(dim) dim@label, character(1)),
  c("n", "p")
)

# R refuses non-conformable arrays; so does the domain policy.
recycle_nonconformable <- function(x, y) {
  declare(type(x = double(n, p), y = double(p, n)))
  x + y
}
expect_false(tccq_analyze(recycle_nonconformable)@success)

# Divisibility that cannot be proven from declared dimensions is refused.
recycle_unprovable <- function(x, w) {
  declare(type(x = double(n, p), w = double(m)))
  x - w
}
expect_false(tccq_analyze(recycle_unprovable)@success)

recycle_x <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 3)
recycle_mu <- c(0.5, -1, 2, 1)
recycle_v <- c(10, 20, 30)

# The original apotheosis composite, end to end against each executable
# backend that claims the typed loop-nest path.
logistic_gradient <- function(x, y, w, lambda) {
  declare(type(
    x = double(n, p),
    y = double(n),
    w = double(p),
    lambda = double()
  ))
  mu <- colMeans(x)
  sigma <- sqrt(colSums((x - mu)^2) / (n - 1L))
  z <- (x - mu) / sigma
  eta <- z %*% w
  prob <- 1 / (1 + exp(-eta))
  grad <- crossprod(z, prob - y) / n + lambda * w
  w - 0.01 * grad
}
logistic_y <- c(0.2, 0.7, 0.4)
logistic_w <- c(0.25, -0.5, 1, 0.75)
logistic_oracle <- local({
  mu <- colMeans(recycle_x)
  sigma <- sqrt(colSums((recycle_x - mu)^2) / (nrow(recycle_x) - 1L))
  z <- (recycle_x - mu) / sigma
  prob <- 1 / (1 + exp(-(z %*% logistic_w)))
  grad <- crossprod(z, drop(prob) - logistic_y) / nrow(recycle_x) + 0.5 * logistic_w
  logistic_w - 0.01 * drop(grad)
})

if (rtinycc_jit_available) {
  recycle_center_jit <- tccq_plan_backend(
    recycle_center_program@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(recycle_center_jit@success)
  expect_equal(
    backend_callable(recycle_center_jit)(recycle_x, recycle_mu),
    recycle_x - recycle_mu
  )

  recycle_rows <- function(x, v) {
    declare(type(x = double(n, p), v = double(n)))
    x - v
  }
  recycle_rows_jit <- tccq_plan_backend(
    tccq_analyze(recycle_rows, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(recycle_rows_jit@success)
  expect_equal(
    backend_callable(recycle_rows_jit)(recycle_x, recycle_v),
    recycle_x - recycle_v
  )

  recycle_reduce <- function(x, v) {
    declare(type(x = double(n, p), v = double(n)))
    sum((x - v)^2)
  }
  recycle_reduce_jit <- tccq_plan_backend(
    tccq_analyze(recycle_reduce, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(recycle_reduce_jit@success)
  expect_equal(
    backend_callable(recycle_reduce_jit)(recycle_x, recycle_v),
    sum((recycle_x - recycle_v)^2)
  )

  # The logistic forward pass: column statistics through buffers and
  # dimension values, standardization through recycling, a contraction,
  # and the sigmoid — one kernel agreeing with R.
  forward_pass <- function(x, w) {
    declare(type(x = double(n, p), w = double(p)))
    mu <- colSums(x) / n
    sigma <- sqrt(colSums((x - mu)^2) / (n - 1))
    z <- (x - mu) / sigma
    eta <- z %*% w
    1 / (1 + exp(-eta))
  }
  forward_pass_jit <- tccq_plan_backend(
    tccq_analyze(forward_pass, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(forward_pass_jit@success)
  forward_pass_w <- c(0.25, -0.5, 1, 0.75)
  forward_pass_oracle <- local({
    mu <- colSums(recycle_x) / nrow(recycle_x)
    sigma <- sqrt(colSums((recycle_x - mu)^2) / (nrow(recycle_x) - 1))
    z <- (recycle_x - mu) / sigma
    drop(1 / (1 + exp(-(z %*% forward_pass_w))))
  })
  expect_equal(
    backend_callable(forward_pass_jit)(recycle_x, forward_pass_w),
    forward_pass_oracle
  )

  # Contraction dims generalize past %*%: crossprod contracts (1, 1) and
  # tcrossprod contracts (2, 2), each one nest with reordered operand axes.
  crossprod_matmat <- function(x, y) {
    declare(type(x = double(n, p), y = double(n, q)))
    crossprod(x, y)
  }
  crossprod_y <- matrix(c(2, 1, 0.5, -1, 3, 0), nrow = 3)
  crossprod_jit <- tccq_plan_backend(
    tccq_analyze(crossprod_matmat, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(crossprod_jit@success)
  expect_equal(
    as.numeric(backend_callable(crossprod_jit)(recycle_x, crossprod_y)),
    as.numeric(crossprod(recycle_x, crossprod_y))
  )

  crossprod_matvec <- function(x, v) {
    declare(type(x = double(n, p), v = double(n)))
    crossprod(x, v)
  }
  crossprod_matvec_jit <- tccq_plan_backend(
    tccq_analyze(crossprod_matvec, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(crossprod_matvec_jit@success)
  expect_equal(
    backend_callable(crossprod_matvec_jit)(recycle_x, recycle_v),
    as.numeric(crossprod(recycle_x, recycle_v))
  )

  tcrossprod_matmat <- function(x, q) {
    declare(type(x = double(n, p), q = double(m, p)))
    tcrossprod(x, q)
  }
  tcrossprod_q <- matrix(c(1, 0.5, -1, 2, 0, 1, 3, -2), nrow = 2)
  tcrossprod_jit <- tccq_plan_backend(
    tccq_analyze(tcrossprod_matmat, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(tcrossprod_jit@success)
  expect_equal(
    as.numeric(backend_callable(tcrossprod_jit)(recycle_x, tcrossprod_q)),
    as.numeric(tcrossprod(recycle_x, tcrossprod_q))
  )

  # Reducers with finalizers: mean-family reductions divide the folded
  # accumulator by the reduced count, in scalar, buffer, and result nests.
  mean_probe <- function(x) {
    declare(type(x = double(n)))
    mean(x * x) / 2
  }
  mean_jit <- tccq_plan_backend(
    tccq_analyze(mean_probe, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(mean_jit@success)
  mean_x <- c(1, 2, 3, 4)
  expect_equal(backend_callable(mean_jit)(mean_x), mean(mean_x * mean_x) / 2)

  col_means_probe <- function(x) {
    declare(type(x = double(n, p)))
    colMeans(x)
  }
  col_means_jit <- tccq_plan_backend(
    tccq_analyze(col_means_probe, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(col_means_jit@success)
  expect_equal(backend_callable(col_means_jit)(recycle_x), colMeans(recycle_x))

  row_means_probe <- function(x) {
    declare(type(x = double(n, p)))
    rowMeans(x) + 1
  }
  row_means_jit <- tccq_plan_backend(
    tccq_analyze(row_means_probe, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(row_means_jit@success)
  expect_equal(backend_callable(row_means_jit)(recycle_x), rowMeans(recycle_x) + 1)

  logistic_gradient_jit <- tccq_plan_backend(
    tccq_analyze(logistic_gradient, strict = TRUE)@value,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(logistic_gradient_jit@success)
  expect_equal(
    backend_callable(logistic_gradient_jit)(recycle_x, logistic_y, logistic_w, 0.5),
    logistic_oracle
  )
}

if (can_build_shared_library("fortran")) {
  logistic_gradient_fortran_plan <- tccq_plan_backend(
    tccq_analyze(logistic_gradient, strict = TRUE)@value,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(logistic_gradient_fortran_plan@success)
  expect_equal(
    backend_callable(logistic_gradient_fortran_plan)(
      recycle_x,
      logistic_y,
      logistic_w,
      0.5
    ),
    logistic_oracle
  )
}

triangular_expected <- c(0, 10, 55)
triangular_inputs <- c(0, 4, 10)
expect_missing_generated_condition <- function(callable) {
  for (missing_bound in list(NA_real_, NaN)) {
    condition <- tryCatch(callable(missing_bound), error = identity)
    expect_true(inherits(condition, "runtime.invalid_logical_condition"))
    expect_equal(
      tccq_condition_diagnostic(condition)@code,
      "runtime.invalid_logical_condition"
    )
  }
}

if (rtinycc_jit_available) {
  triangular_jit <- tccq_plan_backend(
    triangular_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(triangular_jit@success)
  expect_equal(
    vapply(triangular_inputs, backend_callable(triangular_jit), numeric(1)),
    triangular_expected
  )
  expect_missing_generated_condition(backend_callable(triangular_jit))
}

if (can_build_shared_library("c")) {
  triangular_c_shared <- tccq_plan_backend(
    triangular_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(triangular_c_shared@success)
  expect_equal(
    vapply(triangular_inputs, backend_callable(triangular_c_shared), numeric(1)),
    triangular_expected
  )
  expect_missing_generated_condition(backend_callable(triangular_c_shared))
}

if (can_build_shared_library("fortran")) {
  triangular_fortran_shared <- tccq_plan_backend(
    triangular_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(triangular_fortran_shared@success)
  expect_equal(
    vapply(triangular_inputs, backend_callable(triangular_fortran_shared), numeric(1)),
    triangular_expected
  )
  expect_missing_generated_condition(backend_callable(triangular_fortran_shared))
}

conditional_cases <- list(
  c(n = 0, pivot = 2),
  c(n = 4, pivot = 2),
  c(n = 5, pivot = 3)
)
conditional_expected <- vapply(
  conditional_cases,
  function(arguments) conditional_recurrence(arguments[[1L]], arguments[[2L]]),
  numeric(1)
)
check_conditional_recurrence <- function(callable) {
  actual <- vapply(
    conditional_cases,
    function(arguments) callable(arguments[[1L]], arguments[[2L]]),
    numeric(1)
  )
  expect_equal(actual, conditional_expected)
  missing_condition <- tryCatch(callable(2, NA_real_), error = identity)
  expect_true(inherits(missing_condition, "runtime.invalid_logical_condition"))
}

if (rtinycc_jit_available) {
  conditional_jit <- tccq_plan_backend(
    conditional_program,
    tccq_rtinycc_backend(),
    tccq_backend_context(mode = "jit", target = "c")
  )
  expect_true(conditional_jit@success)
  check_conditional_recurrence(backend_callable(conditional_jit))
}

if (can_build_shared_library("c")) {
  conditional_c_shared <- tccq_plan_backend(
    conditional_program,
    tccq_c_backend(),
    tccq_backend_context(mode = "shared_library", target = "c")
  )
  expect_true(conditional_c_shared@success)
  check_conditional_recurrence(backend_callable(conditional_c_shared))
}

if (can_build_shared_library("fortran")) {
  conditional_fortran_shared <- tccq_plan_backend(
    conditional_program,
    tccq_fortran_backend(),
    tccq_backend_context(mode = "shared_library", target = "fortran")
  )
  expect_true(conditional_fortran_shared@success)
  check_conditional_recurrence(backend_callable(conditional_fortran_shared))
}
