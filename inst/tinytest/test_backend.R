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
expect_true(S7::S7_inherits(expression_result@value@resolved_op, TccqResolvedOp))

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
expect_true(S7::S7_inherits(c_products@expression, TccqExpression))
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
expect_equal(c_interface@extent_symbols, "n")
expect_equal(c_interface@extent_names, "extent_n")
expect_equal(c_interface@index_names, "axis_0001")
expect_equal(c_interface@result_count_name, "result_count_0001")
expect_equal(c_interface@parameter_value_ids, c("formal_0001", "formal_0002"))
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
expect_equal(matrix_c_interface@attrs$result_type@shape@rank, 2L)
expect_true(S7::S7_inherits(matrix_c_interface@domain, TccqDomain))
expect_equal(matrix_c_interface@domain@shape@rank, 2L)
expect_equal(matrix_c_interface@extent_symbols, c("n", "p"))
expect_equal(matrix_c_interface@extent_names, c("extent_n", "extent_p"))
expect_equal(matrix_c_interface@index_names, c("axis_0001", "axis_0002"))
expect_true(S7::S7_inherits(backend_products(matrix_c_source_plan)@expression, TccqExpression))
expect_equal(backend_products(matrix_c_source_plan)@expression@type@shape@rank, 2L)
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
expect_equal(matrix_fortran_interface@attrs$result_type@shape@rank, 2L)
expect_true(S7::S7_inherits(matrix_fortran_interface@domain, TccqDomain))
expect_equal(matrix_fortran_interface@domain@shape@rank, 2L)
expect_equal(matrix_fortran_interface@extent_names, c("extent_n", "extent_p"))

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
expect_true(S7::S7_inherits(fortran_products@expression, TccqExpression))
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
expect_true(grepl("exp(sqrt(input_0001[axis_0001]))", backend_source(bound_c_source_plan), fixed = TRUE))

bound_fortran_source_plan <- tccq_plan_backend(
  bound_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(bound_fortran_source_plan@success)
expect_true(grepl("exp(sqrt(input_0001(axis_0001 + 1)))", backend_source(bound_fortran_source_plan), fixed = TRUE))

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
expect_true(S7::S7_inherits(reduction_expression@value@attrs$operation, TccqLoweredOperation))
expect_equal(reduction_expression@value@attrs$operation@family, "reduction")

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
expect_equal(reduction_c_interface@accumulator_name, "accumulator_0001")
expect_true(S7::S7_inherits(reduction_c_interface@domain, TccqDomain))
expect_equal(reduction_c_interface@domain@shape@rank, 1L)
expect_equal(reduction_c_interface@extent_names, "extent_n")
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
expect_equal(matrix_reduction_c_interface@attrs$result_type@shape@rank, 0L)
expect_true(S7::S7_inherits(matrix_reduction_c_interface@domain, TccqDomain))
expect_equal(matrix_reduction_c_interface@domain@shape@rank, 2L)
expect_equal(matrix_reduction_c_interface@extent_names, c("extent_n", "extent_p"))

matrix_reduction_fortran_source_plan <- tccq_plan_backend(
  matrix_reduction_program@value,
  tccq_fortran_backend(),
  tccq_backend_context(mode = "source", target = "fortran")
)
expect_true(matrix_reduction_fortran_source_plan@success)
matrix_reduction_fortran_interface <- backend_interface(matrix_reduction_fortran_source_plan)
expect_equal(matrix_reduction_fortran_interface@kind, "loop_nest")
expect_equal(matrix_reduction_fortran_interface@attrs$result_type@shape@rank, 0L)
expect_true(S7::S7_inherits(matrix_reduction_fortran_interface@domain, TccqDomain))
expect_equal(matrix_reduction_fortran_interface@domain@shape@rank, 2L)
expect_equal(matrix_reduction_fortran_interface@extent_names, c("extent_n", "extent_p"))

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
expect_equal(column_axis_c_interface@extent_names, c("extent_n", "extent_p"))
expect_equal(column_axis_c_interface@result_count_name, "result_count_0001")
expect_equal(column_axis_c_interface@accumulator_name, "accumulator_0001")
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
expect_equal(matrix_vector_c_interface@extent_symbols, c("n", "p"))
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
expect_equal(normalize_groups[[1L]]@outputs, normalize_groups[[2L]]@values[[1L]]@inputs[[2L]])

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
expect_true(nzchar(normalize_intermediate@attrs$scalar_name))
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
expect_true(nzchar(axis_feed_nests@value[[1L]]@attrs$buffer_name))
expect_true(S7::S7_inherits(axis_feed_nests@value[[1L]]@output, TccqAccess))

axis_feed_groups <- axis_reduction_feed_program@value@regions[[1L]]@fusion_groups
expect_equal(
  vapply(axis_feed_groups, function(group) group@kind, character(1)),
  c("axis_reduce", "map")
)

axis_feed_slots <- axis_reduction_feed_program@value@storage_plan@slots
axis_feed_buffer_slot <- Filter(
  function(slot) identical(slot@value_id, axis_feed_nests@value[[1L]]@attrs$result_value_id),
  axis_feed_slots
)[[1L]]
expect_true(axis_feed_buffer_slot@materialized)
expect_false(axis_feed_buffer_slot@reusable)

contraction_feed <- function(x, w, y) {
  declare(type(x = double(n, p), w = double(p), y = double(n)))
  (x %*% w) + y
}
contraction_feed_program <- tccq_analyze(contraction_feed)
expect_true(contraction_feed_program@success)
contraction_feed_nests <- tccq_program_loop_nests(contraction_feed_program@value)
expect_true(contraction_feed_nests@success)
expect_equal(length(contraction_feed_nests@value), 2L)
expect_true(nzchar(contraction_feed_nests@value[[1L]]@attrs$buffer_name))

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

if (rtinycc_jit_available) {
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
