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

planned <- tccq_plan_backend(program, backend, context)
expect_false(planned@ok)
expect_true(S7::S7_inherits(planned@value, TccqBackendPlan))
expect_equal(planned@value@backend_id, "rtinycc")
expect_equal(planned@value@family, "c")
expect_equal(planned@value@mode, "jit")
expect_true(any(vapply(
  planned@diagnostics,
  function(x) identical(x@code, "backend.lowering_absent"),
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
expect_false(missing_capability@ok)
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
expect_false(unsupported_mode@ok)
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
expect_false(compiled@ok)
expect_true(S7::S7_inherits(compiled@value, TccqBackendPlanSet))
expect_true(length(compiled@value@plans) >= 4L)
expect_true(any(vapply(
  compiled@diagnostics,
  function(x) identical(x@code, "backend.lowering_absent"),
  logical(1)
)))
