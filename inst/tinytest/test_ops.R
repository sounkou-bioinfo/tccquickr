library(tinytest)
library(tccquickr)

impl <- tccq_op_impl(
  "custom_op",
  target = "r_api",
  uses_rapi = TRUE
)

expect_true(s7contract::has_trait(impl, TccqOpImplementation))

registry <- tccq_op_registry_add(tccq_default_op_registry(), impl)
call <- tccq_call("custom_op")

expect_true(tccq_registry_supports(registry, call, tccq_op_context()))
expect_false(tccq_registry_supports(
  registry,
  call,
  tccq_op_context(allow_rapi = FALSE)
))

default_registry <- tccq_default_op_registry()
expect_true(tccq_registry_supports(
  default_registry,
  tccq_call("if"),
  tccq_op_context(target = "r_language")
))
expect_false(tccq_registry_supports(
  default_registry,
  tccq_call("if"),
  tccq_op_context(target = "pure_c")
))

custom_program <- function(x) {
  declare(type(x = double(n)))
  custom_op(x)
}

default_result <- tccq_analyze(custom_program)
expect_false(default_result@ok)
expect_true(any(vapply(
  default_result@diagnostics,
  function(x) identical(x@data$call, "custom_op"),
  logical(1)
)))

custom_result <- tccq_analyze(custom_program, registry = registry)
expect_true(custom_result@ok)

device_result <- tccq_analyze(
  custom_program,
  registry = registry,
  context = tccq_op_context(region_kind = "device", memory_space = "device", allow_rapi = FALSE)
)
expect_false(device_result@ok)
expect_true(any(vapply(
  device_result@diagnostics,
  function(x) identical(x@data$call, "custom_op"),
  logical(1)
)))

opaque_impl <- tccq_opaque_op_impl()
opaque_call <- tccq_call("unknown_user_function")
opaque_registry <- tccq_op_registry_add(tccq_default_op_registry(), opaque_impl)

expect_true(tccq_registry_supports(
  opaque_registry,
  opaque_call,
  tccq_op_context(target = "opaque")
))
expect_false(opaque_impl@uses_rapi)
expect_false(opaque_impl@boundary)
expect_equal(opaque_impl@target, "opaque")
