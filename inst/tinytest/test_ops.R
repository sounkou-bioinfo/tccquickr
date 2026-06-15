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

resolved_custom <- tccq_resolve_call(registry, call, tccq_op_context())
expect_true(resolved_custom@success)
expect_true(S7::S7_inherits(resolved_custom@value, TccqResolvedOp))
expect_equal(resolved_custom@value@call@name, "custom_op")
expect_equal(resolved_custom@value@implementation@target, "r_api")
expect_true(resolved_custom@value@uses_rapi)

render_context <- tccq_op_render_context(language = "c", backend_id = "unit_backend")
expect_true(S7::S7_inherits(render_context, TccqOpRenderContext))

same_type_signature <- tccq_op_signature(
  "same_type",
  c(1L, 2L),
  result_type = function(input_types) input_types[[1L]]
)
expect_true(S7::S7_inherits(same_type_signature, TccqOpSignature))
expect_equal(same_type_signature@arity, c(1L, 2L))
same_type_signature_result <- tccq_op_signature_result_type(
  same_type_signature,
  list(tccq_type("integer"))
)
expect_true(same_type_signature_result@success)
expect_equal(same_type_signature_result@value@base, "integer")
bad_signature_arity <- tryCatch(
  tccq_op_signature(
    "bad",
    1.5,
    result_type = function(input_types) input_types[[1L]]
  ),
  error = identity
)
expect_true(inherits(bad_signature_arity, "tccq_error"))

unrenderable_custom <- tccq_op_render(
  resolved_custom@value@implementation,
  "input_0001",
  render_context
)
expect_false(unrenderable_custom@success)
expect_true(any(vapply(
  unrenderable_custom@diagnostics,
  function(x) identical(x@code, "ops.unrenderable_operation"),
  logical(1)
)))

unresolved_custom <- tccq_resolve_call(
  registry,
  call,
  tccq_op_context(allow_rapi = FALSE)
)
expect_false(unresolved_custom@success)
expect_true(any(vapply(
  unresolved_custom@diagnostics,
  function(x) identical(x@code, "ops.unresolved_call"),
  logical(1)
)))

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

bad_elementwise_arity <- tryCatch(
  tccq_elementwise_spec(
    "bad",
    1.5,
    result_type = function(input_types) input_types[[1L]]
  ),
  error = identity
)
expect_true(inherits(bad_elementwise_arity, "tccq_error"))

resolved_plus <- tccq_resolve_call(default_registry, tccq_call("+"), tccq_op_context())
expect_true(resolved_plus@success)
expect_true(S7::S7_inherits(resolved_plus@value@elementwise, TccqElementwiseSpec))
expect_true(S7::S7_inherits(resolved_plus@value@elementwise@signature, TccqOpSignature))
plus_result_type <- tccq_elementwise_result_type(
  resolved_plus@value@elementwise,
  list(tccq_type("integer", tccq_shape(tccq_dim_symbol("n"))), tccq_type("double"))
)
expect_true(plus_result_type@success)
expect_equal(plus_result_type@value@base, "double")
expect_equal(plus_result_type@value@shape@rank, 1L)
c_plus <- tccq_op_render(
  resolved_plus@value@implementation,
  c("left", "right"),
  render_context
)
expect_true(c_plus@success)
expect_equal(c_plus@value, "(left + right)")

resolved_power <- tccq_resolve_call(default_registry, tccq_call("^"), tccq_op_context())
expect_true(S7::S7_inherits(resolved_power@value@elementwise, TccqElementwiseSpec))
power_result_type <- tccq_elementwise_result_type(
  resolved_power@value@elementwise,
  list(tccq_type("integer", tccq_shape(tccq_dim_symbol("n"))), tccq_type("integer"))
)
expect_true(power_result_type@success)
expect_equal(power_result_type@value@base, "double")
fortran_power <- tccq_op_render(
  resolved_power@value@implementation,
  c("left", "right"),
  tccq_op_render_context(language = "fortran", backend_id = "unit_backend")
)
expect_true(fortran_power@success)
expect_equal(fortran_power@value, "(left ** right)")

unary_foo <- tccq_op_impl(
  "foo",
  target = "pure_c",
  region_kind = "kernel",
  render = function(operands, context) operands[[1L]],
  elementwise = tccq_elementwise_spec(
    "foo_unary",
    1L,
    result_type = function(input_types) input_types[[1L]]
  )
)
binary_foo <- tccq_op_impl(
  "foo",
  target = "pure_c",
  region_kind = "kernel",
  render = function(operands, context) sprintf("(%s + %s)", operands[[1L]], operands[[2L]]),
  elementwise = tccq_elementwise_spec(
    "foo_binary",
    2L,
    result_type = function(input_types) input_types[[1L]]
  )
)
foo_registry <- tccq_op_registry(c(unary_foo, binary_foo))
resolved_binary_foo <- tccq_resolve_call(
  foo_registry,
  tccq_call("foo", expr = quote(foo(x, y))),
  tccq_op_context()
)
expect_true(resolved_binary_foo@success)
expect_equal(resolved_binary_foo@value@elementwise@name, "foo_binary")
unresolved_ternary_foo <- tccq_resolve_call(
  foo_registry,
  tccq_call("foo", expr = quote(foo(x, y, z))),
  tccq_op_context()
)
expect_false(unresolved_ternary_foo@success)

resolved_sum <- tccq_resolve_call(default_registry, tccq_call("sum"), tccq_op_context())
expect_true(resolved_sum@success)
expect_equal(resolved_sum@value@target, "pure_c")
expect_equal(resolved_sum@value@region_kind, "kernel")
expect_true(S7::S7_inherits(resolved_sum@value@reduction, TccqReductionSpec))
expect_true(S7::S7_inherits(resolved_sum@value@reduction@signature, TccqOpSignature))
expect_equal(resolved_sum@value@reduction@signature@arity, 1L)
sum_identity <- tccq_reduction_identity(resolved_sum@value@reduction, tccq_type("double"))
expect_true(sum_identity@success)
expect_true(S7::S7_inherits(sum_identity@value, TccqLiteral))
expect_equal(sum_identity@value@value, 0)
sum_combine <- tccq_reduction_combine(
  resolved_sum@value@reduction,
  "accumulator",
  "current",
  render_context
)
expect_true(sum_combine@success)
expect_equal(sum_combine@value, "accumulator + current")
unrendered_sum <- tccq_op_render(
  resolved_sum@value@implementation,
  "input_0001",
  render_context
)
expect_false(unrendered_sum@success)
expect_true(any(vapply(
  unrendered_sum@diagnostics,
  function(x) identical(x@code, "ops.unrenderable_operation"),
  logical(1)
)))
unresolved_binary_sum <- tccq_resolve_call(
  default_registry,
  tccq_call("sum", expr = quote(sum(x, y))),
  tccq_op_context()
)
expect_false(unresolved_binary_sum@success)

if_semantics <- tccq_call_semantics(tccq_call("if"))
expect_equal(if_semantics@evaluator_kind, "special")
expect_equal(if_semantics@forcing_policy, "special")
expect_true(if_semantics@control)

plus_semantics <- tccq_call_semantics(tccq_call("+"))
expect_equal(plus_semantics@evaluator_kind, "builtin")
expect_equal(plus_semantics@forcing_policy, "eager")
expect_equal(plus_semantics@dispatch_kind, "group_generic")

mean_semantics <- tccq_call_semantics(tccq_call("mean"))
expect_equal(mean_semantics@evaluator_kind, "closure")
expect_equal(mean_semantics@forcing_policy, "lazy")
expect_equal(mean_semantics@dispatch_kind, "s3")
expect_true(mean_semantics@lexical_scope)

replacement_semantics <- tccq_call_semantics(tccq_call("[<-"))
expect_true(replacement_semantics@replacement)
expect_equal(replacement_semantics@forcing_policy, "replacement")
expect_equal(replacement_semantics@dispatch_kind, "replacement")

call_index <- tccq_call_index(list(tccq_call("+"), tccq_call("if")))
expect_true(S7::S7_inherits(call_index, TccqCallIndex))
expect_equal(
  vapply(call_index@calls, function(x) x@id, character(1)),
  c("call_0001", "call_0002")
)
expect_equal(
  vapply(call_index@semantics, function(x) x@call@id, character(1)),
  c("call_0001", "call_0002")
)

bad_index <- tryCatch(
  TccqCallIndex(
    calls = list(tccq_call("+", id = "call_a")),
    semantics = list(tccq_call_semantics(tccq_call("+", id = "call_b"))),
    attrs = list()
  ),
  error = function(err) err
)
expect_true(inherits(bad_index, "error"))

custom_program <- function(x) {
  declare(type(x = double(n)))
  custom_op(x)
}

default_result <- tccq_analyze(custom_program)
expect_false(default_result@success)
expect_true(any(vapply(
  default_result@diagnostics,
  function(x) identical(x@data$call, "custom_op"),
  logical(1)
)))

custom_result <- tccq_analyze(custom_program, registry = registry)
expect_false(custom_result@success)
expect_false(custom_result@value@attrs$lowered)
expect_true(any(vapply(
  custom_result@diagnostics,
  function(x) identical(x@code, "lowering.unsupported_call"),
  logical(1)
)))

effectful_plus <- tccq_op_impl(
  "+",
  target = "r_api",
  uses_rapi = TRUE,
  pure = FALSE
)
effectful_registry <- tccq_op_registry(c(
  list(effectful_plus),
  default_registry@implementations
))
effectful_program <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  x + y
}
effectful_result <- tccq_analyze(effectful_program, registry = effectful_registry)
expect_false(effectful_result@success)
expect_false(effectful_result@value@attrs$lowered)
expect_true(any(vapply(
  effectful_result@diagnostics,
  function(x) identical(x@code, "lowering.effectful_operation"),
  logical(1)
)))

device_result <- tccq_analyze(
  custom_program,
  registry = registry,
  context = tccq_op_context(region_kind = "device", memory_space = "device", allow_rapi = FALSE)
)
expect_false(device_result@success)
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
