library(tinytest)
library(tccquickr)

apotheosis_kernel <- function(x, y, w, lambda) {
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

result <- tccq_analyze(apotheosis_kernel)

expect_false(result@success)
expect_true(S7::S7_inherits(result@value, TccqProgram))
expect_true(length(result@diagnostics) >= 1L)
expect_true(all(vapply(
  result@diagnostics,
  function(x) S7::S7_inherits(x, TccqDiagnostic),
  logical(1)
)))
expect_true(any(vapply(
  result@diagnostics,
  function(x) identical(x@code, "frontend.unimplemented_call"),
  logical(1)
)))

program <- result@value
expect_equal(names(program@formals), c("x", "y", "w", "lambda"))
expect_equal(program@formals$x@type@shape@rank, 2L)
expect_equal(program@formals$lambda@type@shape@rank, 0L)

buffer_program <- function(bytes, scratch) {
  declare(type(bytes = raw(n), scratch = buffer(n)))
  bytes
}

buffer_result <- tccq_analyze(buffer_program)
expect_true(buffer_result@success)
expect_equal(buffer_result@value@formals$bytes@type@base, "raw")
expect_equal(buffer_result@value@formals$scratch@type@base, "buffer")

map_chain <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  exp(sqrt(x) + y)
}

map_result <- tccq_analyze(map_chain)
expect_true(map_result@success)
expect_true(map_result@value@attrs$lowered)
expect_equal(map_result@value@result, "value_0003")
expect_true(all(vapply(
  map_result@value@values,
  function(value) S7::S7_inherits(value, TccqValue),
  logical(1)
)))
expect_true(all(grepl(
  "^(formal|value)_[0-9]{4}$",
  vapply(map_result@value@values, function(value) value@id, character(1))
)))
expect_true(S7::S7_inherits(map_result@value@storage_plan, TccqStoragePlan))
expect_true(all(grepl(
  "^slot_[0-9]{4}$",
  vapply(map_result@value@storage_plan@slots, function(slot) slot@id, character(1))
)))
operation_values <- Filter(
  function(value) !value@op %in% c("formal", "literal"),
  map_result@value@values
)
expect_true(all(vapply(
  operation_values,
  function(value) S7::S7_inherits(value@attrs$resolved_op, TccqResolvedOp),
  logical(1)
)))
expect_true(all(vapply(
  operation_values,
  function(value) identical(value@attrs$resolved_op@target, "pure_c"),
  logical(1)
)))
expect_equal(map_result@value@regions[[1L]]@fusion_groups[[1L]]@kind, "map")
expect_equal(map_result@value@regions[[1L]]@fusion_groups[[1L]]@region_kind, "kernel")
expect_equal(map_result@value@regions[[1L]]@fusion_groups[[1L]]@target, "pure_c")

map_reduce <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  sum(exp(x) * y)
}

map_reduce_result <- tccq_analyze(map_reduce)
expect_true(map_reduce_result@success)
expect_true(map_reduce_result@value@attrs$lowered)
expect_equal(map_reduce_result@value@attrs$lowering$strategy, "map-reduce-expression")

reduction_value <- map_reduce_result@value@values[[map_reduce_result@value@result]]
expect_equal(reduction_value@op, "sum")
expect_equal(reduction_value@type@shape@rank, 0L)
expect_equal(reduction_value@attrs$lowering, "reduction")
expect_equal(reduction_value@attrs$reducer, "sum")
expect_true(S7::S7_inherits(reduction_value@attrs$reduction, TccqReductionSpec))
expect_true(S7::S7_inherits(reduction_value@attrs$reduction@signature, TccqOpSignature))
expect_true(S7::S7_inherits(reduction_value@attrs$identity, TccqLiteral))

reduction_fusion <- map_reduce_result@value@regions[[1L]]@fusion_groups[[1L]]
expect_equal(reduction_fusion@kind, "map_reduce")
expect_equal(reduction_fusion@region_kind, "kernel")
expect_equal(reduction_fusion@domain@shape@rank, 1L)
expect_equal(reduction_fusion@attrs$reducer, "sum")
expect_equal(map_reduce_result@value@storage_plan@attrs$strategy, "fused-map-reduce")

power_program <- function(x) {
  declare(type(x = integer(n)))
  x^2L
}

power_result <- tccq_analyze(power_program)
expect_true(power_result@success)
expect_true(power_result@value@attrs$lowered)
power_value <- power_result@value@values[[power_result@value@result]]
expect_equal(power_value@op, "^")
expect_equal(power_value@type@base, "double")
expect_true(S7::S7_inherits(power_value@attrs$elementwise, TccqElementwiseSpec))
expect_true(S7::S7_inherits(power_value@attrs$elementwise@signature, TccqOpSignature))

negation_program <- function(x) {
  declare(type(x = double(n)))
  -x
}

negation_result <- tccq_analyze(negation_program)
expect_true(negation_result@success)
expect_true(negation_result@value@attrs$lowered)
negation_value <- negation_result@value@values[[negation_result@value@result]]
expect_equal(negation_value@op, "-")
expect_equal(length(negation_value@inputs), 1L)
expect_true(S7::S7_inherits(negation_value@attrs$elementwise, TccqElementwiseSpec))

bad_elementwise_arity <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  sqrt(x, y)
}

bad_elementwise_arity_result <- tccq_analyze(bad_elementwise_arity)
expect_false(bad_elementwise_arity_result@success)
expect_false(bad_elementwise_arity_result@value@attrs$lowered)
expect_true(any(vapply(
  bad_elementwise_arity_result@diagnostics,
  function(diagnostic) {
    identical(diagnostic@code, "frontend.unimplemented_call") &&
      identical(diagnostic@data$call, "sqrt")
  },
  logical(1)
)))
bad_elementwise_arity_error <- tryCatch(
  tccq_analyze(bad_elementwise_arity, strict = TRUE),
  error = identity
)
expect_true(inherits(bad_elementwise_arity_error, "tccq_error"))

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

custom_elementwise_result <- tccq_analyze(custom_elementwise, registry = square_registry)
expect_true(custom_elementwise_result@success)
expect_true(custom_elementwise_result@value@attrs$lowered)
custom_elementwise_value <- custom_elementwise_result@value@values[[custom_elementwise_result@value@result]]
expect_equal(custom_elementwise_value@op, "square")
expect_true(S7::S7_inherits(custom_elementwise_value@attrs$elementwise, TccqElementwiseSpec))

bound_chain <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  shifted <- sqrt(x)
  weighted <- exp(shifted) * y
  weighted + y
}

bound_result <- tccq_analyze(bound_chain)
expect_true(bound_result@success)
expect_true(bound_result@value@attrs$lowered)
expect_equal(bound_result@value@result, "value_0004")
expect_equal(bound_result@value@attrs$lowering$local_bindings$shifted, "value_0001")
expect_equal(bound_result@value@attrs$lowering$local_bindings$weighted, "value_0003")
expect_equal(bound_result@value@values$value_0004@inputs, list("value_0003", "formal_0002"))

rebound_local <- function(x) {
  declare(type(x = double(n)))
  shifted <- sqrt(x)
  shifted <- exp(x)
  shifted
}

rebound_result <- tccq_analyze(rebound_local)
expect_false(rebound_result@success)
expect_false(rebound_result@value@attrs$lowered)
expect_true(any(vapply(
  rebound_result@diagnostics,
  function(x) identical(x@code, "lowering.local_rebinding"),
  logical(1)
)))

formal_rebinding <- function(x) {
  declare(type(x = double(n)))
  x <- sqrt(x)
  x
}

formal_rebinding_result <- tccq_analyze(formal_rebinding)
expect_false(formal_rebinding_result@success)
expect_false(formal_rebinding_result@value@attrs$lowered)
expect_true(any(vapply(
  formal_rebinding_result@diagnostics,
  function(x) identical(x@code, "lowering.formal_assignment"),
  logical(1)
)))

formal_mutation <- function(x) {
  declare(type(x = double(n)))
  x[1L] <- 2
  x
}

formal_mutation_result <- tccq_analyze(formal_mutation)
expect_false(formal_mutation_result@success)
expect_false(formal_mutation_result@value@attrs$lowered)
expect_true(any(vapply(
  formal_mutation_result@diagnostics,
  function(x) identical(x@code, "lowering.formal_mutation"),
  logical(1)
)))

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

custom_reduce_result <- tccq_analyze(custom_reduce, registry = fold_add_registry)
expect_true(custom_reduce_result@success)
expect_true(custom_reduce_result@value@attrs$lowered)
expect_equal(custom_reduce_result@value@attrs$lowering$strategy, "map-reduce-expression")
custom_reduction_value <- custom_reduce_result@value@values[[custom_reduce_result@value@result]]
expect_equal(custom_reduction_value@op, "fold_add")
expect_equal(custom_reduction_value@attrs$reducer, "fold_add")
expect_true(S7::S7_inherits(custom_reduction_value@attrs$reduction, TccqReductionSpec))

call_program <- function(x) {
  declare(type(x = double(n)))
  if (length(x) > 0L) {
    x[1L] + sqrt(x[1L])
  } else {
    0
  }
}

calls <- tccq_collect_calls(body(call_program))
call_names <- vapply(calls, function(x) x@name, character(1))
call_kinds <- setNames(vapply(calls, function(x) x@kind, character(1)), call_names)

expect_true("if" %in% call_names)
expect_equal(call_kinds[["if"]], "control")
expect_equal(call_kinds[["["]], "index")
expect_equal(call_kinds[["+"]], "operator")

replacement_calls <- tccq_collect_calls(quote(x[1L] <- 2L))
replacement_names <- vapply(replacement_calls, function(x) x@name, character(1))
replacement_kinds <- setNames(
  vapply(replacement_calls, function(x) x@kind, character(1)),
  replacement_names
)

expect_equal(replacement_kinds[["<-"]], "assignment")
expect_equal(replacement_kinds[["["]], "index")
expect_equal(tccq_call("[<-")@kind, "replacement")
expect_equal(tccq_call("function")@kind, "function_definition")

expect_true(S7::S7_inherits(buffer_result@value@call_index, TccqCallIndex))
expect_true(all(nzchar(vapply(
  buffer_result@value@call_index@calls,
  function(x) x@id,
  character(1)
))))

buffer_semantics <- buffer_result@value@call_index@semantics
expect_true(length(buffer_semantics) > 0L)
expect_true(all(vapply(
  buffer_semantics,
  function(x) S7::S7_inherits(x, TccqCallSemantics),
  logical(1)
)))
expect_true(any(vapply(
  buffer_semantics,
  function(x) identical(x@call@name, "declare") &&
    identical(x@evaluator_kind, "compiler_directive"),
  logical(1)
)))

local({
  local_op <- function(x) x
  local_program <- function(x) {
    declare(type(x = double(n)))
    local_op(x)
  }
  local_result <- tccq_analyze(
    local_program,
    registry = tccq_op_registry_add(
      tccq_default_op_registry(),
      tccq_op_impl("local_op", target = "r_language")
    )
  )
  local_semantics <- local_result@value@call_index@semantics
  local_op_semantics <- Filter(
    function(x) identical(x@call@name, "local_op"),
    local_semantics
  )[[1L]]
  expect_equal(local_op_semantics@evaluator_kind, "closure")
  expect_equal(local_op_semantics@forcing_policy, "lazy")
  expect_true(local_op_semantics@lexical_scope)
})
