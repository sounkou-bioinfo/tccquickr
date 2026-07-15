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

# The original apotheosis composite: column statistics, standardization via
# recycling, a contraction over a computed operand, a crossprod, dimension
# values, and elementwise chains — analyzed and lowered.
expect_true(result@success)
expect_true(S7::S7_inherits(result@value, TccqProgram))
expect_true(result@value@attrs$lowered)

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
expect_equal(length(buffer_result@value@regions[[1L]]@fusion_groups), 0L)

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
operation_payloads <- lapply(operation_values, function(value) value@attrs$operation)
expect_true(all(vapply(
  operation_payloads,
  function(operation) S7::S7_inherits(operation, TccqLoweredOperation),
  logical(1)
)))
expect_true(all(vapply(
  operation_payloads,
  function(operation) identical(operation@resolved_op@target, "pure_c"),
  logical(1)
)))
expect_true(all(vapply(
  operation_payloads,
  function(operation) S7::S7_inherits(operation@signature, TccqOpSignature),
  logical(1)
)))
expect_true(all(vapply(
  operation_payloads,
  function(operation) S7::S7_inherits(operation@domain_policy, TccqDomainPolicy),
  logical(1)
)))
map_fusion <- map_result@value@regions[[1L]]@fusion_groups[[1L]]
expect_equal(map_fusion@kind, "map")
expect_equal(map_fusion@region_kind, "kernel")
expect_equal(map_fusion@target, "pure_c")
expect_true(S7::S7_inherits(map_fusion@contract, TccqFusionContract))
expect_equal(map_fusion@contract@fusion_kind, "map")
expect_equal(map_fusion@contract@storage_strategy, "fused-elementwise")
expect_equal(length(map_fusion@contract@operation_signatures), length(operation_values))
expect_true(all(vapply(
  map_fusion@contract@operation_signatures,
  function(signature) S7::S7_inherits(signature, TccqOpSignature),
  logical(1)
)))
expect_true(all(vapply(
  map_fusion@contract@domain_policies,
  function(domain_policy) S7::S7_inherits(domain_policy, TccqDomainPolicy),
  logical(1)
)))

map_reduce <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  sum(exp(x) * y)
}

map_reduce_result <- tccq_analyze(map_reduce)
expect_true(map_reduce_result@success)
expect_true(map_reduce_result@value@attrs$lowered)
expect_equal(map_reduce_result@value@attrs$lowering$strategy, "map-reduce-expression")

reduction_value <- map_reduce_result@value@values[[map_reduce_result@value@result]]
reduction_operation <- reduction_value@attrs$operation
expect_equal(reduction_value@op, "sum")
expect_equal(reduction_value@type@shape@rank, 0L)
expect_true(S7::S7_inherits(reduction_operation, TccqLoweredOperation))
expect_equal(reduction_operation@family, "reduction")
expect_equal(reduction_operation@reduction@name, "sum")
expect_true(S7::S7_inherits(reduction_operation@reduction, TccqReductionSpec))
expect_true(S7::S7_inherits(reduction_operation@signature, TccqOpSignature))
expect_true(S7::S7_inherits(reduction_operation@domain_policy, TccqDomainPolicy))
expect_true(S7::S7_inherits(reduction_operation@identity, TccqLiteral))

reduction_fusion <- map_reduce_result@value@regions[[1L]]@fusion_groups[[1L]]
expect_equal(reduction_fusion@kind, "map_reduce")
expect_equal(reduction_fusion@region_kind, "kernel")
expect_equal(reduction_fusion@domain@shape@rank, 1L)
expect_true(S7::S7_inherits(reduction_fusion@contract, TccqFusionContract))
expect_equal(reduction_fusion@contract@fusion_kind, "map_reduce")
expect_equal(reduction_fusion@contract@storage_strategy, "fused-map-reduce")
expect_equal(reduction_fusion@contract@result_operation@reduction@name, "sum")
expect_true(all(vapply(
  reduction_fusion@contract@operation_signatures,
  function(signature) S7::S7_inherits(signature, TccqOpSignature),
  logical(1)
)))
expect_true(all(vapply(
  reduction_fusion@contract@domain_policies,
  function(domain_policy) S7::S7_inherits(domain_policy, TccqDomainPolicy),
  logical(1)
)))

matrix_reduce <- function(x, y) {
  declare(type(x = double(n, p), y = double(n, p)))
  sum(exp(x) * y)
}

matrix_reduce_result <- tccq_analyze(matrix_reduce)
expect_true(matrix_reduce_result@success)
expect_true(matrix_reduce_result@value@attrs$lowered)
matrix_reduction_value <- matrix_reduce_result@value@values[[matrix_reduce_result@value@result]]
matrix_reduction_operation <- matrix_reduction_value@attrs$operation
expect_equal(matrix_reduction_value@type@shape@rank, 0L)
expect_true(S7::S7_inherits(matrix_reduction_operation, TccqLoweredOperation))
expect_equal(matrix_reduction_operation@family, "reduction")
matrix_reduction_fusion <- matrix_reduce_result@value@regions[[1L]]@fusion_groups[[1L]]
expect_equal(matrix_reduction_fusion@kind, "map_reduce")
expect_equal(matrix_reduction_fusion@domain@shape@rank, 2L)
expect_true(S7::S7_inherits(matrix_reduction_fusion@contract, TccqFusionContract))

column_axis_reduce <- function(x) {
  declare(type(x = double(n, p)))
  colSums(exp(x))
}

column_axis_reduce_result <- tccq_analyze(column_axis_reduce)
expect_true(column_axis_reduce_result@success)
expect_true(column_axis_reduce_result@value@attrs$lowered)
expect_equal(column_axis_reduce_result@value@attrs$lowering$strategy, "axis-reduce-expression")
column_axis_value <- column_axis_reduce_result@value@values[[column_axis_reduce_result@value@result]]
column_axis_operation <- column_axis_value@attrs$operation
expect_equal(column_axis_value@op, "colSums")
expect_equal(column_axis_value@type@shape@rank, 1L)
expect_equal(column_axis_value@type@shape@dims[[1L]]@label, "p")
expect_true(S7::S7_inherits(column_axis_operation, TccqLoweredOperation))
expect_equal(column_axis_operation@family, "reduction")
expect_equal(column_axis_operation@reduction@name, "sum")
expect_equal(column_axis_operation@attrs$reduction_kind, "axis")
expect_equal(column_axis_operation@attrs$reduction_axes, 1L)
expect_equal(column_axis_operation@attrs$kept_axes, 2L)
column_axis_fusion <- column_axis_reduce_result@value@regions[[1L]]@fusion_groups[[1L]]
expect_equal(column_axis_fusion@kind, "axis_reduce")
expect_equal(column_axis_fusion@domain@shape@rank, 2L)
expect_true(S7::S7_inherits(column_axis_fusion@contract, TccqFusionContract))
expect_equal(column_axis_fusion@contract@fusion_kind, "axis_reduce")
expect_equal(column_axis_fusion@contract@storage_strategy, "fused-axis-reduce")

power_program <- function(x) {
  declare(type(x = integer(n)))
  x^2L
}

power_result <- tccq_analyze(power_program)
expect_true(power_result@success)
expect_true(power_result@value@attrs$lowered)
power_value <- power_result@value@values[[power_result@value@result]]
power_operation <- power_value@attrs$operation
expect_equal(power_value@op, "^")
expect_equal(power_value@type@base, "double")
expect_true(S7::S7_inherits(power_operation, TccqLoweredOperation))
expect_equal(power_operation@family, "elementwise")
expect_true(S7::S7_inherits(power_operation@elementwise, TccqElementwiseSpec))
expect_true(S7::S7_inherits(power_operation@signature, TccqOpSignature))
expect_true(S7::S7_inherits(power_operation@domain_policy, TccqDomainPolicy))

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
expect_true(S7::S7_inherits(negation_value@attrs$operation@elementwise, TccqElementwiseSpec))

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
expect_true(S7::S7_inherits(custom_elementwise_value@attrs$operation, TccqLoweredOperation))
expect_true(S7::S7_inherits(custom_elementwise_value@attrs$operation@elementwise, TccqElementwiseSpec))

# A registry implementation must not be able to claim an S3 generic: the
# method table is a runtime fact, so lowering has to stop at a typed barrier.
square_generic_env <- new.env(parent = environment())
square_generic_env$square <- function(x) UseMethod("square")
s3_square_probe <- function(x) {
  declare(type(x = double(n)))
  square(x)
}
environment(s3_square_probe) <- square_generic_env
s3_square_result <- tccq_analyze(s3_square_probe, registry = square_registry)
expect_false(s3_square_result@success)
expect_true(any(vapply(
  s3_square_result@diagnostics,
  function(x) identical(x@code, "lowering.semantics_barrier"),
  logical(1)
)))

# quickr's declare dialect also allows one type() payload per formal, the
# form anvl's graph-to-quickr lowering emits.
per_formal_declare <- function(x, y) {
  declare(x = type(x = double(n)), y = type(y = double(n)))
  x + y
}
per_formal_result <- tccq_analyze(per_formal_declare)
expect_true(per_formal_result@success)
expect_equal(names(per_formal_result@value@formals), c("x", "y"))

declaration_only <- function(x) {
  declare(type(x = double(n)))
}
declaration_only_result <- tccq_analyze(declaration_only)
expect_false(declaration_only_result@success)
expect_true(any(vapply(
  declaration_only_result@diagnostics,
  function(diagnostic) identical(
    diagnostic@code,
    "lowering.missing_program_result"
  ),
  logical(1)
)))

# A declared length-1 vector recycles into anything, as in R.
length_one_recycle <- function(a, x) {
  declare(type(a = double(1L), x = double(n)))
  a * x + a
}
length_one_result <- tccq_analyze(length_one_recycle)
expect_true(length_one_result@success)

bound_chain <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  shifted <- sqrt(x)
  weighted <- exp(shifted) * y
  weighted + y
}

bound_result <- tccq_analyze(bound_chain)
expect_true(bound_result@success)
expect_true(bound_result@value@attrs$lowered)
expect_equal(bound_result@value@result, "value_0006")
bound_schedule <- bound_result@value@schedule
expect_true(S7::S7_inherits(bound_schedule, TccqProgramSchedule))
expect_equal(
  vapply(
    bound_schedule@steps,
    function(step) step@statement_index,
    integer(1)
  ),
  1:3
)
bound_bindings <- lapply(bound_schedule@steps[1:2], function(step) step@binding)
expect_true(S7::S7_inherits(
  bound_bindings[[1L]],
  TccqLocalBinding
))
expect_equal(bound_bindings[[1L]]@name, "shifted")
expect_equal(bound_bindings[[1L]]@value_id, "value_0001")
expect_equal(bound_bindings[[1L]]@statement_index, 1L)
expect_equal(bound_bindings[[2L]]@name, "weighted")
expect_equal(bound_bindings[[2L]]@value_id, "value_0004")
expect_equal(bound_bindings[[2L]]@statement_index, 2L)
expect_equal(
  vapply(bound_schedule@steps[[2L]]@uses, function(binding) binding@name, character(1)),
  "shifted"
)
expect_equal(
  vapply(bound_schedule@steps[[3L]]@uses, function(binding) binding@name, character(1)),
  "weighted"
)
expect_null(bound_schedule@steps[[3L]]@binding)
expect_null(bound_result@value@attrs$lowering$local_bindings)
expect_equal(bound_result@value@values$value_0006@inputs, list("value_0005", "formal_0002"))
expect_true(S7::S7_inherits(
  bound_result@value@values$value_0002,
  TccqBindingReference
))
expect_identical(
  bound_result@value@values$value_0002@binding,
  bound_bindings[[1L]]
)
expect_true(S7::S7_inherits(
  bound_result@value@values$value_0005,
  TccqBindingReference
))
expect_identical(
  bound_result@value@values$value_0005@binding,
  bound_bindings[[2L]]
)
bound_storage_slots <- bound_result@value@storage_plan@slots
bound_storage_slots_by_value <- bound_storage_slots
names(bound_storage_slots_by_value) <- vapply(
  bound_storage_slots,
  function(slot) slot@value_id,
  character(1)
)
expect_true(S7::S7_inherits(bound_storage_slots_by_value$value_0001@lifetime, TccqStorageLifetime))
expect_equal(bound_storage_slots_by_value$value_0001@lifetime@defined_at, 3L)
expect_equal(bound_storage_slots_by_value$value_0001@lifetime@last_used_at, 6L)
expect_true(S7::S7_inherits(bound_storage_slots_by_value$value_0004@lifetime, TccqStorageLifetime))
expect_equal(bound_storage_slots_by_value$value_0004@lifetime@defined_at, 6L)
expect_equal(bound_storage_slots_by_value$value_0004@lifetime@last_used_at, 8L)
expect_true(S7::S7_inherits(
  bound_storage_slots_by_value$value_0001@allocation,
  TccqStorageAllocation
))
expect_true(S7::S7_inherits(
  bound_storage_slots_by_value$value_0004@allocation,
  TccqStorageAllocation
))

fused_local_chain <- function(x) {
  declare(type(x = double(n)))
  transformed <- exp(x)
  exp(transformed)
}

fused_local_result <- tccq_analyze(fused_local_chain, strict = TRUE)
expect_true(fused_local_result@success)
fused_local_binding <- fused_local_result@value@schedule@steps[[1L]]@binding
fused_local_slots <- fused_local_result@value@storage_plan@slots
fused_local_slot <- fused_local_slots[[match(
  fused_local_binding@value_id,
  vapply(fused_local_slots, function(slot) slot@value_id, character(1))
)]]
expect_false(fused_local_slot@materialized)
expect_null(fused_local_slot@allocation)
expect_equal(length(tccq_program_loop_nests(fused_local_result@value)@value), 1L)

warning_local_chain <- function(x) {
  declare(type(x = double(n)))
  transformed <- sqrt(x)
  exp(transformed)
}

warning_local_result <- tccq_analyze(warning_local_chain, strict = TRUE)
warning_local_binding <- warning_local_result@value@schedule@steps[[1L]]@binding
warning_local_slots <- warning_local_result@value@storage_plan@slots
warning_local_slot <- warning_local_slots[[match(
  warning_local_binding@value_id,
  vapply(warning_local_slots, function(slot) slot@value_id, character(1))
)]]
expect_true(warning_local_slot@materialized)
expect_true(warning_local_result@value@schedule@steps[[1L]]@effect@may_warn)

duplicated_local_read <- function(x) {
  declare(type(x = double(n)))
  transformed <- exp(x)
  transformed / transformed
}

duplicated_local_result <- tccq_analyze(duplicated_local_read, strict = TRUE)
duplicated_local_binding <- duplicated_local_result@value@schedule@steps[[1L]]@binding
duplicated_local_slots <- duplicated_local_result@value@storage_plan@slots
duplicated_local_slot <- duplicated_local_slots[[match(
  duplicated_local_binding@value_id,
  vapply(duplicated_local_slots, function(slot) slot@value_id, character(1))
)]]
expect_true(duplicated_local_slot@materialized)
expect_equal(length(duplicated_local_result@value@schedule@steps[[2L]]@uses), 1L)

nonadjacent_local_read <- function(x) {
  declare(type(x = double(n)))
  first <- exp(x)
  second <- exp(x)
  first / second
}

nonadjacent_local_result <- tccq_analyze(nonadjacent_local_read, strict = TRUE)
nonadjacent_bindings <- lapply(
  nonadjacent_local_result@value@schedule@steps[1:2],
  function(step) step@binding
)
nonadjacent_slots <- nonadjacent_local_result@value@storage_plan@slots
nonadjacent_slots_by_value <- nonadjacent_slots
names(nonadjacent_slots_by_value) <- vapply(
  nonadjacent_slots,
  function(slot) slot@value_id,
  character(1)
)
expect_true(nonadjacent_slots_by_value[[nonadjacent_bindings[[1L]]@value_id]]@materialized)
expect_false(nonadjacent_slots_by_value[[nonadjacent_bindings[[2L]]@value_id]]@materialized)

control_local_chain <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  transformed <- if (flag) exp(x) else exp(-x)
  exp(transformed)
}

control_local_result <- tccq_analyze(control_local_chain, strict = TRUE)
control_local_binding <- control_local_result@value@schedule@steps[[1L]]@binding
control_local_slots <- control_local_result@value@storage_plan@slots
control_local_slot <- control_local_slots[[match(
  control_local_binding@value_id,
  vapply(control_local_slots, function(slot) slot@value_id, character(1))
)]]
expect_true(control_local_slot@materialized)

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

branch_program <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  if (flag) x else -x
}

branch_result <- tccq_analyze(branch_program)
expect_true(branch_result@success)
expect_true(branch_result@value@attrs$lowered)
branch_value <- branch_result@value@values[[branch_result@value@result]]
expect_true(S7::S7_inherits(branch_value, TccqBranch))
expect_true(S7::S7_inherits(branch_value, TccqValue))
expect_equal(branch_value@condition, "formal_0002")
expect_equal(branch_value@consequent, "formal_0001")
expect_equal(branch_value@inputs, list(
  branch_value@condition,
  branch_value@consequent,
  branch_value@alternative
))
expect_equal(branch_value@type@base, "double")
expect_equal(branch_value@type@shape@dims[[1L]]@label, "n")
expect_equal(branch_value@semantics@call@name, "if")
expect_equal(branch_value@semantics@forcing_policy, "special")
branch_fusion <- branch_result@value@regions[[1L]]@fusion_groups[[1L]]
expect_true(identical(branch_fusion@contract@result_value, branch_value))
expect_null(branch_fusion@contract@result_operation)
expect_true(branch_value@effect@may_error)

special_literal_branch <- function(flag) {
  declare(type(flag = logical(), returns = double()))
  if (flag) NaN else Inf
}

special_literal_result <- tccq_analyze(special_literal_branch)
expect_true(special_literal_result@success)
special_literal_values <- Filter(
  function(value) S7::S7_inherits(value@attrs$literal, TccqLiteral),
  special_literal_result@value@values
)
expect_equal(
  vapply(
    special_literal_values,
    function(value) value@attrs$literal@kind,
    character(1),
    USE.NAMES = FALSE
  ),
  c("nan", "pos_inf")
)

missing_condition_branch <- function(x) {
  declare(type(x = double(), returns = double()))
  if (NA) x else -x
}

missing_condition_result <- tccq_analyze(missing_condition_branch)
expect_true(missing_condition_result@success)
missing_condition_value <- missing_condition_result@value@values[[
  missing_condition_result@value@result
]]
missing_condition_literal <- missing_condition_result@value@values[[
  missing_condition_value@condition
]]@attrs$literal
expect_equal(missing_condition_literal@kind, "na")
expect_equal(missing_condition_literal@type@base, "logical")

branch_without_else <- function(flag) {
  declare(type(flag = logical()))
  if (flag) 1
}

branch_without_else_result <- tccq_analyze(branch_without_else)
expect_false(branch_without_else_result@success)
expect_true(any(vapply(
  branch_without_else_result@diagnostics,
  function(x) identical(x@code, "lowering.branch_requires_else"),
  logical(1)
)))

nonlogical_branch_condition <- function(x) {
  declare(type(x = double()))
  if (x) 1 else 2
}
nonlogical_branch_result <- tccq_analyze(nonlogical_branch_condition)
expect_false(nonlogical_branch_result@success)
expect_true(any(vapply(
  nonlogical_branch_result@diagnostics,
  function(x) identical(x@code, "lowering.invalid_branch_condition"),
  logical(1)
)))

incompatible_branch_arms <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  if (flag) x else 0
}
incompatible_branch_result <- tccq_analyze(incompatible_branch_arms)
expect_false(incompatible_branch_result@success)
expect_true(any(vapply(
  incompatible_branch_result@diagnostics,
  function(x) identical(x@code, "lowering.incompatible_branch_types"),
  logical(1)
)))

loop_boundary <- function(x) {
  declare(type(x = double(n)))
  repeat break
  x
}

loop_boundary_result <- tccq_analyze(loop_boundary)
expect_false(loop_boundary_result@success)
expect_true(any(vapply(
  loop_boundary_result@diagnostics,
  function(x) identical(x@code, "lowering.control_flow_boundary"),
  logical(1)
)))

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

triangular_result <- tccq_analyze(triangular_recurrence, strict = TRUE)
expect_true(triangular_result@success)
expect_true(S7::S7_inherits(triangular_result@value@schedule, TccqProgramSchedule))
expect_equal(length(triangular_result@value@schedule@steps), 0L)
expect_true(S7::S7_inherits(triangular_result@value@schedule@body, TccqValueBlock))
expect_equal(triangular_result@value@attrs$lowering$strategy, "sequential-control")
expect_equal(length(triangular_result@value@schedule@body@locals), 2L)
expect_true(all(vapply(
  triangular_result@value@schedule@body@locals,
  function(target) identical(target@kind, "cell") &&
    S7::S7_inherits(target@binding, TccqCell),
  logical(1)
)))
triangular_while <- Filter(
  function(statement) S7::S7_inherits(statement, TccqWhile),
  triangular_result@value@schedule@body@statements
)[[1L]]
expect_true(S7::S7_inherits(triangular_while@body, TccqBlock))
expect_false(S7::S7_inherits(triangular_while@body, TccqValueBlock))
expect_equal(length(triangular_while@body@statements), 2L)
expect_true(triangular_while@effect@writes)
expect_true(triangular_while@effect@may_error)
expect_true(any(vapply(
  triangular_result@value@values,
  S7::S7_inherits,
  logical(1),
  class = TccqCellReference
)))

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

conditional_result <- tccq_analyze(conditional_recurrence, strict = TRUE)
expect_true(conditional_result@success)
conditional_while <- Filter(
  function(statement) S7::S7_inherits(statement, TccqWhile),
  conditional_result@value@schedule@body@statements
)[[1L]]
procedural_if <- Filter(
  function(statement) S7::S7_inherits(statement, TccqIf),
  conditional_while@body@statements
)[[1L]]
expect_false(S7::S7_inherits(procedural_if, TccqConditional))
expect_true(S7::S7_inherits(procedural_if@consequent, TccqBlock))
expect_true(S7::S7_inherits(procedural_if@alternative, TccqBlock))
expect_equal(length(procedural_if@consequent@statements), 1L)
expect_equal(length(procedural_if@alternative@statements), 1L)
expect_true(procedural_if@effect@writes)
expect_true(procedural_if@effect@may_error)

conditional_without_else <- function(n) {
  declare(type(n = double()))
  iteration <- 0
  total <- 0
  while (iteration < n) {
    iteration <- iteration + 1
    if (iteration <= 2) total <- total + iteration
  }
  total
}

conditional_without_else_result <- tccq_analyze(
  conditional_without_else,
  strict = TRUE
)
expect_true(conditional_without_else_result@success)
without_else_while <- Filter(
  function(statement) S7::S7_inherits(statement, TccqWhile),
  conditional_without_else_result@value@schedule@body@statements
)[[1L]]
without_else_if <- Filter(
  function(statement) S7::S7_inherits(statement, TccqIf),
  without_else_while@body@statements
)[[1L]]
expect_equal(length(without_else_if@alternative@statements), 0L)

uninitialized_recurrence <- function(n) {
  declare(type(n = double()))
  total <- 0
  while (total < n) {
    iteration <- 1
    total <- total + iteration
  }
  total
}
uninitialized_recurrence_result <- tccq_analyze(uninitialized_recurrence)
expect_false(uninitialized_recurrence_result@success)
expect_true(any(vapply(
  uninitialized_recurrence_result@diagnostics,
  function(diagnostic) identical(diagnostic@code, "lowering.uninitialized_loop_cell"),
  logical(1)
)))

explicit_replacement <- function(x) {
  declare(type(x = double(n)))
  `[<-`(x, 1L, value = 2)
}

explicit_replacement_result <- tccq_analyze(explicit_replacement)
expect_false(explicit_replacement_result@success)
expect_false(explicit_replacement_result@value@attrs$lowered)
expect_true(any(vapply(
  explicit_replacement_result@diagnostics,
  function(x) identical(x@code, "lowering.replacement_boundary"),
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
custom_reduction_operation <- custom_reduction_value@attrs$operation
expect_equal(custom_reduction_value@op, "fold_add")
expect_equal(custom_reduction_operation@reduction@name, "fold_add")
expect_true(S7::S7_inherits(custom_reduction_operation, TccqLoweredOperation))
expect_true(S7::S7_inherits(custom_reduction_operation@reduction, TccqReductionSpec))

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
expect_equal(replacement_kinds[["[<-"]], "replacement")
synthetic_replacement <- replacement_calls[[match("[<-", replacement_names)]]
expect_equal(synthetic_replacement@origin, "assignment_rewrite")
expect_equal(synthetic_replacement@arity, 3L)
expect_equal(synthetic_replacement@argument_names, c("", "", "value"))
expect_equal(synthetic_replacement@attrs$target_call, "[")
expect_equal(synthetic_replacement@attrs$target_symbol, "x")
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

empty_index_probe <- function(x) {
  declare(type(x = double(n, p)))
  Reduce(`+`, lapply(seq_len(p), function(j) sum(x[, j])))
}
empty_index_result <- tccq_analyze(empty_index_probe)
expect_false(empty_index_result@success)
expect_true(any(vapply(
  empty_index_result@diagnostics,
  function(x) identical(x@code, "frontend.unimplemented_call"),
  logical(1)
)))
