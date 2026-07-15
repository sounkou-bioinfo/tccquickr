library(tinytest)
library(tccquickr)

shape <- tccq_shape(c("n", "p"))
type <- tccq_type("double", shape)

expect_true(S7::S7_inherits(shape, TccqShape))
expect_equal(shape@rank, 2L)
expect_equal(shape@dims[[1L]]@kind, "symbol")
expect_true(S7::S7_inherits(type, TccqType))
expect_equal(type@base, "double")

raw_type <- tccq_type("raw", tccq_shape("n"))
buffer_type <- tccq_type("buffer", tccq_shape("n"))

expect_equal(raw_type@base, "raw")
expect_equal(raw_type@shape@rank, 1L)
expect_equal(buffer_type@base, "buffer")
expect_equal(buffer_type@shape@rank, 1L)

finite <- tccq_literal_finite(1.5)
raw_literal <- tccq_literal_finite(as.raw(42))
na_real <- tccq_literal_na("double")
nan <- tccq_literal_nan()
pos_inf <- tccq_literal_inf()
neg_inf <- tccq_literal_inf(-1L)

expect_true(S7::S7_inherits(finite, TccqLiteral))
expect_equal(finite@kind, "finite")
expect_equal(finite@type@base, "double")
expect_equal(raw_literal@type@base, "raw")
expect_equal(na_real@kind, "na")
expect_equal(nan@kind, "nan")
expect_equal(pos_inf@kind, "pos_inf")
expect_equal(neg_inf@kind, "neg_inf")

bad_na <- tryCatch(
  tccq_literal_na("raw"),
  error = identity
)
expect_true(inherits(bad_na, "tccq_error"))

bad_finite <- tryCatch(
  tccq_literal_finite(Inf),
  error = identity
)
expect_true(inherits(bad_finite, "tccq_error"))

value <- tccq_value(
  "v1",
  "literal",
  type = finite@type,
  attrs = list(literal = finite)
)
local_binding <- tccq_local_binding(
  "answer",
  value@id,
  value@type,
  statement_index = 1L
)
local_step <- tccq_evaluation_step(
  value@id,
  statement_index = 1L,
  effect = value@effect,
  binding = local_binding
)
local_schedule <- tccq_program_schedule(
  steps = list(local_step),
  result = value@id,
  values = list(value)
)
local_program <- tccq_program(
  "local_binding_probe",
  formals = list(),
  schedule = local_schedule,
  values = list(value),
  result = value@id
)

expect_true(S7::S7_inherits(local_binding, TccqLocalBinding))
expect_equal(local_binding@statement_index, 1L)
expect_true(S7::S7_inherits(local_step, TccqEvaluationStep))
expect_true(S7::S7_inherits(local_schedule, TccqProgramSchedule))
expect_identical(local_program@schedule@steps[[1L]]@binding@type, value@type)

unknown_local_value <- tryCatch(
  tccq_program_schedule(
    steps = list(tccq_evaluation_step(
      "missing_value",
      statement_index = 1L,
      effect = value@effect,
      binding = tccq_local_binding(
      "answer",
      "missing_value",
      value@type,
      statement_index = 1L
      )
    )),
    result = "missing_value",
    values = list(value)
  ),
  error = identity
)
expect_true(inherits(unknown_local_value, "error"))

mismatched_local_type <- tryCatch(
  tccq_program_schedule(
    steps = list(tccq_evaluation_step(
      value@id,
      statement_index = 1L,
      effect = value@effect,
      binding = tccq_local_binding(
        "answer",
        value@id,
        tccq_type("integer"),
        statement_index = 1L
      )
    )),
    result = value@id,
    values = list(value)
  ),
  error = identity
)
expect_true(inherits(mismatched_local_type, "error"))

noncontiguous_schedule <- tryCatch(
  tccq_program_schedule(
    steps = list(tccq_evaluation_step(
      value@id,
      statement_index = 2L,
      effect = value@effect
    )),
    result = value@id,
    values = list(value)
  ),
  error = identity
)
expect_true(inherits(noncontiguous_schedule, "error"))

incomplete_schedule_effect <- tryCatch(
  tccq_program_schedule(
    steps = list(tccq_evaluation_step(
      value@id,
      statement_index = 1L,
      effect = tccq_effect(reads = TRUE)
    )),
    result = value@id,
    values = list(value)
  ),
  error = identity
)
expect_true(inherits(incomplete_schedule_effect, "error"))

later_value <- tccq_value(
  "later_value",
  "literal",
  type = value@type,
  effect = tccq_effect()
)
later_binding <- tccq_local_binding(
  "later",
  later_value@id,
  later_value@type,
  statement_index = 2L
)
later_read <- tccq_binding_reference("later_read", later_binding)
early_consumer <- tccq_value(
  "early_consumer",
  "+",
  inputs = list(later_read@id, later_read@id),
  type = value@type,
  effect = tccq_effect(reads = TRUE)
)
use_before_definition <- tryCatch(
  tccq_program_schedule(
    steps = list(
      tccq_evaluation_step(
        early_consumer@id,
        statement_index = 1L,
        effect = early_consumer@effect,
        uses = list(later_binding)
      ),
      tccq_evaluation_step(
        later_value@id,
        statement_index = 2L,
        effect = later_value@effect,
        binding = later_binding
      )
    ),
    result = later_value@id,
    values = list(early_consumer, later_read, later_value)
  ),
  error = identity
)
expect_true(inherits(use_before_definition, "error"))

first_alias <- tccq_local_binding(
  "first_alias",
  value@id,
  value@type,
  statement_index = 1L
)
second_alias <- tccq_local_binding(
  "second_alias",
  value@id,
  value@type,
  statement_index = 2L
)
first_alias_read <- tccq_binding_reference("first_alias_read", first_alias)
second_alias_read <- tccq_binding_reference("second_alias_read", second_alias)
alias_schedule <- tccq_program_schedule(
  steps = list(
    tccq_evaluation_step(
      value@id,
      statement_index = 1L,
      effect = value@effect,
      binding = first_alias
    ),
    tccq_evaluation_step(
      first_alias_read@id,
      statement_index = 2L,
      effect = first_alias_read@effect,
      binding = second_alias,
      uses = list(first_alias)
    ),
    tccq_evaluation_step(
      second_alias_read@id,
      statement_index = 3L,
      effect = second_alias_read@effect,
      uses = list(second_alias)
    )
  ),
  result = second_alias_read@id,
  values = list(value, first_alias_read, second_alias_read)
)
expect_true(S7::S7_inherits(alias_schedule, TccqProgramSchedule))
expect_identical(alias_schedule@steps[[3L]]@uses[[1L]], second_alias)

matrix_type <- tccq_type("double", tccq_shape(c("n", "p")))
matrix_value <- tccq_value(
  "m1",
  "matrix_input",
  type = matrix_type
)

expect_equal(matrix_value@type@shape@rank, 2L)
expect_equal(matrix_value@type@shape@dims[[2L]]@label, "p")

resolved_add <- tccq_resolve_call(
  tccq_default_op_registry(),
  tccq_call("+"),
  tccq_op_context()
)@value
reference_expression <- tccq_expression(
  "formal_0001",
  "reference",
  type = finite@type,
  value_id = "formal_0001",
  op = "formal"
)
literal_expression <- tccq_expression(
  "value_0001",
  "literal",
  type = finite@type,
  value_id = "value_0001",
  op = "literal",
  literal = finite
)
operation_expression <- tccq_expression(
  "value_0002",
  "operation",
  type = finite@type,
  value_id = "value_0002",
  op = "+",
  inputs = list(reference_expression, literal_expression),
  resolved_op = resolved_add
)
logical_scalar <- tccq_type("logical")
combined_effect <- tccq_effect_union(
  tccq_effect(reads = TRUE),
  tccq_effect(writes = TRUE, may_error = TRUE)
)
expect_true(combined_effect@reads)
expect_true(combined_effect@writes)
expect_true(combined_effect@may_error)
expect_false(combined_effect@allocates)
expect_false(combined_effect@boundary)
branch_value <- tccq_branch(
  "value_0003",
  condition = "formal_flag",
  consequent = "formal_0001",
  alternative = "value_0001",
  type = finite@type,
  semantics = tccq_call_semantics(tccq_call(
    "if",
    expr = quote(if (flag) x else 1.5)
  ))
)
condition_expression <- tccq_expression(
  "formal_flag",
  "reference",
  type = logical_scalar,
  op = "formal"
)
branch_expression <- tccq_expression(
  "value_0003",
  "branch",
  type = finite@type,
  inputs = list(condition_expression, reference_expression, literal_expression),
  branch = branch_value
)

expect_true(S7::S7_inherits(reference_expression, TccqExpression))
expect_true(S7::S7_inherits(operation_expression@resolved_op, TccqResolvedOp))
expect_equal(operation_expression@inputs[[2L]]@literal@value, 1.5)
expect_true(S7::S7_inherits(branch_value, TccqBranch))
expect_true(S7::S7_inherits(branch_value, TccqValue))
expect_equal(branch_expression@kind, "branch")
expect_equal(branch_expression@branch@semantics@forcing_policy, "special")

result_target <- tccq_write_target("value_0003", finite@type, kind = "result")
consequent_assignment <- tccq_assignment(
  "statement_consequent",
  result_target,
  reference_expression
)
alternative_assignment <- tccq_assignment(
  "statement_alternative",
  result_target,
  literal_expression
)
conditional_statement <- tccq_conditional(
  "statement_if",
  condition_expression,
  tccq_block(
    "block_consequent",
    result = result_target,
    statements = list(consequent_assignment)
  ),
  tccq_block(
    "block_alternative",
    result = result_target,
    statements = list(alternative_assignment)
  ),
  branch_value
)
statement_block <- tccq_block(
  "block_if",
  result = result_target,
  statements = list(conditional_statement),
  effect = branch_value@effect
)

expect_true(S7::S7_inherits(result_target, TccqWriteTarget))
expect_equal(result_target@storage_type@shape@rank, 0L)
expect_true(S7::S7_inherits(consequent_assignment, TccqStatement))
expect_true(S7::S7_inherits(consequent_assignment, TccqAssignment))
expect_true(S7::S7_inherits(conditional_statement, TccqConditional))
expect_true(S7::S7_inherits(statement_block, TccqBlock))
expect_identical(statement_block@result, result_target)

other_result_target <- tccq_write_target("value_other", finite@type, kind = "result")
bad_block_result <- tryCatch(
  tccq_block(
    "block_bad_result",
    result = other_result_target,
    statements = list(consequent_assignment)
  ),
  error = identity
)
expect_true(inherits(bad_block_result, "error"))

other_result_assignment <- tccq_assignment(
  "statement_other_result",
  other_result_target,
  literal_expression
)
bad_conditional_result <- tryCatch(
  tccq_conditional(
    "statement_bad_result",
    condition_expression,
    tccq_block(
      "block_bad_consequent",
      result = result_target,
      statements = list(consequent_assignment)
    ),
    tccq_block(
      "block_bad_alternative",
      result = other_result_target,
      statements = list(other_result_assignment)
    ),
    branch_value
  ),
  error = identity
)
expect_true(inherits(bad_conditional_result, "error"))

statement_storage <- tccq_storage_slot(
  "slot_statement",
  "value_0003",
  finite@type,
  role = "output",
  materialized = TRUE
)
statement_nest <- tccq_loop_nest(
  "loop_nest_statement",
  axes = list(),
  body = statement_block,
  storage = statement_storage
)
loop_guard <- tccq_loop_guard(condition_expression, branch_value, selected = TRUE)
guarded_statement_nest <- tccq_loop_nest(
  "loop_nest_guarded_statement",
  axes = list(),
  body = statement_block,
  storage = statement_storage,
  guards = list(loop_guard)
)
statement_products <- tccq_backend_products(
  body = statement_block,
  loop_nest = statement_nest,
  loop_nests = list(statement_nest)
)
statement_interface <- tccq_backend_function_interface(
  symbol = "statement_kernel",
  source_language = "c",
  kind = "scalar",
  local_names = "local_0001",
  local_value_ids = "formal_flag",
  local_storage_types = list(logical_scalar),
  result_value_id = "value_0003",
  result_type = finite@type,
  result_name = "result_0001"
)

expect_true(S7::S7_inherits(statement_products@body, TccqBlock))
expect_equal(statement_interface@local_storage_types[[1L]]@base, "logical")
expect_true(S7::S7_inherits(loop_guard, TccqLoopGuard))
expect_true(loop_guard@selected)
expect_identical(guarded_statement_nest@guards[[1L]], loop_guard)
expect_identical(statement_nest@storage, statement_storage)
expect_null(statement_nest@accumulator)

bad_loop_guard <- tryCatch(
  tccq_loop_guard(condition_expression, branch_value, selected = NA),
  error = identity
)
expect_true(inherits(bad_loop_guard, "tccq_error"))

matrix_local_target <- tccq_write_target("matrix_local", matrix_type, kind = "local")
expect_equal(matrix_local_target@type@shape@rank, 2L)
expect_equal(matrix_local_target@storage_type@shape@rank, 0L)

bad_local_storage <- tryCatch(
  tccq_write_target(
    "matrix_local",
    matrix_type,
    storage_type = matrix_type,
    kind = "local"
  ),
  error = identity
)
expect_true(inherits(bad_local_storage, "error"))

bad_statement_products <- tryCatch(
  tccq_backend_products(body = reference_expression, loop_nest = statement_nest),
  error = identity
)
expect_true(inherits(bad_statement_products, "error"))

bad_statement_interface <- tryCatch(
  tccq_backend_function_interface(
    symbol = "bad_statement_kernel",
    source_language = "c",
    kind = "scalar",
    local_names = "local_0001",
    local_value_ids = character(),
    local_storage_types = list(logical_scalar),
    result_value_id = "value_0003",
    result_type = finite@type,
    result_name = "result_0001"
  ),
  error = identity
)
expect_true(inherits(bad_statement_interface, "error"))

bad_assignment <- tryCatch(
  tccq_assignment(
    "statement_bad_type",
    tccq_write_target("logical_local", logical_scalar),
    literal_expression
  ),
  error = identity
)
expect_true(inherits(bad_assignment, "error"))

bad_expression <- tryCatch(
  tccq_expression(
    "bad_expression",
    "operation",
    type = finite@type,
    op = "+",
    inputs = list(reference_expression)
  ),
  error = identity
)
expect_true(inherits(bad_expression, "error"))

domain <- tccq_domain("d_matrix", matrix_type@shape, axes = c("i", "j"))
access <- tccq_access("m1", domain)
fusion <- tccq_fusion_group(
  "fg1",
  "map",
  domain,
  values = list(matrix_value),
  outputs = "m1",
  accesses = list(access),
  region_kind = "parallel",
  target = "pure_c"
)

expect_true(S7::S7_inherits(domain, TccqDomain))
expect_true(S7::S7_inherits(access, TccqAccess))
expect_true(S7::S7_inherits(fusion, TccqFusionGroup))
expect_equal(domain@axes, c("i", "j"))
expect_equal(access@kind, "identity")
expect_equal(fusion@kind, "map")
expect_equal(fusion@region_kind, "parallel")

region <- tccq_region(
  "r1",
  "parallel",
  values = list(value, matrix_value),
  fusion_groups = list(fusion),
  effect = tccq_effect(reads = TRUE),
  memory_space = "host",
  touches_rapi = FALSE
)

expect_true(S7::S7_inherits(region, TccqRegion))
expect_equal(region@kind, "parallel")
expect_false(region@touches_rapi)
expect_equal(length(region@fusion_groups), 1L)

lifetime <- tccq_storage_lifetime("value_0001", defined_at = 2L, last_used_at = 4L)
storage_slot <- tccq_storage_slot(
  "slot_0001",
  "value_0001",
  finite@type,
  role = "temporary",
  materialized = FALSE,
  reusable = TRUE,
  lifetime = lifetime
)

expect_true(S7::S7_inherits(lifetime, TccqStorageLifetime))
expect_true(S7::S7_inherits(storage_slot, TccqStorageSlot))
expect_equal(storage_slot@lifetime@last_used_at, 4L)

bad_lifetime <- tryCatch(
  tccq_storage_lifetime("value_0001", defined_at = 4L, last_used_at = 2L),
  error = identity
)
expect_true(inherits(bad_lifetime, "tccq_error"))

bad_reusable_slot <- tryCatch(
  tccq_storage_slot(
    "slot_bad",
    "value_0001",
    finite@type,
    role = "temporary",
    materialized = FALSE,
    reusable = TRUE
  ),
  error = identity
)
expect_true(inherits(bad_reusable_slot, "tccq_error"))

bad_fusion <- tryCatch(
  tccq_fusion_group(
    "bad_fusion",
    "map",
    domain,
    values = list(matrix_value),
    region_kind = "parallel",
    effect = tccq_effect(boundary = TRUE)
  ),
  error = identity
)
expect_true(inherits(bad_fusion, "tccq_error"))

bad_region <- tryCatch(
  tccq_region(
    "bad",
    "device",
    effect = tccq_effect(boundary = TRUE),
    memory_space = "device"
  ),
  error = identity
)
expect_true(inherits(bad_region, "tccq_error"))

err <- tryCatch(
  tccq_type("data.frame"),
  error = identity
)
expect_true(inherits(err, "tccq_error"))
expect_true(S7::S7_inherits(tccq_condition_diagnostic(err), TccqDiagnostic))
