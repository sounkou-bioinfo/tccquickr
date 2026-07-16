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
lowered_add <- tccq_lowered_operation(
  "elementwise",
  resolved_add,
  elementwise = resolved_add@elementwise
)
reference_expression <- tccq_expression(
  "formal_0001",
  "reference",
  type = finite@type,
  value_id = "formal_0001",
  op = "formal",
  reference = tccq_expression_reference("formal_0001", symbol = "x")
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
  effect = resolved_add@effect,
  operation = lowered_add
)
vector_expression <- tccq_expression(
  "formal_vector",
  "reference",
  type = tccq_type("double", tccq_shape("n")),
  op = "formal",
  reference = tccq_expression_reference("formal_vector", symbol = "x")
)
element_expression <- tccq_expression(
  "formal_vector_element",
  "element",
  type = tccq_type("double"),
  inputs = list(vector_expression)
)
fold_spec <- tccq_reduction_spec(
  "fold_probe",
  identity = function(result_type) tccq_literal_finite(0, type = result_type),
  combine_op = "+"
)
fold_plan <- tccq_reduction_plan(
  fold_spec,
  element_expression,
  list(tccq_loop_axis("axis_0001", tccq_dim_symbol("n"), role = "reduce")),
  tccq_type("double"),
  "reduction_probe",
  tccq_default_op_registry()
)
logical_scalar <- tccq_type("logical")
combined_effect <- tccq_effect_union(
  tccq_effect(reads = TRUE, may_warn = TRUE),
  tccq_effect(writes = TRUE, may_error = TRUE)
)
expect_true(combined_effect@reads)
expect_true(combined_effect@writes)
expect_true(combined_effect@may_error)
expect_true(combined_effect@may_warn)
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
  op = "formal",
  reference = tccq_expression_reference("formal_flag", symbol = "flag")
)
branch_expression <- tccq_expression(
  "value_0003",
  "branch",
  type = finite@type,
  inputs = list(condition_expression, reference_expression, literal_expression),
  branch = branch_value
)

expect_true(S7::S7_inherits(reference_expression, TccqExpression))
expect_true(S7::S7_inherits(reference_expression@reference, TccqExpressionReference))
expect_equal(reference_expression@reference@source_value_id, "formal_0001")
expect_equal(reference_expression@reference@symbol, "x")
expect_true(S7::S7_inherits(operation_expression@operation, TccqLoweredOperation))
expect_identical(operation_expression@operation@resolved_op, resolved_add)
expect_identical(operation_expression@effect, resolved_add@effect)
expect_equal(operation_expression@inputs[[2L]]@literal@value, 1.5)
expect_equal(element_expression@kind, "element")
expect_equal(element_expression@type@shape@rank, 0L)
expect_identical(element_expression@inputs[[1L]], vector_expression)
expect_true(S7::S7_inherits(fold_plan, TccqReductionPlan))
expect_identical(fold_plan@spec, fold_spec)
expect_equal(fold_plan@state@components[[1L]]@name, "accumulator")
expect_equal(fold_plan@updates[[1L]]@value@op, "+")
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
  tccq_value_block(
    "block_consequent",
    result = result_target,
    statements = list(consequent_assignment)
  ),
  tccq_value_block(
    "block_alternative",
    result = result_target,
    statements = list(alternative_assignment)
  ),
  branch_value
)
statement_block <- tccq_value_block(
  "block_if",
  result = result_target,
  statements = list(conditional_statement)
)

expect_true(S7::S7_inherits(result_target, TccqWriteTarget))
expect_equal(result_target@storage_type@shape@rank, 0L)
expect_true(S7::S7_inherits(consequent_assignment, TccqStatement))
expect_true(S7::S7_inherits(consequent_assignment, TccqAssignment))
expect_true(S7::S7_inherits(conditional_statement, TccqIf))
expect_true(S7::S7_inherits(conditional_statement, TccqConditional))
expect_true(S7::S7_inherits(statement_block, TccqBlock))
expect_true(S7::S7_inherits(statement_block, TccqValueBlock))
expect_identical(statement_block@result, result_target)
expect_identical(statement_block@effect, conditional_statement@effect)

procedural_if <- tccq_if(
  "statement_procedural_if",
  condition_expression,
  TccqBlock(
    id = "block_procedural_consequent",
    locals = list(),
    statements = list(consequent_assignment),
    effect = consequent_assignment@effect
  ),
  TccqBlock(
    id = "block_procedural_alternative",
    locals = list(),
    statements = list(alternative_assignment),
    effect = alternative_assignment@effect
  ),
  branch_value@semantics
)
expect_true(S7::S7_inherits(procedural_if, TccqIf))
expect_false(S7::S7_inherits(procedural_if, TccqConditional))
expect_true(procedural_if@effect@may_error)
expect_identical(procedural_if@semantics, branch_value@semantics)

procedural_block <- TccqBlock(
  id = "block_procedural",
  locals = list(),
  statements = list(),
  effect = tccq_effect()
)
expect_true(S7::S7_inherits(procedural_block, TccqBlock))
expect_false(S7::S7_inherits(procedural_block, TccqValueBlock))
expect_true(S7::S7_inherits(procedural_block, TccqProgramBody))

counter_cell <- tccq_cell("counter", "cell_counter", finite@type)
counter_reference <- tccq_cell_reference("value_counter_read", counter_cell)
counter_expression <- tccq_expression(
  "value_counter_read",
  "reference",
  type = counter_cell@type,
  op = "cell",
  effect = counter_reference@effect,
  reference = tccq_expression_reference(
    counter_cell@value_id,
    binding = counter_cell
  )
)
counter_target <- tccq_write_target(
  counter_cell@value_id,
  counter_cell@type,
  kind = "cell",
  binding = counter_cell
)
counter_assignment <- tccq_assignment(
  "statement_counter",
  counter_target,
  counter_expression
)
counter_body <- TccqBlock(
  id = "block_counter",
  locals = list(),
  statements = list(counter_assignment),
  effect = counter_assignment@effect
)
while_semantics <- tccq_call_semantics(tccq_call(
  "while",
  expr = quote(while (flag) counter <- counter)
))
while_statement <- tccq_while(
  "statement_while",
  condition_expression,
  counter_body,
  while_semantics
)

expect_true(S7::S7_inherits(counter_cell, TccqBinding))
expect_false(S7::S7_inherits(counter_cell, TccqLocalBinding))
expect_true(counter_cell@mutable)
expect_identical(counter_reference@cell, counter_cell)
expect_identical(counter_target@binding, counter_cell)
expect_true(counter_assignment@effect@writes)
expect_true(S7::S7_inherits(while_statement, TccqLoop))
expect_true(S7::S7_inherits(while_statement, TccqWhile))
expect_true(while_statement@effect@reads)
expect_true(while_statement@effect@writes)
expect_true(while_statement@effect@may_error)
expect_equal(while_statement@semantics@forcing_policy, "special")

break_semantics <- tccq_call_semantics(tccq_call(
  "break",
  expr = quote(break)
))
next_semantics <- tccq_call_semantics(tccq_call(
  "next",
  expr = quote(next)
))
break_statement <- tccq_loop_transfer(
  "statement_break",
  "break",
  break_semantics
)
next_statement <- tccq_loop_transfer(
  "statement_next",
  "next",
  next_semantics
)
repeat_body <- TccqBlock(
  id = "block_repeat",
  locals = list(),
  statements = list(break_statement),
  effect = break_statement@effect
)
repeat_statement <- tccq_repeat(
  "statement_repeat",
  repeat_body,
  tccq_call_semantics(tccq_call(
    "repeat",
    expr = quote(repeat break)
  ))
)
expect_true(S7::S7_inherits(repeat_statement, TccqLoop))
expect_true(S7::S7_inherits(repeat_statement, TccqRepeat))
expect_true(S7::S7_inherits(break_statement, TccqLoopTransfer))
expect_equal(break_statement@action, "break")
expect_equal(next_statement@action, "next")
expect_identical(repeat_statement@effect, repeat_body@effect)
expect_identical(break_statement@effect, tccq_effect())

switch_selector <- tccq_expression(
  "value_switch_selector",
  "literal",
  type = tccq_type("integer"),
  literal = tccq_literal_finite(2L)
)
switch_statement <- tccq_switch(
  "statement_switch",
  switch_selector,
  tccq_write_target(
    "statement_switch.selector",
    switch_selector@type,
    switch_selector@type
  ),
  list(TccqBlock(
    id = "block_switch_break",
    locals = list(),
    statements = list(break_statement),
    effect = break_statement@effect
  ), TccqBlock(
    id = "block_switch_next",
    locals = list(),
    statements = list(next_statement),
    effect = next_statement@effect
  )),
  tccq_call_semantics(tccq_call(
    "switch",
    expr = quote(switch(2L, break, next))
  ))
)
switch_completion <- tccq_completion(switch_statement)
expect_true(S7::S7_inherits(switch_statement, TccqSwitch))
expect_equal(switch_statement@selector_target@value_id, "statement_switch.selector")
expect_equal(switch_statement@selector_target@kind, "local")
expect_equal(length(switch_statement@alternatives), 2L)
expect_true(switch_completion@falls_through)
expect_true(switch_completion@breaks)
expect_true(switch_completion@continues)
invalid_switch_selector <- tryCatch(
  tccq_switch(
    "statement_invalid_switch",
    tccq_expression(
      "value_double_switch_selector",
      "literal",
      type = tccq_type("double"),
      literal = tccq_literal_finite(1)
    ),
    tccq_write_target(
      "statement_invalid_switch.selector",
      tccq_type("double"),
      tccq_type("double")
    ),
    list(switch_statement@alternatives[[1L]]),
    switch_statement@semantics
  ),
  error = identity
)
expect_true(inherits(invalid_switch_selector, "error"))

break_block <- TccqBlock(
  id = "block_break_completion",
  locals = list(),
  statements = list(break_statement),
  effect = break_statement@effect
)
next_block <- TccqBlock(
  id = "block_next_completion",
  locals = list(),
  statements = list(next_statement),
  effect = next_statement@effect
)
transfer_if <- tccq_if(
  "statement_transfer_if",
  condition_expression,
  break_block,
  next_block,
  branch_value@semantics
)
assignment_completion <- tccq_completion(counter_assignment)
break_completion <- tccq_completion(break_statement)
next_completion <- tccq_completion(next_statement)
transfer_completion <- tccq_completion(transfer_if)
repeat_completion <- tccq_completion(repeat_statement)
unreachable_assignment_completion <- tccq_completion(TccqBlock(
  id = "block_unreachable_assignment",
  locals = list(),
  statements = list(break_statement, counter_assignment),
  effect = counter_assignment@effect
))
nested_loop_completion <- tccq_completion(TccqBlock(
  id = "block_nested_loop_completion",
  locals = list(),
  statements = list(repeat_statement, counter_assignment),
  effect = counter_assignment@effect
))

expect_true(S7::S7_inherits(assignment_completion, TccqControlCompletion))
expect_true(assignment_completion@falls_through)
expect_false(assignment_completion@breaks)
expect_false(break_completion@falls_through)
expect_true(break_completion@breaks)
expect_true(next_completion@continues)
expect_false(transfer_completion@falls_through)
expect_true(transfer_completion@breaks)
expect_true(transfer_completion@continues)
expect_true(repeat_completion@falls_through)
expect_false(repeat_completion@breaks)
expect_false(unreachable_assignment_completion@falls_through)
expect_true(unreachable_assignment_completion@breaks)
expect_true(nested_loop_completion@falls_through)
expect_false(nested_loop_completion@breaks)

for_type <- tccq_type("double", tccq_shape(tccq_dim_symbol("n")))
for_domain <- tccq_domain(
  "domain_for",
  for_type@shape,
  axes = "for_axis",
  attrs = list(kind = "sequential_for")
)
for_source <- tccq_expression(
  "formal_for",
  "reference",
  type = for_type,
  op = "formal",
  effect = tccq_effect(reads = TRUE),
  reference = tccq_expression_reference(
    "formal_for",
    symbol = "x"
  )
)
for_element <- tccq_expression(
  "formal_for",
  "reference",
  type = for_type,
  op = "formal",
  effect = tccq_effect(reads = TRUE),
  reference = tccq_expression_reference(
    "formal_for",
    symbol = "x",
    access = tccq_access(
      "formal_for",
      for_domain,
      index_map = list(tccq_index_expr("for_axis"))
    )
  )
)
for_iteration <- tccq_iteration_plan(
  for_source,
  for_domain,
  for_element,
  tccq_type("double")
)
for_iterator <- tccq_cell("element", "cell_element", tccq_type("double"))
for_iterator_target <- tccq_write_target(
  for_iterator@value_id,
  for_iterator@type,
  kind = "cell",
  binding = for_iterator
)
for_statement <- tccq_for(
  "statement_for",
  for_iterator_target,
  for_iteration,
  repeat_body,
  tccq_call_semantics(tccq_call(
    "for",
    expr = quote(for (element in x) break)
  ))
)
expect_true(S7::S7_inherits(for_statement, TccqLoop))
expect_true(S7::S7_inherits(for_statement, TccqFor))
expect_true(S7::S7_inherits(for_statement@iteration, TccqIterationPlan))
expect_identical(for_statement@iteration@domain, for_domain)
expect_identical(for_statement@iterator@binding, for_iterator)
expect_true(for_statement@effect@reads)
expect_true(for_statement@effect@writes)
expect_true(tccq_completion(for_statement)@falls_through)

mismatched_for_domain <- tryCatch(
  tccq_iteration_plan(
    for_source,
    tccq_domain(
      "other_domain_for",
      for_type@shape,
      axes = "other_for_axis",
      attrs = list(kind = "sequential_for")
    ),
    for_element,
    tccq_type("double")
  ),
  error = identity
)
expect_true(inherits(mismatched_for_domain, "error"))

counter_initialization <- tccq_assignment(
  "statement_counter_initialization",
  counter_target,
  literal_expression
)
counter_result_target <- tccq_write_target(
  counter_reference@id,
  counter_reference@type,
  kind = "result"
)
counter_result_assignment <- tccq_assignment(
  "statement_counter_result",
  counter_result_target,
  counter_expression
)
counter_program_body <- tccq_value_block(
  "block_counter_program",
  result = counter_result_target,
  locals = list(counter_target),
  statements = list(
    counter_initialization,
    while_statement,
    repeat_statement,
    counter_result_assignment
  )
)
counter_program_values <- list(
  tccq_value(
    literal_expression@value_id,
    "literal",
    type = literal_expression@type,
    attrs = list(literal = literal_expression@literal)
  ),
  tccq_value(
    condition_expression@value_id,
    "formal",
    type = condition_expression@type,
    attrs = list(symbol = "flag")
  ),
  counter_reference
)
counter_program_schedule <- tccq_program_schedule(
  steps = list(),
  result = counter_reference@id,
  values = counter_program_values,
  body = counter_program_body
)
expect_equal(length(counter_program_schedule@steps), 0L)
expect_identical(counter_program_schedule@body, counter_program_body)

transfer_outside_loop_body <- tccq_value_block(
  "block_transfer_outside_loop",
  result = counter_result_target,
  locals = list(counter_target),
  statements = list(
    counter_initialization,
    next_statement,
    counter_result_assignment
  )
)
transfer_outside_loop <- tryCatch(
  tccq_program_schedule(
    steps = list(),
    result = counter_reference@id,
    values = counter_program_values,
    body = transfer_outside_loop_body
  ),
  error = identity
)
expect_true(inherits(transfer_outside_loop, "schema.loop_transfer_outside_loop"))

mismatched_body_result <- tryCatch(
  tccq_program_schedule(
    steps = list(),
    result = literal_expression@value_id,
    values = counter_program_values,
    body = counter_program_body
  ),
  error = identity
)
expect_true(inherits(mismatched_body_result, "schema.program_body_result_mismatch"))

mismatched_result_type <- tryCatch(
  tccq_program_schedule(
    steps = list(),
    result = counter_reference@id,
    values = c(
      counter_program_values[-length(counter_program_values)],
      list(tccq_value(
        counter_reference@id,
        "formal",
        type = logical_scalar,
        attrs = list(symbol = "counter")
      ))
    ),
    body = counter_program_body
  ),
  error = identity
)
expect_true(inherits(mismatched_result_type, "schema.program_target_type_mismatch"))

undeclared_local_target <- tccq_write_target(
  literal_expression@value_id,
  literal_expression@type,
  kind = "local"
)
undeclared_local_assignment <- tccq_assignment(
  "statement_undeclared_local",
  undeclared_local_target,
  literal_expression
)
undeclared_local_body <- tccq_value_block(
  "block_undeclared_local",
  result = counter_result_target,
  locals = list(counter_target),
  statements = list(
    counter_initialization,
    undeclared_local_assignment,
    counter_result_assignment
  )
)
undeclared_local_schedule <- tryCatch(
  tccq_program_schedule(
    steps = list(),
    result = counter_reference@id,
    values = counter_program_values,
    body = undeclared_local_body
  ),
  error = identity
)
expect_true(inherits(undeclared_local_schedule, "schema.unowned_program_local"))

uninitialized_counter_body <- tccq_value_block(
  "block_uninitialized_counter",
  result = counter_result_target,
  locals = list(counter_target),
  statements = list(while_statement, counter_result_assignment)
)
uninitialized_counter_schedule <- tryCatch(
  tccq_program_schedule(
    steps = list(),
    result = counter_reference@id,
    values = counter_program_values,
    body = uninitialized_counter_body
  ),
  error = identity
)
expect_true(inherits(
  uninitialized_counter_schedule,
  "schema.program_cell_use_before_definition"
))

ambiguous_counter_schedule <- tryCatch(
  tccq_program_schedule(
    steps = list(local_step),
    result = counter_reference@id,
    values = c(counter_program_values, list(value)),
    body = counter_program_body
  ),
  error = identity
)
expect_true(inherits(ambiguous_counter_schedule, "schema.ambiguous_program_schedule"))

unbound_cell_target <- tryCatch(
  tccq_write_target("cell_unbound", finite@type, kind = "cell"),
  error = identity
)
expect_true(inherits(unbound_cell_target, "error"))

nonscalar_cell <- tryCatch(
  tccq_cell("matrix", "cell_matrix", matrix_type),
  error = identity
)
expect_true(inherits(nonscalar_cell, "error"))

incorrect_assignment_effect <- tryCatch(
  TccqAssignment(
    id = "statement_incorrect_effect",
    effect = tccq_effect(reads = TRUE),
    target = result_target,
    value = reference_expression
  ),
  error = identity
)
expect_true(inherits(incorrect_assignment_effect, "error"))

incorrect_conditional_effect <- tryCatch(
  TccqConditional(
    id = "conditional_incorrect_effect",
    effect = tccq_effect(reads = TRUE),
    condition = condition_expression,
    consequent = conditional_statement@consequent,
    alternative = conditional_statement@alternative,
    branch = branch_value
  ),
  error = identity
)
expect_true(inherits(incorrect_conditional_effect, "error"))

duplicate_statement_block <- tryCatch(
  tccq_value_block(
    "block_duplicate_statement",
    result = result_target,
    statements = list(consequent_assignment, consequent_assignment)
  ),
  error = identity
)
expect_true(inherits(duplicate_statement_block, "error"))

incorrect_block_effect <- tryCatch(
  TccqBlock(
    id = "block_incorrect_effect",
    locals = list(),
    statements = list(consequent_assignment),
    effect = tccq_effect(reads = TRUE)
  ),
  error = identity
)
expect_true(inherits(incorrect_block_effect, "error"))

other_result_target <- tccq_write_target("value_other", finite@type, kind = "result")
bad_block_result <- tryCatch(
  tccq_value_block(
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
    tccq_value_block(
      "block_bad_consequent",
      result = result_target,
      statements = list(consequent_assignment)
    ),
    tccq_value_block(
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
statement_local_binding <- tccq_backend_value_binding(
  "local_0001",
  "formal_flag",
  logical_scalar,
  role = "local"
)
statement_interface <- tccq_backend_function_interface(
  symbol = "statement_kernel",
  source_language = "c",
  kind = "scalar",
  locals = list(statement_local_binding),
  result_value_id = "value_0003",
  result_type = finite@type,
  result_name = "result_0001"
)

expect_true(S7::S7_inherits(statement_products@body, TccqValueBlock))
expect_true(S7::S7_inherits(statement_interface@locals[[1L]], TccqBackendValueBinding))
expect_equal(statement_interface@locals[[1L]]@source_type@base, "logical")
expect_true(S7::S7_inherits(loop_guard, TccqLoopGuard))
expect_true(loop_guard@selected)
expect_identical(guarded_statement_nest@guards[[1L]], loop_guard)
expect_identical(statement_nest@storage, statement_storage)
expect_null(statement_nest@reduction)

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
    parameters = list(tccq_backend_value_binding(
      "local_0001",
      "formal_other",
      logical_scalar,
      role = "parameter"
    )),
    locals = list(statement_local_binding),
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

bad_element_expression <- tryCatch(
  tccq_expression(
    "bad_element_expression",
    "element",
    type = vector_expression@type,
    inputs = list(vector_expression)
  ),
  error = identity
)
expect_true(inherits(bad_element_expression, "error"))

bad_reduction_update <- tccq_assignment(
  "bad_reduction_update",
  tccq_write_target("other_accumulator", tccq_type("double"), kind = "local"),
  fold_plan@updates[[1L]]@value
)
bad_reduction_plan <- tryCatch(
  TccqReductionPlan(
    spec = fold_plan@spec,
    state = fold_plan@state,
    condition = fold_plan@condition,
    updates = list(bad_reduction_update),
    value = fold_plan@value,
    valid = fold_plan@valid
  ),
  error = identity
)
expect_true(inherits(bad_reduction_plan, "error"))

mismatched_expression_operation <- tryCatch(
  tccq_expression(
    "mismatched_expression_operation",
    "operation",
    type = finite@type,
    op = "-",
    inputs = list(reference_expression, literal_expression),
    operation = lowered_add
  ),
  error = identity
)
expect_true(inherits(mismatched_expression_operation, "error"))

missing_reference_payload <- tryCatch(
  tccq_expression(
    "missing_reference_payload",
    "reference",
    type = finite@type,
    op = "formal"
  ),
  error = identity
)
expect_true(inherits(missing_reference_payload, "error"))

bad_slice_reference <- tryCatch(
  tccq_expression(
    "bad_slice_reference",
    "reference",
    type = finite@type,
    op = "formal",
    reference = tccq_expression_reference(
      "formal_0001",
      slice_offsets = c(0L, 1L)
    )
  ),
  error = identity
)
expect_true(inherits(bad_slice_reference, "error"))

domain <- tccq_domain("d_matrix", matrix_type@shape, axes = c("i", "j"))
access <- tccq_access("m1", domain)
recycle_access <- tccq_access(
  "m1",
  domain,
  kind = "recycle",
  index_map = list(tccq_index_expr("i"), tccq_index_expr("j")),
  consumer_shape = matrix_type@shape
)
missing_recycle_shape <- tryCatch(
  tccq_access(
    "m1",
    domain,
    kind = "recycle",
    index_map = list(tccq_index_expr("i"), tccq_index_expr("j"))
  ),
  error = identity
)
unexpected_consumer_shape <- tryCatch(
  tccq_access("m1", domain, consumer_shape = matrix_type@shape),
  error = identity
)
bad_access_reference <- tryCatch(
  tccq_expression_reference("formal_0001", access = access),
  error = identity
)
expect_true(inherits(bad_access_reference, "error"))
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
expect_true(S7::S7_inherits(recycle_access@consumer_shape, TccqShape))
expect_true(inherits(missing_recycle_shape, "error"))
expect_true(inherits(unexpected_consumer_shape, "error"))
expect_true(S7::S7_inherits(fusion, TccqFusionGroup))
expect_equal(domain@axes, c("i", "j"))
expect_equal(access@kind, "identity")
expect_null(access@consumer_shape)
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
allocation <- tccq_storage_allocation("allocation_0001", finite@type)
storage_slot <- tccq_storage_slot(
  "slot_0001",
  "value_0001",
  finite@type,
  role = "temporary",
  materialized = TRUE,
  lifetime = lifetime,
  allocation = allocation
)

expect_true(S7::S7_inherits(lifetime, TccqStorageLifetime))
expect_true(S7::S7_inherits(allocation, TccqStorageAllocation))
expect_true(S7::S7_inherits(storage_slot, TccqStorageSlot))
expect_equal(storage_slot@lifetime@last_used_at, 4L)
expect_identical(storage_slot@allocation, allocation)

bad_lifetime <- tryCatch(
  tccq_storage_lifetime("value_0001", defined_at = 4L, last_used_at = 2L),
  error = identity
)
expect_true(inherits(bad_lifetime, "tccq_error"))

missing_allocation_slot <- tryCatch(
  tccq_storage_slot(
    "slot_bad",
    "value_0001",
    finite@type,
    role = "temporary",
    materialized = TRUE,
    lifetime = lifetime
  ),
  error = identity
)
expect_true(inherits(missing_allocation_slot, "error"))

unmaterialized_allocation_slot <- tryCatch(
  tccq_storage_slot(
    "slot_unmaterialized",
    "value_0001",
    finite@type,
    role = "temporary",
    materialized = FALSE,
    lifetime = lifetime,
    allocation = allocation
  ),
  error = identity
)
expect_true(inherits(unmaterialized_allocation_slot, "error"))

second_lifetime <- tccq_storage_lifetime("value_0002", defined_at = 5L, last_used_at = 6L)
second_storage_slot <- tccq_storage_slot(
  "slot_0002",
  "value_0002",
  finite@type,
  role = "temporary",
  materialized = TRUE,
  lifetime = second_lifetime,
  allocation = allocation
)
shared_storage_plan <- tccq_storage_plan(list(storage_slot, second_storage_slot))
allocation_binding <- tccq_backend_allocation_binding(
  "intermediate_0001",
  allocation,
  list(storage_slot, second_storage_slot)
)
extent_binding <- tccq_backend_extent_binding("extent_n", "n")
expect_equal(
  vapply(shared_storage_plan@slots, function(slot) slot@allocation@id, character(1)),
  rep("allocation_0001", 2L)
)
expect_true(S7::S7_inherits(allocation_binding, TccqBackendAllocationBinding))
expect_equal(length(allocation_binding@slots), 2L)
expect_true(S7::S7_inherits(extent_binding, TccqBackendExtentBinding))
expect_equal(extent_binding@symbol, "n")

conflicting_allocation <- tccq_storage_allocation(
  "allocation_0001",
  finite@type,
  memory_space = "device"
)
conflicting_allocation_slot <- tccq_storage_slot(
  "slot_conflicting_allocation",
  "value_conflicting_allocation",
  finite@type,
  role = "temporary",
  materialized = TRUE,
  lifetime = tccq_storage_lifetime(
    "value_conflicting_allocation",
    defined_at = 7L,
    last_used_at = 8L
  ),
  allocation = conflicting_allocation
)
conflicting_storage_plan <- tryCatch(
  tccq_storage_plan(list(storage_slot, conflicting_allocation_slot)),
  error = identity
)
expect_true(inherits(conflicting_storage_plan, "error"))
conflicting_allocation_binding <- tryCatch(
  tccq_backend_allocation_binding(
    "intermediate_bad",
    allocation,
    list(storage_slot, conflicting_allocation_slot)
  ),
  error = identity
)
expect_true(inherits(conflicting_allocation_binding, "error"))

overlapping_lifetime <- tccq_storage_lifetime(
  "value_0003",
  defined_at = 4L,
  last_used_at = 5L
)
overlapping_slot <- tccq_storage_slot(
  "slot_0003",
  "value_0003",
  finite@type,
  role = "temporary",
  materialized = TRUE,
  lifetime = overlapping_lifetime,
  allocation = allocation
)
overlapping_storage_plan <- tryCatch(
  tccq_storage_plan(list(storage_slot, overlapping_slot)),
  error = identity
)
expect_true(inherits(overlapping_storage_plan, "error"))

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
