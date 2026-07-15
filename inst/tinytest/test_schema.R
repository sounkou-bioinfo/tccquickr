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
