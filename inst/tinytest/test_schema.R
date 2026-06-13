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
matrix_layout <- tccq_layout(2L, order = "column_major", contiguous = TRUE)
matrix_tile <- tccq_tile(tccq_shape(c(32L, 32L)))
matrix_value <- tccq_value(
  "m1",
  "matrix_input",
  type = matrix_type,
  layout = matrix_layout,
  tile = matrix_tile
)

expect_true(S7::S7_inherits(matrix_layout, TccqLayout))
expect_true(S7::S7_inherits(matrix_tile, TccqTile))
expect_equal(matrix_value@type@shape@rank, 2L)
expect_equal(matrix_value@layout@order, "column_major")
expect_equal(matrix_value@tile@shape@rank, 2L)

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

bad_layout <- tryCatch(
  tccq_value(
    "bad_layout",
    "matrix_input",
    type = matrix_type,
    layout = tccq_layout(1L)
  ),
  error = identity
)
expect_true(inherits(bad_layout, "tccq_error"))

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
