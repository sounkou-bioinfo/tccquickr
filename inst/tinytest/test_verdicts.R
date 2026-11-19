# A diagnostic's verdict states what a rejection says about the R program:
# refuted (R fails too), unproven (declared facts do not prove a needed
# relation), unsupported (valid R outside the subset), or NA (no claim).

library(tccquickr)

first_diagnostic <- function(fn) {
  result <- tccq_analyze(fn, strict = FALSE)
  expect_false(result@success)
  result@diagnostics[[1L]]
}

n <- tccq_dim_symbol("n")
p <- tccq_dim_symbol("p")

# Dimension relations decide equality only where every binding agrees.
expect_identical(tccq_dim_relation(tccq_dim_constant(3L), tccq_dim_constant(3L)), "equal")
expect_identical(tccq_dim_relation(tccq_dim_constant(3L), tccq_dim_constant(4L)), "distinct")
expect_identical(tccq_dim_relation(n, n), "equal")
expect_identical(tccq_dim_relation(n, tccq_dim_affine("n", -2L)), "distinct")
expect_identical(tccq_dim_relation(n, p), "unknown")
expect_identical(tccq_dim_relation(n, tccq_dim_constant(3L)), "unknown")
expect_identical(tccq_dim_relation(n, tccq_dim_unknown()), "unknown")
expect_false(tccq_dim_equal(n, p))
expect_true(tccq_dim_equal(tccq_dim_affine("n", 1L), tccq_dim_affine("n", 1L)))

# The default verdict makes no claim, and only listed verdicts validate.
expect_true(is.na(tccq_diagnostic("schema.example", "message", path = "example")@verdict))
expect_identical(
  tccq_diagnostic("schema.example", "message", path = "example", verdict = "unproven")@verdict,
  "unproven"
)
expect_error(tccq_diagnostic("schema.example", "message", path = "example", verdict = "wrong"))

# Contractions: distinct constants refute, unrelated symbols are unproven and
# name the equality that would admit the program.
refuted_matvec <- first_diagnostic(function(x, w) {
  declare(type(x = double(3, 4), w = double(5)))
  x %*% w
})
expect_identical(refuted_matvec@code, "ops.incompatible_contraction_dims")
expect_identical(refuted_matvec@verdict, "refuted")
expect_identical(refuted_matvec@data$requires, character())

unproven_matvec <- first_diagnostic(function(x, w) {
  declare(type(x = double(n, p), w = double(q)))
  x %*% w
})
expect_identical(unproven_matvec@verdict, "unproven")
expect_identical(unproven_matvec@data$requires, "p == q")

# Elementwise arrays: same rule, with each undecided equality named once.
refuted_arrays <- first_diagnostic(function(x, y) {
  declare(type(x = double(2, 3), y = double(3, 2)))
  x + y
})
expect_identical(refuted_arrays@code, "ops.incompatible_elementwise_shapes")
expect_identical(refuted_arrays@verdict, "refuted")

transposed_arrays <- first_diagnostic(function(x, y) {
  declare(type(x = double(n, p), y = double(p, n)))
  x + y
})
expect_identical(transposed_arrays@verdict, "unproven")
expect_identical(transposed_arrays@data$requires, "n == p")

# Each operation is judged on its own operands: the first `+` below is only
# unproven, while the second adds two arrays that cannot conform.
expect_identical(first_diagnostic(function(x, y, z) {
  declare(type(x = double(n, p), y = double(2, 2), z = double(2, 3)))
  x + y + z
})@verdict, "unproven")
expect_identical(first_diagnostic(function(x, y, z) {
  declare(type(x = double(n, p), y = double(2, 2), z = double(2, 3)))
  y + z + x
})@verdict, "refuted")

# Vector recycling is R semantics, so a length mismatch never refutes.
expect_identical(first_diagnostic(function(x, y) {
  declare(type(x = double(3), y = double(2)))
  x + y
})@verdict, "unsupported")
expect_identical(first_diagnostic(function(x, y) {
  declare(type(x = double(n), y = double(p)))
  x + y
})@verdict, "unproven")

# Typed effects narrow a declared effect and never widen it.
narrowing_spec <- tccq_elementwise_spec(
  "narrowing",
  2L,
  function(input_types, result_shape) tccq_type("double", result_shape),
  effect = function(input_types, result_type) tccq_effect(reads = TRUE)
)
widening_spec <- tccq_elementwise_spec(
  "widening",
  2L,
  function(input_types, result_shape) tccq_type("double", result_shape),
  effect = function(input_types, result_type) tccq_effect(reads = TRUE, may_error = TRUE)
)
expect_error(tccq_elementwise_spec(
  "invalid",
  2L,
  function(input_types, result_shape) tccq_type("double", result_shape),
  effect = "reads"
))
effect_registry <- tccq_op_registry_add(
  tccq_op_registry_add(
    tccq_default_op_registry(),
    tccq_op_impl(
      "narrowing",
      target = "neutral",
      region_kind = "kernel",
      effect = tccq_effect(reads = TRUE, may_warn = TRUE),
      elementwise = narrowing_spec
    )
  ),
  tccq_op_impl(
    "widening",
    target = "neutral",
    region_kind = "kernel",
    effect = tccq_effect(reads = TRUE),
    elementwise = widening_spec
  )
)
narrowed <- tccq_analyze(function(x, y) {
  declare(type(x = double(n), y = double(n)))
  narrowing(x, y)
}, registry = effect_registry)
expect_true(narrowed@success)
expect_identical(narrowed@value@values[[narrowed@value@result]]@effect, tccq_effect(reads = TRUE))
widened <- tccq_analyze(function(x, y) {
  declare(type(x = double(n), y = double(n)))
  widening(x, y)
}, registry = effect_registry, strict = FALSE)
expect_false(widened@success)
expect_identical(widened@diagnostics[[1L]]@code, "ops.elementwise_effect_widened")
expect_identical(widened@diagnostics[[1L]]@data$widened, "may_error")

# Valid R the subset does not model.
expect_identical(first_diagnostic(function(x) {
  declare(type(x = double(n)))
  paste(x)
})@verdict, "unsupported")
expect_identical(first_diagnostic(function(x, y) {
  declare(type(x = double(n)))
  x
})@verdict, "unsupported")
raw_compile <- tccq_compile(function(x) {
  declare(type(x = raw(n)))
  x
}, strict = FALSE)
expect_false(raw_compile@success)
expect_identical(raw_compile@diagnostics[[1L]]@code, "backend.unsupported_type")
expect_identical(raw_compile@diagnostics[[1L]]@verdict, "unsupported")

# A math function that can warn is typed by analysis and has no kernel: the
# checker accepts the program and the backend reports it as unsupported.
log_ratio <- tccq_analyze(function(x, y) {
  declare(type(x = double(n, p), y = double(n, p)))
  log(x) - log(y)
})
expect_true(log_ratio@success)
log_ratio_result <- log_ratio@value@values[[log_ratio@value@result]]
expect_identical(log_ratio_result@type@base, "double")
expect_identical(log_ratio_result@type@shape@rank, 2L)
log_ratio_ops <- vapply(log_ratio@value@values, function(value) value@op, character(1))
expect_true(all(vapply(
  log_ratio@value@values[log_ratio_ops == "log"],
  function(value) value@effect@may_warn,
  logical(1)
)))
expect_identical(sum(log_ratio_ops == "log"), 2L)
log_ratio_plan <- tccq_plan_backend(
  log_ratio@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
expect_false(log_ratio_plan@success)
expect_identical(log_ratio_plan@diagnostics[[1L]]@code, "ops.unrenderable_operation")
expect_identical(log_ratio_plan@diagnostics[[1L]]@verdict, "unsupported")
expect_identical(log_ratio_plan@diagnostics[[1L]]@data$op, "log")

# The declared result base matches R for every typed math function.
typed_math <- c(
  "abs", "sign", "floor", "ceiling", "trunc", "sinh", "cosh", "tanh", "atan", "asinh",
  "log", "log2", "log10", "log1p", "expm1", "sin", "cos", "tan", "asin", "acos",
  "acosh", "atanh"
)
for (op in typed_math) {
  typed_kernel <- function(x) NULL
  body(typed_kernel) <- bquote({
    declare(type(x = double(n)))
    .(as.name(op))(x)
  })
  typed <- tccq_analyze(typed_kernel)
  expect_true(typed@success, info = op)
  expect_identical(
    typed@value@values[[typed@value@result]]@type@base,
    typeof(get(op, envir = baseenv())(1)),
    info = op
  )
}

# `abs()` keeps integer input integer in R; only the double form is modeled.
integer_abs <- first_diagnostic(function(x) {
  declare(type(x = integer(n)))
  abs(x)
})
expect_identical(integer_abs@code, "ops.unsupported_elementwise_type")
expect_identical(integer_abs@verdict, "unsupported")
