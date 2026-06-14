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
expect_equal(map_result@value@regions[[1L]]@fusion_groups[[1L]]@kind, "map")
expect_equal(map_result@value@regions[[1L]]@fusion_groups[[1L]]@region_kind, "kernel")

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
