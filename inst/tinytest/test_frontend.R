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

expect_false(result@ok)
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
expect_true(buffer_result@ok)
expect_equal(buffer_result@value@formals$bytes@type@base, "raw")
expect_equal(buffer_result@value@formals$scratch@type@base, "buffer")

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
