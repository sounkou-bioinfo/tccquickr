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
  function(x) identical(x@code, "frontend.unsupported_call"),
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
