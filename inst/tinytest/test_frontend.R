library(tinytest)
library(tccquickr)

result <- tccq_analyze(tccq_apotheosis_kernel())

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
