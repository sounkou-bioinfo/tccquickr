library(tinytest)
library(tccquickr)

shape <- tccq_shape(c("n", "p"))
type <- tccq_type("double", shape)

expect_true(S7::S7_inherits(shape, TccqShape))
expect_equal(shape@rank, 2L)
expect_equal(shape@dims[[1L]]@kind, "symbol")
expect_true(S7::S7_inherits(type, TccqType))
expect_equal(type@base, "double")

err <- tryCatch(
  tccq_type("data.frame"),
  error = identity
)
expect_true(inherits(err, "tccq_error"))
expect_true(S7::S7_inherits(tccq_condition_diagnostic(err), TccqDiagnostic))
