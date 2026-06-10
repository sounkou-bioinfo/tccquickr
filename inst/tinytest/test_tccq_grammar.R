# Grammar coverage probe: invariants that must not silently regress.
# The full table is rendered in docs/r-subset-grammar.md; here we pin the
# dispositions the compiler is contracted to provide.

cov <- tccquickr:::tccq_grammar_coverage()

# The probe harness produces a well-formed table.
expect_true(is.data.frame(cov))
expect_true(all(c("production", "rule", "example", "disposition") %in% names(cov)))
expect_true(all(cov$disposition %in% c("core", "boundary", "rejected", "error")))
# Every probe is constructible (no parse failures in our own examples).
expect_false(any(cov$disposition == "error"))

disp <- function(ex) cov$disposition[match(ex, cov$example)]

# Core surface that must keep compiling to pure C.
for (ex in c("x + 1", "x - 1", "x * 3", "x / 2", "x ^ 2",
             "x > 0", "(x > 0) & (x < 1)", "sin(x)", "sum(x)",
             "a <- x + 1; a", "x[1L]", "x[1:n]",
             "out <- x; for (i in 1:n) { out[i] <- out[i] * 2 }; out")) {
  expect_equal(disp(ex), "core", info = ex)
}

# Unknown calls are a boundary under auto fallback, never silently core.
expect_equal(disp("qux(x)"), "boundary")

# Out-of-subset constructs are rejected (honest gaps).
for (ex in c("x$a", "x@a", "(function(z) z)(x)")) {
  expect_equal(disp(ex), "rejected", info = ex)
}
