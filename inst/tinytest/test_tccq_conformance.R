# Conformance gate (Porffor discipline): the generator emits only `core`
# constructs, so every generated program must match the R interpreter. A drop
# below 100% for the fixed seed is a real regression.

cf <- tccquickr:::tccq_conformance_run(n_cases = 24L)

expect_equal(cf$passed, cf$total)
expect_true(all(cf$results$status == "pass"))
expect_equal(nrow(cf$results), cf$total)
