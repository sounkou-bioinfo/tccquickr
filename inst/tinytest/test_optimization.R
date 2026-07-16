library(tinytest)
library(tccquickr)

dead_store_optimization <- TccqDeadStoreOptimization()

expect_true(S7::S7_inherits(
  dead_store_optimization,
  TccqDeadStoreOptimization
))
expect_true(s7contract::has_trait(
  dead_store_optimization,
  TccqProgramOptimization
))

dead_store_kernel <- function(x) {
  declare(type(x = double()))
  repeat {
    discarded <- exp(x)
    break
  }
  x
}

dead_store_analysis <- tccq_analyze(dead_store_kernel, strict = TRUE)
dead_store_program <- dead_store_analysis@value
dead_store_original_body <- dead_store_program@schedule@body
dead_store_original_repeat <- Filter(
  function(statement) S7::S7_inherits(statement, TccqRepeat),
  dead_store_original_body@statements
)[[1L]]

expect_equal(length(dead_store_original_body@locals), 1L)
expect_equal(length(dead_store_original_repeat@body@statements), 2L)
expect_true(S7::S7_inherits(
  dead_store_original_repeat@body@statements[[1L]],
  TccqAssignment
))

dead_store_result <- with(
  TccqProgramOptimization,
  tccq_optimize(dead_store_optimization, dead_store_program)
)
expect_true(dead_store_result@success)
dead_store_optimized <- dead_store_result@value
dead_store_optimized_body <- dead_store_optimized@schedule@body
dead_store_optimized_repeat <- Filter(
  function(statement) S7::S7_inherits(statement, TccqRepeat),
  dead_store_optimized_body@statements
)[[1L]]

expect_equal(length(dead_store_optimized_body@locals), 0L)
expect_equal(length(dead_store_optimized_repeat@body@statements), 1L)
expect_true(S7::S7_inherits(
  dead_store_optimized_repeat@body@statements[[1L]],
  TccqLoopTransfer
))
expect_false(dead_store_optimized_body@effect@writes)
expect_equal(length(dead_store_optimized@regions[[1L]]@values), 1L)
expect_equal(dead_store_optimized@regions[[1L]]@values[[1L]]@op, "formal")
expect_identical(
  dead_store_optimized@regions[[1L]]@effect,
  dead_store_optimized_body@effect
)

# Optimization is functional: the analyzed program remains unchanged.
expect_equal(length(dead_store_program@schedule@body@locals), 1L)
expect_equal(length(dead_store_original_repeat@body@statements), 2L)

dead_store_verification <- with(
  TccqProgramOptimization,
  tccq_verify_optimization(
    dead_store_optimization,
    before = dead_store_program,
    after = dead_store_optimized
  )
)
expect_true(dead_store_verification@success)

unoptimized_verification <- with(
  TccqProgramOptimization,
  tccq_verify_optimization(
    dead_store_optimization,
    before = dead_store_program,
    after = dead_store_program
  )
)
expect_false(unoptimized_verification@success)
expect_equal(
  unoptimized_verification@diagnostics[[1L]]@code,
  "optimization.verification_failed"
)

idempotent_result <- with(
  TccqProgramOptimization,
  tccq_optimize(dead_store_optimization, dead_store_optimized)
)
expect_true(idempotent_result@success)
expect_identical(idempotent_result@value, dead_store_optimized)

transitive_dead_store_kernel <- function(x) {
  declare(type(x = double()))
  repeat {
    first_dead_value <- exp(x)
    second_dead_value <- first_dead_value
    break
  }
  x
}
transitive_dead_store_program <- tccq_analyze(
  transitive_dead_store_kernel,
  strict = TRUE
)@value
transitive_dead_store_result <- with(
  TccqProgramOptimization,
  tccq_optimize(dead_store_optimization, transitive_dead_store_program)
)
expect_true(transitive_dead_store_result@success)
transitive_repeat <- Filter(
  function(statement) S7::S7_inherits(statement, TccqRepeat),
  transitive_dead_store_result@value@schedule@body@statements
)[[1L]]
expect_equal(length(transitive_repeat@body@statements), 1L)
expect_equal(
  length(transitive_dead_store_result@value@schedule@body@locals),
  0L
)

warning_store_kernel <- function(x) {
  declare(type(x = double()))
  repeat {
    discarded <- sqrt(x)
    break
  }
  x
}

warning_store_program <- tccq_analyze(
  warning_store_kernel,
  strict = TRUE
)@value
warning_store_result <- with(
  TccqProgramOptimization,
  tccq_optimize(dead_store_optimization, warning_store_program)
)
expect_true(warning_store_result@success)
warning_store_repeat <- Filter(
  function(statement) S7::S7_inherits(statement, TccqRepeat),
  warning_store_result@value@schedule@body@statements
)[[1L]]
expect_equal(length(warning_store_repeat@body@statements), 2L)
expect_true(warning_store_repeat@body@statements[[1L]]@value@effect@may_warn)

used_store_kernel <- function(x) {
  declare(type(x = double()))
  retained <- 0
  repeat {
    retained <- exp(x)
    break
  }
  retained
}

used_store_program <- tccq_analyze(used_store_kernel, strict = TRUE)@value
used_store_result <- with(
  TccqProgramOptimization,
  tccq_optimize(dead_store_optimization, used_store_program)
)
expect_true(used_store_result@success)
used_store_repeat <- Filter(
  function(statement) S7::S7_inherits(statement, TccqRepeat),
  used_store_result@value@schedule@body@statements
)[[1L]]
expect_equal(length(used_store_repeat@body@statements), 2L)

linear_kernel <- function(x) {
  declare(type(x = double(n)))
  exp(x)
}
linear_program <- tccq_analyze(linear_kernel, strict = TRUE)@value
linear_result <- with(
  TccqProgramOptimization,
  tccq_optimize(dead_store_optimization, linear_program)
)
expect_true(linear_result@success)
expect_identical(linear_result@value, linear_program)

# The user-facing compiler applies the optimization once before the common
# typed body reaches any source backend.
compiled_dead_store <- tccq_compile(
  dead_store_kernel,
  context = tccq_backend_context(mode = "source")
)
expect_true(compiled_dead_store@success)
expect_equal(names(compiled_dead_store@value@plans), c("rtinycc", "c", "fortran"))
compiled_bodies <- lapply(
  compiled_dead_store@value@plans,
  function(plan) plan@products@body
)
expect_true(all(vapply(
  compiled_bodies,
  function(body) length(body@locals) == 0L,
  logical(1)
)))
expect_true(all(vapply(
  compiled_bodies,
  function(body) identical(body, compiled_bodies[[1L]]),
  logical(1)
)))
expect_false(any(vapply(
  compiled_dead_store@value@plans,
  function(plan) grepl("exp", plan@products@attrs$source, fixed = TRUE),
  logical(1)
)))

# Dead source-graph provenance does not veto the live region consumed by a
# backend. Non-finite literals remain unsupported when they are executable.
dead_literal_kernel <- function(x) {
  declare(type(x = double()))
  repeat {
    discarded_literal <- NaN
    break
  }
  x
}
compiled_dead_literal <- tccq_compile(
  dead_literal_kernel,
  context = tccq_backend_context(mode = "source")
)
expect_true(compiled_dead_literal@success)
expect_true(all(vapply(
  compiled_dead_literal@value@plans,
  function(plan) length(plan@diagnostics) == 0L,
  logical(1)
)))

live_literal_kernel <- function(x) {
  declare(type(x = double()))
  NaN
}
compiled_live_literal <- tccq_compile(
  live_literal_kernel,
  context = tccq_backend_context(mode = "source"),
  strict = FALSE
)
expect_false(compiled_live_literal@success)
expect_equal(
  compiled_live_literal@diagnostics[[1L]]@code,
  "backend.unsupported_literal_kind"
)

# The compiler invokes the verifier independently; an implementation cannot
# smuggle a successful but unverified result into backend planning.
CompilerVerificationProbe <- S7::new_class("CompilerVerificationProbe")
s7contract::impl_trait(
  TccqProgramOptimization,
  CompilerVerificationProbe,
  methods = list(
    optimize = function(optimization, program) {
      tccq_result(success = TRUE, value = program)
    },
    verify = function(optimization, before, after) {
      tccq_result(
        success = FALSE,
        diagnostics = list(tccq_diagnostic(
          "optimization.test_verifier_called",
          "Compiler verification probe rejected the transformed program.",
          phase = "optimization",
          path = "optimization.test"
        ))
      )
    }
  )
)
compiler_verification_probe <- tccq_compile(
  dead_store_kernel,
  optimization = CompilerVerificationProbe(),
  context = tccq_backend_context(mode = "source"),
  strict = FALSE
)
expect_false(compiler_verification_probe@success)
expect_equal(
  compiler_verification_probe@diagnostics[[1L]]@code,
  "optimization.test_verifier_called"
)
