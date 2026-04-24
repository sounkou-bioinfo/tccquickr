# test_tccq_boundary_eval.R

fallback_scalar_fn <- function(x) {
  declare(type(x = double()))
  floor(x)
}

fallback_scalar_ir <- tccq_compile(fallback_scalar_fn, mode = "ir", fallback = "auto")
expect_equal(fallback_scalar_ir$ir$result$tag, "boundary_call")
expect_identical(fallback_scalar_ir$ir$result$api, "r_eval")
expect_identical(fallback_scalar_ir$ir$result$type$mode, "double")

expect_identical(fallback_scalar_ir$boundary_context$headers, character())
expect_identical(fallback_scalar_ir$boundary_context$libraries, character())
expect_identical(tccq_compile(fallback_scalar_fn, fallback = "auto")(42), 42)

fallback_vector_fn <- function(x) {
  declare(type(x = double(NA)))
  floor(x)
}

fallback_vector_ir <- tccq_compile(fallback_vector_fn, mode = "ir", fallback = "auto")
expect_equal(fallback_vector_ir$ir$result$tag, "boundary_call")
expect_identical(fallback_vector_ir$ir$result$type$rank, 1L)
expect_equal(tccq_compile(fallback_vector_fn, fallback = "auto")(c(1, 2, 3)), c(1, 2, 3))

assign("tccq_test_id_vec", function(z) z, envir = .GlobalEnv)

boundary_alias_arg_plan_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  tccq_test_id_vec(y)
}

boundary_alias_arg_plan_ir <- tccq_compile(boundary_alias_arg_plan_fn, mode = "ir", fallback = "auto")
boundary_alias_arg_plan <- boundary_alias_arg_plan_ir$storage_plan$boundary_args[["1"]][[1]]
expect_identical(boundary_alias_arg_plan$strategy, "pass_source")
expect_identical(boundary_alias_arg_plan$source, "x")
expect_equal(tccq_compile(boundary_alias_arg_plan_fn, fallback = "auto")(as.double(1:3)), as.double(1:3))
expect_equal(
  tccq_compile(boundary_alias_arg_plan_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:3)),
  as.double(1:3)
)

altrep_boundary_alias_x <- as.double(1:10)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_boundary_alias_x))), fixed = TRUE)))
tccq_compile(boundary_alias_arg_plan_fn, fallback = "auto")(altrep_boundary_alias_x)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_boundary_alias_x))), fixed = TRUE)))

boundary_alias_length_arg_plan_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  length(tccq_test_id_vec(y))
}

boundary_alias_length_arg_plan_ir <- tccq_compile(boundary_alias_length_arg_plan_fn, mode = "ir", fallback = "auto")
boundary_alias_length_arg_plan <- boundary_alias_length_arg_plan_ir$storage_plan$boundary_args[["1"]][[1]]
expect_identical(boundary_alias_length_arg_plan$strategy, "pass_source")
altrep_boundary_alias_length_x <- as.double(1:10)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_boundary_alias_length_x))), fixed = TRUE)))
expect_equal(tccq_compile(boundary_alias_length_arg_plan_fn, fallback = "auto")(altrep_boundary_alias_length_x), 10L)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_boundary_alias_length_x))), fixed = TRUE)))
altrep_boundary_alias_length_y <- as.double(1:10)
expect_equal(
  tccq_compile(boundary_alias_length_arg_plan_fn, fallback = "auto", backend = tccq_backend_shlib())(altrep_boundary_alias_length_y),
  10L
)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_boundary_alias_length_y))), fixed = TRUE)))

boundary_alias_chain_length_arg_plan_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  z <- y
  length(tccq_test_id_vec(z))
}

boundary_alias_chain_length_arg_plan_ir <- tccq_compile(boundary_alias_chain_length_arg_plan_fn, mode = "ir", fallback = "auto")
boundary_alias_chain_length_arg_plan <- boundary_alias_chain_length_arg_plan_ir$storage_plan$boundary_args[["1"]][[1]]
expect_identical(boundary_alias_chain_length_arg_plan$strategy, "pass_source")
expect_identical(boundary_alias_chain_length_arg_plan$source, "x")
altrep_boundary_alias_chain_x <- as.double(1:10)
expect_equal(tccq_compile(boundary_alias_chain_length_arg_plan_fn, fallback = "auto")(altrep_boundary_alias_chain_x), 10L)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_boundary_alias_chain_x))), fixed = TRUE)))
altrep_boundary_alias_chain_y <- as.double(1:10)
expect_equal(
  tccq_compile(boundary_alias_chain_length_arg_plan_fn, fallback = "auto", backend = tccq_backend_shlib())(altrep_boundary_alias_chain_y),
  10L
)
expect_true(any(grepl("compact", capture.output(.Internal(inspect(altrep_boundary_alias_chain_y))), fixed = TRUE)))

boundary_multi_arg_plan_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x[2:4]
  a <- length(tccq_test_id_vec(y))
  z <- x
  a + length(tccq_test_id_vec(z))
}

boundary_multi_arg_plan_ir <- tccq_compile(boundary_multi_arg_plan_fn, mode = "ir", fallback = "auto")
expect_identical(
  vapply(tccquickr:::tccq_boundary_nodes(boundary_multi_arg_plan_ir$ir), `[[`, integer(1), "boundary_id"),
  1:2
)
expect_identical(
  vapply(tccquickr:::tccq_boundary_nodes(boundary_multi_arg_plan_ir$kernel), `[[`, integer(1), "boundary_id"),
  1:2
)
expect_identical(boundary_multi_arg_plan_ir$storage_plan$boundary_args[["1"]][[1]]$strategy, "materialize_local")
expect_identical(boundary_multi_arg_plan_ir$storage_plan$boundary_args[["2"]][[1]]$strategy, "pass_source")
expect_equal(tccq_compile(boundary_multi_arg_plan_fn, fallback = "auto")(as.double(1:5)), 8L)
expect_equal(
  tccq_compile(boundary_multi_arg_plan_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  8L
)

boundary_owned_arg_plan_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x + 1
  tccq_test_id_vec(y)
}

boundary_owned_arg_plan_ir <- tccq_compile(boundary_owned_arg_plan_fn, mode = "ir", fallback = "auto")
boundary_owned_arg_plan <- boundary_owned_arg_plan_ir$storage_plan$boundary_args[["1"]][[1]]
expect_identical(boundary_owned_arg_plan$strategy, "pass_local")
expect_identical(boundary_owned_arg_plan$sexp, "y")
expect_equal(tccq_compile(boundary_owned_arg_plan_fn, fallback = "auto")(as.double(1:3)), as.double(2:4))

boundary_view_arg_plan_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x[2:4]
  tccq_test_id_vec(y)
}

boundary_view_arg_plan_ir <- tccq_compile(boundary_view_arg_plan_fn, mode = "ir", fallback = "auto")
boundary_view_arg_plan <- boundary_view_arg_plan_ir$storage_plan$boundary_args[["1"]][[1]]
expect_identical(boundary_view_arg_plan$strategy, "materialize_local")
expect_equal(tccq_compile(boundary_view_arg_plan_fn, fallback = "auto")(as.double(1:5)), as.double(2:4))
expect_equal(
  tccq_compile(boundary_view_arg_plan_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:5)),
  as.double(2:4)
)

boundary_mutated_alias_arg_plan_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  y[1] <- 99
  tccq_test_id_vec(y)
}

boundary_mutated_alias_arg_plan_ir <- tccq_compile(boundary_mutated_alias_arg_plan_fn, mode = "ir", fallback = "auto")
boundary_mutated_alias_arg_plan <- boundary_mutated_alias_arg_plan_ir$storage_plan$boundary_args[["1"]][[1]]
expect_identical(boundary_mutated_alias_arg_plan$strategy, "materialize_local")
expect_equal(tccq_compile(boundary_mutated_alias_arg_plan_fn, fallback = "auto")(as.double(1:3)), c(99, 2, 3))
expect_equal(
  tccq_compile(boundary_mutated_alias_arg_plan_fn, fallback = "auto", backend = tccq_backend_shlib())(as.double(1:3)),
  c(99, 2, 3)
)
