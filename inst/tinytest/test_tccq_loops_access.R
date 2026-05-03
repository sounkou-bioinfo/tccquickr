# test_tccq_loops_access.R

braced_declare_fn <- function(x) {
  declare({
    type(x = double(NA))
  })
  sum(x)
}

expect_equal(tccq_compile(braced_declare_fn)(as.double(1:4)), 10)

for_loop_fill_fn <- function(n) {
  declare(type(n = integer()))
  x <- integer(n)
  for (i in 1L:n) {
    x[i] <- i
  }
  x
}

expect_equal(tccq_compile(for_loop_fill_fn)(5L), 1:5)

descending_seq_loop_fn <- function(n) {
  declare(type(n = integer()))
  x <- integer(n)
  for (i in seq(n, 1L)) {
    x[i] <- n - i + 1L
  }
  x
}

expect_equal(tccq_compile(descending_seq_loop_fn)(5L), c(5L, 4L, 3L, 2L, 1L))

matrix_column_view_fn <- function(x, j) {
  declare(type(x = double(NA, NA)), type(j = integer()))
  x[, j]
}

matrix_row_view_fn <- function(x, i) {
  declare(type(x = double(NA, NA)), type(i = integer()))
  x[i, ]
}

mx <- matrix(as.double(1:12), 3L, 4L)
expect_equal(tccq_compile(matrix_column_view_fn)(mx, 3L), mx[, 3L])
expect_equal(tccq_compile(matrix_row_view_fn)(mx, 2L), mx[2L, ])

matrix_vector_rhs_column_store_fn <- function(x, v) {
  declare(type(x = double(NA, NA)), type(v = double(NA)))
  y <- x
  y[, 2L] <- v
  y
}

expect_equal(
  tccq_compile(matrix_vector_rhs_column_store_fn)(mx, as.double(c(9, 8, 7))),
  { y <- mx; y[, 2L] <- c(9, 8, 7); y }
)

vector_gather_fn <- function(x, idx) {
  declare(type(x = integer(NA)), type(idx = integer(NA)))
  x[idx]
}

expect_equal(tccq_compile(vector_gather_fn)(10:15, c(2L, 4L, 1L)), (10:15)[c(2L, 4L, 1L)])

which_max_fn <- function(x) {
  declare(type(x = double(NA)))
  which.max(x)
}

expect_equal(tccq_compile(which_max_fn)(c(1, 3, 2)), which.max(c(1, 3, 2)))

nested_reduce_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- max(x)
  y + 1
}

expect_equal(tccq_compile(nested_reduce_fn)(c(1, 3, 2)), 4)

viterbi_quickr_shape_fn <- function(
  observations,
  states,
  initial_probs,
  transition_probs,
  emission_probs
) {
  declare({
    type(observations = integer(num_steps))
    type(states = integer(num_states))
    type(initial_probs = double(num_states))
    type(transition_probs = double(num_states, num_states))
    type(emission_probs = double(num_states, num_obs))
  })

  num_states <- length(states)
  num_steps <- length(observations)

  trellis <- matrix(0, nrow = length(states), ncol = length(observations))
  backpointer <- matrix(0L, nrow = length(states), ncol = length(observations))

  trellis[, 1] <- initial_probs * emission_probs[, observations[1]]

  for (step in 2:num_steps) {
    for (current_state in 1:num_states) {
      probabilities <- trellis[, step - 1] * transition_probs[, current_state]
      trellis[current_state, step] <- max(probabilities) *
        emission_probs[current_state, observations[step]]
      backpointer[current_state, step] <- which.max(probabilities)
    }
  }

  path <- integer(length(observations))
  path[num_steps] <- which.max(trellis[, num_steps])
  for (step in seq((num_steps - 1), 1)) {
    path[step] <- backpointer[path[step + 1], step + 1]
  }

  states[path]
}

set.seed(42)
num_steps <- 15L
num_states <- 4L
num_obs <- 15L
observations <- sample(1:num_obs, num_steps, replace = TRUE)
states <- 1:num_states
initial_probs <- runif(num_states)
initial_probs <- initial_probs / sum(initial_probs)
transition_probs <- matrix(runif(num_states * num_states), nrow = num_states)
transition_probs <- transition_probs / rowSums(transition_probs)
emission_probs <- matrix(runif(num_states * num_obs), nrow = num_states)
emission_probs <- emission_probs / rowSums(emission_probs)

compiled_viterbi <- tccq_compile(viterbi_quickr_shape_fn)
expect_equal(
  compiled_viterbi(observations, states, initial_probs, transition_probs, emission_probs),
  viterbi_quickr_shape_fn(observations, states, initial_probs, transition_probs, emission_probs)
)
