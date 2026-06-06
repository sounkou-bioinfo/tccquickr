# test_tccq_jit.R

make_fake_backend <- function(counter) {
  tccquickr:::tccq_backend(
    name = "fake",
    capabilities = list(
      c = TRUE,
      compile = TRUE,
      r_api = TRUE,
      boundary_apis = character()
    ),
    compile = function(module, target, ctx = list(), source = NULL, spec = NULL) {
      counter$n <- counter$n + 1L
      compiled_spec <- module$specialization
      callable <- local({
        my_spec <- compiled_spec
        function(x) {
          list(len = length(x), type = typeof(x), specialization = my_spec)
        }
      })

      list(
        backend = "fake",
        source = source,
        compiled = NULL,
        callable = callable,
        module = module
      )
    }
  )
}

jit_specialization_kernel <- function(x) {
  declare(type(x = double(NA)))
  x + 1
}

counter <- new.env(parent = emptyenv())
counter$n <- 0L
backend <- make_fake_backend(counter)
jit <- tccq_jit(jit_specialization_kernel, backend = backend, exact = TRUE)

out4 <- jit(c(1, 2, 3, 4))
out4_again <- jit(c(10, 11, 12, 13))
out2 <- jit(c(5, 6))
out2_again <- jit(c(7, 8))

expect_equal(counter$n, 2L)
expect_equal(attr(jit, "tccq_jit_count")(), 2L)
expect_equal(out4$len, 4L)
expect_equal(out2$len, 2L)
expect_equal(out4$specialization$x$len, 4L)
expect_equal(out2$specialization$x$len, 2L)
expect_identical(out4$specialization, out4_again$specialization)
expect_identical(out2$specialization, out2_again$specialization)

# exact=False should not split by shape; one compile for one dtype/arity signature.
counter2 <- new.env(parent = emptyenv())
counter2$n <- 0L
backend2 <- make_fake_backend(counter2)
jit2 <- tccq_jit(jit_specialization_kernel, backend = backend2, exact = FALSE)

jit2(c(1, 2, 3, 4))
jit2(c(1, 2))
jit2(c(10, 20, 30, 40))

expect_equal(counter2$n, 1L)
expect_equal(attr(jit2, "tccq_jit_count")(), 1L)
expect_true(length(out4$specialization) == 1L || is.null(out4$specialization))
