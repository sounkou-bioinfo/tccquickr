# test_tccq_storage_fast_return.R

patch_then_return <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

src <- tccq_compile(patch_then_return, mode = "code")
allocs <- gregexpr("Rf_allocVector", src, fixed = TRUE)[[1L]]
expect_equal(sum(allocs > 0L), 1L)

compiled_patch_then_return <- tccq_compile(patch_then_return)
expect_equal(compiled_patch_then_return(c(1, 2, 3), 2L, 10), c(1, 10, 3))
