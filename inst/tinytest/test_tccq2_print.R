# test_tccq2_print.R

print_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

print_prog_kernel <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  sum(y)
}

expect_equal(format(tccquickr:::tccq2_type_scalar("double")), "double")
expect_equal(format(tccquickr:::tccq2_type_vector("integer")), "integer[NA]")

mod_sum <- tccq2_compile(print_sum_kernel, mode = "ir")
out_sum <- paste(capture.output(print(mod_sum)), collapse = "\n")
expect_true(grepl("<tccq2_module>", out_sum, fixed = TRUE))
expect_true(grepl("kernel: fold", out_sum, fixed = TRUE))
expect_true(grepl("result type: double", out_sum, fixed = TRUE))

mod_prog <- tccq2_compile(print_prog_kernel, mode = "ir")
out_prog <- paste(capture.output(print(mod_prog)), collapse = "\n")
expect_true(grepl("kernel: kernel_program -> fold", out_prog, fixed = TRUE))
expect_true(grepl("locals:", out_prog, fixed = TRUE))
