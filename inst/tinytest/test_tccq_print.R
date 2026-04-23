# test_tccq_print.R

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

expect_equal(format(tccquickr:::tccq_type_scalar("double")), "double")
expect_equal(format(tccquickr:::tccq_type_vector("integer")), "integer[NA]")

mod_sum <- tccq_compile(print_sum_kernel, mode = "ir")
out_sum <- capture.output(ret_sum <- print(mod_sum))
expect_identical(ret_sum, mod_sum)
expect_true(length(out_sum) >= 3L)

mod_prog <- tccq_compile(print_prog_kernel, mode = "ir")
out_prog <- capture.output(ret_prog <- print(mod_prog))
expect_identical(ret_prog, mod_prog)
expect_true(length(out_prog) >= 4L)
