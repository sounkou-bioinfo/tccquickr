# test_tccq2_fresh_core.R

fresh_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

fresh_vec_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sin(x) + y * y
}

module <- tccquickr:::tccq2_compile(fresh_sum_kernel, mode = "ir")
expect_equal(module$kernel$tag, "fold")
expect_equal(module$kernel$op, "sum")
expect_equal(module$kernel$domain$tag, "domain")
expect_equal(module$kernel$elem$tag, "producer")
expect_equal(module$kernel$elem$elem$tag, "binary")
expect_true(!is.null(module$alloc_plan))
expect_true(!is.null(module$protect_plan))

src <- tccquickr:::tccq2_compile(fresh_sum_kernel, mode = "code")
expect_true(grepl("SEXP tccq2_entry", src, fixed = TRUE))
expect_true(grepl("for (R_xlen_t i = 0; i < n_out; ++i)", src, fixed = TRUE))
expect_true(grepl("acc +=", src, fixed = TRUE))
expect_true(!grepl("tcc_quick", src, fixed = TRUE))

vec_module <- tccquickr:::tccq2_compile(fresh_vec_kernel, mode = "ir")
expect_equal(vec_module$kernel$tag, "materialize")
expect_equal(vec_module$kernel$producer$tag, "producer")
expect_equal(vec_module$kernel$producer$domain$tag, "domain")
expect_equal(vec_module$kernel$producer$elem$tag, "binary")

vec_src <- tccquickr:::tccq2_compile(fresh_vec_kernel, mode = "code")
expect_true(grepl("Rf_allocVector(REALSXP, n_out)", vec_src, fixed = TRUE))
expect_true(grepl("p_out[i]", vec_src, fixed = TRUE))
