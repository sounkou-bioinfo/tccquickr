# test_tccq_shape_domains.R

shape_join_sum_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum(x + y)
}

shape_join_sum_mod <- tccq_compile(shape_join_sum_fn, mode = "ir")
shape_join_id <- shape_join_sum_mod$kernel$domain$shape_domain
shape_join_rec <- shape_join_sum_mod$shape_facts$domains[[shape_join_id]]
expect_equal(shape_join_sum_mod$kernel$tag, "fold")
expect_equal(shape_join_sum_mod$kernel$elem$tag, "producer")
expect_equal(shape_join_rec$kind, "join")
expect_true(all(c("x", "y") %in% shape_join_rec$witness_names))
expect_true(tccquickr:::tccq_kernel_domains_equivalent(
  shape_join_sum_mod$kernel$domain,
  shape_join_sum_mod$kernel$elem$domain,
  shape_facts = shape_join_sum_mod$shape_facts
))
expect_equal(tccq_compile(shape_join_sum_fn)(c(1, 2, 3), c(10, 20, 30)), 66)

shape_alias_domain_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  z <- y + 1
  z
}

shape_alias_domain_mod <- tccq_compile(shape_alias_domain_fn, mode = "ir")
expect_identical(shape_alias_domain_mod$shape_facts$by_name$y, shape_alias_domain_mod$shape_facts$by_name$x)
expect_identical(shape_alias_domain_mod$shape_facts$by_name$z, shape_alias_domain_mod$shape_facts$by_name$x)
expect_equal(
  shape_alias_domain_mod$shape_facts$domains[[shape_alias_domain_mod$shape_facts$by_name$x]]$kind,
  "full"
)
expect_identical(shape_alias_domain_mod$storage_plan$bindings$y$domain_id, shape_alias_domain_mod$shape_facts$by_name$x)
expect_equal(tccq_compile(shape_alias_domain_fn)(c(1, 2, 3)), c(2, 3, 4))

shape_slice_domain_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  y
}

shape_slice_domain_mod <- tccq_compile(shape_slice_domain_fn, mode = "ir")
shape_slice_y <- shape_slice_domain_mod$shape_facts$by_name$y
shape_slice_rec <- shape_slice_domain_mod$shape_facts$domains[[shape_slice_y]]
expect_equal(shape_slice_rec$kind, "slice")
expect_identical(shape_slice_rec$parent, shape_slice_domain_mod$shape_facts$by_name$x)
expect_identical(shape_slice_rec$root_name, "x")
expect_identical(shape_slice_domain_mod$storage_plan$bindings$y$domain_id, shape_slice_y)
expect_equal(tccq_compile(shape_slice_domain_fn)(c(1, 2, 3, 4), 2L, 4L), c(2, 3, 4))

shape_mutated_alias_fn <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  y <- x
  y[1] <- v
  y
}

shape_mutated_alias_mod <- tccq_compile(shape_mutated_alias_fn, mode = "ir")
expect_identical(shape_mutated_alias_mod$shape_facts$by_name$y, shape_mutated_alias_mod$shape_facts$by_name$x)
expect_identical(shape_mutated_alias_mod$storage_plan$bindings$y$domain_id, shape_mutated_alias_mod$shape_facts$by_name$x)
expect_identical(shape_mutated_alias_mod$shape_facts$result_domain, shape_mutated_alias_mod$shape_facts$by_name$x)
expect_equal(tccq_compile(shape_mutated_alias_fn)(c(1, 2, 3), 99), c(99, 2, 3))

shape_chain_locals_fn <- function(x) {
  declare(type(x = double(NA)))
  y <- x
  z <- y + 1
  w <- z + 2
  w
}

expect_equal(
  tccq_compile(shape_chain_locals_fn, backend = tccq_backend_shlib())(c(1, 2, 3)),
  c(4, 5, 6)
)

shape_slice_local_expr_fn <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi] + 1
  y
}

expect_equal(
  tccq_compile(shape_slice_local_expr_fn, backend = tccq_backend_shlib())(c(1, 2, 3, 4), 2L, 4L),
  c(3, 4, 5)
)

shape_mixed_slice_join_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  x + y[1:2]
}

expect_error(
  tccq_compile(shape_mixed_slice_join_fn)(as.double(1:4), as.double(10:13)),
  pattern = "vector length mismatch"
)
expect_error(
  tccq_compile(shape_mixed_slice_join_fn, backend = tccq_backend_shlib())(as.double(1:4), as.double(10:13)),
  pattern = "vector length mismatch"
)

shape_mixed_slice_oob_fn <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  x + y[2:6]
}

expect_error(
  tccq_compile(shape_mixed_slice_oob_fn)(as.double(1:5), as.double(10:13)),
  pattern = "index out of bounds"
)
