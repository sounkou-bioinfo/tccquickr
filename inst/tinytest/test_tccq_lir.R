# test_tccq_lir.R - lowered IR constructors, validation, traversal

# A small but representative function:
#   y <- alloc double[n]; for i in [0,n): y[i] = x[i] + 1.0; return y
n <- tccq_lir_param("n", "xlen")
x <- tccq_lir_param("x", "double", rank = 1L)
y <- tccq_lir_alloc("double", extent = n, own = "owned")

body <- tccq_lir_block(list(
  tccq_lir_for(
    var = "i",
    lo = tccq_lir_const(0L, "xlen"),
    hi = n,
    body = tccq_lir_block(list(
      tccq_lir_store(
        addr = tccq_lir_index(y, terms = list(tccq_lir_temp("i", "xlen")), elt = "double"),
        value = tccq_lir_binop(
          "+",
          tccq_lir_load(
            tccq_lir_index(x, terms = list(tccq_lir_temp("i", "xlen")), elt = "double"),
            "double"
          ),
          tccq_lir_const(1.0, "double"),
          "double"
        )
      )
    ))
  )
))

f <- tccq_lir_func("addone", params = list(n, x), body = body, ret = y)

# Structural identity
expect_true(tccq_is_lir(f))
expect_equal(f$tag, "func")
expect_equal(f$name, "addone")
expect_true(inherits(f, "tccq_lir_func"))

# Validation accepts a well-formed tree
expect_true(tccq_lir_validate(f))

# Validation rejects a bad element type
bad <- tccq_lir_const(1, "complex")
expect_error(tccq_lir_validate(bad), "bad element type")

# Validation rejects a bad ownership tag
bad_alloc <- tccq_lir("alloc", elt = "double", extent = tccq_lir_const(1L, "xlen"), own = "shared")
expect_error(tccq_lir_validate(bad_alloc), "bad ownership")

# Validation rejects unknown tags
expect_error(tccq_lir_validate(tccq_lir("frobnicate")), "unknown node tag")

# Walk visits every node (pre-order); count the loads
loads <- 0L
tccq_lir_walk(f, function(node) {
  if (identical(node$tag, "load")) loads <<- loads + 1L
})
expect_equal(loads, 1L)

# Boundary regions are representable and validate
b <- tccq_lir_boundary("unsupported_call", inputs = list(x), outputs = list(), elt = "void")
expect_true(tccq_lir_validate(b))
