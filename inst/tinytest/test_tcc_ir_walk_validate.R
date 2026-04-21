library(tccquickr)

declare <- function(...) invisible(NULL)
type <- function(...) NULL

# Focus this file on traversal, validation, and wrapper behavior rather than
# mirroring every IR constructor field back to itself.

fallback_node <- tccquickr:::tcc_ir_fallback("unsupported")
expect_equal(names(fallback_node), c("tag", "mode", "reason"))
expect_equal(fallback_node$reason, "unsupported")

seen_calls <- 0L
seen_leaves <- 0L
w_counts <- tccquickr:::tccq_make_walker(
  call = function(e, w) {
    seen_calls <<- seen_calls + 1L
    for (ee in as.list(e)[-1]) {
      if (!missing(ee)) tccquickr:::tccq_walk(ee, w)
    }
  },
  leaf = function(e, w) {
    seen_leaves <<- seen_leaves + 1L
    invisible(NULL)
  }
)
tccquickr:::tccq_walk(quote(f(a, 1L, g(b))), w_counts)
expect_true(seen_calls >= 2L)
expect_true(seen_leaves >= 3L)

handler_hit <- FALSE
w_handler <- tccquickr:::tccq_make_walker(
  handler = function(v, w) {
    if (identical(v, "sin")) {
      function(e, w) {
        handler_hit <<- TRUE
        invisible(NULL)
      }
    }
  }
)
tccquickr:::tccq_walk(quote(sin(1)), w_handler)
expect_true(handler_hit)

expect_true(tccquickr:::tccq_has_boundary(quote(.Call("x", y))))
expect_true(tccquickr:::tccq_has_boundary(quote(f(.Primitive("sqrt")(x)))))
expect_false(tccquickr:::tccq_has_boundary(quote(a + b * c)))

hidden_tag_ir <- list(
  tag = "root",
  mode = "void",
  args = list(
    list(tag = "child", mode = "double"),
    list(tag = "target", mode = "double")
  )
)
expect_true(tccquickr:::tccq_ir_has_tag(hidden_tag_ir, "target"))
expect_false(tccquickr:::tccq_ir_has_tag(hidden_tag_ir, "absent"))

expect_true(tccquickr:::tccq_validate_ir(list(
  tag = "mean_expr",
  mode = "double",
  expr = list(tag = "const", mode = "double", value = 1)
), fallback = "soft"))

expect_error(
  tccquickr:::tccq_validate_ir(
    list(tag = "rf_call", mode = "sexp", shape = "scalar", contract = list()),
    fallback = "hard"
  ),
  pattern = "forbids Rf_eval"
)

hidden_bad_ir <- list(
  tag = "block",
  mode = "void",
  stmts = list(
    list(tag = "matmul", mode = "double", a = list(tag = "const"))
  ),
  result = list(tag = "const", mode = "double", value = 0)
)
expect_error(
  tccquickr:::tccq_validate_ir(hidden_bad_ir, fallback = "soft"),
  pattern = "matmul missing operands"
)

id_scalar <- function(x) {
  declare(type(x = double(1)))
  x
}
decl <- tccquickr:::tcc_quick_parse_declare(id_scalar)
ir <- tccquickr:::tcc_quick_lower(id_scalar, decl)
built <- tccquickr:::tcc_quick_compile(id_scalar, decl, ir)
expect_true(identical(typeof(built$call_ptr), "externalptr"))

wrapped_ptr <- tccquickr:::tcc_quick_make_wrapper(
  built$callable,
  formals(id_scalar),
  built$compiled,
  id_scalar,
  compiled_ptr = built$call_ptr
)
expect_equal(wrapped_ptr(7), id_scalar(7), tolerance = 1e-12)
expect_true(grepl(
  ".rtinycc_call",
  paste(deparse(body(wrapped_ptr)), collapse = " "),
  fixed = TRUE
))
