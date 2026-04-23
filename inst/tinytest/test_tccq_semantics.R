# test_tccq_semantics.R

logical_na_fn <- function(dummy) {
  declare(type(dummy = logical()))
  NA
}

compiled_logical_na <- tccq_compile(logical_na_fn)
expect_identical(compiled_logical_na(TRUE), NA)

identity_vec_fn <- function(x) {
  declare(type(x = double(NA)))
  identity(x)
}
compiled_identity_vec <- tccq_compile(identity_vec_fn)
expect_equal(compiled_identity_vec(c(1, 2, 3)), c(1, 2, 3))

extlib_ctx <- tccquickr:::tccq_context_from_extlibs(list(
  tccquickr:::tccq_external_library(
    name = "demo",
    headers = "#include <demo.h>",
    libraries = "demo",
    include_paths = "/opt/demo/include",
    library_paths = "/opt/demo/lib",
    options = c("-O2", "-DDEMO=1"),
    symbols = list(foo = "foo"),
    effects = list(foo = "pure")
  )
))

expect_identical(extlib_ctx$headers, "#include <demo.h>")
expect_identical(extlib_ctx$libraries, "demo")
expect_identical(extlib_ctx$include_paths, "/opt/demo/include")
expect_identical(extlib_ctx$library_paths, "/opt/demo/lib")
expect_identical(extlib_ctx$options, c("-O2", "-DDEMO=1"))
expect_identical(extlib_ctx$external_symbols$foo, "foo")
expect_identical(extlib_ctx$external_effects$foo, "pure")

expect_identical(tccq_backend_tinycc()$capabilities$cli, FALSE)
expect_identical(tccq_backend_source()$capabilities$emits_source, TRUE)
expect_identical(tccq_backend_shlib()$capabilities$shared_library, TRUE)
