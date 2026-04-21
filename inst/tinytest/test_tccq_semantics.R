# test_tccq_semantics.R

logical_na_fn <- function(dummy) {
  declare(type(dummy = logical()))
  NA
}

logical_na_src <- tccq_compile(logical_na_fn, mode = "code")
expect_true(grepl("NA_LOGICAL", logical_na_src, fixed = TRUE))

compiled_logical_na <- tccq_compile(logical_na_fn)
expect_identical(compiled_logical_na(TRUE), NA)

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

backend_tinycc_src <- paste(deparse(body(tccquickr:::tccq_backend_tinycc)), collapse = "\n")
expect_false(grepl("tcc_run_cli", backend_tinycc_src, fixed = TRUE))
expect_identical(tccquickr:::tccq_backend_tinycc()$capabilities$cli, FALSE)
expect_identical(tccquickr:::tccq_backend_source()$capabilities$emits_source, TRUE)
