# tccq_backend.R - fresh backend protocol
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_backend <- function(name, compile, capabilities = list()) {
  structure(
    list(name = name, compile = compile, capabilities = capabilities),
    class = "tccq_backend"
  )
}

tccq_backend_source <- function() {
  tccq_backend(
    name = "source",
    capabilities = list(c = TRUE, compile = FALSE, r_api = TRUE),
    compile = function(module, target, ctx = list()) {
      src <- target$emit(module, ctx)
      list(
        backend = "source",
        source = src,
        compiled = NULL,
        callable = NULL,
        module = module
      )
    }
  )
}

tccq_backend_tinycc <- function() {
  tccq_backend(
    name = "tinycc",
    capabilities = list(c = TRUE, compile = TRUE, r_api = TRUE, in_memory = TRUE),
    compile = function(module, target, ctx = list()) {
      tccq_require_namespace("Rtinycc")

      src <- target$emit(module, ctx)
      spec <- target$entry_spec(module, ctx)

      ffi <- Rtinycc::tcc_ffi()
      ffi <- Rtinycc::tcc_source(ffi, src)

      headers <- ctx$headers %||% character()
      if (length(headers)) {
        for (header in headers) {
          ffi <- Rtinycc::tcc_header(ffi, header)
        }
      }

      libraries <- ctx$libraries %||% character()
      if (length(libraries)) {
        for (lib in libraries) {
          ffi <- Rtinycc::tcc_library(ffi, lib)
        }
      }

      bindings <- stats::setNames(list(spec), module$entry)
      ffi <- do.call(Rtinycc::tcc_bind, c(list(ffi), bindings))
      compiled <- Rtinycc::tcc_compile(ffi)
      callable <- compiled[[module$entry]]

      list(
        backend = "tinycc",
        source = src,
        compiled = compiled,
        callable = callable,
        module = module
      )
    }
  )
}
