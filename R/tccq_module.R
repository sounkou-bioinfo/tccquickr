# tccq_module.R - fresh compiler module object
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_module <- function(
  entry,
  formal_names,
  types,
  expr,
  body_exprs = list(expr),
  ir = NULL,
  kernel = NULL,
  facts = list(),
  alloc_plan = NULL,
  protect_plan = NULL,
  fallback = "hard",
  boundary_context = NULL,
  storage_plan = NULL
) {
  structure(
    list(
      entry = entry,
      formal_names = formal_names,
      types = types,
      expr = expr,
      body_exprs = body_exprs,
      ir = ir,
      kernel = kernel,
      facts = facts,
      alloc_plan = alloc_plan,
      protect_plan = protect_plan,
      fallback = fallback,
      boundary_context = boundary_context,
      storage_plan = storage_plan
    ),
    class = "tccq_module"
  )
}

tccq_module_validate <- function(module) {
  tccq_assert(inherits(module, "tccq_module"), "expected tccq_module")
  tccq_assert(is.character(module$entry), "module entry must be character")
  tccq_assert(length(module$entry) == 1L, "module entry must have length 1")
  tccq_assert(length(module$formal_names) == length(module$types), "formal/type mismatch")

  missing <- setdiff(module$formal_names, names(module$types))
  if (length(missing)) {
    tccq_abort("missing type declarations for: ", paste(missing, collapse = ", "))
  }

  if (is.null(module$body_exprs) || !length(module$body_exprs)) {
    tccq_abort("module must contain at least one body expression")
  }

  if (!is.null(module$ir)) {
    tccq_ir_validate(module$ir)
  }

  invisible(TRUE)
}

tccq_module_with <- function(module, ...) {
  updates <- list(...)
  for (nm in names(updates)) {
    module[[nm]] <- updates[[nm]]
  }
  module
}
