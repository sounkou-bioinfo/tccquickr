# tccq2_module.R - fresh compiler module object
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_module <- function(
  entry,
  formal_names,
  types,
  expr,
  ir = NULL,
  kernel = NULL,
  facts = list(),
  alloc_plan = NULL,
  protect_plan = NULL
) {
  structure(
    list(
      entry = entry,
      formal_names = formal_names,
      types = types,
      expr = expr,
      ir = ir,
      kernel = kernel,
      facts = facts,
      alloc_plan = alloc_plan,
      protect_plan = protect_plan
    ),
    class = "tccq2_module"
  )
}

tccq2_module_validate <- function(module) {
  tccq2_assert(inherits(module, "tccq2_module"), "expected tccq2_module")
  tccq2_assert(is.character(module$entry), "module entry must be character")
  tccq2_assert(length(module$entry) == 1L, "module entry must have length 1")
  tccq2_assert(length(module$formal_names) == length(module$types), "formal/type mismatch")

  missing <- setdiff(module$formal_names, names(module$types))
  if (length(missing)) {
    tccq2_abort("missing type declarations for: ", paste(missing, collapse = ", "))
  }

  if (!is.null(module$ir)) {
    tccq2_ir_validate(module$ir)
  }

  invisible(TRUE)
}

tccq2_module_with <- function(module, ...) {
  updates <- list(...)
  for (nm in names(updates)) {
    module[[nm]] <- updates[[nm]]
  }
  module
}
