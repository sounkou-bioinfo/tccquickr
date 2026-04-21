# tccq_storage.R - conservative storage planning helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_storage_plan <- function(module) {
  ir <- module$ir
  locals <- if (!is.null(ir)) tccq_ir_program_locals(ir) else list()
  mutated <- if (!is.null(ir)) tccq_ir_program_mutated_names(ir) else character()

  aliases <- list()
  if (!is.null(ir) && identical(ir$tag, "program") && length(ir$stmts)) {
    for (stmt in ir$stmts) {
      if (identical(stmt$tag, "bind") && is.list(stmt$value) && !is.null(stmt$value$tag)) {
        if (identical(stmt$value$tag, "var") && stmt$value$type$rank > 0L) {
          aliases[[stmt$name]] <- list(kind = "alias", source = stmt$value$name)
        } else if (tccq_is_view_node(stmt$value)) {
          base_name <- if (identical(stmt$value$x$tag, "var")) stmt$value$x$name else NA_character_
          aliases[[stmt$name]] <- list(kind = "view", source = base_name)
        }
      }
    }
  }

  direct_return <- FALSE
  if (!is.null(module$kernel) && identical(module$kernel$tag, "kernel_program")) {
    rk <- module$kernel$result_kernel
    if (identical(rk$tag, "materialize") && identical(rk$producer$elem$tag, "var")) {
      nm <- rk$producer$elem$name
      direct_return <- nm %in% names(locals) && is.null(aliases[[nm]])
    }
  }

  list(
    locals = locals,
    aliases = aliases,
    mutated = mutated,
    direct_return = direct_return,
    views = names(Filter(function(x) identical(x$kind, "view"), aliases))
  )
}
