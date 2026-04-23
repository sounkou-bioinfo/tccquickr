# tccq_storage.R - conservative storage and return planning helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_storage_binding <- function(name, type, kind = "owned", source = NULL, mutated = FALSE, contiguous = TRUE) {
  list(
    name = name,
    type = type,
    kind = kind,
    source = source,
    mutated = isTRUE(mutated),
    contiguous = isTRUE(contiguous),
    materialize_on_write = isTRUE(mutated) && kind %in% c("alias", "view"),
    materialize_on_return = !isTRUE(mutated) && kind %in% c("alias", "view"),
    materialize_on_boundary = kind %in% c("alias", "view")
  )
}

tccq_storage_result_plan <- function(module, bindings) {
  kernel <- module$kernel
  if (is.null(kernel)) {
    return(list(kind = "none", strategy = "none"))
  }

  result_kernel <- if (identical(kernel$tag, "kernel_program")) kernel$result_kernel else kernel
  if (is.null(result_kernel)) {
    return(list(kind = "none", strategy = "none"))
  }

  switch(
    result_kernel$tag,
    scalar_kernel = list(kind = "scalar", strategy = "box_scalar"),
    fold = list(kind = "scalar", strategy = "box_scalar"),
    materialize = {
      expr <- result_kernel$producer$elem
      if (identical(expr$tag, "boundary_call")) {
        return(list(kind = "boundary", strategy = "return_boundary_result"))
      }
      if (identical(expr$tag, "var") && expr$name %in% names(bindings)) {
        binding <- bindings[[expr$name]]
        strategy <- switch(
          binding$kind,
          owned = "return_owned_local",
          alias = if (isTRUE(binding$materialize_on_write)) "copy_on_write_return_local" else "copy_on_return",
          view = if (isTRUE(binding$materialize_on_write)) "copy_on_write_return_local" else "copy_on_return",
          "fresh_vector"
        )
        return(list(
          kind = "vector",
          strategy = strategy,
          name = expr$name,
          source = binding$source
        ))
      }
      if (identical(expr$tag, "view1")) {
        return(list(kind = "vector", strategy = "materialize_view"))
      }
      list(kind = "vector", strategy = "fresh_vector")
    },
    list(kind = "unknown", strategy = "unknown")
  )
}

tccq_storage_plan <- function(module) {
  ir <- module$ir
  locals <- if (!is.null(ir)) tccq_ir_program_locals(ir) else list()
  mutated <- if (!is.null(ir)) tccq_ir_program_mutated_names(ir) else character()

  bindings <- lapply(names(locals), function(nm) {
    tccq_storage_binding(nm, locals[[nm]], kind = "owned", mutated = nm %in% mutated)
  })
  names(bindings) <- names(locals)

  if (!is.null(ir) && identical(ir$tag, "program") && length(ir$stmts)) {
    for (stmt in ir$stmts) {
      if (!identical(stmt$tag, "bind") || !is.list(stmt$value) || is.null(stmt$value$tag)) {
        next
      }

      if (identical(stmt$value$tag, "var") && stmt$value$type$rank > 0L) {
        bindings[[stmt$name]] <- tccq_storage_binding(
          stmt$name,
          locals[[stmt$name]],
          kind = "alias",
          source = stmt$value$name,
          mutated = stmt$name %in% mutated
        )
      } else if (tccq_is_view_node(stmt$value)) {
        bindings[[stmt$name]] <- tccq_storage_binding(
          stmt$name,
          locals[[stmt$name]],
          kind = "view",
          source = tccq_ir_view_source_name(stmt$value, bindings = bindings),
          mutated = stmt$name %in% mutated,
          contiguous = TRUE
        )
      }
    }
  }

  aliases <- bindings[vapply(bindings, function(x) x$kind %in% c("alias", "view"), logical(1))]
  result <- tccq_storage_result_plan(module, bindings)

  list(
    locals = locals,
    bindings = bindings,
    aliases = aliases,
    mutated = mutated,
    direct_return = identical(result$strategy, "return_owned_local"),
    result = result,
    views = names(Filter(function(x) identical(x$kind, "view"), bindings))
  )
}
