# tccq_allocation.R - conservative allocation and output-reuse planning
# SPDX-License-Identifier: GPL-3.0-or-later

# A final vector expression can be written back into an owned local only when the
# expression is strictly pointwise over that same local.  This deliberately
# excludes indexed reads, slices, reducers, and boundary calls; those can observe
# non-current elements or cross semantic barriers, so they need a fresh result
# until the middle-end has a richer dependence model.  Scalar operands are safe:
# they have already been copied into C locals and cannot alias vector storage.
tccq_allocation_expr_pointwise_over <- function(expr, name) {
  if (!is.list(expr) || is.null(expr$tag)) {
    return(TRUE)
  }

  switch(
    expr$tag,
    const = TRUE,
    var = {
      if (!is.null(expr$type) && expr$type$rank > 0L) {
        identical(expr$name, name)
      } else {
        TRUE
      }
    },
    unary = tccq_allocation_expr_pointwise_over(expr$x, name),
    call1 = tccq_allocation_expr_pointwise_over(expr$x, name),
    binary = tccq_allocation_expr_pointwise_over(expr$lhs, name) &&
      tccq_allocation_expr_pointwise_over(expr$rhs, name),
    len = TRUE,
    index = FALSE,
    slice_range = FALSE,
    view1 = FALSE,
    reduce = FALSE,
    boundary_call = FALSE,
    FALSE
  )
}

tccq_allocation_result_expr <- function(module) {
  kernel <- module$kernel
  if (is.null(kernel)) {
    return(NULL)
  }
  result_kernel <- if (identical(kernel$tag, "kernel_program")) kernel$result_kernel else kernel
  if (is.null(result_kernel) || !identical(result_kernel$tag, "materialize")) {
    return(NULL)
  }
  result_kernel$producer$elem %||% NULL
}

tccq_allocation_stmt_use_vars <- function(stmt) {
  if (!is.list(stmt) || is.null(stmt$tag)) {
    return(character())
  }

  switch(
    stmt$tag,
    bind = tccq_ir_vars(stmt$value),
    store_index = tccq_unique(c(stmt$name, tccq_ir_vars(stmt$index), tccq_ir_vars(stmt$value))),
    store_range = tccq_unique(c(stmt$name, tccq_ir_vars(stmt$start), tccq_ir_vars(stmt$stop), tccq_ir_vars(stmt$value))),
    character()
  )
}

tccq_allocation_uses_after <- function(ir) {
  if (is.null(ir) || !identical(ir$tag, "program") || !length(ir$stmts)) {
    return(list())
  }

  stmts <- ir$stmts
  uses_after <- vector("list", length(stmts))
  future_uses <- tccq_ir_vars(ir$result)
  for (i in rev(seq_along(stmts))) {
    uses_after[[i]] <- future_uses
    future_uses <- tccq_unique(c(tccq_allocation_stmt_use_vars(stmts[[i]]), future_uses))
  }
  uses_after
}

tccq_allocation_binding_depends_on <- function(bindings, name, target) {
  seen <- character()
  cur <- name
  repeat {
    if (cur %in% seen) {
      return(FALSE)
    }
    seen <- c(seen, cur)
    binding <- bindings[[cur]] %||% NULL
    source <- binding$source %||% NA_character_
    if (is.na(source) || is.null(source)) {
      return(FALSE)
    }
    if (identical(source, target)) {
      return(TRUE)
    }
    if (!source %in% names(bindings)) {
      return(FALSE)
    }
    cur <- source
  }
}

tccq_allocation_has_live_dependent <- function(bindings, target, live_names) {
  for (name in live_names) {
    if (tccq_allocation_binding_depends_on(bindings, name, target)) {
      return(TRUE)
    }
  }
  FALSE
}

tccq_allocation_has_live_current_dependent <- function(bindings, target, live_names, generation) {
  target_generation <- generation[[target]] %||% NA_integer_
  for (name in live_names) {
    if (!tccq_allocation_binding_depends_on(bindings, name, target)) {
      next
    }
    if (identical(generation[[name]] %||% NA_integer_, target_generation)) {
      return(TRUE)
    }
  }
  FALSE
}

tccq_allocation_state_before <- function(module, stmt_index) {
  if (stmt_index <= 1L) {
    return(list(owned = list(), generation = list()))
  }
  module$storage_plan$state_after[[stmt_index - 1L]] %||% list(owned = list(), generation = list())
}

tccq_allocation_final_state <- function(module) {
  module$storage_plan$final_state %||% list(owned = list(), generation = list())
}

tccq_allocation_expr_escape_vars <- function(expr, bindings, boundary_args = NULL) {
  tccq_storage_expr_boundary_escape_vars(expr, bindings, boundary_args = boundary_args)
}

tccq_allocation_stmt_escape_vars <- function(stmt, bindings, boundary_args = NULL) {
  if (!is.list(stmt) || is.null(stmt$tag)) {
    return(character())
  }

  switch(
    stmt$tag,
    bind = tccq_allocation_expr_escape_vars(stmt$value, bindings, boundary_args = boundary_args),
    store_index = tccq_unique(c(
      tccq_allocation_expr_escape_vars(stmt$index, bindings, boundary_args = boundary_args),
      tccq_allocation_expr_escape_vars(stmt$value, bindings, boundary_args = boundary_args)
    )),
    store_range = tccq_unique(c(
      tccq_allocation_expr_escape_vars(stmt$start, bindings, boundary_args = boundary_args),
      tccq_allocation_expr_escape_vars(stmt$stop, bindings, boundary_args = boundary_args),
      tccq_allocation_expr_escape_vars(stmt$value, bindings, boundary_args = boundary_args)
    )),
    character()
  )
}

tccq_allocation_program_escape_vars <- function(ir, bindings, boundary_args = NULL, write_barriers = NULL) {
  if (is.null(ir) || !identical(ir$tag, "program")) {
    return(character())
  }

  escaped <- character()
  for (i in seq_along(ir$stmts %||% list())) {
    stmt <- ir$stmts[[i]]
    escaped <- tccq_unique(c(
      escaped,
      tccq_allocation_stmt_escape_vars(stmt, bindings, boundary_args = boundary_args)
    ))
    barrier <- write_barriers[[as.character(i)]] %||% NULL
    if (isTRUE(barrier$copy_target)) {
      escaped <- setdiff(escaped, barrier$target)
    }
  }
  tccq_unique(c(escaped, tccq_allocation_expr_escape_vars(ir$result, bindings, boundary_args = boundary_args)))
}

tccq_allocation_bind_reuse_plan <- function(module) {
  ir <- module$ir
  if (is.null(ir) || !identical(ir$tag, "program") || !length(ir$stmts)) {
    return(list())
  }

  bindings <- module$storage_plan$bindings %||% list()
  boundary_args <- module$storage_plan$boundary_args %||% list()
  uses_after <- tccq_allocation_uses_after(ir)
  escaped_before <- character()
  out <- list()

  for (i in seq_along(ir$stmts)) {
    stmt <- ir$stmts[[i]]
    if (identical(stmt$tag, "bind") && !is.null(stmt$type) && stmt$type$rank == 1L) {
      target_binding <- bindings[[stmt$name]] %||% NULL
      vector_vars <- tccq_ir_vector_vars(stmt$value)

      if (!is.null(target_binding) && identical(target_binding$kind, "owned") && length(vector_vars) == 1L) {
        source_name <- vector_vars[[1L]]
        source_binding <- bindings[[source_name]] %||% NULL
        source_state <- tccq_allocation_state_before(module, i)
        source_owned <- isTRUE(source_state$owned[[source_name]])
        live_after <- uses_after[[i]] %||% character()

        if (!identical(source_name, stmt$name) &&
            !is.null(source_binding) && source_owned &&
            identical(source_binding$type$mode, stmt$type$mode) && source_binding$type$rank == 1L &&
            tccq_allocation_expr_pointwise_over(stmt$value, source_name) &&
            !source_name %in% live_after &&
            !source_name %in% escaped_before &&
            !tccq_allocation_has_live_current_dependent(bindings, source_name, live_after, source_state$generation)) {
          out[[stmt$name]] <- list(
            strategy = "reuse_owned_local_bind",
            name = stmt$name,
            source = source_name,
            stmt_index = i,
            mode = stmt$type$mode,
            reason = "dead_owned_unescaped_source_pointwise_bind"
          )
        }
      }
    }

    escaped_before <- tccq_unique(c(
      escaped_before,
      tccq_allocation_stmt_escape_vars(stmt, bindings, boundary_args = boundary_args)
    ))
    barrier <- module$storage_plan$write_barriers[[as.character(i)]] %||% NULL
    if (isTRUE(barrier$copy_target)) {
      escaped_before <- setdiff(escaped_before, barrier$target)
    }
  }

  out
}

tccq_allocation_result_reuse_plan <- function(module) {
  expr <- tccq_allocation_result_expr(module)
  if (is.null(expr) || is.null(expr$type) || expr$type$rank != 1L) {
    return(NULL)
  }
  if (expr$tag %in% c("var", "boundary_call", "slice_range", "view1")) {
    return(NULL)
  }

  vector_vars <- tccq_ir_vector_vars(expr)
  if (length(vector_vars) != 1L) {
    return(NULL)
  }

  name <- vector_vars[[1L]]
  bindings <- module$storage_plan$bindings %||% list()
  binding <- bindings[[name]] %||% NULL
  final_state <- tccq_allocation_final_state(module)
  if (is.null(binding) || !isTRUE(final_state$owned[[name]])) {
    return(NULL)
  }
  if (!identical(binding$type$mode, expr$type$mode) || binding$type$rank != 1L) {
    return(NULL)
  }
  if (!tccq_allocation_expr_pointwise_over(expr, name)) {
    return(NULL)
  }

  escaped <- tccq_allocation_program_escape_vars(
    module$ir,
    bindings,
    boundary_args = module$storage_plan$boundary_args %||% list(),
    write_barriers = module$storage_plan$write_barriers %||% list()
  )
  if (name %in% escaped) {
    return(NULL)
  }

  list(
    strategy = "reuse_owned_local_result",
    name = name,
    mode = expr$type$mode,
    reason = "pointwise_final_expression"
  )
}

tccq_allocation_output_plan <- function(kernel) {
  switch(
    kernel$tag,
    scalar_kernel = list(kind = "scalar_result", protect = TRUE),
    materialize = list(kind = "vector_result", protect = TRUE),
    fold = list(kind = "scalar_result", protect = TRUE),
    kernel_program = list(kind = "program_result", protect = TRUE),
    tccq_abort("unknown kernel tag for allocation plan: ", kernel$tag)
  )
}

tccq_allocation_plan <- function(module) {
  kernel <- module$kernel
  list(
    output = tccq_allocation_output_plan(kernel),
    temporaries = list(),
    reuse = list(
      bindings = tccq_allocation_bind_reuse_plan(module),
      result_buffer = tccq_allocation_result_reuse_plan(module)
    )
  )
}
