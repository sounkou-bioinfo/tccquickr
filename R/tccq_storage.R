# tccq_storage.R - conservative storage and return planning helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_storage_binding <- function(name, type, kind = "owned", source = NULL, mutated = FALSE, contiguous = TRUE, domain_id = NULL) {
  list(
    name = name,
    type = type,
    kind = kind,
    source = source,
    domain_id = domain_id,
    mutated = isTRUE(mutated),
    contiguous = isTRUE(contiguous),
    materialize_on_write = isTRUE(mutated) && kind %in% c("alias", "view"),
    materialize_on_return = !isTRUE(mutated) && kind %in% c("alias", "view"),
    materialize_on_boundary = kind %in% c("alias", "view")
  )
}

tccq_storage_stmt_read_vars <- function(stmt) {
  if (!is.list(stmt) || is.null(stmt$tag)) {
    return(character())
  }

  switch(
    stmt$tag,
    bind = tccq_ir_vars(stmt$value),
    store_index = tccq_unique(c(tccq_ir_vars(stmt$index), tccq_ir_vars(stmt$value))),
    store_range = tccq_unique(c(tccq_ir_vars(stmt$start), tccq_ir_vars(stmt$stop), tccq_ir_vars(stmt$value))),
    character()
  )
}

tccq_storage_write_barrier_plan <- function(ir, bindings) {
  if (is.null(ir) || !identical(ir$tag, "program") || !length(ir$stmts)) {
    return(list())
  }

  stmts <- ir$stmts
  bind_index <- list()
  for (i in seq_along(stmts)) {
    if (identical(stmts[[i]]$tag, "bind")) {
      bind_index[[stmts[[i]]$name]] <- i
    }
  }

  reads_after <- vector("list", length(stmts))
  future_reads <- tccq_ir_vars(ir$result)
  for (i in rev(seq_along(stmts))) {
    reads_after[[i]] <- future_reads
    future_reads <- tccq_unique(c(tccq_storage_stmt_read_vars(stmts[[i]]), future_reads))
  }

  local_names <- names(bindings)
  current_owned <- stats::setNames(lapply(bindings, function(x) identical(x$kind, "owned")), local_names)
  current_generation <- stats::setNames(as.list(rep(0L, length(local_names))), local_names)
  borrowed_generation <- list()
  out <- list()

  source_generation <- function(source) {
    if (is.null(source) || is.na(source) || !source %in% names(current_generation)) {
      return(0L)
    }
    current_generation[[source]] %||% 0L
  }

  depends_on <- function(name, target) {
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

  bind_generation_source <- function(stmt, binding) {
    if (identical(binding$kind, "view")) {
      plan <- tccq_ir_normalized_access(stmt$value)
      base_name <- plan$base_name %||% NULL
      if (!is.null(base_name)) {
        return(base_name)
      }
    }
    binding$source %||% NULL
  }

  for (i in seq_along(stmts)) {
    stmt <- stmts[[i]]

    if (identical(stmt$tag, "bind")) {
      binding <- bindings[[stmt$name]] %||% NULL
      if (!is.null(binding)) {
        if (identical(binding$kind, "owned")) {
          current_owned[[stmt$name]] <- TRUE
          current_generation[[stmt$name]] <- 0L
        } else if (binding$kind %in% c("alias", "view")) {
          current_owned[[stmt$name]] <- FALSE
          current_generation[[stmt$name]] <- source_generation(bind_generation_source(stmt, binding))
          borrowed_generation[[stmt$name]] <- current_generation[[stmt$name]]
        }
      }
      next
    }

    if (!stmt$tag %in% c("store_index", "store_range")) {
      next
    }

    target <- stmt$name
    target_owned <- isTRUE(current_owned[[target]])
    target_generation <- current_generation[[target]] %||% 0L
    needed_reads <- tccq_unique(c(tccq_storage_stmt_read_vars(stmt), reads_after[[i]]))
    materialize <- character()

    if (isTRUE(target_owned)) {
      for (borrowed_name in names(borrowed_generation)) {
        if (!borrowed_name %in% needed_reads) {
          next
        }
        if (is.null(bind_index[[borrowed_name]]) || bind_index[[borrowed_name]] >= i) {
          next
        }
        if (depends_on(borrowed_name, target) && identical(borrowed_generation[[borrowed_name]], target_generation)) {
          materialize <- c(materialize, borrowed_name)
        }
      }
    }

    materialize <- tccq_unique(materialize)
    out[[as.character(i)]] <- list(
      stmt_index = i,
      target = target,
      target_owned_before_write = target_owned,
      target_generation = target_generation,
      materialize_views = materialize
    )

    for (borrowed_name in materialize) {
      current_owned[[borrowed_name]] <- TRUE
      current_generation[[borrowed_name]] <- (current_generation[[borrowed_name]] %||% 0L) + 1L
      borrowed_generation[[borrowed_name]] <- NULL
    }

    if (!isTRUE(target_owned)) {
      current_owned[[target]] <- TRUE
      current_generation[[target]] <- target_generation + 1L
      borrowed_generation[[target]] <- NULL
    }
  }

  out
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

tccq_storage_alias_formal_source <- function(name, bindings, formal_names) {
  seen <- character()
  cur <- name

  repeat {
    if (cur %in% seen) {
      return(NULL)
    }
    seen <- c(seen, cur)

    binding <- bindings[[cur]] %||% NULL
    if (is.null(binding) || !identical(binding$kind, "alias") || isTRUE(binding$mutated)) {
      return(NULL)
    }

    source <- binding$source %||% NULL
    if (is.null(source) || is.na(source)) {
      return(NULL)
    }
    if (source %in% formal_names) {
      return(source)
    }
    if (!source %in% names(bindings)) {
      return(NULL)
    }
    cur <- source
  }
}

tccq_storage_boundary_arg_plan <- function(arg, arg_index, boundary, bindings, formal_names) {
  boundary_id <- boundary$boundary_id %||% NA_integer_
  materialize_args <- boundary$materialize_args %||% logical()
  materialize <- length(materialize_args) >= arg_index && isTRUE(materialize_args[[arg_index]])

  if (!is.list(arg) || is.null(arg$tag) || !identical(arg$tag, "var")) {
    return(list(
      boundary_id = boundary_id,
      arg_index = arg_index,
      name = NULL,
      strategy = if (is.list(arg) && !is.null(arg$type) && arg$type$rank == 0L) "box_scalar" else "unsupported_vector_expr",
      materialize_required = materialize
    ))
  }

  if (is.null(arg$type) || arg$type$rank == 0L) {
    return(list(
      boundary_id = boundary_id,
      arg_index = arg_index,
      name = arg$name,
      strategy = if (arg$name %in% formal_names) "pass_formal_scalar" else "box_scalar",
      materialize_required = materialize
    ))
  }

  if (arg$name %in% formal_names) {
    return(list(
      boundary_id = boundary_id,
      arg_index = arg_index,
      name = arg$name,
      strategy = "pass_formal",
      sexp = arg$name,
      materialize_required = materialize
    ))
  }

  binding <- bindings[[arg$name]] %||% NULL
  if (is.null(binding)) {
    return(list(
      boundary_id = boundary_id,
      arg_index = arg_index,
      name = arg$name,
      strategy = "materialize_local",
      sexp = arg$name,
      materialize_required = materialize
    ))
  }

  formal_source <- tccq_storage_alias_formal_source(arg$name, bindings, formal_names)
  if (!is.null(formal_source)) {
    return(list(
      boundary_id = boundary_id,
      arg_index = arg_index,
      name = arg$name,
      strategy = "pass_source",
      source = formal_source,
      sexp = formal_source,
      materialize_required = materialize
    ))
  }

  if (identical(binding$kind, "owned")) {
    return(list(
      boundary_id = boundary_id,
      arg_index = arg_index,
      name = arg$name,
      strategy = "pass_local",
      sexp = arg$name,
      materialize_required = materialize
    ))
  }

  list(
    boundary_id = boundary_id,
    arg_index = arg_index,
    name = arg$name,
    strategy = "materialize_local",
    sexp = arg$name,
    materialize_required = materialize
  )
}

tccq_storage_boundary_arg_plans <- function(module, bindings) {
  boundaries <- tccq_boundary_nodes(module$kernel)
  if (!length(boundaries)) {
    return(list())
  }

  out <- list()
  for (boundary in boundaries) {
    boundary_id <- boundary$boundary_id %||% (length(out) + 1L)
    args <- boundary$args %||% list()
    plans <- vector("list", length(args))
    for (i in seq_along(args)) {
      plans[[i]] <- tccq_storage_boundary_arg_plan(
        args[[i]],
        arg_index = i,
        boundary = boundary,
        bindings = bindings,
        formal_names = module$formal_names
      )
    }
    out[[as.character(boundary_id)]] <- plans
  }
  out
}

tccq_storage_plan <- function(module) {
  ir <- module$ir
  locals <- if (!is.null(ir)) tccq_ir_program_locals(ir) else list()
  mutated <- if (!is.null(ir)) tccq_ir_program_mutated_names(ir) else character()

  shape_by_name <- module$shape_facts$by_name %||% list()

  bindings <- lapply(names(locals), function(nm) {
    tccq_storage_binding(
      nm,
      locals[[nm]],
      kind = "owned",
      mutated = nm %in% mutated,
      domain_id = shape_by_name[[nm]] %||% NULL
    )
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
          mutated = stmt$name %in% mutated,
          domain_id = shape_by_name[[stmt$name]] %||% NULL
        )
      } else if (tccq_is_view_node(stmt$value)) {
        bindings[[stmt$name]] <- tccq_storage_binding(
          stmt$name,
          locals[[stmt$name]],
          kind = "view",
          source = tccq_ir_view_source_name(stmt$value, bindings = bindings),
          mutated = stmt$name %in% mutated,
          contiguous = TRUE,
          domain_id = shape_by_name[[stmt$name]] %||% NULL
        )
      }
    }
  }

  aliases <- bindings[vapply(bindings, function(x) x$kind %in% c("alias", "view"), logical(1))]
  write_barriers <- tccq_storage_write_barrier_plan(ir, bindings)
  boundary_args <- tccq_storage_boundary_arg_plans(module, bindings)
  result <- tccq_storage_result_plan(module, bindings)

  list(
    locals = locals,
    bindings = bindings,
    aliases = aliases,
    mutated = mutated,
    direct_return = identical(result$strategy, "return_owned_local"),
    result = result,
    views = names(Filter(function(x) identical(x$kind, "view"), bindings)),
    write_barriers = write_barriers,
    boundary_args = boundary_args
  )
}
