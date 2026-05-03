# tccq_index_normalize.R - normalize vector index/view access chains for tccq
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_ir_normalized_access <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(NULL)
  }
  node$normalized_access %||% NULL
}

tccq_ir_access_plan <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(NULL)
  }

  switch(
    node$tag,
    var = if (node$type$rank > 0L) {
      list(base = node, base_name = node$name, steps = list())
    } else {
      NULL
    },
    view1 = {
      base <- tccq_ir_access_plan(node$x)
      if (is.null(base)) {
        return(NULL)
      }
      base$steps <- c(base$steps, list(list(kind = "slice", start = node$start, stop = node$stop)))
      base
    },
    slice_range = {
      base <- tccq_ir_access_plan(node$x)
      if (is.null(base)) {
        return(NULL)
      }
      base$steps <- c(base$steps, list(list(kind = "slice", start = node$start, stop = node$stop)))
      base
    },
    index = {
      if (is.null(node$index$type) || node$index$type$rank != 0L) {
        return(NULL)
      }
      base <- tccq_ir_access_plan(node$x)
      if (is.null(base)) {
        return(NULL)
      }
      base$steps <- c(base$steps, list(list(kind = "index", index = node$index)))
      base
    },
    NULL
  )
}

tccq_ir_attach_normalized_access <- function(x) {
  if (!is.list(x)) {
    return(x)
  }

  if (!is.null(x$tag)) {
    for (nm in names(x)) {
      if (nm %in% c("tag", "type", "effect", "barrier", "normalized_access")) {
        next
      }
      x[[nm]] <- tccq_ir_attach_normalized_access(x[[nm]])
    }

    if (x$tag %in% c("view1", "slice_range", "index")) {
      x$normalized_access <- tccq_ir_access_plan(x)
    }

    return(x)
  }

  lapply(x, tccq_ir_attach_normalized_access)
}

tccq_ir_access_needs_hoist <- function(node) {
  plan <- tccq_ir_normalized_access(node)
  !is.null(plan) && length(plan$steps) > 1L
}

tccq_ir_hoist_access_program <- function(node, reserved_names = character()) {
  if (!is.list(node) || is.null(node$tag) || !identical(node$tag, "program")) {
    return(node)
  }

  used_names <- tccq_unique(c(reserved_names, tccq_ir_vars(node), names(tccq_ir_program_locals(node))))
  used_c_names <- tccq_unique(vapply(used_names, tccq_c_ident, character(1)))
  counter <- 0L
  fresh_name <- function(prefix = "tccq_idx_tmp_") {
    repeat {
      counter <<- counter + 1L
      nm <- paste0(prefix, counter)
      c_nm <- tccq_c_ident(nm)
      if (!nm %in% used_names && !c_nm %in% used_c_names) {
        used_names <<- c(used_names, nm)
        used_c_names <<- c(used_c_names, c_nm)
        return(nm)
      }
    }
  }

  hoist_expr <- function(expr, allow_root_access = FALSE) {
    if (!is.list(expr) || is.null(expr$tag)) {
      return(list(stmts = list(), expr = expr))
    }

    if (expr$tag %in% c("index", "view1", "slice_range")) {
      stmts <- list()

      if (!is.null(expr$x)) {
        x <- hoist_expr(expr$x, allow_root_access = TRUE)
        expr$x <- x$expr
        if (!is.null(expr$base)) {
          expr$base <- x$expr
        }
        stmts <- c(stmts, x$stmts)
      }

      if (identical(expr$tag, "index")) {
        index <- hoist_expr(expr$index, allow_root_access = FALSE)
        expr$index <- index$expr
        stmts <- c(stmts, index$stmts)
      }

      if (expr$tag %in% c("view1", "slice_range")) {
        start <- hoist_expr(expr$start, allow_root_access = FALSE)
        stop <- hoist_expr(expr$stop, allow_root_access = FALSE)
        expr$start <- start$expr
        expr$stop <- stop$expr
        stmts <- c(stmts, start$stmts, stop$stmts)
      }

      if (!allow_root_access && tccq_ir_access_needs_hoist(expr)) {
        nm <- fresh_name()
        return(list(
          stmts = c(stmts, list(tccq_ir_bind(nm, expr))),
          expr = tccq_ir_var(nm, expr$type)
        ))
      }
      return(list(stmts = stmts, expr = expr))
    }

    switch(
      expr$tag,
      unary = {
        x <- hoist_expr(expr$x, allow_root_access = FALSE)
        expr$x <- x$expr
        list(stmts = x$stmts, expr = expr)
      },
      call1 = {
        x <- hoist_expr(expr$x, allow_root_access = FALSE)
        expr$x <- x$expr
        list(stmts = x$stmts, expr = expr)
      },
      binary = {
        lhs <- hoist_expr(expr$lhs, allow_root_access = FALSE)
        rhs <- hoist_expr(expr$rhs, allow_root_access = FALSE)
        expr$lhs <- lhs$expr
        expr$rhs <- rhs$expr
        list(stmts = c(lhs$stmts, rhs$stmts), expr = expr)
      },
      vector_fill = {
        value <- hoist_expr(expr$value, allow_root_access = FALSE)
        length <- hoist_expr(expr$length, allow_root_access = FALSE)
        expr$value <- value$expr
        expr$length <- length$expr
        list(stmts = c(value$stmts, length$stmts), expr = expr)
      },
      matrix_view = {
        stmts <- list()
        x <- hoist_expr(expr$x, allow_root_access = TRUE)
        expr$x <- x$expr
        stmts <- c(stmts, x$stmts)
        if (!identical(expr$x$tag, "var")) {
          nm <- fresh_name("tccq_matrix_tmp_")
          stmts <- c(stmts, list(tccq_ir_bind(nm, expr$x)))
          expr$x <- tccq_ir_var(nm, expr$x$type)
        }
        subscripts <- vector("list", length(expr$subscripts %||% list()))
        for (i in seq_along(expr$subscripts %||% list())) {
          sub <- hoist_store_subscript(expr$subscripts[[i]])
          stmts <- c(stmts, sub$stmts)
          subscripts[[i]] <- sub$subscript
        }
        expr$subscripts <- subscripts
        list(stmts = stmts, expr = expr)
      },
      index2 = {
        stmts <- list()
        x <- hoist_expr(expr$x, allow_root_access = TRUE)
        expr$x <- x$expr
        stmts <- c(stmts, x$stmts)
        if (!identical(expr$x$tag, "var")) {
          nm <- fresh_name("tccq_matrix_tmp_")
          stmts <- c(stmts, list(tccq_ir_bind(nm, expr$x)))
          expr$x <- tccq_ir_var(nm, expr$x$type)
        }
        row <- hoist_expr(expr$row, allow_root_access = FALSE)
        col <- hoist_expr(expr$col, allow_root_access = FALSE)
        expr$row <- row$expr
        expr$col <- col$expr
        list(stmts = c(stmts, row$stmts, col$stmts), expr = expr)
      },
      matrix_fill = {
        value <- hoist_expr(expr$value, allow_root_access = FALSE)
        nrow <- hoist_expr(expr$nrow, allow_root_access = FALSE)
        ncol <- hoist_expr(expr$ncol, allow_root_access = FALSE)
        expr$value <- value$expr
        expr$nrow <- nrow$expr
        expr$ncol <- ncol$expr
        list(stmts = c(value$stmts, nrow$stmts, ncol$stmts), expr = expr)
      },
      reduce = {
        x <- hoist_expr(expr$x, allow_root_access = FALSE)
        expr$x <- x$expr
        if (!allow_root_access && !is.null(expr$type) && expr$type$rank == 0L) {
          nm <- fresh_name("tccq_reduce_tmp_")
          return(list(
            stmts = c(x$stmts, list(tccq_ir_bind(nm, expr))),
            expr = tccq_ir_var(nm, expr$type)
          ))
        }
        list(stmts = x$stmts, expr = expr)
      },
      arg_reduce = {
        x <- hoist_expr(expr$x, allow_root_access = FALSE)
        expr$x <- x$expr
        if (!allow_root_access && !is.null(expr$type) && expr$type$rank == 0L) {
          nm <- fresh_name("tccq_arg_reduce_tmp_")
          return(list(
            stmts = c(x$stmts, list(tccq_ir_bind(nm, expr))),
            expr = tccq_ir_var(nm, expr$type)
          ))
        }
        list(stmts = x$stmts, expr = expr)
      },
      len = {
        if (is.list(expr$x) && identical(expr$x$tag, "boundary_call")) {
          stmts <- list()
          args <- vector("list", length(expr$x$args))
          for (i in seq_along(expr$x$args)) {
            arg <- hoist_expr(expr$x$args[[i]], allow_root_access = FALSE)
            stmts <- c(stmts, arg$stmts)
            args[[i]] <- arg$expr
          }
          expr$x$args <- args
          x <- list(stmts = stmts, expr = expr$x)
        } else {
          x <- hoist_expr(expr$x, allow_root_access = FALSE)
        }
        expr$x <- x$expr
        if (!identical(expr$x$tag, "var")) {
          nm <- fresh_name("tccq_len_tmp_")
          return(list(
            stmts = c(x$stmts, list(tccq_ir_bind(nm, expr))),
            expr = tccq_ir_var(nm, expr$type)
          ))
        }
        list(stmts = x$stmts, expr = expr)
      },
      boundary_call = {
        stmts <- list()
        args <- vector("list", length(expr$args))
        for (i in seq_along(expr$args)) {
          arg <- hoist_expr(expr$args[[i]], allow_root_access = FALSE)
          stmts <- c(stmts, arg$stmts)
          args[[i]] <- arg$expr
        }
        expr$args <- args
        if (!allow_root_access && !is.null(expr$type) && expr$type$rank == 0L) {
          nm <- fresh_name("tccq_boundary_tmp_")
          return(list(
            stmts = c(stmts, list(tccq_ir_bind(nm, expr))),
            expr = tccq_ir_var(nm, expr$type)
          ))
        }
        list(stmts = stmts, expr = expr)
      },
      list(stmts = list(), expr = expr)
    )
  }

  hoist_store_subscript <- function(sub) {
    if (identical(sub$kind, "index")) {
      index <- hoist_expr(sub$index, allow_root_access = FALSE)
      sub$index <- index$expr
      return(list(stmts = index$stmts, subscript = sub))
    }
    if (identical(sub$kind, "range")) {
      start <- hoist_expr(sub$start, allow_root_access = FALSE)
      stop <- hoist_expr(sub$stop, allow_root_access = FALSE)
      sub$start <- start$expr
      sub$stop <- stop$expr
      return(list(stmts = c(start$stmts, stop$stmts), subscript = sub))
    }
    if (identical(sub$kind, "all")) {
      return(list(stmts = list(), subscript = sub))
    }
    tccq_abort("unsupported assignment subscript kind: ", sub$kind)
  }

  out_stmts <- list()
  for (stmt in node$stmts %||% list()) {
    if (identical(stmt$tag, "bind")) {
      value <- hoist_expr(stmt$value, allow_root_access = TRUE)
      stmt$value <- value$expr
      stmt$type <- value$expr$type
      out_stmts <- c(out_stmts, value$stmts, list(stmt))
    } else if (identical(stmt$tag, "store_index")) {
      value <- hoist_expr(stmt$value, allow_root_access = FALSE)
      access <- if (!is.null(stmt$access)) hoist_expr(stmt$access, allow_root_access = TRUE) else list(stmts = list(), expr = NULL)
      index <- if (is.null(stmt$access)) hoist_expr(stmt$index, allow_root_access = FALSE) else list(stmts = list(), expr = NULL)
      stmt$access <- access$expr
      stmt$index <- index$expr
      stmt$value <- value$expr
      out_stmts <- c(out_stmts, value$stmts, access$stmts, index$stmts, list(stmt))
    } else if (identical(stmt$tag, "store_range")) {
      value <- hoist_expr(stmt$value, allow_root_access = FALSE)
      access <- if (!is.null(stmt$access)) hoist_expr(stmt$access, allow_root_access = TRUE) else list(stmts = list(), expr = NULL)
      start <- if (is.null(stmt$access)) hoist_expr(stmt$start, allow_root_access = FALSE) else list(stmts = list(), expr = NULL)
      stop <- if (is.null(stmt$access)) hoist_expr(stmt$stop, allow_root_access = FALSE) else list(stmts = list(), expr = NULL)
      stmt$access <- access$expr
      stmt$start <- start$expr
      stmt$stop <- stop$expr
      stmt$value <- value$expr
      out_stmts <- c(out_stmts, value$stmts, access$stmts, start$stmts, stop$stmts, list(stmt))
    } else if (identical(stmt$tag, "store_index2")) {
      value <- hoist_expr(stmt$value, allow_root_access = FALSE)
      row <- hoist_expr(stmt$row, allow_root_access = FALSE)
      col <- hoist_expr(stmt$col, allow_root_access = FALSE)
      stmt$row <- row$expr
      stmt$col <- col$expr
      stmt$value <- value$expr
      out_stmts <- c(out_stmts, value$stmts, row$stmts, col$stmts, list(stmt))
    } else if (identical(stmt$tag, "store_access")) {
      value <- hoist_expr(stmt$value, allow_root_access = FALSE)
      access <- if (!is.null(stmt$access)) hoist_expr(stmt$access, allow_root_access = TRUE) else list(stmts = list(), expr = NULL)
      stmts <- access$stmts
      stmt$access <- access$expr
      subscripts <- vector("list", length(stmt$subscripts %||% list()))
      for (i in seq_along(stmt$subscripts %||% list())) {
        sub <- hoist_store_subscript(stmt$subscripts[[i]])
        stmts <- c(stmts, sub$stmts)
        subscripts[[i]] <- sub$subscript
      }
      stmt$subscripts <- subscripts
      stmt$value <- value$expr
      out_stmts <- c(out_stmts, value$stmts, stmts, list(stmt))
    } else if (identical(stmt$tag, "for_loop")) {
      start <- hoist_expr(stmt$start, allow_root_access = FALSE)
      stop <- hoist_expr(stmt$stop, allow_root_access = FALSE)
      stmt$start <- start$expr
      stmt$stop <- stop$expr
      body_prog <- tccq_ir_program(stmt$body %||% list(), tccq_ir_const(0L, tccq_type_scalar("integer")))
      body_prog <- tccq_ir_hoist_access_program(body_prog, reserved_names = used_names)
      stmt$body <- body_prog$stmts %||% list()
      out_stmts <- c(out_stmts, start$stmts, stop$stmts, list(stmt))
    } else {
      out_stmts <- c(out_stmts, list(stmt))
    }
  }

  result <- hoist_expr(node$result, allow_root_access = TRUE)
  node$stmts <- c(out_stmts, result$stmts)
  node$result <- result$expr
  node$type <- result$expr$type
  node
}

tccq_ir_view_source_name <- function(node, bindings = NULL) {
  if (!tccq_is_view_node(node)) {
    return(NA_character_)
  }

  plan <- tccq_ir_normalized_access(node)
  base_name <- plan$base_name %||% if (identical(node$x$tag, "var")) node$x$name else NA_character_

  if (is.null(bindings)) {
    return(base_name)
  }

  while (!is.na(base_name) && base_name %in% names(bindings) && bindings[[base_name]]$kind %in% c("alias", "view") && !isTRUE(bindings[[base_name]]$materialize_on_write)) {
    next_name <- bindings[[base_name]]$source %||% NA_character_
    if (identical(next_name, base_name) || is.na(next_name)) {
      break
    }
    base_name <- next_name
  }

  base_name
}
