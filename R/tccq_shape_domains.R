# tccq_shape_domains.R - explicit 1D shape/domain facts for tccq
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_ir_shape_domain <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(NULL)
  }
  node$shape_domain %||% NULL
}

tccq_shape_domain <- function(
  id,
  kind,
  names = character(),
  witness_names = character(),
  parent = NULL,
  root_name = NULL,
  members = character(),
  access = NULL,
  reason = NULL
) {
  list(
    id = id,
    kind = kind,
    names = tccq_unique(as.character(names)),
    witness_names = tccq_unique(as.character(witness_names)),
    parent = parent,
    root_name = root_name,
    members = tccq_unique(as.character(members)),
    access = access,
    reason = reason
  )
}

tccq_shape_facts_domain <- function(shape_facts, id) {
  if (is.null(shape_facts) || is.null(id) || !nzchar(id)) {
    return(NULL)
  }
  shape_facts$domains[[id]] %||% NULL
}

tccq_shape_domain_kind <- function(shape_facts, id) {
  rec <- tccq_shape_facts_domain(shape_facts, id)
  rec$kind %||% NULL
}

tccq_shape_domain_witness_names <- function(shape_facts, id) {
  rec <- tccq_shape_facts_domain(shape_facts, id)
  rec$witness_names %||% character()
}

tccq_shape_domain_root_name <- function(shape_facts, id) {
  rec <- tccq_shape_facts_domain(shape_facts, id)
  rec$root_name %||% NULL
}

tccq_shape_domains_equivalent <- function(a, b, shape_facts = NULL) {
  if (!is.null(a) && !is.null(b)) {
    return(identical(a, b))
  }
  FALSE
}

tccq_shape_analyze_module <- function(module) {
  counter <- 0L
  domains <- list()
  join_cache <- list()
  by_name <- list()
  env <- list()

  fresh_domain_id <- function(prefix = "d") {
    counter <<- counter + 1L
    paste0(prefix, counter)
  }

  register_domain <- function(record) {
    domains[[record$id]] <<- record
    record$id
  }

  domain_record <- function(id) {
    domains[[id]] %||% NULL
  }

  update_domain <- function(id, field, value) {
    rec <- domain_record(id)
    if (is.null(rec)) {
      return(invisible(NULL))
    }
    rec[[field]] <- value
    domains[[id]] <<- rec
    invisible(NULL)
  }

  append_domain_names <- function(id, names, witness = TRUE) {
    rec <- domain_record(id)
    if (is.null(rec) || !length(names)) {
      return(invisible(NULL))
    }

    rec$names <- tccq_unique(c(rec$names %||% character(), as.character(names)))
    if (isTRUE(witness)) {
      rec$witness_names <- tccq_unique(c(rec$witness_names %||% character(), as.character(names)))
    }
    domains[[id]] <<- rec
    invisible(NULL)
  }

  new_full_domain <- function(name) {
    id <- fresh_domain_id("dom_full_")
    register_domain(tccq_shape_domain(
      id = id,
      kind = "full",
      names = name,
      witness_names = name,
      root_name = name
    ))
  }

  new_slice_domain <- function(parent_id, access = NULL) {
    parent <- domain_record(parent_id)
    root_name <- parent$root_name %||% NULL
    if (is.null(root_name) && !is.null(parent$parent)) {
      root_name <- tccq_shape_domain_root_name(list(domains = domains), parent$parent)
    }

    id <- fresh_domain_id("dom_slice_")
    register_domain(tccq_shape_domain(
      id = id,
      kind = "slice",
      parent = parent_id,
      root_name = root_name,
      access = access,
      witness_names = character()
    ))
  }

  new_opaque_domain <- function(reason = NULL) {
    id <- fresh_domain_id("dom_opaque_")
    register_domain(tccq_shape_domain(
      id = id,
      kind = "opaque",
      reason = reason,
      witness_names = character()
    ))
  }

  join_domain <- function(member_ids) {
    member_ids <- sort(unique(member_ids[nzchar(member_ids)]))
    if (!length(member_ids)) {
      return(NULL)
    }
    if (length(member_ids) == 1L) {
      return(member_ids[[1L]])
    }

    key <- paste(member_ids, collapse = "|")
    cached <- join_cache[[key]] %||% NULL
    if (!is.null(cached)) {
      return(cached)
    }

    witness_names <- character()
    for (id in member_ids) {
      witness_names <- c(witness_names, tccq_shape_domain_witness_names(list(domains = domains), id))
    }
    witness_names <- tccq_unique(witness_names)

    id <- fresh_domain_id("dom_join_")
    register_domain(tccq_shape_domain(
      id = id,
      kind = "join",
      members = member_ids,
      witness_names = witness_names,
      root_name = if (length(unique(vapply(member_ids, function(x) tccq_shape_domain_root_name(list(domains = domains), x) %||% NA_character_, character(1)))) == 1L) {
        tccq_shape_domain_root_name(list(domains = domains), member_ids[[1L]])
      } else {
        NULL
      }
    ))
    join_cache[[key]] <<- id
    id
  }

  annotate_expr <- function(expr) {
    if (!is.list(expr) || is.null(expr$tag)) {
      return(expr)
    }

    if (identical(expr$tag, "var")) {
      if (expr$type$rank > 0L) {
        domain_id <- env[[expr$name]] %||% by_name[[expr$name]] %||% NULL
        if (is.null(domain_id)) {
          domain_id <- new_opaque_domain(reason = paste0("unbound-var:", expr$name))
        }
        expr$shape_domain <- domain_id
      }
      return(expr)
    }

    if (identical(expr$tag, "const")) {
      return(expr)
    }

    if (expr$tag %in% c("view1", "slice_range")) {
      expr$x <- annotate_expr(expr$x)
      if (!is.null(expr$base)) {
        expr$base <- expr$x
      }
      expr$start <- annotate_expr(expr$start)
      expr$stop <- annotate_expr(expr$stop)
      if (expr$type$rank > 0L) {
        parent_id <- tccq_ir_shape_domain(expr$x)
        expr$shape_domain <- if (is.null(parent_id)) {
          new_opaque_domain(reason = paste0("slice-base:", expr$tag))
        } else {
          new_slice_domain(parent_id, access = tccq_ir_normalized_access(expr))
        }
      }
      return(expr)
    }

    if (identical(expr$tag, "index")) {
      expr$x <- annotate_expr(expr$x)
      expr$index <- annotate_expr(expr$index)
      return(expr)
    }

    if (identical(expr$tag, "unary")) {
      expr$x <- annotate_expr(expr$x)
      if (expr$type$rank > 0L) {
        expr$shape_domain <- tccq_ir_shape_domain(expr$x)
      }
      return(expr)
    }

    if (identical(expr$tag, "call1")) {
      expr$x <- annotate_expr(expr$x)
      if (expr$type$rank > 0L) {
        expr$shape_domain <- tccq_ir_shape_domain(expr$x)
      }
      return(expr)
    }

    if (identical(expr$tag, "binary")) {
      expr$lhs <- annotate_expr(expr$lhs)
      expr$rhs <- annotate_expr(expr$rhs)
      if (expr$type$rank > 0L) {
        lhs_id <- if (expr$lhs$type$rank > 0L) tccq_ir_shape_domain(expr$lhs) else NULL
        rhs_id <- if (expr$rhs$type$rank > 0L) tccq_ir_shape_domain(expr$rhs) else NULL
        expr$shape_domain <- join_domain(c(lhs_id, rhs_id))
      }
      return(expr)
    }

    if (identical(expr$tag, "reduce")) {
      expr$x <- annotate_expr(expr$x)
      return(expr)
    }

    if (identical(expr$tag, "len")) {
      expr$x <- annotate_expr(expr$x)
      return(expr)
    }

    if (identical(expr$tag, "boundary_call")) {
      expr$args <- lapply(expr$args, annotate_expr)
      if (expr$type$rank > 0L) {
        expr$shape_domain <- new_opaque_domain(reason = paste0(expr$api %||% "boundary", ":", expr$name %||% "call"))
      }
      return(expr)
    }

    for (nm in names(expr)) {
      if (nm %in% c("tag", "type", "effect", "barrier", "normalized_access", "shape_domain")) {
        next
      }
      expr[[nm]] <- if (is.list(expr[[nm]]) && !is.null(expr[[nm]]$tag)) {
        annotate_expr(expr[[nm]])
      } else if (is.list(expr[[nm]])) {
        lapply(expr[[nm]], annotate_expr)
      } else {
        expr[[nm]]
      }
    }

    if (expr$type$rank > 0L && is.null(expr$shape_domain)) {
      expr$shape_domain <- new_opaque_domain(reason = paste0("generic:", expr$tag))
    }

    expr
  }

  annotate_stmt <- function(stmt) {
    if (!is.list(stmt) || is.null(stmt$tag)) {
      return(stmt)
    }

    if (identical(stmt$tag, "bind")) {
      stmt$value <- annotate_expr(stmt$value)
      stmt$type <- stmt$value$type
      if (stmt$type$rank > 0L) {
        domain_id <- tccq_ir_shape_domain(stmt$value)
        if (is.null(domain_id)) {
          domain_id <- new_opaque_domain(reason = paste0("bind:", stmt$name))
        }
        env[[stmt$name]] <<- domain_id
        by_name[[stmt$name]] <<- domain_id
        append_domain_names(domain_id, stmt$name, witness = TRUE)
      }
      return(stmt)
    }

    if (identical(stmt$tag, "store_index")) {
      stmt$index <- annotate_expr(stmt$index)
      stmt$value <- annotate_expr(stmt$value)
      return(stmt)
    }

    if (identical(stmt$tag, "store_range")) {
      stmt$start <- annotate_expr(stmt$start)
      stmt$stop <- annotate_expr(stmt$stop)
      stmt$value <- annotate_expr(stmt$value)
      return(stmt)
    }

    stmt
  }

  for (nm in module$formal_names) {
    type <- module$types[[nm]]
    if (!is.null(type) && type$rank > 0L) {
      id <- new_full_domain(nm)
      env[[nm]] <- id
      by_name[[nm]] <- id
    }
  }

  ir <- module$ir
  if (!is.null(ir) && identical(ir$tag, "program")) {
    ir$stmts <- lapply(ir$stmts %||% list(), annotate_stmt)
    ir$result <- annotate_expr(ir$result)
    ir$type <- ir$result$type
  } else if (!is.null(ir)) {
    ir <- annotate_expr(ir)
  }

  result_expr <- if (!is.null(ir) && identical(ir$tag, "program")) ir$result else ir
  shape_facts <- list(
    domains = domains,
    by_name = by_name,
    result_domain = if (!is.null(result_expr) && !is.null(result_expr$type) && result_expr$type$rank > 0L) {
      tccq_ir_shape_domain(result_expr)
    } else {
      NULL
    }
  )

  list(ir = ir, shape_facts = shape_facts)
}
