# tccq_boundary.R - typed boundary nodes and boundary planning helpers
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_ir_boundary_call <- function(
  api,
  name,
  args,
  type,
  effect = "pure",
  barrier = TRUE,
  materialize_args = rep(FALSE, length(args)),
  require_contiguous = rep(FALSE, length(args)),
  headers = character(),
  libraries = character(),
  options = character(),
  result_ownership = c("scalar", "new", "borrowed"),
  metadata = list()
) {
  result_ownership <- match.arg(result_ownership)
  tccq_node(
    "boundary_call",
    api = api,
    name = name,
    args = args,
    materialize_args = materialize_args,
    require_contiguous = require_contiguous,
    headers = headers,
    libraries = libraries,
    options = options,
    result_ownership = result_ownership,
    metadata = metadata,
    type = type,
    effect = effect,
    barrier = barrier
  )
}

tccq_ir_boundary_r_eval <- function(call_expr, args, type) {
  tccq_ir_boundary_call(
    api = "r_eval",
    name = "Rf_eval",
    args = args,
    type = type,
    effect = "r_eval",
    barrier = TRUE,
    materialize_args = rep(TRUE, length(args)),
    result_ownership = "new",
    metadata = list(call_expr = call_expr)
  )
}

tccq_is_boundary_node <- function(node) {
  is.list(node) && !is.null(node$tag) && identical(node$tag, "boundary_call")
}

tccq_boundary_nodes <- function(node) {
  out <- list()
  tccq_ir_walk(node, function(n) {
    if (tccq_is_boundary_node(n)) {
      out[[length(out) + 1L]] <<- n
    }
  })
  out
}

tccq_boundary_annotate_ids <- function(node) {
  counter <- 0L

  annotate <- function(x) {
    if (!is.list(x) || inherits(x, "tccq_type")) {
      return(x)
    }

    if (!is.null(x$tag)) {
      if (tccq_is_boundary_node(x)) {
        counter <<- counter + 1L
        x$boundary_id <- counter
      }
      for (nm in names(x)) {
        if (nm %in% c("tag", "type", "effect", "barrier", "normalized_access", "metadata", "boundary_id")) {
          next
        }
        x[[nm]] <- annotate(x[[nm]])
      }
      return(x)
    }

    lapply(x, annotate)
  }

  annotate(node)
}

tccq_boundary_context <- function(node) {
  boundaries <- tccq_boundary_nodes(node)
  if (!length(boundaries)) {
    return(list(headers = character(), libraries = character(), options = character()))
  }

  headers <- unlist(lapply(boundaries, `[[`, "headers"), use.names = FALSE)
  libraries <- unlist(lapply(boundaries, `[[`, "libraries"), use.names = FALSE)
  options <- unlist(lapply(boundaries, `[[`, "options"), use.names = FALSE)

  list(
    headers = tccq_unique(headers),
    libraries = tccq_unique(libraries),
    options = tccq_unique(options)
  )
}

tccq_boundary_diagnostics <- function(node) {
  boundaries <- tccq_boundary_nodes(node)
  if (!length(boundaries)) {
    return(list())
  }

  lapply(boundaries, function(x) {
    call_expr <- x$metadata$call_expr %||% NULL
    list(
      boundary_id = x$boundary_id %||% NA_integer_,
      api = x$api,
      name = x$name,
      effect = x$effect %||% "pure",
      result_ownership = x$result_ownership %||% NA_character_,
      result_type = if (!is.null(x$type)) tccq_type_to_string(x$type) else NA_character_,
      arg_count = length(x$args %||% list()),
      original_call = if (!is.null(call_expr)) paste(deparse(call_expr, nlines = 1L), collapse = " ") else NA_character_
    )
  })
}

tccq_boundary_supported <- function(node, target = NULL, backend = NULL, fallback = c("auto", "hard")) {
  fallback <- match.arg(fallback)
  if (!tccq_is_boundary_node(node)) {
    return(TRUE)
  }

  if (identical(node$api, "r_eval")) {
    return(identical(fallback, "auto"))
  }

  TRUE
}
