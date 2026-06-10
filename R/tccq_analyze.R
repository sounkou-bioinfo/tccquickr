# tccq_analyze.R - high-level function and compiler reconnaissance helpers
# SPDX-License-Identifier: GPL-3.0-or-later

#' Analyze an R function for typed-compilation reconnaissance
#'
#' `tccq_analyze()` collects high-level AST facts and optional compiler-package
#' observations for a function. This is intentionally a side-band analysis to
#' improve compiler diagnostics and help with boundary-reachability, while keeping
#' the typed-IR path as the optimization frontier.
#'
#' The output is intentionally conservative and mostly advisory.
#'
#' @param fn R function.
#'
#' @return A list of class `tccq_analysis` with fields:
#'   - `formals`: formals metadata
#'   - `ast`: AST observations (calls, symbols, assignments, loops)
#'   - `recommendations`: conservative suggestions for lowering success
#'
#' @export
#'
#' @examples
#' f <- function(x, y = 1) {
#'   declare(type(x = double(NA), y = double(NA)))
#'   sum((x + y) * y)
#' }
#' tccq_analyze(f)
#'
#' @seealso [tccq_compile()], `tccq_jit()`
tccq_analyze <- function(fn) {
  if (!is.function(fn)) {
    tccq_abort("fn must be a function")
  }

  formals_info <- tccq_analyze_formals(fn)
  body_expr <- body(fn)
  ast <- tccq_analyze_ast(body_expr, formals_info$names)
  recommendations <- tccq_analyze_recommendations(formals_info, ast)

  structure(
    list(
      formals = formals_info,
      ast = ast,
      recommendations = unique(recommendations)
    ),
    class = "tccq_analysis"
  )
}

#' @exportS3Method print tccq_analysis
print.tccq_analysis <- function(x, ...) {
  cat("<tccq_analysis>\n")
  if (!is.null(x$formals)) {
    if (length(x$formals$names)) {
      cat("  formals:", paste(x$formals$names, collapse = ", "), "\n")
    } else {
      cat("  formals: <none>\n")
    }
    cat("  has ...:", as.character(x$formals$has_dots), "\n")
  }

  if (!is.null(x$ast)) {
    cat("  has declare:", as.character(x$ast$has_declare), "\n")
    cat("  calls:", as.character(x$ast$calls), "\n")
    if (length(x$ast$special_forms)) {
      cat("  special forms:", paste(x$ast$special_forms, collapse = ", "), "\n")
    }
    if (length(x$ast$dynamic_calls)) {
      cat("  dynamic calls:", paste(x$ast$dynamic_calls, collapse = ", "), "\n")
    }
  }

  if (length(x$recommendations)) {
    cat("  recommendations:\n")
    for (r in x$recommendations) {
      cat("   -", r, "\n")
    }
  }

  invisible(x)
}

# Internal helpers ------------------------------------------------------------

tccq_analyze_formals <- function(fn) {
  fmls <- formals(fn)
  names_fmls <- names(fmls)
  if (is.null(names_fmls)) {
    names_fmls <- character()
  }

  defaults <- lapply(fmls, function(v) {
    if (is.null(v) || (is.symbol(v) && identical(as.character(v), ""))) {
      return(NULL)
    }
    deparse(v, width.cutoff = 500L)
  })

  list(
    names = names_fmls,
    has_dots = any(names_fmls == "..."),
    n = length(names_fmls),
    defaults = defaults
  )
}

#' @keywords internal
#' @noRd
tccq_analyze_ast <- function(expr, formal_names) {
  symbols <- character()
  call_names <- character()
  assignments <- character()
  superassignments <- character()
  loop_vars <- character()
  special_forms <- character()
  dynamic_calls <- character()
  has_declare <- FALSE
  calls <- 0L
  local_bindings <- tccq_unique(formal_names)

  walk_expr <- function(x, locals, nested) {
    if (is.null(x) || is.atomic(x) || is.character(x) || is.numeric(x) || is.logical(x) || is.complex(x)) {
      return(invisible(NULL))
    }

    if (is.symbol(x)) {
      sym <- as.character(x)
      symbols <<- c(symbols, sym)
      return(invisible(NULL))
    }

    if (is.pairlist(x) || is.list(x)) {
      for (child in x) {
        walk_expr(child, locals, nested)
      }
      return(invisible(NULL))
    }

    if (!is.call(x)) {
      return(invisible(NULL))
    }

    calls <<- calls + 1L
    head <- tccq_call_head_name(x)

    if (is.null(head)) {
      for (i in seq_along(x)) {
        walk_expr(x[[i]], locals, nested)
      }
      return(invisible(NULL))
    }

    call_names <<- c(call_names, head)
    if (head %in% c(
      "if", "for", "while", "repeat", "break", "next", "return",
      "function", "{", "<-", "=", "<<-"
    )) {
      special_forms <<- c(special_forms, head)
    }

    if (head %in% c("do.call", "eval", "evalq", "substitute", "get", "get0", "assign", "match.fun", "as.name", "call")) {
      dynamic_calls <<- c(dynamic_calls, head)
    }

    if (identical(head, "declare")) {
      has_declare <<- TRUE
    }

    if (head %in% c("<-", "=")) {
      lhs <- x[[2L]]
      if (is.symbol(lhs)) {
        sym <- as.character(lhs)
        assignments <<- c(assignments, sym)
        if (!nested) {
          local_bindings <<- tccq_unique(c(local_bindings, sym))
        }
      }
    }

    if (identical(head, "<<-")) {
      lhs <- x[[2L]]
      if (is.symbol(lhs)) {
        superassignments <<- c(superassignments, as.character(lhs))
      }
    }

    if (identical(head, "for")) {
      loop <- x[[2L]]
      if (is.symbol(loop)) {
        loop_sym <- as.character(loop)
        loop_vars <<- c(loop_vars, loop_sym)
        if (!nested) {
          local_bindings <<- tccq_unique(c(local_bindings, loop_sym))
        }
      }
    }

    if (identical(head, "function")) {
      fn_locals <- locals
      args_expr <- x[[2L]]
      if (is.pairlist(args_expr) || is.list(args_expr)) {
        fn_locals <- tccq_unique(c(fn_locals, names(args_expr)[nzchar(names(args_expr))]))
        for (i in seq_along(args_expr)) {
          walk_expr(args_expr[[i]], fn_locals, TRUE)
        }
      }
      if (length(x) >= 3L) {
        walk_expr(x[[3L]], fn_locals, TRUE)
      }
      return(invisible(NULL))
    }

    for (i in seq_along(x)) {
      walk_expr(x[[i]], local_bindings, nested)
    }

    invisible(NULL)
  }

  walk_expr(expr, local_bindings, FALSE)

  symbols <- tccq_unique(symbols)

  list(
    has_declare = has_declare,
    has_dots = any(formal_names == "..."),
    calls = calls,
    call_names = tccq_unique(call_names),
    special_forms = tccq_unique(special_forms),
    dynamic_calls = tccq_unique(dynamic_calls),
    assignments = tccq_unique(assignments),
    superassignments = tccq_unique(superassignments),
    loop_vars = tccq_unique(loop_vars),
    symbols = symbols,
    maybe_free_symbols = tccq_unique(setdiff(symbols, local_bindings))
  )
}

#' @keywords internal
#' @noRd
tccq_analyze_recommendations <- function(formals_info, ast_info) {
  out <- character()

  if (!ast_info$has_declare) {
    out <- c(
      out,
      "add declare(type(...)) for typed compilation and stronger optimizer visibility"
    )
  }

  if (ast_info$has_dots) {
    out <- c(out, "functions with ... are usually less suitable for static typed lowering")
  }

  if (length(ast_info$dynamic_calls)) {
    out <- c(
      out,
      "dynamic language calls present (eval/get/do.call-style), likely boundary behavior"
    )
  }

  if (length(ast_info$superassignments)) {
    out <- c(
      out,
      "superassignment detected; semantic safety and optimization boundaries become strict"
    )
  }

  unique(out)
}
