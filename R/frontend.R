#' Analyze a declared R function into the fresh program schema
#'
#' This is intentionally narrow. It parses `declare(type(...))` declarations,
#' records a program schema, and reports unsupported operations as classed
#' diagnostics. It does not lower or compile yet.
#'
#' @param fn Function to analyze.
#' @param strict If `TRUE`, throw the first diagnostic as a classed condition.
#' @export
tccq_analyze <- function(fn, strict = FALSE) {
  if (!is.function(fn)) {
    diagnostic <- tccq_diagnostic(
      "frontend.not_function",
      "`fn` must be a function.",
      phase = "frontend",
      path = "fn",
      data = list(type = typeof(fn))
    )
    if (isTRUE(strict)) {
      tccq_abort_diagnostic(diagnostic)
    }
    return(tccq_result(FALSE, diagnostics = list(diagnostic)))
  }

  diagnostics <- list()
  declarations <- .tccq_extract_declarations(fn)
  diagnostics <- c(diagnostics, declarations$diagnostics)
  global_calls <- codetools::findGlobals(fn, merge = FALSE)$functions

  formal_names <- names(formals(fn))
  declared_names <- names(declarations$bindings)
  missing <- setdiff(formal_names, declared_names)
  if (length(missing) > 0L) {
    diagnostics <- c(diagnostics, lapply(missing, function(name) {
      tccq_diagnostic(
        "frontend.missing_declaration",
        sprintf("Formal `%s` has no declared type.", name),
        phase = "frontend",
        path = sprintf("formals.%s", name),
        data = list(name = name)
      )
    }))
  }

  unsupported <- .tccq_find_unsupported_calls(body(fn), global_calls)
  diagnostics <- c(diagnostics, lapply(unsupported, function(call_name) {
    tccq_diagnostic(
      "frontend.unsupported_call",
      sprintf("Call `%s` is outside the current declared subset.", call_name),
      phase = "frontend",
      path = "body",
      data = list(call = call_name, global_calls = global_calls)
    )
  }))

  program <- tccq_program(
    name = .tccq_function_name(fn),
    formals = declarations$bindings,
    diagnostics = diagnostics
  )

  ok <- length(diagnostics) == 0L
  if (!ok && isTRUE(strict)) {
    tccq_abort_diagnostic(diagnostics[[1L]])
  }
  tccq_result(ok, value = program, diagnostics = diagnostics)
}

#' Compile a declared R function
#'
#' The backend does not exist in the reset. This function exists only to return
#' or throw classed diagnostics through the same result path as analysis.
#'
#' @param fn Function to compile.
#' @param strict If `TRUE`, throw the first diagnostic as a classed condition.
#' @export
tccq_compile <- function(fn, strict = TRUE) {
  analysis <- tccq_analyze(fn, strict = FALSE)
  if (!analysis@ok) {
    if (isTRUE(strict)) {
      tccq_abort_diagnostic(analysis@diagnostics[[1L]])
    }
    return(analysis)
  }

  diagnostic <- tccq_diagnostic(
    "compiler.backend_absent",
    "No backend exists in the hard-reset compiler core yet.",
    phase = "compiler",
    path = "backend"
  )
  if (isTRUE(strict)) {
    tccq_abort_diagnostic(diagnostic)
  }
  tccq_result(FALSE, value = analysis@value, diagnostics = list(diagnostic))
}

#' A known failing target program for the rebuilt compiler
#'
#' The goal is for this statistical kernel to eventually pass through the full
#' declared-R pipeline: symbolic shapes, matrix normalization, logistic map,
#' reductions, matrix-vector multiply, and gradient construction. It currently
#' fails by design and gives us a concrete north star.
#'
#' @export
tccq_apotheosis_kernel <- function() {
  src <- "
    function(x, y, w, lambda) {
      declare(type(
        x = double(n, p),
        y = double(n),
        w = double(p),
        lambda = double()
      ))

      mu <- colMeans(x)
      sigma <- sqrt(colSums((x - mu)^2) / (n - 1L))
      z <- (x - mu) / sigma
      eta <- z %*% w
      prob <- 1 / (1 + exp(-eta))
      grad <- crossprod(z, prob - y) / n + lambda * w
      w - 0.01 * grad
    }
  "
  eval(parse(text = src, keep.source = FALSE)[[1L]], envir = baseenv())
}

.tccq_extract_declarations <- function(fn) {
  declaration <- .tccq_find_declare_type_call(body(fn))
  if (is.null(declaration)) {
    diagnostic <- tccq_diagnostic(
      "frontend.missing_declare",
      "Function body must contain `declare(type(...))`.",
      phase = "frontend",
      path = "body"
    )
    return(list(bindings = list(), diagnostics = list(diagnostic)))
  }

  args <- as.list(declaration)[-1L]
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }

  bindings <- list()
  diagnostics <- list()
  for (i in seq_along(args)) {
    name <- arg_names[[i]]
    if (!nzchar(name)) {
      diagnostics <- c(diagnostics, list(tccq_diagnostic(
        "frontend.unnamed_declaration",
        "Every type declaration must be named.",
        phase = "frontend",
        path = sprintf("declare.%d", i)
      )))
      next
    }

    parsed <- tryCatch(
      .tccq_type_from_call(args[[i]]),
      tccq_error = function(err) err
    )
    if (inherits(parsed, "tccq_error")) {
      diagnostics <- c(diagnostics, list(tccq_condition_diagnostic(parsed)))
      next
    }
    bindings[[name]] <- tccq_binding(name, parsed)
  }

  list(bindings = bindings, diagnostics = diagnostics)
}

.tccq_find_declare_type_call <- function(expr) {
  found <- NULL
  walk <- function(node) {
    if (!is.null(found)) {
      return(NULL)
    }
    if (!is.call(node)) {
      return(NULL)
    }
    if (identical(.tccq_call_name(node), "declare") && length(node) >= 2L) {
      candidate <- node[[2L]]
      if (is.call(candidate) && identical(.tccq_call_name(candidate), "type")) {
        found <<- candidate
        return(NULL)
      }
    }
    for (child in as.list(node)[-1L]) {
      walk(child)
    }
    NULL
  }
  walk(expr)
  found
}

.tccq_type_from_call <- function(expr) {
  if (!is.call(expr)) {
    tccq_abort(
      "frontend.invalid_type_declaration",
      "Type declarations must be calls such as `double(n, p)`.",
      phase = "frontend",
      path = "declare.type",
      data = list(expr = deparse1(expr))
    )
  }

  base <- .tccq_call_name(expr)
  dims <- lapply(as.list(expr)[-1L], .tccq_dim_from_expr)
  tccq_type(base, tccq_shape(dims))
}

.tccq_dim_from_expr <- function(expr) {
  if (is.symbol(expr)) {
    return(tccq_dim_symbol(as.character(expr)))
  }
  if (is.numeric(expr) && length(expr) == 1L) {
    return(tccq_dim_constant(expr))
  }
  tccq_abort(
    "frontend.invalid_dimension_expression",
    "Dimension expressions are limited to symbols and constants for now.",
    phase = "frontend",
    path = "declare.dimension",
    data = list(expr = deparse1(expr))
  )
}

.tccq_find_unsupported_calls <- function(expr, global_calls = character()) {
  calls <- character()
  walk <- function(node) {
    if (!is.call(node)) {
      return(NULL)
    }
    calls <<- c(calls, .tccq_call_name(node))
    for (child in as.list(node)[-1L]) {
      walk(child)
    }
    NULL
  }
  walk(expr)
  calls <- c(calls, global_calls)
  allowed <- c(
    "{", "(", "<-", "=", "declare", "type",
    "logical", "integer", "double", "complex", "character",
    "+", "-", "*", "/", "^", "sqrt", "exp"
  )
  sort(setdiff(unique(calls), allowed))
}

.tccq_call_name <- function(call) {
  head <- call[[1L]]
  if (is.symbol(head)) {
    return(as.character(head))
  }
  deparse1(head)
}

.tccq_function_name <- function(fn) {
  name <- attr(fn, "tccq_name", exact = TRUE)
  if (is.character(name) && length(name) == 1L && nzchar(name)) {
    return(name)
  }
  "anonymous"
}
