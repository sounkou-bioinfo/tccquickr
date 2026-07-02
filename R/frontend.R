#' Analyze a declared R function into the fresh program schema
#'
#' This is intentionally narrow. It parses `declare(type(...))` declarations,
#' records a program schema, reports calls without implementations as classed
#' diagnostics, and runs the first backend-neutral expression lowering pass when
#' frontend facts are clean.
#'
#' @param fn Function to analyze.
#' @param strict If `TRUE`, throw the first diagnostic as a classed condition.
#' @param registry Operation registry used to decide whether calls are
#'   supported.
#' @param context Operation support context.
#' @export
tccq_analyze <- function(
  fn,
  strict = FALSE,
  registry = tccq_default_op_registry(),
  context = tccq_op_context()
) {
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
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }

  frontend_diagnostics <- list()
  declarations <- .tccq_extract_declarations(fn)
  frontend_diagnostics <- c(frontend_diagnostics, declarations$diagnostics)
  global_calls <- codetools::findGlobals(fn, merge = FALSE)$functions

  formal_names <- names(formals(fn))
  declared_names <- names(declarations$bindings)
  missing <- setdiff(formal_names, declared_names)
  if (length(missing) > 0L) {
    frontend_diagnostics <- c(frontend_diagnostics, lapply(missing, function(name) {
      tccq_diagnostic(
        "frontend.missing_declaration",
        sprintf("Formal `%s` has no declared type.", name),
        phase = "frontend",
        path = sprintf("formals.%s", name),
        data = list(name = name)
      )
    }))
  }

  call_index <- tccq_collect_call_index(
    body(fn),
    global_calls = global_calls,
    env = environment(fn)
  )
  calls <- call_index@calls
  unimplemented <- tccq_unimplemented_calls(calls, registry, context)
  frontend_diagnostics <- c(frontend_diagnostics, lapply(unimplemented, function(call_name) {
    tccq_diagnostic(
      "frontend.unimplemented_call",
      sprintf("Call `%s` has no implementation in the current registry/context.", call_name),
      phase = "frontend",
      path = "body",
      data = list(call = call_name, global_calls = global_calls)
    )
  }))

  lowering <- NULL
  lowering_diagnostics <- list()
  if (length(frontend_diagnostics) == 0L) {
    lowering <- tccq_lower_function(
      fn,
      declarations$bindings,
      registry = registry,
      context = context
    )
    lowering_diagnostics <- lowering@diagnostics
  }
  program_diagnostics <- c(frontend_diagnostics, lowering_diagnostics)

  program <- tccq_program(
    name = .tccq_function_name(fn),
    formals = declarations$bindings,
    values = if (is.null(lowering)) list() else lowering@values,
    regions = if (is.null(lowering)) list() else lowering@regions,
    result = if (is.null(lowering)) NULL else lowering@result,
    diagnostics = program_diagnostics,
    call_index = call_index,
    storage_plan = if (is.null(lowering)) NULL else lowering@storage_plan,
    attrs = list(
      lowered = !is.null(lowering) && !is.null(lowering@result),
      lowering = if (is.null(lowering)) NULL else lowering@attrs
    )
  )

  analysis_succeeded <- length(program_diagnostics) == 0L
  if (!analysis_succeeded && isTRUE(strict)) {
    tccq_abort_diagnostic(program_diagnostics[[1L]])
  }
  tccq_result(success = analysis_succeeded, value = program, diagnostics = program_diagnostics)
}

#' Compile a declared R function
#'
#' Compilation currently stops at backend planning. By default it asks the core
#' backend suite to account for the same typed program, so C, Fortran,
#' graph/device, Rtinycc, and R-call evaluation all report constraints through
#' one contract instead of letting one concrete backend shape the IR. The
#' result succeeds when at least one backend produces a working plan; backends
#' that cannot lower the program report feasibility diagnostics in their plans
#' without vetoing the suite.
#'
#' @param fn Function to compile.
#' @param backends Backend implementation descriptors.
#' @param context Backend planning context.
#' @param strict If `TRUE`, throw the first diagnostic as a classed condition.
#' @export
tccq_compile <- function(
  fn,
  backends = tccq_core_backends(),
  context = tccq_backend_context(),
  strict = TRUE
) {
  analysis <- tccq_analyze(fn, strict = FALSE)
  if (!analysis@success) {
    if (isTRUE(strict)) {
      tccq_abort_diagnostic(analysis@diagnostics[[1L]])
    }
    return(analysis)
  }

  plan <- tccq_plan_backends(analysis@value, backends = backends, context = context)
  if (!plan@success && isTRUE(strict)) {
    tccq_abort_diagnostic(plan@diagnostics[[1L]])
  }
  plan
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
    if (identical(tccq_call_name(node), "declare") && length(node) >= 2L) {
      candidate <- node[[2L]]
      if (is.call(candidate) && identical(tccq_call_name(candidate), "type")) {
        found <<- candidate
        return(NULL)
      }
    }
    children <- as.list(node)[-1L]
    for (child_index in seq_along(children)) {
      if (identical(children[[child_index]], quote(expr = ))) {
        next
      }
      walk(children[[child_index]])
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

  base <- tccq_call_name(expr)
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

.tccq_function_name <- function(fn) {
  name <- attr(fn, "tccq_name", exact = TRUE)
  if (is.character(name) && length(name) == 1L && nzchar(name)) {
    return(name)
  }
  "anonymous"
}
