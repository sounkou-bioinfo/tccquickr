#' Compiler diagnostic value
#'
#' Diagnostics are values first and condition payloads second. Compiler code
#' should pass these objects around instead of branching on condition strings.
#'
#' @param code Stable diagnostic code.
#' @param message Human-facing message.
#' @param phase Compiler phase that produced the diagnostic.
#' @param path Optional path into the program or schema.
#' @param data Structured diagnostic payload.
#' @export
TccqDiagnostic <- S7::new_class(
  "TccqDiagnostic",
  package = "tccquickr",
  properties = list(
    code = S7::class_character,
    message = S7::class_character,
    phase = S7::class_character,
    path = S7::class_character,
    data = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@code) != 1L || is.na(self@code) || !nzchar(self@code)) {
      problems <- c(problems, "@code must be a single non-empty string")
    }
    if (length(self@message) != 1L || is.na(self@message) || !nzchar(self@message)) {
      problems <- c(problems, "@message must be a single non-empty string")
    }
    if (length(self@phase) != 1L || is.na(self@phase) || !nzchar(self@phase)) {
      problems <- c(problems, "@phase must be a single non-empty string")
    }
    if (length(self@path) != 1L || is.na(self@path)) {
      problems <- c(problems, "@path must be a single string")
    }
    if (length(problems) > 0L) problems
  }
)

#' Compiler result value
#'
#' @param success Whether the phase completed without diagnostics.
#' @param value Result value for the phase.
#' @param diagnostics List of `TccqDiagnostic` values.
#' @export
TccqResult <- S7::new_class(
  "TccqResult",
  package = "tccquickr",
  properties = list(
    success = S7::class_logical,
    value = S7::class_any,
    diagnostics = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@success) != 1L || is.na(self@success)) {
      problems <- c(problems, "@success must be a single TRUE/FALSE value")
    }
    diagnostics_are_tccq_diagnostics <- vapply(
      self@diagnostics,
      S7::S7_inherits,
      logical(1),
      class = TccqDiagnostic
    )
    if (!all(diagnostics_are_tccq_diagnostics)) {
      problems <- c(problems, "@diagnostics must contain only <TccqDiagnostic> values")
    }
    if (length(problems) > 0L) problems
  }
)

#' Create a compiler diagnostic
#'
#' @param code Stable diagnostic code.
#' @param message Human-facing message.
#' @param phase Compiler phase that produced the diagnostic.
#' @param path Optional path into the program or schema.
#' @param data Structured diagnostic payload.
#' @export
tccq_diagnostic <- function(
  code,
  message,
  phase = "schema",
  path = "",
  data = list()
) {
  .tccq_check_character_scalar(code, "code")
  .tccq_check_character_scalar(message, "message")
  .tccq_check_character_scalar(phase, "phase")
  .tccq_check_character_scalar(path, "path")
  if (!is.list(data)) {
    tccq_abort(
      "schema.invalid_diagnostic_data",
      "`data` must be a list.",
      phase = "schema",
      path = "diagnostic.data"
    )
  }

  TccqDiagnostic(
    code = code,
    message = message,
    phase = phase,
    path = path,
    data = data
  )
}

#' Create a compiler result
#'
#' @param success Whether the phase completed without diagnostics.
#' @param value Result value for the phase.
#' @param diagnostics List of `TccqDiagnostic` values.
#' @export
tccq_result <- function(success, value = NULL, diagnostics = list()) {
  .tccq_check_logical_scalar(success, "success")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  TccqResult(success = success, value = value, diagnostics = diagnostics)
}

tccq_abort <- function(
  code,
  message,
  phase = "schema",
  path = "",
  data = list(),
  class = character(),
  call = NULL
) {
  diagnostic <- tccq_diagnostic(
    code = code,
    message = message,
    phase = phase,
    path = path,
    data = data
  )
  tccq_abort_diagnostic(diagnostic, class = class, call = call)
}

tccq_abort_diagnostic <- function(diagnostic, class = character(), call = NULL) {
  .tccq_check_s7(diagnostic, TccqDiagnostic, "TccqDiagnostic", "diagnostic")
  condition <- structure(
    list(
      message = diagnostic@message,
      call = call,
      diagnostic = diagnostic
    ),
    class = c(
      class,
      paste0("tccq_error_", diagnostic@phase),
      diagnostic@code,
      "tccq_error",
      "error",
      "condition"
    )
  )
  stop(condition)
}

#' Extract a diagnostic from a compiler condition
#'
#' @param condition A compiler condition.
#' @export
tccq_condition_diagnostic <- function(condition) {
  condition$diagnostic
}
