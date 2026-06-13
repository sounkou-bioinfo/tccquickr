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
  )
)

#' Compiler result value
#'
#' @param ok Whether compilation can continue.
#' @param value Result value for the phase.
#' @param diagnostics List of `TccqDiagnostic` values.
#' @export
TccqResult <- S7::new_class(
  "TccqResult",
  package = "tccquickr",
  properties = list(
    ok = S7::class_logical,
    value = S7::class_any,
    diagnostics = S7::class_list
  )
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
#' @param ok Whether compilation can continue.
#' @param value Result value for the phase.
#' @param diagnostics List of `TccqDiagnostic` values.
#' @export
tccq_result <- function(ok, value = NULL, diagnostics = list()) {
  .tccq_check_logical_scalar(ok, "ok")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  TccqResult(ok = ok, value = value, diagnostics = diagnostics)
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
