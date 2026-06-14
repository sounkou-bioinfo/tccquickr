#' Compiler pass implementation
#'
#' A pass is an S7 object whose `run` function must accept a `TccqProgram` and
#' return a `TccqProgram`. The protocol is also represented as a `s7contract`
#' interface so passes can be checked progressively at runtime.
#'
#' @param name Pass name.
#' @param run Pass implementation.
#' @export
TccqPassSpec <- S7::new_class(
  "TccqPassSpec",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    run = S7::class_function
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (!is.function(self@run)) {
      problems <- c(problems, "@run must be a function")
    }
    if (length(problems) > 0L) problems
  }
)

#' Run a compiler pass
#'
#' @param pass Compiler pass.
#' @param program Program value.
#' @export
tccq_pass_run <- S7::new_generic(
  "tccq_pass_run",
  dispatch_args = c("pass", "program"),
  function(pass, program) S7::S7_dispatch()
)

S7::method(tccq_pass_run, list(TccqPassSpec, TccqProgram)) <- function(pass, program) {
  pass@run(program)
}

#' Compiler pass interface
#'
#' @export
TccqPass <- s7contract::new_interface(
  "TccqPass",
  package = "tccquickr",
  generics = list(
    run = s7contract::interface_requirement(
      tccq_pass_run,
      args = list(program = TccqProgram),
      returns = TccqProgram
    )
  )
)

#' Construct a compiler pass
#'
#' @param name Pass name.
#' @param run Pass implementation.
#' @export
tccq_pass <- function(name, run) {
  .tccq_check_character_scalar(name, "name")
  if (!is.function(run)) {
    tccq_abort(
      "schema.invalid_pass_run",
      "`run` must be a function.",
      phase = "schema",
      path = "pass.run"
    )
  }
  TccqPassSpec(name = name, run = run)
}

#' Run a contract-checked pass pipeline
#'
#' @param program Program value.
#' @param passes Compiler pass or list of passes.
#' @export
tccq_run_pipeline <- function(program, passes) {
  .tccq_check_s7(program, TccqProgram, "TccqProgram", "program")
  if (S7::S7_inherits(passes, TccqPassSpec)) {
    passes <- list(passes)
  }
  if (!is.list(passes)) {
    tccq_abort(
      "schema.invalid_passes",
      "`passes` must be a pass or list of passes.",
      phase = "schema",
      path = "pipeline.passes"
    )
  }

  out <- program
  for (i in seq_along(passes)) {
    pass <- passes[[i]]
    s7contract::assert_implements(pass, TccqPass, arg = sprintf("passes[[%d]]", i))
    out <- with(TccqPass, tccq_pass_run(pass, out))
  }
  out
}
