#' Dead-store optimization
#'
#' This optimization removes assignments to compiler-owned local or cell
#' storage when the target is never read and evaluating the assigned expression
#' cannot write, allocate, cross a boundary, signal an error, or warn. It is
#' deliberately limited to structured neutral program bodies; graph-level dead
#' value elimination must also rewrite schedules, regions, fusion contracts,
#' storage lifetimes, and allocations and therefore requires a separate pass.
#'
#' @export
TccqDeadStoreOptimization <- S7::new_class(
  "TccqDeadStoreOptimization",
  package = "tccquickr"
)

TccqDeadStoreAnalysis <- S7::new_class(
  "TccqDeadStoreAnalysis",
  package = "tccquickr",
  properties = list(
    read_value_ids = S7::class_character,
    expression_value_ids = S7::class_character,
    required_target_ids = S7::class_character,
    statements_by_id = S7::class_list,
    unsupported_statements = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    statements_are_typed <- vapply(
      self@statements_by_id,
      S7::S7_inherits,
      logical(1),
      class = TccqStatement
    )
    unsupported_are_typed <- vapply(
      self@unsupported_statements,
      S7::S7_inherits,
      logical(1),
      class = TccqStatement
    )
    if (!all(statements_are_typed) || !all(unsupported_are_typed)) {
      problems <- c(
        problems,
        "statement collections must contain TccqStatement values"
      )
    }
    if (all(statements_are_typed) && length(self@statements_by_id) > 0L) {
      statement_ids <- unname(vapply(
        self@statements_by_id,
        function(statement) statement@id,
        character(1)
      ))
      if (
        is.null(names(self@statements_by_id)) ||
          !identical(names(self@statements_by_id), statement_ids)
      ) {
        problems <- c(problems, "@statements_by_id names must match statement ids")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Optimize a typed program
#'
#' @param optimization Optimization implementation.
#' @param program Typed program to transform.
#' @return A [TccqResult] containing the transformed [TccqProgram].
#' @export
tccq_optimize <- S7::new_generic(
  "tccq_optimize",
  dispatch_args = c("optimization", "program"),
  function(optimization, program) S7::S7_dispatch()
)

#' Verify an optimized program
#'
#' @param optimization Optimization implementation.
#' @param before Program before transformation.
#' @param after Program after transformation.
#' @return A [TccqResult] containing `after` when its pass invariants hold.
#' @export
tccq_verify_optimization <- S7::new_generic(
  "tccq_verify_optimization",
  dispatch_args = c("optimization", "before", "after"),
  function(optimization, before, after) S7::S7_dispatch()
)

#' Program optimization trait
#'
#' Optimizations explicitly opt into transformation and verification. The
#' compiler invokes both through this contract before backend planning.
#'
#' @export
TccqProgramOptimization <- s7contract::new_trait(
  "TccqProgramOptimization",
  package = "tccquickr",
  methods = list(
    optimize = s7contract::trait_method(
      tccq_optimize,
      args = list(program = TccqProgram),
      returns = TccqResult
    ),
    verify = s7contract::trait_method(
      tccq_verify_optimization,
      args = list(before = TccqProgram, after = TccqProgram),
      returns = TccqResult
    )
  )
)

tccq_register_optimization_traits <- function() {
  diagnostic_result <- function(code, message, program, data = list()) {
    diagnostic <- tccq_diagnostic(
      code,
      message,
      phase = "optimization",
      path = "program.body",
      data = c(list(program = program@name), data)
    )
    tccq_result(success = FALSE, diagnostics = list(diagnostic))
  }

  structured_program_is_supported <- function(program) {
    length(program@schedule@steps) == 0L &&
      length(program@regions) == 1L &&
      length(program@regions[[1L]]@fusion_groups) == 0L
  }

  unsupported_statement_result <- function(facts, program) {
    if (length(facts@unsupported_statements) == 0L) {
      return(NULL)
    }
    diagnostic_result(
      "optimization.unsupported_statement",
      "Dead-store elimination encountered an unknown neutral statement.",
      program,
      data = list(
        statements = vapply(
          facts@unsupported_statements,
          function(statement) statement@id,
          character(1)
        )
      )
    )
  }

  expression_facts <- function(body, required_value_ids = character()) {
    read_value_ids <- character()
    expression_value_ids <- character()
    required_target_ids <- required_value_ids
    statements_by_id <- list()
    unsupported_statements <- list()

    visit_expression <- function(expression) {
      expression_value_ids <<- c(expression_value_ids, expression@value_id)
      if (!is.null(expression@reference)) {
        read_value_ids <<- c(
          read_value_ids,
          expression@reference@source_value_id
        )
      }
      for (input in expression@inputs) {
        visit_expression(input)
      }
      invisible(NULL)
    }

    visit_iteration <- function(iteration) {
      visit_expression(iteration@source)
      if (S7::S7_inherits(iteration@element, TccqExpression)) {
        visit_expression(iteration@element)
      }
      invisible(NULL)
    }

    visit_block <- function(block) {
      if (S7::S7_inherits(block, TccqValueBlock)) {
        required_target_ids <<- c(
          required_target_ids,
          block@result@value_id
        )
      }
      for (statement in block@statements) {
        statements_by_id[[statement@id]] <<- statement
        if (S7::S7_inherits(statement, TccqAssignment)) {
          visit_expression(statement@value)
        } else if (S7::S7_inherits(statement, TccqSwitch)) {
          visit_expression(statement@selector)
          required_target_ids <<- c(
            required_target_ids,
            statement@selector_target@value_id
          )
          for (alternative in statement@alternatives) {
            visit_block(alternative)
          }
        } else if (S7::S7_inherits(statement, TccqIf)) {
          visit_expression(statement@condition)
          visit_block(statement@consequent)
          visit_block(statement@alternative)
        } else if (S7::S7_inherits(statement, TccqWhile)) {
          visit_expression(statement@condition)
          visit_block(statement@body)
        } else if (S7::S7_inherits(statement, TccqRepeat)) {
          visit_block(statement@body)
        } else if (S7::S7_inherits(statement, TccqFor)) {
          required_target_ids <<- c(
            required_target_ids,
            statement@iterator@value_id
          )
          visit_iteration(statement@iteration)
          visit_block(statement@body)
        } else if (!S7::S7_inherits(statement, TccqLoopTransfer)) {
          unsupported_statements[[length(unsupported_statements) + 1L]] <<-
            statement
        }
      }
      invisible(NULL)
    }

    visit_block(body)
    TccqDeadStoreAnalysis(
      read_value_ids = unique(read_value_ids),
      expression_value_ids = unique(expression_value_ids),
      required_target_ids = unique(required_target_ids),
      statements_by_id = statements_by_id,
      unsupported_statements = unsupported_statements
    )
  }

  expression_is_discardable <- function(expression) {
    effect <- expression@effect
    !isTRUE(effect@writes) &&
      !isTRUE(effect@allocates) &&
      !isTRUE(effect@boundary) &&
      !isTRUE(effect@may_error) &&
      !isTRUE(effect@may_warn)
  }

  assignment_is_dead <- function(statement, facts) {
    S7::S7_inherits(statement, TccqAssignment) &&
      statement@target@kind %in% c("local", "cell") &&
      !statement@target@value_id %in% facts@read_value_ids &&
      !statement@target@value_id %in% facts@required_target_ids &&
      expression_is_discardable(statement@value)
  }

  statement_target_ids <- function(statement) {
    if (S7::S7_inherits(statement, TccqAssignment)) {
      return(statement@target@value_id)
    }
    if (S7::S7_inherits(statement, TccqSwitch)) {
      return(c(
        statement@selector_target@value_id,
        unlist(lapply(
          statement@alternatives,
          block_target_ids
        ), use.names = FALSE)
      ))
    }
    if (S7::S7_inherits(statement, TccqIf)) {
      return(c(
        block_target_ids(statement@consequent),
        block_target_ids(statement@alternative)
      ))
    }
    if (S7::S7_inherits(statement, TccqFor)) {
      return(c(
        statement@iterator@value_id,
        block_target_ids(statement@body)
      ))
    }
    if (S7::S7_inherits(statement, TccqLoop)) {
      return(block_target_ids(statement@body))
    }
    character()
  }

  block_target_ids <- function(block) {
    target_ids <- unlist(
      lapply(block@statements, statement_target_ids),
      use.names = FALSE
    )
    if (S7::S7_inherits(block, TccqValueBlock)) {
      target_ids <- c(target_ids, block@result@value_id)
    }
    unique(target_ids)
  }

  transform_body <- function(body, required_value_ids) {
    transform_block <- function(block, facts) {
      transformed_statements <- list()
      for (statement in block@statements) {
        if (assignment_is_dead(statement, facts)) {
          next
        }

        transformed_statement <- if (S7::S7_inherits(statement, TccqConditional)) {
          tccq_conditional(
            statement@id,
            statement@condition,
            transform_block(statement@consequent, facts),
            transform_block(statement@alternative, facts),
            statement@branch
          )
        } else if (S7::S7_inherits(statement, TccqSwitch)) {
          tccq_switch(
            statement@id,
            statement@selector,
            statement@selector_target,
            lapply(statement@alternatives, transform_block, facts = facts),
            statement@semantics
          )
        } else if (S7::S7_inherits(statement, TccqIf)) {
          tccq_if(
            statement@id,
            statement@condition,
            transform_block(statement@consequent, facts),
            transform_block(statement@alternative, facts),
            statement@semantics
          )
        } else if (S7::S7_inherits(statement, TccqWhile)) {
          tccq_while(
            statement@id,
            statement@condition,
            transform_block(statement@body, facts),
            statement@semantics
          )
        } else if (S7::S7_inherits(statement, TccqRepeat)) {
          tccq_repeat(
            statement@id,
            transform_block(statement@body, facts),
            statement@semantics
          )
        } else if (S7::S7_inherits(statement, TccqFor)) {
          tccq_for(
            statement@id,
            statement@iterator,
            statement@iteration,
            transform_block(statement@body, facts),
            statement@semantics
          )
        } else {
          statement
        }
        transformed_statements[[length(transformed_statements) + 1L]] <-
          transformed_statement
      }

      retained_target_ids <- unique(c(
        facts@read_value_ids,
        facts@required_target_ids,
        unlist(
          lapply(transformed_statements, statement_target_ids),
          use.names = FALSE
        )
      ))
      retained_locals <- Filter(
        function(local) local@value_id %in% retained_target_ids,
        block@locals
      )
      if (S7::S7_inherits(block, TccqValueBlock)) {
        return(tccq_value_block(
          block@id,
          block@result,
          locals = retained_locals,
          statements = transformed_statements
        ))
      }
      TccqBlock(
        id = block@id,
        locals = retained_locals,
        statements = transformed_statements,
        effect = Reduce(
          tccq_effect_union,
          lapply(transformed_statements, function(statement) statement@effect),
          init = tccq_effect()
        )
      )
    }

    repeat {
      facts <- expression_facts(
        body,
        required_value_ids = required_value_ids
      )
      transformed_body <- transform_block(body, facts)
      if (identical(transformed_body, body)) {
        return(body)
      }
      body <- transformed_body
    }
  }

  rebuild_structured_program <- function(program, transformed_body) {
    storage_slots <- if (is.null(program@storage_plan)) {
      list()
    } else {
      program@storage_plan@slots
    }
    storage_value_ids <- vapply(
      storage_slots,
      function(slot) slot@value_id,
      character(1)
    )
    transformed_facts <- expression_facts(
      transformed_body,
      required_value_ids = storage_value_ids
    )
    live_region_value_ids <- unique(c(
      transformed_facts@expression_value_ids,
      transformed_facts@read_value_ids,
      transformed_facts@required_target_ids,
      program@result
    ))
    transformed_regions <- lapply(program@regions, function(region) {
      tccq_region(
        region@id,
        region@kind,
        values = Filter(
          function(value) value@id %in% live_region_value_ids,
          region@values
        ),
        fusion_groups = region@fusion_groups,
        effect = transformed_body@effect,
        memory_space = region@memory_space,
        touches_rapi = region@touches_rapi,
        attrs = region@attrs
      )
    })
    transformed_schedule <- tccq_program_schedule(
      steps = list(),
      result = program@schedule@result,
      values = program@values,
      body = transformed_body
    )
    tccq_program(
      name = program@name,
      formals = program@formals,
      schedule = transformed_schedule,
      values = program@values,
      regions = transformed_regions,
      result = program@result,
      diagnostics = program@diagnostics,
      call_index = program@call_index,
      storage_plan = program@storage_plan,
      attrs = program@attrs
    )
  }

  verify_dead_store_result <- function(before, after) {
    before_body <- before@schedule@body
    after_body <- after@schedule@body
    storage_slots <- if (is.null(before@storage_plan)) {
      list()
    } else {
      before@storage_plan@slots
    }
    storage_value_ids <- vapply(
      storage_slots,
      function(slot) slot@value_id,
      character(1)
    )
    before_facts <- expression_facts(
      before_body,
      required_value_ids = storage_value_ids
    )
    after_facts <- expression_facts(
      after_body,
      required_value_ids = storage_value_ids
    )
    expected_body <- transform_body(before_body, storage_value_ids)
    expected_program <- rebuild_structured_program(before, expected_body)
    expected_facts <- expression_facts(
      expected_body,
      required_value_ids = storage_value_ids
    )
    removed_statement_ids <- setdiff(
      names(before_facts@statements_by_id),
      names(after_facts@statements_by_id)
    )
    added_statement_ids <- setdiff(
      names(after_facts@statements_by_id),
      names(before_facts@statements_by_id)
    )
    expected_removed_statement_ids <- setdiff(
      names(before_facts@statements_by_id),
      names(expected_facts@statements_by_id)
    )
    unexpected_removed_statement_ids <- setdiff(
      removed_statement_ids,
      expected_removed_statement_ids
    )
    remaining_dead_stores <- Filter(function(statement) {
      assignment_is_dead(statement, after_facts)
    }, after_facts@statements_by_id)

    program_fields_preserved <-
      identical(before@name, after@name) &&
      identical(before@formals, after@formals) &&
      identical(before@values, after@values) &&
      identical(before@result, after@result) &&
      identical(before@diagnostics, after@diagnostics) &&
      identical(before@call_index, after@call_index) &&
      identical(before@storage_plan, after@storage_plan) &&
      identical(before@attrs, after@attrs) &&
      identical(before@schedule@steps, after@schedule@steps) &&
      identical(before@schedule@result, after@schedule@result)
    regions_match_live_program <-
      length(before@regions) == length(after@regions) &&
      all(vapply(seq_along(before@regions), function(position) {
        before_region <- before@regions[[position]]
        after_region <- after@regions[[position]]
        expected_region <- expected_program@regions[[position]]
        identical(before_region@id, after_region@id) &&
          identical(before_region@kind, after_region@kind) &&
          identical(before_region@fusion_groups, after_region@fusion_groups) &&
          identical(before_region@memory_space, after_region@memory_space) &&
          identical(before_region@touches_rapi, after_region@touches_rapi) &&
          identical(before_region@attrs, after_region@attrs) &&
          identical(after_region@effect, after_body@effect) &&
          identical(after_region@values, expected_region@values)
      }, logical(1)))

    list(
      valid = identical(after, expected_program) &&
        program_fields_preserved &&
        regions_match_live_program &&
        length(added_statement_ids) == 0L &&
        length(unexpected_removed_statement_ids) == 0L &&
        length(remaining_dead_stores) == 0L,
      matches_expected_program = identical(after, expected_program),
      program_fields_preserved = program_fields_preserved,
      regions_match_live_program = regions_match_live_program,
      removed_statement_ids = removed_statement_ids,
      added_statement_ids = added_statement_ids,
      unexpected_removed_statement_ids = unexpected_removed_statement_ids,
      remaining_dead_store_ids = names(remaining_dead_stores)
    )
  }

  s7contract::impl_trait(
    TccqProgramOptimization,
    TccqDeadStoreOptimization,
    methods = list(
      optimize = function(optimization, program) {
        if (
          is.null(program@schedule) ||
            !S7::S7_inherits(program@schedule@body, TccqValueBlock)
        ) {
          return(tccq_result(
            success = TRUE,
            value = program,
            diagnostics = list()
          ))
        }
        if (!structured_program_is_supported(program)) {
          return(diagnostic_result(
            "optimization.unsupported_structured_program",
            paste0(
              "Dead-store elimination requires one structured region without ",
              "a linear schedule or fusion groups."
            ),
            program
          ))
        }

        storage_slots <- if (is.null(program@storage_plan)) {
          list()
        } else {
          program@storage_plan@slots
        }
        storage_value_ids <- vapply(
          storage_slots,
          function(slot) slot@value_id,
          character(1)
        )
        facts <- expression_facts(
          program@schedule@body,
          required_value_ids = storage_value_ids
        )
        unsupported_statement <- unsupported_statement_result(facts, program)
        if (!is.null(unsupported_statement)) {
          return(unsupported_statement)
        }

        optimized_program <- tryCatch(
          rebuild_structured_program(
            program,
            transform_body(program@schedule@body, storage_value_ids)
          ),
          tccq_error = identity,
          error = identity
        )
        if (inherits(optimized_program, "tccq_error")) {
          return(tccq_result(
            success = FALSE,
            diagnostics = list(tccq_condition_diagnostic(optimized_program))
          ))
        }
        if (inherits(optimized_program, "error")) {
          return(diagnostic_result(
            "optimization.transformation_failed",
            "Dead-store elimination failed while rebuilding typed control.",
            program,
            data = list(message = conditionMessage(optimized_program))
          ))
        }
        with(
          TccqProgramOptimization,
          tccq_verify_optimization(
            optimization,
            before = program,
            after = optimized_program
          )
        )
      },
      verify = function(optimization, before, after) {
        before_is_structured <-
          !is.null(before@schedule) &&
            S7::S7_inherits(before@schedule@body, TccqValueBlock)
        if (!before_is_structured) {
          if (!identical(before, after)) {
            return(diagnostic_result(
              "optimization.changed_unsupported_program",
              "Dead-store elimination changed a program without a structured body.",
              before
            ))
          }
          return(tccq_result(
            success = TRUE,
            value = after,
            diagnostics = list()
          ))
        }
        after_is_structured <-
          !is.null(after@schedule) &&
            S7::S7_inherits(after@schedule@body, TccqValueBlock)
        if (!after_is_structured) {
          return(diagnostic_result(
            "optimization.verification_failed",
            "Dead-store elimination removed the structured program body.",
            before
          ))
        }
        if (!structured_program_is_supported(before)) {
          return(diagnostic_result(
            "optimization.unsupported_structured_program",
            paste0(
              "Dead-store elimination requires one structured region without ",
              "a linear schedule or fusion groups."
            ),
            before
          ))
        }
        storage_slots <- if (is.null(before@storage_plan)) {
          list()
        } else {
          before@storage_plan@slots
        }
        storage_value_ids <- vapply(
          storage_slots,
          function(slot) slot@value_id,
          character(1)
        )
        before_facts <- expression_facts(
          before@schedule@body,
          required_value_ids = storage_value_ids
        )
        unsupported_statement <- unsupported_statement_result(
          before_facts,
          before
        )
        if (!is.null(unsupported_statement)) {
          return(unsupported_statement)
        }
        verification <- tryCatch(
          verify_dead_store_result(before, after),
          tccq_error = identity,
          error = identity
        )
        if (inherits(verification, "tccq_error")) {
          return(tccq_result(
            success = FALSE,
            diagnostics = list(tccq_condition_diagnostic(verification))
          ))
        }
        if (inherits(verification, "error")) {
          return(diagnostic_result(
            "optimization.verification_failed",
            "Dead-store verification could not rebuild the expected program.",
            before,
            data = list(message = conditionMessage(verification))
          ))
        }
        if (!isTRUE(verification$valid)) {
          return(diagnostic_result(
            "optimization.verification_failed",
            "The dead-store result does not satisfy its preservation contract.",
            before,
            data = verification[-1L]
          ))
        }
        tccq_result(success = TRUE, value = after, diagnostics = list())
      }
    ),
    replace = TRUE
  )
  invisible(TRUE)
}
