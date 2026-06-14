TCCQ_EXPRESSION_KINDS <- c("reference", "literal", "operation")

#' Backend-neutral expression tree
#'
#' `TccqExpression` is the small tree form consumed by source printers. It is
#' built from typed lowered values and resolved operations, so C, Fortran,
#' TinyCC, and later printers do not rediscover expression semantics from raw
#' operation strings.
#'
#' @param id Stable expression id.
#' @param kind Expression kind.
#' @param value_id Lowered value id represented by this expression.
#' @param op Operation name, or `formal`/`literal` for leaves.
#' @param inputs Child expressions.
#' @param type Result type.
#' @param literal Literal payload for literal expressions.
#' @param resolved_op Selected operation implementation for operation
#'   expressions.
#' @param attrs Structured metadata.
#' @export
TccqExpression <- S7::new_class(
  "TccqExpression",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    kind = S7::class_character,
    value_id = S7::class_character,
    op = S7::class_character,
    inputs = S7::class_list,
    type = TccqType,
    literal = S7::new_union(NULL, TccqLiteral),
    resolved_op = S7::new_union(NULL, TccqResolvedOp),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (length(self@op) != 1L || is.na(self@op) || !nzchar(self@op)) {
      problems <- c(problems, "@op must be a single non-empty string")
    }
    if (length(self@kind) != 1L || is.na(self@kind) || !self@kind %in% TCCQ_EXPRESSION_KINDS) {
      problems <- c(problems, "@kind must be one supported expression kind")
    }
    inputs_are_expressions <- vapply(
      self@inputs,
      S7::S7_inherits,
      logical(1),
      class = TccqExpression
    )
    if (!all(inputs_are_expressions)) {
      problems <- c(problems, "@inputs must contain only <TccqExpression> values")
    }
    reference_has_payload <- length(self@inputs) > 0L ||
      !is.null(self@literal) ||
      !is.null(self@resolved_op)
    if (identical(self@kind, "reference") && reference_has_payload) {
      problems <- c(
        problems,
        "reference expressions cannot have inputs, literals, or resolved operations"
      )
    }
    literal_has_invalid_payload <- length(self@inputs) > 0L ||
      is.null(self@literal) ||
      !is.null(self@resolved_op)
    if (identical(self@kind, "literal") && literal_has_invalid_payload) {
      problems <- c(
        problems,
        "literal expressions must have one literal and no inputs or resolved operation"
      )
    }
    operation_has_invalid_payload <- length(self@inputs) == 0L ||
      !is.null(self@literal) ||
      is.null(self@resolved_op)
    if (identical(self@kind, "operation") && operation_has_invalid_payload) {
      problems <- c(
        problems,
        "operation expressions must have inputs, a resolved operation, and no literal"
      )
    }
    if (!is.null(self@literal) && !identical(self@literal@type@base, self@type@base)) {
      problems <- c(problems, "@literal type base must match @type base")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a backend-neutral expression
#'
#' @param id Stable expression id.
#' @param kind Expression kind.
#' @param type Result type.
#' @param value_id Lowered value id represented by this expression.
#' @param op Operation name.
#' @param inputs Child expressions.
#' @param literal Literal payload for literal expressions.
#' @param resolved_op Selected implementation for operation expressions.
#' @param attrs Structured metadata.
#' @export
tccq_expression <- function(
  id,
  kind,
  type,
  value_id = id,
  op = kind,
  inputs = list(),
  literal = NULL,
  resolved_op = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_character_scalar(op, "op")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_list_of(inputs, TccqExpression, "TccqExpression", "inputs")
  .tccq_check_optional_s7(literal, TccqLiteral, "TccqLiteral", "literal")
  .tccq_check_optional_s7(resolved_op, TccqResolvedOp, "TccqResolvedOp", "resolved_op")
  .tccq_check_list(attrs, "attrs")

  TccqExpression(
    id = id,
    kind = kind,
    value_id = value_id,
    op = op,
    inputs = inputs,
    type = type,
    literal = literal,
    resolved_op = resolved_op,
    attrs = attrs
  )
}

#' Build a backend-neutral expression tree from a lowered program
#'
#' @param program Lowered program.
#' @param value_id Result value id to root the expression tree at.
#' @export
tccq_expression_tree <- function(program, value_id = program@result) {
  .tccq_check_s7(program, TccqProgram, "TccqProgram", "program")

  expression_diagnostic <- function(code, message, path, data = list()) {
    tccq_diagnostic(
      code,
      message,
      phase = "expression",
      path = path,
      data = c(list(program = program@name), data)
    )
  }

  diagnostics <- list()
  build_expression <- function(current_value_id, value_stack) {
    current_value_id_is_valid <- is.character(current_value_id) &&
      length(current_value_id) == 1L &&
      !is.na(current_value_id) &&
      nzchar(current_value_id)
    if (!current_value_id_is_valid) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.invalid_value_id",
        "Expression roots and inputs must be non-empty value ids.",
        "expression.value_id",
        data = list(value_id = current_value_id)
      )))
      return(NULL)
    }
    if (current_value_id %in% value_stack) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.cycle",
        "Expression values must form an acyclic tree for source printing.",
        "expression.inputs",
        data = list(value_id = current_value_id, stack = value_stack)
      )))
      return(NULL)
    }

    value <- program@values[[current_value_id]]
    if (is.null(value)) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.unknown_value",
        "Expression value id is not present in the lowered program.",
        "expression.value_id",
        data = list(value_id = current_value_id)
      )))
      return(NULL)
    }

    if (identical(value@op, "formal")) {
      return(tccq_expression(
        id = current_value_id,
        kind = "reference",
        value_id = current_value_id,
        op = value@op,
        type = value@type,
        attrs = list(symbol = value@attrs$symbol)
      ))
    }
    if (identical(value@op, "literal")) {
      literal <- value@attrs$literal
      if (!S7::S7_inherits(literal, TccqLiteral)) {
        diagnostics <<- c(diagnostics, list(expression_diagnostic(
          "expression.invalid_literal",
          "Literal lowered values must carry a <TccqLiteral> payload.",
          "expression.literal",
          data = list(value_id = current_value_id)
        )))
        return(NULL)
      }
      return(tccq_expression(
        id = current_value_id,
        kind = "literal",
        value_id = current_value_id,
        op = value@op,
        type = value@type,
        literal = literal
      ))
    }

    resolved_operation <- value@attrs$resolved_op
    if (!S7::S7_inherits(resolved_operation, TccqResolvedOp)) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.unresolved_operation",
        "Operation lowered values must carry a <TccqResolvedOp> payload.",
        "expression.resolved_op",
        data = list(value_id = current_value_id, op = value@op)
      )))
      return(NULL)
    }
    operation_matches_resolution <- identical(value@op, resolved_operation@call@name) ||
      (identical(value@op, "negate") && identical(resolved_operation@call@name, "-"))
    if (!operation_matches_resolution) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.operation_mismatch",
        "Operation value name must match its resolved call implementation.",
        "expression.resolved_op",
        data = list(
          value_id = current_value_id,
          op = value@op,
          resolved_call = resolved_operation@call@name
        )
      )))
      return(NULL)
    }

    input_expressions <- lapply(
      value@inputs,
      build_expression,
      value_stack = c(value_stack, current_value_id)
    )
    if (any(vapply(input_expressions, is.null, logical(1)))) {
      return(NULL)
    }
    tccq_expression(
      id = current_value_id,
      kind = "operation",
      value_id = current_value_id,
      op = value@op,
      inputs = input_expressions,
      type = value@type,
      resolved_op = resolved_operation,
      attrs = c(list(effect = value@effect), value@attrs)
    )
  }

  if (is.null(value_id)) {
    diagnostic <- expression_diagnostic(
      "expression.missing_result",
      "A backend-neutral expression tree needs a result value id.",
      "expression.result"
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }

  expression <- build_expression(value_id, character())
  if (length(diagnostics) > 0L) {
    return(tccq_result(success = FALSE, diagnostics = diagnostics))
  }
  tccq_result(success = TRUE, value = expression)
}
