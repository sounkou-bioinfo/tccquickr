TCCQ_EXPRESSION_KINDS <- c("reference", "literal", "operation", "branch")
TCCQ_LOOP_CALL_NAMES <- c("for", "while", "repeat")
TCCQ_LOOP_TRANSFER_ACTIONS <- c("break", "next")

#' Typed expression reference
#'
#' A reference names the logical source value read by a neutral expression.
#' It keeps lexical binding, source symbol, slice, and affine access facts out
#' of open-ended expression metadata. Physical allocation remains a separate
#' storage-plan concern.
#'
#' @param source_value_id Logical value id supplying the referenced storage.
#' @param symbol Optional source symbol, or an empty string when not applicable.
#' @param binding Optional lexical binding represented by the reference.
#' @param slice_offsets Zero-based slice offsets, or an empty integer vector.
#' @param access Optional typed domain access attached by loop-nest planning.
#' @export
TccqExpressionReference <- S7::new_class(
  "TccqExpressionReference",
  package = "tccquickr",
  properties = list(
    source_value_id = S7::class_character,
    symbol = S7::class_character,
    binding = S7::new_union(NULL, TccqBinding),
    slice_offsets = S7::class_integer,
    access = S7::new_union(NULL, TccqAccess)
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@source_value_id) != 1L ||
        is.na(self@source_value_id) ||
        !nzchar(self@source_value_id)
    ) {
      problems <- c(problems, "@source_value_id must be one non-empty string")
    }
    if (length(self@symbol) != 1L || is.na(self@symbol)) {
      problems <- c(problems, "@symbol must be one non-missing string")
    }
    if (!is.integer(self@slice_offsets) || anyNA(self@slice_offsets)) {
      problems <- c(problems, "@slice_offsets must be a non-missing integer vector")
    }
    if (
      !is.null(self@binding) &&
        !identical(self@binding@value_id, self@source_value_id)
    ) {
      problems <- c(problems, "@binding value id must match @source_value_id")
    }
    if (
      !is.null(self@access) &&
        !identical(self@access@value_id, self@source_value_id)
    ) {
      problems <- c(problems, "@access value id must match @source_value_id")
    }
    if (length(problems) > 0L) problems
  }
)

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
#' @param effect Effect of evaluating the expression.
#' @param literal Literal payload for literal expressions.
#' @param operation Typed lowered-operation payload for operation expressions.
#' @param branch Typed conditional payload for branch expressions.
#' @param reference Typed source payload for reference expressions.
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
    effect = TccqEffect,
    literal = S7::new_union(NULL, TccqLiteral),
    operation = S7::new_union(NULL, TccqLoweredOperation),
    branch = S7::new_union(NULL, TccqBranch),
    reference = S7::new_union(NULL, TccqExpressionReference)
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
    reference_payload_invalid <- length(self@inputs) > 0L ||
      !is.null(self@literal) ||
      !is.null(self@operation) ||
      !is.null(self@branch) ||
      is.null(self@reference)
    if (identical(self@kind, "reference") && reference_payload_invalid) {
      problems <- c(
        problems,
        "reference expressions need one reference and no inputs, literal, operation, or branch"
      )
    }
    literal_has_invalid_payload <- length(self@inputs) > 0L ||
      is.null(self@literal) ||
      !is.null(self@operation) ||
      !is.null(self@branch) ||
      !is.null(self@reference)
    if (identical(self@kind, "literal") && literal_has_invalid_payload) {
      problems <- c(
        problems,
        "literal expressions must have one literal and no inputs or operation payload"
      )
    }
    operation_has_invalid_payload <- length(self@inputs) == 0L ||
      !is.null(self@literal) ||
      is.null(self@operation) ||
      !is.null(self@branch) ||
      !is.null(self@reference)
    if (identical(self@kind, "operation") && operation_has_invalid_payload) {
      problems <- c(
        problems,
        "operation expressions must have inputs, one operation payload, and no literal"
      )
    }
    branch_has_invalid_payload <- length(self@inputs) != 3L ||
      !is.null(self@literal) ||
      !is.null(self@operation) ||
      is.null(self@branch) ||
      !is.null(self@reference)
    if (identical(self@kind, "branch") && branch_has_invalid_payload) {
      problems <- c(
        problems,
        "branch expressions must have three inputs, a branch payload, and no literal or operation payload"
      )
    }
    if (
      !is.null(self@branch) &&
        all(inputs_are_expressions) &&
        !identical(self@branch@inputs, lapply(self@inputs, function(input) input@value_id))
    ) {
      problems <- c(problems, "@branch incoming ids must match @inputs")
    }
    if (!is.null(self@literal) && !identical(self@literal@type@base, self@type@base)) {
      problems <- c(problems, "@literal type base must match @type base")
    }
    if (
      !is.null(self@operation) &&
        !identical(self@operation@resolved_op@call@name, self@op)
    ) {
      problems <- c(problems, "@operation call name must match @op")
    }
    if (
      !is.null(self@reference) &&
        length(self@reference@slice_offsets) > 0L &&
        length(self@reference@slice_offsets) != self@type@shape@rank
    ) {
      problems <- c(problems, "@reference slice offsets must match @type rank")
    }
    if (
      !is.null(self@reference) &&
        !is.null(self@reference@binding) &&
        !identical(self@reference@binding@type, self@type)
    ) {
      problems <- c(problems, "@reference binding type must match @type")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a typed expression reference
#'
#' @inheritParams TccqExpressionReference
#' @export
tccq_expression_reference <- function(
  source_value_id,
  symbol = "",
  binding = NULL,
  slice_offsets = integer(),
  access = NULL
) {
  .tccq_check_character_scalar(source_value_id, "source_value_id")
  .tccq_check_character_or_empty(symbol, "symbol")
  .tccq_check_optional_s7(binding, TccqBinding, "TccqBinding", "binding")
  if (!is.integer(slice_offsets) || anyNA(slice_offsets)) {
    tccq_abort(
      "schema.invalid_expression_slice_offsets",
      "`slice_offsets` must be a non-missing integer vector.",
      phase = "schema",
      path = "expression_reference.slice_offsets"
    )
  }
  .tccq_check_optional_s7(access, TccqAccess, "TccqAccess", "access")
  TccqExpressionReference(
    source_value_id = source_value_id,
    symbol = symbol,
    binding = binding,
    slice_offsets = slice_offsets,
    access = access
  )
}

#' Construct a backend-neutral expression
#'
#' @param id Stable expression id.
#' @param kind Expression kind.
#' @param type Result type.
#' @param value_id Lowered value id represented by this expression.
#' @param op Operation name.
#' @param inputs Child expressions.
#' @param effect Effect of evaluating the expression.
#' @param literal Literal payload for literal expressions.
#' @param operation Typed lowered-operation payload for operation expressions.
#' @param branch Typed conditional payload for branch expressions.
#' @param reference Typed source payload for reference expressions.
#' @export
tccq_expression <- function(
  id,
  kind,
  type,
  value_id = id,
  op = kind,
  inputs = list(),
  effect = tccq_effect(),
  literal = NULL,
  operation = NULL,
  branch = NULL,
  reference = NULL
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_character_scalar(op, "op")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_list_of(inputs, TccqExpression, "TccqExpression", "inputs")
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  .tccq_check_optional_s7(literal, TccqLiteral, "TccqLiteral", "literal")
  .tccq_check_optional_s7(
    operation,
    TccqLoweredOperation,
    "TccqLoweredOperation",
    "operation"
  )
  .tccq_check_optional_s7(branch, TccqBranch, "TccqBranch", "branch")
  .tccq_check_optional_s7(
    reference,
    TccqExpressionReference,
    "TccqExpressionReference",
    "reference"
  )
  TccqExpression(
    id = id,
    kind = kind,
    value_id = value_id,
    op = op,
    inputs = inputs,
    type = type,
    effect = effect,
    literal = literal,
    operation = operation,
    branch = branch,
    reference = reference
  )
}

#' Build a backend-neutral expression tree from a lowered value graph
#'
#' @param graph Lowered value graph.
#' @param value_id Result value id to root the expression tree at.
#' @export
tccq_expression_tree <- S7::new_generic(
  "tccq_expression_tree",
  dispatch_args = "graph",
  function(graph, value_id = NULL) S7::S7_dispatch()
)

S7::method(tccq_expression_tree, TccqValueGraph) <- function(graph, value_id = NULL) {
  if (is.null(value_id)) {
    value_id <- graph@result
  }
  graph_name <- if (S7::S7_inherits(graph, TccqProgram)) graph@name else "lowering"

  expression_diagnostic <- function(code, message, path, data = list()) {
    tccq_diagnostic(
      code,
      message,
      phase = "expression",
      path = path,
      data = c(list(program = graph_name), data)
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

    value <- graph@values[[current_value_id]]
    if (is.null(value)) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.unknown_value",
        "Expression value id is not present in the lowered program.",
        "expression.value_id",
        data = list(value_id = current_value_id)
      )))
      return(NULL)
    }

    if (value@op %in% c("formal", "dim_symbol")) {
      return(tccq_expression(
        id = current_value_id,
        kind = "reference",
        value_id = current_value_id,
        op = value@op,
        type = value@type,
        effect = value@effect,
        reference = tccq_expression_reference(
          current_value_id,
          symbol = value@attrs$symbol
        )
      ))
    }
    if (S7::S7_inherits(value, TccqBindingReference)) {
      return(tccq_expression(
        id = current_value_id,
        kind = "reference",
        value_id = current_value_id,
        op = "local",
        type = value@type,
        effect = value@effect,
        reference = tccq_expression_reference(
          value@binding@value_id,
          binding = value@binding
        )
      ))
    }
    if (S7::S7_inherits(value, TccqCellReference)) {
      return(tccq_expression(
        id = current_value_id,
        kind = "reference",
        value_id = current_value_id,
        op = "cell",
        type = value@type,
        effect = value@effect,
        reference = tccq_expression_reference(
          value@cell@value_id,
          binding = value@cell
        )
      ))
    }
    if (identical(value@op, "[")) {
      slice_offsets <- integer()
      source_value <- value
      while (identical(source_value@op, "[")) {
        current_offsets <- as.integer(source_value@attrs$slice_offsets %||% integer())
        slice_offsets <- if (length(slice_offsets) == 0L) {
          current_offsets
        } else {
          slice_offsets + current_offsets
        }
        source_value <- graph@values[[source_value@inputs[[1L]]]]
        if (is.null(source_value)) {
          diagnostics <<- c(diagnostics, list(expression_diagnostic(
            "expression.unknown_value",
            "Slice source value id is not present in the lowered program.",
            "expression.value_id",
            data = list(value_id = current_value_id)
          )))
          return(NULL)
        }
      }
      if (!identical(source_value@op, "formal")) {
        diagnostics <<- c(diagnostics, list(expression_diagnostic(
          "expression.slice_of_computed_value",
          "Slices of computed values are not lowerable without materialization yet.",
          "expression.slice",
          data = list(value_id = current_value_id, source_op = source_value@op)
        )))
        return(NULL)
      }
      return(tccq_expression(
        id = current_value_id,
        kind = "reference",
        value_id = current_value_id,
        op = "formal",
        type = value@type,
        effect = value@effect,
        reference = tccq_expression_reference(
          source_value@id,
          symbol = source_value@attrs$symbol,
          slice_offsets = slice_offsets
        )
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
        effect = value@effect,
        literal = literal
      ))
    }
    if (S7::S7_inherits(value, TccqBranch)) {
      input_expressions <- lapply(
        value@inputs,
        build_expression,
        value_stack = c(value_stack, current_value_id)
      )
      if (any(vapply(input_expressions, is.null, logical(1)))) {
        return(NULL)
      }
      return(tccq_expression(
        id = current_value_id,
        kind = "branch",
        value_id = current_value_id,
        op = value@op,
        inputs = input_expressions,
        type = value@type,
        effect = value@effect,
        branch = value
      ))
    }

    lowered_operation <- value@attrs$operation
    if (!S7::S7_inherits(lowered_operation, TccqLoweredOperation)) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.missing_lowered_operation",
        "Operation lowered values must carry a <TccqLoweredOperation> payload.",
        "expression.operation",
        data = list(value_id = current_value_id, op = value@op)
      )))
      return(NULL)
    }

    resolved_operation <- lowered_operation@resolved_op
    if (!S7::S7_inherits(resolved_operation, TccqResolvedOp)) {
      diagnostics <<- c(diagnostics, list(expression_diagnostic(
        "expression.unresolved_operation",
        "Operation lowered values must carry a <TccqResolvedOp> payload.",
        "expression.resolved_op",
        data = list(value_id = current_value_id, op = value@op)
      )))
      return(NULL)
    }
    operation_matches_resolution <- identical(value@op, resolved_operation@call@name)
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
      effect = value@effect,
      operation = lowered_operation
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

#' Mutable lexical cell
#'
#' A cell is storage for loop-carried scalar state. It is deliberately distinct
#' from [TccqLocalBinding], whose identity remains one immutable SSA
#' definition. Source names are assigned later by the backend interface.
#'
#' @inheritParams TccqBinding
#' @param value_id Stable storage identity.
#' @param storage_type Scalar storage type.
#' @export
TccqCell <- S7::new_class(
  "TccqCell",
  package = "tccquickr",
  parent = TccqBinding,
  properties = list(
    value_id = S7::class_character,
    storage_type = TccqType
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (!isTRUE(self@mutable)) {
      problems <- c(problems, "cells must be mutable bindings")
    }
    if (self@type@shape@rank != 0L || self@storage_type@shape@rank != 0L) {
      problems <- c(problems, "cells currently require scalar semantic and storage types")
    }
    if (!identical(self@type@base, self@storage_type@base)) {
      problems <- c(problems, "@type and @storage_type must have the same base type")
    }
    if (length(problems) > 0L) problems
  }
)

#' Lowered mutable-cell read
#'
#' @inheritParams TccqValue
#' @param cell Mutable cell being read.
#' @export
TccqCellReference <- S7::new_class(
  "TccqCellReference",
  package = "tccquickr",
  parent = TccqValue,
  properties = list(cell = TccqCell),
  validator = function(self) {
    problems <- character()
    if (!identical(self@op, "cell_reference")) {
      problems <- c(problems, "cell references must use the `cell_reference` operation")
    }
    if (length(self@inputs) != 0L) {
      problems <- c(problems, "cell references do not own value-graph inputs")
    }
    if (!identical(self@type, self@cell@type)) {
      problems <- c(problems, "@type must match the referenced cell")
    }
    if (
      !isTRUE(self@effect@reads) ||
        isTRUE(self@effect@writes) ||
        isTRUE(self@effect@allocates) ||
        isTRUE(self@effect@boundary) ||
        isTRUE(self@effect@may_error) ||
        isTRUE(self@effect@may_warn)
    ) {
      problems <- c(problems, "cell references must be read-only effects")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a mutable lexical cell
#'
#' @inheritParams TccqCell
#' @export
tccq_cell <- function(name, value_id, type, storage_type = tccq_type(type@base)) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_s7(storage_type, TccqType, "TccqType", "storage_type")
  TccqCell(
    name = name,
    type = type,
    mutable = TRUE,
    value_id = value_id,
    storage_type = storage_type
  )
}

#' Construct a lowered mutable-cell read
#'
#' @param id Stable value id for this read.
#' @param cell Mutable cell being read.
#' @export
tccq_cell_reference <- function(id, cell) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(cell, TccqCell, "TccqCell", "cell")
  TccqCellReference(
    id = id,
    op = "cell_reference",
    inputs = list(),
    type = cell@type,
    effect = tccq_effect(reads = TRUE),
    attrs = list(),
    cell = cell
  )
}

TCCQ_WRITE_TARGET_KINDS <- c("local", "result", "cell")

#' Neutral write target
#'
#' A write target names a typed value destination without spelling it in C,
#' Fortran, or another target language. Result targets are resolved through a
#' loop nest's output plan; local targets are mapped to generated names by
#' [TccqBackendFunctionInterface]. The semantic type retains the value domain,
#' while the scalar storage type describes the element written at this target.
#'
#' @param value_id Stable value id written by the target.
#' @param type Semantic value type.
#' @param storage_type Scalar storage type written at the target.
#' @param kind Target kind: `local`, `result`, or `cell`.
#' @param binding Optional binding owned by the target.
#' @export
TccqWriteTarget <- S7::new_class(
  "TccqWriteTarget",
  package = "tccquickr",
  properties = list(
    value_id = S7::class_character,
    type = TccqType,
    storage_type = TccqType,
    kind = S7::class_character,
    binding = S7::new_union(NULL, TccqBinding)
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (length(self@kind) != 1L || is.na(self@kind) || !self@kind %in% TCCQ_WRITE_TARGET_KINDS) {
      problems <- c(problems, "@kind must be one supported write-target kind")
    }
    if (self@storage_type@shape@rank != 0L) {
      problems <- c(problems, "@storage_type must be scalar")
    }
    if (!identical(self@storage_type@base, self@type@base)) {
      problems <- c(problems, "@storage_type and @type must have the same base type")
    }
    if (identical(self@kind, "cell")) {
      if (!S7::S7_inherits(self@binding, TccqCell)) {
        problems <- c(problems, "cell targets must carry their <TccqCell> binding")
      } else if (
        !identical(self@value_id, self@binding@value_id) ||
          !identical(self@type, self@binding@type) ||
          !identical(self@storage_type, self@binding@storage_type)
      ) {
        problems <- c(problems, "cell target facts must match the bound cell")
      }
    } else if (!is.null(self@binding)) {
      problems <- c(problems, "only cell targets may carry a binding")
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral statement
#'
#' Base class for typed statements consumed by source backends. Concrete
#' statement classes own their operands and control structure.
#'
#' @param id Stable statement id.
#' @param effect Effect summary of evaluating the statement.
#' @export
TccqStatement <- S7::new_class(
  "TccqStatement",
  package = "tccquickr",
  abstract = TRUE,
  properties = list(
    id = S7::class_character,
    effect = TccqEffect
  ),
  validator = function(self) {
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      "@id must be a single non-empty string"
    }
  }
)

#' Neutral statement block
#'
#' A block owns lexical local declarations and ordered typed statements. Its
#' nesting is semantic: locals and statements inside one branch arm are not
#' evaluated from another arm. A plain block does not imply that evaluation
#' yields a value; [TccqValueBlock] adds that stronger completion contract.
#'
#' @param id Stable block id.
#' @param locals Ordered local write targets declared by the block.
#' @param statements Ordered `TccqStatement` values.
#' @param effect Exact effect summary of evaluating the statements.
#' @export
TccqBlock <- S7::new_class(
  "TccqBlock",
  package = "tccquickr",
  parent = TccqProgramBody,
  properties = list(
    id = S7::class_character,
    locals = S7::class_list,
    statements = S7::class_list,
    effect = TccqEffect
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    locals_are_targets <- vapply(
      self@locals,
      S7::S7_inherits,
      logical(1),
      class = TccqWriteTarget
    )
    if (!all(locals_are_targets)) {
      problems <- c(problems, "@locals must contain only <TccqWriteTarget> values")
    }
    if (all(locals_are_targets)) {
      local_kinds <- vapply(self@locals, function(local) local@kind, character(1))
      local_value_ids <- vapply(self@locals, function(local) local@value_id, character(1))
      if (any(!local_kinds %in% c("local", "cell"))) {
        problems <- c(problems, "@locals must contain local or cell write targets")
      }
      if (anyDuplicated(local_value_ids)) {
        problems <- c(problems, "@locals must have unique value ids")
      }
    }
    statements_are_typed <- vapply(
      self@statements,
      S7::S7_inherits,
      logical(1),
      class = TccqStatement
    )
    if (!all(statements_are_typed)) {
      problems <- c(problems, "@statements must contain only <TccqStatement> values")
    }
    if (all(statements_are_typed)) {
      statement_ids <- vapply(self@statements, function(statement) statement@id, character(1))
      if (anyDuplicated(statement_ids)) {
        problems <- c(problems, "@statements must have unique ids")
      }
      statement_effect <- Reduce(
        tccq_effect_union,
        lapply(self@statements, function(statement) statement@effect),
        init = tccq_effect()
      )
      if (!identical(self@effect, statement_effect)) {
        problems <- c(problems, "@effect must equal the union of statement effects")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Value-producing neutral statement block
#'
#' A value block strengthens [TccqBlock] by requiring its terminal statement to
#' write one typed result target on every path. Array loop nests and reducers
#' consume this subtype; future procedural control can use the base block
#' without fabricating a result value.
#'
#' @inheritParams TccqBlock
#' @param result Typed target produced on every terminal path through the block.
#' @export
TccqValueBlock <- S7::new_class(
  "TccqValueBlock",
  package = "tccquickr",
  parent = TccqBlock,
  properties = list(result = TccqWriteTarget),
  validator = function(self) {
    problems <- character()
    if (length(self@statements) == 0L) {
      problems <- c(problems, "@statements must end in a statement producing @result")
    } else {
      terminal_statement <- self@statements[[length(self@statements)]]
      terminal_produces_result <- if (S7::S7_inherits(terminal_statement, TccqAssignment)) {
        identical(terminal_statement@target, self@result)
      } else if (S7::S7_inherits(terminal_statement, TccqConditional)) {
        identical(terminal_statement@consequent@result, self@result) &&
          identical(terminal_statement@alternative@result, self@result)
      } else {
        FALSE
      }
      if (!terminal_produces_result) {
        problems <- c(problems, "the terminal statement must produce @result on every path")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral assignment statement
#'
#' @inheritParams TccqStatement
#' @param target Typed destination.
#' @param value Backend-neutral value expression.
#' @export
TccqAssignment <- S7::new_class(
  "TccqAssignment",
  package = "tccquickr",
  parent = TccqStatement,
  properties = list(
    target = TccqWriteTarget,
    value = TccqExpression
  ),
  validator = function(self) {
    problems <- character()
    if (!identical(self@target@type, self@value@type)) {
      problems <- c(problems, "@target and @value must have identical semantic types")
    }
    expected_effect <- if (identical(self@target@kind, "cell")) {
      tccq_effect_union(self@value@effect, tccq_effect(writes = TRUE))
    } else {
      self@value@effect
    }
    if (!identical(self@effect, expected_effect)) {
      problems <- c(problems, "@effect must match the assignment target and expression")
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral procedural if statement
#'
#' @inheritParams TccqStatement
#' @param condition Scalar logical condition expression.
#' @param consequent Block evaluated when the condition is true.
#' @param alternative Block evaluated when the condition is false.
#' @param semantics Evaluator facts for the originating `if` special form.
#' @export
TccqIf <- S7::new_class(
  "TccqIf",
  package = "tccquickr",
  parent = TccqStatement,
  properties = list(
    condition = TccqExpression,
    consequent = TccqBlock,
    alternative = TccqBlock,
    semantics = TccqCallSemantics
  ),
  validator = function(self) {
    problems <- character()
    if (
      !identical(self@condition@type@base, "logical") ||
        self@condition@type@shape@rank != 0L
    ) {
      problems <- c(problems, "@condition must be a scalar logical expression")
    }
    if (
      !identical(self@semantics@call@name, "if") ||
        !isTRUE(self@semantics@control) ||
        !identical(self@semantics@forcing_policy, "special")
    ) {
      problems <- c(problems, "@semantics must describe the R `if` special form")
    }
    expected_effect <- Reduce(
      tccq_effect_union,
      list(
        self@condition@effect,
        self@consequent@effect,
        self@alternative@effect,
        tccq_effect(may_error = TRUE)
      ),
      init = tccq_effect()
    )
    if (!identical(self@effect, expected_effect)) {
      problems <- c(problems, "@effect must include the condition, both arms, and condition error")
    }
    if (length(problems) > 0L) problems
  }
)

#' Value-producing neutral conditional
#'
#' This is the stricter value-producing form of `TccqIf`. Both arms must be
#' value blocks writing the same target, and the retained [TccqBranch] must
#' agree with the procedural control facts. The branch retains the effect of
#' the source value; the statement effect covers only work left in its
#' normalized arms after reductions or contractions have been extracted.
#'
#' @inheritParams TccqIf
#' @param branch Source branch payload retaining R special-form semantics.
#' @export
TccqConditional <- S7::new_class(
  "TccqConditional",
  package = "tccquickr",
  parent = TccqIf,
  properties = list(branch = TccqBranch),
  validator = function(self) {
    problems <- character()
    if (
      !S7::S7_inherits(self@consequent, TccqValueBlock) ||
        !S7::S7_inherits(self@alternative, TccqValueBlock)
    ) {
      problems <- c(problems, "value conditionals require value-producing arm blocks")
    } else if (!identical(self@consequent@result, self@alternative@result)) {
      problems <- c(problems, "both branch blocks must produce the same result target")
    }
    if (!identical(self@condition@value_id, self@branch@condition)) {
      problems <- c(problems, "@condition value id must match @branch condition")
    }
    if (!identical(self@semantics, self@branch@semantics)) {
      problems <- c(problems, "@semantics must match the source branch payload")
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral sequential loop
#'
#' Abstract parent for sequential loops whose body is a typed statement block.
#' Concrete loop classes add their own entry and completion rules.
#'
#' @inheritParams TccqStatement
#' @param body Procedural loop body.
#' @param semantics Evaluator facts for the originating R loop special form.
#' @export
TccqLoop <- S7::new_class(
  "TccqLoop",
  package = "tccquickr",
  parent = TccqStatement,
  abstract = TRUE,
  properties = list(
    body = TccqBlock,
    semantics = TccqCallSemantics
  ),
  validator = function(self) {
    if (
      !self@semantics@call@name %in% TCCQ_LOOP_CALL_NAMES ||
        !isTRUE(self@semantics@control) ||
        !identical(self@semantics@forcing_policy, "special")
    ) {
      "@semantics must describe an R loop special form"
    }
  }
)

#' Neutral while statement
#'
#' This is sequential recurrence, not a [TccqLoopNest]. The condition is
#' re-evaluated before every iteration and the body may update explicitly
#' declared mutable cells.
#'
#' @inheritParams TccqLoop
#' @param condition Scalar logical condition expression.
#' @export
TccqWhile <- S7::new_class(
  "TccqWhile",
  package = "tccquickr",
  parent = TccqLoop,
  properties = list(condition = TccqExpression),
  validator = function(self) {
    problems <- character()
    if (
      !identical(self@condition@type@base, "logical") ||
        self@condition@type@shape@rank != 0L
    ) {
      problems <- c(problems, "@condition must be a scalar logical expression")
    }
    if (
      !identical(self@semantics@call@name, "while") ||
        !isTRUE(self@semantics@control) ||
        !identical(self@semantics@forcing_policy, "special")
    ) {
      problems <- c(problems, "@semantics must describe the R `while` special form")
    }
    expected_effect <- Reduce(
      tccq_effect_union,
      list(
        self@condition@effect,
        self@body@effect,
        tccq_effect(may_error = TRUE)
      ),
      init = tccq_effect()
    )
    if (!identical(self@effect, expected_effect)) {
      problems <- c(problems, "@effect must include the condition, body, and condition error")
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral repeat statement
#'
#' A repeat loop enters its body unconditionally and relies on a typed loop
#' transfer to terminate or begin another iteration.
#'
#' @inheritParams TccqLoop
#' @export
TccqRepeat <- S7::new_class(
  "TccqRepeat",
  package = "tccquickr",
  parent = TccqLoop,
  validator = function(self) {
    problems <- character()
    if (!identical(self@semantics@call@name, "repeat")) {
      problems <- c(problems, "@semantics must describe the R `repeat` special form")
    }
    if (!identical(self@effect, self@body@effect)) {
      problems <- c(problems, "@effect must match the repeat body")
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral for statement
#'
#' A for loop evaluates one typed iterable and assigns each selected element to
#' its iteration cell before evaluating the body. The iteration domain and
#' access are explicit so source backends do not reconstruct traversal from R
#' syntax. The current concrete slice accepts rank-1 atomic iterables.
#'
#' @inheritParams TccqLoop
#' @param iterator Mutable scalar destination receiving each element.
#' @param iterable Typed iterable expression carrying its domain access.
#' @param domain Iteration domain evaluated by the loop.
#' @export
TccqFor <- S7::new_class(
  "TccqFor",
  package = "tccquickr",
  parent = TccqLoop,
  properties = list(
    iterator = TccqWriteTarget,
    iterable = TccqExpression,
    domain = TccqDomain
  ),
  validator = function(self) {
    problems <- character()
    if (!identical(self@semantics@call@name, "for")) {
      problems <- c(problems, "@semantics must describe the R `for` special form")
    }
    if (
      !identical(self@iterator@kind, "cell") ||
        !S7::S7_inherits(self@iterator@binding, TccqCell) ||
        self@iterator@type@shape@rank != 0L
    ) {
      problems <- c(problems, "@iterator must be a mutable scalar cell target")
    }
    if (self@iterable@type@shape@rank != 1L) {
      problems <- c(problems, "@iterable must currently be a rank-1 value")
    }
    if (!identical(self@domain@shape, self@iterable@type@shape)) {
      problems <- c(problems, "@domain shape must match the iterable shape")
    }
    if (!identical(self@iterator@type@base, self@iterable@type@base)) {
      problems <- c(problems, "@iterator and @iterable must have the same base type")
    }
    access <- if (is.null(self@iterable@reference)) {
      NULL
    } else {
      self@iterable@reference@access
    }
    if (
      !S7::S7_inherits(access, TccqAccess) ||
        !identical(access@domain, self@domain) ||
        !identical(access@kind, "identity")
    ) {
      problems <- c(problems, "@iterable must carry an identity access over @domain")
    }
    if (
      S7::S7_inherits(access, TccqAccess) &&
        self@domain@shape@rank == 1L &&
        length(self@domain@axes) == 1L
    ) {
      exact_identity <- length(access@index_map) == 1L &&
        identical(access@index_map[[1L]]@axis, self@domain@axes[[1L]]) &&
        identical(access@index_map[[1L]]@offset, 0L)
      if (!exact_identity) {
        problems <- c(problems, "@iterable identity access must select the current domain element")
      }
    }
    expected_effect <- Reduce(
      tccq_effect_union,
      list(
        self@iterable@effect,
        self@body@effect,
        tccq_effect(writes = TRUE)
      ),
      init = tccq_effect()
    )
    if (!identical(self@effect, expected_effect)) {
      problems <- c(problems, "@effect must include iterable evaluation, iterator writes, and body effects")
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral loop transfer statement
#'
#' A transfer is structured control completion, not a read/write side effect.
#' Its action always targets the nearest enclosing sequential loop.
#'
#' @inheritParams TccqStatement
#' @param action Loop action, either `break` or `next`.
#' @param semantics Evaluator facts for the originating R control special form.
#' @export
TccqLoopTransfer <- S7::new_class(
  "TccqLoopTransfer",
  package = "tccquickr",
  parent = TccqStatement,
  properties = list(
    action = S7::class_character,
    semantics = TccqCallSemantics
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@action) != 1L ||
        is.na(self@action) ||
        !self@action %in% TCCQ_LOOP_TRANSFER_ACTIONS
    ) {
      problems <- c(problems, "@action must be `break` or `next`")
    }
    if (
      !identical(self@semantics@call@name, self@action) ||
        !isTRUE(self@semantics@control) ||
        !identical(self@semantics@forcing_policy, "special")
    ) {
      problems <- c(problems, "@semantics must describe the selected R loop transfer")
    }
    if (!identical(self@effect, tccq_effect())) {
      problems <- c(problems, "loop transfers must not fabricate ordinary effects")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a neutral write target
#'
#' @inheritParams TccqWriteTarget
#' @export
tccq_write_target <- function(
  value_id,
  type,
  storage_type = tccq_type(type@base),
  kind = "local",
  binding = NULL
) {
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_s7(storage_type, TccqType, "TccqType", "storage_type")
  .tccq_check_character_scalar(kind, "kind")
  .tccq_check_optional_s7(binding, TccqBinding, "TccqBinding", "binding")
  TccqWriteTarget(
    value_id = value_id,
    type = type,
    storage_type = storage_type,
    kind = kind,
    binding = binding
  )
}

#' Construct a value-producing neutral statement block
#'
#' @inheritParams TccqValueBlock
#' @export
tccq_value_block <- function(
  id,
  result,
  locals = list(),
  statements = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(result, TccqWriteTarget, "TccqWriteTarget", "result")
  .tccq_check_list_of(locals, TccqWriteTarget, "TccqWriteTarget", "locals")
  .tccq_check_list_of(statements, TccqStatement, "TccqStatement", "statements")
  effect <- Reduce(
    tccq_effect_union,
    lapply(statements, function(statement) statement@effect),
    init = tccq_effect()
  )
  TccqValueBlock(
    id = id,
    locals = locals,
    statements = statements,
    result = result,
    effect = effect
  )
}

#' Construct a neutral assignment
#'
#' @inheritParams TccqAssignment
#' @export
tccq_assignment <- function(id, target, value) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(target, TccqWriteTarget, "TccqWriteTarget", "target")
  .tccq_check_s7(value, TccqExpression, "TccqExpression", "value")
  effect <- if (identical(target@kind, "cell")) {
    tccq_effect_union(value@effect, tccq_effect(writes = TRUE))
  } else {
    value@effect
  }
  TccqAssignment(id = id, effect = effect, target = target, value = value)
}

#' Construct a neutral procedural if
#'
#' @inheritParams TccqIf
#' @export
tccq_if <- function(id, condition, consequent, alternative, semantics) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(condition, TccqExpression, "TccqExpression", "condition")
  .tccq_check_s7(consequent, TccqBlock, "TccqBlock", "consequent")
  .tccq_check_s7(alternative, TccqBlock, "TccqBlock", "alternative")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  effect <- Reduce(
    tccq_effect_union,
    list(
      condition@effect,
      consequent@effect,
      alternative@effect,
      tccq_effect(may_error = TRUE)
    ),
    init = tccq_effect()
  )
  TccqIf(
    id = id,
    effect = effect,
    condition = condition,
    consequent = consequent,
    alternative = alternative,
    semantics = semantics
  )
}

#' Construct a value-producing neutral conditional
#'
#' @inheritParams TccqConditional
#' @export
tccq_conditional <- function(
  id,
  condition,
  consequent,
  alternative,
  branch
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(condition, TccqExpression, "TccqExpression", "condition")
  .tccq_check_s7(consequent, TccqValueBlock, "TccqValueBlock", "consequent")
  .tccq_check_s7(alternative, TccqValueBlock, "TccqValueBlock", "alternative")
  .tccq_check_s7(branch, TccqBranch, "TccqBranch", "branch")
  effect <- Reduce(
    tccq_effect_union,
    list(
      condition@effect,
      consequent@effect,
      alternative@effect,
      tccq_effect(may_error = TRUE)
    ),
    init = tccq_effect()
  )
  TccqConditional(
    id = id,
    effect = effect,
    condition = condition,
    consequent = consequent,
    alternative = alternative,
    semantics = branch@semantics,
    branch = branch
  )
}

#' Construct a neutral while statement
#'
#' @inheritParams TccqWhile
#' @export
tccq_while <- function(id, condition, body, semantics) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(condition, TccqExpression, "TccqExpression", "condition")
  .tccq_check_s7(body, TccqBlock, "TccqBlock", "body")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  effect <- Reduce(
    tccq_effect_union,
    list(condition@effect, body@effect, tccq_effect(may_error = TRUE)),
    init = tccq_effect()
  )
  TccqWhile(
    id = id,
    effect = effect,
    condition = condition,
    body = body,
    semantics = semantics
  )
}

#' Construct a neutral repeat statement
#'
#' @inheritParams TccqRepeat
#' @export
tccq_repeat <- function(id, body, semantics) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(body, TccqBlock, "TccqBlock", "body")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  TccqRepeat(id = id, effect = body@effect, body = body, semantics = semantics)
}

#' Construct a neutral for statement
#'
#' @inheritParams TccqFor
#' @export
tccq_for <- function(id, iterator, iterable, domain, body, semantics) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(iterator, TccqWriteTarget, "TccqWriteTarget", "iterator")
  .tccq_check_s7(iterable, TccqExpression, "TccqExpression", "iterable")
  .tccq_check_s7(domain, TccqDomain, "TccqDomain", "domain")
  .tccq_check_s7(body, TccqBlock, "TccqBlock", "body")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  TccqFor(
    id = id,
    effect = Reduce(
      tccq_effect_union,
      list(iterable@effect, body@effect, tccq_effect(writes = TRUE)),
      init = tccq_effect()
    ),
    body = body,
    semantics = semantics,
    iterator = iterator,
    iterable = iterable,
    domain = domain
  )
}

#' Construct a neutral loop transfer
#'
#' @inheritParams TccqLoopTransfer
#' @export
tccq_loop_transfer <- function(id, action, semantics) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(action, "action")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  TccqLoopTransfer(
    id = id,
    effect = tccq_effect(),
    action = action,
    semantics = semantics
  )
}

#' Selected-arm loop guard
#'
#' A loop guard records one branch decision that must select a loop nest. An
#' ordered list of guards is a control path from outermost to innermost branch;
#' source backends nest those guards around the same `TccqLoopNest` plan.
#'
#' @param condition Scalar logical condition expression.
#' @param branch Source branch payload retaining R special-form semantics.
#' @param selected Whether the consequent (`TRUE`) or alternative (`FALSE`) arm
#'   selects the guarded loop nest.
#' @export
TccqLoopGuard <- S7::new_class(
  "TccqLoopGuard",
  package = "tccquickr",
  properties = list(
    condition = TccqExpression,
    branch = TccqBranch,
    selected = S7::class_logical
  ),
  validator = function(self) {
    problems <- character()
    if (
      !identical(self@condition@type@base, "logical") ||
        self@condition@type@shape@rank != 0L
    ) {
      problems <- c(problems, "@condition must be a scalar logical expression")
    }
    if (!identical(self@condition@value_id, self@branch@condition)) {
      problems <- c(problems, "@condition value id must match @branch condition")
    }
    if (length(self@selected) != 1L || is.na(self@selected)) {
      problems <- c(problems, "@selected must be one non-missing logical value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a selected-arm loop guard
#'
#' @inheritParams TccqLoopGuard
#' @export
tccq_loop_guard <- function(condition, branch, selected) {
  .tccq_check_s7(condition, TccqExpression, "TccqExpression", "condition")
  .tccq_check_s7(branch, TccqBranch, "TccqBranch", "branch")
  .tccq_check_logical_scalar(selected, "selected")
  TccqLoopGuard(condition = condition, branch = branch, selected = selected)
}

#' Loop-nest program plan
#'
#' `TccqLoopNest` is the single backend-neutral iteration plan consumed by
#' source printers, in the spirit of the SAC with-loop. One loop nest carries
#' ordered typed axes (`map` axes produce output positions, `reduce` axes fold
#' into an accumulator), a value expression or typed statement block whose
#' references carry typed affine accesses, an optional reducer with its
#' identity, and an output access. Elementwise maps, full and per-axis
#' reductions, contractions, stencils, and control-valued results are all
#' instances of this one value; printers must not reintroduce per-family loop
#' shapes or backend-local control trees.
#'
#' @param id Stable loop-nest id.
#' @param domain Iteration domain naming the axes.
#' @param axes Ordered `TccqLoopAxis` values, outermost first.
#' @param body Body expression or typed statement block with access-carrying
#'   references.
#' @param output Output access over map axes, or `NULL` for scalar results.
#' @param reducer Reduction metadata for reduce axes, or `NULL`.
#' @param identity Reducer identity literal, or `NULL`.
#' @param accumulator Typed scalar accumulator target, or `NULL`.
#' @param storage Typed storage slot receiving the nest result.
#' @param guards Ordered `TccqLoopGuard` control path selecting this nest.
#' @export
TccqLoopNest <- S7::new_class(
  "TccqLoopNest",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    domain = TccqDomain,
    axes = S7::class_list,
    body = S7::new_union(TccqExpression, TccqValueBlock),
    output = S7::new_union(NULL, TccqAccess),
    reducer = S7::new_union(NULL, TccqReductionSpec),
    identity = S7::new_union(NULL, TccqLiteral),
    accumulator = S7::new_union(NULL, TccqWriteTarget),
    storage = TccqStorageSlot,
    guards = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    axes_are_typed <- vapply(self@axes, S7::S7_inherits, logical(1), class = TccqLoopAxis)
    if (!all(axes_are_typed)) {
      problems <- c(problems, "@axes must contain only <TccqLoopAxis> values")
    }
    if (all(axes_are_typed)) {
      axis_names <- vapply(self@axes, function(axis) axis@name, character(1))
      if (anyDuplicated(axis_names)) {
        problems <- c(problems, "@axes must have unique names")
      }
      if (!identical(self@domain@axes, axis_names)) {
        problems <- c(problems, "@domain axes must match @axes names in order")
      }
      axis_roles <- vapply(self@axes, function(axis) axis@role, character(1))
      has_reduce_axes <- any(axis_roles == "reduce")
      if (has_reduce_axes && !S7::S7_inherits(self@reducer, TccqReductionSpec)) {
        problems <- c(problems, "loop nests with reduce axes must carry a reducer")
      }
      if (!has_reduce_axes && !is.null(self@reducer)) {
        problems <- c(problems, "loop nests without reduce axes cannot carry a reducer")
      }
      if (!is.null(self@output)) {
        map_axis_names <- axis_names[axis_roles == "map"]
        output_axes <- vapply(self@output@index_map, function(index) index@axis, character(1))
        if (length(setdiff(setdiff(output_axes, ""), map_axis_names)) > 0L) {
          problems <- c(problems, "@output may only index map axes")
        }
      }
    }
    reducer_present <- S7::S7_inherits(self@reducer, TccqReductionSpec)
    identity_present <- S7::S7_inherits(self@identity, TccqLiteral)
    if (reducer_present != identity_present) {
      problems <- c(problems, "@reducer and @identity must be present together")
    }
    accumulator_present <- S7::S7_inherits(self@accumulator, TccqWriteTarget)
    if (reducer_present != accumulator_present) {
      problems <- c(problems, "@reducer and @accumulator must be present together")
    }
    if (accumulator_present) {
      if (!identical(self@accumulator@kind, "local")) {
        problems <- c(problems, "@accumulator must be a local write target")
      }
      if (self@accumulator@type@shape@rank != 0L) {
        problems <- c(problems, "@accumulator must have scalar semantic type")
      }
      if (!identical(self@accumulator@type@base, self@storage@type@base)) {
        problems <- c(problems, "@accumulator and @storage must have the same base type")
      }
    }
    if (!self@storage@role %in% c("temporary", "output")) {
      problems <- c(problems, "@storage must be a temporary or output slot")
    }
    if (!isTRUE(self@storage@materialized)) {
      problems <- c(problems, "@storage must describe a materialized result")
    }
    guards_are_typed <- vapply(
      self@guards,
      S7::S7_inherits,
      logical(1),
      class = TccqLoopGuard
    )
    if (!all(guards_are_typed)) {
      problems <- c(problems, "@guards must contain only <TccqLoopGuard> values")
    }
    if (self@storage@type@shape@rank > 0L && is.null(self@output)) {
      problems <- c(problems, "non-scalar loop nests must carry an output access")
    }
    if (self@storage@type@shape@rank == 0L && !is.null(self@output)) {
      problems <- c(problems, "scalar loop nests cannot carry an output access")
    }
    if (!is.null(self@output) && !identical(self@output@value_id, self@storage@value_id)) {
      problems <- c(problems, "@output must write the value owned by @storage")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a loop nest
#'
#' @param id Stable loop-nest id.
#' @param axes Ordered `TccqLoopAxis` values, outermost first.
#' @param body Body expression or typed statement block with access-carrying
#'   references.
#' @param output Output access over map axes, or `NULL` for scalar results.
#' @param reducer Reduction metadata for reduce axes, or `NULL`.
#' @param identity Reducer identity literal, or `NULL`.
#' @param domain Optional iteration domain. Defaults to one built from `axes`.
#' @param accumulator Typed scalar accumulator target, or `NULL`.
#' @param storage Typed storage slot receiving the nest result.
#' @param guards Ordered `TccqLoopGuard` control path selecting this nest.
#' @export
tccq_loop_nest <- function(
  id,
  axes,
  body,
  storage,
  output = NULL,
  reducer = NULL,
  identity = NULL,
  domain = NULL,
  accumulator = NULL,
  guards = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_list_of(axes, TccqLoopAxis, "TccqLoopAxis", "axes")
  body_is_supported <- S7::S7_inherits(body, TccqExpression) ||
    S7::S7_inherits(body, TccqValueBlock)
  if (!body_is_supported) {
    tccq_abort(
      "schema.invalid_loop_nest_body",
      "`body` must inherit from <TccqExpression> or <TccqValueBlock>.",
      phase = "schema",
      path = "loop_nest.body"
    )
  }
  .tccq_check_s7(storage, TccqStorageSlot, "TccqStorageSlot", "storage")
  .tccq_check_optional_s7(output, TccqAccess, "TccqAccess", "output")
  .tccq_check_optional_s7(reducer, TccqReductionSpec, "TccqReductionSpec", "reducer")
  .tccq_check_optional_s7(identity, TccqLiteral, "TccqLiteral", "identity")
  .tccq_check_optional_s7(accumulator, TccqWriteTarget, "TccqWriteTarget", "accumulator")
  .tccq_check_list_of(guards, TccqLoopGuard, "TccqLoopGuard", "guards")
  if (is.null(domain)) {
    domain <- tccq_domain(
      sprintf("%s.domain", id),
      tccq_shape(lapply(axes, function(axis) axis@extent)),
      axes = vapply(axes, function(axis) axis@name, character(1))
    )
  }
  .tccq_check_s7(domain, TccqDomain, "TccqDomain", "domain")

  TccqLoopNest(
    id = id,
    domain = domain,
    axes = axes,
    body = body,
    output = output,
    reducer = reducer,
    identity = identity,
    accumulator = accumulator,
    storage = storage,
    guards = guards
  )
}

#' Plan the ordered loop nests for a lowered program
#'
#' This is the pass that turns a lowered value graph into the ordered sequence
#' of loop nests consumed by source printers. Every non-root scalar reduction
#' becomes its own all-reduce nest whose result is a named scalar intermediate,
#' and the program result becomes the final nest, whose body references those
#' intermediates through scalar accesses. Elementwise maps become all-map
#' nests, full reductions become all-reduce nests, per-axis reductions and
#' contractions become mixed nests, and slice values disappear into affine
#' accesses. Programs the planner cannot express honestly return structured
#' diagnostics.
#'
#' @param program Lowered program.
#' @export
tccq_program_loop_nests <- function(program) {
  .tccq_check_s7(program, TccqProgram, "TccqProgram", "program")

  nest_diagnostic <- function(code, message, data = list()) {
    tccq_diagnostic(
      code,
      message,
      phase = "loop_nest",
      path = "loop_nest",
      data = c(list(program = program@name), data)
    )
  }
  failed <- function(diagnostic) {
    tccq_result(success = FALSE, diagnostics = list(diagnostic))
  }

  if (is.null(program@result) || is.null(program@values[[program@result]])) {
    return(failed(nest_diagnostic(
      "loop_nest.missing_result",
      "Loop-nest planning needs a lowered program result."
    )))
  }
  if (!S7::S7_inherits(program@storage_plan, TccqStoragePlan)) {
    return(failed(nest_diagnostic(
      "loop_nest.missing_storage_plan",
      "Loop-nest planning needs the typed program storage plan."
    )))
  }
  if (!S7::S7_inherits(program@schedule, TccqProgramSchedule)) {
    return(failed(nest_diagnostic(
      "loop_nest.missing_program_schedule",
      "Loop-nest planning needs the typed program evaluation schedule."
    )))
  }
  storage_slots <- program@storage_plan@slots
  storage_value_ids <- vapply(storage_slots, function(slot) slot@value_id, character(1))
  if (anyDuplicated(storage_value_ids)) {
    return(failed(nest_diagnostic(
      "loop_nest.duplicate_storage_value",
      "The storage plan must assign one slot to each lowered value."
    )))
  }
  storage_by_value_id <- storage_slots
  names(storage_by_value_id) <- storage_value_ids
  storage_for <- function(value_id) {
    storage <- storage_by_value_id[[value_id]]
    if (is.null(storage)) {
      tccq_abort_diagnostic(nest_diagnostic(
        "loop_nest.missing_value_storage",
        "A loop-nest result has no typed storage slot.",
        data = list(value_id = value_id)
      ))
    }
    storage
  }
  expression_result <- tccq_expression_tree(program)
  if (!expression_result@success) {
    return(tccq_result(success = FALSE, diagnostics = expression_result@diagnostics))
  }
  root <- expression_result@value

  expression_contains <- function(expression, predicate) {
    isTRUE(predicate(expression)) || any(vapply(
      expression@inputs,
      expression_contains,
      logical(1),
      predicate = predicate
    ))
  }
  expression_family <- function(expression) {
    operation <- expression@operation
    if (S7::S7_inherits(operation, TccqLoweredOperation)) operation@family else NULL
  }

  axis_name <- function(position) sprintf("axis_%04d", position)

  # Post-order extraction: every non-root reduction or contraction subtree
  # becomes an intermediate nest — a named scalar for rank-0 results, a
  # materialized buffer otherwise — and the consumer tree keeps a reference in
  # its place. Inner extractions run before the subtrees that consume them, so
  # `intermediates` is already in dependency order, and extraction is keyed by
  # value id so a value consumed twice materializes once.
  intermediates <- list()
  replacements <- new.env(parent = emptyenv())
  fused_definitions <- new.env(parent = emptyenv())
  intermediate_guards <- new.env(parent = emptyenv())
  materialization_definitions <- new.env(parent = emptyenv())
  materialize <- function(expression, guards, definition_binding = NULL) {
    if (any(vapply(
      guards,
      function(guard) expression_contains(
        guard@condition,
        function(candidate) identical(candidate@kind, "branch")
      ),
      logical(1)
    ))) {
      tccq_abort_diagnostic(nest_diagnostic(
        "loop_nest.statement_control_guard",
        "A selected-arm materialization needs its control guard normalized to a scalar value.",
        data = list(value_id = expression@value_id)
      ))
    }
    intermediates[[length(intermediates) + 1L]] <<- expression
    intermediate_guards[[expression@value_id]] <- guards
    if (S7::S7_inherits(definition_binding, TccqLocalBinding)) {
      materialization_definitions[[expression@value_id]] <- definition_binding
    }
    replacement <- tccq_expression(
      id = expression@value_id,
      kind = "reference",
      value_id = expression@value_id,
      op = "intermediate",
      type = expression@type,
      effect = tccq_effect(reads = TRUE),
      reference = tccq_expression_reference(expression@value_id)
    )
    replacements[[expression@value_id]] <- replacement
    replacement
  }
  extract <- function(expression, is_root, guards = list(), definition_binding = NULL) {
    if (
      identical(expression@kind, "reference") &&
        identical(expression@op, "local") &&
        S7::S7_inherits(expression@reference@binding, TccqLocalBinding)
    ) {
      binding <- expression@reference@binding
      binding_storage <- storage_for(binding@value_id)
      if (!isTRUE(binding_storage@materialized)) {
        fused_definition <- fused_definitions[[binding@name]]
        if (is.null(fused_definition)) {
          tccq_abort_diagnostic(nest_diagnostic(
            "loop_nest.missing_fused_definition",
            "A non-materialized local read must have one dominating fused definition.",
            data = list(binding = binding@name, value_id = binding@value_id)
          ))
        }
        return(fused_definition)
      }
    }
    replacement <- replacements[[expression@value_id]]
    materialization_definition <- materialization_definitions[[expression@value_id]]
    definition_owns_schedule <- S7::S7_inherits(
      materialization_definition,
      TccqLocalBinding
    )
    if (!is.null(replacement) && (!is_root || definition_owns_schedule)) {
      if (
        !definition_owns_schedule &&
          !identical(intermediate_guards[[expression@value_id]], guards)
      ) {
        tccq_abort_diagnostic(nest_diagnostic(
          "loop_nest.incompatible_materialization_paths",
          "One materialized value is consumed through incompatible control paths.",
          data = list(value_id = expression@value_id)
        ))
      }
      return(replacement)
    }
    definition_is_expression <- S7::S7_inherits(
      definition_binding,
      TccqLocalBinding
    ) && identical(definition_binding@value_id, expression@value_id)
    definition_requires_materialization <- definition_is_expression &&
      isTRUE(storage_for(definition_binding@value_id)@materialized)

    if (identical(expression@kind, "branch")) {
      expression@inputs[[1L]] <- extract(
        expression@inputs[[1L]],
        is_root = FALSE,
        guards = guards,
        definition_binding = definition_binding
      )
      consequent_guard <- tccq_loop_guard(
        expression@inputs[[1L]],
        expression@branch,
        selected = TRUE
      )
      alternative_guard <- tccq_loop_guard(
        expression@inputs[[1L]],
        expression@branch,
        selected = FALSE
      )
      expression@inputs[[2L]] <- extract(
        expression@inputs[[2L]],
        is_root = FALSE,
        guards = c(guards, list(consequent_guard)),
        definition_binding = definition_binding
      )
      expression@inputs[[3L]] <- extract(
        expression@inputs[[3L]],
        is_root = FALSE,
        guards = c(guards, list(alternative_guard)),
        definition_binding = definition_binding
      )
      if (!is_root && definition_requires_materialization) {
        return(materialize(expression, guards, definition_binding))
      }
      return(expression)
    }
    if (!identical(expression@kind, "operation")) {
      source_value_id <- if (is.null(expression@reference)) {
        expression@value_id
      } else {
        expression@reference@source_value_id
      }
      already_has_source_storage <- identical(expression@kind, "reference") &&
        expression@op %in% c("formal", "dim_symbol") &&
        identical(source_value_id, expression@value_id)
      if (!is_root && definition_requires_materialization && !already_has_source_storage) {
        return(materialize(expression, guards, definition_binding))
      }
      return(expression)
    }
    expression@inputs <- lapply(
      expression@inputs,
      extract,
      is_root = FALSE,
      guards = guards,
      definition_binding = definition_binding
    )
    family <- expression_family(expression)
    if (
      !is_root &&
        (
          (!is.null(family) && family %in% c("reduction", "contraction")) ||
            definition_requires_materialization
        )
    ) {
      return(materialize(expression, guards, definition_binding))
    }
    expression
  }

  scheduled_steps <- program@schedule@steps
  for (step_position in seq_along(scheduled_steps)) {
    step <- scheduled_steps[[step_position]]
    is_final_unbound_result <- step_position == length(scheduled_steps) &&
      is.null(step@binding)
    if (is_final_unbound_result) {
      next
    }
    step_expression_result <- tccq_expression_tree(program, step@value_id)
    if (!step_expression_result@success) {
      return(tccq_result(
        success = FALSE,
        diagnostics = step_expression_result@diagnostics
      ))
    }
    step_expression <- tryCatch(
      extract(
        step_expression_result@value,
        is_root = FALSE,
        guards = list(),
        definition_binding = step@binding
      ),
      tccq_error = identity
    )
    if (inherits(step_expression, "tccq_error")) {
      return(tccq_result(
        success = FALSE,
        diagnostics = list(tccq_condition_diagnostic(step_expression))
      ))
    }
    if (
      S7::S7_inherits(step@binding, TccqLocalBinding) &&
        !isTRUE(storage_for(step@binding@value_id)@materialized)
    ) {
      fused_definitions[[step@binding@name]] <- step_expression
    }
  }
  root <- tryCatch(extract(root, is_root = TRUE), tccq_error = identity)
  if (inherits(root, "tccq_error")) {
    return(tccq_result(
      success = FALSE,
      diagnostics = list(tccq_condition_diagnostic(root))
    ))
  }
  root_operation <- root@operation
  root_family <- if (S7::S7_inherits(root_operation, TccqLoweredOperation)) {
    root_operation@family
  } else {
    "elementwise"
  }

  dim_signature <- function(dim) {
    paste(dim@kind, dim@label, dim@value, sep = ":")
  }

  # Rewrite one expression subtree so every reference carries a typed access.
  # `axis_names` maps tensor-axis positions of values in this subtree to
  # iteration-axis names, and `iteration_dims` carries the extent of each of
  # those positions. A reference whose dimensions match its positions gets an
  # identity or slice access; a shorter reference recycles over the iteration
  # order, R-style, through a modulo-linear access.
  annotate <- function(expression, axis_names, domain, iteration_dims) {
    if (identical(expression@kind, "literal")) {
      return(expression)
    }
    if (identical(expression@kind, "reference")) {
      rank <- expression@type@shape@rank
      if (rank == 0L) {
        access <- tccq_access(
          expression@reference@source_value_id,
          domain,
          kind = "scalar"
        )
      } else {
        if (rank > length(axis_names)) {
          tccq_abort_diagnostic(nest_diagnostic(
            "loop_nest.rank_mismatch",
            "A referenced value has more axes than the iteration space of its subtree.",
            data = list(value_id = expression@value_id, rank = rank)
          ))
        }
        reference_dims <- expression@type@shape@dims
        aligned <- rank == length(axis_names) && identical(
          vapply(reference_dims, dim_signature, character(1)),
          vapply(iteration_dims, dim_signature, character(1))
        )
        if (aligned) {
          offsets <- expression@reference@slice_offsets
          if (length(offsets) == 0L) {
            offsets <- rep(0L, rank)
          }
          index_map <- lapply(seq_len(rank), function(position) {
            tccq_index_expr(axis_names[[position]], offsets[[position]])
          })
          access_kind <- if (any(offsets != 0L)) "slice" else "identity"
          access <- tccq_access(
            expression@reference@source_value_id,
            domain,
            kind = access_kind,
            index_map = index_map
          )
        } else {
          access <- tccq_access(
            expression@reference@source_value_id,
            domain,
            kind = "recycle",
            index_map = lapply(axis_names, function(name) tccq_index_expr(name, 0L)),
            consumer_shape = tccq_shape(iteration_dims)
          )
        }
      }
      reference <- expression@reference
      reference@access <- access
      expression@reference <- reference
      return(expression)
    }
    if (identical(expression@kind, "branch")) {
      expression@inputs <- lapply(
        expression@inputs,
        annotate,
        axis_names = axis_names,
        domain = domain,
        iteration_dims = iteration_dims
      )
      return(expression)
    }
    family <- expression_family(expression)
    if (!identical(family, "elementwise")) {
      tccq_abort_diagnostic(nest_diagnostic(
        "loop_nest.unsupported_composition",
        "Contractions and array reductions must be materialized before entering another loop nest.",
        data = list(value_id = expression@value_id, op = expression@op, family = family)
      ))
    }
    expression@inputs <- lapply(
      expression@inputs,
      annotate,
      axis_names = axis_names,
      domain = domain,
      iteration_dims = iteration_dims
    )
    expression
  }

  annotate_loop_guards <- function(guards, axis_names, domain, iteration_dims) {
    lapply(guards, function(guard) {
      tccq_loop_guard(
        annotate(guard@condition, axis_names, domain, iteration_dims),
        guard@branch,
        guard@selected
      )
    })
  }

  reduction_nest <- function(expression, nest_id, guards = list()) {
    operation <- expression@operation
    input_shape <- expression@inputs[[1L]]@type@shape
    reduction_axes <- as.integer(operation@attrs$reduction_axes %||% seq_len(input_shape@rank))
    kept_axes <- as.integer(operation@attrs$kept_axes %||% integer())
    names_by_position <- vapply(seq_len(input_shape@rank), axis_name, character(1))
    loop_order <- c(kept_axes, reduction_axes)
    axes <- lapply(loop_order, function(position) {
      tccq_loop_axis(
        names_by_position[[position]],
        input_shape@dims[[position]],
        role = if (position %in% reduction_axes) "reduce" else "map"
      )
    })
    domain <- tccq_domain(
      sprintf("%s.domain", nest_id),
      tccq_shape(lapply(loop_order, function(position) input_shape@dims[[position]])),
      axes = names_by_position[loop_order]
    )
    guards <- annotate_loop_guards(
      guards,
      names_by_position[loop_order],
      domain,
      lapply(loop_order, function(position) input_shape@dims[[position]])
    )
    body <- annotate(expression@inputs[[1L]], names_by_position, domain, input_shape@dims)
    if (expression_contains(body, function(candidate) identical(candidate@kind, "branch"))) {
      body_target <- tccq_write_target(
        sprintf("%s.body_result", nest_id),
        body@type,
        kind = "local"
      )
      body <- normalize_statement_body(
        body,
        body_target,
        domain,
        target_is_block_local = TRUE
      )
    }
    output <- if (length(kept_axes) > 0L) {
      tccq_access(
        expression@value_id,
        domain,
        kind = "identity",
        index_map = lapply(kept_axes, function(position) {
          tccq_index_expr(names_by_position[[position]], 0L)
        })
      )
    } else {
      NULL
    }
    tccq_loop_nest(
      nest_id,
      axes = axes,
      body = body,
      storage = storage_for(expression@value_id),
      output = output,
      reducer = operation@reduction,
      identity = operation@identity,
      domain = domain,
      accumulator = tccq_write_target(
        sprintf("%s.accumulator", nest_id),
        tccq_type(expression@type@base),
        kind = "local"
      ),
      guards = guards
    )
  }

  contraction_nest <- function(expression, nest_id, guards = list()) {
    operation <- expression@operation
    contraction_spec <- operation@contraction
    contract_dims <- as.integer(contraction_spec@attrs$contract_dims %||% c(2L, 1L))
    left_contract <- contract_dims[[1L]]
    right_contract <- contract_dims[[2L]]
    left <- expression@inputs[[1L]]
    right <- expression@inputs[[2L]]
    left_shape <- left@type@shape
    right_shape <- right@type@shape
    result_rank <- expression@type@shape@rank
    map_names <- vapply(seq_len(result_rank), axis_name, character(1))
    reduce_name <- axis_name(result_rank + 1L)
    contracted_dim <- left_shape@dims[[left_contract]]
    axes <- c(
      lapply(seq_len(result_rank), function(position) {
        tccq_loop_axis(
          map_names[[position]],
          expression@type@shape@dims[[position]],
          role = "map"
        )
      }),
      list(tccq_loop_axis(reduce_name, contracted_dim, role = "reduce"))
    )
    domain <- tccq_domain(
      sprintf("%s.domain", nest_id),
      tccq_shape(c(expression@type@shape@dims, list(contracted_dim))),
      axes = c(map_names, reduce_name)
    )
    guards <- annotate_loop_guards(
      guards,
      c(map_names, reduce_name),
      domain,
      c(expression@type@shape@dims, list(contracted_dim))
    )
    left_axis_names <- character(2L)
    left_axis_names[[left_contract]] <- reduce_name
    left_axis_names[[setdiff(1:2, left_contract)]] <- map_names[[1L]]
    right_axis_names <- if (right_shape@rank == 1L) {
      reduce_name
    } else {
      names_by_position <- character(2L)
      names_by_position[[right_contract]] <- reduce_name
      names_by_position[[setdiff(1:2, right_contract)]] <- map_names[[2L]]
      names_by_position
    }
    combine_call <- str2lang(sprintf("left %s right", contraction_spec@combine_op))
    combine_resolution <- tccq_resolve_call(
      program@attrs$registry %||% tccq_default_op_registry(),
      tccq_call(contraction_spec@combine_op, expr = combine_call),
      tccq_op_context()
    )
    if (!combine_resolution@success) {
      tccq_abort_diagnostic(nest_diagnostic(
        "loop_nest.unresolved_combine",
        "The contraction combine operation has no lowerable implementation.",
        data = list(op = contraction_spec@combine_op)
      ))
    }
    combine_operation <- tccq_lowered_operation(
      "elementwise",
      combine_resolution@value,
      elementwise = combine_resolution@value@elementwise
    )
    body <- tccq_expression(
      id = sprintf("%s_combine", expression@value_id),
      kind = "operation",
      value_id = expression@value_id,
      op = contraction_spec@combine_op,
      inputs = list(
        annotate(left, left_axis_names, domain, left_shape@dims),
        annotate(right, right_axis_names, domain, right_shape@dims)
      ),
      type = tccq_type(expression@type@base),
      effect = combine_resolution@value@effect,
      operation = combine_operation
    )
    if (expression_contains(body, function(candidate) identical(candidate@kind, "branch"))) {
      body_target <- tccq_write_target(
        sprintf("%s.body_result", nest_id),
        body@type,
        kind = "local"
      )
      body <- normalize_statement_body(
        body,
        body_target,
        domain,
        target_is_block_local = TRUE
      )
    }
    output <- tccq_access(
      expression@value_id,
      domain,
      kind = "identity",
      index_map = lapply(map_names, function(name) tccq_index_expr(name, 0L))
    )
    tccq_loop_nest(
      nest_id,
      axes = axes,
      body = body,
      storage = storage_for(expression@value_id),
      output = output,
      reducer = contraction_spec@reducer,
      identity = operation@identity,
      domain = domain,
      accumulator = tccq_write_target(
        sprintf("%s.accumulator", nest_id),
        tccq_type(expression@type@base),
        kind = "local"
      ),
      guards = guards
    )
  }

  map_nest <- function(expression, nest_id, guards = list()) {
    result_shape <- expression@type@shape
    names_by_position <- vapply(seq_len(result_shape@rank), axis_name, character(1))
    axes <- lapply(seq_len(result_shape@rank), function(position) {
      tccq_loop_axis(
        names_by_position[[position]],
        result_shape@dims[[position]],
        role = "map"
      )
    })
    domain <- tccq_domain(
      sprintf("%s.domain", nest_id),
      result_shape,
      axes = names_by_position
    )
    guards <- annotate_loop_guards(
      guards,
      names_by_position,
      domain,
      result_shape@dims
    )
    body <- annotate(expression, names_by_position, domain, result_shape@dims)
    output <- if (result_shape@rank > 0L) {
      tccq_access(
        expression@value_id,
        domain,
        kind = "identity",
        index_map = lapply(names_by_position, function(name) {
          tccq_index_expr(name, 0L)
        })
      )
    } else {
      NULL
    }
    if (expression_contains(body, function(candidate) identical(candidate@kind, "branch"))) {
      body <- normalize_statement_body(
        body,
        tccq_write_target(expression@value_id, expression@type, kind = "result"),
        domain
      )
    }
    tccq_loop_nest(
      nest_id,
      axes = axes,
      body = body,
      storage = storage_for(expression@value_id),
      output = output,
      domain = domain,
      guards = guards
    )
  }

  intermediate_nest <- function(expression, nest_index, guards) {
    nest_id <- sprintf("loop_nest_%04d", nest_index)
    if (identical(expression_family(expression), "contraction")) {
      contraction_nest(expression, nest_id, guards)
    } else if (identical(expression_family(expression), "reduction")) {
      reduction_nest(expression, nest_id, guards)
    } else {
      map_nest(expression, nest_id, guards)
    }
  }

  block_index <- 0L
  statement_index <- 0L
  next_block_id <- function() {
    block_index <<- block_index + 1L
    sprintf("block_%04d", block_index)
  }
  next_statement_id <- function() {
    statement_index <<- statement_index + 1L
    sprintf("statement_%04d", statement_index)
  }
  target_reference <- function(target, domain) {
    tccq_expression(
      target@value_id,
      "reference",
      type = target@type,
      op = "local",
      effect = tccq_effect(reads = TRUE),
      reference = tccq_expression_reference(
        target@value_id,
        access = tccq_access(target@value_id, domain, kind = "scalar")
      )
    )
  }

  # Normalize value-producing control into explicit neutral statements. Any
  # control-valued operand is evaluated into a typed scalar storage target
  # before its consumer. Arm blocks remain nested, preserving R's selected-arm
  # evaluation instead of hoisting work across control boundaries.
  statement_block <- function(expression, target, domain, available_targets) {
    if (identical(expression@kind, "branch")) {
      condition <- expression@inputs[[1L]]
      locals <- list()
      statements <- list()
      if (expression_contains(
        condition,
        function(input) identical(input@kind, "branch")
      )) {
        condition_target <- tccq_write_target(
          condition@value_id,
          condition@type,
          kind = "local"
        )
        available_targets[[condition@value_id]] <- condition_target
        condition_block <- statement_block(
          condition,
          condition_target,
          domain,
          available_targets
        )
        locals <- c(list(condition_target), condition_block@locals)
        statements <- c(statements, condition_block@statements)
        condition <- target_reference(condition_target, domain)
      }

      consequent <- statement_block(
        expression@inputs[[2L]],
        target,
        domain,
        new.env(parent = available_targets)
      )
      alternative <- statement_block(
        expression@inputs[[3L]],
        target,
        domain,
        new.env(parent = available_targets)
      )
      conditional <- tccq_conditional(
        next_statement_id(),
        condition = condition,
        consequent = consequent,
        alternative = alternative,
        branch = expression@branch
      )
      return(tccq_value_block(
        next_block_id(),
        result = target,
        locals = locals,
        statements = c(statements, list(conditional))
      ))
    }

    locals <- list()
    statements <- list()
    if (identical(expression@kind, "operation")) {
      rewritten_inputs <- vector("list", length(expression@inputs))
      for (input_position in seq_along(expression@inputs)) {
        input <- expression@inputs[[input_position]]
        input_contains_branch <- expression_contains(
          input,
          function(candidate) identical(candidate@kind, "branch")
        )
        if (!input_contains_branch) {
          rewritten_inputs[[input_position]] <- input
          next
        }

        input_target <- get0(
          input@value_id,
          envir = available_targets,
          inherits = TRUE,
          ifnotfound = NULL
        )
        if (is.null(input_target)) {
          input_target <- tccq_write_target(input@value_id, input@type, kind = "local")
          available_targets[[input@value_id]] <- input_target
          input_block <- statement_block(
            input,
            input_target,
            domain,
            available_targets
          )
          locals <- c(locals, list(input_target), input_block@locals)
          statements <- c(statements, input_block@statements)
        }
        rewritten_inputs[[input_position]] <- target_reference(input_target, domain)
      }
      expression <- tccq_expression(
        id = expression@id,
        kind = expression@kind,
        value_id = expression@value_id,
        op = expression@op,
        inputs = rewritten_inputs,
        type = expression@type,
        effect = expression@effect,
        operation = expression@operation
      )
    }

    assignment <- tccq_assignment(
      next_statement_id(),
      target,
      expression
    )
    statements <- c(statements, list(assignment))
    tccq_value_block(
      next_block_id(),
      result = target,
      locals = locals,
      statements = statements
    )
  }

  normalize_statement_body <- function(
    expression,
    target,
    domain,
    target_is_block_local = FALSE
  ) {
    block <- statement_block(
      expression,
      target,
      domain,
      new.env(parent = emptyenv())
    )
    if (!isTRUE(target_is_block_local)) {
      return(block)
    }
    tccq_value_block(
      block@id,
      result = block@result,
      locals = c(list(target), block@locals),
      statements = block@statements
    )
  }

  build <- function() {
    if (identical(root_family, "reduction")) {
      return(reduction_nest(root, "loop_nest_main"))
    }
    if (identical(root_family, "contraction")) {
      return(contraction_nest(root, "loop_nest_main"))
    }
    map_nest(root, "loop_nest_main")
  }

  nests <- tryCatch(
    c(
      unname(Map(
        intermediate_nest,
        intermediates,
        seq_along(intermediates),
        lapply(intermediates, function(expression) {
          intermediate_guards[[expression@value_id]]
        })
      )),
      list(build())
    ),
    tccq_error = identity
  )
  if (inherits(nests, "tccq_error")) {
    return(tccq_result(
      success = FALSE,
      diagnostics = list(tccq_condition_diagnostic(nests))
    ))
  }
  tccq_result(success = TRUE, value = nests)
}

#' Plan the loop nest for a single-nest lowered program
#'
#' Thin wrapper over [tccq_program_loop_nests()] for programs that plan to
#' exactly one nest. Multi-nest programs return a structured diagnostic
#' pointing at the plural planner.
#'
#' @param program Lowered program.
#' @export
tccq_program_loop_nest <- function(program) {
  nests_result <- tccq_program_loop_nests(program)
  if (!nests_result@success) {
    return(nests_result)
  }
  nests <- nests_result@value
  if (length(nests) != 1L) {
    return(tccq_result(success = FALSE, diagnostics = list(tccq_diagnostic(
      "loop_nest.multi_nest_program",
      "This program plans multiple loop nests; consume tccq_program_loop_nests().",
      phase = "loop_nest",
      path = "loop_nest",
      data = list(program = program@name, nests = length(nests))
    ))))
  }
  tccq_result(success = TRUE, value = nests[[1L]])
}
