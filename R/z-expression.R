TCCQ_EXPRESSION_KINDS <- c(
  "reference", "literal", "operation", "branch", "element", "indexed"
)
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

#' Typed declared-dimension reference
#'
#' A dimension reference identifies the scalar ABI extent corresponding to one
#' declared symbolic dimension. It is distinct from an ordinary scalar binding
#' with the same source spelling.
#'
#' @inheritParams TccqExpressionReference
#' @param dimension Declared symbolic dimension supplied by the reference.
#' @export
TccqDimensionReference <- S7::new_class(
  "TccqDimensionReference",
  package = "tccquickr",
  parent = TccqExpressionReference,
  properties = list(dimension = TccqDim),
  validator = function(self) {
    problems <- character()
    if (!identical(self@dimension@kind, "symbol")) {
      problems <- c(problems, "@dimension must be symbolic")
    }
    if (!identical(self@symbol, self@dimension@label)) {
      problems <- c(problems, "@symbol must name @dimension")
    }
    if (!is.null(self@binding) || length(self@slice_offsets) > 0L) {
      problems <- c(problems, "dimension references cannot carry binding or slice facts")
    }
    if (!is.null(self@access) && !identical(self@access@kind, "scalar")) {
      problems <- c(problems, "dimension references may carry only scalar access")
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
#' @param index_proofs Per-axis bounds proofs for indexed expressions.
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
    reference = S7::new_union(NULL, TccqExpressionReference),
    index_proofs = S7::class_list
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
    index_proofs_are_typed <- vapply(
      self@index_proofs,
      S7::S7_inherits,
      logical(1),
      class = TccqIndexProof
    )
    if (!all(index_proofs_are_typed)) {
      problems <- c(problems, "@index_proofs must contain only <TccqIndexProof> values")
    }
    if (!identical(self@kind, "indexed") && length(self@index_proofs) > 0L) {
      problems <- c(problems, "only indexed expressions may carry @index_proofs")
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
    element_has_invalid_payload <- length(self@inputs) != 1L ||
      !is.null(self@literal) ||
      !is.null(self@operation) ||
      !is.null(self@branch) ||
      !is.null(self@reference)
    if (identical(self@kind, "element") && element_has_invalid_payload) {
      problems <- c(
        problems,
        "element expressions need one input and no literal, operation, branch, or reference payload"
      )
    }
    if (
      identical(self@kind, "element") &&
        all(inputs_are_expressions) &&
        (
          self@type@shape@rank != 0L ||
            !identical(self@type@base, self@inputs[[1L]]@type@base)
        )
    ) {
      problems <- c(problems, "element expressions must project one scalar of the input base type")
    }
    indexed_has_invalid_payload <- length(self@inputs) < 2L ||
      !is.null(self@literal) ||
      is.null(self@operation) ||
      !is.null(self@branch) ||
      is.null(self@reference) ||
      is.null(self@reference@access) ||
      !identical(self@reference@access@kind, "extract")
    if (identical(self@kind, "indexed") && indexed_has_invalid_payload) {
      problems <- c(
        problems,
        "indexed expressions need source and selector inputs plus one typed extract access"
      )
    }
    if (
      identical(self@kind, "indexed") &&
        length(self@inputs) >= 2L &&
        all(inputs_are_expressions)
    ) {
      source <- self@inputs[[1L]]
      selectors <- self@inputs[-1L]
      source_reference <- source@reference
      access <- if (is.null(self@reference)) NULL else self@reference@access
      indexed_types_match <-
        source@type@shape@rank == length(selectors) &&
        source@type@shape@rank == length(self@index_proofs) &&
        source@type@shape@rank > 0L &&
        all(vapply(selectors, function(selector) {
          selector@type@shape@rank == 0L &&
            identical(selector@type@base, "integer")
        }, logical(1))) &&
        self@type@shape@rank == 0L &&
        identical(self@type@base, source@type@base)
      if (!indexed_types_match) {
        problems <- c(problems, "indexed expressions require one scalar integer selector per source axis")
      }
      proofs_match_source <-
        indexed_types_match &&
          all(index_proofs_are_typed) &&
          all(vapply(seq_along(self@index_proofs), function(position) {
            identical(
              self@index_proofs[[position]]@source_extent,
              source@type@shape@dims[[position]]
            )
          }, logical(1)))
      proof_axes <- if (proofs_match_source) {
        vapply(
          self@index_proofs,
          function(proof) proof@iteration@domain@axes[[1L]],
          character(1)
        )
      } else {
        character()
      }
      unique_axis_positions <- !duplicated(proof_axes)
      expected_domain_dims <- if (proofs_match_source) {
        lapply(
          self@index_proofs[unique_axis_positions],
          function(proof) proof@iteration@domain@shape@dims[[1L]]
        )
      } else {
        list()
      }
      indexed_access_matches <-
        proofs_match_source &&
        !is.null(source_reference) &&
        !is.null(access) &&
        identical(access@value_id, source_reference@source_value_id) &&
        identical(access@domain@axes, proof_axes[unique_axis_positions]) &&
        identical(access@domain@shape@dims, expected_domain_dims) &&
        length(access@index_map) == source@type@shape@rank &&
        identical(
          access@index_map,
          lapply(self@index_proofs, function(proof) proof@index)
        ) &&
        all(vapply(seq_along(selectors), function(position) {
          selector <- selectors[[position]]
          proof <- self@index_proofs[[position]]
          identical(selector@kind, "reference") &&
            identical(selector@value_id, proof@selector@id) &&
            !is.null(selector@reference) &&
            identical(selector@reference@binding, proof@selector@cell)
        }, logical(1)))
      if (!indexed_access_matches) {
        problems <- c(problems, "indexed expression access must match its per-axis bounds proofs")
      }
      if (
        !S7::S7_inherits(self@operation, TccqLoweredOperation) ||
          !identical(self@operation@family, "subscript")
      ) {
        problems <- c(problems, "indexed expressions must carry a subscript operation")
      }
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

#' Construct a typed declared-dimension reference
#'
#' @inheritParams TccqDimensionReference
#' @export
tccq_dimension_reference <- function(source_value_id, dimension) {
  .tccq_check_character_scalar(source_value_id, "source_value_id")
  .tccq_check_s7(dimension, TccqDim, "TccqDim", "dimension")
  TccqDimensionReference(
    source_value_id = source_value_id,
    symbol = dimension@label,
    binding = NULL,
    slice_offsets = integer(),
    access = NULL,
    dimension = dimension
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
#' @param index_proofs Per-axis bounds proofs for indexed expressions.
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
  reference = NULL,
  index_proofs = list()
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
  .tccq_check_list_of(index_proofs, TccqIndexProof, "TccqIndexProof", "index_proofs")
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
    reference = reference,
    index_proofs = index_proofs
  )
}

#' Build a registered elementwise expression
#'
#' This transformation resolves one operation through the supplied registry,
#' applies its signature to typed input expressions, and returns the ordinary
#' neutral expression consumed by every source backend.
#'
#' @param registry Operation registry used for resolution.
#' @param op Operation name.
#' @param inputs Ordered scalar or array input expressions.
#' @param id Stable expression id.
#' @return A `TccqResult` containing a `TccqExpression`.
#' @export
tccq_elementwise_expression <- function(registry, op, inputs, id) {
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  .tccq_check_character_scalar(op, "op")
  .tccq_check_list_of(inputs, TccqExpression, "TccqExpression", "inputs")
  .tccq_check_character_scalar(id, "id")
  call_expression <- as.call(c(
    list(as.name(op)),
    rep(list(quote(.tccq_operand)), length(inputs))
  ))
  resolution <- tccq_resolve_call(
    registry,
    tccq_call(op, expr = call_expression),
    tccq_op_context()
  )
  if (!resolution@success) {
    return(resolution)
  }
  resolved_operation <- resolution@value
  if (!S7::S7_inherits(resolved_operation@elementwise, TccqElementwiseSpec)) {
    return(tccq_result(
      success = FALSE,
      diagnostics = list(tccq_diagnostic(
        "expression.operation_not_elementwise",
        "A neutral expression operation must resolve to an elementwise implementation.",
        phase = "expression",
        path = "expression.operation",
        data = list(op = op)
      ))
    ))
  }
  if (S7::S7_inherits(resolved_operation@body, TccqOpBody)) {
    return(tccq_result(
      success = FALSE,
      diagnostics = list(tccq_diagnostic(
        "expression.unexpanded_operation_body",
        "Neutral operation bodies must be expanded before expression construction.",
        phase = "expression",
        path = "expression.operation_body",
        data = list(op = op)
      ))
    ))
  }
  result_type <- tccq_op_signature_result_type(
    resolved_operation@elementwise@signature,
    lapply(inputs, function(input) input@type)
  )
  if (!result_type@success) {
    return(result_type)
  }
  tccq_result(
    success = TRUE,
    value = tccq_expression(
      id = id,
      kind = "operation",
      value_id = id,
      op = op,
      inputs = inputs,
      type = result_type@value,
      effect = resolved_operation@effect,
      operation = tccq_lowered_operation(
        "elementwise",
        resolved_operation,
        elementwise = resolved_operation@elementwise
      )
    )
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

    if (identical(value@op, "formal")) {
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
    if (identical(value@op, "dim_symbol")) {
      return(tccq_expression(
        id = current_value_id,
        kind = "reference",
        value_id = current_value_id,
        op = value@op,
        type = value@type,
        effect = value@effect,
        reference = tccq_dimension_reference(
          current_value_id,
          tccq_dim_symbol(value@attrs$symbol)
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
    if (S7::S7_inherits(value, TccqIndexedValue)) {
      source <- build_expression(value@inputs[[1L]], c(value_stack, current_value_id))
      selectors <- lapply(
        value@inputs[-1L],
        build_expression,
        value_stack = c(value_stack, current_value_id)
      )
      if (is.null(source) || any(vapply(selectors, is.null, logical(1)))) {
        return(NULL)
      }
      if (
        !identical(source@kind, "reference") ||
          is.null(source@reference) ||
          !is.null(source@reference@access)
      ) {
        diagnostics <<- c(diagnostics, list(expression_diagnostic(
          "expression.indexed_source_not_reference",
          "A proven indexed read currently requires one direct source reference.",
          "expression.indexed.source",
          data = list(value_id = current_value_id)
        )))
        return(NULL)
      }
      return(tccq_expression(
        id = current_value_id,
        kind = "indexed",
        value_id = current_value_id,
        op = value@op,
        inputs = c(list(source), selectors),
        type = value@type,
        effect = value@effect,
        operation = value@operation,
        reference = tccq_expression_reference(
          value@access@value_id,
          symbol = source@reference@symbol,
          access = value@access
        ),
        index_proofs = value@index_proofs
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

#' Structured control completion
#'
#' Completion is distinct from [TccqEffect]. It records whether structured
#' control may reach the next statement or transfer to the nearest enclosing
#' loop. These facts let dominance and later transformations reason about
#' normal paths without treating `break` or `next` as fabricated side effects.
#'
#' @param falls_through Whether evaluation may reach the next statement.
#' @param breaks Whether evaluation may break from the nearest enclosing loop.
#' @param continues Whether evaluation may continue the nearest enclosing loop.
#' @export
TccqControlCompletion <- S7::new_class(
  "TccqControlCompletion",
  package = "tccquickr",
  properties = list(
    falls_through = S7::class_logical,
    breaks = S7::class_logical,
    continues = S7::class_logical
  ),
  validator = function(self) {
    properties <- list(
      falls_through = self@falls_through,
      breaks = self@breaks,
      continues = self@continues
    )
    invalid_properties <- names(properties)[vapply(
      properties,
      function(value) length(value) != 1L || is.na(value),
      logical(1)
    )]
    if (length(invalid_properties) > 0L) {
      sprintf(
        "@%s must be a single TRUE/FALSE value",
        invalid_properties[[1L]]
      )
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

#' Neutral positional switch statement
#'
#' A positional switch evaluates one scalar integer selector exactly once, then
#' evaluates at most one ordered alternative. An unmatched position evaluates
#' no alternative, matching numeric `switch()` returning `NULL` when its value
#' is used only for control. Character selection and value-producing switches
#' require separate representation work.
#'
#' @inheritParams TccqStatement
#' @param selector Scalar integer selector expression.
#' @param selector_target Typed local target for the evaluate-once selector.
#' @param alternatives Ordered positional alternative blocks.
#' @param semantics Evaluator facts for the originating `switch` special form.
#' @export
TccqSwitch <- S7::new_class(
  "TccqSwitch",
  package = "tccquickr",
  parent = TccqStatement,
  properties = list(
    selector = TccqExpression,
    selector_target = TccqWriteTarget,
    alternatives = S7::class_list,
    semantics = TccqCallSemantics
  ),
  validator = function(self) {
    problems <- character()
    if (
      !identical(self@selector@type@base, "integer") ||
        self@selector@type@shape@rank != 0L
    ) {
      problems <- c(problems, "@selector must be a scalar integer expression")
    }
    if (
      !identical(self@selector_target@kind, "local") ||
        !identical(self@selector_target@type, self@selector@type) ||
        !identical(self@selector_target@storage_type, self@selector@type)
    ) {
      problems <- c(problems, "@selector_target must be a scalar local matching @selector")
    }
    if (identical(self@selector_target@value_id, self@selector@value_id)) {
      problems <- c(problems, "@selector_target must be distinct from the source value")
    }
    alternatives_are_blocks <- vapply(
      self@alternatives,
      S7::S7_inherits,
      logical(1),
      class = TccqBlock
    )
    if (length(self@alternatives) == 0L || !all(alternatives_are_blocks)) {
      problems <- c(problems, "@alternatives must contain at least one <TccqBlock>")
    }
    if (
      !identical(self@semantics@call@name, "switch") ||
        !isTRUE(self@semantics@control) ||
        !identical(self@semantics@forcing_policy, "special")
    ) {
      problems <- c(problems, "@semantics must describe the R `switch` special form")
    }
    if (all(alternatives_are_blocks)) {
      expected_effect <- Reduce(
        tccq_effect_union,
        c(
          list(self@selector@effect),
          lapply(self@alternatives, function(alternative) alternative@effect)
        ),
        init = tccq_effect()
      )
      if (!identical(self@effect, expected_effect)) {
        problems <- c(problems, "@effect must include the selector and every alternative")
      }
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

#' Backend-neutral iteration plan
#'
#' An iteration plan separates one-time source evaluation from selection of the
#' current element. Stored-vector iteration carries a typed element expression
#' with an access map. Virtual unit sequences carry an affine index expression
#' and the resolved operation implementation that defines the sequence.
#'
#' @param source Expression evaluated once to establish iteration.
#' @param domain One-dimensional iteration domain.
#' @param element Current element expression or affine induction value.
#' @param element_type Scalar type assigned to the iteration cell.
#' @param resolved_op Resolved operation for virtual iteration, or `NULL` for
#'   stored-value iteration.
#' @param semantics Evaluator facts for the virtual iteration call, or `NULL`.
#' @param effect Effect summary for source evaluation and element selection.
#' @export
TccqIterationPlan <- S7::new_class(
  "TccqIterationPlan",
  package = "tccquickr",
  properties = list(
    source = TccqExpression,
    domain = TccqDomain,
    element = S7::new_union(TccqExpression, TccqIndexExpr),
    element_type = TccqType,
    resolved_op = S7::new_union(NULL, TccqResolvedOp),
    semantics = S7::new_union(NULL, TccqCallSemantics),
    effect = TccqEffect
  ),
  validator = function(self) {
    problems <- character()
    if (self@domain@shape@rank != 1L) {
      problems <- c(problems, "@domain must be one-dimensional")
    }
    if (self@element_type@shape@rank != 0L) {
      problems <- c(problems, "@element_type must be scalar")
    }

    element_is_expression <- S7::S7_inherits(self@element, TccqExpression)
    element_is_induction <- S7::S7_inherits(self@element, TccqIndexExpr)
    if (element_is_expression) {
      access <- if (is.null(self@element@reference)) {
        NULL
      } else {
        self@element@reference@access
      }
      source_has_direct_reference <-
        identical(self@source@kind, "reference") &&
        !is.null(self@source@reference) &&
        is.null(self@source@reference@access) &&
        length(self@source@reference@slice_offsets) == 0L
      if (!source_has_direct_reference) {
        problems <- c(problems, "stored iteration source must be one direct reference")
      }
      if (
        self@source@type@shape@rank != 1L ||
          !identical(self@domain@shape, self@source@type@shape)
      ) {
        problems <- c(problems, "stored iteration source and domain must share one rank-1 shape")
      }
      if (
        !identical(self@element@type, self@source@type) ||
          !identical(self@element_type@base, self@source@type@base)
      ) {
        problems <- c(problems, "stored iteration element facts must match the source base type")
      }
      if (
        !S7::S7_inherits(access, TccqAccess) ||
          !identical(access@domain, self@domain) ||
          !identical(access@kind, "identity")
      ) {
        problems <- c(problems, "stored iteration elements must carry identity access over @domain")
      } else {
        exact_identity <- length(access@index_map) == 1L &&
          length(self@domain@axes) == 1L &&
          identical(access@index_map[[1L]]@axis, self@domain@axes[[1L]]) &&
          identical(access@index_map[[1L]]@offset, 0L) &&
          identical(access@value_id, self@source@value_id)
        if (!exact_identity) {
          problems <- c(problems, "stored iteration access must select the current source element")
        }
      }
      if (!is.null(self@resolved_op) || !is.null(self@semantics)) {
        problems <- c(problems, "stored iteration must not carry virtual-operation facts")
      }
    }
    if (element_is_induction) {
      source_dimension <- if (length(self@domain@shape@dims) == 1L) {
        self@domain@shape@dims[[1L]]
      } else {
        NULL
      }
      source_is_extent_reference <-
        identical(self@source@kind, "reference") &&
        S7::S7_inherits(self@source@reference, TccqDimensionReference) &&
        is.null(self@source@reference@access) &&
        S7::S7_inherits(source_dimension, TccqDim) &&
        identical(source_dimension, self@source@reference@dimension)
      source_is_extent_literal <-
        identical(self@source@kind, "literal") &&
        !is.null(self@source@literal) &&
        identical(self@source@literal@kind, "finite") &&
        identical(self@source@literal@type@base, "integer") &&
        S7::S7_inherits(source_dimension, TccqDim) &&
        identical(source_dimension@kind, "constant") &&
        identical(
          as.integer(self@source@literal@value),
          source_dimension@value
        )
      if (self@source@type@shape@rank != 0L) {
        problems <- c(problems, "virtual iteration source must be scalar")
      }
      if (!source_is_extent_reference && !source_is_extent_literal) {
        problems <- c(problems, "virtual iteration source must match its symbolic or constant domain extent")
      }
      if (!self@element_type@base %in% c("integer", "double")) {
        problems <- c(problems, "affine induction values must be integer or double scalars")
      }
      if (
        length(self@domain@axes) != 1L ||
          !identical(self@element@axis, self@domain@axes[[1L]]) ||
          is.null(self@resolved_op) ||
          !S7::S7_inherits(self@resolved_op@iteration, TccqIterationSpec) ||
          is.null(self@semantics)
      ) {
        problems <- c(problems, "virtual iteration requires aligned domain, implementation, and evaluator facts")
      } else {
        if (!identical(self@element@offset, self@resolved_op@iteration@start)) {
          problems <- c(problems, "affine induction offset must match the iteration implementation start")
        }
        if (!identical(self@resolved_op@call@id, self@semantics@call@id)) {
          problems <- c(problems, "virtual iteration call and evaluator facts must have the same id")
        }
        result_type <- tccq_op_signature_result_type(
          self@resolved_op@iteration@signature,
          list(self@source@type)
        )
        if (
          !result_type@success ||
            result_type@value@shape@rank != 1L ||
            !identical(result_type@value@base, self@element_type@base)
        ) {
          problems <- c(problems, "virtual iteration element type must satisfy its operation signature")
        }
        if (
          !isTRUE(self@resolved_op@pure) ||
            isTRUE(self@resolved_op@boundary) ||
            !self@semantics@forcing_policy %in% c("eager", "lazy") ||
            isTRUE(self@semantics@control) ||
            isTRUE(self@semantics@replacement)
        ) {
          problems <- c(problems, "virtual iteration must be a pure ordinary non-boundary call")
        }
      }
    }
    expected_effect <- self@source@effect
    if (element_is_expression) {
      expected_effect <- tccq_effect_union(expected_effect, self@element@effect)
    }
    if (!is.null(self@resolved_op)) {
      expected_effect <- tccq_effect_union(expected_effect, self@resolved_op@effect)
    }
    if (!identical(self@effect, expected_effect)) {
      problems <- c(problems, "@effect must include source, element, and iteration-operation effects")
    }
    if (length(problems) > 0L) problems
  }
)

#' Proven affine scalar index
#'
#' An index proof ties one R iterator cell to one source axis. The iteration
#' establishes the cell range, `index` is the zero-based storage access, and
#' `source_extent` proves that the full affine access remains in bounds. A
#' non-zero offset additionally retains the resolved `+` call and its evaluator
#' facts; the range proof is what discharges integer overflow and subscript
#' warnings for that normalized selector.
#'
#' @param iterator Iterator target populated by the enclosing virtual loop.
#' @param selector Direct reference to the iterator cell.
#' @param iteration Virtual iteration establishing the selector range.
#' @param source_extent Extent of the indexed source axis.
#' @param index Zero-based affine storage index.
#' @param operation Lowered selector operation for a shifted index, or `NULL`.
#' @param semantics Evaluator facts for the shifted selector call, or `NULL`.
#' @export
TccqIndexProof <- S7::new_class(
  "TccqIndexProof",
  package = "tccquickr",
  properties = list(
    iterator = TccqWriteTarget,
    selector = TccqCellReference,
    iteration = TccqIterationPlan,
    source_extent = TccqDim,
    index = TccqIndexExpr,
    operation = S7::new_union(NULL, TccqLoweredOperation),
    semantics = S7::new_union(NULL, TccqCallSemantics)
  ),
  validator = function(self) {
    problems <- character()
    iterator_owns_selector <-
      identical(self@iterator@kind, "cell") &&
        S7::S7_inherits(self@iterator@binding, TccqCell) &&
        identical(self@iterator@binding, self@selector@cell) &&
        identical(self@iterator@value_id, self@selector@cell@value_id) &&
        identical(self@iterator@type, self@iteration@element_type)
    if (!iterator_owns_selector) {
      problems <- c(problems, "@iterator must own the selector cell populated by @iteration")
    }
    if (
      !identical(self@selector@type@base, "integer") ||
        self@selector@type@shape@rank != 0L ||
        !identical(self@selector@type, self@iteration@element_type)
    ) {
      problems <- c(problems, "@selector must be the scalar integer value of @iteration")
    }
    iteration_is_one_based <-
      S7::S7_inherits(self@iteration@element, TccqIndexExpr) &&
        !is.null(self@iteration@resolved_op) &&
        S7::S7_inherits(self@iteration@resolved_op@iteration, TccqIterationSpec) &&
        identical(self@iteration@element@offset, 1L) &&
        identical(self@iteration@resolved_op@iteration@start, 1L)
    if (!iteration_is_one_based) {
      problems <- c(problems, "@iteration must prove a one-based virtual unit sequence")
    }
    if (
      length(self@iteration@domain@axes) != 1L ||
        !identical(self@index@axis, self@iteration@domain@axes[[1L]])
    ) {
      problems <- c(problems, "@index axis must match the iteration domain axis")
    }

    domain_extent <- self@iteration@domain@shape@dims[[1L]]
    source_offset <- if (identical(self@source_extent@kind, "symbol")) {
      0L
    } else if (identical(self@source_extent@kind, "affine")) {
      self@source_extent@value
    } else {
      NA_integer_
    }
    domain_offset <- if (identical(domain_extent@kind, "symbol")) {
      0L
    } else if (identical(domain_extent@kind, "affine")) {
      domain_extent@value
    } else {
      NA_integer_
    }
    same_symbolic_base <-
      self@source_extent@kind %in% c("symbol", "affine") &&
        domain_extent@kind %in% c("symbol", "affine") &&
        identical(self@source_extent@label, domain_extent@label)
    available_offset <- if (
      identical(self@source_extent@kind, "constant") &&
        identical(domain_extent@kind, "constant")
    ) {
      as.double(self@source_extent@value) - as.double(domain_extent@value)
    } else if (same_symbolic_base) {
      as.double(source_offset) - as.double(domain_offset)
    } else {
      NA_integer_
    }
    bounds_are_proven <-
      !is.na(available_offset) &&
        self@index@offset >= 0L &&
        self@index@offset <= available_offset
    if (!bounds_are_proven) {
      problems <- c(
        problems,
        "source and iteration extents must prove every affine selector position in bounds"
      )
    }

    has_shift_operation <- !is.null(self@operation)
    has_shift_semantics <- !is.null(self@semantics)
    if (!identical(has_shift_operation, has_shift_semantics)) {
      problems <- c(problems, "shifted indices require both @operation and @semantics")
    } else if (!has_shift_operation && !identical(self@index@offset, 0L)) {
      problems <- c(problems, "a non-zero affine offset requires a retained selector operation")
    } else if (has_shift_operation) {
      selector_expr <- self@semantics@call@expr
      selector_is_canonical_shift <-
        self@index@offset > 0L &&
          is.call(selector_expr) &&
          identical(tccq_call_name(selector_expr), "+") &&
          length(selector_expr) == 3L &&
          is.symbol(selector_expr[[2L]]) &&
          identical(as.character(selector_expr[[2L]]), self@selector@cell@name) &&
          is.integer(selector_expr[[3L]]) &&
          length(selector_expr[[3L]]) == 1L &&
          !is.na(selector_expr[[3L]]) &&
          identical(as.integer(selector_expr[[3L]]), self@index@offset)
      if (!selector_is_canonical_shift) {
        problems <- c(problems, "shifted indices must normalize `iterator + positive_integer_literal`")
      }
      operation_is_integer_addition <-
        identical(self@operation@family, "elementwise") &&
          identical(self@operation@resolved_op@call@name, "+") &&
          identical(
            self@operation@resolved_op@call@id,
            self@semantics@call@id
          ) &&
          isTRUE(self@operation@resolved_op@pure) &&
          !isTRUE(self@operation@resolved_op@uses_rapi) &&
          !isTRUE(self@operation@resolved_op@boundary) &&
          !isTRUE(self@operation@resolved_op@effect@writes) &&
          !isTRUE(self@operation@resolved_op@effect@allocates) &&
          !isTRUE(self@operation@resolved_op@effect@boundary) &&
          !isTRUE(self@operation@resolved_op@effect@may_error) &&
          self@semantics@forcing_policy %in% c("eager", "lazy") &&
          !isTRUE(self@semantics@control) &&
          !isTRUE(self@semantics@replacement)
      if (!operation_is_integer_addition) {
        problems <- c(problems, "shifted indices require a pure ordinary integer `+` implementation")
      }
      selector_type <- tccq_op_signature_result_type(
        self@operation@signature,
        list(self@selector@type, tccq_type("integer"))
      )
      if (!selector_type@success || !identical(selector_type@value, self@selector@type)) {
        problems <- c(problems, "the retained selector operation must preserve scalar integer type")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Proven scalar indexed read
#'
#' This value represents one scalar extraction only when every source axis has
#' a [TccqIndexProof]. Proofs may share an iteration, as in a diagonal read. The
#' stored access is zero-based; iterator cells retain R's one-based values. No
#' general R subscript semantics are implied by this class.
#'
#' @inheritParams TccqValue
#' @param source_type Type of the source storage.
#' @param index_proofs Per-axis affine selector and bounds proofs.
#' @param access Zero-based source access corresponding to the proofs.
#' @param operation Lowered subscript implementation payload.
#' @param semantics Evaluator facts for the originating `[` call.
#' @export
TccqIndexedValue <- S7::new_class(
  "TccqIndexedValue",
  package = "tccquickr",
  parent = TccqValue,
  properties = list(
    source_type = TccqType,
    index_proofs = S7::class_list,
    access = TccqAccess,
    operation = TccqLoweredOperation,
    semantics = TccqCallSemantics
  ),
  validator = function(self) {
    problems <- character()
    source_rank <- self@source_type@shape@rank
    proofs_match_rank <- source_rank > 0L && length(self@index_proofs) == source_rank
    if (!identical(self@op, "[") || length(self@inputs) != source_rank + 1L) {
      problems <- c(problems, "indexed values must be `[` calls with one selector per source axis")
    }
    if (!proofs_match_rank) {
      problems <- c(problems, "@index_proofs must contain one proof per source axis")
    }
    proofs_are_typed <- vapply(
      self@index_proofs,
      S7::S7_inherits,
      logical(1),
      class = TccqIndexProof
    )
    if (!all(proofs_are_typed)) {
      problems <- c(problems, "@index_proofs must contain only <TccqIndexProof> values")
    }
    if (
      proofs_match_rank &&
        length(self@inputs) == source_rank + 1L &&
        all(proofs_are_typed) &&
        (
          !identical(self@inputs[[1L]], self@access@value_id) ||
            !identical(
              self@inputs[-1L],
              lapply(self@index_proofs, function(proof) proof@selector@id)
            )
        )
    ) {
      problems <- c(problems, "@inputs must identify the accessed source and selector references")
    }
    if (self@type@shape@rank != 0L || !identical(self@source_type@base, self@type@base)) {
      problems <- c(problems, "indexed values require a non-scalar source and scalar result of one base type")
    }
    if (proofs_match_rank && all(proofs_are_typed)) {
      proof_extents_match <- vapply(seq_len(source_rank), function(position) {
        identical(
          self@source_type@shape@dims[[position]],
          self@index_proofs[[position]]@source_extent
        )
      }, logical(1))
      if (!all(proof_extents_match)) {
        problems <- c(problems, "each index proof must own its corresponding source extent")
      }
    }
    expected_axes <- if (all(proofs_are_typed)) {
      unique(vapply(
        self@index_proofs,
        function(proof) proof@iteration@domain@axes[[1L]],
        character(1)
      ))
    } else {
      character()
    }
    expected_domain_dims <- if (all(proofs_are_typed)) {
      iteration_axes <- vapply(
        self@index_proofs,
        function(proof) proof@iteration@domain@axes[[1L]],
        character(1)
      )
      lapply(
        self@index_proofs[!duplicated(iteration_axes)],
        function(proof) proof@iteration@domain@shape@dims[[1L]]
      )
    } else {
      list()
    }
    exact_access <-
      identical(self@access@kind, "extract") &&
      identical(self@access@domain@axes, expected_axes) &&
      identical(self@access@domain@shape@dims, expected_domain_dims) &&
      length(self@access@index_map) == source_rank &&
      all(vapply(seq_along(self@access@index_map), function(position) {
        position <= length(self@index_proofs) &&
          S7::S7_inherits(self@index_proofs[[position]], TccqIndexProof) &&
          identical(self@access@index_map[[position]], self@index_proofs[[position]]@index)
      }, logical(1)))
    if (!exact_access) {
      problems <- c(problems, "@access must match the proven affine source indices")
    }
    if (
      !identical(self@operation@family, "subscript") ||
        !S7::S7_inherits(self@operation@subscript, TccqSubscriptSpec)
    ) {
      problems <- c(problems, "@operation must carry the proven scalar subscript contract")
    }
    if (!identical(self@attrs$operation, self@operation)) {
      problems <- c(problems, "@attrs$operation must match the typed operation payload")
    }
    if (
      !identical(self@semantics@call@name, "[") ||
        isTRUE(self@semantics@control) ||
        isTRUE(self@semantics@replacement) ||
        !identical(self@semantics@call@id, self@operation@resolved_op@call@id)
    ) {
      problems <- c(problems, "@semantics must match the non-replacement `[` implementation")
    }
    invalid_effect <-
      !isTRUE(self@effect@reads) ||
        isTRUE(self@effect@writes) ||
        isTRUE(self@effect@allocates) ||
        isTRUE(self@effect@boundary) ||
        isTRUE(self@effect@may_error) ||
        isTRUE(self@effect@may_warn)
    if (invalid_effect) {
      problems <- c(problems, "proven indexed reads must have a read-only effect")
    }
    if (all(proofs_are_typed)) {
      expected_effect <- Reduce(
        tccq_effect_union,
        lapply(self@index_proofs, function(proof) proof@selector@effect),
        init = self@operation@resolved_op@effect
      )
      if (!identical(self@effect, expected_effect)) {
        problems <- c(problems, "@effect must combine selector and subscript implementation effects")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Neutral for statement
#'
#' A for loop evaluates one typed iteration plan and assigns each selected
#' element to its iteration cell before evaluating the body. Source evaluation,
#' element selection, and iteration domain are explicit so source backends do
#' not reconstruct traversal from R syntax.
#'
#' @inheritParams TccqLoop
#' @param iterator Mutable scalar destination receiving each element.
#' @param iteration Backend-neutral iteration plan.
#' @export
TccqFor <- S7::new_class(
  "TccqFor",
  package = "tccquickr",
  parent = TccqLoop,
  properties = list(
    iterator = TccqWriteTarget,
    iteration = TccqIterationPlan
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
    if (!identical(self@iterator@type, self@iteration@element_type)) {
      problems <- c(problems, "@iterator type must match the iteration element type")
    }
    expected_effect <- Reduce(
      tccq_effect_union,
      list(
        self@iteration@effect,
        self@body@effect,
        tccq_effect(writes = TRUE)
      ),
      init = tccq_effect()
    )
    if (!identical(self@effect, expected_effect)) {
      problems <- c(problems, "@effect must include iteration, iterator writes, and body effects")
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

#' Construct a neutral positional switch
#'
#' @inheritParams TccqSwitch
#' @export
tccq_switch <- function(
  id,
  selector,
  selector_target,
  alternatives,
  semantics
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(selector, TccqExpression, "TccqExpression", "selector")
  .tccq_check_s7(selector_target, TccqWriteTarget, "TccqWriteTarget", "selector_target")
  .tccq_check_list_of(alternatives, TccqBlock, "TccqBlock", "alternatives")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  effect <- Reduce(
    tccq_effect_union,
    c(
      list(selector@effect),
      lapply(alternatives, function(alternative) alternative@effect)
    ),
    init = tccq_effect()
  )
  TccqSwitch(
    id = id,
    effect = effect,
    selector = selector,
    selector_target = selector_target,
    alternatives = alternatives,
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

#' Construct a backend-neutral iteration plan
#'
#' @inheritParams TccqIterationPlan
#' @export
tccq_iteration_plan <- function(
  source,
  domain,
  element,
  element_type,
  resolved_op = NULL,
  semantics = NULL
) {
  .tccq_check_s7(source, TccqExpression, "TccqExpression", "source")
  .tccq_check_s7(domain, TccqDomain, "TccqDomain", "domain")
  if (
    !S7::S7_inherits(element, TccqExpression) &&
      !S7::S7_inherits(element, TccqIndexExpr)
  ) {
    tccq_abort(
      "schema.invalid_iteration_element",
      "`element` must be a <TccqExpression> or <TccqIndexExpr> value.",
      phase = "schema",
      path = "iteration.element",
      data = list(element = element)
    )
  }
  .tccq_check_s7(element_type, TccqType, "TccqType", "element_type")
  .tccq_check_optional_s7(resolved_op, TccqResolvedOp, "TccqResolvedOp", "resolved_op")
  .tccq_check_optional_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  effect <- source@effect
  if (S7::S7_inherits(element, TccqExpression)) {
    effect <- tccq_effect_union(effect, element@effect)
  }
  if (!is.null(resolved_op)) {
    effect <- tccq_effect_union(effect, resolved_op@effect)
  }
  TccqIterationPlan(
    source = source,
    domain = domain,
    element = element,
    element_type = element_type,
    resolved_op = resolved_op,
    semantics = semantics,
    effect = effect
  )
}

#' Construct a proven affine scalar index
#'
#' @inheritParams TccqIndexProof
#' @export
tccq_index_proof <- function(
  iterator,
  selector,
  iteration,
  source_extent,
  index,
  operation = NULL,
  semantics = NULL
) {
  .tccq_check_s7(iterator, TccqWriteTarget, "TccqWriteTarget", "iterator")
  .tccq_check_s7(selector, TccqCellReference, "TccqCellReference", "selector")
  .tccq_check_s7(iteration, TccqIterationPlan, "TccqIterationPlan", "iteration")
  .tccq_check_s7(source_extent, TccqDim, "TccqDim", "source_extent")
  .tccq_check_s7(index, TccqIndexExpr, "TccqIndexExpr", "index")
  .tccq_check_optional_s7(
    operation,
    TccqLoweredOperation,
    "TccqLoweredOperation",
    "operation"
  )
  .tccq_check_optional_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  TccqIndexProof(
    iterator = iterator,
    selector = selector,
    iteration = iteration,
    source_extent = source_extent,
    index = index,
    operation = operation,
    semantics = semantics
  )
}

#' Construct a proven scalar indexed read
#'
#' @param id Stable value id.
#' @param source_id Stable value id of the source storage.
#' @param source_type Type of the source storage.
#' @param index_proofs Per-axis affine selector and bounds proofs.
#' @param access Zero-based source access corresponding to the proofs.
#' @param operation Lowered subscript implementation payload.
#' @param semantics Evaluator facts for the originating `[` call.
#' @export
tccq_indexed_value <- function(
  id,
  source_id,
  source_type,
  index_proofs,
  access,
  operation,
  semantics
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(source_id, "source_id")
  .tccq_check_s7(source_type, TccqType, "TccqType", "source_type")
  .tccq_check_list_of(index_proofs, TccqIndexProof, "TccqIndexProof", "index_proofs")
  .tccq_check_s7(access, TccqAccess, "TccqAccess", "access")
  .tccq_check_s7(operation, TccqLoweredOperation, "TccqLoweredOperation", "operation")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")

  result_type <- tccq_op_signature_result_type(
    operation@signature,
    c(
      list(source_type),
      lapply(index_proofs, function(proof) proof@selector@type)
    )
  )
  if (!result_type@success) {
    tccq_abort_diagnostic(result_type@diagnostics[[1L]])
  }
  TccqIndexedValue(
    id = id,
    op = "[",
    inputs = c(
      list(source_id),
      lapply(index_proofs, function(proof) proof@selector@id)
    ),
    type = result_type@value,
    effect = Reduce(
      tccq_effect_union,
      lapply(index_proofs, function(proof) proof@selector@effect),
      init = operation@resolved_op@effect
    ),
    attrs = list(operation = operation),
    source_type = source_type,
    index_proofs = index_proofs,
    access = access,
    operation = operation,
    semantics = semantics
  )
}

#' Construct a neutral for statement
#'
#' @inheritParams TccqFor
#' @export
tccq_for <- function(id, iterator, iteration, body, semantics) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(iterator, TccqWriteTarget, "TccqWriteTarget", "iterator")
  .tccq_check_s7(iteration, TccqIterationPlan, "TccqIterationPlan", "iteration")
  .tccq_check_s7(body, TccqBlock, "TccqBlock", "body")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  TccqFor(
    id = id,
    effect = Reduce(
      tccq_effect_union,
      list(iteration@effect, body@effect, tccq_effect(writes = TRUE)),
      init = tccq_effect()
    ),
    body = body,
    semantics = semantics,
    iterator = iterator,
    iteration = iteration
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

#' Compute structured control completion
#'
#' Completion summarizes only paths reachable in evaluation order. Nested
#' loops consume their own `break` and `next` transfers, while a block retains
#' transfers that target its nearest enclosing loop.
#'
#' @param control Typed statement or statement block.
#' @return A `TccqControlCompletion` value.
#' @export
tccq_completion <- S7::new_generic(
  "tccq_completion",
  dispatch_args = "control",
  function(control) S7::S7_dispatch()
)

S7::method(tccq_completion, TccqAssignment) <- function(control) {
  TccqControlCompletion(
    falls_through = TRUE,
    breaks = FALSE,
    continues = FALSE
  )
}

S7::method(tccq_completion, TccqLoopTransfer) <- function(control) {
  TccqControlCompletion(
    falls_through = FALSE,
    breaks = identical(control@action, "break"),
    continues = identical(control@action, "next")
  )
}

S7::method(tccq_completion, TccqIf) <- function(control) {
  consequent <- tccq_completion(control@consequent)
  alternative <- tccq_completion(control@alternative)
  TccqControlCompletion(
    falls_through = consequent@falls_through || alternative@falls_through,
    breaks = consequent@breaks || alternative@breaks,
    continues = consequent@continues || alternative@continues
  )
}

S7::method(tccq_completion, TccqSwitch) <- function(control) {
  alternative_completions <- lapply(control@alternatives, tccq_completion)
  TccqControlCompletion(
    falls_through = TRUE,
    breaks = any(vapply(
      alternative_completions,
      function(completion) completion@breaks,
      logical(1)
    )),
    continues = any(vapply(
      alternative_completions,
      function(completion) completion@continues,
      logical(1)
    ))
  )
}

S7::method(tccq_completion, TccqWhile) <- function(control) {
  TccqControlCompletion(
    falls_through = TRUE,
    breaks = FALSE,
    continues = FALSE
  )
}

S7::method(tccq_completion, TccqFor) <- function(control) {
  TccqControlCompletion(
    falls_through = TRUE,
    breaks = FALSE,
    continues = FALSE
  )
}

S7::method(tccq_completion, TccqRepeat) <- function(control) {
  body_completion <- tccq_completion(control@body)
  TccqControlCompletion(
    falls_through = body_completion@breaks,
    breaks = FALSE,
    continues = FALSE
  )
}

S7::method(tccq_completion, TccqBlock) <- function(control) {
  falls_through <- TRUE
  breaks <- FALSE
  continues <- FALSE
  for (statement in control@statements) {
    if (!falls_through) {
      break
    }
    statement_completion <- tccq_completion(statement)
    breaks <- breaks || statement_completion@breaks
    continues <- continues || statement_completion@continues
    falls_through <- statement_completion@falls_through
  }
  TccqControlCompletion(
    falls_through = falls_through,
    breaks = breaks,
    continues = continues
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

#' Reduction-state component
#'
#' A component is one typed scalar in a reducer's neutral state. Its semantic
#' name is a portable identifier so backend interfaces can derive generated
#' names without sanitizing arbitrary source text.
#'
#' @param name Semantic component name.
#' @param target Neutral local target that stores the component.
#' @param identity Initial scalar literal.
#' @export
TccqReductionStateComponent <- S7::new_class(
  "TccqReductionStateComponent",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    target = TccqWriteTarget,
    identity = TccqLiteral
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@name) != 1L ||
        is.na(self@name) ||
        !grepl("^[A-Za-z][A-Za-z0-9_]*$", self@name)
    ) {
      problems <- c(problems, "@name must be one portable identifier")
    }
    if (!identical(self@target@kind, "local") || self@target@type@shape@rank != 0L) {
      problems <- c(problems, "@target must be a scalar local target")
    }
    if (!identical(self@identity@type, self@target@storage_type)) {
      problems <- c(problems, "@identity type must match @target storage type")
    }
    if (length(problems) > 0L) problems
  }
)

#' Typed neutral reduction state
#'
#' @param components Named reduction-state components.
#' @export
TccqReductionState <- S7::new_class(
  "TccqReductionState",
  package = "tccquickr",
  properties = list(components = S7::class_list),
  validator = function(self) {
    problems <- character()
    components_are_typed <- vapply(
      self@components,
      S7::S7_inherits,
      logical(1),
      class = TccqReductionStateComponent
    )
    if (length(self@components) == 0L || !all(components_are_typed)) {
      problems <- c(problems, "@components must contain typed reduction-state components")
    }
    if (all(components_are_typed)) {
      component_names <- vapply(self@components, function(component) component@name, character(1))
      if (anyDuplicated(component_names)) {
        problems <- c(problems, "@components must have unique semantic names")
      }
      target_ids <- vapply(
        self@components,
        function(component) component@target@value_id,
        character(1)
      )
      if (anyDuplicated(target_ids)) {
        problems <- c(problems, "@components must have unique target identities")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Closed neutral reduction plan
#'
#' A reduction plan owns the reducer, its typed state, the optional condition
#' guarding each transition, one typed assignment per state component, the
#' completed-state projection, and an optional validity expression. No target
#' source appears in this contract.
#'
#' @param spec Reduction implementation metadata.
#' @param state Typed neutral state.
#' @param condition Optional scalar logical transition condition.
#' @param updates Ordered state-component assignments.
#' @param value Scalar result projected from completed state.
#' @param valid Optional scalar logical expression proving that `value` exists.
#' @export
TccqReductionPlan <- S7::new_class(
  "TccqReductionPlan",
  package = "tccquickr",
  properties = list(
    spec = TccqReductionSpec,
    state = TccqReductionState,
    condition = S7::new_union(NULL, TccqExpression),
    updates = S7::class_list,
    value = TccqExpression,
    valid = S7::new_union(NULL, TccqExpression)
  ),
  validator = function(self) {
    problems <- character()
    components <- self@state@components
    component_names <- vapply(components, function(component) component@name, character(1))
    component_targets <- vapply(
      components,
      function(component) component@target@value_id,
      character(1)
    )
    updates_are_assignments <- vapply(
      self@updates,
      S7::S7_inherits,
      logical(1),
      class = TccqAssignment
    )
    if (length(self@updates) != length(components) || !all(updates_are_assignments)) {
      problems <- c(problems, "@updates must contain one typed assignment per state component")
    } else {
      update_targets <- vapply(
        self@updates,
        function(update) update@target@value_id,
        character(1)
      )
      if (!identical(update_targets, component_targets)) {
        problems <- c(problems, "@updates must target state components in component order")
      }
    }
    if (!is.null(self@condition) && (
      !identical(self@condition@type@base, "logical") ||
        self@condition@type@shape@rank != 0L
    )) {
      problems <- c(problems, "@condition must be NULL or a scalar logical expression")
    }
    if (self@value@type@shape@rank != 0L) {
      problems <- c(problems, "@value must be a scalar expression")
    }
    if (!is.null(self@valid) && (
      !identical(self@valid@type@base, "logical") || self@valid@type@shape@rank != 0L
    )) {
      problems <- c(problems, "@valid must be NULL or a scalar logical expression")
    }
    if (identical(self@spec@empty_policy, "error") != !is.null(self@valid)) {
      problems <- c(problems, "reducers with an error empty policy must carry one validity expression")
    }
    if (S7::S7_inherits(self@spec, TccqFoldReductionSpec)) {
      if (!identical(component_names, "accumulator") || !is.null(self@condition)) {
        problems <- c(problems, "fold reductions require one unconditional accumulator component")
      }
    }
    if (S7::S7_inherits(self@spec, TccqArgReductionSpec)) {
      expected_names <- c("seen", "best_value", "best_index")
      expected_bases <- c("logical", "double", "integer")
      component_bases <- vapply(
        components,
        function(component) component@target@type@base,
        character(1)
      )
      if (!identical(component_names, expected_names) || !identical(component_bases, expected_bases)) {
        problems <- c(problems, "argument reductions require seen, best-value, and best-index state")
      }
      if (is.null(self@condition)) {
        problems <- c(problems, "argument reductions require a transition condition")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a reduction-state component
#'
#' @inheritParams TccqReductionStateComponent
#' @export
tccq_reduction_state_component <- function(name, target, identity) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_s7(target, TccqWriteTarget, "TccqWriteTarget", "target")
  .tccq_check_s7(identity, TccqLiteral, "TccqLiteral", "identity")
  TccqReductionStateComponent(name = name, target = target, identity = identity)
}

#' Build a neutral reduction plan
#'
#' @param spec Reduction metadata.
#' @param value Scalar input-element expression.
#' @param axes Ordered reduce axes.
#' @param result_type Scalar result type.
#' @param id Stable loop-nest id.
#' @param registry Operation registry used to build transition expressions.
#' @export
tccq_reduction_plan <- S7::new_generic(
  "tccq_reduction_plan",
  dispatch_args = "spec",
  function(spec, value, axes, result_type, id, registry) S7::S7_dispatch()
)

S7::method(tccq_reduction_plan, TccqFoldReductionSpec) <- function(
  spec,
  value,
  axes,
  result_type,
  id,
  registry
) {
  .tccq_check_s7(value, TccqExpression, "TccqExpression", "value")
  .tccq_check_list_of(axes, TccqLoopAxis, "TccqLoopAxis", "axes")
  .tccq_check_s7(result_type, TccqType, "TccqType", "result_type")
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  identity_result <- tccq_reduction_identity(spec, result_type)
  if (!identity_result@success) {
    tccq_abort_diagnostic(identity_result@diagnostics[[1L]])
  }
  accumulator <- tccq_reduction_state_component(
    "accumulator",
    tccq_write_target(sprintf("%s.accumulator", id), result_type, kind = "local"),
    identity_result@value
  )
  state <- TccqReductionState(components = list(accumulator))
  accumulator_value <- tccq_expression(
    id = sprintf("%s.accumulator.read", id),
    kind = "reference",
    value_id = accumulator@target@value_id,
    op = "reduction_state",
    type = accumulator@target@type,
    effect = tccq_effect(reads = TRUE),
    reference = tccq_expression_reference(accumulator@target@value_id)
  )
  combined <- tccq_elementwise_expression(
    registry,
    spec@combine_op,
    list(accumulator_value, value),
    sprintf("%s.combine", id)
  )
  if (!combined@success) {
    tccq_abort_diagnostic(combined@diagnostics[[1L]])
  }
  projected_value <- accumulator_value
  if (nzchar(spec@finalize_op)) {
    extent_expression <- function(axis, position) {
      dimension <- axis@extent
      if (identical(dimension@kind, "constant")) {
        literal <- tccq_literal_finite(as.integer(dimension@value))
        return(tccq_expression(
          id = sprintf("%s.count.%04d", id, position),
          kind = "literal",
          type = literal@type,
          literal = literal
        ))
      }
      if (!dimension@kind %in% c("symbol", "affine")) {
        tccq_abort(
          "loop_nest.unsupported_reduction_extent",
          "Reduction finalization requires a constant, symbolic, or affine extent.",
          phase = "loop_nest",
          path = "loop_nest.reduction.count",
          data = list(reducer = spec@name, extent = dimension)
        )
      }
      symbol_dimension <- tccq_dim_symbol(dimension@label)
      symbol_value <- tccq_expression(
        id = sprintf("%s.count.%04d.symbol", id, position),
        kind = "reference",
        value_id = sprintf("dimension.%s", dimension@label),
        op = "dimension",
        type = tccq_type("integer"),
        effect = tccq_effect(reads = TRUE),
        reference = tccq_dimension_reference(
          sprintf("dimension.%s", dimension@label),
          symbol_dimension
        )
      )
      if (identical(dimension@kind, "symbol") || dimension@value == 0L) {
        return(symbol_value)
      }
      offset <- tccq_literal_finite(as.integer(abs(dimension@value)))
      offset_value <- tccq_expression(
        id = sprintf("%s.count.%04d.offset", id, position),
        kind = "literal",
        type = offset@type,
        literal = offset
      )
      adjusted <- tccq_elementwise_expression(
        registry,
        if (dimension@value < 0L) "-" else "+",
        list(symbol_value, offset_value),
        sprintf("%s.count.%04d.affine", id, position)
      )
      if (!adjusted@success) {
        tccq_abort_diagnostic(adjusted@diagnostics[[1L]])
      }
      adjusted@value
    }
    count_values <- Map(extent_expression, axes, seq_along(axes))
    reduced_count <- count_values[[1L]]
    if (length(count_values) > 1L) {
      for (position in 2:length(count_values)) {
        product <- tccq_elementwise_expression(
          registry,
          "*",
          list(reduced_count, count_values[[position]]),
          sprintf("%s.count.product.%04d", id, position)
        )
        if (!product@success) {
          tccq_abort_diagnostic(product@diagnostics[[1L]])
        }
        reduced_count <- product@value
      }
    }
    finalized <- tccq_elementwise_expression(
      registry,
      spec@finalize_op,
      list(accumulator_value, reduced_count),
      sprintf("%s.finalize", id)
    )
    if (!finalized@success) {
      tccq_abort_diagnostic(finalized@diagnostics[[1L]])
    }
    projected_value <- finalized@value
  }
  TccqReductionPlan(
    spec = spec,
    state = state,
    condition = NULL,
    updates = list(tccq_assignment(
      sprintf("%s.accumulator.update", id),
      accumulator@target,
      combined@value
    )),
    value = projected_value,
    valid = NULL
  )
}

S7::method(tccq_reduction_plan, TccqArgReductionSpec) <- function(
  spec,
  value,
  axes,
  result_type,
  id,
  registry
) {
  .tccq_check_s7(value, TccqExpression, "TccqExpression", "value")
  .tccq_check_list_of(axes, TccqLoopAxis, "TccqLoopAxis", "axes")
  .tccq_check_s7(result_type, TccqType, "TccqType", "result_type")
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  if (
    value@type@shape@rank != 0L ||
      !identical(value@type@base, "double") ||
      result_type@shape@rank != 0L ||
      !identical(result_type@base, "integer") ||
      length(axes) != 1L
  ) {
    tccq_abort(
      "loop_nest.invalid_argument_reduction_types",
      "Argument reductions require one double-valued reduce axis and an integer result.",
      phase = "loop_nest",
      path = "loop_nest.reduction_state",
      data = list(value_type = value@type, result_type = result_type, axes = axes)
    )
  }
  component <- function(name, type, identity) {
    tccq_reduction_state_component(
      name,
      tccq_write_target(sprintf("%s.%s", id, name), type, kind = "local"),
      identity
    )
  }
  state <- TccqReductionState(
    components = list(
      component("seen", tccq_type("logical"), tccq_literal_finite(FALSE)),
      component("best_value", tccq_type("double"), tccq_literal_finite(0)),
      component("best_index", tccq_type("integer"), tccq_literal_finite(0L))
    )
  )
  state_value <- function(name) {
    component_names <- vapply(state@components, function(item) item@name, character(1))
    state_component <- state@components[[match(name, component_names)]]
    tccq_expression(
      id = sprintf("%s.%s.read", id, name),
      kind = "reference",
      value_id = state_component@target@value_id,
      op = "reduction_state",
      type = state_component@target@type,
      effect = tccq_effect(reads = TRUE),
      reference = tccq_expression_reference(state_component@target@value_id)
    )
  }
  operation <- function(name, inputs, suffix) {
    operation_result <- tccq_elementwise_expression(
      registry,
      name,
      inputs,
      sprintf("%s.%s", id, suffix)
    )
    if (!operation_result@success) {
      tccq_abort_diagnostic(operation_result@diagnostics[[1L]])
    }
    operation_result@value
  }
  literal_value <- function(value, suffix) {
    literal <- tccq_literal_finite(value)
    tccq_expression(
      id = sprintf("%s.%s", id, suffix),
      kind = "literal",
      type = literal@type,
      literal = literal
    )
  }
  seen <- state_value("seen")
  best_value <- state_value("best_value")
  best_index <- state_value("best_index")
  not_missing <- operation("!", list(operation("is.na", list(value), "missing")), "not_missing")
  not_seen <- operation("!", list(seen), "not_seen")
  preferred <- operation(
    if (identical(spec@direction, "max")) ">" else "<",
    list(value, best_value),
    "preferred"
  )
  selectable <- operation("|", list(not_seen, preferred), "selectable")
  condition <- operation("&", list(not_missing, selectable), "condition")
  axis_value <- tccq_expression(
    id = sprintf("%s.index", id),
    kind = "reference",
    value_id = axes[[1L]]@name,
    op = "loop_index",
    type = tccq_type("integer"),
    effect = tccq_effect(reads = TRUE),
    reference = tccq_expression_reference(axes[[1L]]@name)
  )
  selected_index <- operation(
    "+",
    list(axis_value, literal_value(1L, "one")),
    "selected_index"
  )
  targets <- lapply(state@components, function(item) item@target)
  TccqReductionPlan(
    spec = spec,
    state = state,
    condition = condition,
    updates = list(
      tccq_assignment(sprintf("%s.seen.update", id), targets[[1L]], literal_value(TRUE, "true")),
      tccq_assignment(sprintf("%s.best_value.update", id), targets[[2L]], value),
      tccq_assignment(sprintf("%s.best_index.update", id), targets[[3L]], selected_index)
    ),
    value = best_index,
    valid = seen
  )
}

#' Loop-nest program plan
#'
#' `TccqLoopNest` is the single backend-neutral iteration plan consumed by
#' source printers, in the spirit of the SAC with-loop. One loop nest carries
#' ordered typed axes (`map` axes produce output positions, `reduce` axes fold
#' into an accumulator), a value expression or typed statement block whose
#' references carry typed affine accesses, an optional closed reduction plan,
#' and an output access. Elementwise maps, full and per-axis
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
#' @param reduction Closed neutral reduction plan for reduce axes, or `NULL`.
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
    reduction = S7::new_union(NULL, TccqReductionPlan),
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
      if (has_reduce_axes && !S7::S7_inherits(self@reduction, TccqReductionPlan)) {
        problems <- c(problems, "loop nests with reduce axes must carry a reduction plan")
      }
      if (!has_reduce_axes && !is.null(self@reduction)) {
        problems <- c(problems, "loop nests without reduce axes cannot carry a reduction plan")
      }
      if (!is.null(self@output)) {
        map_axis_names <- axis_names[axis_roles == "map"]
        output_axes <- vapply(self@output@index_map, function(index) index@axis, character(1))
        if (length(setdiff(setdiff(output_axes, ""), map_axis_names)) > 0L) {
          problems <- c(problems, "@output may only index map axes")
        }
      }
    }
    if (!is.null(self@reduction)) {
      if (
        self@reduction@value@type@shape@rank != 0L ||
          !identical(self@reduction@value@type@base, self@storage@type@base)
      ) {
        problems <- c(problems, "@reduction must project one scalar element of @storage")
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
#' @param reduction Closed neutral reduction plan for reduce axes, or `NULL`.
#' @param domain Optional iteration domain. Defaults to one built from `axes`.
#' @param storage Typed storage slot receiving the nest result.
#' @param guards Ordered `TccqLoopGuard` control path selecting this nest.
#' @export
tccq_loop_nest <- function(
  id,
  axes,
  body,
  storage,
  output = NULL,
  reduction = NULL,
  domain = NULL,
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
  .tccq_check_optional_s7(
    reduction,
    TccqReductionPlan,
    "TccqReductionPlan",
    "reduction"
  )
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
    reduction = reduction,
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

  loop_element <- function(expression, id) {
    tccq_expression(
      id = id,
      kind = "element",
      inputs = list(expression),
      type = tccq_type(expression@type@base),
      effect = expression@effect
    )
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
    if (
      S7::S7_inherits(operation@reduction, TccqArgReductionSpec) &&
        length(reduction_axes) != 1L
    ) {
      tccq_abort_diagnostic(nest_diagnostic(
        "loop_nest.unsupported_argument_reduction_rank",
        "Argument reductions currently require exactly one reduce axis.",
        data = list(reducer = operation@reduction@name, reduce_axes = reduction_axes)
      ))
    }
    reduction_body_value <- if (S7::S7_inherits(body, TccqValueBlock)) {
      tccq_expression(
        id = sprintf("%s.body.read", nest_id),
        kind = "reference",
        value_id = body@result@value_id,
        op = "reduction_body",
        type = body@result@type,
        effect = tccq_effect(reads = TRUE),
        reference = tccq_expression_reference(
          body@result@value_id,
          access = tccq_access(body@result@value_id, domain, kind = "scalar")
        )
      )
    } else {
      body
    }
    reduction_value <- loop_element(
      reduction_body_value,
      sprintf("%s.body.element", nest_id)
    )
    reduction_plan <- tccq_reduction_plan(
      operation@reduction,
      reduction_value,
      Filter(function(axis) identical(axis@role, "reduce"), axes),
      tccq_type(expression@type@base),
      nest_id,
      program@attrs$registry %||% tccq_default_op_registry()
    )
    tccq_loop_nest(
      nest_id,
      axes = axes,
      body = body,
      storage = storage_for(expression@value_id),
      output = output,
      reduction = reduction_plan,
      domain = domain,
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
    left_element <- loop_element(
      annotate(left, left_axis_names, domain, left_shape@dims),
      sprintf("%s_left_element", expression@value_id)
    )
    right_element <- loop_element(
      annotate(right, right_axis_names, domain, right_shape@dims),
      sprintf("%s_right_element", expression@value_id)
    )
    combined <- tccq_elementwise_expression(
      program@attrs$registry %||% tccq_default_op_registry(),
      contraction_spec@combine_op,
      list(left_element, right_element),
      sprintf("%s_combine", expression@value_id)
    )
    if (!combined@success) {
      tccq_abort_diagnostic(nest_diagnostic(
        "loop_nest.unresolved_combine",
        "The contraction combine operation has no lowerable implementation.",
        data = list(op = contraction_spec@combine_op)
      ))
    }
    body <- combined@value
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
    reduction_body_value <- if (S7::S7_inherits(body, TccqValueBlock)) {
      tccq_expression(
        id = sprintf("%s.body.read", nest_id),
        kind = "reference",
        value_id = body@result@value_id,
        op = "reduction_body",
        type = body@result@type,
        effect = tccq_effect(reads = TRUE),
        reference = tccq_expression_reference(
          body@result@value_id,
          access = tccq_access(body@result@value_id, domain, kind = "scalar")
        )
      )
    } else {
      body
    }
    reduction_value <- loop_element(
      reduction_body_value,
      sprintf("%s.body.element", nest_id)
    )
    reduction_plan <- tccq_reduction_plan(
      contraction_spec@reducer,
      reduction_value,
      Filter(function(axis) identical(axis@role, "reduce"), axes),
      tccq_type(expression@type@base),
      nest_id,
      program@attrs$registry %||% tccq_default_op_registry()
    )
    tccq_loop_nest(
      nest_id,
      axes = axes,
      body = body,
      storage = storage_for(expression@value_id),
      output = output,
      reduction = reduction_plan,
      domain = domain,
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
    if (expression@kind %in% c("operation", "element")) {
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
        literal = expression@literal,
        operation = expression@operation,
        branch = expression@branch,
        reference = expression@reference
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
