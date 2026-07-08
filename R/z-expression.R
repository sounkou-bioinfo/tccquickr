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

    if (value@op %in% c("formal", "dim_symbol")) {
      return(tccq_expression(
        id = current_value_id,
        kind = "reference",
        value_id = current_value_id,
        op = value@op,
        type = value@type,
        attrs = list(symbol = value@attrs$symbol, storage_value_id = current_value_id)
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
        source_value <- program@values[[source_value@inputs[[1L]]]]
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
        attrs = list(
          symbol = source_value@attrs$symbol,
          storage_value_id = source_value@id,
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
        literal = literal
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
    value_attrs <- value@attrs
    value_attrs$operation <- NULL

    tccq_expression(
      id = current_value_id,
      kind = "operation",
      value_id = current_value_id,
      op = value@op,
      inputs = input_expressions,
      type = value@type,
      resolved_op = resolved_operation,
      attrs = c(list(effect = value@effect, operation = lowered_operation), value_attrs)
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

#' Loop-nest program plan
#'
#' `TccqLoopNest` is the single backend-neutral iteration plan consumed by
#' source printers, in the spirit of the SAC with-loop. One loop nest carries
#' ordered typed axes (`map` axes produce output positions, `reduce` axes fold
#' into an accumulator), a body expression whose references carry typed affine
#' accesses, an optional reducer with its identity, and an output access.
#' Elementwise maps, full and per-axis reductions, contractions, and stencils
#' are all instances of this one value; printers must not reintroduce
#' per-family loop shapes.
#'
#' @param id Stable loop-nest id.
#' @param domain Iteration domain naming the axes.
#' @param axes Ordered `TccqLoopAxis` values, outermost first.
#' @param body Body expression with access-carrying references.
#' @param result_type Result type of the loop nest.
#' @param output Output access over map axes, or `NULL` for scalar results.
#' @param reducer Reduction metadata for reduce axes, or `NULL`.
#' @param identity Reducer identity literal, or `NULL`.
#' @param attrs Structured metadata.
#' @export
TccqLoopNest <- S7::new_class(
  "TccqLoopNest",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    domain = TccqDomain,
    axes = S7::class_list,
    body = TccqExpression,
    result_type = TccqType,
    output = S7::new_union(NULL, TccqAccess),
    reducer = S7::new_union(NULL, TccqReductionSpec),
    identity = S7::new_union(NULL, TccqLiteral),
    attrs = S7::class_list
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
    if (self@result_type@shape@rank > 0L && is.null(self@output)) {
      problems <- c(problems, "non-scalar loop nests must carry an output access")
    }
    if (self@result_type@shape@rank == 0L && !is.null(self@output)) {
      problems <- c(problems, "scalar loop nests cannot carry an output access")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a loop nest
#'
#' @param id Stable loop-nest id.
#' @param axes Ordered `TccqLoopAxis` values, outermost first.
#' @param body Body expression with access-carrying references.
#' @param result_type Result type of the loop nest.
#' @param output Output access over map axes, or `NULL` for scalar results.
#' @param reducer Reduction metadata for reduce axes, or `NULL`.
#' @param identity Reducer identity literal, or `NULL`.
#' @param domain Optional iteration domain. Defaults to one built from `axes`.
#' @param attrs Structured metadata.
#' @export
tccq_loop_nest <- function(
  id,
  axes,
  body,
  result_type,
  output = NULL,
  reducer = NULL,
  identity = NULL,
  domain = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_list_of(axes, TccqLoopAxis, "TccqLoopAxis", "axes")
  .tccq_check_s7(body, TccqExpression, "TccqExpression", "body")
  .tccq_check_s7(result_type, TccqType, "TccqType", "result_type")
  .tccq_check_optional_s7(output, TccqAccess, "TccqAccess", "output")
  .tccq_check_optional_s7(reducer, TccqReductionSpec, "TccqReductionSpec", "reducer")
  .tccq_check_optional_s7(identity, TccqLiteral, "TccqLiteral", "identity")
  .tccq_check_list(attrs, "attrs")
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
    result_type = result_type,
    output = output,
    reducer = reducer,
    identity = identity,
    attrs = attrs
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
  expression_result <- tccq_expression_tree(program)
  if (!expression_result@success) {
    return(tccq_result(success = FALSE, diagnostics = expression_result@diagnostics))
  }
  root <- expression_result@value

  axis_name <- function(position) sprintf("axis_%04d", position)

  expression_family <- function(expression) {
    operation <- expression@attrs$operation
    if (S7::S7_inherits(operation, TccqLoweredOperation)) operation@family else NULL
  }

  # Post-order extraction: every non-root reduction or contraction subtree
  # becomes an intermediate nest — a named scalar for rank-0 results, a
  # materialized buffer otherwise — and the consumer tree keeps a reference in
  # its place. Inner extractions run before the subtrees that consume them, so
  # `intermediates` is already in dependency order, and extraction is keyed by
  # value id so a value consumed twice materializes once.
  intermediates <- list()
  replacements <- new.env(parent = emptyenv())
  extract <- function(expression, is_root) {
    if (!identical(expression@kind, "operation")) {
      return(expression)
    }
    if (!is_root && !is.null(replacements[[expression@value_id]])) {
      return(replacements[[expression@value_id]])
    }
    expression@inputs <- lapply(expression@inputs, extract, is_root = FALSE)
    family <- expression_family(expression)
    if (
      !is_root &&
        !is.null(family) &&
        family %in% c("reduction", "contraction")
    ) {
      intermediates[[length(intermediates) + 1L]] <<- expression
      replacement <- tccq_expression(
        id = expression@value_id,
        kind = "reference",
        value_id = expression@value_id,
        op = "intermediate",
        type = expression@type,
        attrs = list(storage_value_id = expression@value_id, intermediate = TRUE)
      )
      replacements[[expression@value_id]] <- replacement
      return(replacement)
    }
    expression
  }
  root <- extract(root, is_root = TRUE)
  root_operation <- root@attrs$operation
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
          expression@attrs$storage_value_id %||% expression@value_id,
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
          offsets <- as.integer(expression@attrs$slice_offsets %||% integer())
          if (length(offsets) == 0L) {
            offsets <- rep(0L, rank)
          }
          index_map <- lapply(seq_len(rank), function(position) {
            tccq_index_expr(axis_names[[position]], offsets[[position]])
          })
          access_kind <- if (any(offsets != 0L)) "slice" else "identity"
          access <- tccq_access(
            expression@attrs$storage_value_id %||% expression@value_id,
            domain,
            kind = access_kind,
            index_map = index_map
          )
        } else {
          access <- tccq_access(
            expression@attrs$storage_value_id %||% expression@value_id,
            domain,
            kind = "recycle",
            index_map = lapply(axis_names, function(name) tccq_index_expr(name, 0L)),
            attrs = list(consumer_dims = iteration_dims)
          )
        }
      }
      expression@attrs$access <- access
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

  nest_role_attrs <- function(expression, role) {
    attrs <- list(result_value_id = expression@value_id)
    if (identical(role, "result")) {
      return(attrs)
    }
    if (expression@type@shape@rank == 0L) {
      attrs$scalar_name <- sprintf("scalar_%s", expression@value_id)
    } else {
      attrs$buffer_name <- sprintf("buffer_%s", expression@value_id)
      attrs$scalar_name <- sprintf("acc_%s", expression@value_id)
    }
    attrs
  }

  reduction_nest <- function(expression, nest_id, role) {
    operation <- expression@attrs$operation
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
    body <- annotate(expression@inputs[[1L]], names_by_position, domain, input_shape@dims)
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
      result_type = expression@type,
      output = output,
      reducer = operation@reduction,
      identity = operation@identity,
      domain = domain,
      attrs = nest_role_attrs(expression, role)
    )
  }

  contraction_nest <- function(expression, nest_id, role) {
    operation <- expression@attrs$operation
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
      resolved_op = combine_resolution@value
    )
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
      result_type = expression@type,
      output = output,
      reducer = contraction_spec@reducer,
      identity = operation@identity,
      domain = domain,
      attrs = nest_role_attrs(expression, role)
    )
  }

  intermediate_nest <- function(expression, nest_index) {
    nest_id <- sprintf("loop_nest_%04d", nest_index)
    if (identical(expression_family(expression), "contraction")) {
      contraction_nest(expression, nest_id, "intermediate")
    } else {
      reduction_nest(expression, nest_id, "intermediate")
    }
  }

  build <- function() {
    result_value <- program@values[[program@result]]

    if (identical(root_family, "reduction")) {
      return(reduction_nest(root, "loop_nest_main", "result"))
    }
    if (identical(root_family, "contraction")) {
      return(contraction_nest(root, "loop_nest_main", "result"))
    }

    result_shape <- result_value@type@shape
    names_by_position <- vapply(seq_len(result_shape@rank), axis_name, character(1))
    axes <- lapply(seq_len(result_shape@rank), function(position) {
      tccq_loop_axis(names_by_position[[position]], result_shape@dims[[position]], role = "map")
    })
    domain <- tccq_domain(
      "loop_nest_main.domain",
      result_shape,
      axes = names_by_position
    )
    body <- annotate(root, names_by_position, domain, result_shape@dims)
    output <- if (result_shape@rank > 0L) {
      tccq_access(
        program@result,
        domain,
        kind = "identity",
        index_map = lapply(names_by_position, function(name) tccq_index_expr(name, 0L))
      )
    } else {
      NULL
    }
    tccq_loop_nest(
      "loop_nest_main",
      axes = axes,
      body = body,
      result_type = result_value@type,
      output = output,
      domain = domain,
      attrs = list(result_value_id = program@result)
    )
  }

  nests <- tryCatch(
    c(
      unname(Map(intermediate_nest, intermediates, seq_along(intermediates))),
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
