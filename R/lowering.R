#' Lower a declared function into a backend-neutral plan
#'
#' The lowerer produces `TccqValue`, `TccqRegion`, `TccqFusionGroup`, and
#' `TccqStoragePlan` objects. It is a middle-end pass result, not a C, Fortran,
#' or TinyCC lowering.
#'
#' @param fn Function to lower.
#' @param bindings Declared formal bindings.
#' @param registry Operation registry used for operation availability.
#' @param context Operation support context.
#' @param call_index Typed call facts for `fn`, or `NULL` to collect them.
#' @export
tccq_lower_function <- function(
  fn,
  bindings,
  registry = tccq_default_op_registry(),
  context = tccq_op_context(),
  call_index = NULL
) {
  new_plan <- function(
    values = list(),
    regions = list(),
    result = NULL,
    storage_plan = NULL,
    diagnostics = list(),
    attrs = list()
  ) {
    TccqLoweringPlan(
      values = values,
      regions = regions,
      result = result,
      storage_plan = storage_plan,
      diagnostics = diagnostics,
      attrs = attrs
    )
  }

  executable_expressions <- function(expr) {
    expressions <- if (is.call(expr) && identical(tccq_call_name(expr), "{")) {
      as.list(expr)[-1L]
    } else {
      list(expr)
    }
    Filter(function(candidate) !is_declaration(candidate), expressions)
  }

  is_declaration <- function(expr) {
    is.call(expr) &&
      identical(tccq_call_name(expr), "declare") &&
      length(expr) >= 2L &&
      is.call(expr[[2L]]) &&
      identical(tccq_call_name(expr[[2L]]), "type")
  }

  new_lowering_state <- function() {
    state <- new.env(parent = emptyenv())
    state$formal_names <- names(bindings)
    state$symbol_value_ids <- list()
    state$local_bindings <- list()
    state$values <- list()
    state$value_counter <- 0L
    state$dim_symbols <- unique(unlist(lapply(bindings, function(binding) {
      labels <- vapply(binding@type@shape@dims, function(dim) dim@label, character(1))
      labels[nzchar(labels)]
    })))
    for (binding_index in seq_along(bindings)) {
      binding_name <- names(bindings)[[binding_index]]
      binding <- bindings[[binding_name]]
      value_id <- sprintf("formal_%04d", binding_index)
      state$symbol_value_ids[[binding_name]] <- value_id
      add_value(
        state,
        tccq_value(
          id = value_id,
          op = "formal",
          inputs = list(),
          type = binding@type,
          effect = tccq_effect(reads = TRUE),
          attrs = list(symbol = binding@name)
        )
      )
    }
    state
  }

  add_value <- function(state, value) {
    state$values[[value@id]] <- value
    value@id
  }

  next_value_id <- function(state) {
    state$value_counter <- state$value_counter + 1L
    sprintf("value_%04d", state$value_counter)
  }

  lower_statement <- function(expr, state) {
    if (is.call(expr) && tccq_call_name(expr) %in% c("<-", "=") && length(expr) == 3L) {
      return(lower_assignment(expr, state))
    }
    lower_expression(expr, state)
  }

  lower_assignment <- function(expr, state) {
    target <- expr[[2L]]
    value_expr <- expr[[3L]]
    if (is.call(target)) {
      target_name <- tccq_call_name(target)
      target_root <- if (length(target) >= 2L && is.symbol(target[[2L]])) {
        as.character(target[[2L]])
      } else {
        NULL
      }
      if (!is.null(target_root) && target_root %in% state$formal_names) {
        return(diagnostic_value(
          "lowering.formal_mutation",
          sprintf("Formal `%s` cannot be mutated by the current single-assignment lowerer.", target_root),
          expr,
          data = list(symbol = target_root, target = target_name)
        ))
      }
      return(diagnostic_value(
        "lowering.unsupported_assignment_target",
        "Only simple local symbol bindings are lowerable assignments for now.",
        expr,
        data = list(target = target_name)
      ))
    }
    if (!is.symbol(target)) {
      return(diagnostic_value(
        "lowering.unsupported_assignment_target",
        "Assignment targets must be local symbols in the current lowerer.",
        expr,
        data = list(target = typeof(target))
      ))
    }

    binding_name <- as.character(target)
    if (binding_name %in% state$formal_names) {
      return(diagnostic_value(
        "lowering.formal_assignment",
        sprintf("Formal `%s` cannot be rebound by the current single-assignment lowerer.", binding_name),
        expr,
        data = list(symbol = binding_name)
      ))
    }
    if (!is.null(state$local_bindings[[binding_name]])) {
      return(diagnostic_value(
        "lowering.local_rebinding",
        sprintf("Local `%s` is already bound; rebinding is not lowerable yet.", binding_name),
        expr,
        data = list(symbol = binding_name)
      ))
    }

    result <- lower_expression(value_expr, state)
    if (length(result$diagnostics) > 0L) {
      return(result)
    }
    state$symbol_value_ids[[binding_name]] <- result$value_id
    state$local_bindings[[binding_name]] <- result$value_id
    result
  }

  lower_expression <- function(expr, state) {
    if (is.symbol(expr)) {
      return(lower_symbol(expr, state))
    }
    if (is.atomic(expr) && length(expr) == 1L) {
      return(lower_literal(expr, state))
    }
    if (!is.call(expr)) {
      return(diagnostic_value(
        "lowering.unsupported_expression",
        "Only symbols, scalar literals, and calls are lowerable for now.",
        expr
      ))
    }

    call_name <- tccq_call_name(expr)
    if (identical(call_name, "(") && length(expr) == 2L) {
      return(lower_expression(expr[[2L]], state))
    }
    if (identical(call_name, "[")) {
      return(lower_slice(expr, state))
    }
    if (identical(call_name, ":")) {
      return(diagnostic_value(
        "lowering.range_outside_slice",
        "Range expressions are only lowerable as `[` slice bounds for now.",
        expr
      ))
    }

    resolution <- tccq_resolve_call(registry, tccq_call(call_name, expr = expr), context)
    if (
      resolution@success &&
        S7::S7_inherits(resolution@value@elementwise, TccqElementwiseSpec)
    ) {
      return(lower_elementwise(resolution@value, as.list(expr)[-1L], expr, state))
    }
    if (
      resolution@success &&
        S7::S7_inherits(resolution@value@reduction, TccqReductionSpec)
    ) {
      return(lower_reduction(resolution@value, as.list(expr)[-1L], expr, state))
    }
    if (
      resolution@success &&
        S7::S7_inherits(resolution@value@contraction, TccqContractionSpec)
    ) {
      return(lower_contraction(resolution@value, as.list(expr)[-1L], expr, state))
    }
    if (resolution@success && (!isTRUE(resolution@value@pure) || isTRUE(resolution@value@boundary))) {
      return(diagnostic_value(
        "lowering.effectful_operation",
        sprintf("Operation `%s` is modeled but is not legal in a fused expression region.", call_name),
        expr,
        data = list(
          op = call_name,
          target = resolution@value@target,
          boundary = resolution@value@boundary,
          pure = resolution@value@pure
        )
      ))
    }
    if (!resolution@success) {
      return(diagnostic_value(
        "lowering.unimplemented_operation",
        sprintf("Operation `%s` has no lowerable implementation in this context.", call_name),
        expr,
        data = list(op = call_name, diagnostics = resolution@diagnostics)
      ))
    }

    diagnostic_value(
      "lowering.unsupported_call",
      sprintf("Call `%s` is not lowerable by the current expression pass.", call_name),
      expr,
      data = list(call = call_name)
    )
  }

  lower_symbol <- function(expr, state) {
    symbol_name <- as.character(expr)
    value_id <- state$symbol_value_ids[[symbol_name]]
    if (!is.null(value_id)) {
      value <- state$values[[value_id]]
      return(list(value_id = value_id, type = value@type, diagnostics = list()))
    }
    diagnostic_value(
      "lowering.unbound_symbol",
      sprintf("Symbol `%s` is not a declared binding.", symbol_name),
      expr,
      data = list(symbol = symbol_name)
    )
  }

  lower_literal <- function(expr, state) {
    literal <- tryCatch(
      tccq_literal_finite(expr),
      tccq_error = function(err) err
    )
    if (inherits(literal, "tccq_error")) {
      return(list(
        value_id = NULL,
        type = NULL,
        diagnostics = list(tccq_condition_diagnostic(literal))
      ))
    }

    value_id <- next_value_id(state)
    add_value(
      state,
      tccq_value(
        id = value_id,
        op = "literal",
        inputs = list(),
        type = literal@type,
        effect = tccq_effect(reads = FALSE),
        attrs = list(literal = literal)
      )
    )
    list(value_id = value_id, type = literal@type, diagnostics = list())
  }

  lower_elementwise <- function(resolved_operation, args, expr, state) {
    op <- resolved_operation@call@name
    elementwise_spec <- resolved_operation@elementwise
    signature <- elementwise_spec@signature
    if (!(length(args) %in% signature@arity)) {
      return(diagnostic_value(
        "lowering.unsupported_elementwise_arity",
        sprintf("Elementwise operation `%s` does not accept this argument count.", op),
        expr,
        data = list(op = op, arity = length(args), supported = signature@arity)
      ))
    }
    if (!isTRUE(resolved_operation@pure) || isTRUE(resolved_operation@boundary)) {
      return(diagnostic_value(
        "lowering.effectful_operation",
        sprintf("Operation `%s` is modeled but is not legal in an elementwise fused region.", op),
        expr,
        data = list(
          op = op,
          target = resolved_operation@target,
          boundary = resolved_operation@boundary,
          pure = resolved_operation@pure
        )
      ))
    }

    lowered_args <- lapply(args, lower_expression, state = state)
    diagnostics <- unlist(lapply(lowered_args, `[[`, "diagnostics"), recursive = FALSE)
    if (length(diagnostics) > 0L) {
      return(list(value_id = NULL, type = NULL, diagnostics = diagnostics))
    }

    input_ids <- lapply(lowered_args, `[[`, "value_id")
    input_types <- lapply(lowered_args, `[[`, "type")
    result_type <- tccq_elementwise_result_type(elementwise_spec, input_types)
    if (!result_type@success) {
      return(list(value_id = NULL, type = NULL, diagnostics = result_type@diagnostics))
    }

    value_id <- next_value_id(state)
    operation <- tccq_lowered_operation(
      "elementwise",
      resolved_operation,
      elementwise = elementwise_spec
    )
    add_value(
      state,
      tccq_value(
        id = value_id,
        op = op,
        inputs = input_ids,
        type = result_type@value,
        effect = resolved_operation@effect,
        attrs = list(operation = operation)
      )
    )
    list(value_id = value_id, type = result_type@value, diagnostics = list())
  }

  lower_reduction <- function(resolved_operation, args, expr, state) {
    reduction_spec <- resolved_operation@reduction
    reducer <- reduction_spec@name
    surface_op <- resolved_operation@call@name
    signature <- reduction_spec@signature
    if (!(length(args) %in% signature@arity)) {
      return(diagnostic_value(
        "lowering.unsupported_reducer_arity",
        sprintf("Reducer `%s` does not accept this argument count.", reducer),
        expr,
        data = list(reducer = reducer, arity = length(args), supported = signature@arity)
      ))
    }
    if (length(args) != 1L) {
      return(diagnostic_value(
        "lowering.unsupported_reducer_lowering_arity",
        sprintf("Reducer `%s` currently lowers exactly one expression argument.", reducer),
        expr,
        data = list(reducer = reducer, arity = length(args), supported = 1L)
      ))
    }

    if (!isTRUE(resolved_operation@pure) || isTRUE(resolved_operation@boundary)) {
      return(diagnostic_value(
        "lowering.effectful_operation",
        sprintf("Reducer `%s` is modeled but is not legal in a fused reduction region.", reducer),
        expr,
        data = list(
          op = reducer,
          target = resolved_operation@target,
          boundary = resolved_operation@boundary,
          pure = resolved_operation@pure
        )
      ))
    }

    lowered_arg <- lower_expression(args[[1L]], state)
    if (length(lowered_arg$diagnostics) > 0L) {
      return(lowered_arg)
    }
    if (lowered_arg$type@shape@rank == 0L) {
      return(diagnostic_value(
        "lowering.unsupported_reducer_rank",
        "The current reducer lowerer supports only non-scalar full-domain inputs.",
        expr,
        data = list(reducer = reducer, rank = lowered_arg$type@shape@rank)
      ))
    }
    result_type_result <- tccq_op_signature_result_type(signature, list(lowered_arg$type))
    if (!result_type_result@success) {
      return(list(value_id = NULL, type = NULL, diagnostics = result_type_result@diagnostics))
    }
    result_type <- result_type_result@value
    reduction_axes <- reduction_spec@attrs$reduction_axes %||% seq_len(lowered_arg$type@shape@rank)
    kept_axes <- reduction_spec@attrs$kept_axes %||% integer()
    reduction_kind <- if (length(kept_axes) > 0L) "axis" else "full"
    accumulator_type <- if (identical(reduction_kind, "axis")) {
      tccq_type(result_type@base)
    } else {
      result_type
    }
    identity_result <- tccq_reduction_identity(reduction_spec, accumulator_type)
    if (!identity_result@success) {
      return(list(value_id = NULL, type = NULL, diagnostics = identity_result@diagnostics))
    }
    value_id <- next_value_id(state)
    operation <- tccq_lowered_operation(
      "reduction",
      resolved_operation,
      reduction = reduction_spec,
      identity = identity_result@value,
      attrs = list(
        reduction_kind = reduction_kind,
        reduction_axes = as.integer(reduction_axes),
        kept_axes = as.integer(kept_axes),
        axis_kind = reduction_spec@attrs$axis_kind %||% ""
      )
    )
    add_value(
      state,
      tccq_value(
        id = value_id,
        op = surface_op,
        inputs = list(lowered_arg$value_id),
        type = result_type,
        effect = resolved_operation@effect,
        attrs = list(
          operation = operation,
          reduction_kind = reduction_kind,
          reduction_axes = as.integer(reduction_axes),
          kept_axes = as.integer(kept_axes)
        )
      )
    )
    list(value_id = value_id, type = result_type, diagnostics = list())
  }

  affine_bound <- function(expr, state) {
    if (is.numeric(expr) && length(expr) == 1L && !is.na(expr) && expr == as.integer(expr)) {
      return(list(symbol = "", offset = as.integer(expr)))
    }
    if (is.symbol(expr)) {
      symbol_name <- as.character(expr)
      if (symbol_name %in% state$dim_symbols) {
        return(list(symbol = symbol_name, offset = 0L))
      }
      return(NULL)
    }
    if (!is.call(expr)) {
      return(NULL)
    }
    bound_call_name <- tccq_call_name(expr)
    if (identical(bound_call_name, "(") && length(expr) == 2L) {
      return(affine_bound(expr[[2L]], state))
    }
    if (bound_call_name %in% c("+", "-") && length(expr) == 3L) {
      left <- affine_bound(expr[[2L]], state)
      right <- affine_bound(expr[[3L]], state)
      if (is.null(left) || is.null(right)) {
        return(NULL)
      }
      if (identical(bound_call_name, "-") && !nzchar(right$symbol)) {
        return(list(symbol = left$symbol, offset = left$offset - right$offset))
      }
      if (identical(bound_call_name, "+") && (!nzchar(left$symbol) || !nzchar(right$symbol))) {
        symbol <- if (nzchar(left$symbol)) left$symbol else right$symbol
        return(list(symbol = symbol, offset = left$offset + right$offset))
      }
      return(NULL)
    }
    if (identical(bound_call_name, "-") && length(expr) == 2L) {
      inner <- affine_bound(expr[[2L]], state)
      if (is.null(inner) || nzchar(inner$symbol)) {
        return(NULL)
      }
      return(list(symbol = "", offset = -inner$offset))
    }
    NULL
  }

  lower_slice <- function(expr, state) {
    if (length(expr) != 3L) {
      return(diagnostic_value(
        "lowering.unsupported_index",
        "Only single-axis `[` slices are lowerable for now.",
        expr,
        data = list(arity = length(expr) - 2L)
      ))
    }
    target <- lower_expression(expr[[2L]], state)
    if (length(target$diagnostics) > 0L) {
      return(target)
    }
    if (target$type@shape@rank != 1L) {
      return(diagnostic_value(
        "lowering.unsupported_slice_rank",
        "Slices currently apply to rank-1 values with one range index.",
        expr,
        data = list(rank = target$type@shape@rank)
      ))
    }
    index_expr <- expr[[3L]]
    if (!(is.call(index_expr) && identical(tccq_call_name(index_expr), ":") && length(index_expr) == 3L)) {
      return(diagnostic_value(
        "lowering.unsupported_index",
        "Slice indices must be `from:to` ranges for now.",
        expr,
        data = list(index = deparse1(index_expr))
      ))
    }
    from <- affine_bound(index_expr[[2L]], state)
    to <- affine_bound(index_expr[[3L]], state)
    if (is.null(from) || is.null(to)) {
      return(diagnostic_value(
        "lowering.non_affine_slice_bounds",
        "Slice bounds must be integer constants or declared dimension symbols plus offsets.",
        expr,
        data = list(index = deparse1(index_expr), dim_symbols = state$dim_symbols)
      ))
    }
    if (nzchar(from$symbol) || from$offset < 1L) {
      return(diagnostic_value(
        "lowering.unsupported_slice_lower_bound",
        "Slice lower bounds must be positive integer constants for now.",
        expr,
        data = list(from = deparse1(index_expr[[2L]]))
      ))
    }
    extent <- if (nzchar(to$symbol)) {
      tccq_dim_affine(to$symbol, to$offset - from$offset + 1L)
    } else {
      if (to$offset - from$offset + 1L < 0L) {
        return(diagnostic_value(
          "lowering.empty_slice",
          "Slice ranges must have non-negative extent.",
          expr,
          data = list(index = deparse1(index_expr))
        ))
      }
      tccq_dim_constant(to$offset - from$offset + 1L)
    }
    result_type <- tccq_type(target$type@base, tccq_shape(list(extent)))
    value_id <- next_value_id(state)
    add_value(
      state,
      tccq_value(
        id = value_id,
        op = "[",
        inputs = list(target$value_id),
        type = result_type,
        effect = tccq_effect(reads = TRUE),
        attrs = list(slice_offsets = from$offset - 1L)
      )
    )
    list(value_id = value_id, type = result_type, diagnostics = list())
  }

  lower_contraction <- function(resolved_operation, args, expr, state) {
    contraction_spec <- resolved_operation@contraction
    surface_op <- resolved_operation@call@name
    signature <- contraction_spec@signature
    if (!(length(args) %in% signature@arity)) {
      return(diagnostic_value(
        "lowering.unsupported_contraction_arity",
        sprintf("Contraction `%s` does not accept this argument count.", surface_op),
        expr,
        data = list(op = surface_op, arity = length(args), supported = signature@arity)
      ))
    }
    if (!isTRUE(resolved_operation@pure) || isTRUE(resolved_operation@boundary)) {
      return(diagnostic_value(
        "lowering.effectful_operation",
        sprintf("Contraction `%s` is modeled but is not legal in a fused region.", surface_op),
        expr,
        data = list(
          op = surface_op,
          target = resolved_operation@target,
          boundary = resolved_operation@boundary,
          pure = resolved_operation@pure
        )
      ))
    }

    lowered_args <- lapply(args, lower_expression, state = state)
    diagnostics <- unlist(lapply(lowered_args, `[[`, "diagnostics"), recursive = FALSE)
    if (length(diagnostics) > 0L) {
      return(list(value_id = NULL, type = NULL, diagnostics = diagnostics))
    }

    input_ids <- lapply(lowered_args, `[[`, "value_id")
    input_types <- lapply(lowered_args, `[[`, "type")
    result_type_result <- tccq_op_signature_result_type(signature, input_types)
    if (!result_type_result@success) {
      return(list(value_id = NULL, type = NULL, diagnostics = result_type_result@diagnostics))
    }
    result_type <- result_type_result@value
    identity_result <- tccq_reduction_identity(
      contraction_spec@reducer,
      tccq_type(result_type@base)
    )
    if (!identity_result@success) {
      return(list(value_id = NULL, type = NULL, diagnostics = identity_result@diagnostics))
    }
    value_id <- next_value_id(state)
    operation <- tccq_lowered_operation(
      "contraction",
      resolved_operation,
      contraction = contraction_spec,
      identity = identity_result@value
    )
    add_value(
      state,
      tccq_value(
        id = value_id,
        op = surface_op,
        inputs = input_ids,
        type = result_type,
        effect = resolved_operation@effect,
        attrs = list(operation = operation)
      )
    )
    list(value_id = value_id, type = result_type, diagnostics = list())
  }

  lowered_operation <- function(value) {
    operation <- value@attrs$operation
    if (S7::S7_inherits(operation, TccqLoweredOperation)) operation else NULL
  }

  value_is_reduction <- function(value) {
    operation <- lowered_operation(value)
    !is.null(operation) && identical(operation@family, "reduction")
  }

  value_is_axis_reduction <- function(value) {
    operation <- lowered_operation(value)
    !is.null(operation) &&
      identical(operation@family, "reduction") &&
      identical(operation@attrs$reduction_kind, "axis")
  }

  value_is_contraction <- function(value) {
    operation <- lowered_operation(value)
    !is.null(operation) && identical(operation@family, "contraction")
  }

  value_is_slice <- function(value) {
    identical(value@op, "[")
  }

  plan_regions <- function(values, result_id) {
    result_value <- values[[result_id]]
    result_operation <- lowered_operation(result_value)
    result_is_reduction <- value_is_reduction(result_value)
    result_is_contraction <- value_is_contraction(result_value)
    domain_shape <- if (isTRUE(result_is_reduction)) {
      values[[result_value@inputs[[1L]]]]@type@shape
    } else if (isTRUE(result_is_contraction)) {
      contracted_dim <- values[[result_value@inputs[[1L]]]]@type@shape@dims[[2L]]
      tccq_shape(c(result_value@type@shape@dims, list(contracted_dim)))
    } else {
      result_value@type@shape
    }
    domain <- tccq_domain("domain_main", domain_shape)
    lowered_values <- unname(values)
    operation_values <- Filter(
      function(value) !value@op %in% c("formal", "literal"),
      lowered_values
    )
    operation_value_ids <- vapply(operation_values, function(value) value@id, character(1))
    lowered_operations <- lapply(operation_values, lowered_operation)
    lowered_operation_is_typed <- vapply(
      lowered_operations,
      function(operation) S7::S7_inherits(operation, TccqLoweredOperation),
      logical(1)
    )
    lowered_operations <- lowered_operations[lowered_operation_is_typed]
    names(lowered_operations) <- operation_value_ids[lowered_operation_is_typed]
    if (length(lowered_operations) == 0L) {
      region_effect <- tccq_effect()
      return(list(tccq_region(
        "region_main",
        "host",
        values = lowered_values,
        fusion_groups = list(),
        effect = region_effect,
        memory_space = "host",
        touches_rapi = FALSE,
        attrs = list(result = result_id, operation = result_operation)
      )))
    }
    resolved_operations <- Filter(
      function(resolved_operation) S7::S7_inherits(resolved_operation, TccqResolvedOp),
      lapply(lowered_operations, function(operation) operation@resolved_op)
    )
    accesses <- lapply(lowered_values, function(value) {
      access_kind <- if (value@type@shape@rank == 0L) {
        "scalar"
      } else if (value_is_slice(value)) {
        "slice"
      } else {
        "identity"
      }
      tccq_access(value@id, domain, kind = access_kind)
    })
    region_kind <- if (domain_shape@rank > 0L) "kernel" else "host"
    region_target <- common_region_field(
      resolved_operations,
      function(resolved_operation) resolved_operation@target,
      default = "any"
    )
    region_effect <- combine_effects(lapply(operation_values, function(value) value@effect))

    # Every non-root scalar reduction is its own fused nest over its input
    # domain; the remaining operations fuse into the main group over the
    # result domain. This value-level partition is the typed record of the
    # multi-nest composition decision the loop-nest planner realizes.
    intermediate_reductions <- Filter(function(value) {
      value_is_reduction(value) &&
        value@type@shape@rank == 0L &&
        !identical(value@id, result_id)
    }, operation_values)
    intermediate_ids <- vapply(intermediate_reductions, function(value) value@id, character(1))
    intermediate_groups <- unname(Map(function(value, group_index) {
      input_shape <- values[[value@inputs[[1L]]]]@type@shape
      group_domain <- tccq_domain(sprintf("domain_%04d", group_index), input_shape)
      operation <- lowered_operation(value)
      tccq_fusion_group(
        sprintf("fusion_%04d", group_index),
        "map_reduce",
        domain = group_domain,
        values = list(value),
        outputs = value@id,
        accesses = list(tccq_access(value@id, group_domain, kind = "scalar")),
        region_kind = region_kind,
        target = region_target,
        effect = value@effect,
        contract = tccq_fusion_contract(
          "map_reduce",
          operations = stats::setNames(list(operation), value@id),
          result_operation = operation
        )
      )
    }, intermediate_reductions, seq_along(intermediate_reductions)))

    main_values <- Filter(
      function(value) !value@id %in% intermediate_ids,
      operation_values
    )
    fusion_kind <- if (isTRUE(result_is_contraction)) {
      "contract"
    } else if (isTRUE(value_is_axis_reduction(result_value))) {
      "axis_reduce"
    } else if (isTRUE(result_is_reduction)) {
      "map_reduce"
    } else if (any(vapply(operation_values, value_is_slice, logical(1)))) {
      "stencil"
    } else {
      "map"
    }
    fusion_contract <- tccq_fusion_contract(
      fusion_kind,
      operations = lowered_operations[setdiff(names(lowered_operations), intermediate_ids)],
      result_operation = result_operation
    )
    fusion <- tccq_fusion_group(
      "fusion_main",
      fusion_kind,
      domain = domain,
      values = main_values,
      outputs = result_id,
      accesses = accesses,
      region_kind = region_kind,
      target = region_target,
      effect = region_effect,
      contract = fusion_contract
    )
    list(tccq_region(
      "region_main",
      region_kind,
      values = lowered_values,
      fusion_groups = c(intermediate_groups, list(fusion)),
      effect = region_effect,
      memory_space = "host",
      touches_rapi = FALSE,
      attrs = list(result = result_id, operation = result_operation)
    ))
  }

  common_region_field <- function(resolved_operations, field, default) {
    values <- unique(vapply(resolved_operations, field, character(1)))
    values <- setdiff(values, "any")
    if (length(values) == 1L) {
      return(values[[1L]])
    }
    default
  }

  combine_effects <- function(effects) {
    if (length(effects) == 0L) {
      return(tccq_effect())
    }
    tccq_effect(
      reads = any(vapply(effects, function(effect) effect@reads, logical(1))),
      writes = any(vapply(effects, function(effect) effect@writes, logical(1))),
      allocates = any(vapply(effects, function(effect) effect@allocates, logical(1))),
      boundary = any(vapply(effects, function(effect) effect@boundary, logical(1))),
      may_error = any(vapply(effects, function(effect) effect@may_error, logical(1)))
    )
  }

  result_strategy <- function(values, result_id) {
    result_value <- values[[result_id]]
    if (value_is_contraction(result_value)) {
      return("contract")
    }
    if (value_is_axis_reduction(result_value)) {
      return("axis-reduce")
    }
    if (value_is_reduction(result_value)) {
      return("map-reduce")
    }
    if (any(vapply(unname(values), value_is_slice, logical(1)))) {
      return("stencil")
    }
    "elementwise"
  }

  plan_storage <- function(values, result_id) {
    lowered_values <- unname(values)
    lifetimes <- storage_lifetimes(lowered_values, result_id)
    slots <- Map(function(value, slot_index) {
      role <- storage_role(value, result_id)
      materialized <- role %in% c("input", "output")
      reusable <- identical(role, "temporary") && !isTRUE(materialized)
      lifetime <- lifetimes[[value@id]]
      tccq_storage_slot(
        id = sprintf("slot_%04d", slot_index),
        value_id = value@id,
        type = value@type,
        role = role,
        materialized = materialized,
        reusable = reusable,
        lifetime = lifetime,
        attrs = list(op = value@op)
      )
    }, lowered_values, seq_along(lowered_values))
    slot_ids <- vapply(slots, function(slot) slot@id, character(1))
    slots_by_id <- slots
    names(slots_by_id) <- slot_ids
    reuse_groups <- storage_reuse_groups(slots_by_id)
    tccq_storage_plan(
      slots = unname(slots_by_id),
      reuse_groups = reuse_groups,
      attrs = list(
        strategy = sprintf("fused-%s", result_strategy(values, result_id)),
        lifetimes = lifetimes
      )
    )
  }

  storage_lifetimes <- function(lowered_values, result_id) {
    value_ids <- vapply(lowered_values, function(value) value@id, character(1))
    def_positions <- seq_along(value_ids)
    names(def_positions) <- value_ids
    last_use_positions <- def_positions

    for (use_position in seq_along(lowered_values)) {
      value <- lowered_values[[use_position]]
      for (input_id in vapply(value@inputs, as.character, character(1))) {
        if (input_id %in% names(last_use_positions)) {
          last_use_positions[[input_id]] <- max(last_use_positions[[input_id]], use_position)
        }
      }
    }
    if (result_id %in% names(last_use_positions)) {
      last_use_positions[[result_id]] <- length(lowered_values) + 1L
    }

    lifetimes <- Map(function(value_id, def_position, last_use_position) {
      tccq_storage_lifetime(
        value_id,
        defined_at = unname(def_position),
        last_used_at = unname(last_use_position)
      )
    }, value_ids, def_positions, last_use_positions)
    names(lifetimes) <- value_ids
    lifetimes
  }

  storage_reuse_groups <- function(slots_by_id) {
    temporary_slots <- Filter(
      function(slot) identical(slot@role, "temporary") && isTRUE(slot@reusable),
      slots_by_id
    )
    ordered_slots <- temporary_slots[order(vapply(
      temporary_slots,
      function(slot) slot@lifetime@defined_at,
      integer(1)
    ))]
    reuse_groups <- list()

    for (slot in ordered_slots) {
      added_to_group <- FALSE
      for (group_index in seq_along(reuse_groups)) {
        group <- reuse_groups[[group_index]]
        can_share_group <- all(vapply(
          group,
          function(slot_id) storage_slots_can_share(slots_by_id[[slot_id]], slot),
          logical(1)
        ))
        if (isTRUE(can_share_group)) {
          reuse_groups[[group_index]] <- c(group, slot@id)
          added_to_group <- TRUE
          break
        }
      }
      if (!isTRUE(added_to_group)) {
        reuse_groups[[length(reuse_groups) + 1L]] <- slot@id
      }
    }

    Filter(function(group) length(group) > 1L, reuse_groups)
  }

  storage_slots_can_share <- function(existing_slot, candidate_slot) {
    storage_types_compatible(existing_slot@type, candidate_slot@type) &&
      existing_slot@lifetime@last_used_at < candidate_slot@lifetime@defined_at
  }

  storage_types_compatible <- function(left_type, right_type) {
    identical(left_type@base, right_type@base) &&
      identical(left_type@shape@rank, right_type@shape@rank) &&
      identical(left_type@shape@dims, right_type@shape@dims)
  }

  storage_role <- function(value, result_id) {
    if (identical(value@id, result_id)) {
      return("output")
    }
    if (identical(value@op, "formal")) {
      return("input")
    }
    if (identical(value@op, "literal")) {
      return("literal")
    }
    "temporary"
  }

  diagnostic_value <- function(code, message, expr, data = list()) {
    list(
      value_id = NULL,
      type = NULL,
      diagnostics = list(tccq_diagnostic(
        code,
        message,
        phase = "lowering",
        path = "expression",
        data = c(list(expr = deparse1(expr)), data)
      ))
    )
  }

  if (!is.function(fn)) {
    return(new_plan(diagnostics = list(tccq_diagnostic(
      "lowering.not_function",
      "`fn` must be a function.",
      phase = "lowering",
      path = "fn",
      data = list(type = typeof(fn))
    ))))
  }
  .tccq_check_list_of(bindings, TccqBinding, "TccqBinding", "bindings")
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  .tccq_check_s7(context, TccqOpContext, "TccqOpContext", "context")
  if (is.null(call_index)) {
    call_index <- tccq_collect_call_index(
      body(fn),
      global_calls = codetools::findGlobals(fn, merge = FALSE)$functions,
      env = environment(fn)
    )
  }
  .tccq_check_s7(call_index, TccqCallIndex, "TccqCallIndex", "call_index")

  expressions <- executable_expressions(body(fn))
  if (length(expressions) == 0L) {
    return(new_plan())
  }

  # The kernel model replaces a call with a registry implementation: lazy
  # closure forcing is compatible when arguments are pure, and a name the
  # environment does not bind is registry vocabulary, not invalid R. The one
  # observed fact no implementation may claim away is runtime S3 dispatch --
  # the method table is not a compile-time fact, so the environment's generic
  # could disagree with the registry's single implementation at run time.
  # Declaration vocabulary inside declare(type(...)) is not an operation
  # candidate, so the barrier only judges calls in executable statements.
  executable_call_names <- unique(vapply(
    unlist(lapply(expressions, tccq_collect_calls), recursive = FALSE),
    function(call) call@name,
    character(1)
  ))
  semantics_barrier <- function(semantics) {
    call <- semantics@call
    if (!call@name %in% executable_call_names) {
      return(NULL)
    }
    if (!identical(semantics@dispatch_kind, "s3")) {
      return(NULL)
    }
    tccq_diagnostic(
      "lowering.semantics_barrier",
      sprintf(
        "Call `%s` cannot enter a kernel region: runtime S3 dispatch cannot be resolved to one typed implementation.",
        call@name
      ),
      phase = "lowering",
      path = sprintf("call_index.%s", call@id),
      data = list(
        call = call@name,
        evaluator_kind = semantics@evaluator_kind,
        forcing_policy = semantics@forcing_policy,
        dispatch_kind = semantics@dispatch_kind
      )
    )
  }
  barrier_diagnostics <- Filter(
    Negate(is.null),
    lapply(call_index@semantics, semantics_barrier)
  )
  if (length(barrier_diagnostics) > 0L) {
    return(new_plan(diagnostics = barrier_diagnostics))
  }

  state <- new_lowering_state()
  result <- NULL
  for (expr in expressions) {
    result <- lower_statement(expr, state)
    if (length(result$diagnostics) > 0L) {
      return(new_plan(
        values = state$values,
        diagnostics = result$diagnostics,
        attrs = list(local_bindings = state$local_bindings)
      ))
    }
  }

  unsupported_compositions <- Filter(function(value) {
    operation <- lowered_operation(value)
    if (is.null(operation) || identical(value@id, result$value_id)) {
      return(FALSE)
    }
    if (identical(operation@family, "contraction")) {
      return(TRUE)
    }
    identical(operation@family, "reduction") && value@type@shape@rank > 0L
  }, unname(state$values))
  if (length(unsupported_compositions) > 0L) {
    return(new_plan(
      values = state$values,
      diagnostics = list(tccq_diagnostic(
        "lowering.unsupported_composition",
        "Non-root contractions and axis reductions need materialized array intermediates; only scalar reductions may feed later loop nests.",
        phase = "lowering",
        path = "expression",
        data = list(value_ids = vapply(
          unsupported_compositions,
          function(value) value@id,
          character(1)
        ))
      )),
      attrs = list(local_bindings = state$local_bindings)
    ))
  }

  storage_plan <- plan_storage(state$values, result$value_id)
  regions <- plan_regions(state$values, result$value_id)
  new_plan(
    values = state$values,
    regions = regions,
    result = result$value_id,
    storage_plan = storage_plan,
    attrs = list(
      strategy = sprintf("%s-expression", result_strategy(state$values, result$value_id)),
      local_bindings = state$local_bindings
    )
  )
}
