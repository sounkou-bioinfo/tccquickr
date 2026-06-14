#' Lower a declared function into a backend-neutral plan
#'
#' The lowerer produces `TccqValue`, `TccqRegion`, `TccqFusionGroup`, and
#' `TccqStoragePlan` objects. It is a middle-end pass result, not a C, Fortran,
#' TinyCC, or CUDA lowering.
#'
#' @param fn Function to lower.
#' @param bindings Declared formal bindings.
#' @param registry Operation registry used for operation availability.
#' @param context Operation support context.
#' @export
tccq_lower_function <- function(
  fn,
  bindings,
  registry = tccq_default_op_registry(),
  context = tccq_op_context()
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
    state$symbol_value_ids <- list()
    state$values <- list()
    state$value_counter <- 0L
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

    if (identical(call_name, "-") && length(expr) == 2L) {
      return(lower_call("negate", as.list(expr)[-1L], expr, state))
    }
    if (call_name %in% c("+", "-", "*", "/") && length(expr) == 3L) {
      return(lower_call(call_name, as.list(expr)[-1L], expr, state))
    }
    if (call_name %in% c("sqrt", "exp") && length(expr) == 2L) {
      return(lower_call(call_name, as.list(expr)[-1L], expr, state))
    }
    if (identical(call_name, "sum")) {
      return(lower_reduction(call_name, as.list(expr)[-1L], expr, state))
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

  lower_call <- function(op, args, expr, state) {
    call <- tccq_call(if (identical(op, "negate")) "-" else op, expr = expr)
    resolution <- tccq_resolve_call(registry, call, context)
    if (!resolution@success) {
      return(diagnostic_value(
        "lowering.unimplemented_operation",
        sprintf("Operation `%s` has no lowerable implementation in this context.", op),
        expr,
        data = list(op = op, diagnostics = resolution@diagnostics)
      ))
    }
    resolved_operation <- resolution@value
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
    result_type <- infer_result_type(op, input_types)
    if (S7::S7_inherits(result_type, TccqDiagnostic)) {
      return(list(value_id = NULL, type = NULL, diagnostics = list(result_type)))
    }

    value_id <- next_value_id(state)
    add_value(
      state,
      tccq_value(
        id = value_id,
        op = op,
        inputs = input_ids,
        type = result_type,
        effect = resolved_operation@effect,
        attrs = list(
          lowering = "elementwise",
          resolved_op = resolved_operation
        )
      )
    )
    list(value_id = value_id, type = result_type, diagnostics = list())
  }

  lower_reduction <- function(reducer, args, expr, state) {
    if (length(args) != 1L) {
      return(diagnostic_value(
        "lowering.unsupported_reducer_arity",
        sprintf("Reducer `%s` currently accepts exactly one expression argument.", reducer),
        expr,
        data = list(reducer = reducer, arity = length(args))
      ))
    }

    call <- tccq_call(reducer, expr = expr)
    resolution <- tccq_resolve_call(registry, call, context)
    if (!resolution@success) {
      return(diagnostic_value(
        "lowering.unimplemented_operation",
        sprintf("Reducer `%s` has no lowerable implementation in this context.", reducer),
        expr,
        data = list(op = reducer, diagnostics = resolution@diagnostics)
      ))
    }
    resolved_operation <- resolution@value
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
    if (lowered_arg$type@shape@rank != 1L) {
      return(diagnostic_value(
        "lowering.unsupported_reducer_rank",
        "The current reducer lowerer supports only rank-one inputs.",
        expr,
        data = list(reducer = reducer, rank = lowered_arg$type@shape@rank)
      ))
    }
    if (!lowered_arg$type@base %in% c("integer", "double")) {
      return(diagnostic_value(
        "lowering.unsupported_type",
        "The current reducer lowerer only supports integer and double values.",
        expr,
        data = list(base = lowered_arg$type@base)
      ))
    }

    result_type <- tccq_type(lowered_arg$type@base)
    identity_value <- if (identical(result_type@base, "integer")) 0L else 0
    identity <- tccq_literal_finite(identity_value, type = result_type)
    value_id <- next_value_id(state)
    add_value(
      state,
      tccq_value(
        id = value_id,
        op = reducer,
        inputs = list(lowered_arg$value_id),
        type = result_type,
        effect = resolved_operation@effect,
        attrs = list(
          lowering = "reduction",
          reducer = reducer,
          identity = identity,
          resolved_op = resolved_operation
        )
      )
    )
    list(value_id = value_id, type = result_type, diagnostics = list())
  }

  infer_result_type <- function(op, input_types) {
    unsupported_bases <- setdiff(
      unique(vapply(input_types, function(type) type@base, character(1))),
      c("integer", "double")
    )
    if (length(unsupported_bases) > 0L) {
      return(tccq_diagnostic(
        "lowering.unsupported_type",
        "The current expression lowerer only supports integer and double values.",
        phase = "lowering",
        path = "expression.type",
        data = list(base = unsupported_bases)
      ))
    }

    non_scalar_types <- Filter(function(type) type@shape@rank > 0L, input_types)
    shape <- tccq_shape()
    if (length(non_scalar_types) > 0L) {
      shape_keys <- unique(vapply(non_scalar_types, shape_key, character(1)))
      if (length(shape_keys) != 1L) {
        return(tccq_diagnostic(
          "lowering.incompatible_shapes",
          "Vectorized expression inputs must have the same shape.",
          phase = "lowering",
          path = "expression.shape",
          data = list(shapes = shape_keys)
        ))
      }
      shape <- non_scalar_types[[1L]]@shape
    }

    result_base <- if (
      op %in% c("/", "sqrt", "exp") ||
        any(vapply(input_types, function(type) identical(type@base, "double"), logical(1)))
    ) {
      "double"
    } else {
      "integer"
    }
    tccq_type(result_base, shape)
  }

  shape_key <- function(type) {
    paste(
      vapply(type@shape@dims, function(dim) {
        if (identical(dim@kind, "constant")) {
          return(sprintf("constant:%d", dim@value))
        }
        if (identical(dim@kind, "symbol")) {
          return(sprintf("symbol:%s", dim@label))
        }
        "unknown"
      }, character(1)),
      collapse = "/"
    )
  }

  plan_regions <- function(values, result_id) {
    result_value <- values[[result_id]]
    result_is_reduction <- identical(result_value@attrs$lowering, "reduction")
    domain_shape <- if (isTRUE(result_is_reduction)) {
      values[[result_value@inputs[[1L]]]]@type@shape
    } else {
      result_value@type@shape
    }
    domain <- tccq_domain("domain_main", domain_shape)
    lowered_values <- unname(values)
    operation_values <- Filter(
      function(value) !value@op %in% c("formal", "literal"),
      lowered_values
    )
    resolved_operations <- Filter(
      function(resolved_operation) S7::S7_inherits(resolved_operation, TccqResolvedOp),
      lapply(operation_values, function(value) value@attrs$resolved_op)
    )
    accesses <- lapply(lowered_values, function(value) {
      access_kind <- if (value@type@shape@rank == 0L) "scalar" else "identity"
      tccq_access(value@id, domain, kind = access_kind)
    })
    region_kind <- if (domain_shape@rank > 0L) "kernel" else "host"
    region_target <- common_region_field(
      resolved_operations,
      function(resolved_operation) resolved_operation@target,
      default = "any"
    )
    region_effect <- combine_effects(lapply(operation_values, function(value) value@effect))
    fusion_kind <- if (isTRUE(result_is_reduction)) "map_reduce" else "map"
    fusion_attrs <- if (isTRUE(result_is_reduction)) {
      list(
        storage = "fused-map-reduce",
        reducer = result_value@attrs$reducer,
        identity = result_value@attrs$identity
      )
    } else {
      list(storage = "fused-elementwise")
    }
    fusion <- tccq_fusion_group(
      "fusion_main",
      fusion_kind,
      domain = domain,
      values = operation_values,
      outputs = result_id,
      accesses = accesses,
      region_kind = region_kind,
      target = region_target,
      effect = region_effect,
      attrs = fusion_attrs
    )
    list(tccq_region(
      "region_main",
      region_kind,
      values = lowered_values,
      fusion_groups = list(fusion),
      effect = region_effect,
      memory_space = "host",
      touches_rapi = FALSE,
      attrs = list(result = result_id, lowering = result_value@attrs$lowering)
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

  plan_storage <- function(values, result_id) {
    lowered_values <- unname(values)
    slots <- Map(function(value, slot_index) {
      role <- storage_role(value, result_id)
      materialized <- role %in% c("input", "output")
      reusable <- identical(role, "temporary") && !isTRUE(materialized)
      tccq_storage_slot(
        id = sprintf("slot_%04d", slot_index),
        value_id = value@id,
        type = value@type,
        role = role,
        materialized = materialized,
        reusable = reusable,
        attrs = list(op = value@op)
      )
    }, lowered_values, seq_along(lowered_values))
    reusable_slot_ids <- vapply(
      Filter(function(slot) identical(slot@role, "temporary"), slots),
      function(slot) slot@id,
      character(1)
    )
    reuse_groups <- if (length(reusable_slot_ids) > 0L) {
      list(reusable_slot_ids)
    } else {
      list()
    }
    tccq_storage_plan(
      slots = slots,
      reuse_groups = reuse_groups,
      attrs = list(strategy = if (identical(values[[result_id]]@attrs$lowering, "reduction")) {
        "fused-map-reduce"
      } else {
        "fused-elementwise"
      })
    )
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

  expressions <- executable_expressions(body(fn))
  if (length(expressions) == 0L) {
    return(new_plan())
  }

  state <- new_lowering_state()
  result <- lower_expression(expressions[[length(expressions)]], state)
  if (length(result$diagnostics) > 0L) {
    return(new_plan(values = state$values, diagnostics = result$diagnostics))
  }

  storage_plan <- plan_storage(state$values, result$value_id)
  regions <- plan_regions(state$values, result$value_id)
  new_plan(
    values = state$values,
    regions = regions,
    result = result$value_id,
    storage_plan = storage_plan,
    attrs = list(strategy = if (identical(state$values[[result$value_id]]@attrs$lowering, "reduction")) {
      "map-reduce-expression"
    } else {
      "elementwise-expression"
    })
  )
}
