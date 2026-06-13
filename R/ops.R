#' R call observed by the frontend
#'
#' @param name Call name.
#' @param expr Original R call expression.
#' @param origin Source of the call observation.
#' @export
TccqCall <- S7::new_class(
  "TccqCall",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    expr = S7::class_any,
    origin = S7::class_character
  )
)

#' Operation support query context
#'
#' @param phase Compiler phase issuing the query.
#' @param target Requested implementation target, or `any`.
#' @param region_kind Requested execution region kind, or `any`.
#' @param memory_space Requested memory space, or `any`.
#' @param allow_rapi Whether implementations touching the R C API are allowed.
#' @param allow_boundary Whether boundary/fallback implementations are allowed.
#' @export
TccqOpContext <- S7::new_class(
  "TccqOpContext",
  package = "tccquickr",
  properties = list(
    phase = S7::class_character,
    target = S7::class_character,
    region_kind = S7::class_character,
    memory_space = S7::class_character,
    allow_rapi = S7::class_logical,
    allow_boundary = S7::class_logical
  )
)

#' Operation implementation descriptor
#'
#' @param op Operation or function name.
#' @param target Implementation target, such as `syntax`, `r_api`, `pure_c`,
#'   `fortran`, `mojo`, or `cuda`.
#' @param region_kind Region kind the implementation can run in, or `any`.
#' @param memory_space Memory space the implementation expects, or `any`.
#' @param uses_rapi Whether the implementation touches the R C API.
#' @param boundary Whether the implementation is a boundary/fallback.
#' @param pure Whether the implementation is semantically pure.
#' @param supports Predicate receiving a `TccqCall` and `TccqOpContext`.
#' @export
TccqOpImpl <- S7::new_class(
  "TccqOpImpl",
  package = "tccquickr",
  properties = list(
    op = S7::class_character,
    target = S7::class_character,
    region_kind = S7::class_character,
    memory_space = S7::class_character,
    uses_rapi = S7::class_logical,
    boundary = S7::class_logical,
    pure = S7::class_logical,
    supports = S7::class_function
  )
)

#' Operation registry
#'
#' @param implementations List of operation implementations.
#' @export
TccqOpRegistry <- S7::new_class(
  "TccqOpRegistry",
  package = "tccquickr",
  properties = list(
    implementations = S7::class_list
  )
)

#' Query operation implementation support
#'
#' @param impl Operation implementation.
#' @param call Observed R call.
#' @param context Operation query context.
#' @export
tccq_op_supports <- S7::new_generic(
  "tccq_op_supports",
  dispatch_args = "impl",
  function(impl, call, context) S7::S7_dispatch()
)

#' Operation implementation trait
#'
#' Implementations must explicitly opt into this trait before they can be used
#' by an operation registry.
#'
#' @export
TccqOpImplementation <- s7contract::new_trait(
  "TccqOpImplementation",
  package = "tccquickr",
  methods = list(
    supports = s7contract::trait_method(
      tccq_op_supports,
      args = list(call = TccqCall, context = TccqOpContext),
      returns = S7::class_logical
    )
  )
)

tccq_register_traits <- function() {
  s7contract::impl_trait(
    TccqOpImplementation,
    TccqOpImpl,
    methods = list(
      supports = function(impl, call, context) {
        .tccq_op_impl_supports(impl, call, context)
      }
    ),
    replace = TRUE
  )
  invisible(TRUE)
}

#' Construct an observed call
#'
#' @param name Call name.
#' @param expr Original R call expression.
#' @param origin Source of the call observation.
#' @export
tccq_call <- function(name, expr = NULL, origin = "ast") {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_character_scalar(origin, "origin")
  TccqCall(name = name, expr = expr, origin = origin)
}

#' Construct an operation support context
#'
#' @param phase Compiler phase issuing the query.
#' @param target Requested implementation target, or `any`.
#' @param region_kind Requested execution region kind, or `any`.
#' @param memory_space Requested memory space, or `any`.
#' @param allow_rapi Whether implementations touching the R C API are allowed.
#' @param allow_boundary Whether boundary/fallback implementations are allowed.
#' @export
tccq_op_context <- function(
  phase = "frontend",
  target = "any",
  region_kind = "any",
  memory_space = "any",
  allow_rapi = TRUE,
  allow_boundary = FALSE
) {
  .tccq_check_character_scalar(phase, "phase")
  .tccq_check_character_scalar(target, "target")
  .tccq_check_region_query_kind(region_kind, "region_kind")
  .tccq_check_memory_space_query(memory_space, "memory_space")
  .tccq_check_logical_scalar(allow_rapi, "allow_rapi")
  .tccq_check_logical_scalar(allow_boundary, "allow_boundary")

  TccqOpContext(
    phase = phase,
    target = target,
    region_kind = region_kind,
    memory_space = memory_space,
    allow_rapi = allow_rapi,
    allow_boundary = allow_boundary
  )
}

#' Construct an operation implementation
#'
#' @param op Operation or function name.
#' @param target Implementation target.
#' @param region_kind Region kind the implementation can run in, or `any`.
#' @param memory_space Memory space the implementation expects, or `any`.
#' @param uses_rapi Whether the implementation touches the R C API.
#' @param boundary Whether the implementation is a boundary/fallback.
#' @param pure Whether the implementation is semantically pure.
#' @param supports Predicate receiving a `TccqCall` and `TccqOpContext`.
#' @export
tccq_op_impl <- function(
  op,
  target,
  region_kind = "any",
  memory_space = "any",
  uses_rapi = FALSE,
  boundary = FALSE,
  pure = TRUE,
  supports = function(call, context) TRUE
) {
  .tccq_check_character_scalar(op, "op")
  .tccq_check_character_scalar(target, "target")
  .tccq_check_region_query_kind(region_kind, "region_kind")
  .tccq_check_memory_space_query(memory_space, "memory_space")
  .tccq_check_logical_scalar(uses_rapi, "uses_rapi")
  .tccq_check_logical_scalar(boundary, "boundary")
  .tccq_check_logical_scalar(pure, "pure")
  if (!is.function(supports)) {
    tccq_abort(
      "schema.invalid_op_supports",
      "`supports` must be a predicate function.",
      phase = "schema",
      path = "op.supports"
    )
  }

  TccqOpImpl(
    op = op,
    target = target,
    region_kind = region_kind,
    memory_space = memory_space,
    uses_rapi = uses_rapi,
    boundary = boundary,
    pure = pure,
    supports = supports
  )
}

#' Construct an operation registry
#'
#' @param implementations List of operation implementations.
#' @export
tccq_op_registry <- function(implementations = list()) {
  if (S7::S7_inherits(implementations, TccqOpImpl)) {
    implementations <- list(implementations)
  }
  .tccq_check_list_of(
    implementations,
    TccqOpImpl,
    "TccqOpImpl",
    "implementations"
  )
  for (i in seq_along(implementations)) {
    s7contract::assert_trait(
      implementations[[i]],
      TccqOpImplementation,
      arg = sprintf("implementations[[%d]]", i)
    )
  }

  TccqOpRegistry(implementations = implementations)
}

#' Default operation registry for the reset frontend
#'
#' @export
tccq_default_op_registry <- function() {
  syntax_ops <- c("{", "(", "<-", "=", "declare", "type", TCCQ_BASE_TYPES)
  scalar_ops <- c("+", "-", "*", "/", "^", "sqrt", "exp")
  tccq_op_registry(c(
    lapply(syntax_ops, function(op) {
      tccq_op_impl(op, target = "syntax", pure = FALSE)
    }),
    lapply(scalar_ops, function(op) {
      tccq_op_impl(op, target = "pure_c", region_kind = "kernel")
    })
  ))
}

#' Add operation implementations to a registry
#'
#' @param registry Operation registry.
#' @param implementations Operation implementation or list of implementations.
#' @export
tccq_op_registry_add <- function(registry, implementations) {
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  if (S7::S7_inherits(implementations, TccqOpImpl)) {
    implementations <- list(implementations)
  }
  tccq_op_registry(c(registry@implementations, implementations))
}

#' Collect calls from an R expression
#'
#' @param expr R expression to inspect.
#' @param global_calls Additional function names found by `codetools`.
#' @export
tccq_collect_calls <- function(expr, global_calls = character()) {
  calls <- list()
  walk <- function(node) {
    if (!is.call(node)) {
      return(NULL)
    }
    calls[[length(calls) + 1L]] <<- tccq_call(tccq_call_name(node), node, "ast")
    for (child in as.list(node)[-1L]) {
      walk(child)
    }
    NULL
  }
  walk(expr)
  for (name in global_calls) {
    calls[[length(calls) + 1L]] <- tccq_call(name, origin = "codetools")
  }
  calls
}

#' Find unsupported calls for an operation registry and context
#'
#' @param calls List of `TccqCall` objects.
#' @param registry Operation registry.
#' @param context Operation query context.
#' @export
tccq_unsupported_calls <- function(
  calls,
  registry = tccq_default_op_registry(),
  context = tccq_op_context()
) {
  .tccq_check_list_of(calls, TccqCall, "TccqCall", "calls")
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  .tccq_check_s7(context, TccqOpContext, "TccqOpContext", "context")

  names <- unique(vapply(calls, function(call) call@name, character(1)))
  unsupported <- character()
  for (name in names) {
    call <- calls[[match(name, vapply(calls, function(x) x@name, character(1)))]]
    if (!tccq_registry_supports(registry, call, context)) {
      unsupported <- c(unsupported, name)
    }
  }
  sort(unsupported)
}

#' Query whether any registry implementation supports a call
#'
#' @param registry Operation registry.
#' @param call Observed R call.
#' @param context Operation query context.
#' @export
tccq_registry_supports <- function(registry, call, context = tccq_op_context()) {
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  .tccq_check_s7(call, TccqCall, "TccqCall", "call")
  .tccq_check_s7(context, TccqOpContext, "TccqOpContext", "context")

  for (impl in registry@implementations) {
    s7contract::assert_trait(impl, TccqOpImplementation, arg = "impl")
    ok <- with(TccqOpImplementation, tccq_op_supports(impl, call, context))
    if (isTRUE(ok)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Return the name of an R call
#'
#' @param call R call.
#' @export
tccq_call_name <- function(call) {
  if (!is.call(call)) {
    tccq_abort(
      "schema.not_call",
      "`call` must be an R call.",
      phase = "schema",
      path = "call",
      data = list(type = typeof(call))
    )
  }
  head <- call[[1L]]
  if (is.symbol(head)) {
    return(as.character(head))
  }
  deparse1(head)
}

.tccq_op_impl_supports <- function(impl, call, context) {
  if (!identical(impl@op, call@name)) {
    return(FALSE)
  }
  if (!identical(context@target, "any") && !identical(impl@target, context@target)) {
    return(FALSE)
  }
  if (
    !identical(context@region_kind, "any") &&
      !identical(impl@region_kind, "any") &&
      !identical(impl@region_kind, context@region_kind)
  ) {
    return(FALSE)
  }
  if (
    !identical(context@memory_space, "any") &&
      !identical(impl@memory_space, "any") &&
      !identical(impl@memory_space, context@memory_space)
  ) {
    return(FALSE)
  }
  if (isTRUE(impl@uses_rapi) && !isTRUE(context@allow_rapi)) {
    return(FALSE)
  }
  if (isTRUE(impl@boundary) && !isTRUE(context@allow_boundary)) {
    return(FALSE)
  }
  isTRUE(impl@supports(call, context))
}

.tccq_check_region_query_kind <- function(kind, arg) {
  .tccq_check_character_scalar(kind, arg)
  if (!kind %in% c("any", TCCQ_REGION_KINDS)) {
    tccq_abort(
      "schema.invalid_region_query_kind",
      sprintf("`%s` is not a supported region query kind.", arg),
      phase = "schema",
      path = arg,
      data = list(kind = kind, supported = c("any", TCCQ_REGION_KINDS))
    )
  }
}

.tccq_check_memory_space_query <- function(memory_space, arg) {
  .tccq_check_character_scalar(memory_space, arg)
  if (!memory_space %in% c("any", TCCQ_MEMORY_SPACES)) {
    tccq_abort(
      "schema.invalid_memory_space_query",
      sprintf("`%s` is not a supported memory-space query.", arg),
      phase = "schema",
      path = arg,
      data = list(memory_space = memory_space, supported = c("any", TCCQ_MEMORY_SPACES))
    )
  }
}
