TCCQ_ANY_OP <- "<any>"

TCCQ_OPERATOR_CALL_NAMES <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "%*%", "%o%", "%x%", "%in%", "%||%",
  ":", ">", ">=", "<", "<=", "==", "!=", "!", "&", "&&", "|", "||", "~",
  "<-", "<<-", "->", "->>", "="
)

TCCQ_OPS_GROUP_CALL_NAMES <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "&", "|", "!", "==", "!=", "<",
  "<=", ">=", ">"
)

TCCQ_MATH_GROUP_CALL_NAMES <- c(
  "abs", "sign", "sqrt", "floor", "ceiling", "trunc", "round", "signif",
  "exp", "log", "expm1", "log1p", "cos", "sin", "tan", "cospi", "sinpi",
  "tanpi", "acos", "asin", "atan", "cosh", "sinh", "tanh", "acosh",
  "asinh", "atanh", "lgamma", "gamma", "digamma", "trigamma", "cumsum",
  "cumprod", "cummax", "cummin"
)

TCCQ_SUMMARY_GROUP_CALL_NAMES <- c("all", "any", "sum", "prod", "min", "max", "range")

TCCQ_OP_RENDER_LANGUAGES <- c("c", "fortran")

TCCQ_S3_PRIMITIVE_GENERIC_NAMES <- get0(
  ".S3PrimitiveGenerics",
  envir = baseenv(),
  inherits = FALSE,
  ifnotfound = character()
)

#' Operation support query context
#'
#' @param phase Compiler phase issuing the query.
#' @param target Requested implementation target, or `any`.
#' @param region_kind Requested execution region kind, or `any`.
#' @param memory_space Requested memory space, or `any`.
#' @param allow_rapi Whether implementations touching the R C API are allowed.
#' @param allow_boundary Whether explicit boundary implementations are allowed.
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
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@phase) != 1L || is.na(self@phase) || !nzchar(self@phase)) {
      problems <- c(problems, "@phase must be a single non-empty string")
    }
    if (length(self@target) != 1L || is.na(self@target) || !nzchar(self@target)) {
      problems <- c(problems, "@target must be a single non-empty string")
    }
    has_supported_region_query <- length(self@region_kind) == 1L &&
      !is.na(self@region_kind) &&
      self@region_kind %in% c("any", TCCQ_REGION_KINDS)
    if (!has_supported_region_query) {
      problems <- c(problems, "@region_kind must be `any` or a supported region kind")
    }
    has_supported_memory_query <- length(self@memory_space) == 1L &&
      !is.na(self@memory_space) &&
      self@memory_space %in% c("any", TCCQ_MEMORY_SPACES)
    if (!has_supported_memory_query) {
      problems <- c(problems, "@memory_space must be `any` or a supported memory space")
    }
    if (length(self@allow_rapi) != 1L || is.na(self@allow_rapi)) {
      problems <- c(problems, "@allow_rapi must be a single TRUE/FALSE value")
    }
    if (length(self@allow_boundary) != 1L || is.na(self@allow_boundary)) {
      problems <- c(problems, "@allow_boundary must be a single TRUE/FALSE value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Operation source-rendering context
#'
#' @param language Target source language.
#' @param backend_id Backend requesting rendering.
#' @param attrs Structured rendering metadata.
#' @export
TccqOpRenderContext <- S7::new_class(
  "TccqOpRenderContext",
  package = "tccquickr",
  properties = list(
    language = S7::class_character,
    backend_id = S7::class_character,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@language) != 1L ||
        is.na(self@language) ||
        !self@language %in% TCCQ_OP_RENDER_LANGUAGES
    ) {
      problems <- c(problems, "@language must be one supported render language")
    }
    if (length(self@backend_id) != 1L || is.na(self@backend_id) || !nzchar(self@backend_id)) {
      problems <- c(problems, "@backend_id must be a single non-empty string")
    }
    if (length(problems) > 0L) problems
  }
)

#' Operation domain policy
#'
#' A domain policy describes how input shapes determine an operation result
#' shape before target source generation. It is the typed home for rules such
#' as scalar broadcast, common elementwise domains, and scalar reducer results.
#'
#' @param name Human-readable domain policy name.
#' @param result_shape Function from input `TccqType` list to result
#'   `TccqShape`.
#' @param attrs Structured domain-policy metadata.
#' @export
TccqDomainPolicy <- S7::new_class(
  "TccqDomainPolicy",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    result_shape = S7::class_function,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (!is.function(self@result_shape)) {
      problems <- c(problems, "@result_shape must be a function")
    }
    if (length(problems) > 0L) problems
  }
)

#' Operation signature metadata
#'
#' A signature is the shared operation contract for argument count and result
#' typing. It may also carry a domain policy for result shape. Elementwise,
#' reduction, and future operation families should carry a signature instead of
#' each inventing their own arity, shape, and type checks.
#'
#' @param name Human-readable operation signature name.
#' @param arity Accepted argument counts.
#' @param result_type Function from input `TccqType` list to result `TccqType`.
#' @param domain_policy Optional result-shape policy.
#' @param attrs Structured signature metadata.
#' @export
TccqOpSignature <- S7::new_class(
  "TccqOpSignature",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    arity = S7::class_integer,
    result_type = S7::class_function,
    domain_policy = S7::new_union(NULL, TccqDomainPolicy),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (length(self@arity) == 0L || anyNA(self@arity) || any(self@arity <= 0L)) {
      problems <- c(problems, "@arity must contain positive integer arities")
    }
    if (!is.function(self@result_type)) {
      problems <- c(problems, "@result_type must be a function")
    }
    if (length(problems) > 0L) problems
  }
)

#' Elementwise implementation metadata
#'
#' An elementwise spec is attached to an operation implementation when calls to
#' that implementation can lower to elementwise values over the compiler domain
#' model. It carries an operation signature so lowering does not recognize
#' elementwise operations by spelling.
#'
#' @param name Human-readable elementwise operation name.
#' @param signature Shared operation signature.
#' @param attrs Structured elementwise metadata.
#' @export
TccqElementwiseSpec <- S7::new_class(
  "TccqElementwiseSpec",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    signature = TccqOpSignature,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (length(problems) > 0L) problems
  }
)

#' Reduction implementation metadata
#'
#' A reduction spec is attached to an operation implementation when calls to
#' that implementation can lower to a fold-like region. The lowerer and source
#' printers consume this object instead of recognizing reducer names directly.
#'
#' @param name Human-readable reduction name.
#' @param signature Shared operation signature.
#' @param identity Function from result `TccqType` to identity `TccqLiteral`.
#' @param combine Function from accumulator/source strings and render context
#'   to a target-source combine expression.
#' @param associative Whether the reducer is associative under its declared
#'   semantics.
#' @param commutative Whether the reducer is commutative under its declared
#'   semantics.
#' @param attrs Structured reduction metadata.
#' @export
TccqReductionSpec <- S7::new_class(
  "TccqReductionSpec",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    signature = TccqOpSignature,
    identity = S7::class_function,
    combine = S7::class_function,
    associative = S7::class_logical,
    commutative = S7::class_logical,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (!is.function(self@identity)) {
      problems <- c(problems, "@identity must be a function")
    }
    if (!is.function(self@combine)) {
      problems <- c(problems, "@combine must be a function")
    }
    if (length(self@associative) != 1L || is.na(self@associative)) {
      problems <- c(problems, "@associative must be a single TRUE/FALSE value")
    }
    if (length(self@commutative) != 1L || is.na(self@commutative)) {
      problems <- c(problems, "@commutative must be a single TRUE/FALSE value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Operation implementation descriptor
#'
#' @param op Operation or function name.
#' @param target Implementation target, such as `r_language`, `r_api`, `pure_c`,
#'   `fortran`, `mojo`, or `cuda`.
#' @param region_kind Region kind the implementation can run in, or `any`.
#' @param memory_space Memory space the implementation expects, or `any`.
#' @param uses_rapi Whether the implementation touches the R C API.
#' @param boundary Whether the implementation crosses a boundary.
#' @param pure Whether the implementation is semantically pure.
#' @param effect Effect summary for calls handled by this implementation.
#' @param supports Predicate receiving a `TccqCall` and `TccqOpContext`.
#' @param render Optional source renderer receiving operand strings and a
#'   `TccqOpRenderContext`.
#' @param elementwise Optional elementwise metadata.
#' @param reduction Optional reduction metadata.
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
    effect = TccqEffect,
    supports = S7::class_function,
    render = S7::new_union(NULL, S7::class_function),
    elementwise = S7::new_union(NULL, TccqElementwiseSpec),
    reduction = S7::new_union(NULL, TccqReductionSpec)
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@op) != 1L || is.na(self@op) || !nzchar(self@op)) {
      problems <- c(problems, "@op must be a single non-empty string")
    }
    if (length(self@target) != 1L || is.na(self@target) || !nzchar(self@target)) {
      problems <- c(problems, "@target must be a single non-empty string")
    }
    has_supported_region_query <- length(self@region_kind) == 1L &&
      !is.na(self@region_kind) &&
      self@region_kind %in% c("any", TCCQ_REGION_KINDS)
    if (!has_supported_region_query) {
      problems <- c(problems, "@region_kind must be `any` or a supported region kind")
    }
    has_supported_memory_query <- length(self@memory_space) == 1L &&
      !is.na(self@memory_space) &&
      self@memory_space %in% c("any", TCCQ_MEMORY_SPACES)
    if (!has_supported_memory_query) {
      problems <- c(problems, "@memory_space must be `any` or a supported memory space")
    }
    if (length(self@uses_rapi) != 1L || is.na(self@uses_rapi)) {
      problems <- c(problems, "@uses_rapi must be a single TRUE/FALSE value")
    }
    if (length(self@boundary) != 1L || is.na(self@boundary)) {
      problems <- c(problems, "@boundary must be a single TRUE/FALSE value")
    }
    if (length(self@pure) != 1L || is.na(self@pure)) {
      problems <- c(problems, "@pure must be a single TRUE/FALSE value")
    }
    if (!is.function(self@supports)) {
      problems <- c(problems, "@supports must be a predicate function")
    }
    if (length(problems) > 0L) problems
  }
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
  ),
  validator = function(self) {
    problems <- character()
    implementations_are_tccq_op_impls <- vapply(
      self@implementations,
      S7::S7_inherits,
      logical(1),
      class = TccqOpImpl
    )
    if (!all(implementations_are_tccq_op_impls)) {
      problems <- c(problems, "@implementations must contain only <TccqOpImpl> values")
    }
    if (length(problems) > 0L) problems
  }
)

#' Resolved operation implementation
#'
#' A resolved operation records the implementation selected for one observed
#' call in one operation context. Lowering and middle-end passes should carry
#' this object rather than re-checking support from raw operation names.
#'
#' @param call Observed R call.
#' @param implementation Selected operation implementation.
#' @param target Selected implementation target.
#' @param region_kind Region kind supplied by the implementation.
#' @param memory_space Memory space supplied by the implementation.
#' @param uses_rapi Whether the selected implementation touches the R C API.
#' @param boundary Whether the selected implementation crosses a boundary.
#' @param pure Whether the selected implementation is semantically pure.
#' @param effect Effect summary supplied by the implementation.
#' @param elementwise Optional elementwise metadata supplied by the
#'   implementation.
#' @param reduction Optional reduction metadata supplied by the implementation.
#' @param attrs Structured resolution metadata.
#' @export
TccqResolvedOp <- S7::new_class(
  "TccqResolvedOp",
  package = "tccquickr",
  properties = list(
    call = TccqCall,
    implementation = TccqOpImpl,
    target = S7::class_character,
    region_kind = S7::class_character,
    memory_space = S7::class_character,
    uses_rapi = S7::class_logical,
    boundary = S7::class_logical,
    pure = S7::class_logical,
    effect = TccqEffect,
    elementwise = S7::new_union(NULL, TccqElementwiseSpec),
    reduction = S7::new_union(NULL, TccqReductionSpec),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@target) != 1L || is.na(self@target) || !nzchar(self@target)) {
      problems <- c(problems, "@target must be a single non-empty string")
    }
    if (
      length(self@region_kind) != 1L ||
        is.na(self@region_kind) ||
        !self@region_kind %in% c("any", TCCQ_REGION_KINDS)
    ) {
      problems <- c(problems, "@region_kind must be any or one supported region kind")
    }
    if (
      length(self@memory_space) != 1L ||
        is.na(self@memory_space) ||
        !self@memory_space %in% c("any", TCCQ_MEMORY_SPACES)
    ) {
      problems <- c(problems, "@memory_space must be any or one supported memory space")
    }
    logical_fields <- list(
      uses_rapi = self@uses_rapi,
      boundary = self@boundary,
      pure = self@pure
    )
    for (field_name in names(logical_fields)) {
      field_value <- logical_fields[[field_name]]
      if (length(field_value) != 1L || is.na(field_value)) {
        problems <- c(problems, sprintf("@%s must be a single TRUE/FALSE value", field_name))
      }
    }
    if (length(problems) > 0L) problems
  }
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

#' Render an operation implementation for source output
#'
#' @param impl Operation implementation.
#' @param operands Rendered operand strings.
#' @param context Rendering context.
#' @export
tccq_op_render <- S7::new_generic(
  "tccq_op_render",
  dispatch_args = "impl",
  function(impl, operands, context) S7::S7_dispatch()
)

S7::method(tccq_op_render, TccqOpImpl) <- function(impl, operands, context) {
  if (!is.character(operands) || anyNA(operands) || any(!nzchar(operands))) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_render_operands",
      "Operation render operands must be non-empty strings.",
      phase = "ops",
      path = "op.render.operands",
      data = list(op = impl@op, operands = operands)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  .tccq_check_s7(context, TccqOpRenderContext, "TccqOpRenderContext", "context")
  if (is.null(impl@render)) {
    diagnostic <- tccq_diagnostic(
      "ops.unrenderable_operation",
      "Operation implementation has no source renderer.",
      phase = "ops",
      path = "op.render",
      data = list(
        op = impl@op,
        target = impl@target,
        language = context@language,
        backend = context@backend_id
      )
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  rendered_operation <- tryCatch(
    impl@render(operands, context),
    error = identity
  )
  if (inherits(rendered_operation, "error")) {
    diagnostic <- tccq_diagnostic(
      "ops.render_failed",
      conditionMessage(rendered_operation),
      phase = "ops",
      path = "op.render",
      data = list(
        op = impl@op,
        target = impl@target,
        language = context@language,
        backend = context@backend_id
      )
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  if (
    !is.character(rendered_operation) ||
      length(rendered_operation) != 1L ||
      is.na(rendered_operation) ||
      !nzchar(rendered_operation)
  ) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_render_result",
      "Operation renderer must return one non-empty source string.",
      phase = "ops",
      path = "op.render",
      data = list(
        op = impl@op,
        target = impl@target,
        language = context@language,
        backend = context@backend_id
      )
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  tccq_result(success = TRUE, value = rendered_operation)
}

#' Construct operation domain policy metadata
#'
#' @param name Human-readable domain policy name.
#' @param result_shape Function from input `TccqType` list to result
#'   `TccqShape`.
#' @param attrs Structured domain-policy metadata.
#' @export
tccq_domain_policy <- function(
  name,
  result_shape,
  attrs = list()
) {
  .tccq_check_character_scalar(name, "name")
  if (!is.function(result_shape)) {
    tccq_abort(
      "schema.invalid_domain_policy_result_shape",
      "`result_shape` must be a function.",
      phase = "schema",
      path = "domain_policy.result_shape"
    )
  }
  .tccq_check_list(attrs, "attrs")

  TccqDomainPolicy(
    name = name,
    result_shape = result_shape,
    attrs = attrs
  )
}

#' Return an operation domain-policy result shape
#'
#' @param policy Domain policy.
#' @param input_types List of input `TccqType` values.
#' @export
tccq_domain_policy_result_shape <- S7::new_generic(
  "tccq_domain_policy_result_shape",
  dispatch_args = "policy",
  function(policy, input_types) S7::S7_dispatch()
)

S7::method(tccq_domain_policy_result_shape, TccqDomainPolicy) <- function(policy, input_types) {
  .tccq_check_list_of(input_types, TccqType, "TccqType", "input_types")
  result_shape <- tryCatch(
    policy@result_shape(input_types),
    tccq_error = identity,
    error = identity
  )
  if (inherits(result_shape, "tccq_error")) {
    return(tccq_result(success = FALSE, diagnostics = list(tccq_condition_diagnostic(result_shape))))
  }
  if (inherits(result_shape, "error")) {
    diagnostic <- tccq_diagnostic(
      "ops.domain_policy_result_shape_failed",
      conditionMessage(result_shape),
      phase = "ops",
      path = "domain_policy.result_shape",
      data = list(policy = policy@name)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  if (!S7::S7_inherits(result_shape, TccqShape)) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_domain_policy_result_shape",
      "Domain policy result-shape functions must return a <TccqShape>.",
      phase = "ops",
      path = "domain_policy.result_shape",
      data = list(policy = policy@name, type = class(result_shape))
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  tccq_result(success = TRUE, value = result_shape)
}

#' Construct operation signature metadata
#'
#' @param name Human-readable operation signature name.
#' @param arity Accepted argument counts.
#' @param result_type Function from input `TccqType` list to result `TccqType`.
#' @param attrs Structured signature metadata.
#' @param domain_policy Optional result-shape policy.
#' @export
tccq_op_signature <- function(
  name,
  arity,
  result_type,
  attrs = list(),
  domain_policy = NULL
) {
  .tccq_check_character_scalar(name, "name")
  if (
    !is.numeric(arity) ||
      length(arity) == 0L ||
      anyNA(arity) ||
      any(arity <= 0L) ||
      any(arity != as.integer(arity))
  ) {
    tccq_abort(
      "schema.invalid_op_signature_arity",
      "`arity` must contain positive integer arities.",
      phase = "schema",
      path = "op_signature.arity",
      data = list(arity = arity)
    )
  }
  arity <- unique(as.integer(arity))
  if (!is.function(result_type)) {
    tccq_abort(
      "schema.invalid_op_signature_result_type",
      "`result_type` must be a function.",
      phase = "schema",
      path = "op_signature.result_type"
    )
  }
  .tccq_check_optional_s7(domain_policy, TccqDomainPolicy, "TccqDomainPolicy", "domain_policy")
  .tccq_check_list(attrs, "attrs")

  TccqOpSignature(
    name = name,
    arity = arity,
    result_type = result_type,
    domain_policy = domain_policy,
    attrs = attrs
  )
}

#' Return an operation signature result type
#'
#' @param signature Operation signature.
#' @param input_types List of input `TccqType` values.
#' @export
tccq_op_signature_result_type <- S7::new_generic(
  "tccq_op_signature_result_type",
  dispatch_args = "signature",
  function(signature, input_types) S7::S7_dispatch()
)

S7::method(tccq_op_signature_result_type, TccqOpSignature) <- function(signature, input_types) {
  result_type_accepts_shape <- function(result_type) {
    parameters <- formals(result_type)
    if (is.null(parameters)) {
      return(FALSE)
    }
    "..." %in% names(parameters) || length(parameters) >= 2L
  }

  .tccq_check_list_of(input_types, TccqType, "TccqType", "input_types")
  if (!(length(input_types) %in% signature@arity)) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_op_signature_arity",
      "Operation signature arity is not supported by this implementation.",
      phase = "ops",
      path = "op_signature.arity",
      data = list(
        operation = signature@name,
        arity = length(input_types),
        supported = signature@arity
      )
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  result_shape <- NULL
  if (!is.null(signature@domain_policy)) {
    result_shape_result <- tccq_domain_policy_result_shape(signature@domain_policy, input_types)
    if (!result_shape_result@success) {
      return(tccq_result(success = FALSE, diagnostics = result_shape_result@diagnostics))
    }
    result_shape <- result_shape_result@value
  }

  result_type <- tryCatch(
    if (result_type_accepts_shape(signature@result_type)) {
      signature@result_type(input_types, result_shape)
    } else {
      signature@result_type(input_types)
    },
    tccq_error = identity,
    error = identity
  )
  if (inherits(result_type, "tccq_error")) {
    return(tccq_result(success = FALSE, diagnostics = list(tccq_condition_diagnostic(result_type))))
  }
  if (inherits(result_type, "error")) {
    diagnostic <- tccq_diagnostic(
      "ops.op_signature_result_type_failed",
      conditionMessage(result_type),
      phase = "ops",
      path = "op_signature.result_type",
      data = list(operation = signature@name)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  if (!S7::S7_inherits(result_type, TccqType)) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_op_signature_result_type",
      "Operation signature result-type functions must return a <TccqType>.",
      phase = "ops",
      path = "op_signature.result_type",
      data = list(operation = signature@name, type = class(result_type))
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  tccq_result(success = TRUE, value = result_type)
}

#' Construct elementwise implementation metadata
#'
#' @param name Human-readable elementwise operation name.
#' @param arity Accepted argument counts.
#' @param result_type Function from input `TccqType` list to result `TccqType`.
#' @param attrs Structured elementwise metadata.
#' @param domain_policy Optional result-shape policy.
#' @export
tccq_elementwise_spec <- function(
  name,
  arity,
  result_type,
  attrs = list(),
  domain_policy = NULL
) {
  .tccq_check_character_scalar(name, "name")
  if (!is.function(result_type)) {
    tccq_abort(
      "schema.invalid_elementwise_result_type",
      "`result_type` must be a function.",
      phase = "schema",
      path = "elementwise.result_type"
    )
  }
  .tccq_check_list(attrs, "attrs")
  .tccq_check_optional_s7(domain_policy, TccqDomainPolicy, "TccqDomainPolicy", "domain_policy")

  TccqElementwiseSpec(
    name = name,
    signature = tccq_op_signature(
      name,
      arity,
      result_type,
      domain_policy = domain_policy
    ),
    attrs = attrs
  )
}

#' Return an elementwise operation result type
#'
#' @param spec Elementwise metadata.
#' @param input_types List of input `TccqType` values.
#' @export
tccq_elementwise_result_type <- S7::new_generic(
  "tccq_elementwise_result_type",
  dispatch_args = "spec",
  function(spec, input_types) S7::S7_dispatch()
)

S7::method(tccq_elementwise_result_type, TccqElementwiseSpec) <- function(spec, input_types) {
  tccq_op_signature_result_type(spec@signature, input_types)
}

#' Construct reduction implementation metadata
#'
#' @param name Human-readable reduction name.
#' @param identity Function from result `TccqType` to identity `TccqLiteral`.
#' @param combine Function from accumulator/source strings and render context
#'   to a target-source combine expression.
#' @param associative Whether the reducer is associative.
#' @param commutative Whether the reducer is commutative.
#' @param attrs Structured reduction metadata.
#' @param signature Shared operation signature. By default, reductions accept
#'   one input and return a scalar of the input base type.
#' @export
tccq_reduction_spec <- function(
  name,
  identity,
  combine,
  associative = TRUE,
  commutative = TRUE,
  attrs = list(),
  signature = NULL
) {
  .tccq_check_character_scalar(name, "name")
  if (!is.function(identity)) {
    tccq_abort(
      "schema.invalid_reduction_identity",
      "`identity` must be a function.",
      phase = "schema",
      path = "reduction.identity"
    )
  }
  if (!is.function(combine)) {
    tccq_abort(
      "schema.invalid_reduction_combine",
      "`combine` must be a function.",
      phase = "schema",
      path = "reduction.combine"
    )
  }
  .tccq_check_logical_scalar(associative, "associative")
  .tccq_check_logical_scalar(commutative, "commutative")
  .tccq_check_list(attrs, "attrs")
  if (is.null(signature)) {
    signature <- tccq_op_signature(
      name,
      1L,
      result_type = function(input_types, result_shape) {
        tccq_type(input_types[[1L]]@base, result_shape)
      },
      domain_policy = tccq_domain_policy(
        sprintf("%s_scalar_result", name),
        result_shape = function(input_types) {
          tccq_shape()
        }
      )
    )
  } else {
    .tccq_check_s7(signature, TccqOpSignature, "TccqOpSignature", "signature")
  }

  TccqReductionSpec(
    name = name,
    signature = signature,
    identity = identity,
    combine = combine,
    associative = associative,
    commutative = commutative,
    attrs = attrs
  )
}

#' Return a reducer identity literal
#'
#' @param spec Reduction metadata.
#' @param type Result type for the reduction.
#' @export
tccq_reduction_identity <- S7::new_generic(
  "tccq_reduction_identity",
  dispatch_args = "spec",
  function(spec, type) S7::S7_dispatch()
)

S7::method(tccq_reduction_identity, TccqReductionSpec) <- function(spec, type) {
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  identity <- tryCatch(
    spec@identity(type),
    tccq_error = identity,
    error = identity
  )
  if (inherits(identity, "tccq_error")) {
    return(tccq_result(success = FALSE, diagnostics = list(tccq_condition_diagnostic(identity))))
  }
  if (inherits(identity, "error")) {
    diagnostic <- tccq_diagnostic(
      "ops.reduction_identity_failed",
      conditionMessage(identity),
      phase = "ops",
      path = "reduction.identity",
      data = list(reducer = spec@name)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  if (!S7::S7_inherits(identity, TccqLiteral)) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_reduction_identity",
      "Reduction identity functions must return a <TccqLiteral>.",
      phase = "ops",
      path = "reduction.identity",
      data = list(reducer = spec@name, type = class(identity))
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  tccq_result(success = TRUE, value = identity)
}

#' Render a reduction combine expression
#'
#' @param spec Reduction metadata.
#' @param accumulator Accumulator source expression.
#' @param value Current element source expression.
#' @param context Operation rendering context.
#' @export
tccq_reduction_combine <- S7::new_generic(
  "tccq_reduction_combine",
  dispatch_args = "spec",
  function(spec, accumulator, value, context) S7::S7_dispatch()
)

S7::method(tccq_reduction_combine, TccqReductionSpec) <- function(spec, accumulator, value, context) {
  if (!is.character(accumulator) || length(accumulator) != 1L || is.na(accumulator) || !nzchar(accumulator)) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_reduction_accumulator",
      "Reduction accumulators must be one non-empty source string.",
      phase = "ops",
      path = "reduction.accumulator",
      data = list(reducer = spec@name, accumulator = accumulator)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_reduction_value",
      "Reduction values must be one non-empty source string.",
      phase = "ops",
      path = "reduction.value",
      data = list(reducer = spec@name, value = value)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  .tccq_check_s7(context, TccqOpRenderContext, "TccqOpRenderContext", "context")
  combined <- tryCatch(
    spec@combine(accumulator, value, context),
    error = identity
  )
  if (inherits(combined, "error")) {
    diagnostic <- tccq_diagnostic(
      "ops.reduction_combine_failed",
      conditionMessage(combined),
      phase = "ops",
      path = "reduction.combine",
      data = list(reducer = spec@name, language = context@language)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  if (!is.character(combined) || length(combined) != 1L || is.na(combined) || !nzchar(combined)) {
    diagnostic <- tccq_diagnostic(
      "ops.invalid_reduction_combine",
      "Reduction combine functions must return one non-empty source string.",
      phase = "ops",
      path = "reduction.combine",
      data = list(reducer = spec@name, language = context@language)
    )
    return(tccq_result(success = FALSE, diagnostics = list(diagnostic)))
  }
  tccq_result(success = TRUE, value = combined)
}

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
        if (!identical(impl@op, TCCQ_ANY_OP) && !identical(impl@op, call@name)) {
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
        if (
          S7::S7_inherits(impl@elementwise, TccqElementwiseSpec) &&
            !is.na(call@arity) &&
            !(call@arity %in% impl@elementwise@signature@arity)
        ) {
          return(FALSE)
        }
        if (
          S7::S7_inherits(impl@reduction, TccqReductionSpec) &&
            !is.na(call@arity) &&
            !(call@arity %in% impl@reduction@signature@arity)
        ) {
          return(FALSE)
        }
        isTRUE(impl@supports(call, context))
      }
    ),
    replace = TRUE
  )
  invisible(TRUE)
}

#' Construct an observed call
#'
#' @param id Stable call id.
#' @param name Call name.
#' @param expr Original R call expression.
#' @param origin Source of the call observation.
#' @param kind Structural call kind.
#' @param arity Number of supplied arguments, or `NA_integer_`.
#' @param argument_names Supplied argument tags.
#' @param attrs Structured call attributes.
#' @export
tccq_call <- function(
  name,
  expr = NULL,
  origin = "ast",
  id = "",
  kind = NULL,
  arity = NULL,
  argument_names = NULL,
  attrs = list()
) {
  infer_kind <- function(call_name) {
    if (identical(call_name, "{")) {
      return("block")
    }
    if (identical(call_name, "(")) {
      return("grouping")
    }
    if (call_name %in% c("if", "for", "while", "repeat", "break", "next", "switch")) {
      return("control")
    }
    if (identical(call_name, "function")) {
      return("function_definition")
    }
    if (call_name %in% c("[", "[[", "$", "@")) {
      return("index")
    }
    if (call_name %in% c("<-", "<<-", "->", "->>", "=")) {
      return("assignment")
    }
    if (
      grepl("<-$", call_name) &&
        !call_name %in% c("<-", "<<-", "->", "->>")
    ) {
      return("replacement")
    }
    if (call_name %in% TCCQ_OPERATOR_CALL_NAMES) {
      return("operator")
    }
    "call"
  }

  argument_names_from_expr <- function(call_expr) {
    if (!is.call(call_expr)) {
      return(character())
    }
    args <- as.list(call_expr)[-1L]
    names <- names(args)
    if (is.null(names)) {
      return(rep("", length(args)))
    }
    names[is.na(names)] <- ""
    names
  }

  .tccq_check_character_scalar(name, "name")
  .tccq_check_character_scalar(origin, "origin")
  .tccq_check_character_or_empty(id, "id")
  if (is.null(kind)) {
    kind <- infer_kind(name)
  }
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_CALL_KINDS) {
    tccq_abort(
      "schema.invalid_call_kind",
      "`kind` is not a supported call kind.",
      phase = "schema",
      path = "call.kind",
      data = list(kind = kind, supported = TCCQ_CALL_KINDS)
    )
  }
  if (is.null(arity)) {
    arity <- if (is.call(expr)) {
      as.integer(length(expr) - 1L)
    } else {
      NA_integer_
    }
  } else {
    arity <- .tccq_check_optional_nonnegative_integer(arity, "arity")
  }
  if (is.null(argument_names)) {
    argument_names <- argument_names_from_expr(expr)
  }
  if (!is.character(argument_names) || anyNA(argument_names)) {
    tccq_abort(
      "schema.invalid_call_argument_names",
      "`argument_names` must be a character vector.",
      phase = "schema",
      path = "call.argument_names",
      data = list(argument_names = argument_names)
    )
  }
  .tccq_check_list(attrs, "attrs")

  TccqCall(
    id = id,
    name = name,
    expr = expr,
    origin = origin,
    kind = kind,
    arity = arity,
    argument_names = argument_names,
    attrs = attrs
  )
}

#' Construct call evaluator facts
#'
#' @param call Observed call.
#' @param env Environment used to resolve ordinary function names.
#' @param evaluator_kind Optional evaluator kind override.
#' @param forcing_policy Optional forcing-policy override.
#' @param dispatch_kind Optional dispatch-kind override.
#' @param lexical_scope Optional lexical-scope override.
#' @param replacement Optional replacement flag override.
#' @param control Optional control flag override.
#' @param attrs Structured semantic attributes.
#' @export
tccq_call_semantics <- function(
  call,
  env = baseenv(),
  evaluator_kind = NULL,
  forcing_policy = NULL,
  dispatch_kind = NULL,
  lexical_scope = NULL,
  replacement = NULL,
  control = NULL,
  attrs = list()
) {
  .tccq_check_s7(call, TccqCall, "TccqCall", "call")
  if (!is.environment(env)) {
    tccq_abort(
      "schema.invalid_environment",
      "`env` must be an environment.",
      phase = "schema",
      path = "call_semantics.env",
      data = list(actual = typeof(env))
    )
  }

  function_from_env <- function(call_name) {
    if (call_name %in% c("declare", "type")) {
      return(NULL)
    }
    get0(call_name, envir = env, mode = "function", inherits = TRUE)
  }

  body_uses_call <- function(expr, call_name) {
    found <- FALSE
    walk <- function(node) {
      if (found || !is.call(node)) {
        return(NULL)
      }
      if (identical(tccq_call_name(node), call_name)) {
        found <<- TRUE
        return(NULL)
      }
      for (child in as.list(node)[-1L]) {
        walk(child)
      }
      NULL
    }
    walk(expr)
    found
  }

  evaluator_kind_from_function <- function(function_object) {
    if (call@name %in% c("declare", "type")) {
      return("compiler_directive")
    }
    if (is.function(function_object)) {
      kind <- typeof(function_object)
      if (kind %in% c("special", "builtin", "closure")) {
        return(kind)
      }
    }
    "unknown"
  }

  forcing_policy_from_evaluator <- function(inferred_evaluator_kind) {
    if (identical(call@kind, "replacement")) {
      return("replacement")
    }
    switch(
      inferred_evaluator_kind,
      compiler_directive = "compiler",
      special = "special",
      builtin = "eager",
      closure = "lazy",
      unknown = "unknown",
      "unknown"
    )
  }

  dispatch_kind_from_function <- function(function_object, inferred_evaluator_kind) {
    if (identical(call@kind, "replacement")) {
      return("replacement")
    }
    if (call@name %in% c("UseMethod", "NextMethod")) {
      return("s3")
    }
    if (call@name %in% c(
      TCCQ_OPS_GROUP_CALL_NAMES,
      TCCQ_MATH_GROUP_CALL_NAMES,
      TCCQ_SUMMARY_GROUP_CALL_NAMES
    )) {
      return("group_generic")
    }
    if (call@name %in% TCCQ_S3_PRIMITIVE_GENERIC_NAMES) {
      return("s3_primitive")
    }
    if (
      identical(inferred_evaluator_kind, "closure") &&
        body_uses_call(body(function_object), "UseMethod")
    ) {
      return("s3")
    }
    if (identical(inferred_evaluator_kind, "unknown")) {
      return("unknown")
    }
    "none"
  }

  function_object <- function_from_env(call@name)
  inferred_evaluator_kind <- evaluator_kind_from_function(function_object)
  facts <- list(
    evaluator_kind = inferred_evaluator_kind,
    forcing_policy = forcing_policy_from_evaluator(inferred_evaluator_kind),
    dispatch_kind = dispatch_kind_from_function(function_object, inferred_evaluator_kind),
    lexical_scope = identical(inferred_evaluator_kind, "closure") ||
      identical(call@kind, "function_definition"),
    replacement = identical(call@kind, "replacement"),
    control = identical(call@kind, "control"),
    attrs = list(
      resolved = !is.null(function_object),
      primitive = is.function(function_object) && is.primitive(function_object)
    )
  )
  evaluator_kind <- evaluator_kind %||% facts$evaluator_kind
  forcing_policy <- forcing_policy %||% facts$forcing_policy
  dispatch_kind <- dispatch_kind %||% facts$dispatch_kind
  lexical_scope <- lexical_scope %||% facts$lexical_scope
  replacement <- replacement %||% facts$replacement
  control <- control %||% facts$control

  .tccq_check_character_scalar(evaluator_kind, "evaluator_kind")
  .tccq_check_character_scalar(forcing_policy, "forcing_policy")
  .tccq_check_character_scalar(dispatch_kind, "dispatch_kind")
  if (!evaluator_kind %in% TCCQ_EVALUATOR_KINDS) {
    tccq_abort(
      "schema.invalid_evaluator_kind",
      "`evaluator_kind` is not supported.",
      phase = "schema",
      path = "call_semantics.evaluator_kind",
      data = list(value = evaluator_kind, supported = TCCQ_EVALUATOR_KINDS)
    )
  }
  if (!forcing_policy %in% TCCQ_FORCING_POLICIES) {
    tccq_abort(
      "schema.invalid_forcing_policy",
      "`forcing_policy` is not supported.",
      phase = "schema",
      path = "call_semantics.forcing_policy",
      data = list(value = forcing_policy, supported = TCCQ_FORCING_POLICIES)
    )
  }
  if (!dispatch_kind %in% TCCQ_DISPATCH_KINDS) {
    tccq_abort(
      "schema.invalid_dispatch_kind",
      "`dispatch_kind` is not supported.",
      phase = "schema",
      path = "call_semantics.dispatch_kind",
      data = list(value = dispatch_kind, supported = TCCQ_DISPATCH_KINDS)
    )
  }
  .tccq_check_logical_scalar(lexical_scope, "lexical_scope")
  .tccq_check_logical_scalar(replacement, "replacement")
  .tccq_check_logical_scalar(control, "control")
  .tccq_check_list(attrs, "attrs")

  TccqCallSemantics(
    call = call,
    evaluator_kind = evaluator_kind,
    forcing_policy = forcing_policy,
    dispatch_kind = dispatch_kind,
    lexical_scope = lexical_scope,
    replacement = replacement,
    control = control,
    attrs = c(facts$attrs, attrs)
  )
}

#' Collect evaluator facts for calls
#'
#' @param calls List of `TccqCall` objects.
#' @param env Environment used to resolve ordinary function names.
#' @export
tccq_collect_call_semantics <- function(calls, env = baseenv()) {
  .tccq_check_list_of(calls, TccqCall, "TccqCall", "calls")
  if (!is.environment(env)) {
    tccq_abort(
      "schema.invalid_environment",
      "`env` must be an environment.",
      phase = "schema",
      path = "call_semantics.env",
      data = list(actual = typeof(env))
    )
  }
  lapply(calls, tccq_call_semantics, env = env)
}

#' Construct a typed call index
#'
#' @param calls List of observed calls.
#' @param semantics Optional list of call evaluator facts. If omitted, inferred.
#' @param env Environment used when inferring missing semantics.
#' @param attrs Structured index attributes.
#' @export
tccq_call_index <- function(
  calls,
  semantics = NULL,
  env = baseenv(),
  attrs = list()
) {
  .tccq_check_list_of(calls, TccqCall, "TccqCall", "calls")

  call_with_id <- function(call, id) {
    tccq_call(
      call@name,
      expr = call@expr,
      origin = call@origin,
      id = id,
      kind = call@kind,
      arity = call@arity,
      argument_names = call@argument_names,
      attrs = call@attrs
    )
  }

  call_ids <- vapply(calls, function(call) call@id, character(1))
  if (any(!nzchar(call_ids)) || anyDuplicated(call_ids)) {
    calls <- lapply(seq_along(calls), function(i) {
      call_with_id(calls[[i]], sprintf("call_%04d", i))
    })
  }

  if (is.null(semantics)) {
    semantics <- tccq_collect_call_semantics(calls, env = env)
  }
  .tccq_check_list_of(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  call_ids <- vapply(calls, function(call) call@id, character(1))
  if (any(!nzchar(call_ids)) || anyDuplicated(call_ids)) {
    tccq_abort(
      "schema.invalid_call_index_ids",
      "Call index ids must be non-empty and unique.",
      phase = "schema",
      path = "call_index.calls",
      data = list(ids = call_ids)
    )
  }
  semantic_ids <- vapply(semantics, function(x) x@call@id, character(1))
  if (!identical(call_ids, semantic_ids)) {
    tccq_abort(
      "schema.call_index_mismatch",
      "Call semantics must align one-to-one with calls by id.",
      phase = "schema",
      path = "call_index.semantics",
      data = list(calls = call_ids, semantics = semantic_ids)
    )
  }
  .tccq_check_list(attrs, "attrs")

  TccqCallIndex(calls = calls, semantics = semantics, attrs = attrs)
}

#' Collect a typed call index from an R expression
#'
#' @param expr R expression to inspect.
#' @param global_calls Additional function names found by `codetools`.
#' @param env Environment used to resolve ordinary function names.
#' @param attrs Structured index attributes.
#' @export
tccq_collect_call_index <- function(
  expr,
  global_calls = character(),
  env = baseenv(),
  attrs = list()
) {
  tccq_call_index(
    tccq_collect_calls(expr, global_calls = global_calls),
    env = env,
    attrs = attrs
  )
}

#' Construct an operation support context
#'
#' @param phase Compiler phase issuing the query.
#' @param target Requested implementation target, or `any`.
#' @param region_kind Requested execution region kind, or `any`.
#' @param memory_space Requested memory space, or `any`.
#' @param allow_rapi Whether implementations touching the R C API are allowed.
#' @param allow_boundary Whether explicit boundary implementations are allowed.
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

#' Construct an operation source-rendering context
#'
#' @param language Target source language.
#' @param backend_id Backend requesting rendering.
#' @param attrs Structured rendering metadata.
#' @export
tccq_op_render_context <- function(language, backend_id, attrs = list()) {
  .tccq_check_character_scalar(language, "language")
  .tccq_check_character_scalar(backend_id, "backend_id")
  .tccq_check_list(attrs, "attrs")

  TccqOpRenderContext(
    language = language,
    backend_id = backend_id,
    attrs = attrs
  )
}

#' Construct an operation implementation
#'
#' @param op Operation or function name.
#' @param target Implementation target.
#' @param region_kind Region kind the implementation can run in, or `any`.
#' @param memory_space Memory space the implementation expects, or `any`.
#' @param uses_rapi Whether the implementation touches the R C API.
#' @param boundary Whether the implementation crosses a boundary.
#' @param pure Whether the implementation is semantically pure.
#' @param effect Effect summary for calls handled by this implementation.
#' @param supports Predicate receiving a `TccqCall` and `TccqOpContext`.
#' @param render Optional source renderer receiving operand strings and a
#'   `TccqOpRenderContext`.
#' @param elementwise Optional elementwise metadata.
#' @param reduction Optional reduction metadata.
#' @export
tccq_op_impl <- function(
  op,
  target,
  region_kind = "any",
  memory_space = "any",
  uses_rapi = FALSE,
  boundary = FALSE,
  pure = TRUE,
  effect = NULL,
  supports = function(call, context) TRUE,
  render = NULL,
  elementwise = NULL,
  reduction = NULL
) {
  .tccq_check_character_scalar(op, "op")
  .tccq_check_character_scalar(target, "target")
  .tccq_check_region_query_kind(region_kind, "region_kind")
  .tccq_check_memory_space_query(memory_space, "memory_space")
  .tccq_check_logical_scalar(uses_rapi, "uses_rapi")
  .tccq_check_logical_scalar(boundary, "boundary")
  .tccq_check_logical_scalar(pure, "pure")
  if (is.null(effect)) {
    effect <- tccq_effect(
      reads = TRUE,
      writes = !isTRUE(pure),
      allocates = isTRUE(uses_rapi) || isTRUE(boundary),
      boundary = boundary,
      may_error = isTRUE(uses_rapi) || isTRUE(boundary)
    )
  }
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  if (!is.function(supports)) {
    tccq_abort(
      "schema.invalid_op_supports",
      "`supports` must be a predicate function.",
      phase = "schema",
      path = "op.supports"
    )
  }
  if (!is.null(render) && !is.function(render)) {
    tccq_abort(
      "schema.invalid_op_renderer",
      "`render` must be NULL or a function.",
      phase = "schema",
      path = "op.render"
    )
  }
  .tccq_check_optional_s7(elementwise, TccqElementwiseSpec, "TccqElementwiseSpec", "elementwise")
  .tccq_check_optional_s7(reduction, TccqReductionSpec, "TccqReductionSpec", "reduction")

  TccqOpImpl(
    op = op,
    target = target,
    region_kind = region_kind,
    memory_space = memory_space,
    uses_rapi = uses_rapi,
    boundary = boundary,
    pure = pure,
    effect = effect,
    supports = supports,
    render = render,
    elementwise = elementwise,
    reduction = reduction
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

#' Construct a resolved operation
#'
#' @param call Observed R call.
#' @param implementation Selected operation implementation.
#' @param attrs Structured resolution metadata.
#' @export
tccq_resolved_op <- function(call, implementation, attrs = list()) {
  .tccq_check_s7(call, TccqCall, "TccqCall", "call")
  .tccq_check_s7(implementation, TccqOpImpl, "TccqOpImpl", "implementation")
  .tccq_check_list(attrs, "attrs")

  TccqResolvedOp(
    call = call,
    implementation = implementation,
    target = implementation@target,
    region_kind = implementation@region_kind,
    memory_space = implementation@memory_space,
    uses_rapi = implementation@uses_rapi,
    boundary = implementation@boundary,
    pure = implementation@pure,
    effect = implementation@effect,
    elementwise = implementation@elementwise,
    reduction = implementation@reduction,
    attrs = attrs
  )
}

#' Default operation registry for the reset frontend
#'
#' The default registry names the R language call forms that the frontend
#' recognizes structurally. It is not a source-syntax whitelist and it is not a
#' backend lowering promise.
#'
#' @export
tccq_default_op_registry <- function() {
  scalar_renderers <- list(
    "+" = function(operands, context) sprintf("(%s + %s)", operands[[1L]], operands[[2L]]),
    "-" = function(operands, context) {
      if (length(operands) == 1L) {
        return(sprintf("(-%s)", operands[[1L]]))
      }
      sprintf("(%s - %s)", operands[[1L]], operands[[2L]])
    },
    "*" = function(operands, context) sprintf("(%s * %s)", operands[[1L]], operands[[2L]]),
    "/" = function(operands, context) sprintf("(%s / %s)", operands[[1L]], operands[[2L]]),
    "^" = function(operands, context) {
      if (identical(context@language, "fortran")) {
        return(sprintf("(%s ** %s)", operands[[1L]], operands[[2L]]))
      }
      sprintf("pow(%s, %s)", operands[[1L]], operands[[2L]])
    },
    sqrt = function(operands, context) sprintf("sqrt(%s)", operands[[1L]]),
    exp = function(operands, context) sprintf("exp(%s)", operands[[1L]])
  )
  elementwise_domain_policy <- tccq_domain_policy(
    "elementwise_common_shape",
    result_shape = function(input_types) {
      shape_label <- function(shape) {
        labels <- vapply(shape@dims, function(dim) {
          if (identical(dim@kind, "constant")) {
            return(sprintf("constant:%d", dim@value))
          }
          if (identical(dim@kind, "symbol")) {
            return(sprintf("symbol:%s", dim@label))
          }
          "unknown"
        }, character(1))
        paste(labels, collapse = "/")
      }

      non_scalar_shapes <- lapply(
        Filter(function(type) type@shape@rank > 0L, input_types),
        function(type) type@shape
      )
      if (length(non_scalar_shapes) == 0L) {
        return(tccq_shape())
      }
      common_shape <- non_scalar_shapes[[1L]]
      compatible_shapes <- vapply(non_scalar_shapes, identical, logical(1), common_shape)
      if (!all(compatible_shapes)) {
        shape_labels <- unique(vapply(non_scalar_shapes, shape_label, character(1)))
        tccq_abort(
          "ops.incompatible_elementwise_shapes",
          "Vectorized expression inputs must have the same shape.",
          phase = "ops",
          path = "domain_policy.result_shape",
          data = list(shapes = shape_labels)
        )
      }
      common_shape
    }
  )
  numeric_elementwise_result_type <- function(force_double = FALSE) {
    function(input_types, result_shape) {
      unsupported_bases <- setdiff(
        unique(vapply(input_types, function(type) type@base, character(1))),
        c("integer", "double")
      )
      if (length(unsupported_bases) > 0L) {
        tccq_abort(
          "ops.unsupported_elementwise_type",
          "The default elementwise implementations support integer and double values.",
          phase = "ops",
          path = "elementwise.type",
          data = list(base = unsupported_bases)
        )
      }

      result_base <- if (
        isTRUE(force_double) ||
          any(vapply(input_types, function(type) identical(type@base, "double"), logical(1)))
      ) {
        "double"
      } else {
        "integer"
      }
      tccq_type(result_base, result_shape)
    }
  }
  elementwise_specs <- list(
    "+" = tccq_elementwise_spec(
      "+",
      2L,
      numeric_elementwise_result_type(),
      domain_policy = elementwise_domain_policy
    ),
    "-" = tccq_elementwise_spec(
      "-",
      c(1L, 2L),
      numeric_elementwise_result_type(),
      domain_policy = elementwise_domain_policy
    ),
    "*" = tccq_elementwise_spec(
      "*",
      2L,
      numeric_elementwise_result_type(),
      domain_policy = elementwise_domain_policy
    ),
    "/" = tccq_elementwise_spec(
      "/",
      2L,
      numeric_elementwise_result_type(force_double = TRUE),
      domain_policy = elementwise_domain_policy
    ),
    "^" = tccq_elementwise_spec(
      "^",
      2L,
      numeric_elementwise_result_type(force_double = TRUE),
      domain_policy = elementwise_domain_policy
    ),
    sqrt = tccq_elementwise_spec(
      "sqrt",
      1L,
      numeric_elementwise_result_type(force_double = TRUE),
      domain_policy = elementwise_domain_policy
    ),
    exp = tccq_elementwise_spec(
      "exp",
      1L,
      numeric_elementwise_result_type(force_double = TRUE),
      domain_policy = elementwise_domain_policy
    )
  )
  base_sum_reduction <- tccq_reduction_spec(
    "sum",
    identity = function(type) {
      if (!type@base %in% c("integer", "double")) {
        tccq_abort(
          "ops.unsupported_reduction_identity_type",
          "The default sum identity supports integer and double results.",
          phase = "ops",
          path = "reduction.identity",
          data = list(reducer = "sum", base = type@base)
        )
      }
      identity_value <- if (identical(type@base, "integer")) 0L else 0
      tccq_literal_finite(identity_value, type = type)
    },
    combine = function(accumulator, value, context) {
      sprintf("%s + %s", accumulator, value)
    }
  )
  language_ops <- c(
    "{", "(", "<-", "<<-", "->", "->>", "=",
    "if", "for", "while", "repeat", "break", "next", "switch", "function",
    "[", "[[", "$", "@", "[<-", "[[<-", "$<-", "@<-",
    "declare", "type", TCCQ_BASE_TYPES
  )
  tccq_op_registry(c(
    lapply(language_ops, function(op) {
      tccq_op_impl(op, target = "r_language", pure = FALSE)
    }),
    lapply(names(elementwise_specs), function(op) {
      tccq_op_impl(
        op,
        target = "pure_c",
        region_kind = "kernel",
        render = scalar_renderers[[op]],
        elementwise = elementwise_specs[[op]]
      )
    }),
    list(
      tccq_op_impl(
        "sum",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        reduction = base_sum_reduction
      )
    )
  ))
}

#' Opaque operation implementation
#'
#' In R, every call is an operation candidate. This descriptor represents calls
#' whose concrete implementation, purity, and effects have not been refined yet.
#' It is not R call evaluation; that is only one possible backend implementation
#' family.
#'
#' @export
tccq_opaque_op_impl <- function() {
  tccq_op_impl(
    op = TCCQ_ANY_OP,
    target = "opaque",
    region_kind = "any",
    memory_space = "any",
    uses_rapi = FALSE,
    boundary = FALSE,
    pure = FALSE,
    effect = tccq_effect(
      reads = TRUE,
      writes = TRUE,
      allocates = TRUE,
      boundary = FALSE,
      may_error = TRUE
    )
  )
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
  next_id <- 0L
  make_id <- function() {
    next_id <<- next_id + 1L
    sprintf("call_%04d", next_id)
  }
  walk <- function(node) {
    if (!is.call(node)) {
      return(NULL)
    }
    calls[[length(calls) + 1L]] <<- tccq_call(
      tccq_call_name(node),
      node,
      "ast",
      id = make_id()
    )
    for (child in as.list(node)[-1L]) {
      walk(child)
    }
    NULL
  }
  walk(expr)
  for (name in global_calls) {
    calls[[length(calls) + 1L]] <- tccq_call(name, origin = "codetools", id = make_id())
  }
  calls
}

#' Find calls without an implementation for a registry and context
#'
#' @param calls List of `TccqCall` objects.
#' @param registry Operation registry.
#' @param context Operation query context.
#' @export
tccq_unimplemented_calls <- function(
  calls,
  registry = tccq_default_op_registry(),
  context = tccq_op_context()
) {
  .tccq_check_list_of(calls, TccqCall, "TccqCall", "calls")
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  .tccq_check_s7(context, TccqOpContext, "TccqOpContext", "context")

  names <- unique(vapply(calls, function(call) call@name, character(1)))
  unimplemented <- character()
  for (name in names) {
    call <- calls[[match(name, vapply(calls, function(x) x@name, character(1)))]]
    if (!tccq_registry_supports(registry, call, context)) {
      unimplemented <- c(unimplemented, name)
    }
  }
  sort(unimplemented)
}

#' Query whether any registry implementation supports a call
#'
#' @param registry Operation registry.
#' @param call Observed R call.
#' @param context Operation query context.
#' @export
tccq_registry_supports <- function(registry, call, context = tccq_op_context()) {
  tccq_resolve_call(registry, call, context)@success
}

#' Resolve a call to one operation implementation
#'
#' @param registry Operation registry.
#' @param call Observed R call.
#' @param context Operation query context.
#' @export
tccq_resolve_call <- function(registry, call, context = tccq_op_context()) {
  .tccq_check_s7(registry, TccqOpRegistry, "TccqOpRegistry", "registry")
  .tccq_check_s7(call, TccqCall, "TccqCall", "call")
  .tccq_check_s7(context, TccqOpContext, "TccqOpContext", "context")

  for (implementation in registry@implementations) {
    s7contract::assert_trait(implementation, TccqOpImplementation, arg = "implementation")
    implementation_supports_call <- with(
      TccqOpImplementation,
      tccq_op_supports(implementation, call, context)
    )
    if (isTRUE(implementation_supports_call)) {
      return(tccq_result(
        success = TRUE,
        value = tccq_resolved_op(
          call,
          implementation,
          attrs = list(context = context)
        )
      ))
    }
  }
  diagnostic <- tccq_diagnostic(
    "ops.unresolved_call",
    sprintf("Call `%s` has no implementation in the current registry/context.", call@name),
    phase = "ops",
    path = "call",
    data = list(
      call = call@name,
      target = context@target,
      region_kind = context@region_kind,
      memory_space = context@memory_space,
      allow_rapi = context@allow_rapi,
      allow_boundary = context@allow_boundary
    )
  )
  tccq_result(success = FALSE, diagnostics = list(diagnostic))
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
  if (is.character(head) && length(head) == 1L && !is.na(head)) {
    return(head)
  }
  deparse1(head)
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
