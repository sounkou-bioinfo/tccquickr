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
TCCQ_LOWERED_OPERATION_FAMILIES <- c(
  "elementwise", "reduction", "contraction", "subscript"
)
TCCQ_REDUCTION_EMPTY_POLICIES <- c("identity", "error")

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

#' Operation arity contract
#'
#' Arity is a typed operation fact rather than an expanded vector of convenient
#' argument counts. Exact contracts enumerate accepted counts; interval
#' contracts own inclusive lower and optional upper bounds. This lets
#' rank-polymorphic operations such as `[` remain independent of an arbitrary
#' maximum array rank.
#'
#' @param counts Exact accepted argument counts.
#' @param minimum Inclusive lower argument-count bound, or `NULL` for an exact
#'   contract.
#' @param maximum Inclusive upper argument-count bound, or `NULL` for no upper
#'   bound.
#' @export
TccqArity <- S7::new_class(
  "TccqArity",
  package = "tccquickr",
  properties = list(
    counts = S7::class_integer,
    minimum = S7::new_union(NULL, S7::class_integer),
    maximum = S7::new_union(NULL, S7::class_integer)
  ),
  validator = function(self) {
    problems <- character()
    exact <- length(self@counts) > 0L
    interval <- !is.null(self@minimum)
    if (exact == interval) {
      problems <- c(problems, "arity must be either exact counts or one interval")
    }
    if (exact && (anyNA(self@counts) || any(self@counts <= 0L))) {
      problems <- c(problems, "@counts must contain positive integers")
    }
    if (exact && anyDuplicated(self@counts)) {
      problems <- c(problems, "@counts must not contain duplicates")
    }
    if (interval && (length(self@minimum) != 1L || is.na(self@minimum) || self@minimum <= 0L)) {
      problems <- c(problems, "@minimum must be one positive integer")
    }
    if (!is.null(self@maximum)) {
      if (
        !interval ||
          length(self@maximum) != 1L ||
          is.na(self@maximum) ||
          self@maximum < self@minimum
      ) {
        problems <- c(problems, "@maximum must be one integer no smaller than @minimum")
      }
    }
    if (exact && !is.null(self@maximum)) {
      problems <- c(problems, "exact arity must not carry interval bounds")
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
#' @param arity A [TccqArity] contract.
#' @param result_type Function from input `TccqType` list to result `TccqType`.
#' @param domain_policy Optional result-shape policy.
#' @param attrs Structured signature metadata.
#' @export
TccqOpSignature <- S7::new_class(
  "TccqOpSignature",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    arity = TccqArity,
    result_type = S7::class_function,
    domain_policy = S7::new_union(NULL, TccqDomainPolicy),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
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
#' that implementation can lower to a typed reduction state. Concrete
#' subclasses define that state and its transition; the lowerer and source
#' printers consume the shared protocol instead of recognizing reducer names.
#'
#' @param name Human-readable reduction name.
#' @param signature Shared operation signature.
#' @param associative Whether the reducer is associative under its declared
#'   semantics.
#' @param commutative Whether the reducer is commutative under its declared
#'   semantics.
#' @param empty_policy Behavior when no value contributes to the reduction.
#' @param attrs Structured reduction metadata.
#' @export
TccqReductionSpec <- S7::new_class(
  "TccqReductionSpec",
  package = "tccquickr",
  abstract = TRUE,
  properties = list(
    name = S7::class_character,
    signature = TccqOpSignature,
    associative = S7::class_logical,
    commutative = S7::class_logical,
    empty_policy = S7::class_character,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (length(self@associative) != 1L || is.na(self@associative)) {
      problems <- c(problems, "@associative must be a single TRUE/FALSE value")
    }
    if (length(self@commutative) != 1L || is.na(self@commutative)) {
      problems <- c(problems, "@commutative must be a single TRUE/FALSE value")
    }
    if (
      length(self@empty_policy) != 1L ||
        is.na(self@empty_policy) ||
        !self@empty_policy %in% TCCQ_REDUCTION_EMPTY_POLICIES
    ) {
      problems <- c(problems, "@empty_policy must be a supported reduction empty policy")
    }
    if (length(problems) > 0L) problems
  }
)

#' Scalar fold reduction metadata
#'
#' A fold reduction has one accumulator identity, one combine operation, and
#' an optional finalizer. Its neutral loop state has exactly one component.
#'
#' @inheritParams TccqReductionSpec
#' @param identity Function from result `TccqType` to identity `TccqLiteral`.
#' @param combine_op Registered elementwise operation combining the accumulator
#'   and current value.
#' @param finalize_op Optional registered elementwise operation combining the
#'   completed accumulator and reduced element count.
#' @export
TccqFoldReductionSpec <- S7::new_class(
  "TccqFoldReductionSpec",
  package = "tccquickr",
  parent = TccqReductionSpec,
  properties = list(
    identity = S7::class_function,
    combine_op = S7::class_character,
    finalize_op = S7::class_character
  ),
  validator = function(self) {
    problems <- character()
    if (!is.function(self@identity)) {
      problems <- c(problems, "@identity must be a function")
    }
    if (length(self@combine_op) != 1L || is.na(self@combine_op) || !nzchar(self@combine_op)) {
      problems <- c(problems, "@combine_op must be one non-empty operation name")
    }
    if (length(self@finalize_op) > 1L || anyNA(self@finalize_op)) {
      problems <- c(problems, "@finalize_op must be empty or one non-missing operation name")
    }
    if (!identical(self@empty_policy, "identity")) {
      problems <- c(problems, "fold reductions must use the identity empty policy")
    }
    if (length(problems) > 0L) problems
  }
)

#' Argument-selection reduction metadata
#'
#' An argument reduction tracks whether a value was seen, the selected value,
#' and its one-based index. The first concrete form supports R-compatible
#' maximum selection while ignoring missing double values.
#'
#' @inheritParams TccqReductionSpec
#' @param direction Value-selection direction.
#' @param missing Missing-value policy.
#' @param ties Tie-selection policy.
#' @export
TccqArgReductionSpec <- S7::new_class(
  "TccqArgReductionSpec",
  package = "tccquickr",
  parent = TccqReductionSpec,
  properties = list(
    direction = S7::class_character,
    missing = S7::class_character,
    ties = S7::class_character
  ),
  validator = function(self) {
    problems <- character()
    if (!identical(self@direction, "max")) {
      problems <- c(problems, "@direction must currently be `max`")
    }
    if (!identical(self@missing, "ignore")) {
      problems <- c(problems, "@missing must currently be `ignore`")
    }
    if (!identical(self@ties, "first")) {
      problems <- c(problems, "@ties must currently be `first`")
    }
    if (!identical(self@empty_policy, "error")) {
      problems <- c(problems, "argument reductions must use the error empty policy")
    }
    if (length(problems) > 0L) problems
  }
)

#' Contraction implementation metadata
#'
#' A contraction spec is attached to an operation implementation when calls to
#' that implementation lower to a loop nest that multiplies (or otherwise
#' combines) aligned elements along shared reduce axes and folds them with a
#' reducer, such as `%*%`. It carries the shared operation signature, the
#' reducer used for the fold, and the elementwise combine operation applied
#' before folding.
#'
#' @param name Human-readable contraction name.
#' @param signature Shared operation signature.
#' @param reducer Reduction metadata used for the contracted axes.
#' @param combine_op Elementwise operation name combining aligned elements.
#' @param attrs Structured contraction metadata.
#' @export
TccqContractionSpec <- S7::new_class(
  "TccqContractionSpec",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    signature = TccqOpSignature,
    reducer = TccqFoldReductionSpec,
    combine_op = S7::class_character,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (length(self@combine_op) != 1L || is.na(self@combine_op) || !nzchar(self@combine_op)) {
      problems <- c(problems, "@combine_op must be a single non-empty operation name")
    }
    if (length(problems) > 0L) problems
  }
)

#' Iteration implementation metadata
#'
#' An iteration spec describes an operation whose result can be consumed as a
#' virtual one-dimensional iteration space. The shared signature owns argument
#' and result typing; `extent_arg` selects the argument proving the domain
#' extent, while `start` defines the first affine induction value. The current
#' form deliberately represents unit-stride sequences only.
#'
#' @param name Human-readable iteration operation name.
#' @param signature Shared operation signature.
#' @param extent_arg One-based argument position supplying the domain extent.
#' @param start First integer value produced by the iteration.
#' @export
TccqIterationSpec <- S7::new_class(
  "TccqIterationSpec",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    signature = TccqOpSignature,
    extent_arg = S7::class_integer,
    start = S7::class_integer
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (
      length(self@extent_arg) != 1L ||
        is.na(self@extent_arg) ||
        self@extent_arg < 1L
    ) {
      problems <- c(problems, "@extent_arg must be one positive argument position")
    }
    if (length(self@start) != 1L || is.na(self@start)) {
      problems <- c(problems, "@start must be one integer")
    }
    if (
      !identical(self@signature@arity@counts, 1L) ||
        !is.null(self@signature@arity@minimum) ||
        !identical(self@extent_arg, 1L)
    ) {
      problems <- c(problems, "the current iteration contract must have exactly one extent argument")
    }
    if (length(problems) > 0L) problems
  }
)

#' Subscript implementation metadata
#'
#' A subscript spec describes one typed R indexing contract. The first slice is
#' deliberately strict: one scalar integer selector per atomic source axis,
#' with every enclosing iteration proving that its selector is one-based and in
#' bounds. Other selector kinds require their own shape and bounds semantics.
#'
#' @param name Human-readable subscript operation name.
#' @param signature Shared operation signature.
#' @export
TccqSubscriptSpec <- S7::new_class(
  "TccqSubscriptSpec",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    signature = TccqOpSignature
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    minimum_supported_arity <- if (length(self@signature@arity@counts) > 0L) {
      min(self@signature@arity@counts)
    } else {
      self@signature@arity@minimum
    }
    if (minimum_supported_arity < 2L) {
      problems <- c(problems, "a subscript contract must require source and selector arguments")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend-neutral operation body
#'
#' An operation body is one R expression whose symbols are bound by exact call
#' tags and then positionally through `@parameters`. Its call index preserves
#' the evaluator facts observed when the body was declared. Lowering expands
#' the body into ordinary typed values before fusion, loop-nest planning, or
#' backend rendering.
#'
#' @param parameters Ordered operation parameter names.
#' @param expression Unevaluated R expression implementing the operation.
#' @param call_index Typed calls and evaluator facts for `expression`.
#' @export
TccqOpBody <- S7::new_class(
  "TccqOpBody",
  package = "tccquickr",
  properties = list(
    parameters = S7::class_character,
    expression = S7::new_union(S7::class_language, S7::class_atomic),
    call_index = TccqCallIndex
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@parameters) == 0L ||
        anyNA(self@parameters) ||
        any(!nzchar(self@parameters)) ||
        anyDuplicated(self@parameters)
    ) {
      problems <- c(problems, "@parameters must be unique non-empty names")
    }
    if ("..." %in% self@parameters) {
      problems <- c(problems, "operation bodies cannot have a `...` parameter")
    }
    if (is.atomic(self@expression) && length(self@expression) != 1L) {
      problems <- c(problems, "atomic operation bodies must be scalar")
    }

    observed_calls <- tccq_collect_calls(self@expression)
    calls_align <- length(observed_calls) == length(self@call_index@calls) && all(vapply(
      seq_along(observed_calls),
      function(position) {
        observed <- observed_calls[[position]]
        indexed <- self@call_index@calls[[position]]
        identical(observed@name, indexed@name) &&
          identical(observed@expr, indexed@expr) &&
          identical(observed@kind, indexed@kind) &&
          identical(observed@arity, indexed@arity) &&
          identical(observed@argument_names, indexed@argument_names)
      },
      logical(1)
    ))
    if (!calls_align) {
      problems <- c(problems, "@call_index must describe @expression one-to-one")
    }
    if (any(vapply(
      self@call_index@semantics,
      function(semantics) {
        isTRUE(semantics@control) ||
          isTRUE(semantics@replacement) ||
          identical(semantics@forcing_policy, "special")
      },
      logical(1)
    ))) {
      problems <- c(
        problems,
        paste0(
          "the current neutral operation body supports expressions without ",
          "control, replacement, or special-forcing calls"
        )
      )
    }
    if (length(problems) > 0L) problems
  }
)

#' Operation implementation descriptor
#'
#' @param op Operation or function name.
#' @param target Implementation target, such as `r_language`, `pure_c`, or
#'   `fortran`.
#' @param region_kind Region kind the implementation can run in, or `any`.
#' @param memory_space Memory space the implementation expects, or `any`.
#' @param uses_rapi Whether the implementation touches the R C API.
#' @param boundary Whether the implementation crosses a boundary.
#' @param pure Whether the implementation is semantically pure.
#' @param effect Effect summary for calls handled by this implementation.
#' @param supports Predicate receiving a `TccqCall` and `TccqOpContext`.
#' @param render Optional source renderer receiving operand strings and a
#'   `TccqOpRenderContext`.
#' @param body Optional backend-neutral operation body.
#' @param elementwise Optional elementwise metadata.
#' @param reduction Optional reduction metadata.
#' @param contraction Optional contraction metadata.
#' @param iteration Optional iteration metadata.
#' @param subscript Optional subscript metadata.
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
    body = S7::new_union(NULL, TccqOpBody),
    elementwise = S7::new_union(NULL, TccqElementwiseSpec),
    reduction = S7::new_union(NULL, TccqReductionSpec),
    contraction = S7::new_union(NULL, TccqContractionSpec),
    iteration = S7::new_union(NULL, TccqIterationSpec),
    subscript = S7::new_union(NULL, TccqSubscriptSpec)
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
    if (!is.null(self@body)) {
      body_has_invalid_implementation <-
        !identical(self@target, "neutral") ||
        !isTRUE(self@pure) ||
        isTRUE(self@uses_rapi) ||
        isTRUE(self@boundary) ||
        isTRUE(self@effect@writes) ||
        isTRUE(self@effect@allocates) ||
        isTRUE(self@effect@boundary) ||
        !is.null(self@render) ||
        !S7::S7_inherits(self@elementwise, TccqElementwiseSpec)
      if (body_has_invalid_implementation) {
        problems <- c(
          problems,
          "neutral operation bodies require a pure, renderer-free elementwise implementation"
        )
      } else if (
        length(self@elementwise@signature@arity@counts) != 1L ||
          !identical(
            self@elementwise@signature@arity@counts[[1L]],
            as.integer(length(self@body@parameters))
          ) ||
          !is.null(self@elementwise@signature@arity@minimum) ||
          !is.null(self@elementwise@signature@arity@maximum)
      ) {
        problems <- c(problems, "neutral operation body parameters must match the elementwise arity")
      }
    }
    family_count <- sum(c(
      !is.null(self@elementwise),
      !is.null(self@reduction),
      !is.null(self@contraction),
      !is.null(self@iteration),
      !is.null(self@subscript)
    ))
    if (family_count > 1L) {
      problems <- c(problems, "an operation implementation may declare at most one operation family")
    }
    if (!is.null(self@iteration)) {
      invalid_iteration_effect <-
        !isTRUE(self@pure) ||
        isTRUE(self@uses_rapi) ||
        isTRUE(self@boundary) ||
        isTRUE(self@effect@writes) ||
        isTRUE(self@effect@allocates) ||
        isTRUE(self@effect@boundary) ||
        isTRUE(self@effect@may_error) ||
        isTRUE(self@effect@may_warn)
      if (invalid_iteration_effect) {
        problems <- c(problems, "virtual iteration implementations must be pure read-only operations")
      }
    }
    if (!is.null(self@subscript)) {
      invalid_subscript_effect <-
        !isTRUE(self@pure) ||
          isTRUE(self@uses_rapi) ||
          isTRUE(self@boundary) ||
          isTRUE(self@effect@writes) ||
          isTRUE(self@effect@allocates) ||
          isTRUE(self@effect@boundary) ||
          isTRUE(self@effect@may_error) ||
          isTRUE(self@effect@may_warn)
      if (invalid_subscript_effect) {
        problems <- c(problems, "proven subscript implementations must be pure read-only operations")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Operation registry
#'
#' @param implementations List of operation implementations.
#' @param op_index Named list from operation name to implementation positions,
#'   used to resolve calls without scanning every implementation. Built by
#'   [tccq_op_registry()]; an empty index falls back to a full scan.
#' @export
TccqOpRegistry <- S7::new_class(
  "TccqOpRegistry",
  package = "tccquickr",
  properties = list(
    implementations = S7::class_list,
    op_index = S7::class_list
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
    if (
      length(self@op_index) > 0L &&
        (is.null(names(self@op_index)) || any(!nzchar(names(self@op_index))))
    ) {
      problems <- c(problems, "@op_index must be named by operation names")
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
#' @param contraction Optional contraction metadata supplied by the
#'   implementation.
#' @param iteration Optional iteration metadata supplied by the implementation.
#' @param subscript Optional subscript metadata supplied by the implementation.
#' @param body Optional backend-neutral body supplied by the implementation.
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
    body = S7::new_union(NULL, TccqOpBody),
    elementwise = S7::new_union(NULL, TccqElementwiseSpec),
    reduction = S7::new_union(NULL, TccqReductionSpec),
    contraction = S7::new_union(NULL, TccqContractionSpec),
    iteration = S7::new_union(NULL, TccqIterationSpec),
    subscript = S7::new_union(NULL, TccqSubscriptSpec),
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
    implementation_matches_call <- self@implementation@op %in% c(
      self@call@name,
      TCCQ_ANY_OP
    )
    if (!implementation_matches_call) {
      problems <- c(problems, "@implementation must handle @call")
    }
    implementation_snapshot_matches <-
      identical(self@target, self@implementation@target) &&
      identical(self@region_kind, self@implementation@region_kind) &&
      identical(self@memory_space, self@implementation@memory_space) &&
      identical(self@uses_rapi, self@implementation@uses_rapi) &&
      identical(self@boundary, self@implementation@boundary) &&
      identical(self@pure, self@implementation@pure) &&
      identical(self@effect, self@implementation@effect) &&
      identical(self@body, self@implementation@body) &&
      identical(self@elementwise, self@implementation@elementwise) &&
      identical(self@reduction, self@implementation@reduction) &&
      identical(self@contraction, self@implementation@contraction) &&
      identical(self@iteration, self@implementation@iteration) &&
      identical(self@subscript, self@implementation@subscript)
    if (!implementation_snapshot_matches) {
      problems <- c(problems, "resolved implementation fields must match @implementation")
    }
    if (length(problems) > 0L) problems
  }
)

#' Lowered operation payload
#'
#' `TccqLoweredOperation` is the typed payload attached to operation values
#' after lowering. It keeps operation family, selected implementation,
#' signature, domain policy, and optional reducer facts together so later
#' passes do not branch on loose value attributes.
#'
#' @param family Lowered operation family.
#' @param resolved_op Selected operation implementation.
#' @param signature Shared operation signature.
#' @param domain_policy Optional result-domain policy.
#' @param elementwise Optional elementwise metadata.
#' @param reduction Optional reduction metadata.
#' @param contraction Optional contraction metadata.
#' @param subscript Optional subscript metadata.
#' @param attrs Structured metadata.
#' @export
TccqLoweredOperation <- S7::new_class(
  "TccqLoweredOperation",
  package = "tccquickr",
  properties = list(
    family = S7::class_character,
    resolved_op = TccqResolvedOp,
    signature = TccqOpSignature,
    domain_policy = S7::new_union(NULL, TccqDomainPolicy),
    elementwise = S7::new_union(NULL, TccqElementwiseSpec),
    reduction = S7::new_union(NULL, TccqReductionSpec),
    contraction = S7::new_union(NULL, TccqContractionSpec),
    subscript = S7::new_union(NULL, TccqSubscriptSpec),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@family) != 1L ||
        is.na(self@family) ||
        !self@family %in% TCCQ_LOWERED_OPERATION_FAMILIES
    ) {
      problems <- c(problems, "@family must be one supported lowered operation family")
    }
    if (identical(self@family, "elementwise")) {
      if (!S7::S7_inherits(self@elementwise, TccqElementwiseSpec)) {
        problems <- c(problems, "elementwise lowered operations must carry elementwise metadata")
      }
      if (
        !is.null(self@reduction) ||
          !is.null(self@contraction) ||
          !is.null(self@subscript)
      ) {
        problems <- c(problems, "elementwise lowered operations cannot carry reducer metadata")
      }
      if (!S7::S7_inherits(self@resolved_op@elementwise, TccqElementwiseSpec)) {
        problems <- c(problems, "elementwise lowered operations need an elementwise resolved op")
      }
    }
    if (identical(self@family, "reduction")) {
      if (!S7::S7_inherits(self@reduction, TccqReductionSpec)) {
        problems <- c(problems, "reduction lowered operations must carry reduction metadata")
      }
      if (
        !is.null(self@elementwise) ||
          !is.null(self@contraction) ||
          !is.null(self@subscript)
      ) {
        problems <- c(problems, "reduction lowered operations cannot carry other family metadata")
      }
      if (!S7::S7_inherits(self@resolved_op@reduction, TccqReductionSpec)) {
        problems <- c(problems, "reduction lowered operations need a reduction resolved op")
      }
    }
    if (identical(self@family, "contraction")) {
      if (!S7::S7_inherits(self@contraction, TccqContractionSpec)) {
        problems <- c(problems, "contraction lowered operations must carry contraction metadata")
      }
      if (
        !is.null(self@elementwise) ||
          !is.null(self@reduction) ||
          !is.null(self@subscript)
      ) {
        problems <- c(problems, "contraction lowered operations cannot carry other family metadata")
      }
      if (!S7::S7_inherits(self@resolved_op@contraction, TccqContractionSpec)) {
        problems <- c(problems, "contraction lowered operations need a contraction resolved op")
      }
    }
    if (identical(self@family, "subscript")) {
      if (!S7::S7_inherits(self@subscript, TccqSubscriptSpec)) {
        problems <- c(problems, "subscript lowered operations must carry subscript metadata")
      }
      if (
        !is.null(self@elementwise) ||
          !is.null(self@reduction) ||
          !is.null(self@contraction)
      ) {
        problems <- c(problems, "subscript lowered operations cannot carry arithmetic family metadata")
      }
      if (!S7::S7_inherits(self@resolved_op@subscript, TccqSubscriptSpec)) {
        problems <- c(problems, "subscript lowered operations need a subscript resolved op")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Fusion operation contract
#'
#' `TccqFusionContract` is the typed payload attached to a fusion group after
#' region planning. It keeps the lowered operation payloads, signatures, domain
#' policies, typed result value, optional result operation, and storage strategy
#' together so fusion legality and later optimization passes do not inspect
#' loose group attributes. Control values such as `TccqBranch` are valid fusion
#' results even when the group contains no lowered operations.
#'
#' @param fusion_kind Fusion kind owned by the contract.
#' @param storage_strategy Storage strategy requested by the fusion plan.
#' @param operations Named lowered operations by value id.
#' @param result_value Typed value produced by the fusion group.
#' @param result_operation Optional lowered operation carried by `result_value`.
#' @param operation_signatures Named operation signatures by value id.
#' @param domain_policies Named domain policies by value id.
#' @param attrs Structured metadata.
#' @export
TccqFusionContract <- S7::new_class(
  "TccqFusionContract",
  package = "tccquickr",
  properties = list(
    fusion_kind = S7::class_character,
    storage_strategy = S7::class_character,
    operations = S7::class_list,
    result_value = TccqValue,
    result_operation = S7::new_union(NULL, TccqLoweredOperation),
    operation_signatures = S7::class_list,
    domain_policies = S7::class_list,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@fusion_kind) != 1L ||
        is.na(self@fusion_kind) ||
        !self@fusion_kind %in% TCCQ_FUSION_KINDS
    ) {
      problems <- c(problems, "@fusion_kind must be one supported fusion kind")
    }
    if (length(self@storage_strategy) != 1L || is.na(self@storage_strategy) || !nzchar(self@storage_strategy)) {
      problems <- c(problems, "@storage_strategy must be a single non-empty string")
    }
    operations_are_lowered <- vapply(
      self@operations,
      S7::S7_inherits,
      logical(1),
      class = TccqLoweredOperation
    )
    signatures_are_typed <- vapply(
      self@operation_signatures,
      S7::S7_inherits,
      logical(1),
      class = TccqOpSignature
    )
    policies_are_typed <- vapply(
      self@domain_policies,
      S7::S7_inherits,
      logical(1),
      class = TccqDomainPolicy
    )
    operation_ids <- names(self@operations)
    signature_ids <- names(self@operation_signatures)
    domain_policy_ids <- names(self@domain_policies)
    if (!all(operations_are_lowered)) {
      problems <- c(problems, "@operations must contain only <TccqLoweredOperation> values")
    }
    if (
      length(self@operations) > 0L &&
        (is.null(operation_ids) || anyNA(operation_ids) || any(!nzchar(operation_ids)) || anyDuplicated(operation_ids))
    ) {
      problems <- c(problems, "@operations must be named by unique non-empty value ids")
    }
    if (!all(signatures_are_typed)) {
      problems <- c(problems, "@operation_signatures must contain only <TccqOpSignature> values")
    }
    if (!identical(signature_ids, operation_ids)) {
      problems <- c(problems, "@operation_signatures must align with @operations by value id")
    }
    if (!all(policies_are_typed)) {
      problems <- c(problems, "@domain_policies must contain only <TccqDomainPolicy> values")
    }
    if (!identical(domain_policy_ids, operation_ids)) {
      problems <- c(problems, "@domain_policies must align with @operations by value id")
    }
    result_value_operation <- self@result_value@attrs$operation
    if (!is.null(result_value_operation) && !S7::S7_inherits(result_value_operation, TccqLoweredOperation)) {
      problems <- c(problems, "@result_value operation metadata must be a <TccqLoweredOperation>")
    }
    if (!identical(result_value_operation, self@result_operation)) {
      problems <- c(problems, "@result_operation must match the operation carried by @result_value")
    }
    if (
      !is.null(self@result_operation) &&
        (!self@result_value@id %in% operation_ids ||
          !identical(self@operations[[self@result_value@id]], self@result_operation))
    ) {
      problems <- c(problems, "@result_operation must be indexed by the result value id in @operations")
    }
    if (identical(self@fusion_kind, "map") && any(vapply(
      self@operations,
      function(operation) identical(operation@family, "reduction"),
      logical(1)
    ))) {
      problems <- c(problems, "map fusion contracts cannot contain reduction operations")
    }
    fusion_needs_reduction_result <- length(self@fusion_kind) == 1L &&
      !is.na(self@fusion_kind) &&
      self@fusion_kind %in% c("map_reduce", "axis_reduce")
    if (
      fusion_needs_reduction_result &&
        (is.null(self@result_operation) || !identical(self@result_operation@family, "reduction"))
    ) {
      problems <- c(problems, "reduction fusion contracts need a reduction result operation")
    }
    if (
      identical(self@fusion_kind, "contract") &&
        (is.null(self@result_operation) || !identical(self@result_operation@family, "contraction"))
    ) {
      problems <- c(problems, "contract fusion contracts need a contraction result operation")
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

#' Construct the standard elementwise domain policy
#'
#' The standard elementwise policy returns the shared non-scalar input shape
#' and accepts scalar broadcasting. Following R's recycling rule, a shorter
#' non-scalar operand is also accepted when its dimension multiset divides the
#' longest operand's dimension multiset (so its total length provably divides
#' the iteration size); the result takes the longest shape, and accesses to
#' the shorter operand recycle over the iteration order. Shapes whose
#' divisibility cannot be proven from declared dimensions are rejected.
#'
#' @export
tccq_elementwise_domain_policy <- function() {
  tccq_domain_policy(
    "elementwise_common_shape",
    result_shape = function(input_types) {
      dim_label <- function(dim) {
        if (identical(dim@kind, "constant")) {
          return(sprintf("constant:%d", dim@value))
        }
        if (identical(dim@kind, "symbol")) {
          return(sprintf("symbol:%s", dim@label))
        }
        if (identical(dim@kind, "affine")) {
          return(sprintf("affine:%s%+d", dim@label, dim@value))
        }
        "unknown"
      }
      shape_dim_labels <- function(shape) {
        vapply(shape@dims, dim_label, character(1))
      }

      non_scalar_shapes <- lapply(
        Filter(function(type) type@shape@rank > 0L, input_types),
        function(type) type@shape
      )
      if (length(non_scalar_shapes) == 0L) {
        return(tccq_shape())
      }

      dim_labels <- lapply(non_scalar_shapes, shape_dim_labels)
      shape_labels <- vapply(dim_labels, paste, character(1), collapse = "/")
      if (length(unique(shape_labels)) == 1L) {
        return(non_scalar_shapes[[1L]])
      }

      multiset_contains <- function(big, small) {
        if (any(big == "unknown") || any(small == "unknown")) {
          return(FALSE)
        }
        # A length-1 dimension divides everything, as in R's recycling.
        small <- small[small != "constant:1"]
        remaining <- big
        for (item in small) {
          hit <- match(item, remaining)
          if (is.na(hit)) {
            return(FALSE)
          }
          remaining <- remaining[-hit]
        }
        TRUE
      }
      # R's rule: operands of rank >= 2 must agree exactly (non-conformable
      # arrays are errors, never recycled); only lower-rank operands recycle,
      # and only when their length provably divides the host's.
      ranks <- vapply(non_scalar_shapes, function(shape) shape@rank, integer(1))
      array_positions <- which(ranks >= 2L)
      if (length(unique(shape_labels[array_positions])) > 1L) {
        tccq_abort(
          "ops.incompatible_elementwise_shapes",
          "Array operands of one elementwise operation must share one ordered shape.",
          phase = "ops",
          path = "domain_policy.result_shape",
          data = list(shapes = shape_labels)
        )
      }
      host_position <- if (length(array_positions) > 0L) {
        array_positions[[1L]]
      } else {
        hosts_all <- vapply(seq_along(dim_labels), function(candidate) {
          all(vapply(
            dim_labels,
            multiset_contains,
            logical(1),
            big = dim_labels[[candidate]]
          ))
        }, logical(1))
        if (any(hosts_all)) which(hosts_all)[[1L]] else NULL
      }
      recycle_is_provable <- !is.null(host_position) && all(vapply(
        dim_labels,
        multiset_contains,
        logical(1),
        big = dim_labels[[host_position]]
      ))
      if (!recycle_is_provable) {
        tccq_abort(
          "ops.incompatible_elementwise_shapes",
          "Elementwise inputs must share one shape or recycle operands whose dimensions divide the host shape.",
          phase = "ops",
          path = "domain_policy.result_shape",
          data = list(shapes = shape_labels)
        )
      }
      non_scalar_shapes[[host_position]]
    }
  )
}

#' Construct an operation arity contract
#'
#' @inheritParams TccqArity
#' @export
tccq_arity <- function(counts = integer(), minimum = NULL, maximum = NULL) {
  if (
    !is.numeric(counts) ||
      anyNA(counts) ||
      any(counts <= 0L) ||
      any(counts != as.integer(counts))
  ) {
    tccq_abort(
      "schema.invalid_arity_counts",
      "`counts` must contain positive integers.",
      phase = "schema",
      path = "arity.counts",
      data = list(counts = counts)
    )
  }
  if (!is.null(minimum) && (
    !is.numeric(minimum) ||
      length(minimum) != 1L ||
      is.na(minimum) ||
      minimum <= 0L ||
      minimum != as.integer(minimum)
  )) {
    tccq_abort(
      "schema.invalid_arity_minimum",
      "`minimum` must be one positive integer or `NULL`.",
      phase = "schema",
      path = "arity.minimum",
      data = list(minimum = minimum)
    )
  }
  if (!is.null(maximum) && (
    !is.numeric(maximum) ||
      length(maximum) != 1L ||
      is.na(maximum) ||
      maximum <= 0L ||
      maximum != as.integer(maximum)
  )) {
    tccq_abort(
      "schema.invalid_arity_maximum",
      "`maximum` must be one positive integer or `NULL`.",
      phase = "schema",
      path = "arity.maximum",
      data = list(maximum = maximum)
    )
  }
  exact_contract <- length(counts) > 0L
  interval_contract <- !is.null(minimum)
  if (identical(exact_contract, interval_contract)) {
    tccq_abort(
      "schema.invalid_arity_contract",
      "Arity must declare either exact `counts` or one `minimum` interval.",
      phase = "schema",
      path = "arity",
      data = list(counts = counts, minimum = minimum, maximum = maximum)
    )
  }
  if (!is.null(maximum) && (!interval_contract || maximum < minimum)) {
    tccq_abort(
      "schema.invalid_arity_maximum",
      "`maximum` requires `minimum` and must be no smaller than it.",
      phase = "schema",
      path = "arity.maximum",
      data = list(minimum = minimum, maximum = maximum)
    )
  }
  TccqArity(
    counts = unique(as.integer(counts)),
    minimum = if (is.null(minimum)) NULL else as.integer(minimum),
    maximum = if (is.null(maximum)) NULL else as.integer(maximum)
  )
}

#' Test an operation arity contract
#'
#' @param arity A [TccqArity] contract.
#' @param count Observed argument count.
#' @return One logical value.
#' @export
tccq_arity_accepts <- S7::new_generic(
  "tccq_arity_accepts",
  dispatch_args = "arity",
  function(arity, count) S7::S7_dispatch()
)

S7::method(tccq_arity_accepts, TccqArity) <- function(arity, count) {
  if (
    !is.numeric(count) ||
      length(count) != 1L ||
      is.na(count) ||
      count < 0L ||
      count != as.integer(count)
  ) {
    tccq_abort(
      "schema.invalid_observed_arity",
      "`count` must be one non-negative integer.",
      phase = "schema",
      path = "arity.count",
      data = list(count = count)
    )
  }
  count <- as.integer(count)
  if (length(arity@counts) > 0L) {
    return(count %in% arity@counts)
  }
  count >= arity@minimum && (is.null(arity@maximum) || count <= arity@maximum)
}

#' Construct operation signature metadata
#'
#' @param name Human-readable operation signature name.
#' @param arity Accepted argument counts or a [TccqArity] contract.
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
  if (!S7::S7_inherits(arity, TccqArity)) {
    arity <- tccq_arity(arity)
  }
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
  if (!tccq_arity_accepts(signature@arity, length(input_types))) {
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
  if (is.null(domain_policy)) {
    domain_policy <- tccq_elementwise_domain_policy()
  }
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
#' @param finalize_op Optional registered operation applied to the completed
#'   accumulator and reduced element count.
#' @param combine_op Registered operation combining the accumulator and current
#'   value.
#' @param associative Whether the reducer is associative.
#' @param commutative Whether the reducer is commutative.
#' @param attrs Structured reduction metadata.
#' @param signature Shared operation signature. By default, reductions accept
#'   one input and return a scalar of the input base type.
#' @export
tccq_reduction_spec <- function(
  name,
  identity,
  combine_op,
  finalize_op = "",
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
  .tccq_check_character_scalar(combine_op, "combine_op")
  .tccq_check_character_or_empty(finalize_op, "finalize_op")
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

  TccqFoldReductionSpec(
    name = name,
    signature = signature,
    associative = associative,
    commutative = commutative,
    empty_policy = "identity",
    attrs = attrs,
    identity = identity,
    combine_op = combine_op,
    finalize_op = finalize_op
  )
}

#' Construct argument-selection reduction metadata
#'
#' @param name Human-readable reduction name.
#' @param signature Shared operation signature.
#' @param direction Value-selection direction.
#' @param missing Missing-value policy.
#' @param ties Tie-selection policy.
#' @param attrs Structured reduction metadata.
#' @export
tccq_arg_reduction_spec <- function(
  name,
  signature,
  direction = "max",
  missing = "ignore",
  ties = "first",
  attrs = list()
) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_s7(signature, TccqOpSignature, "TccqOpSignature", "signature")
  .tccq_check_character_scalar(direction, "direction")
  .tccq_check_character_scalar(missing, "missing")
  .tccq_check_character_scalar(ties, "ties")
  .tccq_check_list(attrs, "attrs")

  TccqArgReductionSpec(
    name = name,
    signature = signature,
    associative = FALSE,
    commutative = FALSE,
    empty_policy = "error",
    attrs = attrs,
    direction = direction,
    missing = missing,
    ties = ties
  )
}

#' Construct contraction implementation metadata
#'
#' @param name Human-readable contraction name.
#' @param signature Shared operation signature.
#' @param reducer Reduction metadata used for the contracted axes.
#' @param combine_op Elementwise operation name combining aligned elements.
#' @param attrs Structured contraction metadata.
#' @export
tccq_contraction_spec <- function(
  name,
  signature,
  reducer,
  combine_op = "*",
  attrs = list()
) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_s7(signature, TccqOpSignature, "TccqOpSignature", "signature")
  .tccq_check_s7(reducer, TccqFoldReductionSpec, "TccqFoldReductionSpec", "reducer")
  .tccq_check_character_scalar(combine_op, "combine_op")
  .tccq_check_list(attrs, "attrs")

  TccqContractionSpec(
    name = name,
    signature = signature,
    reducer = reducer,
    combine_op = combine_op,
    attrs = attrs
  )
}

#' Construct iteration implementation metadata
#'
#' @inheritParams TccqIterationSpec
#' @export
tccq_iteration_spec <- function(name, signature, extent_arg = 1L, start = 1L) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_s7(signature, TccqOpSignature, "TccqOpSignature", "signature")
  if (
    !is.numeric(extent_arg) ||
      length(extent_arg) != 1L ||
      is.na(extent_arg) ||
      extent_arg != as.integer(extent_arg) ||
      extent_arg < 1L
  ) {
    tccq_abort(
      "schema.invalid_iteration_extent_arg",
      "`extent_arg` must be one positive integer argument position.",
      phase = "schema",
      path = "iteration.extent_arg",
      data = list(extent_arg = extent_arg)
    )
  }
  if (
    !is.numeric(start) ||
      length(start) != 1L ||
      is.na(start) ||
      start != as.integer(start)
  ) {
    tccq_abort(
      "schema.invalid_iteration_start",
      "`start` must be one integer.",
      phase = "schema",
      path = "iteration.start",
      data = list(start = start)
    )
  }
  TccqIterationSpec(
    name = name,
    signature = signature,
    extent_arg = as.integer(extent_arg),
    start = as.integer(start)
  )
}

#' Construct subscript implementation metadata
#'
#' @inheritParams TccqSubscriptSpec
#' @export
tccq_subscript_spec <- function(name, signature) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_s7(signature, TccqOpSignature, "TccqOpSignature", "signature")
  minimum_supported_arity <- if (length(signature@arity@counts) > 0L) {
    min(signature@arity@counts)
  } else {
    signature@arity@minimum
  }
  if (minimum_supported_arity < 2L) {
    tccq_abort(
      "schema.invalid_subscript_arity",
      "A subscript signature must require source and selector arguments.",
      phase = "schema",
      path = "subscript.signature.arity",
      data = list(name = name, arity = signature@arity)
    )
  }
  TccqSubscriptSpec(
    name = name,
    signature = signature
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

S7::method(tccq_reduction_identity, TccqFoldReductionSpec) <- function(spec, type) {
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
        signature <- if (S7::S7_inherits(impl@elementwise, TccqElementwiseSpec)) {
          impl@elementwise@signature
        } else if (S7::S7_inherits(impl@reduction, TccqReductionSpec)) {
          impl@reduction@signature
        } else if (S7::S7_inherits(impl@contraction, TccqContractionSpec)) {
          impl@contraction@signature
        } else if (S7::S7_inherits(impl@iteration, TccqIterationSpec)) {
          impl@iteration@signature
        } else if (S7::S7_inherits(impl@subscript, TccqSubscriptSpec)) {
          impl@subscript@signature
        } else {
          NULL
        }
        if (
          !is.null(signature) &&
            !is.na(call@arity) &&
            !tccq_arity_accepts(signature@arity, call@arity)
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
      children <- as.list(node)[-1L]
      for (child_index in seq_along(children)) {
        if (identical(children[[child_index]], quote(expr = ))) {
          next
        }
        walk(children[[child_index]])
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
  inferred_dispatch_kind <- dispatch_kind_from_function(function_object, inferred_evaluator_kind)
  s3_default_exists <- identical(inferred_dispatch_kind, "s3") &&
    !is.null(tryCatch(
      utils::getS3method(call@name, "default", optional = TRUE, envir = env),
      error = function(err) NULL
    ))
  facts <- list(
    evaluator_kind = inferred_evaluator_kind,
    forcing_policy = forcing_policy_from_evaluator(inferred_evaluator_kind),
    dispatch_kind = inferred_dispatch_kind,
    lexical_scope = identical(inferred_evaluator_kind, "closure") ||
      identical(call@kind, "function_definition"),
    replacement = identical(call@kind, "replacement"),
    control = identical(call@kind, "control"),
    attrs = list(
      resolved = !is.null(function_object),
      primitive = is.function(function_object) && is.primitive(function_object),
      s3_default_exists = s3_default_exists
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

#' Construct a backend-neutral operation body
#'
#' `fn` supplies only ordered parameter names and one implementation expression;
#' parameter types come from the operation signature at each expansion site.
#' Exact call tags bind first and remaining arguments bind positionally. Partial
#' tags, defaults, and `...` are excluded because their complete R argument and
#' promise semantics are not represented by this expression-body contract.
#'
#' @param fn Function with simple formals and one implementation expression.
#' @export
tccq_op_body <- function(fn) {
  if (!is.function(fn)) {
    tccq_abort(
      "schema.invalid_op_body",
      "`fn` must be a function.",
      phase = "schema",
      path = "op_body.fn",
      data = list(actual = typeof(fn))
    )
  }

  formal_values <- as.list(formals(fn))
  parameters <- names(formal_values)
  simple_formals <- length(parameters) > 0L &&
    !anyNA(parameters) &&
    all(nzchar(parameters)) &&
    !anyDuplicated(parameters) &&
    !"..." %in% parameters &&
    all(vapply(
      formal_values,
      function(default) identical(default, quote(expr = )),
      logical(1)
    ))
  if (!simple_formals) {
    tccq_abort(
      "schema.invalid_op_body_formals",
      "An operation body needs unique simple formals without defaults or `...`.",
      phase = "schema",
      path = "op_body.parameters",
      data = list(parameters = parameters)
    )
  }

  implementation_forms <- if (
    is.call(body(fn)) && identical(tccq_call_name(body(fn)), "{")
  ) {
    as.list(body(fn))[-1L]
  } else {
    list(body(fn))
  }
  if (length(implementation_forms) != 1L) {
    tccq_abort(
      "schema.invalid_op_body_expression_count",
      "An operation body must contain exactly one expression.",
      phase = "schema",
      path = "op_body.expression",
      data = list(expressions = length(implementation_forms))
    )
  }
  expression <- implementation_forms[[1L]]
  if (!is.language(expression) && !(is.atomic(expression) && length(expression) == 1L)) {
    tccq_abort(
      "schema.invalid_op_body_expression",
      "An operation body must be one language expression or scalar literal.",
      phase = "schema",
      path = "op_body.expression",
      data = list(actual = typeof(expression), length = length(expression))
    )
  }

  call_index <- tccq_collect_call_index(
    expression,
    env = environment(fn),
    attrs = list(origin = "operation_body")
  )
  evaluator_sensitive_calls <- Filter(
    function(semantics) {
      isTRUE(semantics@control) ||
        isTRUE(semantics@replacement) ||
        identical(semantics@forcing_policy, "special")
    },
    call_index@semantics
  )
  if (length(evaluator_sensitive_calls) > 0L) {
    tccq_abort(
      "schema.unsupported_op_body_semantics",
      paste0(
        "The current neutral operation body cannot contain control, replacement, ",
        "or special-forcing calls."
      ),
      phase = "schema",
      path = "op_body.call_index",
      data = list(calls = vapply(
        evaluator_sensitive_calls,
        function(semantics) semantics@call@name,
        character(1)
      ))
    )
  }

  TccqOpBody(
    parameters = parameters,
    expression = expression,
    call_index = call_index
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
#' @param body Optional backend-neutral operation body.
#' @param elementwise Optional elementwise metadata.
#' @param reduction Optional reduction metadata.
#' @param contraction Optional contraction metadata.
#' @param iteration Optional iteration metadata.
#' @param subscript Optional subscript metadata.
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
  body = NULL,
  elementwise = NULL,
  reduction = NULL,
  contraction = NULL,
  iteration = NULL,
  subscript = NULL
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
  .tccq_check_optional_s7(body, TccqOpBody, "TccqOpBody", "body")
  .tccq_check_optional_s7(elementwise, TccqElementwiseSpec, "TccqElementwiseSpec", "elementwise")
  .tccq_check_optional_s7(reduction, TccqReductionSpec, "TccqReductionSpec", "reduction")
  .tccq_check_optional_s7(contraction, TccqContractionSpec, "TccqContractionSpec", "contraction")
  .tccq_check_optional_s7(iteration, TccqIterationSpec, "TccqIterationSpec", "iteration")
  .tccq_check_optional_s7(subscript, TccqSubscriptSpec, "TccqSubscriptSpec", "subscript")
  if (!is.null(body)) {
    body_has_invalid_implementation <-
      !identical(target, "neutral") ||
      !isTRUE(pure) ||
      isTRUE(uses_rapi) ||
      isTRUE(boundary) ||
      isTRUE(effect@writes) ||
      isTRUE(effect@allocates) ||
      isTRUE(effect@boundary) ||
      !is.null(render) ||
      !S7::S7_inherits(elementwise, TccqElementwiseSpec)
    body_arity_matches <- S7::S7_inherits(elementwise, TccqElementwiseSpec) &&
      length(elementwise@signature@arity@counts) == 1L &&
      identical(
        elementwise@signature@arity@counts[[1L]],
        as.integer(length(body@parameters))
      ) &&
      is.null(elementwise@signature@arity@minimum) &&
      is.null(elementwise@signature@arity@maximum)
    if (body_has_invalid_implementation || !body_arity_matches) {
      tccq_abort(
        "schema.invalid_op_body_implementation",
        paste0(
          "A neutral operation body requires a pure, renderer-free elementwise ",
          "implementation whose single arity matches its parameters."
        ),
        phase = "schema",
        path = "op.body",
        data = list(
          op = op,
          target = target,
          parameters = body@parameters,
          arity = if (S7::S7_inherits(elementwise, TccqElementwiseSpec)) {
            elementwise@signature@arity
          } else {
            integer()
          }
        )
      )
    }
  }
  if (
    !is.null(iteration) &&
      (
        !isTRUE(pure) ||
          isTRUE(uses_rapi) ||
          isTRUE(boundary) ||
          isTRUE(effect@writes) ||
          isTRUE(effect@allocates) ||
          isTRUE(effect@boundary) ||
          isTRUE(effect@may_error) ||
          isTRUE(effect@may_warn)
      )
  ) {
    tccq_abort(
      "schema.invalid_iteration_implementation",
      "Virtual iteration implementations must be pure read-only operations.",
      phase = "schema",
      path = "op.iteration",
      data = list(op = op, effect = effect)
    )
  }
  if (
    !is.null(subscript) &&
      (
        !isTRUE(pure) ||
          isTRUE(uses_rapi) ||
          isTRUE(boundary) ||
          isTRUE(effect@writes) ||
          isTRUE(effect@allocates) ||
          isTRUE(effect@boundary) ||
          isTRUE(effect@may_error) ||
          isTRUE(effect@may_warn)
      )
  ) {
    tccq_abort(
      "schema.invalid_subscript_implementation",
      "Proven subscript implementations must be pure read-only operations.",
      phase = "schema",
      path = "op.subscript",
      data = list(op = op, effect = effect)
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
    effect = effect,
    supports = supports,
    render = render,
    body = body,
    elementwise = elementwise,
    reduction = reduction,
    contraction = contraction,
    iteration = iteration,
    subscript = subscript
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

  op_names <- vapply(implementations, function(impl) impl@op, character(1))
  TccqOpRegistry(
    implementations = implementations,
    op_index = split(seq_along(implementations), op_names)
  )
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
    body = implementation@body,
    elementwise = implementation@elementwise,
    reduction = implementation@reduction,
    contraction = implementation@contraction,
    iteration = implementation@iteration,
    subscript = implementation@subscript,
    attrs = attrs
  )
}

#' Construct a lowered operation payload
#'
#' @param family Lowered operation family.
#' @param resolved_op Selected operation implementation.
#' @param signature Optional operation signature. Defaults to the family spec.
#' @param elementwise Optional elementwise metadata.
#' @param reduction Optional reduction metadata.
#' @param contraction Optional contraction metadata.
#' @param subscript Optional subscript metadata.
#' @param attrs Structured metadata.
#' @export
tccq_lowered_operation <- function(
  family,
  resolved_op,
  signature = NULL,
  elementwise = NULL,
  reduction = NULL,
  contraction = NULL,
  subscript = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(family, "family")
  if (!family %in% TCCQ_LOWERED_OPERATION_FAMILIES) {
    tccq_abort(
      "schema.invalid_lowered_operation_family",
      "`family` is not a supported lowered operation family.",
      phase = "schema",
      path = "lowered_operation.family",
      data = list(family = family, supported = TCCQ_LOWERED_OPERATION_FAMILIES)
    )
  }
  .tccq_check_s7(resolved_op, TccqResolvedOp, "TccqResolvedOp", "resolved_op")
  .tccq_check_optional_s7(signature, TccqOpSignature, "TccqOpSignature", "signature")
  .tccq_check_optional_s7(elementwise, TccqElementwiseSpec, "TccqElementwiseSpec", "elementwise")
  .tccq_check_optional_s7(reduction, TccqReductionSpec, "TccqReductionSpec", "reduction")
  .tccq_check_optional_s7(contraction, TccqContractionSpec, "TccqContractionSpec", "contraction")
  .tccq_check_optional_s7(subscript, TccqSubscriptSpec, "TccqSubscriptSpec", "subscript")
  .tccq_check_list(attrs, "attrs")

  if (identical(family, "elementwise")) {
    elementwise <- elementwise %||% resolved_op@elementwise
    if (!S7::S7_inherits(elementwise, TccqElementwiseSpec)) {
      tccq_abort(
        "schema.lowered_operation_elementwise_required",
        "Elementwise lowered operations must carry elementwise metadata.",
        phase = "schema",
        path = "lowered_operation.elementwise",
        data = list(op = resolved_op@call@name)
      )
    }
    if (!is.null(reduction)) {
      tccq_abort(
        "schema.invalid_lowered_operation_payload",
        "Elementwise lowered operations cannot carry reducer metadata.",
        phase = "schema",
        path = "lowered_operation.reduction",
        data = list(op = resolved_op@call@name)
      )
    }
    signature <- signature %||% elementwise@signature
  }

  if (identical(family, "reduction")) {
    reduction <- reduction %||% resolved_op@reduction
    if (!S7::S7_inherits(reduction, TccqReductionSpec)) {
      tccq_abort(
        "schema.lowered_operation_reduction_required",
        "Reduction lowered operations must carry reduction metadata.",
        phase = "schema",
        path = "lowered_operation.reduction",
        data = list(op = resolved_op@call@name)
      )
    }
    if (!is.null(elementwise)) {
      tccq_abort(
        "schema.invalid_lowered_operation_payload",
        "Reduction lowered operations cannot carry elementwise metadata.",
        phase = "schema",
        path = "lowered_operation.elementwise",
        data = list(op = resolved_op@call@name)
      )
    }
    signature <- signature %||% reduction@signature
  }

  if (identical(family, "contraction")) {
    contraction <- contraction %||% resolved_op@contraction
    if (!S7::S7_inherits(contraction, TccqContractionSpec)) {
      tccq_abort(
        "schema.lowered_operation_contraction_required",
        "Contraction lowered operations must carry contraction metadata.",
        phase = "schema",
        path = "lowered_operation.contraction",
        data = list(op = resolved_op@call@name)
      )
    }
    signature <- signature %||% contraction@signature
  }

  if (identical(family, "subscript")) {
    subscript <- subscript %||% resolved_op@subscript
    if (!S7::S7_inherits(subscript, TccqSubscriptSpec)) {
      tccq_abort(
        "schema.lowered_operation_subscript_required",
        "Subscript lowered operations must carry subscript metadata.",
        phase = "schema",
        path = "lowered_operation.subscript",
        data = list(op = resolved_op@call@name)
      )
    }
    if (!is.null(elementwise) || !is.null(reduction) || !is.null(contraction)) {
      tccq_abort(
        "schema.invalid_lowered_operation_payload",
        "Subscript lowered operations cannot carry arithmetic family metadata.",
        phase = "schema",
        path = "lowered_operation.subscript",
        data = list(op = resolved_op@call@name)
      )
    }
    signature <- signature %||% subscript@signature
  }

  TccqLoweredOperation(
    family = family,
    resolved_op = resolved_op,
    signature = signature,
    domain_policy = signature@domain_policy,
    elementwise = elementwise,
    reduction = reduction,
    contraction = contraction,
    subscript = subscript,
    attrs = attrs
  )
}

#' Construct a fusion operation contract
#'
#' @param fusion_kind Fusion kind owned by the contract.
#' @param result_value Typed value produced by the fusion group.
#' @param operations Named lowered operations by value id. Control-only groups
#'   may use an empty list.
#' @param storage_strategy Optional storage strategy. Defaults from
#'   `fusion_kind`.
#' @param attrs Structured metadata.
#' @export
tccq_fusion_contract <- function(
  fusion_kind,
  result_value,
  operations = list(),
  storage_strategy = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(fusion_kind, "fusion_kind")
  if (!fusion_kind %in% TCCQ_FUSION_KINDS) {
    tccq_abort(
      "schema.invalid_fusion_kind",
      "`fusion_kind` is not a supported fusion kind.",
      phase = "schema",
      path = "fusion_contract.fusion_kind",
      data = list(fusion_kind = fusion_kind, supported = TCCQ_FUSION_KINDS)
    )
  }
  if (S7::S7_inherits(operations, TccqLoweredOperation)) {
    operations <- list(operations)
  }
  .tccq_check_list_of(operations, TccqLoweredOperation, "TccqLoweredOperation", "operations")
  .tccq_check_s7(result_value, TccqValue, "TccqValue", "result_value")
  operation_ids <- names(operations)
  if (
    length(operations) > 0L &&
      (is.null(operation_ids) ||
        anyNA(operation_ids) ||
        any(!nzchar(operation_ids)) ||
        anyDuplicated(operation_ids))
  ) {
    tccq_abort(
      "schema.invalid_fusion_contract_operations",
      "`operations` must be named by unique non-empty value ids.",
      phase = "schema",
      path = "fusion_contract.operations",
      data = list(operation_ids = operation_ids)
    )
  }
  result_operation <- result_value@attrs$operation
  .tccq_check_optional_s7(
    result_operation,
    TccqLoweredOperation,
    "TccqLoweredOperation",
    "result_value@attrs$operation"
  )
  if (is.null(storage_strategy)) {
    storage_strategy <- switch(
      fusion_kind,
      map = "fused-elementwise",
      map_reduce = "fused-map-reduce",
      axis_reduce = "fused-axis-reduce",
      paste0("fused-", fusion_kind)
    )
  }
  .tccq_check_character_scalar(storage_strategy, "storage_strategy")
  .tccq_check_list(attrs, "attrs")

  operation_signatures <- lapply(operations, function(operation) operation@signature)
  domain_policies <- lapply(operations, function(operation) operation@domain_policy)
  missing_domain_policy <- vapply(domain_policies, is.null, logical(1))
  if (any(missing_domain_policy)) {
    tccq_abort(
      "schema.fusion_contract_missing_domain_policy",
      "Fusion operations must carry domain policies.",
      phase = "schema",
      path = "fusion_contract.domain_policies",
      data = list(operation_ids = names(domain_policies)[missing_domain_policy])
    )
  }

  TccqFusionContract(
    fusion_kind = fusion_kind,
    storage_strategy = storage_strategy,
    operations = operations,
    result_value = result_value,
    result_operation = result_operation,
    operation_signatures = operation_signatures,
    domain_policies = domain_policies,
    attrs = attrs
  )
}

the_default_op_registry <- new.env(parent = emptyenv())

#' Default operation registry for the reset frontend
#'
#' The default registry names the R language call forms that the frontend
#' recognizes structurally. It is not a source-syntax whitelist and it is not a
#' backend lowering promise. The registry is immutable, so it is built once
#' per session and shared.
#'
#' @export
tccq_default_op_registry <- function() {
  if (!is.null(the_default_op_registry$registry)) {
    return(the_default_op_registry$registry)
  }
  comparison_renderer <- function(c_operator, fortran_operator = c_operator) {
    function(operands, context) {
      operator <- if (identical(context@language, "fortran")) {
        fortran_operator
      } else {
        c_operator
      }
      comparison <- sprintf(
        "(%s %s %s)",
        operands[[1L]],
        operator,
        operands[[2L]]
      )
      if (identical(context@language, "fortran")) {
        return(sprintf(
          paste0(
            "merge(tccq_na_logical, merge(1_c_int, 0_c_int, %s), ",
            "ieee_is_nan(real(%s, c_double)) .or. ",
            "ieee_is_nan(real(%s, c_double)))"
          ),
          comparison,
          operands[[1L]],
          operands[[2L]]
        ))
      }
      sprintf(
        paste0(
          "((isnan((double)(%s)) || isnan((double)(%s))) ",
          "? TCCQ_NA_LOGICAL : (%s ? 1 : 0))"
        ),
        operands[[1L]],
        operands[[2L]],
        comparison
      )
    }
  }
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
    "<" = comparison_renderer("<"),
    "<=" = comparison_renderer("<="),
    ">" = comparison_renderer(">"),
    ">=" = comparison_renderer(">="),
    "==" = comparison_renderer("=="),
    "!=" = comparison_renderer("!=", "/="),
    "!" = function(operands, context) {
      if (identical(context@language, "fortran")) {
        return(sprintf(
          "merge(tccq_na_logical, merge(1_c_int, 0_c_int, %s == 0_c_int), %s == tccq_na_logical)",
          operands[[1L]],
          operands[[1L]]
        ))
      }
      sprintf(
        "((%s == TCCQ_NA_LOGICAL) ? TCCQ_NA_LOGICAL : (%s == 0 ? 1 : 0))",
        operands[[1L]],
        operands[[1L]]
      )
    },
    "&" = function(operands, context) {
      if (identical(context@language, "fortran")) {
        return(sprintf(
          paste0(
            "merge(0_c_int, merge(tccq_na_logical, 1_c_int, ",
            "%s == tccq_na_logical .or. %s == tccq_na_logical), ",
            "%s == 0_c_int .or. %s == 0_c_int)"
          ),
          operands[[1L]], operands[[2L]], operands[[1L]], operands[[2L]]
        ))
      }
      sprintf(
        paste0(
          "((%s == 0 || %s == 0) ? 0 : ",
          "((%s == TCCQ_NA_LOGICAL || %s == TCCQ_NA_LOGICAL) ? TCCQ_NA_LOGICAL : 1))"
        ),
        operands[[1L]], operands[[2L]], operands[[1L]], operands[[2L]]
      )
    },
    "|" = function(operands, context) {
      if (identical(context@language, "fortran")) {
        return(sprintf(
          paste0(
            "merge(1_c_int, merge(tccq_na_logical, 0_c_int, ",
            "%s == tccq_na_logical .or. %s == tccq_na_logical), ",
            "(%s /= tccq_na_logical .and. %s /= 0_c_int) .or. ",
            "(%s /= tccq_na_logical .and. %s /= 0_c_int))"
          ),
          operands[[1L]], operands[[2L]],
          operands[[1L]], operands[[1L]], operands[[2L]], operands[[2L]]
        ))
      }
      sprintf(
        paste0(
          "(((%s != TCCQ_NA_LOGICAL && %s != 0) || ",
          "(%s != TCCQ_NA_LOGICAL && %s != 0)) ? 1 : ",
          "((%s == TCCQ_NA_LOGICAL || %s == TCCQ_NA_LOGICAL) ? TCCQ_NA_LOGICAL : 0))"
        ),
        operands[[1L]], operands[[1L]], operands[[2L]], operands[[2L]],
        operands[[1L]], operands[[2L]]
      )
    },
    "is.na" = function(operands, context) {
      if (identical(context@language, "fortran")) {
        return(sprintf(
          "merge(1_c_int, 0_c_int, ieee_is_nan(real(%s, c_double)))",
          operands[[1L]]
        ))
      }
      sprintf("(isnan((double)(%s)) ? 1 : 0)", operands[[1L]])
    },
    sqrt = function(operands, context) sprintf("sqrt(%s)", operands[[1L]]),
    exp = function(operands, context) sprintf("exp(%s)", operands[[1L]])
  )
  elementwise_domain_policy <- tccq_elementwise_domain_policy()
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
  scalar_numeric_comparison_domain <- tccq_domain_policy(
    "scalar_numeric_comparison",
    result_shape = function(input_types) {
      if (any(vapply(input_types, function(type) type@shape@rank != 0L, logical(1)))) {
        tccq_abort(
          "ops.non_scalar_comparison",
          "The current comparison implementation accepts scalar operands only.",
          phase = "ops",
          path = "comparison.domain"
        )
      }
      tccq_shape()
    },
    attrs = list(operation_family = "comparison")
  )
  numeric_comparison_result_type <- function(input_types, result_shape) {
    unsupported_bases <- setdiff(
      unique(vapply(input_types, function(type) type@base, character(1))),
      c("integer", "double")
    )
    if (length(unsupported_bases) > 0L) {
      tccq_abort(
        "ops.unsupported_comparison_type",
        "The current comparison implementation accepts integer and double operands.",
        phase = "ops",
        path = "comparison.type",
        data = list(base = unsupported_bases)
      )
    }
    tccq_type("logical", result_shape)
  }
  logical_result_type <- function(input_types, result_shape) {
    if (any(vapply(input_types, function(type) !identical(type@base, "logical"), logical(1)))) {
      tccq_abort(
        "ops.unsupported_logical_type",
        "Logical operations require logical inputs.",
        phase = "ops",
        path = "logical.type",
        data = list(types = input_types)
      )
    }
    tccq_type("logical", result_shape)
  }
  missing_result_type <- function(input_types, result_shape) {
    if (length(input_types) != 1L || !identical(input_types[[1L]]@base, "double")) {
      tccq_abort(
        "ops.unsupported_missing_type",
        "The current missing-value predicate requires one double input.",
        phase = "ops",
        path = "missing.type",
        data = list(types = input_types)
      )
    }
    tccq_type("logical", result_shape)
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
    "<" = tccq_elementwise_spec(
      "<",
      2L,
      numeric_comparison_result_type,
      domain_policy = scalar_numeric_comparison_domain
    ),
    "<=" = tccq_elementwise_spec(
      "<=",
      2L,
      numeric_comparison_result_type,
      domain_policy = scalar_numeric_comparison_domain
    ),
    ">" = tccq_elementwise_spec(
      ">",
      2L,
      numeric_comparison_result_type,
      domain_policy = scalar_numeric_comparison_domain
    ),
    ">=" = tccq_elementwise_spec(
      ">=",
      2L,
      numeric_comparison_result_type,
      domain_policy = scalar_numeric_comparison_domain
    ),
    "==" = tccq_elementwise_spec(
      "==",
      2L,
      numeric_comparison_result_type,
      domain_policy = scalar_numeric_comparison_domain
    ),
    "!=" = tccq_elementwise_spec(
      "!=",
      2L,
      numeric_comparison_result_type,
      domain_policy = scalar_numeric_comparison_domain
    ),
    "!" = tccq_elementwise_spec(
      "!",
      1L,
      logical_result_type,
      domain_policy = elementwise_domain_policy
    ),
    "&" = tccq_elementwise_spec(
      "&",
      2L,
      logical_result_type,
      domain_policy = elementwise_domain_policy
    ),
    "|" = tccq_elementwise_spec(
      "|",
      2L,
      logical_result_type,
      domain_policy = elementwise_domain_policy
    ),
    "is.na" = tccq_elementwise_spec(
      "is.na",
      1L,
      missing_result_type,
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
  elementwise_effects <- list(
    "+" = tccq_effect(reads = TRUE, may_warn = TRUE),
    "-" = tccq_effect(reads = TRUE, may_warn = TRUE),
    "*" = tccq_effect(reads = TRUE, may_warn = TRUE),
    "/" = tccq_effect(reads = TRUE),
    "^" = tccq_effect(reads = TRUE),
    "<" = tccq_effect(reads = TRUE),
    "<=" = tccq_effect(reads = TRUE),
    ">" = tccq_effect(reads = TRUE),
    ">=" = tccq_effect(reads = TRUE),
    "==" = tccq_effect(reads = TRUE),
    "!=" = tccq_effect(reads = TRUE),
    "!" = tccq_effect(reads = TRUE),
    "&" = tccq_effect(reads = TRUE),
    "|" = tccq_effect(reads = TRUE),
    "is.na" = tccq_effect(reads = TRUE),
    sqrt = tccq_effect(reads = TRUE, may_warn = TRUE),
    exp = tccq_effect(reads = TRUE)
  )
  sum_identity <- function(type) {
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
  }
  numeric_axis_reduction_type <- function(input_types, result_shape) {
    input_type <- input_types[[1L]]
    if (!input_type@base %in% c("integer", "double")) {
      tccq_abort(
        "ops.unsupported_axis_reduction_type",
        "Axis reductions currently support integer and double inputs.",
        phase = "ops",
        path = "axis_reduction.type",
        data = list(base = input_type@base)
      )
    }
    tccq_type("double", result_shape)
  }
  axis_reduction_domain_policy <- function(name, kept_axis) {
    tccq_domain_policy(
      name,
      result_shape = function(input_types) {
        input_shape <- input_types[[1L]]@shape
        if (input_shape@rank != 2L) {
          tccq_abort(
            "ops.unsupported_axis_reduction_rank",
            "The default axis reductions currently require rank-2 inputs.",
            phase = "ops",
            path = "axis_reduction.shape",
            data = list(rank = input_shape@rank)
          )
        }
        tccq_shape(list(input_shape@dims[[kept_axis]]))
      }
    )
  }
  base_sum_reduction <- tccq_reduction_spec(
    "sum",
    identity = sum_identity,
    combine_op = "+"
  )
  base_mean_reduction <- tccq_reduction_spec(
    "mean",
    identity = sum_identity,
    combine_op = "+",
    finalize_op = "/"
  )
  column_mean_reduction <- tccq_reduction_spec(
    "mean",
    identity = sum_identity,
    combine_op = "+",
    finalize_op = "/",
    signature = tccq_op_signature(
      "colMeans",
      1L,
      result_type = numeric_axis_reduction_type,
      domain_policy = axis_reduction_domain_policy("axis_reduce_columns_mean", kept_axis = 2L)
    ),
    attrs = list(reduction_axes = 1L, kept_axes = 2L, axis_kind = "columns")
  )
  row_mean_reduction <- tccq_reduction_spec(
    "mean",
    identity = sum_identity,
    combine_op = "+",
    finalize_op = "/",
    signature = tccq_op_signature(
      "rowMeans",
      1L,
      result_type = numeric_axis_reduction_type,
      domain_policy = axis_reduction_domain_policy("axis_reduce_rows_mean", kept_axis = 1L)
    ),
    attrs = list(reduction_axes = 2L, kept_axes = 1L, axis_kind = "rows")
  )
  contraction_domain_policy <- function(name, op_name, contract_dims) {
    left_contract <- as.integer(contract_dims[[1L]])
    right_contract <- as.integer(contract_dims[[2L]])
    tccq_domain_policy(
      name,
      result_shape = function(input_types) {
        left_shape <- input_types[[1L]]@shape
        right_shape <- input_types[[2L]]@shape
        rank_supported <- left_shape@rank == 2L &&
          right_shape@rank %in% c(1L, 2L) &&
          !(right_shape@rank == 1L && right_contract != 1L)
        if (!rank_supported) {
          tccq_abort(
            "ops.unsupported_contraction_rank",
            sprintf(
              "`%s` currently contracts a rank-2 input with a rank-1 or rank-2 input.",
              op_name
            ),
            phase = "ops",
            path = "domain_policy.result_shape",
            data = list(left_rank = left_shape@rank, right_rank = right_shape@rank)
          )
        }
        if (!tccq_dim_equal(left_shape@dims[[left_contract]], right_shape@dims[[right_contract]])) {
          tccq_abort(
            "ops.incompatible_contraction_dims",
            sprintf("`%s` inputs must agree on the contracted dimension.", op_name),
            phase = "ops",
            path = "domain_policy.result_shape",
            data = list(
              left = left_shape@dims[[left_contract]]@label,
              right = right_shape@dims[[right_contract]]@label
            )
          )
        }
        left_free <- setdiff(1:2, left_contract)
        if (right_shape@rank == 1L) {
          return(tccq_shape(list(left_shape@dims[[left_free]])))
        }
        right_free <- setdiff(1:2, right_contract)
        tccq_shape(list(left_shape@dims[[left_free]], right_shape@dims[[right_free]]))
      }
    )
  }
  contraction_result_type <- function(input_types, result_shape) {
    unsupported_bases <- setdiff(
      unique(vapply(input_types, function(type) type@base, character(1))),
      c("integer", "double")
    )
    if (length(unsupported_bases) > 0L) {
      tccq_abort(
        "ops.unsupported_contraction_type",
        "Contractions currently support integer and double inputs.",
        phase = "ops",
        path = "contraction.type",
        data = list(base = unsupported_bases)
      )
    }
    tccq_type("double", result_shape)
  }
  new_contraction <- function(op_name, policy_name, contract_dims) {
    tccq_contraction_spec(
      op_name,
      signature = tccq_op_signature(
        op_name,
        2L,
        result_type = contraction_result_type,
        domain_policy = contraction_domain_policy(policy_name, op_name, contract_dims)
      ),
      reducer = base_sum_reduction,
      combine_op = "*",
      attrs = list(contract_dims = as.integer(contract_dims))
    )
  }
  matmul_contraction <- new_contraction("%*%", "contract_inner_dim", c(2L, 1L))
  crossprod_contraction <- new_contraction("crossprod", "contract_first_dims", c(1L, 1L))
  tcrossprod_contraction <- new_contraction("tcrossprod", "contract_second_dims", c(2L, 2L))
  column_sum_reduction <- tccq_reduction_spec(
    "sum",
    identity = sum_identity,
    combine_op = "+",
    signature = tccq_op_signature(
      "colSums",
      1L,
      result_type = numeric_axis_reduction_type,
      domain_policy = axis_reduction_domain_policy("axis_reduce_columns", kept_axis = 2L)
    ),
    attrs = list(reduction_axes = 1L, kept_axes = 2L, axis_kind = "columns")
  )
  row_sum_reduction <- tccq_reduction_spec(
    "sum",
    identity = sum_identity,
    combine_op = "+",
    signature = tccq_op_signature(
      "rowSums",
      1L,
      result_type = numeric_axis_reduction_type,
      domain_policy = axis_reduction_domain_policy("axis_reduce_rows", kept_axis = 1L)
    ),
    attrs = list(reduction_axes = 2L, kept_axes = 1L, axis_kind = "rows")
  )
  which_max_signature <- tccq_op_signature(
    "which.max",
    1L,
    result_type = function(input_types, result_shape) {
      input_type <- input_types[[1L]]
      if (input_type@shape@rank != 1L || !identical(input_type@base, "double")) {
        tccq_abort(
          "ops.unsupported_argument_reduction_type",
          "`which.max` currently requires one rank-1 double input.",
          phase = "ops",
          path = "argument_reduction.type",
          data = list(type = input_type)
        )
      }
      tccq_type("integer", result_shape)
    },
    domain_policy = tccq_domain_policy(
      "argument_reduction_scalar_result",
      result_shape = function(input_types) tccq_shape()
    )
  )
  which_max_reduction <- tccq_arg_reduction_spec(
    "which.max",
    signature = which_max_signature
  )
  seq_len_iteration <- tccq_iteration_spec(
    "seq_len",
    signature = tccq_op_signature(
      "seq_len",
      1L,
      result_type = function(input_types) {
        input_type <- input_types[[1L]]
        if (
          input_type@shape@rank != 0L ||
            !input_type@base %in% c("integer", "double")
        ) {
          tccq_abort(
            "ops.invalid_sequence_extent_type",
            "`seq_len` iteration requires one scalar integer or double extent.",
            phase = "ops",
            path = "iteration.extent",
            data = list(type = input_type)
          )
        }
        tccq_type("integer", tccq_shape(tccq_dim_unknown()))
      }
    ),
    extent_arg = 1L,
    start = 1L
  )
  atomic_subscript <- tccq_subscript_spec(
    "atomic_element",
    signature = tccq_op_signature(
      "[",
      tccq_arity(minimum = 2L),
      result_type = function(input_types, result_shape) {
        source_type <- input_types[[1L]]
        selector_types <- input_types[-1L]
        if (
          source_type@shape@rank == 0L ||
            source_type@shape@rank != length(selector_types) ||
            !all(vapply(selector_types, function(selector_type) {
              selector_type@shape@rank == 0L &&
                identical(selector_type@base, "integer")
            }, logical(1)))
        ) {
          tccq_abort(
            "ops.invalid_subscript_types",
            "The proven subscript requires one scalar integer selector per source axis.",
            phase = "ops",
            path = "subscript.type",
            data = list(source = source_type, selectors = selector_types)
          )
        }
        tccq_type(source_type@base, result_shape)
      },
      domain_policy = tccq_domain_policy(
        "atomic_element_scalar_result",
        result_shape = function(input_types) tccq_shape()
      )
    )
  )
  language_ops <- c(
    "{", "(", "<-", "<<-", "->", "->>", "=",
    "if", "for", "while", "repeat", "break", "next", "switch", "function",
    "[", "[[", "$", "@", "[<-", "[[<-", "$<-", "@<-", ":",
    "declare", "type", TCCQ_BASE_TYPES
  )
  registry <- tccq_op_registry(c(
    list(tccq_op_impl(
      "[",
      target = "native",
      region_kind = "kernel",
      effect = tccq_effect(reads = TRUE),
      supports = function(call, context) {
        !is.na(call@arity) &&
          call@arity >= 2L &&
          length(call@argument_names) == call@arity &&
          !any(nzchar(call@argument_names)) &&
          is.call(call@expr) &&
          length(call@expr) == call@arity + 1L &&
          all(vapply(as.list(call@expr)[-(1:2)], function(selector) {
            if (!is.symbol(selector)) {
              return(FALSE)
            }
            selector_name <- as.character(selector)
            length(selector_name) == 1L && nzchar(selector_name)
          }, logical(1)))
      },
      subscript = atomic_subscript
    )),
    lapply(language_ops, function(op) {
      tccq_op_impl(op, target = "r_language", pure = FALSE)
    }),
    lapply(names(elementwise_specs), function(op) {
      tccq_op_impl(
        op,
        target = "pure_c",
        region_kind = "kernel",
        effect = elementwise_effects[[op]],
        render = scalar_renderers[[op]],
        elementwise = elementwise_specs[[op]]
      )
    }),
    list(
      tccq_op_impl(
        "seq_len",
        target = "native",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        iteration = seq_len_iteration
      ),
      tccq_op_impl(
        "sum",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        reduction = base_sum_reduction
      ),
      tccq_op_impl(
        "colSums",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        reduction = column_sum_reduction
      ),
      tccq_op_impl(
        "rowSums",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        reduction = row_sum_reduction
      ),
      tccq_op_impl(
        "mean",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        reduction = base_mean_reduction
      ),
      tccq_op_impl(
        "colMeans",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        reduction = column_mean_reduction
      ),
      tccq_op_impl(
        "rowMeans",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        reduction = row_mean_reduction
      ),
      tccq_op_impl(
        "which.max",
        target = "native",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE, may_error = TRUE),
        reduction = which_max_reduction
      ),
      tccq_op_impl(
        "%*%",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        contraction = matmul_contraction
      ),
      tccq_op_impl(
        "crossprod",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        contraction = crossprod_contraction
      ),
      tccq_op_impl(
        "tcrossprod",
        target = "pure_c",
        region_kind = "kernel",
        effect = tccq_effect(reads = TRUE),
        contraction = tcrossprod_contraction
      )
    )
  ))
  the_default_op_registry$registry <- registry
  registry
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
    call_name <- tccq_call_name(node)
    calls[[length(calls) + 1L]] <<- tccq_call(
      call_name,
      node,
      "ast",
      id = make_id()
    )
    if (
      call_name %in% c("<-", "<<-", "=") &&
        length(node) == 3L &&
        is.call(node[[2L]])
    ) {
      replacement_target <- node[[2L]]
      target_call_name <- tccq_call_name(replacement_target)
      replacement_args <- as.list(replacement_target)[-1L]
      replacement_arg_names <- names(replacement_args)
      if (is.null(replacement_arg_names)) {
        replacement_arg_names <- rep("", length(replacement_args))
      }
      replacement_arg_names[is.na(replacement_arg_names)] <- ""
      target_symbol <- if (length(replacement_target) >= 2L && is.symbol(replacement_target[[2L]])) {
        as.character(replacement_target[[2L]])
      } else {
        ""
      }
      calls[[length(calls) + 1L]] <<- tccq_call(
        paste0(target_call_name, "<-"),
        node,
        "assignment_rewrite",
        id = make_id(),
        kind = "replacement",
        arity = as.integer(length(replacement_args) + 1L),
        argument_names = c(replacement_arg_names, "value"),
        attrs = list(
          assignment = call_name,
          target_call = target_call_name,
          target_symbol = target_symbol,
          syntax = "complex_assignment"
        )
      )
    }
    children <- as.list(node)[-1L]
    for (child_index in seq_along(children)) {
      if (identical(children[[child_index]], quote(expr = ))) {
        next
      }
      walk(children[[child_index]])
    }
    NULL
  }
  walk(expr)
  observed_names <- unique(vapply(calls, function(call) call@name, character(1)))
  for (name in setdiff(global_calls, observed_names)) {
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

  # Trait membership is asserted once at the registry constructor boundary,
  # so resolution dispatches the trait generic directly, and the op-name
  # index narrows the scan to this call's candidates plus wildcards.
  candidate_positions <- if (length(registry@op_index) > 0L) {
    sort(c(
      registry@op_index[[call@name]],
      registry@op_index[[TCCQ_ANY_OP]]
    ))
  } else {
    seq_along(registry@implementations)
  }
  for (position in candidate_positions) {
    implementation <- registry@implementations[[position]]
    if (isTRUE(tccq_op_supports(implementation, call, context))) {
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
