TCCQ_BASE_TYPES <- c(
  "logical",
  "integer",
  "double",
  "complex",
  "character",
  "raw",
  "buffer"
)
TCCQ_DIM_KINDS <- c("constant", "symbol", "affine", "unknown")
TCCQ_AXIS_ROLES <- c("map", "reduce")
TCCQ_LITERAL_KINDS <- c("finite", "na", "nan", "pos_inf", "neg_inf")
TCCQ_ACCESS_KINDS <- c("identity", "scalar", "broadcast", "slice", "recycle", "transpose", "custom")
TCCQ_FUSION_KINDS <- c("map", "map_reduce", "axis_reduce", "contract", "stencil", "custom")
TCCQ_REGION_KINDS <- c("host", "kernel", "parallel", "device")
TCCQ_MEMORY_SPACES <- c("r", "host", "device", "opaque")
TCCQ_STORAGE_ROLES <- c("input", "literal", "temporary", "output")
TCCQ_CALL_KINDS <- c(
  "call",
  "operator",
  "assignment",
  "control",
  "block",
  "grouping",
  "index",
  "replacement",
  "function_definition",
  "unknown"
)
TCCQ_EVALUATOR_KINDS <- c(
  "special",
  "builtin",
  "closure",
  "compiler_directive",
  "unknown"
)
TCCQ_FORCING_POLICIES <- c(
  "eager",
  "lazy",
  "special",
  "replacement",
  "compiler",
  "unknown"
)
TCCQ_DISPATCH_KINDS <- c(
  "none",
  "group_generic",
  "s3",
  "s3_primitive",
  "replacement",
  "unknown"
)

#' R call observed by the frontend
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
TccqCall <- S7::new_class(
  "TccqCall",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    name = S7::class_character,
    expr = S7::class_any,
    origin = S7::class_character,
    kind = S7::class_character,
    arity = S7::class_integer,
    argument_names = S7::class_character,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id)) {
      problems <- c(problems, "@id must be a single string")
    }
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (length(self@origin) != 1L || is.na(self@origin) || !nzchar(self@origin)) {
      problems <- c(problems, "@origin must be a single non-empty string")
    }
    if (
      length(self@kind) != 1L ||
        is.na(self@kind) ||
        !self@kind %in% TCCQ_CALL_KINDS
    ) {
      problems <- c(problems, "@kind must be one supported call kind")
    }
    if (length(self@arity) != 1L) {
      problems <- c(problems, "@arity must be a single integer or NA")
    }
    if (anyNA(self@argument_names)) {
      problems <- c(problems, "@argument_names must not contain NA")
    }
    if (length(problems) > 0L) problems
  }
)

#' R call evaluator facts
#'
#' `TccqCallSemantics` records evaluator-level facts about an observed call. It
#' is not a lowering decision: special forms, builtins, closures, replacement
#' calls, group generics, and unknown calls still need later operation and
#' backend implementations.
#'
#' @param call Observed call.
#' @param evaluator_kind Evaluator kind.
#' @param forcing_policy Promise-forcing policy.
#' @param dispatch_kind Dispatch family.
#' @param lexical_scope Whether lexical closure environment matters.
#' @param replacement Whether this is a replacement-function call.
#' @param control Whether this is a control-form call.
#' @param attrs Structured semantic attributes.
#' @export
TccqCallSemantics <- S7::new_class(
  "TccqCallSemantics",
  package = "tccquickr",
  properties = list(
    call = TccqCall,
    evaluator_kind = S7::class_character,
    forcing_policy = S7::class_character,
    dispatch_kind = S7::class_character,
    lexical_scope = S7::class_logical,
    replacement = S7::class_logical,
    control = S7::class_logical,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (
      length(self@evaluator_kind) != 1L ||
        is.na(self@evaluator_kind) ||
        !self@evaluator_kind %in% TCCQ_EVALUATOR_KINDS
    ) {
      problems <- c(problems, "@evaluator_kind must be one supported evaluator kind")
    }
    if (
      length(self@forcing_policy) != 1L ||
        is.na(self@forcing_policy) ||
        !self@forcing_policy %in% TCCQ_FORCING_POLICIES
    ) {
      problems <- c(problems, "@forcing_policy must be one supported forcing policy")
    }
    if (
      length(self@dispatch_kind) != 1L ||
        is.na(self@dispatch_kind) ||
        !self@dispatch_kind %in% TCCQ_DISPATCH_KINDS
    ) {
      problems <- c(problems, "@dispatch_kind must be one supported dispatch kind")
    }
    if (length(self@lexical_scope) != 1L || is.na(self@lexical_scope)) {
      problems <- c(problems, "@lexical_scope must be a single TRUE/FALSE value")
    }
    if (length(self@replacement) != 1L || is.na(self@replacement)) {
      problems <- c(problems, "@replacement must be a single TRUE/FALSE value")
    }
    if (length(self@control) != 1L || is.na(self@control)) {
      problems <- c(problems, "@control must be a single TRUE/FALSE value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Indexed frontend call facts
#'
#' `TccqCallIndex` is the typed handoff from frontend analysis to later lowering
#' passes. It keeps observed calls and evaluator facts aligned by stable call id.
#'
#' @param calls List of observed calls.
#' @param semantics List of call evaluator facts.
#' @param attrs Structured index attributes.
#' @export
TccqCallIndex <- S7::new_class(
  "TccqCallIndex",
  package = "tccquickr",
  properties = list(
    calls = S7::class_list,
    semantics = S7::class_list,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    calls_are_tccq_calls <- vapply(self@calls, S7::S7_inherits, logical(1), class = TccqCall)
    semantics_are_tccq_call_semantics <- vapply(
      self@semantics,
      S7::S7_inherits,
      logical(1),
      class = TccqCallSemantics
    )
    if (!all(calls_are_tccq_calls)) {
      problems <- c(problems, "@calls must contain only <TccqCall> values")
    }
    if (!all(semantics_are_tccq_call_semantics)) {
      problems <- c(problems, "@semantics must contain only <TccqCallSemantics> values")
    }
    if (all(calls_are_tccq_calls) && all(semantics_are_tccq_call_semantics)) {
      call_ids <- vapply(self@calls, function(x) x@id, character(1))
      semantic_ids <- vapply(self@semantics, function(x) x@call@id, character(1))
      if (length(call_ids) != length(semantic_ids)) {
        problems <- c(problems, "@calls and @semantics must have the same length")
      } else if (any(!nzchar(call_ids)) || anyDuplicated(call_ids)) {
        problems <- c(problems, "@calls ids must be non-empty and unique")
      } else if (!identical(call_ids, semantic_ids)) {
        problems <- c(problems, "@semantics must align one-to-one with @calls by id")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Shape dimension
#'
#' @param kind Dimension kind.
#' @param label Symbolic dimension label.
#' @param value Constant dimension value.
#' @export
TccqDim <- S7::new_class(
  "TccqDim",
  package = "tccquickr",
  properties = list(
    kind = S7::class_character,
    label = S7::class_character,
    value = S7::class_integer
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@kind) != 1L || is.na(self@kind) || !self@kind %in% TCCQ_DIM_KINDS) {
      problems <- c(problems, "@kind must be one supported dimension kind")
    }
    if (length(self@label) != 1L || is.na(self@label)) {
      problems <- c(problems, "@label must be a single string")
    }
    if (length(self@value) != 1L) {
      problems <- c(problems, "@value must be a single integer or NA")
    }
    if (identical(self@kind, "symbol") && !grepl("^[A-Za-z.][A-Za-z0-9_.]*$", self@label)) {
      problems <- c(problems, "@label must be a valid symbolic dimension name for symbol dimensions")
    }
    if (identical(self@kind, "constant") && (is.na(self@value) || self@value < 0L)) {
      problems <- c(problems, "@value must be a non-negative integer for constant dimensions")
    }
    if (identical(self@kind, "affine")) {
      if (!grepl("^[A-Za-z.][A-Za-z0-9_.]*$", self@label)) {
        problems <- c(problems, "@label must be a valid symbolic dimension name for affine dimensions")
      }
      if (is.na(self@value)) {
        problems <- c(problems, "@value must be a signed integer offset for affine dimensions")
      }
    }
    if (identical(self@kind, "unknown") && (!identical(self@label, "") || !is.na(self@value))) {
      problems <- c(problems, "unknown dimensions must have empty @label and NA @value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Shape value
#'
#' @param rank Number of dimensions.
#' @param dims List of `TccqDim` values.
#' @export
TccqShape <- S7::new_class(
  "TccqShape",
  package = "tccquickr",
  properties = list(
    rank = S7::class_integer,
    dims = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    has_valid_rank <- length(self@rank) == 1L && !is.na(self@rank) && self@rank >= 0L
    if (!has_valid_rank) {
      problems <- c(problems, "@rank must be one non-negative integer")
    }
    dims_are_tccq_dims <- vapply(self@dims, S7::S7_inherits, logical(1), class = TccqDim)
    if (!all(dims_are_tccq_dims)) {
      problems <- c(problems, "@dims must contain only <TccqDim> values")
    }
    if (has_valid_rank && length(self@dims) != self@rank) {
      problems <- c(problems, "@dims length must match @rank")
    }
    if (length(problems) > 0L) problems
  }
)

#' Iteration domain
#'
#' A domain is the semantic iteration space for elementwise, reduction, stencil,
#' or tiled work. It is separate from shape so transformed programs can name and
#' reuse iteration spaces directly.
#'
#' @param id Stable domain id.
#' @param shape Domain shape.
#' @param axes Axis names.
#' @param attrs Structured domain attributes.
#' @export
TccqDomain <- S7::new_class(
  "TccqDomain",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    shape = TccqShape,
    axes = S7::class_character,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (length(self@axes) != self@shape@rank || anyNA(self@axes) || any(!nzchar(self@axes))) {
      problems <- c(problems, "@axes must be non-empty names matching @shape rank")
    }
    if (length(problems) > 0L) problems
  }
)

#' Affine index expression
#'
#' An index expression maps one stored-tensor axis into the iteration space of
#' a loop nest as `axis + offset`, or to a constant position when `axis` is
#' empty. It is the typed unit of access maps, so stencil shifts, broadcasts,
#' and transposes are dimension facts rather than printed index strings.
#'
#' @param axis Iteration-axis name, or empty for a constant index.
#' @param offset Signed integer offset (zero-based).
#' @export
TccqIndexExpr <- S7::new_class(
  "TccqIndexExpr",
  package = "tccquickr",
  properties = list(
    axis = S7::class_character,
    offset = S7::class_integer
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@axis) != 1L || is.na(self@axis)) {
      problems <- c(problems, "@axis must be a single string")
    }
    if (length(self@offset) != 1L || is.na(self@offset)) {
      problems <- c(problems, "@offset must be a single integer")
    }
    is_constant_index <- length(self@axis) == 1L && !is.na(self@axis) && !nzchar(self@axis)
    if (is_constant_index && length(self@offset) == 1L && !is.na(self@offset) && self@offset < 0L) {
      problems <- c(problems, "constant index expressions must be non-negative")
    }
    if (length(problems) > 0L) problems
  }
)

#' Loop-nest iteration axis
#'
#' An axis is one dimension of a loop-nest iteration space: a stable name, a
#' typed extent, and a role. `map` axes produce output positions; `reduce` axes
#' fold into an accumulator.
#'
#' @param name Iteration-axis name.
#' @param extent Typed axis extent.
#' @param role Axis role, `map` or `reduce`.
#' @export
TccqLoopAxis <- S7::new_class(
  "TccqLoopAxis",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    extent = TccqDim,
    role = S7::class_character
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (length(self@role) != 1L || is.na(self@role) || !self@role %in% TCCQ_AXIS_ROLES) {
      problems <- c(problems, "@role must be one supported axis role")
    }
    if (identical(self@extent@kind, "unknown")) {
      problems <- c(problems, "@extent must be a constant, symbolic, or affine dimension")
    }
    if (length(problems) > 0L) problems
  }
)

#' Declared compiler type
#'
#' @param base Scalar or storage base type.
#' @param shape Shape value.
#' @export
TccqType <- S7::new_class(
  "TccqType",
  package = "tccquickr",
  properties = list(
    base = S7::class_character,
    shape = TccqShape
  ),
  validator = function(self) {
    if (length(self@base) != 1L || is.na(self@base) || !self@base %in% TCCQ_BASE_TYPES) {
      "@base must be one supported compiler base type"
    }
  }
)

#' Scalar literal value
#'
#' @param kind Literal kind.
#' @param type Scalar type.
#' @param value Literal payload.
#' @export
TccqLiteral <- S7::new_class(
  "TccqLiteral",
  package = "tccquickr",
  properties = list(
    kind = S7::class_character,
    type = TccqType,
    value = S7::class_any
  ),
  validator = function(self) {
    problems <- character()
    has_supported_literal_kind <- length(self@kind) == 1L &&
      !is.na(self@kind) &&
      self@kind %in% TCCQ_LITERAL_KINDS
    literal_kind <- if (has_supported_literal_kind) self@kind else ""
    if (!has_supported_literal_kind) {
      problems <- c(problems, "@kind must be one supported literal kind")
    }
    if (self@type@shape@rank != 0L) {
      problems <- c(problems, "@type must be scalar")
    }
    if (identical(literal_kind, "na") && self@type@base %in% c("raw", "buffer")) {
      problems <- c(problems, "NA literals are not valid for raw or buffer types")
    }
    if (literal_kind %in% c("nan", "pos_inf", "neg_inf") && !identical(self@type@base, "double")) {
      problems <- c(problems, "NaN and infinite literals must be double scalars")
    }
    if (identical(literal_kind, "finite") && length(self@value) != 1L) {
      problems <- c(problems, "finite literals must have length-one payloads")
    }
    if (identical(literal_kind, "finite") && identical(self@type@base, "buffer")) {
      problems <- c(problems, "buffer has no finite scalar literal payload")
    }
    if (identical(literal_kind, "finite") && (is.atomic(self@value) || is.complex(self@value)) && anyNA(self@value)) {
      problems <- c(problems, "finite literals cannot contain NA or NaN payloads")
    }
    if (identical(literal_kind, "finite") && is.numeric(self@value) && is.infinite(self@value)) {
      problems <- c(problems, "finite literals cannot contain infinite payloads")
    }
    if (length(problems) > 0L) problems
  }
)

#' Compiler effect summary
#'
#' @param reads Whether the operation reads program state.
#' @param writes Whether the operation writes program state.
#' @param allocates Whether the operation allocates storage.
#' @param boundary Whether the operation crosses an unsupported boundary.
#' @param may_error Whether the operation may signal at runtime.
#' @param may_warn Whether the operation may warn at runtime.
#' @export
TccqEffect <- S7::new_class(
  "TccqEffect",
  package = "tccquickr",
  properties = list(
    reads = S7::class_logical,
    writes = S7::class_logical,
    allocates = S7::class_logical,
    boundary = S7::class_logical,
    may_error = S7::class_logical,
    may_warn = S7::class_logical
  ),
  validator = function(self) {
    problems <- character()
    values <- list(
      reads = self@reads,
      writes = self@writes,
      allocates = self@allocates,
      boundary = self@boundary,
      may_error = self@may_error,
      may_warn = self@may_warn
    )
    for (prop in names(values)) {
      value <- values[[prop]]
      if (length(value) != 1L || is.na(value)) {
        problems <- c(problems, sprintf("@%s must be a single TRUE/FALSE value", prop))
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Program binding
#'
#' @param name Binding name.
#' @param type Binding type.
#' @param mutable Whether the binding may be mutated.
#' @export
TccqBinding <- S7::new_class(
  "TccqBinding",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    type = TccqType,
    mutable = S7::class_logical
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    if (length(self@mutable) != 1L || is.na(self@mutable)) {
      problems <- c(problems, "@mutable must be a single TRUE/FALSE value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Local value binding
#'
#' A local binding records the value produced by one top-level assignment in
#' the declared subset. Its statement position is a semantic definition
#' boundary used by scheduling; it is not source-printer metadata.
#'
#' @inheritParams TccqBinding
#' @param value_id Lowered value bound to the local name.
#' @param statement_index One-based executable statement position defining the
#'   binding.
#' @export
TccqLocalBinding <- S7::new_class(
  "TccqLocalBinding",
  package = "tccquickr",
  parent = TccqBinding,
  properties = list(
    value_id = S7::class_character,
    statement_index = S7::class_integer
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (
      length(self@statement_index) != 1L ||
        is.na(self@statement_index) ||
        self@statement_index < 1L
    ) {
      problems <- c(problems, "@statement_index must be one positive integer")
    }
    if (length(problems) > 0L) problems
  }
)

#' Scheduled R evaluation
#'
#' One step records one executable top-level form after declarations have been
#' removed. Assignment steps carry the local binding they define; expression
#' statements carry no binding. The effect summarizes the complete value graph
#' evaluated by the step.
#'
#' @param value_id Lowered value evaluated by this step.
#' @param statement_index One-based executable statement position.
#' @param binding Optional local binding defined by the evaluation.
#' @param uses Local bindings read by the evaluation.
#' @param effect Effect of evaluating the complete step.
#' @export
TccqEvaluationStep <- S7::new_class(
  "TccqEvaluationStep",
  package = "tccquickr",
  properties = list(
    value_id = S7::class_character,
    statement_index = S7::class_integer,
    binding = S7::new_union(NULL, TccqLocalBinding),
    uses = S7::class_list,
    effect = TccqEffect
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (
      length(self@statement_index) != 1L ||
        is.na(self@statement_index) ||
        self@statement_index < 1L
    ) {
      problems <- c(problems, "@statement_index must be one positive integer")
    }
    if (!is.null(self@binding)) {
      if (!identical(self@binding@statement_index, self@statement_index)) {
        problems <- c(problems, "@binding and step must have the same statement position")
      }
    }
    uses_are_bindings <- vapply(
      self@uses,
      S7::S7_inherits,
      logical(1),
      class = TccqLocalBinding
    )
    if (!all(uses_are_bindings)) {
      problems <- c(problems, "@uses must contain only <TccqLocalBinding> values")
    }
    if (all(uses_are_bindings)) {
      use_names <- vapply(self@uses, function(binding) binding@name, character(1))
      if (anyDuplicated(use_names)) {
        problems <- c(problems, "@uses must identify each local binding once")
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Ordered program evaluation schedule
#'
#' The schedule is the semantic owner of top-level R evaluation order. Its
#' steps are contiguous after declarations are removed, and its final step
#' produces the program result. Loop-nest planning consumes this value rather
#' than reconstructing statement order from value ids or source text.
#'
#' @param steps Ordered `TccqEvaluationStep` values.
#' @param result Result value id produced by the final step.
#' @export
TccqProgramSchedule <- S7::new_class(
  "TccqProgramSchedule",
  package = "tccquickr",
  properties = list(
    steps = S7::class_list,
    result = S7::class_character
  ),
  validator = function(self) {
    problems <- character()
    steps_are_typed <- vapply(
      self@steps,
      S7::S7_inherits,
      logical(1),
      class = TccqEvaluationStep
    )
    if (!all(steps_are_typed)) {
      problems <- c(problems, "@steps must contain only <TccqEvaluationStep> values")
    }
    if (length(self@result) != 1L || is.na(self@result) || !nzchar(self@result)) {
      problems <- c(problems, "@result must be a single non-empty value id")
    }
    if (all(steps_are_typed)) {
      if (length(self@steps) == 0L) {
        problems <- c(problems, "@steps must contain the result evaluation")
      } else {
        statement_indices <- vapply(
          self@steps,
          function(step) step@statement_index,
          integer(1)
        )
        if (!identical(statement_indices, seq_along(self@steps))) {
          problems <- c(problems, "@steps must have contiguous statement positions in order")
        }
        if (!identical(self@steps[[length(self@steps)]]@value_id, self@result)) {
          problems <- c(problems, "the final step must produce @result")
        }
        bindings <- Filter(
          function(step) S7::S7_inherits(step@binding, TccqLocalBinding),
          self@steps
        )
        binding_names <- vapply(
          bindings,
          function(step) step@binding@name,
          character(1)
        )
        if (anyDuplicated(binding_names)) {
          problems <- c(problems, "scheduled local bindings must have unique names")
        }
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' IR value
#'
#' @param id Stable value id.
#' @param op Operation name.
#' @param inputs Input value ids or references.
#' @param type Result type.
#' @param effect Effect summary.
#' @param attrs Structured operation attributes.
#' @export
TccqValue <- S7::new_class(
  "TccqValue",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    op = S7::class_character,
    inputs = S7::class_list,
    type = TccqType,
    effect = TccqEffect,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (length(self@op) != 1L || is.na(self@op) || !nzchar(self@op)) {
      problems <- c(problems, "@op must be a single non-empty string")
    }
    if (length(problems) > 0L) problems
  }
)

#' Local binding reference
#'
#' A binding reference is a lexical read of an already evaluated local. It
#' retains the exact binding identity while pointing at the binding's storage
#' value. Expression and effect traversal stop at this node; reading a local
#' must not re-evaluate the graph that defined it.
#'
#' @inheritParams TccqValue
#' @param binding Local binding being read.
#' @export
TccqBindingReference <- S7::new_class(
  "TccqBindingReference",
  package = "tccquickr",
  parent = TccqValue,
  properties = list(binding = TccqLocalBinding),
  validator = function(self) {
    problems <- character()
    if (!identical(self@op, "binding_reference")) {
      problems <- c(problems, "binding references must use the `binding_reference` operation")
    }
    if (!identical(self@inputs, list(self@binding@value_id))) {
      problems <- c(problems, "@inputs must identify the bound storage value")
    }
    if (!identical(self@type, self@binding@type)) {
      problems <- c(problems, "@type must match the referenced binding")
    }
    if (
      !isTRUE(self@effect@reads) ||
        isTRUE(self@effect@writes) ||
        isTRUE(self@effect@allocates) ||
        isTRUE(self@effect@boundary) ||
        isTRUE(self@effect@may_error) ||
        isTRUE(self@effect@may_warn)
    ) {
      problems <- c(problems, "binding references must be read-only effects")
    }
    if (length(problems) > 0L) problems
  }
)

#' Lazy conditional IR value
#'
#' `TccqBranch` is the value-level representation of R's `if` special form.
#' Its three incoming value ids are analyzed eagerly by the compiler, but the
#' recorded special-form semantics require generated code to evaluate exactly
#' one result arm at runtime.
#'
#' @inheritParams TccqValue
#' @param condition Scalar logical condition value id.
#' @param consequent Value id produced when the condition is true.
#' @param alternative Value id produced when the condition is false.
#' @param semantics Evaluator facts for the originating `if` call.
#' @export
TccqBranch <- S7::new_class(
  "TccqBranch",
  package = "tccquickr",
  parent = TccqValue,
  properties = list(
    condition = S7::class_character,
    consequent = S7::class_character,
    alternative = S7::class_character,
    semantics = TccqCallSemantics
  ),
  validator = function(self) {
    problems <- character()
    incoming_ids <- c(self@condition, self@consequent, self@alternative)
    if (length(incoming_ids) != 3L || anyNA(incoming_ids) || any(!nzchar(incoming_ids))) {
      problems <- c(problems, "branch incoming value ids must be non-empty strings")
    }
    if (!identical(self@op, "if")) {
      problems <- c(problems, "branch values must use the `if` operation")
    }
    if (!identical(self@inputs, as.list(incoming_ids))) {
      problems <- c(problems, "@inputs must match condition, consequent, and alternative ids")
    }
    if (
      !identical(self@semantics@call@name, "if") ||
        !isTRUE(self@semantics@control) ||
        !identical(self@semantics@forcing_policy, "special")
    ) {
      problems <- c(problems, "@semantics must describe the R `if` special form")
    }
    if (length(problems) > 0L) problems
  }
)

#' Domain access mapping
#'
#' Access describes how a value is read or written over a domain. It is the
#' schema-level place for identity indexing, scalar broadcast, slicing,
#' transpose, and later richer affine/custom maps.
#'
#' @param value_id Referenced value id.
#' @param domain Access domain.
#' @param kind Access kind.
#' @param index_map Structured index-map payload.
#' @param attrs Structured access attributes.
#' @export
TccqAccess <- S7::new_class(
  "TccqAccess",
  package = "tccquickr",
  properties = list(
    value_id = S7::class_character,
    domain = TccqDomain,
    kind = S7::class_character,
    index_map = S7::class_list,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (length(self@kind) != 1L || is.na(self@kind) || !self@kind %in% TCCQ_ACCESS_KINDS) {
      problems <- c(problems, "@kind must be one supported access kind")
    }
    index_map_is_typed <- vapply(
      self@index_map,
      S7::S7_inherits,
      logical(1),
      class = TccqIndexExpr
    )
    if (!all(index_map_is_typed)) {
      problems <- c(problems, "@index_map must contain only <TccqIndexExpr> values")
    }
    index_axes <- vapply(self@index_map, function(index) index@axis, character(1))
    referenced_axes <- setdiff(unique(index_axes), "")
    if (length(setdiff(referenced_axes, self@domain@axes)) > 0L) {
      problems <- c(problems, "@index_map may only reference @domain axes")
    }
    if (length(problems) > 0L) problems
  }
)

#' Fusion group
#'
#' A fusion group is a typed plan candidate, not emitted code. It groups values
#' that may share one domain and region when operation implementations, effects,
#' access maps, layout, and backend constraints allow it.
#'
#' @param id Stable fusion-group id.
#' @param kind Fusion kind.
#' @param domain Shared iteration domain.
#' @param values List of IR values in the group.
#' @param outputs Output value ids.
#' @param accesses List of access mappings.
#' @param region_kind Candidate execution region kind, or `any`.
#' @param target Candidate implementation target, or `any`.
#' @param effect Effect summary for the fused group.
#' @param contract Typed fusion operation/storage contract.
#' @param attrs Structured fusion attributes.
#' @export
TccqFusionGroup <- S7::new_class(
  "TccqFusionGroup",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    kind = S7::class_character,
    domain = TccqDomain,
    values = S7::class_list,
    outputs = S7::class_character,
    accesses = S7::class_list,
    region_kind = S7::class_character,
    target = S7::class_character,
    effect = TccqEffect,
    contract = S7::class_any,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    has_supported_fusion_kind <- length(self@kind) == 1L &&
      !is.na(self@kind) &&
      self@kind %in% TCCQ_FUSION_KINDS
    has_supported_region_kind <- length(self@region_kind) == 1L &&
      !is.na(self@region_kind) &&
      self@region_kind %in% c("any", TCCQ_REGION_KINDS)
    if (!has_supported_fusion_kind) {
      problems <- c(problems, "@kind must be one supported fusion kind")
    }
    values_are_tccq_values <- vapply(self@values, S7::S7_inherits, logical(1), class = TccqValue)
    accesses_are_tccq_accesses <- vapply(self@accesses, S7::S7_inherits, logical(1), class = TccqAccess)
    if (!all(values_are_tccq_values)) {
      problems <- c(problems, "@values must contain only <TccqValue> values")
    }
    if (anyNA(self@outputs) || any(!nzchar(self@outputs))) {
      problems <- c(problems, "@outputs must contain non-empty value ids")
    }
    if (!all(accesses_are_tccq_accesses)) {
      problems <- c(problems, "@accesses must contain only <TccqAccess> values")
    }
    if (!has_supported_region_kind) {
      problems <- c(problems, "@region_kind must be any or one supported region kind")
    }
    if (length(self@target) != 1L || is.na(self@target) || !nzchar(self@target)) {
      problems <- c(problems, "@target must be a single non-empty string")
    }
    if (has_supported_region_kind && self@region_kind %in% c("kernel", "parallel", "device") && isTRUE(self@effect@boundary)) {
      problems <- c(problems, "kernel, parallel, and device fusion groups cannot contain boundary effects")
    }
    if (!is.null(self@contract) && !S7::S7_inherits(self@contract, TccqFusionContract)) {
      problems <- c(problems, "@contract must be NULL or a <TccqFusionContract> value")
    }
    if (
      !is.null(self@contract) &&
        S7::S7_inherits(self@contract, TccqFusionContract) &&
        !identical(self@contract@fusion_kind, self@kind)
    ) {
      problems <- c(problems, "@contract fusion kind must match @kind")
    }
    if (
      !is.null(self@contract) &&
        S7::S7_inherits(self@contract, TccqFusionContract) &&
        (!self@contract@result_value@id %in% self@outputs ||
          !self@contract@result_value@id %in% vapply(self@values, function(value) value@id, character(1)))
    ) {
      problems <- c(problems, "@contract result value must be an output value of the fusion group")
    }
    if (length(problems) > 0L) problems
  }
)

#' Executable code region
#'
#' Regions classify a group of IR values by where they may run and whether they
#' may touch the R C API. `kernel`, `parallel`, and `device` regions are
#' intended to be R-API-free.
#'
#' @param id Stable region id.
#' @param kind Region kind.
#' @param values List of IR values in the region.
#' @param fusion_groups List of fusion groups in the region.
#' @param effect Region effect summary.
#' @param memory_space Dominant memory space for the region.
#' @param touches_rapi Whether the region may call into the R C API.
#' @param attrs Structured region attributes.
#' @export
TccqRegion <- S7::new_class(
  "TccqRegion",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    kind = S7::class_character,
    values = S7::class_list,
    fusion_groups = S7::class_list,
    effect = TccqEffect,
    memory_space = S7::class_character,
    touches_rapi = S7::class_logical,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    has_supported_region_kind <- length(self@kind) == 1L &&
      !is.na(self@kind) &&
      self@kind %in% TCCQ_REGION_KINDS
    has_supported_memory_space <- length(self@memory_space) == 1L &&
      !is.na(self@memory_space) &&
      self@memory_space %in% TCCQ_MEMORY_SPACES
    if (!has_supported_region_kind) {
      problems <- c(problems, "@kind must be one supported region kind")
    }
    values_are_tccq_values <- vapply(self@values, S7::S7_inherits, logical(1), class = TccqValue)
    fusion_groups_are_tccq_fusion_groups <- vapply(
      self@fusion_groups,
      S7::S7_inherits,
      logical(1),
      class = TccqFusionGroup
    )
    if (!all(values_are_tccq_values)) {
      problems <- c(problems, "@values must contain only <TccqValue> values")
    }
    if (!all(fusion_groups_are_tccq_fusion_groups)) {
      problems <- c(problems, "@fusion_groups must contain only <TccqFusionGroup> values")
    }
    if (!has_supported_memory_space) {
      problems <- c(problems, "@memory_space must be one supported memory space")
    }
    if (length(self@touches_rapi) != 1L || is.na(self@touches_rapi)) {
      problems <- c(problems, "@touches_rapi must be a single TRUE/FALSE value")
    }
    if (has_supported_region_kind && self@kind %in% c("kernel", "parallel", "device") && isTRUE(self@touches_rapi)) {
      problems <- c(problems, "kernel, parallel, and device regions must not touch the R C API")
    }
    if (has_supported_region_kind && self@kind %in% c("kernel", "parallel", "device") && isTRUE(self@effect@boundary)) {
      problems <- c(problems, "kernel, parallel, and device regions cannot contain boundary effects")
    }
    if (has_supported_region_kind && has_supported_memory_space && identical(self@kind, "device") && !identical(self@memory_space, "device")) {
      problems <- c(problems, "device regions must use device memory space")
    }
    if (has_supported_region_kind && has_supported_memory_space && identical(self@kind, "host") && identical(self@memory_space, "device")) {
      problems <- c(problems, "host regions cannot directly use device memory space")
    }
    if (length(problems) > 0L) problems
  }
)

#' Storage lifetime interval
#'
#' A storage lifetime records the definition and last-use positions used by
#' storage reuse planning. It is a typed optimization fact, not source-printer
#' metadata.
#'
#' @param value_id Value whose lifetime is described.
#' @param defined_at Linear definition position in the lowered value stream.
#' @param last_used_at Last position where the value must remain live.
#' @export
TccqStorageLifetime <- S7::new_class(
  "TccqStorageLifetime",
  package = "tccquickr",
  properties = list(
    value_id = S7::class_character,
    defined_at = S7::class_integer,
    last_used_at = S7::class_integer
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (length(self@defined_at) != 1L || is.na(self@defined_at) || self@defined_at < 1L) {
      problems <- c(problems, "@defined_at must be a positive integer position")
    }
    if (length(self@last_used_at) != 1L || is.na(self@last_used_at) || self@last_used_at < self@defined_at) {
      problems <- c(problems, "@last_used_at must be greater than or equal to @defined_at")
    }
    if (length(problems) > 0L) problems
  }
)

#' Storage slot
#'
#' A storage slot records whether a value is materialized, reusable, or only
#' present as a fused expression inside a generated region.
#'
#' @param id Stable storage-slot id.
#' @param value_id Value owned by this slot.
#' @param type Slot value type.
#' @param role Storage role.
#' @param materialized Whether storage exists at runtime.
#' @param reusable Whether later planning may reuse the slot.
#' @param aliases Value ids known to share storage with this slot.
#' @param lifetime Optional typed liveness interval for storage reuse.
#' @param attrs Structured slot metadata.
#' @export
TccqStorageSlot <- S7::new_class(
  "TccqStorageSlot",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    value_id = S7::class_character,
    type = TccqType,
    role = S7::class_character,
    materialized = S7::class_logical,
    reusable = S7::class_logical,
    aliases = S7::class_character,
    lifetime = S7::new_union(NULL, TccqStorageLifetime),
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
    if (length(self@role) != 1L || is.na(self@role) || !self@role %in% TCCQ_STORAGE_ROLES) {
      problems <- c(problems, "@role must be one supported storage role")
    }
    if (length(self@materialized) != 1L || is.na(self@materialized)) {
      problems <- c(problems, "@materialized must be a single TRUE/FALSE value")
    }
    if (length(self@reusable) != 1L || is.na(self@reusable)) {
      problems <- c(problems, "@reusable must be a single TRUE/FALSE value")
    }
    if (anyNA(self@aliases) || any(!nzchar(self@aliases))) {
      problems <- c(problems, "@aliases must contain non-empty value ids")
    }
    if (identical(self@role, "literal") && isTRUE(self@materialized)) {
      problems <- c(problems, "literal storage slots must not be materialized")
    }
    if (identical(self@role, "input") && isTRUE(self@reusable)) {
      problems <- c(problems, "input storage slots must not be reusable")
    }
    if (isTRUE(self@reusable) && is.null(self@lifetime)) {
      problems <- c(problems, "reusable storage slots must carry a typed lifetime")
    }
    if (isTRUE(self@reusable) && !isTRUE(self@materialized)) {
      problems <- c(problems, "only materialized storage slots can be reused")
    }
    if (!is.null(self@lifetime) && !identical(self@lifetime@value_id, self@value_id)) {
      problems <- c(problems, "@lifetime value id must match @value_id")
    }
    if (length(problems) > 0L) problems
  }
)

#' Storage plan
#'
#' A storage plan belongs to the middle end. It is consumed by backends but does
#' not depend on C, Fortran, TinyCC, or any concrete emitter.
#'
#' @param slots List of storage slots.
#' @param reuse_groups List of storage-slot ids that may share allocation.
#' @param attrs Structured storage-plan metadata.
#' @export
TccqStoragePlan <- S7::new_class(
  "TccqStoragePlan",
  package = "tccquickr",
  properties = list(
    slots = S7::class_list,
    reuse_groups = S7::class_list,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    slots_are_tccq_storage_slots <- vapply(
      self@slots,
      S7::S7_inherits,
      logical(1),
      class = TccqStorageSlot
    )
    if (!all(slots_are_tccq_storage_slots)) {
      problems <- c(problems, "@slots must contain only <TccqStorageSlot> values")
    }
    for (group_index in seq_along(self@reuse_groups)) {
      group <- self@reuse_groups[[group_index]]
      if (!is.character(group) || anyNA(group) || any(!nzchar(group))) {
        problems <- c(
          problems,
          sprintf("@reuse_groups[[%d]] must contain non-empty storage-slot ids", group_index)
        )
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Lowering plan
#'
#' A lowering plan is the typed result of lowering declared R language objects
#' into backend-neutral values, regions, and storage facts. Backend printers
#' consume this plan through `TccqProgram`; they do not re-discover compiler
#' semantics from source text.
#'
#' @param values List of lowered IR values.
#' @param schedule Optional ordered program evaluation schedule.
#' @param regions List of executable code regions.
#' @param result Result value id, or `NULL` when no expression was lowered.
#' @param storage_plan Optional middle-end storage plan.
#' @param diagnostics List of diagnostics attached to lowering.
#' @param attrs Structured lowering metadata.
#' @export
TccqLoweringPlan <- S7::new_class(
  "TccqLoweringPlan",
  package = "tccquickr",
  properties = list(
    values = S7::class_list,
    schedule = S7::new_union(NULL, TccqProgramSchedule),
    regions = S7::class_list,
    result = S7::class_any,
    storage_plan = S7::new_union(NULL, TccqStoragePlan),
    diagnostics = S7::class_list,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    values_are_tccq_values <- vapply(self@values, S7::S7_inherits, logical(1), class = TccqValue)
    regions_are_tccq_regions <- vapply(self@regions, S7::S7_inherits, logical(1), class = TccqRegion)
    diagnostics_are_tccq_diagnostics <- vapply(
      self@diagnostics,
      S7::S7_inherits,
      logical(1),
      class = TccqDiagnostic
    )
    if (!all(values_are_tccq_values)) {
      problems <- c(problems, "@values must contain only <TccqValue> values")
    }
    if (!all(regions_are_tccq_regions)) {
      problems <- c(problems, "@regions must contain only <TccqRegion> values")
    }
    if (!is.null(self@result)) {
      if (length(self@result) != 1L || is.na(self@result) || !nzchar(self@result)) {
        problems <- c(problems, "@result must be NULL or one non-empty value id")
      }
    }
    if (!all(diagnostics_are_tccq_diagnostics)) {
      problems <- c(problems, "@diagnostics must contain only <TccqDiagnostic> values")
    }
    if (!is.null(self@schedule) && !identical(self@schedule@result, self@result)) {
      problems <- c(problems, "@schedule and @result must identify the same value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Program schema
#'
#' @param name Program name.
#' @param formals Named list of formal bindings.
#' @param schedule Optional ordered program evaluation schedule.
#' @param values List of IR values.
#' @param regions List of executable code regions.
#' @param result Result value id or object.
#' @param diagnostics List of diagnostics attached to the program.
#' @param call_index Typed frontend call handoff.
#' @param storage_plan Optional middle-end storage plan.
#' @param attrs Structured program attributes.
#' @export
TccqProgram <- S7::new_class(
  "TccqProgram",
  package = "tccquickr",
  properties = list(
    name = S7::class_character,
    formals = S7::class_list,
    schedule = S7::new_union(NULL, TccqProgramSchedule),
    values = S7::class_list,
    regions = S7::class_list,
    result = S7::class_any,
    diagnostics = S7::class_list,
    call_index = S7::new_union(NULL, TccqCallIndex),
    storage_plan = S7::new_union(NULL, TccqStoragePlan),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@name) != 1L || is.na(self@name) || !nzchar(self@name)) {
      problems <- c(problems, "@name must be a single non-empty string")
    }
    formals_are_tccq_bindings <- vapply(self@formals, S7::S7_inherits, logical(1), class = TccqBinding)
    values_are_tccq_values <- vapply(self@values, S7::S7_inherits, logical(1), class = TccqValue)
    regions_are_tccq_regions <- vapply(self@regions, S7::S7_inherits, logical(1), class = TccqRegion)
    diagnostics_are_tccq_diagnostics <- vapply(
      self@diagnostics,
      S7::S7_inherits,
      logical(1),
      class = TccqDiagnostic
    )
    if (!all(formals_are_tccq_bindings)) {
      problems <- c(problems, "@formals must contain only <TccqBinding> values")
    }
    if (!all(values_are_tccq_values)) {
      problems <- c(problems, "@values must contain only <TccqValue> values")
    }
    if (!all(regions_are_tccq_regions)) {
      problems <- c(problems, "@regions must contain only <TccqRegion> values")
    }
    if (!all(diagnostics_are_tccq_diagnostics)) {
      problems <- c(problems, "@diagnostics must contain only <TccqDiagnostic> values")
    }
    if (!is.null(self@schedule) && !identical(self@schedule@result, self@result)) {
      problems <- c(problems, "@schedule and @result must identify the same value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Construct a symbolic dimension
#'
#' @param name Dimension symbol.
#' @export
tccq_dim_symbol <- function(name) {
  .tccq_check_character_scalar(name, "name")
  if (!grepl("^[A-Za-z.][A-Za-z0-9_.]*$", name)) {
    tccq_abort(
      "schema.invalid_dim_symbol",
      "`name` must be a simple dimension symbol.",
      phase = "schema",
      path = "dim.name",
      data = list(name = name)
    )
  }
  TccqDim(kind = "symbol", label = name, value = NA_integer_)
}

#' Construct a constant dimension
#'
#' @param value Non-negative integer dimension.
#' @export
tccq_dim_constant <- function(value) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < 0) {
    tccq_abort(
      "schema.invalid_dim_constant",
      "`value` must be a single non-negative integer.",
      phase = "schema",
      path = "dim.value",
      data = list(value = value)
    )
  }
  if (value != as.integer(value)) {
    tccq_abort(
      "schema.invalid_dim_constant",
      "`value` must be integral.",
      phase = "schema",
      path = "dim.value",
      data = list(value = value)
    )
  }
  TccqDim(kind = "constant", label = "", value = as.integer(value))
}

#' Construct an unknown dimension
#'
#' @export
tccq_dim_unknown <- function() {
  TccqDim(kind = "unknown", label = "", value = NA_integer_)
}

#' Construct an affine dimension
#'
#' An affine dimension is a symbolic extent plus a signed integer offset, such
#' as the `n - 2` extent of an interior stencil domain. It keeps shape
#' arithmetic a typed dimension fact instead of a backend string.
#'
#' @param name Symbolic dimension name.
#' @param offset Signed integer offset added to the symbolic extent.
#' @export
tccq_dim_affine <- function(name, offset) {
  .tccq_check_character_scalar(name, "name")
  if (!grepl("^[A-Za-z.][A-Za-z0-9_.]*$", name)) {
    tccq_abort(
      "schema.invalid_dim_symbol",
      "`name` must be a simple dimension symbol.",
      phase = "schema",
      path = "dim.name",
      data = list(name = name)
    )
  }
  if (!is.numeric(offset) || length(offset) != 1L || is.na(offset) || offset != as.integer(offset)) {
    tccq_abort(
      "schema.invalid_dim_offset",
      "`offset` must be a single signed integer.",
      phase = "schema",
      path = "dim.offset",
      data = list(offset = offset)
    )
  }
  offset <- as.integer(offset)
  if (identical(offset, 0L)) {
    return(tccq_dim_symbol(name))
  }
  TccqDim(kind = "affine", label = name, value = offset)
}

#' Compare two dimensions for semantic equality
#'
#' Constant dimensions compare by value, symbolic and affine dimensions by
#' symbol and offset. Unknown dimensions never compare equal.
#'
#' @param left,right `TccqDim` values.
#' @export
tccq_dim_equal <- function(left, right) {
  .tccq_check_s7(left, TccqDim, "TccqDim", "left")
  .tccq_check_s7(right, TccqDim, "TccqDim", "right")
  if (identical(left@kind, "unknown") || identical(right@kind, "unknown")) {
    return(FALSE)
  }
  identical(left@kind, right@kind) &&
    identical(left@label, right@label) &&
    identical(left@value, right@value)
}

#' Construct a shape
#'
#' @param dims A list, character vector, integer vector, or single dimension.
#' @export
tccq_shape <- function(dims = list()) {
  dims <- .tccq_normalize_dims(dims)
  TccqShape(rank = as.integer(length(dims)), dims = dims)
}

#' Construct an iteration domain
#'
#' @param id Stable domain id.
#' @param shape Domain shape.
#' @param axes Optional axis names. Defaults to `i1`, `i2`, ...
#' @param attrs Structured domain attributes.
#' @export
tccq_domain <- function(id, shape, axes = NULL, attrs = list()) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(shape, TccqShape, "TccqShape", "shape")
  if (is.null(axes)) {
    axes <- if (shape@rank == 0L) character() else paste0("i", seq_len(shape@rank))
  }
  if (!is.character(axes) || length(axes) != shape@rank || anyNA(axes) || any(!nzchar(axes))) {
    tccq_abort(
      "schema.invalid_domain_axes",
      "`axes` must be non-empty names matching domain rank.",
      phase = "schema",
      path = "domain.axes",
      data = list(rank = shape@rank, axes = axes)
    )
  }
  if (!is.list(attrs)) {
    tccq_abort("schema.invalid_attrs", "`attrs` must be a list.")
  }

  TccqDomain(id = id, shape = shape, axes = axes, attrs = attrs)
}

#' Construct an affine index expression
#'
#' @param axis Iteration-axis name, or empty for a constant index.
#' @param offset Signed integer offset (zero-based).
#' @export
tccq_index_expr <- function(axis = "", offset = 0L) {
  .tccq_check_character_or_empty(axis, "axis")
  if (!is.numeric(offset) || length(offset) != 1L || is.na(offset) || offset != as.integer(offset)) {
    tccq_abort(
      "schema.invalid_index_offset",
      "`offset` must be a single signed integer.",
      phase = "schema",
      path = "index_expr.offset",
      data = list(offset = offset)
    )
  }
  TccqIndexExpr(axis = axis, offset = as.integer(offset))
}

#' Construct a loop-nest iteration axis
#'
#' @param name Iteration-axis name.
#' @param extent Typed axis extent.
#' @param role Axis role, `map` or `reduce`.
#' @export
tccq_loop_axis <- function(name, extent, role = "map") {
  .tccq_check_character_scalar(name, "name")
  extent <- .tccq_as_dim(extent, "extent")
  .tccq_check_character_scalar(role, "role")
  if (!role %in% TCCQ_AXIS_ROLES) {
    tccq_abort(
      "schema.invalid_axis_role",
      "`role` is not a supported axis role.",
      phase = "schema",
      path = "axis.role",
      data = list(role = role, supported = TCCQ_AXIS_ROLES)
    )
  }
  TccqLoopAxis(name = name, extent = extent, role = role)
}

#' Construct a compiler type
#'
#' @param base Scalar or storage base type.
#' @param shape Shape value.
#' @export
tccq_type <- function(base, shape = tccq_shape()) {
  .tccq_check_character_scalar(base, "base")
  if (!base %in% TCCQ_BASE_TYPES) {
    tccq_abort(
      "schema.invalid_base_type",
      "`base` is not a supported compiler type.",
      phase = "schema",
      path = "type.base",
      data = list(base = base, supported = TCCQ_BASE_TYPES)
    )
  }
  .tccq_check_s7(shape, TccqShape, "TccqShape", "shape")
  TccqType(base = base, shape = shape)
}

#' Construct a scalar literal
#'
#' @param kind Literal kind: finite, na, nan, pos_inf, or neg_inf.
#' @param type Scalar type.
#' @param value Literal payload for finite values.
#' @export
tccq_literal <- function(kind, type, value = NULL) {
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_LITERAL_KINDS) {
    tccq_abort(
      "schema.invalid_literal_kind",
      "`kind` is not a supported literal kind.",
      phase = "schema",
      path = "literal.kind",
      data = list(kind = kind, supported = TCCQ_LITERAL_KINDS)
    )
  }
  .tccq_check_scalar_type(type, "type")
  .tccq_check_literal_type(kind, type)
  if (identical(kind, "finite")) {
    .tccq_check_finite_literal_value(value, type@base)
  } else {
    value <- switch(
      kind,
      na = NULL,
      nan = NaN,
      pos_inf = Inf,
      neg_inf = -Inf
    )
  }

  TccqLiteral(kind = kind, type = type, value = value)
}

#' Construct a finite scalar literal
#'
#' @param value Finite scalar value.
#' @param type Optional scalar type. If omitted, it is inferred from `value`.
#' @export
tccq_literal_finite <- function(value, type = NULL) {
  if (is.null(type)) {
    type <- .tccq_infer_finite_literal_type(value)
  }
  tccq_literal("finite", type, value = value)
}

#' Construct a typed NA literal
#'
#' @param base Base type for the missing value.
#' @export
tccq_literal_na <- function(base) {
  tccq_literal("na", .tccq_scalar_type(base))
}

#' Construct a NaN literal
#'
#' @export
tccq_literal_nan <- function() {
  tccq_literal("nan", .tccq_scalar_type("double"), value = NaN)
}

#' Construct an infinite double literal
#'
#' @param sign Positive or negative sign.
#' @export
tccq_literal_inf <- function(sign = 1L) {
  if (!is.numeric(sign) || length(sign) != 1L || is.na(sign) || sign == 0) {
    tccq_abort(
      "schema.invalid_inf_sign",
      "`sign` must be a single non-zero number.",
      phase = "schema",
      path = "literal.sign",
      data = list(sign = sign)
    )
  }
  kind <- if (sign > 0) "pos_inf" else "neg_inf"
  value <- if (sign > 0) Inf else -Inf
  tccq_literal(kind, .tccq_scalar_type("double"), value = value)
}

#' Construct an effect summary
#'
#' @param reads Whether the operation reads program state.
#' @param writes Whether the operation writes program state.
#' @param allocates Whether the operation allocates storage.
#' @param boundary Whether the operation crosses an unsupported boundary.
#' @param may_error Whether the operation may signal at runtime.
#' @param may_warn Whether the operation may warn at runtime.
#' @export
tccq_effect <- function(
  reads = FALSE,
  writes = FALSE,
  allocates = FALSE,
  boundary = FALSE,
  may_error = FALSE,
  may_warn = FALSE
) {
  .tccq_check_logical_scalar(reads, "reads")
  .tccq_check_logical_scalar(writes, "writes")
  .tccq_check_logical_scalar(allocates, "allocates")
  .tccq_check_logical_scalar(boundary, "boundary")
  .tccq_check_logical_scalar(may_error, "may_error")
  .tccq_check_logical_scalar(may_warn, "may_warn")

  TccqEffect(
    reads = reads,
    writes = writes,
    allocates = allocates,
    boundary = boundary,
    may_error = may_error,
    may_warn = may_warn
  )
}

#' Combine effect summaries
#'
#' Effect summaries are conservative may-properties. Combining two summaries
#' retains every effect reported by either input.
#'
#' @param effect Effect summary.
#' @param other Effect summary to combine with `effect`.
#' @export
tccq_effect_union <- S7::new_generic(
  "tccq_effect_union",
  dispatch_args = "effect",
  function(effect, other) S7::S7_dispatch()
)

S7::method(tccq_effect_union, TccqEffect) <- function(effect, other) {
  .tccq_check_s7(other, TccqEffect, "TccqEffect", "other")
  tccq_effect(
    reads = effect@reads || other@reads,
    writes = effect@writes || other@writes,
    allocates = effect@allocates || other@allocates,
    boundary = effect@boundary || other@boundary,
    may_error = effect@may_error || other@may_error,
    may_warn = effect@may_warn || other@may_warn
  )
}

#' Construct a program binding
#'
#' @param name Binding name.
#' @param type Binding type.
#' @param mutable Whether the binding may be mutated.
#' @export
tccq_binding <- function(name, type, mutable = FALSE) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_logical_scalar(mutable, "mutable")
  TccqBinding(name = name, type = type, mutable = mutable)
}

#' Construct a local value binding
#'
#' @inheritParams TccqLocalBinding
#' @export
tccq_local_binding <- function(
  name,
  value_id,
  type,
  statement_index,
  mutable = FALSE
) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  statement_index <- .tccq_check_positive_integer(statement_index, "statement_index")
  .tccq_check_logical_scalar(mutable, "mutable")
  TccqLocalBinding(
    name = name,
    type = type,
    mutable = mutable,
    value_id = value_id,
    statement_index = statement_index
  )
}

#' Construct a scheduled R evaluation
#'
#' @inheritParams TccqEvaluationStep
#' @export
tccq_evaluation_step <- function(
  value_id,
  statement_index,
  effect,
  binding = NULL,
  uses = list()
) {
  .tccq_check_character_scalar(value_id, "value_id")
  statement_index <- .tccq_check_positive_integer(statement_index, "statement_index")
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  .tccq_check_optional_s7(binding, TccqLocalBinding, "TccqLocalBinding", "binding")
  .tccq_check_list_of(uses, TccqLocalBinding, "TccqLocalBinding", "uses")
  TccqEvaluationStep(
    value_id = value_id,
    statement_index = statement_index,
    binding = binding,
    uses = uses,
    effect = effect
  )
}

#' Construct an ordered program evaluation schedule
#'
#' `values` is the lowered value graph used to verify references, binding
#' types, dominance, and complete step effects. It is checked at this boundary
#' but is not duplicated inside the schedule.
#'
#' @inheritParams TccqProgramSchedule
#' @param values Lowered values referenced by the schedule.
#' @export
tccq_program_schedule <- function(steps, result, values) {
  .tccq_check_list_of(
    steps,
    TccqEvaluationStep,
    "TccqEvaluationStep",
    "steps"
  )
  .tccq_check_character_scalar(result, "result")
  .tccq_check_list_of(values, TccqValue, "TccqValue", "values")

  value_ids <- vapply(values, function(value) value@id, character(1))
  if (anyDuplicated(value_ids)) {
    tccq_abort(
      "schema.duplicate_schedule_value",
      "`values` must have unique value ids when constructing a schedule.",
      phase = "schema",
      path = "program_schedule.values"
    )
  }
  values_by_id <- values
  names(values_by_id) <- value_ids

  reachable_value_ids <- function(value_id, visited = character()) {
    if (value_id %in% visited) {
      return(character())
    }
    value <- values_by_id[[value_id]]
    if (is.null(value)) {
      tccq_abort(
        "schema.unknown_schedule_value",
        "A scheduled evaluation references a value outside the lowered graph.",
        phase = "schema",
        path = "program_schedule.values",
        data = list(value_id = value_id)
      )
    }
    if (S7::S7_inherits(value, TccqBindingReference)) {
      return(value_id)
    }
    input_ids <- vapply(value@inputs, as.character, character(1))
    c(
      value_id,
      unlist(lapply(
        input_ids,
        reachable_value_ids,
        visited = c(visited, value_id)
      ), use.names = FALSE)
    )
  }

  binding_steps <- Filter(
    function(step) S7::S7_inherits(step@binding, TccqLocalBinding),
    steps
  )
  scheduled_bindings <- lapply(binding_steps, function(step) step@binding)
  names(scheduled_bindings) <- vapply(
    binding_steps,
    function(step) step@binding@name,
    character(1)
  )

  for (step in steps) {
    reachable_ids <- unique(reachable_value_ids(step@value_id))
    reference_bindings <- lapply(
      Filter(
        function(value_id) S7::S7_inherits(
          values_by_id[[value_id]],
          TccqBindingReference
        ),
        reachable_ids
      ),
      function(value_id) values_by_id[[value_id]]@binding
    )
    reference_bindings <- reference_bindings[!duplicated(vapply(
      reference_bindings,
      function(binding) binding@name,
      character(1)
    ))]
    reference_names <- vapply(
      reference_bindings,
      function(binding) binding@name,
      character(1)
    )
    recorded_names <- vapply(
      step@uses,
      function(binding) binding@name,
      character(1)
    )
    use_sets_match <- setequal(reference_names, recorded_names) && all(vapply(
      reference_names,
      function(binding_name) {
        identical(
          reference_bindings[[match(binding_name, reference_names)]],
          step@uses[[match(binding_name, recorded_names)]]
        )
      },
      logical(1)
    ))
    if (!use_sets_match) {
      tccq_abort(
        "schema.schedule_binding_uses_mismatch",
        "A schedule step must record exactly the local binding references in its value graph.",
        phase = "schema",
        path = "program_schedule.uses",
        data = list(statement_index = step@statement_index, value_id = step@value_id)
      )
    }
    reachable_effect <- Reduce(
      tccq_effect_union,
      lapply(reachable_ids, function(value_id) values_by_id[[value_id]]@effect),
      init = tccq_effect()
    )
    if (!identical(step@effect, reachable_effect)) {
      tccq_abort(
        "schema.incomplete_schedule_effect",
        "A schedule step effect must summarize its complete reachable value graph.",
        phase = "schema",
        path = "program_schedule.effect",
        data = list(statement_index = step@statement_index, value_id = step@value_id)
      )
    }
    for (used_binding in step@uses) {
      scheduled_binding <- scheduled_bindings[[used_binding@name]]
      if (is.null(scheduled_binding) || !identical(scheduled_binding, used_binding)) {
        tccq_abort(
          "schema.unknown_schedule_binding",
          "A schedule step must use the exact local binding defined by this schedule.",
          phase = "schema",
          path = "program_schedule.dominance",
          data = list(
            statement_index = step@statement_index,
            binding = used_binding@name
          )
        )
      }
      if (used_binding@statement_index >= step@statement_index) {
        tccq_abort(
          "schema.schedule_use_before_definition",
          "Every local binding used by a schedule step must already be defined.",
          phase = "schema",
          path = "program_schedule.dominance",
          data = list(
            statement_index = step@statement_index,
            binding = used_binding@name
          )
        )
      }
    }
    if (
      S7::S7_inherits(step@binding, TccqLocalBinding) &&
        is.null(values_by_id[[step@binding@value_id]])
    ) {
      tccq_abort(
        "schema.unknown_schedule_binding_value",
        "A scheduled local binding must reference a value in the lowered graph.",
        phase = "schema",
        path = "program_schedule.binding",
        data = list(binding = step@binding@name, value_id = step@binding@value_id)
      )
    }
    if (
      S7::S7_inherits(step@binding, TccqLocalBinding) &&
        (
          !identical(step@binding@type, values_by_id[[step@value_id]]@type) ||
            !identical(step@binding@type, values_by_id[[step@binding@value_id]]@type)
        )
    ) {
      tccq_abort(
        "schema.schedule_binding_type_mismatch",
        "A scheduled local binding must have the type of its lowered value.",
        phase = "schema",
        path = "program_schedule.binding",
        data = list(binding = step@binding@name, value_id = step@value_id)
      )
    }
  }

  TccqProgramSchedule(steps = steps, result = result)
}

#' Construct an IR value
#'
#' @param id Stable value id.
#' @param op Operation name.
#' @param inputs Input value ids or references.
#' @param type Result type.
#' @param effect Effect summary.
#' @param attrs Structured operation attributes.
#' @export
tccq_value <- function(
  id,
  op,
  inputs = list(),
  type,
  effect = tccq_effect(),
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(op, "op")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  if (!is.list(inputs)) {
    tccq_abort("schema.invalid_inputs", "`inputs` must be a list.")
  }
  if (!is.list(attrs)) {
    tccq_abort("schema.invalid_attrs", "`attrs` must be a list.")
  }

  TccqValue(
    id = id,
    op = op,
    inputs = inputs,
    type = type,
    effect = effect,
    attrs = attrs
  )
}

#' Construct a local binding reference
#'
#' @param id Stable reference value id.
#' @param binding Local binding being read.
#' @export
tccq_binding_reference <- function(id, binding) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_s7(binding, TccqLocalBinding, "TccqLocalBinding", "binding")
  TccqBindingReference(
    id = id,
    op = "binding_reference",
    inputs = list(binding@value_id),
    type = binding@type,
    effect = tccq_effect(reads = TRUE),
    attrs = list(),
    binding = binding
  )
}

#' Construct a lazy conditional IR value
#'
#' @param id Stable value id.
#' @param condition Scalar logical condition value id.
#' @param consequent Value id selected when the condition is true.
#' @param alternative Value id selected when the condition is false.
#' @param type Joined branch result type.
#' @param semantics Evaluator facts for the originating `if` call.
#' @param effect Conservative effect summary across the condition and both arms.
#' @param attrs Structured branch metadata.
#' @export
tccq_branch <- function(
  id,
  condition,
  consequent,
  alternative,
  type,
  semantics,
  effect = tccq_effect(),
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(condition, "condition")
  .tccq_check_character_scalar(consequent, "consequent")
  .tccq_check_character_scalar(alternative, "alternative")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_s7(semantics, TccqCallSemantics, "TccqCallSemantics", "semantics")
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  .tccq_check_list(attrs, "attrs")

  TccqBranch(
    id = id,
    op = "if",
    inputs = list(condition, consequent, alternative),
    type = type,
    effect = effect,
    attrs = attrs,
    condition = condition,
    consequent = consequent,
    alternative = alternative,
    semantics = semantics
  )
}

#' Construct a domain access mapping
#'
#' @param value_id Referenced value id.
#' @param domain Access domain.
#' @param kind Access kind.
#' @param index_map Structured index-map payload.
#' @param attrs Structured access attributes.
#' @export
tccq_access <- function(
  value_id,
  domain,
  kind = "identity",
  index_map = list(),
  attrs = list()
) {
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_s7(domain, TccqDomain, "TccqDomain", "domain")
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_ACCESS_KINDS) {
    tccq_abort(
      "schema.invalid_access_kind",
      "`kind` is not a supported access kind.",
      phase = "schema",
      path = "access.kind",
      data = list(kind = kind, supported = TCCQ_ACCESS_KINDS)
    )
  }
  if (!is.list(index_map)) {
    tccq_abort("schema.invalid_index_map", "`index_map` must be a list.")
  }
  if (!is.list(attrs)) {
    tccq_abort("schema.invalid_attrs", "`attrs` must be a list.")
  }

  TccqAccess(
    value_id = value_id,
    domain = domain,
    kind = kind,
    index_map = index_map,
    attrs = attrs
  )
}

#' Construct a fusion group
#'
#' @param id Stable fusion-group id.
#' @param kind Fusion kind.
#' @param domain Shared iteration domain.
#' @param values List of IR values in the group.
#' @param outputs Output value ids.
#' @param accesses List of access mappings.
#' @param region_kind Candidate execution region kind, or `any`.
#' @param target Candidate implementation target, or `any`.
#' @param effect Effect summary for the fused group.
#' @param contract Typed fusion operation/storage contract.
#' @param attrs Structured fusion attributes.
#' @export
tccq_fusion_group <- function(
  id,
  kind,
  domain,
  values = list(),
  outputs = character(),
  accesses = list(),
  region_kind = "kernel",
  target = "any",
  effect = tccq_effect(),
  contract = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_FUSION_KINDS) {
    tccq_abort(
      "schema.invalid_fusion_kind",
      "`kind` is not a supported fusion kind.",
      phase = "schema",
      path = "fusion.kind",
      data = list(kind = kind, supported = TCCQ_FUSION_KINDS)
    )
  }
  .tccq_check_s7(domain, TccqDomain, "TccqDomain", "domain")
  .tccq_check_list_of(values, TccqValue, "TccqValue", "values")
  if (!is.character(outputs) || anyNA(outputs) || any(!nzchar(outputs))) {
    tccq_abort(
      "schema.invalid_fusion_outputs",
      "`outputs` must be value ids.",
      phase = "schema",
      path = "fusion.outputs",
      data = list(outputs = outputs)
    )
  }
  .tccq_check_list_of(accesses, TccqAccess, "TccqAccess", "accesses")
  .tccq_check_region_query_kind(region_kind, "region_kind")
  .tccq_check_character_scalar(target, "target")
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  .tccq_check_optional_s7(contract, TccqFusionContract, "TccqFusionContract", "contract")
  if (!is.list(attrs)) {
    tccq_abort("schema.invalid_attrs", "`attrs` must be a list.")
  }
  .tccq_check_fusion_legality(region_kind, effect)
  if (!is.null(contract) && !identical(contract@fusion_kind, kind)) {
    tccq_abort(
      "schema.fusion_contract_kind_mismatch",
      "`contract` fusion kind must match `kind`.",
      phase = "schema",
      path = "fusion.contract",
      data = list(kind = kind, contract_kind = contract@fusion_kind)
    )
  }

  TccqFusionGroup(
    id = id,
    kind = kind,
    domain = domain,
    values = values,
    outputs = outputs,
    accesses = accesses,
    region_kind = region_kind,
    target = target,
    effect = effect,
    contract = contract,
    attrs = attrs
  )
}

#' Construct an executable code region
#'
#' @param id Stable region id.
#' @param kind Region kind: host, kernel, parallel, or device.
#' @param values List of IR values in the region.
#' @param fusion_groups List of fusion groups in the region.
#' @param effect Region effect summary.
#' @param memory_space Dominant memory space: r, host, device, or opaque.
#' @param touches_rapi Whether the region may call into the R C API.
#' @param attrs Structured region attributes.
#' @export
tccq_region <- function(
  id,
  kind,
  values = list(),
  fusion_groups = list(),
  effect = tccq_effect(),
  memory_space = "host",
  touches_rapi = FALSE,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_REGION_KINDS) {
    tccq_abort(
      "schema.invalid_region_kind",
      "`kind` is not a supported region kind.",
      phase = "schema",
      path = "region.kind",
      data = list(kind = kind, supported = TCCQ_REGION_KINDS)
    )
  }
  .tccq_check_list_of(values, TccqValue, "TccqValue", "values")
  .tccq_check_list_of(
    fusion_groups,
    TccqFusionGroup,
    "TccqFusionGroup",
    "fusion_groups"
  )
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  .tccq_check_character_scalar(memory_space, "memory_space")
  if (!memory_space %in% TCCQ_MEMORY_SPACES) {
    tccq_abort(
      "schema.invalid_memory_space",
      "`memory_space` is not supported.",
      phase = "schema",
      path = "region.memory_space",
      data = list(memory_space = memory_space, supported = TCCQ_MEMORY_SPACES)
    )
  }
  .tccq_check_logical_scalar(touches_rapi, "touches_rapi")
  if (!is.list(attrs)) {
    tccq_abort("schema.invalid_attrs", "`attrs` must be a list.")
  }
  .tccq_check_region_legality(kind, effect, memory_space, touches_rapi)

  TccqRegion(
    id = id,
    kind = kind,
    values = values,
    fusion_groups = fusion_groups,
    effect = effect,
    memory_space = memory_space,
    touches_rapi = touches_rapi,
    attrs = attrs
  )
}

#' Construct a storage slot
#'
#' @param id Stable storage-slot id.
#' @param value_id Value owned by this slot.
#' @param type Slot value type.
#' @param role Storage role.
#' @param materialized Whether storage exists at runtime.
#' @param reusable Whether later planning may reuse the slot.
#' @param aliases Value ids known to share storage with this slot.
#' @param lifetime Optional typed liveness interval for storage reuse.
#' @param attrs Structured slot metadata.
#' @export
tccq_storage_slot <- function(
  id,
  value_id,
  type,
  role,
  materialized,
  reusable = FALSE,
  aliases = character(),
  lifetime = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_character_scalar(role, "role")
  if (!role %in% TCCQ_STORAGE_ROLES) {
    tccq_abort(
      "schema.invalid_storage_role",
      "`role` is not a supported storage role.",
      phase = "schema",
      path = "storage.role",
      data = list(role = role, supported = TCCQ_STORAGE_ROLES)
    )
  }
  .tccq_check_logical_scalar(materialized, "materialized")
  .tccq_check_logical_scalar(reusable, "reusable")
  if (!is.character(aliases) || anyNA(aliases) || any(!nzchar(aliases))) {
    tccq_abort(
      "schema.invalid_storage_aliases",
      "`aliases` must contain non-empty value ids.",
      phase = "schema",
      path = "storage.aliases"
    )
  }
  .tccq_check_optional_s7(lifetime, TccqStorageLifetime, "TccqStorageLifetime", "lifetime")
  if (isTRUE(reusable) && is.null(lifetime)) {
    tccq_abort(
      "schema.storage_lifetime_required",
      "Reusable storage slots must carry a typed lifetime.",
      phase = "schema",
      path = "storage.lifetime",
      data = list(slot = id, value_id = value_id)
    )
  }
  if (!is.null(lifetime) && !identical(lifetime@value_id, value_id)) {
    tccq_abort(
      "schema.storage_lifetime_mismatch",
      "Storage slot lifetime value id must match the slot value id.",
      phase = "schema",
      path = "storage.lifetime.value_id",
      data = list(slot = id, value_id = value_id, lifetime_value_id = lifetime@value_id)
    )
  }
  .tccq_check_list(attrs, "attrs")

  TccqStorageSlot(
    id = id,
    value_id = value_id,
    type = type,
    role = role,
    materialized = materialized,
    reusable = reusable,
    aliases = aliases,
    lifetime = lifetime,
    attrs = attrs
  )
}

#' Construct a storage lifetime interval
#'
#' @param value_id Value whose lifetime is described.
#' @param defined_at Linear definition position in the lowered value stream.
#' @param last_used_at Last position where the value must remain live.
#' @export
tccq_storage_lifetime <- function(value_id, defined_at, last_used_at) {
  .tccq_check_character_scalar(value_id, "value_id")
  defined_at <- .tccq_check_positive_integer(defined_at, "defined_at")
  last_used_at <- .tccq_check_positive_integer(last_used_at, "last_used_at")
  if (last_used_at < defined_at) {
    tccq_abort(
      "schema.invalid_storage_lifetime",
      "`last_used_at` must be greater than or equal to `defined_at`.",
      phase = "schema",
      path = "storage_lifetime.last_used_at",
      data = list(value_id = value_id, defined_at = defined_at, last_used_at = last_used_at)
    )
  }

  TccqStorageLifetime(
    value_id = value_id,
    defined_at = defined_at,
    last_used_at = last_used_at
  )
}

#' Construct a storage plan
#'
#' @param slots List of storage slots.
#' @param reuse_groups List of storage-slot ids that may share allocation.
#' @param attrs Structured storage-plan metadata.
#' @export
tccq_storage_plan <- function(slots = list(), reuse_groups = list(), attrs = list()) {
  .tccq_check_list_of(slots, TccqStorageSlot, "TccqStorageSlot", "slots")
  if (!is.list(reuse_groups)) {
    tccq_abort(
      "schema.invalid_storage_reuse_groups",
      "`reuse_groups` must be a list.",
      phase = "schema",
      path = "storage.reuse_groups"
    )
  }
  .tccq_check_list(attrs, "attrs")

  TccqStoragePlan(slots = slots, reuse_groups = reuse_groups, attrs = attrs)
}

#' Construct a program schema
#'
#' @param name Program name.
#' @param formals Named list of formal bindings.
#' @param schedule Optional ordered program evaluation schedule.
#' @param values List of IR values.
#' @param regions List of executable code regions.
#' @param result Result value id or object.
#' @param diagnostics List of diagnostics attached to the program.
#' @param call_index Typed frontend call handoff.
#' @param storage_plan Optional middle-end storage plan.
#' @param attrs Structured program attributes.
#' @export
tccq_program <- function(
  name,
  formals,
  schedule = NULL,
  values = list(),
  regions = list(),
  result = NULL,
  diagnostics = list(),
  call_index = NULL,
  storage_plan = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_list_of(formals, TccqBinding, "TccqBinding", "formals")
  .tccq_check_list_of(values, TccqValue, "TccqValue", "values")
  .tccq_check_optional_s7(
    schedule,
    TccqProgramSchedule,
    "TccqProgramSchedule",
    "schedule"
  )
  if (!is.null(schedule)) {
    schedule <- tccq_program_schedule(schedule@steps, schedule@result, values)
  }
  .tccq_check_list_of(regions, TccqRegion, "TccqRegion", "regions")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  .tccq_check_optional_s7(call_index, TccqCallIndex, "TccqCallIndex", "call_index")
  .tccq_check_optional_s7(storage_plan, TccqStoragePlan, "TccqStoragePlan", "storage_plan")
  .tccq_check_list(attrs, "attrs")
  TccqProgram(
    name = name,
    formals = formals,
    schedule = schedule,
    values = values,
    regions = regions,
    result = result,
    diagnostics = diagnostics,
    call_index = call_index,
    storage_plan = storage_plan,
    attrs = attrs
  )
}

.tccq_normalize_dims <- function(dims) {
  if (missing(dims) || is.null(dims)) {
    return(list())
  }
  if (S7::S7_inherits(dims, TccqDim)) {
    return(list(dims))
  }
  if (is.character(dims) || is.numeric(dims)) {
    dims <- as.list(dims)
  }
  if (!is.list(dims)) {
    tccq_abort(
      "schema.invalid_shape_dims",
      "`dims` must be dimensions, symbols, or constants.",
      phase = "schema",
      path = "shape.dims",
      data = list(type = typeof(dims))
    )
  }
  lapply(seq_along(dims), function(i) {
    .tccq_as_dim(dims[[i]], sprintf("dims[[%d]]", i))
  })
}

.tccq_as_dim <- function(x, arg) {
  if (S7::S7_inherits(x, TccqDim)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(tccq_dim_symbol(x))
  }
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    return(tccq_dim_constant(x))
  }
  tccq_abort(
    "schema.invalid_dimension",
    sprintf("`%s` must be a dimension object, symbol, or constant.", arg),
    phase = "schema",
    path = arg,
    data = list(value = x)
  )
}

.tccq_scalar_type <- function(base) {
  tccq_type(base, tccq_shape())
}

.tccq_check_scalar_type <- function(type, arg) {
  .tccq_check_s7(type, TccqType, "TccqType", arg)
  if (!identical(type@shape@rank, 0L)) {
    tccq_abort(
      "schema.literal_non_scalar_type",
      sprintf("`%s` must have scalar rank.", arg),
      phase = "schema",
      path = arg,
      data = list(rank = type@shape@rank)
    )
  }
}

.tccq_check_literal_type <- function(kind, type) {
  if (identical(kind, "na") && type@base %in% c("raw", "buffer")) {
    tccq_abort(
      "schema.invalid_na_literal_type",
      "`NA` literals are not valid for raw or buffer types.",
      phase = "schema",
      path = "literal.type",
      data = list(base = type@base)
    )
  }
  if (kind %in% c("nan", "pos_inf", "neg_inf") && !identical(type@base, "double")) {
    tccq_abort(
      "schema.invalid_special_numeric_literal_type",
      "`NaN` and infinite literals are currently double scalars.",
      phase = "schema",
      path = "literal.type",
      data = list(kind = kind, base = type@base)
    )
  }
}

.tccq_check_finite_literal_value <- function(value, base) {
  if (length(value) != 1L) {
    tccq_abort(
      "schema.invalid_finite_literal_length",
      "Finite scalar literals must have length 1.",
      phase = "schema",
      path = "literal.value",
      data = list(length = length(value))
    )
  }
  if (identical(base, "buffer")) {
    tccq_abort(
      "schema.invalid_buffer_literal",
      "`buffer` has no finite scalar literal payload.",
      phase = "schema",
      path = "literal.value"
    )
  }
  if ((is.atomic(value) || is.complex(value)) && anyNA(value)) {
    tccq_abort(
      "schema.invalid_finite_literal_special",
      "Use the dedicated special-literal constructors for NA and NaN.",
      phase = "schema",
      path = "literal.value",
      data = list(value = value)
    )
  }
  if (is.numeric(value) && is.infinite(value)) {
    tccq_abort(
      "schema.invalid_finite_literal_special",
      "Use `tccq_literal_inf()` for infinite literals.",
      phase = "schema",
      path = "literal.value",
      data = list(value = value)
    )
  }
  inferred <- .tccq_infer_finite_literal_base(value)
  if (!identical(inferred, base)) {
    tccq_abort(
      "schema.finite_literal_type_mismatch",
      "Finite literal payload does not match the requested base type.",
      phase = "schema",
      path = "literal.value",
      data = list(expected = base, actual = inferred)
    )
  }
}

.tccq_infer_finite_literal_type <- function(value) {
  .tccq_scalar_type(.tccq_infer_finite_literal_base(value))
}

.tccq_infer_finite_literal_base <- function(value) {
  if (is.raw(value) && length(value) == 1L) {
    return("raw")
  }
  type <- typeof(value)
  switch(
    type,
    logical = "logical",
    integer = "integer",
    double = "double",
    complex = "complex",
    character = "character",
    tccq_abort(
      "schema.unsupported_literal_payload",
      "Unsupported finite literal payload type.",
      phase = "schema",
      path = "literal.value",
      data = list(type = type)
    )
  )
}

.tccq_check_region_legality <- function(kind, effect, memory_space, touches_rapi) {
  if (kind %in% c("kernel", "parallel", "device") && isTRUE(touches_rapi)) {
    tccq_abort(
      "schema.region_touches_rapi",
      "`kernel`, `parallel`, and `device` regions must not touch the R C API.",
      phase = "schema",
      path = "region.touches_rapi",
      data = list(kind = kind)
    )
  }
  if (kind %in% c("kernel", "parallel", "device") && isTRUE(effect@boundary)) {
    tccq_abort(
      "schema.region_boundary_effect",
      "`kernel`, `parallel`, and `device` regions cannot contain boundary effects.",
      phase = "schema",
      path = "region.effect",
      data = list(kind = kind)
    )
  }
  if (identical(kind, "device") && !identical(memory_space, "device")) {
    tccq_abort(
      "schema.device_region_memory_space",
      "`device` regions must use device memory space.",
      phase = "schema",
      path = "region.memory_space",
      data = list(memory_space = memory_space)
    )
  }
  if (identical(kind, "host") && identical(memory_space, "device")) {
    tccq_abort(
      "schema.host_region_device_memory",
      "`host` regions cannot declare device memory space.",
      phase = "schema",
      path = "region.memory_space"
    )
  }
}

.tccq_check_fusion_legality <- function(region_kind, effect) {
  if (identical(region_kind, "any")) {
    return(invisible(NULL))
  }
  if (region_kind %in% c("kernel", "parallel", "device") && isTRUE(effect@boundary)) {
    tccq_abort(
      "schema.fusion_boundary_effect",
      "`kernel`, `parallel`, and `device` fusion groups cannot contain boundary effects.",
      phase = "schema",
      path = "fusion.effect",
      data = list(region_kind = region_kind)
    )
  }
  invisible(NULL)
}

.tccq_check_optional_s7 <- function(x, class, label, arg) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  .tccq_check_s7(x, class, label, arg)
}
