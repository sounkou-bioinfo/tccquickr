TCCQ_BASE_TYPES <- c(
  "logical",
  "integer",
  "double",
  "complex",
  "character",
  "raw",
  "buffer"
)
TCCQ_DIM_KINDS <- c("constant", "symbol", "unknown")
TCCQ_LITERAL_KINDS <- c("finite", "na", "nan", "pos_inf", "neg_inf")
TCCQ_LAYOUT_ORDERS <- c("unknown", "column_major", "row_major", "strided", "opaque")
TCCQ_ACCESS_KINDS <- c("identity", "scalar", "broadcast", "slice", "transpose", "custom")
TCCQ_FUSION_KINDS <- c("map", "map_reduce", "stencil", "tile", "custom")
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

#' Physical array layout
#'
#' Layout describes physical storage order and strides. Shape remains the
#' semantic array extent; layout is only about representation.
#'
#' @param rank Layout rank.
#' @param order Layout order.
#' @param strides Per-axis strides.
#' @param offset Storage offset.
#' @param contiguous Whether the layout is contiguous.
#' @export
TccqLayout <- S7::new_class(
  "TccqLayout",
  package = "tccquickr",
  properties = list(
    rank = S7::class_integer,
    order = S7::class_character,
    strides = S7::class_list,
    offset = TccqDim,
    contiguous = S7::class_logical
  ),
  validator = function(self) {
    problems <- character()
    has_valid_rank <- length(self@rank) == 1L && !is.na(self@rank) && self@rank >= 0L
    if (!has_valid_rank) {
      problems <- c(problems, "@rank must be one non-negative integer")
    }
    if (length(self@order) != 1L || is.na(self@order) || !self@order %in% TCCQ_LAYOUT_ORDERS) {
      problems <- c(problems, "@order must be one supported layout order")
    }
    strides_are_tccq_dims <- vapply(self@strides, S7::S7_inherits, logical(1), class = TccqDim)
    if (!all(strides_are_tccq_dims)) {
      problems <- c(problems, "@strides must contain only <TccqDim> values")
    }
    if (has_valid_rank && length(self@strides) != self@rank) {
      problems <- c(problems, "@strides length must match @rank")
    }
    if (length(self@contiguous) != 1L || is.na(self@contiguous)) {
      problems <- c(problems, "@contiguous must be a single TRUE/FALSE value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Array tile description
#'
#' Tile metadata describes a rectangular partition of an array domain. It does
#' not change the value type.
#'
#' @param shape Tile shape.
#' @param origin Per-axis tile origin.
#' @export
TccqTile <- S7::new_class(
  "TccqTile",
  package = "tccquickr",
  properties = list(
    shape = TccqShape,
    origin = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    origins_are_tccq_dims <- vapply(self@origin, S7::S7_inherits, logical(1), class = TccqDim)
    if (!all(origins_are_tccq_dims)) {
      problems <- c(problems, "@origin must contain only <TccqDim> values")
    }
    if (length(self@origin) != self@shape@rank) {
      problems <- c(problems, "@origin length must match @shape rank")
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
#' @export
TccqEffect <- S7::new_class(
  "TccqEffect",
  package = "tccquickr",
  properties = list(
    reads = S7::class_logical,
    writes = S7::class_logical,
    allocates = S7::class_logical,
    boundary = S7::class_logical,
    may_error = S7::class_logical
  ),
  validator = function(self) {
    problems <- character()
    values <- list(
      reads = self@reads,
      writes = self@writes,
      allocates = self@allocates,
      boundary = self@boundary,
      may_error = self@may_error
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

#' IR value
#'
#' @param id Stable value id.
#' @param op Operation name.
#' @param inputs Input value ids or references.
#' @param type Result type.
#' @param effect Effect summary.
#' @param layout Optional physical layout.
#' @param tile Optional tile metadata.
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
    layout = S7::new_union(NULL, TccqLayout),
    tile = S7::new_union(NULL, TccqTile),
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
    if (!is.null(self@layout) && self@layout@rank != self@type@shape@rank) {
      problems <- c(problems, "@layout rank must match @type shape rank")
    }
    if (!is.null(self@tile) && self@tile@shape@rank != self@type@shape@rank) {
      problems <- c(problems, "@tile shape rank must match @type shape rank")
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
    if (length(problems) > 0L) problems
  }
)

#' Program schema
#'
#' @param name Program name.
#' @param formals Named list of formal bindings.
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

#' Construct a shape
#'
#' @param dims A list, character vector, integer vector, or single dimension.
#' @export
tccq_shape <- function(dims = list()) {
  dims <- .tccq_normalize_dims(dims)
  TccqShape(rank = as.integer(length(dims)), dims = dims)
}

#' Construct a physical array layout
#'
#' @param rank Layout rank.
#' @param order Layout order.
#' @param strides Optional per-axis strides. Defaults to unknown strides.
#' @param offset Storage offset.
#' @param contiguous Whether the layout is contiguous.
#' @export
tccq_layout <- function(
  rank,
  order = "unknown",
  strides = NULL,
  offset = tccq_dim_constant(0L),
  contiguous = FALSE
) {
  if (!is.numeric(rank) || length(rank) != 1L || is.na(rank) || rank < 0) {
    tccq_abort(
      "schema.invalid_layout_rank",
      "`rank` must be a non-negative integer.",
      phase = "schema",
      path = "layout.rank",
      data = list(rank = rank)
    )
  }
  rank <- as.integer(rank)
  .tccq_check_character_scalar(order, "order")
  if (!order %in% TCCQ_LAYOUT_ORDERS) {
    tccq_abort(
      "schema.invalid_layout_order",
      "`order` is not supported.",
      phase = "schema",
      path = "layout.order",
      data = list(order = order, supported = TCCQ_LAYOUT_ORDERS)
    )
  }
  if (is.null(strides)) {
    strides <- replicate(rank, tccq_dim_unknown(), simplify = FALSE)
  } else {
    strides <- .tccq_normalize_dims(strides)
  }
  if (length(strides) != rank) {
    tccq_abort(
      "schema.layout_stride_rank_mismatch",
      "`strides` length must match `rank`.",
      phase = "schema",
      path = "layout.strides",
      data = list(rank = rank, strides = length(strides))
    )
  }
  offset <- .tccq_as_dim(offset, "offset")
  .tccq_check_logical_scalar(contiguous, "contiguous")

  TccqLayout(
    rank = rank,
    order = order,
    strides = strides,
    offset = offset,
    contiguous = contiguous
  )
}

#' Construct tile metadata
#'
#' @param shape Tile shape.
#' @param origin Optional per-axis origin. Defaults to zero origin.
#' @export
tccq_tile <- function(shape, origin = NULL) {
  .tccq_check_s7(shape, TccqShape, "TccqShape", "shape")
  if (is.null(origin)) {
    origin <- replicate(shape@rank, tccq_dim_constant(0L), simplify = FALSE)
  } else {
    origin <- .tccq_normalize_dims(origin)
  }
  if (length(origin) != shape@rank) {
    tccq_abort(
      "schema.tile_origin_rank_mismatch",
      "`origin` length must match tile rank.",
      phase = "schema",
      path = "tile.origin",
      data = list(rank = shape@rank, origin = length(origin))
    )
  }

  TccqTile(shape = shape, origin = origin)
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
#' @export
tccq_effect <- function(
  reads = FALSE,
  writes = FALSE,
  allocates = FALSE,
  boundary = FALSE,
  may_error = FALSE
) {
  .tccq_check_logical_scalar(reads, "reads")
  .tccq_check_logical_scalar(writes, "writes")
  .tccq_check_logical_scalar(allocates, "allocates")
  .tccq_check_logical_scalar(boundary, "boundary")
  .tccq_check_logical_scalar(may_error, "may_error")

  TccqEffect(
    reads = reads,
    writes = writes,
    allocates = allocates,
    boundary = boundary,
    may_error = may_error
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

#' Construct an IR value
#'
#' @param id Stable value id.
#' @param op Operation name.
#' @param inputs Input value ids or references.
#' @param type Result type.
#' @param effect Effect summary.
#' @param layout Optional physical layout.
#' @param tile Optional tile metadata.
#' @param attrs Structured operation attributes.
#' @export
tccq_value <- function(
  id,
  op,
  inputs = list(),
  type,
  effect = tccq_effect(),
  layout = NULL,
  tile = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(op, "op")
  .tccq_check_s7(type, TccqType, "TccqType", "type")
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  .tccq_check_optional_s7(layout, TccqLayout, "TccqLayout", "layout")
  .tccq_check_optional_s7(tile, TccqTile, "TccqTile", "tile")
  .tccq_check_value_layout_rank(type, layout, tile)
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
    layout = layout,
    tile = tile,
    attrs = attrs
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
  if (!is.list(attrs)) {
    tccq_abort("schema.invalid_attrs", "`attrs` must be a list.")
  }
  .tccq_check_fusion_legality(region_kind, effect)

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
  .tccq_check_list_of(regions, TccqRegion, "TccqRegion", "regions")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  .tccq_check_optional_s7(call_index, TccqCallIndex, "TccqCallIndex", "call_index")
  .tccq_check_optional_s7(storage_plan, TccqStoragePlan, "TccqStoragePlan", "storage_plan")
  .tccq_check_list(attrs, "attrs")
  TccqProgram(
    name = name,
    formals = formals,
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

.tccq_check_value_layout_rank <- function(type, layout, tile) {
  rank <- type@shape@rank
  if (!is.null(layout) && !identical(layout@rank, rank)) {
    tccq_abort(
      "schema.value_layout_rank_mismatch",
      "Value layout rank must match value type rank.",
      phase = "schema",
      path = "value.layout",
      data = list(type_rank = rank, layout_rank = layout@rank)
    )
  }
  if (!is.null(tile) && !identical(tile@shape@rank, rank)) {
    tccq_abort(
      "schema.value_tile_rank_mismatch",
      "Value tile rank must match value type rank.",
      phase = "schema",
      path = "value.tile",
      data = list(type_rank = rank, tile_rank = tile@shape@rank)
    )
  }
}
