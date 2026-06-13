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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
)

#' Program schema
#'
#' @param name Program name.
#' @param formals Named list of formal bindings.
#' @param values List of IR values.
#' @param regions List of executable code regions.
#' @param result Result value id or object.
#' @param diagnostics List of diagnostics attached to the program.
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
    diagnostics = S7::class_list
  )
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

#' Construct a program schema
#'
#' @param name Program name.
#' @param formals Named list of formal bindings.
#' @param values List of IR values.
#' @param regions List of executable code regions.
#' @param result Result value id or object.
#' @param diagnostics List of diagnostics attached to the program.
#' @export
tccq_program <- function(
  name,
  formals,
  values = list(),
  regions = list(),
  result = NULL,
  diagnostics = list()
) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_list_of(formals, TccqBinding, "TccqBinding", "formals")
  .tccq_check_list_of(values, TccqValue, "TccqValue", "values")
  .tccq_check_list_of(regions, TccqRegion, "TccqRegion", "regions")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  TccqProgram(
    name = name,
    formals = formals,
    values = values,
    regions = regions,
    result = result,
    diagnostics = diagnostics
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
