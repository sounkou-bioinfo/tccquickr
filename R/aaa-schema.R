TCCQ_BASE_TYPES <- c("logical", "integer", "double", "complex", "character")
TCCQ_DIM_KINDS <- c("constant", "symbol", "unknown")

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

#' Declared compiler type
#'
#' @param base Scalar base type.
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
  )
)

#' Program schema
#'
#' @param name Program name.
#' @param formals Named list of formal bindings.
#' @param values List of IR values.
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

#' Construct a compiler type
#'
#' @param base Scalar base type.
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

#' Construct a program schema
#'
#' @param name Program name.
#' @param formals Named list of formal bindings.
#' @param values List of IR values.
#' @param result Result value id or object.
#' @param diagnostics List of diagnostics attached to the program.
#' @export
tccq_program <- function(
  name,
  formals,
  values = list(),
  result = NULL,
  diagnostics = list()
) {
  .tccq_check_character_scalar(name, "name")
  .tccq_check_list_of(formals, TccqBinding, "TccqBinding", "formals")
  .tccq_check_list_of(values, TccqValue, "TccqValue", "values")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  TccqProgram(
    name = name,
    formals = formals,
    values = values,
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
