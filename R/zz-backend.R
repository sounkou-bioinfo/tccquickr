TCCQ_BACKEND_MODES <- c("source", "jit", "shared_library")
TCCQ_BACKEND_FAMILIES <- c("c", "fortran")
TCCQ_BACKEND_CAPABILITIES <- c(
  "source",
  "jit",
  "shared_library",
  "r_api",
  "native",
  "host_memory",
  "buffer_bridge",
  "kernel"
)
TCCQ_BRIDGE_KINDS <- c(
  "sexp_to_scalar",
  "scalar_to_sexp",
  "sexp_to_buffer",
  "buffer_to_sexp",
  "boundary"
)
TCCQ_BACKEND_FUNCTION_KINDS <- c("scalar", "loop_nest", "structured")
TCCQ_BACKEND_FUNCTION_ABIS <- c("c", "fortran_bind_c")
TCCQ_BACKEND_RESULT_PLACEMENTS <- c("return", "output_argument")
TCCQ_RUNTIME_STATUS_CLASS <- "tccq_runtime_status"
TCCQ_BACKEND_ARTIFACT_KINDS <- c("source", "shared_library", "jit_callable", "native_callable")
TCCQ_BACKEND_VALUE_BINDING_ROLES <- c("parameter", "local")

#' Backend planning context
#'
#' @param mode Requested backend mode.
#' @param target Requested target language or runtime, or `any`.
#' @param allow_boundary Whether explicit R call-evaluation boundary plans are
#'   allowed.
#' @param required_capabilities Capabilities that candidate backends must expose.
#' @param attrs Structured backend-context attributes.
#' @export
TccqBackendContext <- S7::new_class(
  "TccqBackendContext",
  package = "tccquickr",
  properties = list(
    mode = S7::class_character,
    target = S7::class_character,
    allow_boundary = S7::class_logical,
    required_capabilities = S7::class_character,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@mode) != 1L || is.na(self@mode) || !self@mode %in% TCCQ_BACKEND_MODES) {
      problems <- c(problems, "@mode must be one supported backend mode")
    }
    if (length(self@target) != 1L || is.na(self@target) || !nzchar(self@target)) {
      problems <- c(problems, "@target must be a single non-empty string")
    }
    if (length(self@allow_boundary) != 1L || is.na(self@allow_boundary)) {
      problems <- c(problems, "@allow_boundary must be a single TRUE/FALSE value")
    }
    unsupported_required_capabilities <- setdiff(
      unique(self@required_capabilities),
      TCCQ_BACKEND_CAPABILITIES
    )
    if (anyNA(self@required_capabilities) || length(unsupported_required_capabilities) > 0L) {
      problems <- c(problems, "@required_capabilities must contain only supported backend capabilities")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend bridge plan
#'
#' Bridges model representation transitions such as `SEXP -> host buffer`,
#' `host -> device`, layout conversion, tile materialization, or R
#' call-evaluation boundaries. They are explicit backend-plan values, not
#' emitter branches.
#'
#' @param id Stable bridge id.
#' @param kind Bridge kind.
#' @param from_space Source memory space.
#' @param to_space Destination memory space.
#' @param from_type Optional source type.
#' @param to_type Optional destination type.
#' @param effect Bridge effect summary.
#' @param attrs Structured bridge attributes.
#' @export
TccqBridgePlan <- S7::new_class(
  "TccqBridgePlan",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    kind = S7::class_character,
    from_space = S7::class_character,
    to_space = S7::class_character,
    from_type = S7::new_union(NULL, TccqType),
    to_type = S7::new_union(NULL, TccqType),
    effect = TccqEffect,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (length(self@kind) != 1L || is.na(self@kind) || !self@kind %in% TCCQ_BRIDGE_KINDS) {
      problems <- c(problems, "@kind must be one supported bridge kind")
    }
    if (
      length(self@from_space) != 1L ||
        is.na(self@from_space) ||
        !self@from_space %in% TCCQ_MEMORY_SPACES
    ) {
      problems <- c(problems, "@from_space must be one supported memory space")
    }
    if (
      length(self@to_space) != 1L ||
        is.na(self@to_space) ||
        !self@to_space %in% TCCQ_MEMORY_SPACES
    ) {
      problems <- c(problems, "@to_space must be one supported memory space")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend value binding
#'
#' A value binding assigns one generated source identifier and source type to a
#' neutral value. Parameters and scalar locals share this representation so
#' their names, identities, and types cannot drift in parallel arrays.
#'
#' @param source_name Generated source identifier.
#' @param value_id Neutral value id.
#' @param source_type Type represented by the generated identifier.
#' @param role Whether the identifier is an ABI parameter or scalar local.
#' @export
TccqBackendValueBinding <- S7::new_class(
  "TccqBackendValueBinding",
  package = "tccquickr",
  properties = list(
    source_name = S7::class_character,
    value_id = S7::class_character,
    source_type = TccqType,
    role = S7::class_character
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@source_name) != 1L || is.na(self@source_name) || !nzchar(self@source_name)) {
      problems <- c(problems, "@source_name must be a single non-empty string")
    }
    if (length(self@value_id) != 1L || is.na(self@value_id) || !nzchar(self@value_id)) {
      problems <- c(problems, "@value_id must be a single non-empty string")
    }
    if (
      length(self@role) != 1L ||
        is.na(self@role) ||
        !self@role %in% TCCQ_BACKEND_VALUE_BINDING_ROLES
    ) {
      problems <- c(problems, "@role must be `parameter` or `local`")
    }
    if (identical(self@role, "local") && self@source_type@shape@rank != 0L) {
      problems <- c(problems, "local backend value bindings must have scalar source types")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend allocation binding
#'
#' An allocation binding assigns one generated source identifier to one typed
#' physical allocation. Every materialized slot sharing that allocation is
#' carried by the same binding.
#'
#' @param source_name Generated source identifier.
#' @param allocation Physical allocation represented by the identifier.
#' @param slots Materialized temporary slots sharing the allocation.
#' @export
TccqBackendAllocationBinding <- S7::new_class(
  "TccqBackendAllocationBinding",
  package = "tccquickr",
  properties = list(
    source_name = S7::class_character,
    allocation = TccqStorageAllocation,
    slots = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@source_name) != 1L || is.na(self@source_name) || !nzchar(self@source_name)) {
      problems <- c(problems, "@source_name must be a single non-empty string")
    }
    slots_are_typed <- vapply(
      self@slots,
      S7::S7_inherits,
      logical(1),
      class = TccqStorageSlot
    )
    if (length(self@slots) == 0L || !all(slots_are_typed)) {
      problems <- c(problems, "@slots must contain at least one <TccqStorageSlot> value")
    }
    if (all(slots_are_typed) && length(self@slots) > 0L) {
      value_ids <- vapply(self@slots, function(slot) slot@value_id, character(1))
      if (anyDuplicated(value_ids)) {
        problems <- c(problems, "@slots must have unique value ids")
      }
      valid_slots <- vapply(self@slots, function(slot) {
        identical(slot@role, "temporary") &&
          isTRUE(slot@materialized) &&
          S7::S7_inherits(slot@allocation, TccqStorageAllocation) &&
          identical(slot@allocation, self@allocation)
      }, logical(1))
      if (!all(valid_slots)) {
        problems <- c(
          problems,
          "@slots must be materialized temporaries owning the bound allocation"
        )
      }
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend extent binding
#'
#' An extent binding assigns one generated ABI parameter to one declared
#' dimension symbol.
#'
#' @param source_name Generated extent parameter name.
#' @param symbol Declared dimension symbol.
#' @export
TccqBackendExtentBinding <- S7::new_class(
  "TccqBackendExtentBinding",
  package = "tccquickr",
  properties = list(
    source_name = S7::class_character,
    symbol = S7::class_character
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@source_name) != 1L || is.na(self@source_name) || !nzchar(self@source_name)) {
      problems <- c(problems, "@source_name must be a single non-empty string")
    }
    if (length(self@symbol) != 1L || is.na(self@symbol) || !nzchar(self@symbol)) {
      problems <- c(problems, "@symbol must be a single non-empty string")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend callable error channel
#'
#' Generated code writes zero on success or the one-based position of a
#' diagnostic on failure. Wrappers turn the status back into the corresponding
#' classed runtime diagnostic.
#'
#' @param source_name Generated status-output parameter name.
#' @param diagnostics Ordered runtime diagnostics addressable by status code.
#' @export
TccqBackendErrorChannel <- S7::new_class(
  "TccqBackendErrorChannel",
  package = "tccquickr",
  properties = list(
    source_name = S7::class_character,
    diagnostics = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@source_name) != 1L || is.na(self@source_name) || !nzchar(self@source_name)) {
      problems <- c(problems, "@source_name must be a single non-empty string")
    }
    diagnostics_are_typed <- vapply(
      self@diagnostics,
      S7::S7_inherits,
      logical(1),
      class = TccqDiagnostic
    )
    if (length(self@diagnostics) == 0L || !all(diagnostics_are_typed)) {
      problems <- c(problems, "@diagnostics must contain typed runtime diagnostics")
    }
    if (all(diagnostics_are_typed) && any(vapply(
      self@diagnostics,
      function(diagnostic) !identical(diagnostic@phase, "runtime"),
      logical(1)
    ))) {
      problems <- c(problems, "error-channel diagnostics must belong to the runtime phase")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend function interface
#'
#' A backend function interface is the source-level callable boundary consumed
#' by C, Rtinycc, Fortran, and later source printers. Generated parameters,
#' scalar locals, physical allocations, and extents are closed typed bindings;
#' positional arrays are not an interface protocol.
#'
#' @param symbol Generated function symbol.
#' @param source_language Source language consumed by the printer.
#' @param kind Function shape: `scalar`, `loop_nest`, or `structured`.
#' @param abi Callable ABI.
#' @param parameters Ordered ABI parameter bindings.
#' @param locals Ordered scalar-local bindings.
#' @param allocations Ordered physical-allocation bindings.
#' @param result_value_id Lowered result value id.
#' @param result_type Declared semantic result type.
#' @param result_placement Whether the result is returned or passed by output argument.
#' @param result_name Generated result/output variable name, or empty string.
#' @param domain Loop-nest iteration domain, or `NULL`.
#' @param extents Ordered symbolic-extent bindings carried by the ABI.
#' @param index_names Generated data-parallel and structured loop index names.
#' @param result_dims Typed result dimensions.
#' @param result_count_name Generated result element-count parameter name, or
#'   empty string for scalar results.
#' @param error_channel Optional generated callable error channel.
#' @export
TccqBackendFunctionInterface <- S7::new_class(
  "TccqBackendFunctionInterface",
  package = "tccquickr",
  properties = list(
    symbol = S7::class_character,
    source_language = S7::class_character,
    kind = S7::class_character,
    abi = S7::class_character,
    parameters = S7::class_list,
    locals = S7::class_list,
    allocations = S7::class_list,
    result_value_id = S7::class_character,
    result_type = TccqType,
    result_placement = S7::class_character,
    result_name = S7::class_character,
    domain = S7::new_union(NULL, TccqDomain),
    extents = S7::class_list,
    index_names = S7::class_character,
    result_dims = S7::class_list,
    result_count_name = S7::class_character,
    error_channel = S7::new_union(NULL, TccqBackendErrorChannel)
  ),
  validator = function(self) {
    problems <- character()
    scalar_strings <- list(
      symbol = self@symbol,
      source_language = self@source_language,
      kind = self@kind,
      abi = self@abi,
      result_value_id = self@result_value_id,
      result_placement = self@result_placement,
      result_name = self@result_name,
      result_count_name = self@result_count_name
    )
    for (field_name in names(scalar_strings)) {
      field_value <- scalar_strings[[field_name]]
      if (length(field_value) != 1L || is.na(field_value)) {
        problems <- c(problems, sprintf("@%s must be a single string", field_name))
      }
    }
    if (length(self@symbol) == 1L && !is.na(self@symbol) && !nzchar(self@symbol)) {
      problems <- c(problems, "@symbol must be non-empty")
    }
    if (
      length(self@source_language) == 1L &&
        !is.na(self@source_language) &&
        !self@source_language %in% TCCQ_OP_RENDER_LANGUAGES
    ) {
      problems <- c(problems, "@source_language must be one supported source language")
    }
    if (
      length(self@kind) == 1L &&
        !is.na(self@kind) &&
        !self@kind %in% TCCQ_BACKEND_FUNCTION_KINDS
    ) {
      problems <- c(problems, "@kind must be one supported backend function kind")
    }
    if (
      length(self@abi) == 1L &&
        !is.na(self@abi) &&
        !self@abi %in% TCCQ_BACKEND_FUNCTION_ABIS
    ) {
      problems <- c(problems, "@abi must be one supported backend function ABI")
    }
    if (
      length(self@result_placement) == 1L &&
        !is.na(self@result_placement) &&
        !self@result_placement %in% TCCQ_BACKEND_RESULT_PLACEMENTS
    ) {
      problems <- c(problems, "@result_placement must be one supported result placement")
    }
    parameters_are_typed <- vapply(
      self@parameters,
      S7::S7_inherits,
      logical(1),
      class = TccqBackendValueBinding
    )
    if (!all(parameters_are_typed) || any(vapply(
      self@parameters[parameters_are_typed],
      function(binding) !identical(binding@role, "parameter"),
      logical(1)
    ))) {
      problems <- c(problems, "@parameters must contain only parameter value bindings")
    }
    locals_are_typed <- vapply(
      self@locals,
      S7::S7_inherits,
      logical(1),
      class = TccqBackendValueBinding
    )
    if (!all(locals_are_typed) || any(vapply(
      self@locals[locals_are_typed],
      function(binding) !identical(binding@role, "local"),
      logical(1)
    ))) {
      problems <- c(problems, "@locals must contain only local value bindings")
    }
    allocations_are_typed <- vapply(
      self@allocations,
      S7::S7_inherits,
      logical(1),
      class = TccqBackendAllocationBinding
    )
    if (!all(allocations_are_typed)) {
      problems <- c(problems, "@allocations must contain only <TccqBackendAllocationBinding> values")
    }
    extents_are_typed <- vapply(
      self@extents,
      S7::S7_inherits,
      logical(1),
      class = TccqBackendExtentBinding
    )
    if (!all(extents_are_typed)) {
      problems <- c(problems, "@extents must contain only <TccqBackendExtentBinding> values")
    }
    value_bindings <- c(self@parameters[parameters_are_typed], self@locals[locals_are_typed])
    if (length(value_bindings) > 0L) {
      value_ids <- vapply(value_bindings, function(binding) binding@value_id, character(1))
      if (anyDuplicated(value_ids)) {
        problems <- c(problems, "parameter and local bindings must have unique value ids")
      }
    }
    if (all(allocations_are_typed) && length(self@allocations) > 0L) {
      allocation_ids <- vapply(
        self@allocations,
        function(binding) binding@allocation@id,
        character(1)
      )
      if (anyDuplicated(allocation_ids)) {
        problems <- c(problems, "@allocations must have unique physical allocation ids")
      }
      slot_value_ids <- unlist(lapply(
        self@allocations,
        function(binding) vapply(binding@slots, function(slot) slot@value_id, character(1))
      ), use.names = FALSE)
      if (anyDuplicated(slot_value_ids)) {
        problems <- c(problems, "allocation bindings must not share storage slots")
      }
    }
    if (all(extents_are_typed) && length(self@extents) > 0L) {
      extent_symbols <- vapply(self@extents, function(binding) binding@symbol, character(1))
      if (anyDuplicated(extent_symbols)) {
        problems <- c(problems, "@extents must have unique dimension symbols")
      }
    }
    generated_names <- c(
      vapply(value_bindings, function(binding) binding@source_name, character(1)),
      if (all(allocations_are_typed)) {
        vapply(self@allocations, function(binding) binding@source_name, character(1))
      },
      if (all(extents_are_typed)) {
        vapply(self@extents, function(binding) binding@source_name, character(1))
      },
      self@index_names,
      self@result_name,
      self@result_count_name,
      if (is.null(self@error_channel)) "" else self@error_channel@source_name
    )
    generated_names <- generated_names[nzchar(generated_names)]
    if (anyDuplicated(generated_names)) {
      problems <- c(problems, "generated function-scope identifiers must be unique")
    }
    if (!is.null(self@domain)) {
      if (length(self@index_names) < self@domain@shape@rank) {
        problems <- c(problems, "@index_names must cover at least the @domain rank")
      }
    }
    if (anyNA(self@index_names) || any(!nzchar(self@index_names)) || anyDuplicated(self@index_names)) {
      problems <- c(problems, "@index_names must contain unique non-empty strings")
    }
    result_dims_are_typed <- vapply(self@result_dims, S7::S7_inherits, logical(1), class = TccqDim)
    if (!all(result_dims_are_typed)) {
      problems <- c(problems, "@result_dims must contain only <TccqDim> values")
    }
    if (length(self@result_dims) > 0L && !nzchar(self@result_count_name)) {
      problems <- c(problems, "non-scalar results must have a result-count name")
    }
    if (length(self@result_dims) == 0L && nzchar(self@result_count_name)) {
      problems <- c(problems, "scalar results must not have a result-count name")
    }
    if (!identical(self@result_dims, self@result_type@shape@dims)) {
      problems <- c(problems, "@result_dims must match @result_type shape dimensions")
    }
    if (identical(self@kind, "loop_nest") && (is.null(self@domain) || self@domain@shape@rank == 0L)) {
      problems <- c(problems, "loop-nest interfaces must carry a non-scalar iteration domain")
    }
    if (identical(self@result_placement, "output_argument") && !nzchar(self@result_name)) {
      problems <- c(problems, "output-argument results must have a result name")
    }
    if (identical(self@source_language, "fortran") && !identical(self@abi, "fortran_bind_c")) {
      problems <- c(problems, "Fortran source interfaces must use the fortran_bind_c ABI")
    }
    if (identical(self@source_language, "c") && !identical(self@abi, "c")) {
      problems <- c(problems, "C source interfaces must use the C ABI")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend artifact
#'
#' Backend artifacts are concrete products of a backend plan: generated source,
#' a compiled shared library, or a JIT callable. They are explicit plan values
#' so source, shared-library, and JIT modes cannot silently collapse into the
#' same loose string attribute.
#'
#' @param id Stable artifact id.
#' @param kind Artifact kind.
#' @param source_language Source language used to produce the artifact, or empty.
#' @param path Filesystem path for file-backed artifacts, or empty.
#' @param attrs Structured artifact metadata.
#' @export
TccqBackendArtifact <- S7::new_class(
  "TccqBackendArtifact",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    kind = S7::class_character,
    source_language = S7::class_character,
    path = S7::class_character,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    scalar_strings <- list(
      id = self@id,
      kind = self@kind,
      source_language = self@source_language,
      path = self@path
    )
    for (field_name in names(scalar_strings)) {
      field_value <- scalar_strings[[field_name]]
      if (length(field_value) != 1L || is.na(field_value)) {
        problems <- c(problems, sprintf("@%s must be a single string", field_name))
      }
    }
    if (length(self@id) == 1L && !is.na(self@id) && !nzchar(self@id)) {
      problems <- c(problems, "@id must be non-empty")
    }
    if (
      length(self@kind) == 1L &&
        !is.na(self@kind) &&
        !self@kind %in% TCCQ_BACKEND_ARTIFACT_KINDS
    ) {
      problems <- c(problems, "@kind must be one supported backend artifact kind")
    }
    if (
      length(self@source_language) == 1L &&
        !is.na(self@source_language) &&
        nzchar(self@source_language) &&
        !self@source_language %in% TCCQ_OP_RENDER_LANGUAGES
    ) {
      problems <- c(problems, "@source_language must be empty or one supported source language")
    }
    if (
      length(self@kind) == 1L &&
        !is.na(self@kind) &&
        self@kind %in% c("source", "shared_library") &&
        !nzchar(self@source_language)
    ) {
      problems <- c(problems, "source-backed artifacts must record a source language")
    }
    if (
      identical(self@kind, "source") &&
        !nzchar(self@path) &&
        (
          is.null(self@attrs$text) ||
            length(self@attrs$text) != 1L ||
            is.na(self@attrs$text) ||
            !is.character(self@attrs$text) ||
            !nzchar(self@attrs$text)
        )
    ) {
      problems <- c(problems, "source artifacts must carry inline source text or a path")
    }
    if (identical(self@kind, "shared_library") && !nzchar(self@path)) {
      problems <- c(problems, "shared-library artifacts must carry a filesystem path")
    }
    if (
      self@kind %in% c("jit_callable", "native_callable") &&
        (is.null(self@attrs$callable) || !is.function(self@attrs$callable))
    ) {
      problems <- c(problems, "callable artifacts must carry a callable function")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend products
#'
#' Backend products are the typed outputs owned by a backend plan: the callable
#' interface, loop nest, expression or statement block consumed by the source
#' printer, storage plan, and concrete artifacts. Runtime handles such as loaded
#' libraries and compiled callables stay in `attrs`, but under this typed
#' products value rather than directly on the backend plan.
#'
#' @param function_interface Source-level callable interface, if source exists.
#' @param body Expression tree or statement block consumed by the backend, if
#'   source exists.
#' @param loop_nest Result loop nest consumed by the backend, if source exists.
#' @param loop_nests Ordered loop nests, intermediates first, result nest last.
#' @param storage_plan Storage plan consumed by the backend, if available.
#' @param artifacts Named backend artifacts.
#' @param attrs Structured product metadata.
#' @export
TccqBackendProducts <- S7::new_class(
  "TccqBackendProducts",
  package = "tccquickr",
  properties = list(
    function_interface = S7::new_union(NULL, TccqBackendFunctionInterface),
    body = S7::new_union(NULL, TccqExpression, TccqValueBlock),
    loop_nest = S7::new_union(NULL, TccqLoopNest),
    loop_nests = S7::class_list,
    storage_plan = S7::new_union(NULL, TccqStoragePlan),
    artifacts = S7::class_list,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    nests_are_typed <- vapply(
      self@loop_nests,
      S7::S7_inherits,
      logical(1),
      class = TccqLoopNest
    )
    if (!all(nests_are_typed)) {
      problems <- c(problems, "@loop_nests must contain only <TccqLoopNest> values")
    }
    if (
      !is.null(self@loop_nest) &&
        !is.null(self@body) &&
        !identical(self@body, self@loop_nest@body)
    ) {
      problems <- c(problems, "@body must match @loop_nest@body")
    }
    artifacts_are_typed <- vapply(
      self@artifacts,
      S7::S7_inherits,
      logical(1),
      class = TccqBackendArtifact
    )
    artifact_ids <- names(self@artifacts)
    if (!all(artifacts_are_typed)) {
      problems <- c(problems, "@artifacts must contain only <TccqBackendArtifact> values")
    }
    if (
      length(self@artifacts) > 0L &&
        (is.null(artifact_ids) || anyNA(artifact_ids) || any(!nzchar(artifact_ids)) || anyDuplicated(artifact_ids))
    ) {
      problems <- c(problems, "@artifacts must be named by unique non-empty artifact roles")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend plan
#'
#' @param id Stable plan id.
#' @param backend_id Backend spec id.
#' @param family Backend family.
#' @param mode Backend mode.
#' @param target Target language or runtime.
#' @param capabilities Backend capabilities visible to this plan.
#' @param regions Regions assigned to the backend plan.
#' @param bridges Representation-transition plans.
#' @param diagnostics Diagnostics attached to this plan.
#' @param products Typed backend products.
#' @param attrs Structured plan attributes.
#' @export
TccqBackendPlan <- S7::new_class(
  "TccqBackendPlan",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    backend_id = S7::class_character,
    family = S7::class_character,
    mode = S7::class_character,
    target = S7::class_character,
    capabilities = S7::class_character,
    regions = S7::class_list,
    bridges = S7::class_list,
    diagnostics = S7::class_list,
    products = S7::new_union(NULL, TccqBackendProducts),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (length(self@backend_id) != 1L || is.na(self@backend_id) || !nzchar(self@backend_id)) {
      problems <- c(problems, "@backend_id must be a single non-empty string")
    }
    if (length(self@family) != 1L || is.na(self@family) || !self@family %in% TCCQ_BACKEND_FAMILIES) {
      problems <- c(problems, "@family must be one supported backend family")
    }
    if (length(self@mode) != 1L || is.na(self@mode) || !self@mode %in% TCCQ_BACKEND_MODES) {
      problems <- c(problems, "@mode must be one supported backend mode")
    }
    if (length(self@target) != 1L || is.na(self@target) || !nzchar(self@target)) {
      problems <- c(problems, "@target must be a single non-empty string")
    }
    unsupported_capabilities <- setdiff(unique(self@capabilities), TCCQ_BACKEND_CAPABILITIES)
    if (anyNA(self@capabilities) || length(unsupported_capabilities) > 0L) {
      problems <- c(problems, "@capabilities must contain only supported backend capabilities")
    }
    regions_are_tccq_regions <- vapply(self@regions, S7::S7_inherits, logical(1), class = TccqRegion)
    bridges_are_tccq_bridge_plans <- vapply(
      self@bridges,
      S7::S7_inherits,
      logical(1),
      class = TccqBridgePlan
    )
    diagnostics_are_tccq_diagnostics <- vapply(
      self@diagnostics,
      S7::S7_inherits,
      logical(1),
      class = TccqDiagnostic
    )
    if (!all(regions_are_tccq_regions)) {
      problems <- c(problems, "@regions must contain only <TccqRegion> values")
    }
    if (!all(bridges_are_tccq_bridge_plans)) {
      problems <- c(problems, "@bridges must contain only <TccqBridgePlan> values")
    }
    if (!all(diagnostics_are_tccq_diagnostics)) {
      problems <- c(problems, "@diagnostics must contain only <TccqDiagnostic> values")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend plan set
#'
#' A plan set is the neutral result of asking several backend families to
#' account for the same typed program. It exists to keep the core honest: C,
#' Fortran, and future graph, device, or R call-evaluation implementations must
#' report their constraints through the same contract.
#'
#' @param program_name Program name.
#' @param plans Backend plans.
#' @param diagnostics Diagnostics collected while planning.
#' @param attrs Structured plan-set attributes.
#' @export
TccqBackendPlanSet <- S7::new_class(
  "TccqBackendPlanSet",
  package = "tccquickr",
  properties = list(
    program_name = S7::class_character,
    plans = S7::class_list,
    diagnostics = S7::class_list,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@program_name) != 1L || is.na(self@program_name) || !nzchar(self@program_name)) {
      problems <- c(problems, "@program_name must be a single non-empty string")
    }
    plans_are_tccq_backend_plans <- vapply(
      self@plans,
      S7::S7_inherits,
      logical(1),
      class = TccqBackendPlan
    )
    diagnostics_are_tccq_diagnostics <- vapply(
      self@diagnostics,
      S7::S7_inherits,
      logical(1),
      class = TccqDiagnostic
    )
    if (!all(plans_are_tccq_backend_plans)) {
      problems <- c(problems, "@plans must contain only <TccqBackendPlan> values")
    }
    if (!all(diagnostics_are_tccq_diagnostics)) {
      problems <- c(problems, "@diagnostics must contain only <TccqDiagnostic> values")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend implementation descriptor
#'
#' @param id Stable backend id.
#' @param family Backend family.
#' @param target Target language or runtime.
#' @param driver Concrete driver name.
#' @param modes Supported backend modes.
#' @param region_kinds Supported region kinds.
#' @param memory_spaces Supported memory spaces.
#' @param capabilities Capabilities exposed by this backend.
#' @param uses_rapi Whether this backend may touch the R C API.
#' @param attrs Structured backend attributes.
#' @param prepare Planning implementation.
#' @export
TccqBackendSpec <- S7::new_class(
  "TccqBackendSpec",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    family = S7::class_character,
    target = S7::class_character,
    driver = S7::class_character,
    modes = S7::class_character,
    region_kinds = S7::class_character,
    memory_spaces = S7::class_character,
    capabilities = S7::class_character,
    uses_rapi = S7::class_logical,
    attrs = S7::class_list,
    prepare = S7::class_function
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (length(self@family) != 1L || is.na(self@family) || !self@family %in% TCCQ_BACKEND_FAMILIES) {
      problems <- c(problems, "@family must be one supported backend family")
    }
    if (length(self@target) != 1L || is.na(self@target) || !nzchar(self@target)) {
      problems <- c(problems, "@target must be a single non-empty string")
    }
    if (length(self@driver) != 1L || is.na(self@driver) || !nzchar(self@driver)) {
      problems <- c(problems, "@driver must be a single non-empty string")
    }
    unsupported_modes <- setdiff(unique(self@modes), TCCQ_BACKEND_MODES)
    if (length(self@modes) == 0L || anyNA(self@modes) || length(unsupported_modes) > 0L) {
      problems <- c(problems, "@modes must contain supported backend modes")
    }
    unsupported_region_kinds <- setdiff(unique(self@region_kinds), TCCQ_REGION_KINDS)
    if (
      length(self@region_kinds) == 0L ||
        anyNA(self@region_kinds) ||
        length(unsupported_region_kinds) > 0L
    ) {
      problems <- c(problems, "@region_kinds must contain supported region kinds")
    }
    unsupported_memory_spaces <- setdiff(unique(self@memory_spaces), TCCQ_MEMORY_SPACES)
    if (
      length(self@memory_spaces) == 0L ||
        anyNA(self@memory_spaces) ||
        length(unsupported_memory_spaces) > 0L
    ) {
      problems <- c(problems, "@memory_spaces must contain supported memory spaces")
    }
    unsupported_capabilities <- setdiff(unique(self@capabilities), TCCQ_BACKEND_CAPABILITIES)
    if (
      length(self@capabilities) == 0L ||
        anyNA(self@capabilities) ||
        length(unsupported_capabilities) > 0L
    ) {
      problems <- c(problems, "@capabilities must contain supported backend capabilities")
    }
    if (length(self@uses_rapi) != 1L || is.na(self@uses_rapi)) {
      problems <- c(problems, "@uses_rapi must be a single TRUE/FALSE value")
    }
    if (!is.function(self@prepare)) {
      problems <- c(problems, "@prepare must be a backend planning function")
    }
    if (length(problems) > 0L) problems
  }
)

#' Prepare a backend plan
#'
#' @param backend Backend implementation.
#' @param program Program to plan.
#' @param context Backend planning context.
#' @export
tccq_backend_prepare <- S7::new_generic(
  "tccq_backend_prepare",
  dispatch_args = c("backend", "program", "context"),
  function(backend, program, context) S7::S7_dispatch()
)

#' Backend implementation trait
#'
#' Backend implementations must opt into this trait before compiler code uses
#' them for planning.
#'
#' @export
TccqBackend <- s7contract::new_trait(
  "TccqBackend",
  package = "tccquickr",
  methods = list(
    prepare = s7contract::trait_method(
      tccq_backend_prepare,
      args = list(program = TccqProgram, context = TccqBackendContext),
      returns = TccqResult
    )
  )
)

tccq_register_backend_traits <- function() {
  empty_backend_plan <- function(backend, context, diagnostics = list()) {
    tccq_backend_plan(
      id = sprintf("%s.%s.plan", backend@id, context@mode),
      backend_id = backend@id,
      family = backend@family,
      mode = context@mode,
      target = if (identical(context@target, "any")) backend@target else context@target,
      capabilities = backend@capabilities,
      diagnostics = diagnostics,
      attrs = list(driver = backend@driver)
    )
  }

  backend_context_diagnostic <- function(backend, context) {
    if (!context@mode %in% backend@modes) {
      return(tccq_diagnostic(
        "backend.unsupported_mode",
        "Backend does not support the requested mode.",
        phase = "backend",
        path = "backend_context.mode",
        data = list(
          backend = backend@id,
          requested = context@mode,
          supported = backend@modes
        )
      ))
    }

    if (!identical(context@target, "any") && !identical(context@target, backend@target)) {
      return(tccq_diagnostic(
        "backend.unsupported_target",
        "Backend does not support the requested target.",
        phase = "backend",
        path = "backend_context.target",
        data = list(
          backend = backend@id,
          requested = context@target,
          supported = backend@target
        )
      ))
    }

    missing_capabilities <- setdiff(context@required_capabilities, backend@capabilities)
    if (length(missing_capabilities) > 0L) {
      return(tccq_diagnostic(
        "backend.missing_capability",
        "Backend does not expose all requested capabilities.",
        phase = "backend",
        path = "backend_context.required_capabilities",
        data = list(
          backend = backend@id,
          requested = context@required_capabilities,
          missing = missing_capabilities,
          supported = backend@capabilities
        )
      ))
    }

    NULL
  }

  s7contract::impl_trait(
    TccqBackend,
    TccqBackendSpec,
    methods = list(
      prepare = function(backend, program, context) {
        context_diagnostic <- backend_context_diagnostic(backend, context)
        if (!is.null(context_diagnostic)) {
          plan <- empty_backend_plan(backend, context, diagnostics = list(context_diagnostic))
          return(tccq_result(success = FALSE, value = plan, diagnostics = list(context_diagnostic)))
        }

        result <- backend@prepare(backend, program, context)
        .tccq_check_s7(result, TccqResult, "TccqResult", "backend result")
        result
      }
    ),
    replace = TRUE
  )
  invisible(TRUE)
}

#' Construct a backend planning context
#'
#' @param mode Requested backend mode.
#' @param target Requested target language or runtime, or `any`.
#' @param allow_boundary Whether explicit R call-evaluation boundary plans are
#'   allowed.
#' @param required_capabilities Capabilities that candidate backends must expose.
#' @param attrs Structured backend-context attributes.
#' @export
tccq_backend_context <- function(
  mode = "source",
  target = "any",
  allow_boundary = FALSE,
  required_capabilities = character(),
  attrs = list()
) {
  .tccq_check_character_scalar(mode, "mode")
  if (!mode %in% TCCQ_BACKEND_MODES) {
    tccq_abort(
      "schema.invalid_backend_mode",
      "`mode` is not a supported backend mode.",
      phase = "schema",
      path = "backend_context.mode",
      data = list(mode = mode, supported = TCCQ_BACKEND_MODES)
    )
  }
  .tccq_check_character_scalar(target, "target")
  .tccq_check_logical_scalar(allow_boundary, "allow_boundary")
  .tccq_check_character_set(
    required_capabilities,
    TCCQ_BACKEND_CAPABILITIES,
    "required_capabilities",
    allow_empty = TRUE
  )
  .tccq_check_list(attrs, "attrs")

  TccqBackendContext(
    mode = mode,
    target = target,
    allow_boundary = allow_boundary,
    required_capabilities = required_capabilities,
    attrs = attrs
  )
}

#' Construct a backend bridge plan
#'
#' @param id Stable bridge id.
#' @param kind Bridge kind.
#' @param from_space Source memory space.
#' @param to_space Destination memory space.
#' @param from_type Optional source type.
#' @param to_type Optional destination type.
#' @param effect Bridge effect summary.
#' @param attrs Structured bridge attributes.
#' @export
tccq_bridge_plan <- function(
  id,
  kind,
  from_space,
  to_space,
  from_type = NULL,
  to_type = NULL,
  effect = tccq_effect(reads = TRUE),
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_BRIDGE_KINDS) {
    tccq_abort(
      "schema.invalid_bridge_kind",
      "`kind` is not a supported bridge kind.",
      phase = "schema",
      path = "bridge.kind",
      data = list(kind = kind, supported = TCCQ_BRIDGE_KINDS)
    )
  }
  .tccq_check_memory_space(from_space, "from_space")
  .tccq_check_memory_space(to_space, "to_space")
  .tccq_check_optional_s7(from_type, TccqType, "TccqType", "from_type")
  .tccq_check_optional_s7(to_type, TccqType, "TccqType", "to_type")
  .tccq_check_s7(effect, TccqEffect, "TccqEffect", "effect")
  .tccq_check_list(attrs, "attrs")

  TccqBridgePlan(
    id = id,
    kind = kind,
    from_space = from_space,
    to_space = to_space,
    from_type = from_type,
    to_type = to_type,
    effect = effect,
    attrs = attrs
  )
}

#' Construct a backend value binding
#'
#' @inheritParams TccqBackendValueBinding
#' @export
tccq_backend_value_binding <- function(source_name, value_id, source_type, role) {
  .tccq_check_character_scalar(source_name, "source_name")
  .tccq_check_character_scalar(value_id, "value_id")
  .tccq_check_s7(source_type, TccqType, "TccqType", "source_type")
  .tccq_check_character_scalar(role, "role")
  TccqBackendValueBinding(
    source_name = source_name,
    value_id = value_id,
    source_type = source_type,
    role = role
  )
}

#' Construct a backend allocation binding
#'
#' @inheritParams TccqBackendAllocationBinding
#' @export
tccq_backend_allocation_binding <- function(source_name, allocation, slots) {
  .tccq_check_character_scalar(source_name, "source_name")
  .tccq_check_s7(allocation, TccqStorageAllocation, "TccqStorageAllocation", "allocation")
  .tccq_check_list_of(slots, TccqStorageSlot, "TccqStorageSlot", "slots")
  TccqBackendAllocationBinding(
    source_name = source_name,
    allocation = allocation,
    slots = slots
  )
}

#' Construct a backend extent binding
#'
#' @inheritParams TccqBackendExtentBinding
#' @export
tccq_backend_extent_binding <- function(source_name, symbol) {
  .tccq_check_character_scalar(source_name, "source_name")
  .tccq_check_character_scalar(symbol, "symbol")
  TccqBackendExtentBinding(source_name = source_name, symbol = symbol)
}

#' Construct a backend callable error channel
#'
#' @inheritParams TccqBackendErrorChannel
#' @export
tccq_backend_error_channel <- function(source_name, diagnostics) {
  .tccq_check_character_scalar(source_name, "source_name")
  .tccq_check_list_of(
    diagnostics,
    TccqDiagnostic,
    "TccqDiagnostic",
    "diagnostics"
  )
  TccqBackendErrorChannel(
    source_name = source_name,
    diagnostics = diagnostics
  )
}

#' Construct a backend function interface
#'
#' @inheritParams TccqBackendFunctionInterface
#' @export
tccq_backend_function_interface <- function(
  symbol,
  source_language,
  kind,
  abi = "",
  parameters = list(),
  locals = list(),
  allocations = list(),
  result_value_id,
  result_type,
  result_placement = "return",
  result_name = "",
  domain = NULL,
  extents = list(),
  index_names = character(),
  result_dims = list(),
  result_count_name = "",
  error_channel = NULL
) {
  .tccq_check_character_scalar(symbol, "symbol")
  .tccq_check_character_scalar(source_language, "source_language")
  if (!source_language %in% TCCQ_OP_RENDER_LANGUAGES) {
    tccq_abort(
      "schema.invalid_backend_function_source_language",
      "`source_language` is not supported.",
      phase = "schema",
      path = "backend_function.source_language",
      data = list(source_language = source_language, supported = TCCQ_OP_RENDER_LANGUAGES)
    )
  }
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_BACKEND_FUNCTION_KINDS) {
    tccq_abort(
      "schema.invalid_backend_function_kind",
      "`kind` is not a supported backend function kind.",
      phase = "schema",
      path = "backend_function.kind",
      data = list(kind = kind, supported = TCCQ_BACKEND_FUNCTION_KINDS)
    )
  }
  .tccq_check_character_or_empty(abi, "abi")
  if (!nzchar(abi)) {
    abi <- switch(
      source_language,
      c = "c",
      fortran = "fortran_bind_c",
      ""
    )
  }
  if (!abi %in% TCCQ_BACKEND_FUNCTION_ABIS) {
    tccq_abort(
      "schema.invalid_backend_function_abi",
      "`abi` is not a supported backend function ABI.",
      phase = "schema",
      path = "backend_function.abi",
      data = list(abi = abi, supported = TCCQ_BACKEND_FUNCTION_ABIS)
    )
  }
  .tccq_check_list_of(
    parameters,
    TccqBackendValueBinding,
    "TccqBackendValueBinding",
    "parameters"
  )
  .tccq_check_list_of(locals, TccqBackendValueBinding, "TccqBackendValueBinding", "locals")
  .tccq_check_list_of(
    allocations,
    TccqBackendAllocationBinding,
    "TccqBackendAllocationBinding",
    "allocations"
  )
  .tccq_check_character_scalar(result_value_id, "result_value_id")
  .tccq_check_s7(result_type, TccqType, "TccqType", "result_type")
  .tccq_check_character_scalar(result_placement, "result_placement")
  if (!result_placement %in% TCCQ_BACKEND_RESULT_PLACEMENTS) {
    tccq_abort(
      "schema.invalid_backend_function_result_placement",
      "`result_placement` is not supported.",
      phase = "schema",
      path = "backend_function.result_placement",
      data = list(
        result_placement = result_placement,
        supported = TCCQ_BACKEND_RESULT_PLACEMENTS
      )
    )
  }
  .tccq_check_character_or_empty(result_name, "result_name")
  .tccq_check_optional_s7(domain, TccqDomain, "TccqDomain", "domain")
  .tccq_check_list_of(extents, TccqBackendExtentBinding, "TccqBackendExtentBinding", "extents")
  if (!is.character(index_names) || anyNA(index_names) || any(!nzchar(index_names))) {
    tccq_abort(
      "schema.invalid_backend_function_indexes",
      "`index_names` must contain non-empty strings.",
      phase = "schema",
      path = "backend_function.index_names"
    )
  }
  .tccq_check_list_of(result_dims, TccqDim, "TccqDim", "result_dims")
  .tccq_check_character_or_empty(result_count_name, "result_count_name")
  .tccq_check_optional_s7(
    error_channel,
    TccqBackendErrorChannel,
    "TccqBackendErrorChannel",
    "error_channel"
  )

  TccqBackendFunctionInterface(
    symbol = symbol,
    source_language = source_language,
    kind = kind,
    abi = abi,
    parameters = parameters,
    locals = locals,
    allocations = allocations,
    result_value_id = result_value_id,
    result_type = result_type,
    result_placement = result_placement,
    result_name = result_name,
    domain = domain,
    extents = extents,
    index_names = index_names,
    result_dims = result_dims,
    result_count_name = result_count_name,
    error_channel = error_channel
  )
}

#' Construct a backend artifact
#'
#' @param id Stable artifact id.
#' @param kind Artifact kind.
#' @param source_language Source language used to produce the artifact, or empty.
#' @param path Filesystem path for file-backed artifacts, or empty.
#' @param attrs Structured artifact metadata.
#' @export
tccq_backend_artifact <- function(
  id,
  kind,
  source_language = "",
  path = "",
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_BACKEND_ARTIFACT_KINDS) {
    tccq_abort(
      "schema.invalid_backend_artifact_kind",
      "`kind` is not a supported backend artifact kind.",
      phase = "schema",
      path = "backend_artifact.kind",
      data = list(kind = kind, supported = TCCQ_BACKEND_ARTIFACT_KINDS)
    )
  }
  .tccq_check_character_or_empty(source_language, "source_language")
  if (nzchar(source_language) && !source_language %in% TCCQ_OP_RENDER_LANGUAGES) {
    tccq_abort(
      "schema.invalid_backend_artifact_source_language",
      "`source_language` is not supported.",
      phase = "schema",
      path = "backend_artifact.source_language",
      data = list(source_language = source_language, supported = TCCQ_OP_RENDER_LANGUAGES)
    )
  }
  .tccq_check_character_or_empty(path, "path")
  .tccq_check_list(attrs, "attrs")

  TccqBackendArtifact(
    id = id,
    kind = kind,
    source_language = source_language,
    path = path,
    attrs = attrs
  )
}

#' Construct backend products
#'
#' @param function_interface Source-level callable interface, if source exists.
#' @param body Expression tree or statement block consumed by the backend, if
#'   source exists.
#' @param loop_nest Result loop nest consumed by the backend, if source exists.
#' @param loop_nests Ordered loop nests, intermediates first, result nest last.
#' @param storage_plan Storage plan consumed by the backend, if available.
#' @param artifacts Named backend artifacts.
#' @param attrs Structured product metadata.
#' @export
tccq_backend_products <- function(
  function_interface = NULL,
  body = NULL,
  loop_nest = NULL,
  loop_nests = list(),
  storage_plan = NULL,
  artifacts = list(),
  attrs = list()
) {
  .tccq_check_optional_s7(
    function_interface,
    TccqBackendFunctionInterface,
    "TccqBackendFunctionInterface",
    "function_interface"
  )
  body_is_supported <- is.null(body) ||
    S7::S7_inherits(body, TccqExpression) ||
    S7::S7_inherits(body, TccqValueBlock)
  if (!body_is_supported) {
    tccq_abort(
      "schema.invalid_backend_product_body",
      "`body` must inherit from <TccqExpression> or <TccqValueBlock>, or be `NULL`.",
      phase = "schema",
      path = "backend_products.body"
    )
  }
  .tccq_check_optional_s7(loop_nest, TccqLoopNest, "TccqLoopNest", "loop_nest")
  .tccq_check_list_of(loop_nests, TccqLoopNest, "TccqLoopNest", "loop_nests")
  .tccq_check_optional_s7(storage_plan, TccqStoragePlan, "TccqStoragePlan", "storage_plan")
  .tccq_check_list_of(artifacts, TccqBackendArtifact, "TccqBackendArtifact", "artifacts")
  artifact_roles <- names(artifacts)
  if (
    length(artifacts) > 0L &&
      (is.null(artifact_roles) || anyNA(artifact_roles) || any(!nzchar(artifact_roles)) || anyDuplicated(artifact_roles))
  ) {
    tccq_abort(
      "schema.invalid_backend_product_artifacts",
      "`artifacts` must be named by unique non-empty artifact roles.",
      phase = "schema",
      path = "backend_products.artifacts",
      data = list(artifact_roles = artifact_roles)
    )
  }
  .tccq_check_list(attrs, "attrs")

  TccqBackendProducts(
    function_interface = function_interface,
    body = body,
    loop_nest = loop_nest,
    loop_nests = loop_nests,
    storage_plan = storage_plan,
    artifacts = artifacts,
    attrs = attrs
  )
}

#' Construct a backend plan
#'
#' @param id Stable plan id.
#' @param backend_id Backend spec id.
#' @param family Backend family.
#' @param mode Backend mode.
#' @param target Target language or runtime.
#' @param capabilities Backend capabilities visible to this plan.
#' @param regions Regions assigned to the backend plan.
#' @param bridges Representation-transition plans.
#' @param diagnostics Diagnostics attached to this plan.
#' @param products Typed backend products.
#' @param attrs Structured plan attributes.
#' @export
tccq_backend_plan <- function(
  id,
  backend_id,
  family,
  mode,
  target,
  capabilities = character(),
  regions = list(),
  bridges = list(),
  diagnostics = list(),
  products = NULL,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(backend_id, "backend_id")
  .tccq_check_character_scalar(family, "family")
  if (!family %in% TCCQ_BACKEND_FAMILIES) {
    tccq_abort(
      "schema.invalid_backend_family",
      "`family` is not a supported backend family.",
      phase = "schema",
      path = "backend_plan.family",
      data = list(family = family, supported = TCCQ_BACKEND_FAMILIES)
    )
  }
  .tccq_check_character_scalar(mode, "mode")
  if (!mode %in% TCCQ_BACKEND_MODES) {
    tccq_abort(
      "schema.invalid_backend_mode",
      "`mode` is not a supported backend mode.",
      phase = "schema",
      path = "backend_plan.mode",
      data = list(mode = mode, supported = TCCQ_BACKEND_MODES)
    )
  }
  .tccq_check_character_scalar(target, "target")
  .tccq_check_character_set(
    capabilities,
    TCCQ_BACKEND_CAPABILITIES,
    "capabilities",
    allow_empty = TRUE
  )
  .tccq_check_list_of(regions, TccqRegion, "TccqRegion", "regions")
  .tccq_check_list_of(bridges, TccqBridgePlan, "TccqBridgePlan", "bridges")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  .tccq_check_optional_s7(products, TccqBackendProducts, "TccqBackendProducts", "products")
  .tccq_check_list(attrs, "attrs")

  TccqBackendPlan(
    id = id,
    backend_id = backend_id,
    family = family,
    mode = mode,
    target = target,
    capabilities = capabilities,
    regions = regions,
    bridges = bridges,
    diagnostics = diagnostics,
    products = products,
    attrs = attrs
  )
}

#' Construct a backend plan set
#'
#' @param program_name Program name.
#' @param plans Backend plans.
#' @param diagnostics Diagnostics collected while planning.
#' @param attrs Structured plan-set attributes.
#' @export
tccq_backend_plan_set <- function(
  program_name,
  plans = list(),
  diagnostics = list(),
  attrs = list()
) {
  .tccq_check_character_scalar(program_name, "program_name")
  .tccq_check_list_of(plans, TccqBackendPlan, "TccqBackendPlan", "plans")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
  .tccq_check_list(attrs, "attrs")

  TccqBackendPlanSet(
    program_name = program_name,
    plans = plans,
    diagnostics = diagnostics,
    attrs = attrs
  )
}

#' Construct a backend implementation descriptor
#'
#' @param id Stable backend id.
#' @param family Backend family.
#' @param target Target language or runtime.
#' @param driver Concrete driver name.
#' @param modes Supported backend modes.
#' @param region_kinds Supported region kinds.
#' @param memory_spaces Supported memory spaces.
#' @param capabilities Capabilities exposed by this backend.
#' @param uses_rapi Whether this backend may touch the R C API.
#' @param attrs Structured backend attributes.
#' @param prepare Planning implementation.
#' @export
tccq_backend_spec <- function(
  id,
  family,
  target,
  driver,
  modes,
  region_kinds,
  memory_spaces,
  capabilities,
  uses_rapi,
  attrs = list(),
  prepare = NULL
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(family, "family")
  if (!family %in% TCCQ_BACKEND_FAMILIES) {
    tccq_abort(
      "schema.invalid_backend_family",
      "`family` is not a supported backend family.",
      phase = "schema",
      path = "backend.family",
      data = list(family = family, supported = TCCQ_BACKEND_FAMILIES)
    )
  }
  .tccq_check_character_scalar(target, "target")
  .tccq_check_character_scalar(driver, "driver")
  .tccq_check_character_set(modes, TCCQ_BACKEND_MODES, "modes")
  .tccq_check_character_set(region_kinds, TCCQ_REGION_KINDS, "region_kinds")
  .tccq_check_character_set(memory_spaces, TCCQ_MEMORY_SPACES, "memory_spaces")
  .tccq_check_character_set(capabilities, TCCQ_BACKEND_CAPABILITIES, "capabilities")
  .tccq_check_logical_scalar(uses_rapi, "uses_rapi")
  .tccq_check_list(attrs, "attrs")
  if (is.null(prepare)) {
    prepare <- function(backend, program, context) {
      diagnostic <- tccq_diagnostic(
        "backend.lowering_absent",
        "Backend planning is typed, but lowering is not implemented for this program yet.",
        phase = "backend",
        path = sprintf("backend.%s", backend@id),
        data = list(
          backend = backend@id,
          family = backend@family,
          driver = backend@driver,
          target = backend@target,
          mode = context@mode,
          capabilities = backend@capabilities,
          program = program@name
        )
      )
      plan <- tccq_backend_plan(
        id = sprintf("%s.%s.plan", backend@id, context@mode),
        backend_id = backend@id,
        family = backend@family,
        mode = context@mode,
        target = if (identical(context@target, "any")) backend@target else context@target,
        capabilities = backend@capabilities,
        diagnostics = list(diagnostic),
        attrs = list(driver = backend@driver)
      )
      tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic))
    }
  }
  if (!is.function(prepare)) {
    tccq_abort(
      "schema.invalid_backend_prepare",
      "`prepare` must be a backend planning function.",
      phase = "schema",
      path = "backend.prepare"
    )
  }

  TccqBackendSpec(
    id = id,
    family = family,
    target = target,
    driver = driver,
    modes = modes,
    region_kinds = region_kinds,
    memory_spaces = memory_spaces,
    capabilities = capabilities,
    uses_rapi = uses_rapi,
    attrs = attrs,
    prepare = prepare
  )
}

#' Generic C backend descriptor
#'
#' @export
tccq_c_backend <- function() {
  tccq_backend_spec(
    id = "c",
    family = "c",
    target = "c",
    driver = "system-c",
    modes = c("source", "shared_library"),
    region_kinds = c("host", "kernel"),
    memory_spaces = c("r", "host"),
    capabilities = c(
      "source",
      "shared_library",
      "native",
      "r_api",
      "host_memory",
      "buffer_bridge",
      "kernel"
    ),
    uses_rapi = TRUE,
    attrs = list(role = "generic_c"),
    prepare = new_lowered_backend_prepare("c")
  )
}

#' Rtinycc backend descriptor
#'
#' This descriptor models the available TinyCC-backed C path without making it
#' the compiler architecture. It can produce backend plans once the typed IR is
#' lowerable, but today it still reports structured lowering absence.
#'
#' @export
tccq_rtinycc_backend <- function() {
  tccq_backend_spec(
    id = "rtinycc",
    family = "c",
    target = "c",
    driver = "Rtinycc",
    modes = c("source", "jit"),
    region_kinds = c("host", "kernel"),
    memory_spaces = c("r", "host"),
    capabilities = c(
      "source",
      "jit",
      "native",
      "r_api",
      "host_memory",
      "buffer_bridge",
      "kernel"
    ),
    uses_rapi = TRUE,
    attrs = list(runtime = "tinycc"),
    prepare = new_lowered_backend_prepare("c", execute_with_rtinycc = TRUE)
  )
}

#' Quickr-style Fortran backend descriptor
#'
#' This descriptor captures the pressure from quickr's Fortran path: subroutine
#' lowering, C/R bridges, R CMD SHLIB, BLAS/LAPACK linkage, and optional OpenMP.
#'
#' @export
tccq_fortran_backend <- function() {
  tccq_backend_spec(
    id = "quickr_fortran",
    family = "fortran",
    target = "fortran",
    driver = "R-CMD-SHLIB",
    modes = c("source", "shared_library"),
    region_kinds = c("host", "kernel"),
    memory_spaces = c("r", "host"),
    capabilities = c(
      "source",
      "shared_library",
      "native",
      "r_api",
      "host_memory",
      "buffer_bridge",
      "kernel"
    ),
    uses_rapi = TRUE,
    attrs = list(role = "quickr_fortran"),
    prepare = new_lowered_backend_prepare("fortran")
  )
}

#' Core backend suite
#'
#' Every descriptor in the core suite prints from the shared loop nest and
#' function interface. A backend family enters this suite together with its
#' first real lowering; capability strings describe implemented behavior only.
#'
#' @param include_rtinycc Whether to include the `Rtinycc` TinyCC descriptor.
#' @export
tccq_core_backends <- function(include_rtinycc = TRUE) {
  backends <- list(
    c = tccq_c_backend(),
    fortran = tccq_fortran_backend()
  )
  if (isTRUE(include_rtinycc)) {
    backends <- append(list(rtinycc = tccq_rtinycc_backend()), backends)
  }
  backends
}

#' Plan a backend for a typed program
#'
#' @param program Program to plan.
#' @param backend Backend implementation.
#' @param context Backend planning context.
#' @export
tccq_plan_backend <- function(
  program,
  backend = tccq_c_backend(),
  context = tccq_backend_context()
) {
  .tccq_check_s7(program, TccqProgram, "TccqProgram", "program")
  .tccq_check_s7(backend, TccqBackendSpec, "TccqBackendSpec", "backend")
  .tccq_check_s7(context, TccqBackendContext, "TccqBackendContext", "context")
  s7contract::assert_trait(backend, TccqBackend, arg = "backend")
  with(TccqBackend, tccq_backend_prepare(backend, program, context))
}

#' Plan several backends for a typed program
#'
#' Every backend accounts for the program and reports constraints through its
#' own plan. The plan set succeeds when at least one backend produces a working
#' plan; backends that cannot lower the program contribute feasibility
#' diagnostics without vetoing the suite.
#'
#' @param program Program to plan.
#' @param backends Backend implementation descriptors.
#' @param context Backend planning context.
#' @export
tccq_plan_backends <- function(
  program,
  backends = tccq_core_backends(),
  context = tccq_backend_context()
) {
  .tccq_check_s7(program, TccqProgram, "TccqProgram", "program")
  if (S7::S7_inherits(backends, TccqBackendSpec)) {
    backends <- list(backends)
  }
  .tccq_check_list_of(backends, TccqBackendSpec, "TccqBackendSpec", "backends")
  .tccq_check_s7(context, TccqBackendContext, "TccqBackendContext", "context")

  results <- lapply(backends, function(backend) {
    tccq_plan_backend(program, backend = backend, context = context)
  })
  plans <- lapply(results, function(result) result@value)
  diagnostics <- unlist(lapply(results, function(result) result@diagnostics), recursive = FALSE)
  plan_set <- tccq_backend_plan_set(
    program@name,
    plans = plans,
    diagnostics = diagnostics,
    attrs = list(backends = vapply(backends, function(x) x@id, character(1)))
  )
  backend_plan_succeeded <- vapply(results, function(result) result@success, logical(1))
  tccq_result(
    success = any(backend_plan_succeeded),
    value = plan_set,
    diagnostics = diagnostics
  )
}

new_lowered_backend_prepare <- function(source_language, execute_with_rtinycc = FALSE) {
  force(source_language)
  force(execute_with_rtinycc)

  function(backend, program, context) {
    c_identifier <- function(prefix, ordinal) {
      sprintf("%s_%04d", prefix, ordinal)
    }

    hash_text <- function(text) {
      hash_value <- 0
      for (character_code in utf8ToInt(text)) {
        hash_value <- (hash_value * 131 + character_code) %% 2147483647
      }
      sprintf("%07d", as.integer(hash_value))
    }

    value_signature <- function(value) {
      paste(
        value@id,
        value@op,
        value@type@base,
        value@type@shape@rank,
        paste(vapply(value@inputs, as.character, character(1)), collapse = ","),
        sep = ":"
      )
    }

    source_symbol <- function() {
      signature <- paste(
        c(
          source_language,
          backend@id,
          program@result,
          vapply(program@values, value_signature, character(1))
        ),
        collapse = "|"
      )
      sprintf("tccq_%s_%s", source_language, hash_text(signature))
    }

    formal_values <- function() {
      Filter(function(value) identical(value@op, "formal"), unname(program@values))
    }

    result_value <- function() {
      if (is.null(program@result) || is.null(program@values[[program@result]])) {
        return(NULL)
      }
      program@values[[program@result]]
    }

    diagnostic_plan <- function(diagnostics) {
      tccq_backend_plan(
        id = sprintf("%s.%s.plan", backend@id, context@mode),
        backend_id = backend@id,
        family = backend@family,
        mode = context@mode,
        target = if (identical(context@target, "any")) backend@target else context@target,
        capabilities = backend@capabilities,
        diagnostics = diagnostics,
        attrs = list(driver = backend@driver)
      )
    }

    source_artifact <- function(symbol, source, path = "") {
      tccq_backend_artifact(
        id = sprintf("%s.source", symbol),
        kind = "source",
        source_language = source_language,
        path = path,
        attrs = list(text = source)
      )
    }

    shared_library_artifact <- function(symbol, path) {
      tccq_backend_artifact(
        id = sprintf("%s.shared_library", symbol),
        kind = "shared_library",
        source_language = source_language,
        path = path,
        attrs = list()
      )
    }

    jit_callable_artifact <- function(symbol, callable) {
      tccq_backend_artifact(
        id = sprintf("%s.jit_callable", symbol),
        kind = "jit_callable",
        source_language = source_language,
        attrs = list(callable = callable)
      )
    }

    native_callable_artifact <- function(symbol, callable) {
      tccq_backend_artifact(
        id = sprintf("%s.native_callable", symbol),
        kind = "native_callable",
        source_language = source_language,
        attrs = list(callable = callable)
      )
    }

    product_attrs <- function(plan) {
      products <- plan@products %||% tccq_backend_products()
      products@attrs
    }

    set_product_attrs <- function(plan, attrs) {
      products <- plan@products %||% tccq_backend_products()
      products@attrs <- attrs
      plan@products <- products
      plan
    }

    set_product_artifact <- function(plan, role, artifact) {
      products <- plan@products %||% tccq_backend_products()
      artifacts <- products@artifacts
      artifacts[[role]] <- artifact
      products@artifacts <- artifacts
      plan@products <- products
      plan
    }

    validate_lowered_program <- function(result, formals) {
      if (is.null(result) || length(program@regions) == 0L) {
        return(tccq_diagnostic(
          "backend.lowering_absent",
          "Backend planning needs a lowered program with a result and at least one region.",
          phase = "backend",
          path = sprintf("backend.%s", backend@id),
          data = list(backend = backend@id, program = program@name)
        ))
      }
      unsupported_literals <- Filter(function(value) {
        literal <- value@attrs$literal
        S7::S7_inherits(literal, TccqLiteral) && !identical(literal@kind, "finite")
      }, program@values)
      if (length(unsupported_literals) > 0L) {
        return(tccq_diagnostic(
          "backend.unsupported_literal_kind",
          "Source backends do not yet implement non-finite R literal semantics.",
          phase = "backend",
          path = sprintf("backend.%s.literal", backend@id),
          data = list(
            literals = lapply(unsupported_literals, function(value) value@attrs$literal)
          )
        ))
      }
      unsupported_formals <- Filter(function(value) {
        type <- value@type
        !identical(type@base, "double") &&
          !(identical(type@base, "logical") && type@shape@rank == 0L)
      }, formals)
      result_type <- result@type
      result_is_supported <- identical(result_type@base, "double") ||
        (result_type@shape@rank == 0L && result_type@base %in% c("logical", "integer"))
      unsupported_values <- c(unsupported_formals, if (result_is_supported) list() else list(result))
      if (length(unsupported_values) > 0L) {
        return(tccq_diagnostic(
          "backend.unsupported_type",
          "Source backends currently support double values, scalar logical inputs, and scalar logical or integer results.",
          phase = "backend",
          path = sprintf("backend.%s.type", backend@id),
          data = list(
            types = lapply(unsupported_values, function(value) value@type)
          )
        ))
      }
      NULL
    }

    source_scalar_type <- function(type, language) {
      if (identical(type@base, "double")) {
        return(if (identical(language, "fortran")) "real(c_double)" else "double")
      }
      if (type@base %in% c("logical", "integer")) {
        return(if (identical(language, "fortran")) "integer(c_int)" else "int")
      }
      tccq_abort(
        "backend.unsupported_scalar_type",
        "The source backend has no scalar spelling for this semantic type.",
        phase = "backend",
        path = sprintf("backend.%s.type", backend@id),
        data = list(backend = backend@id, type = type, language = language)
      )
    }

    sanitize_extent_symbol <- function(symbol) {
      gsub("[^A-Za-z0-9_]", "_", symbol)
    }

    extent_plan <- function(formals, nests) {
      symbols <- character()
      note_dim <- function(dim) {
        if (dim@kind %in% c("symbol", "affine") && !dim@label %in% symbols) {
          symbols <<- c(symbols, dim@label)
        }
      }
      for (value in formals) {
        for (dim in value@type@shape@dims) {
          note_dim(dim)
        }
      }
      for (nest in nests) {
        for (axis in nest@axes) {
          note_dim(axis@extent)
        }
        for (dim in nest@storage@type@shape@dims) {
          note_dim(dim)
        }
      }
      list(
        symbols = symbols,
        names = vapply(symbols, function(symbol) {
          sprintf("extent_%s", sanitize_extent_symbol(symbol))
        }, character(1), USE.NAMES = FALSE)
      )
    }

    extent_text <- function(dim, extent_by_symbol) {
      if (identical(dim@kind, "constant")) {
        return(sprintf("%d", dim@value))
      }
      extent_name <- extent_by_symbol[[dim@label]]
      if (dim@kind %in% c("symbol", "affine") && is.null(extent_name)) {
        tccq_abort(
          "backend.unbound_extent_symbol",
          "A dimension symbol is not bound to an extent parameter.",
          phase = "backend",
          path = sprintf("backend.%s.extents", backend@id),
          data = list(backend = backend@id, symbol = dim@label)
        )
      }
      if (identical(dim@kind, "symbol")) {
        return(extent_name)
      }
      if (identical(dim@kind, "affine")) {
        operator <- if (dim@value < 0L) "-" else "+"
        return(sprintf("(%s %s %d)", extent_name, operator, abs(dim@value)))
      }
      tccq_abort(
        "backend.unknown_extent",
        "Source printing needs constant, symbolic, or affine extents.",
        phase = "backend",
        path = sprintf("backend.%s.extents", backend@id),
        data = list(backend = backend@id, kind = dim@kind)
      )
    }

    stride_texts <- function(dims, extent_by_symbol) {
      strides <- character(length(dims))
      factors <- character()
      for (position in seq_along(dims)) {
        strides[[position]] <- if (length(factors) == 0L) {
          "1"
        } else if (length(factors) == 1L) {
          factors[[1L]]
        } else {
          sprintf("(%s)", paste(factors, collapse = " * "))
        }
        factors <- c(factors, extent_text(dims[[position]], extent_by_symbol))
      }
      strides
    }

    index_term_text <- function(index) {
      if (!nzchar(index@axis)) {
        return(sprintf("%d", index@offset))
      }
      if (identical(index@offset, 0L)) {
        return(index@axis)
      }
      operator <- if (index@offset < 0L) "-" else "+"
      sprintf("(%s %s %d)", index@axis, operator, abs(index@offset))
    }

    linear_index_text <- function(access, dims, extent_by_symbol) {
      strides <- stride_texts(dims, extent_by_symbol)
      terms <- character()
      for (position in seq_along(access@index_map)) {
        term <- index_term_text(access@index_map[[position]])
        if (identical(term, "0")) {
          next
        }
        terms <- c(terms, if (identical(strides[[position]], "1")) {
          term
        } else {
          sprintf("%s * %s", term, strides[[position]])
        })
      }
      if (length(terms) == 0L) "0" else paste(terms, collapse = " + ")
    }

    expression_text <- function(expression, emit_context) {
      if (expression@kind %in% c("reference", "indexed")) {
        access <- expression@reference@access
        if (!S7::S7_inherits(access, TccqAccess)) {
          if (expression@type@shape@rank == 0L) {
            source_name <- emit_context$source_name_by_value_id[[
              expression@reference@source_value_id
            ]]
            if (!is.null(source_name) && nzchar(source_name)) {
              return(source_name)
            }
          }
          tccq_abort(
            "backend.missing_access",
            "Array references require a typed access and scalar references require a bound source.",
            phase = "backend",
            path = sprintf("backend.%s.access", backend@id),
            data = list(backend = backend@id, value_id = expression@value_id)
          )
        }
        source_name <- emit_context$source_name_by_value_id[[access@value_id]]
        if (identical(access@kind, "scalar")) {
          return(source_name)
        }
        storage_type <- emit_context$storage_type_by_value_id[[access@value_id]]
        if (identical(access@kind, "recycle")) {
          consumer_linear <- linear_index_text(
            access,
            access@consumer_shape@dims,
            emit_context$extent_by_symbol
          )
          length_text <- paste(
            vapply(
              storage_type@shape@dims,
              extent_text,
              character(1),
              extent_by_symbol = emit_context$extent_by_symbol
            ),
            collapse = " * "
          )
          if (identical(emit_context$language, "fortran")) {
            return(sprintf(
              "%s(mod(%s, %s) + 1)",
              source_name,
              consumer_linear,
              length_text
            ))
          }
          return(sprintf("%s[(%s) %% (%s)]", source_name, consumer_linear, length_text))
        }
        linear <- linear_index_text(access, storage_type@shape@dims, emit_context$extent_by_symbol)
        if (identical(emit_context$language, "fortran")) {
          return(sprintf("%s(%s + 1)", source_name, linear))
        }
        return(sprintf("%s[%s]", source_name, linear))
      }
      if (identical(expression@kind, "literal")) {
        return(literal_text(expression@literal, emit_context$language))
      }
      if (identical(expression@kind, "element")) {
        return(expression_text(expression@inputs[[1L]], emit_context))
      }
      if (identical(expression@kind, "branch")) {
        tccq_abort(
          "backend.branch_requires_statement_context",
          "A conditional expression must be emitted through a statement-valued assignment.",
          phase = "backend",
          path = sprintf("backend.%s.expression", backend@id),
          data = list(backend = backend@id, value_id = expression@value_id)
        )
      }

      inputs <- vapply(expression@inputs, expression_text, character(1), emit_context = emit_context)
      render_result <- tccq_op_render(
        expression@operation@resolved_op@implementation,
        inputs,
        tccq_op_render_context(
          language = emit_context$language,
          backend_id = backend@id,
          attrs = list(value_id = expression@value_id, op = expression@op)
        )
      )
      if (!render_result@success) {
        tccq_abort_diagnostic(render_result@diagnostics[[1L]])
      }
      render_result@value
    }

    backend_function_interface <- function(symbol, nests, result, formals, body = NULL) {
      structured_body <- S7::S7_inherits(body, TccqValueBlock)
      if (structured_body && length(nests) > 0L) {
        tccq_abort(
          "backend.competing_iteration_plans",
          "A source plan cannot carry both a structured program body and loop nests.",
          phase = "backend",
          path = sprintf("backend.%s.interface", backend@id)
        )
      }
      if (!structured_body && length(nests) == 0L) {
        tccq_abort(
          "backend.missing_executable_body",
          "A source plan needs a structured program body or at least one loop nest.",
          phase = "backend",
          path = sprintf("backend.%s.interface", backend@id)
        )
      }
      result_nest <- if (structured_body) NULL else nests[[length(nests)]]
      intermediate_nests <- if (structured_body) list() else nests[-length(nests)]
      parameter_names <- c_identifier("input", seq_along(formals))
      parameters <- unname(Map(function(source_name, value) {
        tccq_backend_value_binding(
          source_name,
          value@id,
          value@type,
          role = "parameter"
        )
      }, parameter_names, formals))
      state_components <- unlist(lapply(nests, function(nest) {
        if (is.null(nest@reduction)) list() else nest@reduction@state@components
      }), recursive = FALSE)
      state_targets <- lapply(state_components, function(component) component@target)
      local_targets <- list()
      local_value_ids <- character()
      for (state_target in state_targets) {
        local_targets[[length(local_targets) + 1L]] <- state_target
        local_value_ids <- c(local_value_ids, state_target@value_id)
      }
      statement_index_names <- character()
      structured_body_has_condition <- FALSE
      walk_block <- function(block) {
        for (local_target in block@locals) {
          if (!local_target@value_id %in% local_value_ids) {
            local_targets[[length(local_targets) + 1L]] <<- local_target
            local_value_ids <<- c(local_value_ids, local_target@value_id)
          }
        }
        for (statement in block@statements) {
          if (S7::S7_inherits(statement, TccqSwitch)) {
            selector_target <- statement@selector_target
            if (!selector_target@value_id %in% local_value_ids) {
              local_targets[[length(local_targets) + 1L]] <<- selector_target
              local_value_ids <<- c(local_value_ids, selector_target@value_id)
            }
            for (alternative in statement@alternatives) {
              walk_block(alternative)
            }
          } else if (S7::S7_inherits(statement, TccqIf)) {
            structured_body_has_condition <<- TRUE
            walk_block(statement@consequent)
            walk_block(statement@alternative)
          } else if (S7::S7_inherits(statement, TccqLoop)) {
            if (S7::S7_inherits(statement, TccqWhile)) {
              structured_body_has_condition <<- TRUE
            }
            if (S7::S7_inherits(statement, TccqFor)) {
              statement_index_names <<- c(
                statement_index_names,
                statement@iteration@domain@axes
              )
            }
            walk_block(statement@body)
          }
        }
        invisible(NULL)
      }
      for (nest in nests) {
        if (S7::S7_inherits(nest@body, TccqValueBlock)) {
          walk_block(nest@body)
        }
      }
      if (structured_body) {
        walk_block(body)
      }
      state_component_names <- vapply(
        state_components,
        function(component) component@name,
        character(1)
      )
      state_component_occurrences <- integer(length(state_component_names))
      for (position in seq_along(state_component_names)) {
        state_component_occurrences[[position]] <- sum(
          state_component_names[seq_len(position)] == state_component_names[[position]]
        )
      }
      state_names <- sprintf("%s_%04d", state_component_names, state_component_occurrences)
      statement_local_count <- length(local_targets) - length(state_targets)
      local_names <- c(
        state_names,
        c_identifier("local", seq_len(statement_local_count))
      )
      locals <- unname(Map(function(source_name, target) {
        tccq_backend_value_binding(
          source_name,
          target@value_id,
          target@storage_type,
          role = "local"
        )
      }, local_names, local_targets))
      intermediate_allocation_ids <- vapply(
        intermediate_nests,
        function(nest) nest@storage@allocation@id,
        character(1)
      )
      allocation_ids <- unique(intermediate_allocation_ids)
      allocation_names <- stats::setNames(
        c_identifier("intermediate", seq_along(allocation_ids)),
        allocation_ids
      )
      allocations <- lapply(allocation_ids, function(allocation_id) {
        slots <- lapply(
          intermediate_nests[intermediate_allocation_ids == allocation_id],
          function(nest) nest@storage
        )
        tccq_backend_allocation_binding(
          unname(allocation_names[[allocation_id]]),
          slots[[1L]]@allocation,
          slots
        )
      })
      extents <- extent_plan(formals, nests)
      extent_bindings <- unname(Map(function(symbol, source_name) {
        tccq_backend_extent_binding(source_name, symbol)
      }, extents$symbols, extents$names))
      kind <- if (structured_body) {
        "structured"
      } else if (length(result_nest@axes) == 0L) {
        "scalar"
      } else {
        "loop_nest"
      }
      for (nest in nests) {
        roles <- vapply(nest@axes, function(axis) axis@role, character(1))
        if (length(roles) > 1L && any(diff(match(roles, c("map", "reduce"))) < 0L)) {
          tccq_abort(
            "backend.unsupported_axis_order",
            "Source printers currently require map axes to precede reduce axes.",
            phase = "backend",
            path = sprintf("backend.%s.axes", backend@id),
            data = list(backend = backend@id, roles = roles)
          )
        }
      }
      index_names <- unique(c(
        unlist(lapply(nests, function(nest) {
          vapply(nest@axes, function(axis) axis@name, character(1))
        })) %||% character(),
        statement_index_names
      ))
      result_rank <- result@type@shape@rank
      has_condition <- any(vapply(
        nests,
        function(nest) {
          length(nest@guards) > 0L ||
            (!is.null(nest@reduction) && !is.null(nest@reduction@condition))
        },
        logical(1)
      )) || structured_body_has_condition
      runtime_diagnostics <- if (has_condition) {
        list(tccq_diagnostic(
          "runtime.invalid_logical_condition",
          "A generated scalar condition evaluated to a missing value.",
          phase = "runtime",
          path = "callable.condition",
          data = list(program = program@name, backend = backend@id)
        ))
      } else {
        list()
      }
      has_empty_reduction_error <- any(vapply(nests, function(nest) {
        !is.null(nest@reduction) &&
          identical(nest@reduction@spec@empty_policy, "error")
      }, logical(1)))
      if (has_empty_reduction_error) {
        runtime_diagnostics <- c(runtime_diagnostics, list(tccq_diagnostic(
          "runtime.reduction_has_no_value",
          "A generated reduction had no selectable value.",
          phase = "runtime",
          path = "callable.reduction",
          data = list(program = program@name, backend = backend@id)
        )))
      }
      if (
        has_condition &&
          identical(source_language, "c") &&
          length(allocations) > 0L
      ) {
        runtime_diagnostics <- c(runtime_diagnostics, list(tccq_diagnostic(
          "runtime.generated_allocation_failed",
          "Generated code could not allocate an intermediate buffer.",
          phase = "runtime",
          path = "callable.allocation",
          data = list(program = program@name, backend = backend@id)
        )))
      }
      error_channel <- if (length(runtime_diagnostics) > 0L) {
        tccq_backend_error_channel("status_0001", runtime_diagnostics)
      } else {
        NULL
      }
      result_needs_local <- structured_body || (
        S7::S7_inherits(result_nest@body, TccqValueBlock) &&
          is.null(result_nest@reduction)
      )
      tccq_backend_function_interface(
        symbol = symbol,
        source_language = source_language,
        kind = kind,
        abi = if (identical(source_language, "fortran")) "fortran_bind_c" else "c",
        parameters = parameters,
        locals = locals,
        allocations = allocations,
        result_value_id = result@id,
        result_type = result@type,
        result_placement = if (
          result_rank > 0L &&
            (identical(source_language, "fortran") || !is.null(error_channel))
        ) {
          "output_argument"
        } else {
          "return"
        },
        result_name = if (identical(source_language, "fortran") || result_rank > 0L) {
          "output"
        } else if (result_needs_local) {
          "result_0001"
        } else {
          ""
        },
        domain = if (kind %in% c("scalar", "structured")) NULL else result_nest@domain,
        extents = extent_bindings,
        index_names = index_names,
        result_dims = result@type@shape@dims,
        result_count_name = if (result_rank > 0L) "result_count_0001" else "",
        error_channel = error_channel
      )
    }

    new_emit_context <- function(interface, formals, language) {
      extent_symbols <- vapply(interface@extents, function(binding) binding@symbol, character(1))
      extent_names <- vapply(interface@extents, function(binding) binding@source_name, character(1))
      extent_by_symbol <- as.list(stats::setNames(extent_names, extent_symbols))
      value_bindings <- c(interface@parameters, interface@locals)
      value_ids <- vapply(value_bindings, function(binding) binding@value_id, character(1))
      source_names <- vapply(value_bindings, function(binding) binding@source_name, character(1))
      source_name_by_value_id <- as.list(stats::setNames(source_names, value_ids))
      source_name_by_value_id[interface@index_names] <- as.list(interface@index_names)
      dimension_value_ids <- sprintf(
        "dimension.%s",
        vapply(interface@extents, function(binding) binding@symbol, character(1))
      )
      source_name_by_value_id[dimension_value_ids] <- as.list(vapply(
        interface@extents,
        function(binding) binding@source_name,
        character(1)
      ))
      for (allocation_binding in interface@allocations) {
        allocation_value_ids <- vapply(
          allocation_binding@slots,
          function(slot) slot@value_id,
          character(1)
        )
        source_name_by_value_id[allocation_value_ids] <- as.list(rep(
          allocation_binding@source_name,
          length(allocation_value_ids)
        ))
      }
      storage_type_by_value_id <- lapply(
        stats::setNames(formals, vapply(formals, function(value) value@id, character(1))),
        function(value) value@type
      )
      storage_type_by_value_id[interface@index_names] <- rep(
        list(tccq_type("integer")),
        length(interface@index_names)
      )
      storage_type_by_value_id[dimension_value_ids] <- rep(
        list(tccq_type("integer")),
        length(dimension_value_ids)
      )
      local_value_ids <- vapply(interface@locals, function(binding) binding@value_id, character(1))
      storage_type_by_value_id[local_value_ids] <- lapply(
        interface@locals,
        function(binding) binding@source_type
      )
      for (allocation_binding in interface@allocations) {
        allocation_value_ids <- vapply(
          allocation_binding@slots,
          function(slot) slot@value_id,
          character(1)
        )
        storage_type_by_value_id[allocation_value_ids] <- lapply(
          allocation_binding@slots,
          function(slot) slot@type
        )
      }
      list(
        extent_by_symbol = extent_by_symbol,
        source_name_by_value_id = source_name_by_value_id,
        storage_type_by_value_id = storage_type_by_value_id,
        language = language
      )
    }

    literal_text <- function(literal, language) {
      if (!identical(literal@kind, "finite")) {
        tccq_abort(
          "backend.unsupported_literal_kind",
          "Source emission requires an implemented non-finite literal policy.",
          phase = "backend",
          path = sprintf("backend.%s.literal", backend@id),
          data = list(backend = backend@id, literal = literal)
        )
      }
      if (identical(literal@type@base, "logical")) {
        if (identical(language, "fortran")) {
          return(if (isTRUE(literal@value)) "1_c_int" else "0_c_int")
        }
        return(if (isTRUE(literal@value)) "1" else "0")
      }
      if (identical(literal@type@base, "integer")) {
        suffix <- if (identical(language, "fortran")) "_c_int" else ""
        return(sprintf("%d%s", as.integer(literal@value), suffix))
      }
      value <- formatC(as.numeric(literal@value), digits = 17L, format = "fg")
      if (identical(literal@type@base, "double") && !grepl("[.eE]", value)) {
        value <- paste0(value, ".0")
      }
      if (identical(language, "fortran") && identical(literal@type@base, "double")) {
        return(sprintf("%s_c_double", value))
      }
      value
    }

    loop_plan <- function(interface, nest, emit_context) {
      map_axes <- Filter(function(axis) identical(axis@role, "map"), nest@axes)
      reduce_axes <- Filter(function(axis) identical(axis@role, "reduce"), nest@axes)
      materialization_name <- if (identical(nest@storage@role, "temporary")) {
        emit_context$source_name_by_value_id[[nest@storage@value_id]]
      } else {
        interface@result_name
      }
      if (
        identical(nest@storage@role, "temporary") &&
          (is.null(materialization_name) || !nzchar(materialization_name))
      ) {
        tccq_abort(
          "backend.unbound_intermediate",
          "A materialized intermediate has no generated source name.",
          phase = "backend",
          path = sprintf("backend.%s.intermediate", backend@id),
          data = list(
            backend = backend@id,
            nest = nest@id,
            value_id = nest@storage@value_id
          )
        )
      }
      body_requires_statements <- S7::S7_inherits(nest@body, TccqValueBlock)
      body_text <- if (!body_requires_statements) {
        expression_text(nest@body, emit_context)
      } else if (!is.null(nest@reduction)) {
        if (!identical(nest@body@result@kind, "local")) {
          tccq_abort(
            "backend.reducer_block_result_not_local",
            "A statement-producing reducer body must expose a block-local result target.",
            phase = "backend",
            path = sprintf("backend.%s.reducer_body", backend@id),
            data = list(backend = backend@id, nest = nest@id)
          )
        }
        block_result_name <- emit_context$source_name_by_value_id[[nest@body@result@value_id]]
        if (is.null(block_result_name) || !nzchar(block_result_name)) {
          tccq_abort(
            "backend.unbound_reducer_block_result",
            "A statement-producing reducer body has no generated result name.",
            phase = "backend",
            path = sprintf("backend.%s.reducer_body", backend@id),
            data = list(
              backend = backend@id,
              nest = nest@id,
              value_id = nest@body@result@value_id
            )
          )
        }
        block_result_name
      } else {
        ""
      }
      value_text <- body_text
      if (!is.null(nest@reduction)) {
        state_source_names <- emit_context$source_name_by_value_id
        state_target_ids <- vapply(
          nest@reduction@state@components,
          function(component) component@target@value_id,
          character(1)
        )
        unbound_state_targets <- state_target_ids[vapply(
          state_target_ids,
          function(value_id) {
            source_name <- state_source_names[[value_id]]
            is.null(source_name) || !nzchar(source_name)
          },
          logical(1)
        )]
        if (length(unbound_state_targets) > 0L) {
          tccq_abort(
            "backend.unbound_reduction_state",
            "A reduction-state component has no generated source name.",
            phase = "backend",
            path = sprintf("backend.%s.reduction_state", backend@id),
            data = list(backend = backend@id, nest = nest@id, targets = unbound_state_targets)
          )
        }
        value_text <- expression_text(nest@reduction@value, emit_context)
      }
      output_index <- if (!is.null(nest@output)) {
        linear_index_text(
          nest@output,
          nest@storage@type@shape@dims,
          emit_context$extent_by_symbol
        )
      } else {
        ""
      }
      list(
        map_axes = map_axes,
        reduce_axes = reduce_axes,
        reduction = nest@reduction,
        materialization_name = materialization_name,
        body_text = body_text,
        value_text = value_text,
        output_index = output_index,
        body = nest@body,
        body_requires_statements = body_requires_statements,
        guards = nest@guards
      )
    }

    register_dim_symbols <- function(emit_context) {
      for (value in program@values) {
        if (!identical(value@op, "dim_symbol")) {
          next
        }
        extent_name <- emit_context$extent_by_symbol[[value@attrs$symbol]]
        if (is.null(extent_name)) {
          tccq_abort(
            "backend.unbound_extent_symbol",
            "A dimension symbol used as a value is not bound to an extent parameter.",
            phase = "backend",
            path = sprintf("backend.%s.extents", backend@id),
            data = list(backend = backend@id, symbol = value@attrs$symbol)
          )
        }
        emit_context$source_name_by_value_id[[value@id]] <- if (
          identical(emit_context$language, "fortran")
        ) {
          sprintf("real(%s, c_double)", extent_name)
        } else {
          sprintf("(double)%s", extent_name)
        }
      }
      emit_context
    }

    buffer_size_text <- function(nest, emit_context) {
      paste(
        vapply(
          nest@storage@type@shape@dims,
          extent_text,
          character(1),
          extent_by_symbol = emit_context$extent_by_symbol
        ),
        collapse = " * "
      )
    }

    emit_c_source <- function(interface, nests, result, formals, body = NULL) {
      structured_body <- S7::S7_inherits(body, TccqValueBlock)
      result_nest <- if (structured_body) NULL else nests[[length(nests)]]
      intermediate_nests <- if (structured_body) list() else nests[-length(nests)]
      parameter_names <- vapply(
        interface@parameters,
        function(binding) binding@source_name,
        character(1)
      )
      extent_names <- vapply(
        interface@extents,
        function(binding) binding@source_name,
        character(1)
      )
      emit_context <- new_emit_context(interface, formals, "c")
      emit_context <- register_dim_symbols(emit_context)
      intermediate_plans <- lapply(
        intermediate_nests,
        loop_plan,
        interface = interface,
        emit_context = emit_context
      )
      intermediate_allocation_ids <- vapply(
        intermediate_nests,
        function(nest) nest@storage@allocation@id,
        character(1)
      )
      first_allocation_use <- !duplicated(intermediate_allocation_ids)
      plan <- if (structured_body) NULL else loop_plan(interface, result_nest, emit_context)
      parameter_declarations <- unlist(Map(function(value, parameter_name) {
        if (value@type@shape@rank == 0L) {
          sprintf("%s %s", source_scalar_type(value@type, "c"), parameter_name)
        } else {
          sprintf("const double *%s", parameter_name)
        }
      }, formals, parameter_names), use.names = FALSE)
      returns_buffer <- result@type@shape@rank > 0L
      output_argument <- identical(interface@result_placement, "output_argument")
      signature_declarations <- c(
        parameter_declarations,
        if (length(extent_names) > 0L) sprintf("int %s", extent_names),
        if (nzchar(interface@result_count_name)) sprintf("int %s", interface@result_count_name),
        if (!is.null(interface@error_channel)) {
          sprintf("int *%s", interface@error_channel@source_name)
        },
        if (output_argument) {
          sprintf("double *%s", interface@result_name)
        }
      )
      has_intermediate_buffer <- any(vapply(
        intermediate_nests,
        function(nest) nest@storage@type@shape@rank > 0L,
        logical(1)
      ))
      return_type <- if (output_argument) {
        "void"
      } else if (returns_buffer) {
        "double"
      } else {
        source_scalar_type(result@type, "c")
      }
      lines <- c(
        "#include <math.h>",
        "#include <limits.h>",
        "#include <stddef.h>",
        if ((returns_buffer && !output_argument) || has_intermediate_buffer) "#include <stdlib.h>",
        "#define TCCQ_NA_LOGICAL INT_MIN",
        "",
        sprintf(
          "%s %s%s(%s) {",
          return_type,
          if (returns_buffer && !output_argument) "*" else "",
          interface@symbol,
          paste(signature_declarations, collapse = ", ")
        )
      )
      depth <- 1L
      push <- function(text, level = depth) {
        lines <<- c(lines, paste0(strrep("  ", level), text))
      }
      if (!is.null(interface@error_channel)) {
        push(sprintf("*%s = 0;", interface@error_channel@source_name))
      }
      allocation_failure_status <- if (is.null(interface@error_channel)) {
        NA_integer_
      } else {
        match(
          "runtime.generated_allocation_failed",
          vapply(
            interface@error_channel@diagnostics,
            function(diagnostic) diagnostic@code,
            character(1)
          )
        )
      }
      condition_failure_status <- if (is.null(interface@error_channel)) {
        NA_integer_
      } else {
        match(
          "runtime.invalid_logical_condition",
          vapply(
            interface@error_channel@diagnostics,
            function(diagnostic) diagnostic@code,
            character(1)
          )
        )
      }
      reduction_failure_status <- if (is.null(interface@error_channel)) {
        NA_integer_
      } else {
        match(
          "runtime.reduction_has_no_value",
          vapply(
            interface@error_channel@diagnostics,
            function(diagnostic) diagnostic@code,
            character(1)
          )
        )
      }
      emit_state_initialization <- function(reduction_plan) {
        for (component in reduction_plan$reduction@state@components) {
          source_name <- emit_context$source_name_by_value_id[[component@target@value_id]]
          push(sprintf(
            "%s %s = %s;",
            source_scalar_type(component@target@storage_type, "c"),
            source_name,
            literal_text(component@identity, "c")
          ))
        }
      }
      emit_state_step <- function(reduction_plan) {
        condition <- reduction_plan$reduction@condition
        if (!is.null(condition)) {
          push("{")
          depth <<- depth + 1L
          emit_condition_value(condition)
          push("if (condition_value != 0) {")
          depth <<- depth + 1L
        }
        for (assignment in reduction_plan$reduction@updates) {
          source_name <- emit_context$source_name_by_value_id[[assignment@target@value_id]]
          push(sprintf(
            "%s = %s;",
            source_name,
            expression_text(assignment@value, emit_context)
          ))
        }
        if (!is.null(condition)) {
          depth <<- depth - 1L
          push("}")
          depth <<- depth - 1L
          push("}")
        }
      }
      emit_reduction_validity <- function(reduction_plan) {
        validity <- reduction_plan$reduction@valid
        if (is.null(validity)) {
          return(invisible(NULL))
        }
        if (is.na(reduction_failure_status)) {
          tccq_abort(
            "backend.missing_error_channel",
            "A fallible reduction requires a callable error channel.",
            phase = "backend",
            path = sprintf("backend.%s.error_channel", backend@id)
          )
        }
        push(sprintf("if (!(%s)) {", expression_text(validity, emit_context)))
        push(sprintf(
          "*%s = %d;",
          interface@error_channel@source_name,
          reduction_failure_status
        ), depth + 1L)
        push("goto tccq_runtime_failure;", depth + 1L)
        push("}")
        invisible(NULL)
      }
      emit_condition_value <- function(condition) {
        if (is.na(condition_failure_status)) {
          tccq_abort(
            "backend.missing_error_channel",
            "A generated control condition requires a callable error channel.",
            phase = "backend",
            path = sprintf("backend.%s.error_channel", backend@id)
          )
        }
        push(sprintf("int condition_value = %s;", expression_text(condition, emit_context)))
        push("if (condition_value == TCCQ_NA_LOGICAL) {")
        push(sprintf(
          "*%s = %d;",
          interface@error_channel@source_name,
          condition_failure_status
        ), depth + 1L)
        push("goto tccq_runtime_failure;", depth + 1L)
        push("}")
      }
      open_loop <- function(axis) {
        extent <- extent_text(axis@extent, emit_context$extent_by_symbol)
        push(sprintf("for (int %s = 0; %s < %s; ++%s) {", axis@name, axis@name, extent, axis@name))
        depth <<- depth + 1L
      }
      close_loop <- function(axis) {
        depth <<- depth - 1L
        push("}")
      }
      open_guards <- function(guards) {
        for (guard in guards) {
          push("{")
          depth <<- depth + 1L
          emit_condition_value(guard@condition)
          comparison <- if (isTRUE(guard@selected)) "!= 0" else "== 0"
          push(sprintf("if (condition_value %s) {", comparison))
          depth <<- depth + 1L
        }
      }
      close_guards <- function(guards) {
        for (guard in rev(guards)) {
          depth <<- depth - 1L
          push("}")
          depth <<- depth - 1L
          push("}")
        }
      }
      emit_statement <- NULL
      emit_statement_block <- function(block, result_target, after_statements = NULL) {
        push("{")
        depth <<- depth + 1L
        for (local_target in block@locals) {
          local_name <- emit_context$source_name_by_value_id[[local_target@value_id]]
          if (is.null(local_name) || !nzchar(local_name)) {
            tccq_abort(
              "backend.unbound_write_target",
              "A typed local has no generated source name.",
              phase = "backend",
              path = sprintf("backend.%s.local", backend@id),
              data = list(backend = backend@id, value_id = local_target@value_id)
            )
          }
          local_storage_type <- emit_context$storage_type_by_value_id[[local_target@value_id]]
          if (!identical(local_storage_type, local_target@storage_type)) {
            tccq_abort(
              "backend.local_storage_type_mismatch",
              "A block local disagrees with its backend interface storage type.",
              phase = "backend",
              path = sprintf("backend.%s.local", backend@id),
              data = list(backend = backend@id, value_id = local_target@value_id)
            )
          }
          push(sprintf(
            "%s %s;",
            source_scalar_type(local_storage_type, "c"),
            local_name
          ))
        }
        for (statement in block@statements) {
          emit_statement(statement, result_target)
        }
        if (!is.null(after_statements)) {
          after_statements()
        }
        depth <<- depth - 1L
        push("}")
        invisible(NULL)
      }
      emit_statement <- function(statement, result_target) {
        if (S7::S7_inherits(statement, TccqAssignment)) {
          target <- if (statement@target@kind %in% c("local", "cell")) {
            emit_context$source_name_by_value_id[[statement@target@value_id]]
          } else {
            result_target
          }
          if (is.null(target) || !nzchar(target)) {
            tccq_abort(
              "backend.unbound_write_target",
              "A typed write target has no generated source destination.",
              phase = "backend",
              path = sprintf("backend.%s.target", backend@id),
              data = list(backend = backend@id, value_id = statement@target@value_id)
            )
          }
          push(sprintf("%s = %s;", target, expression_text(statement@value, emit_context)))
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqLoopTransfer)) {
          source_action <- if (identical(statement@action, "break")) "break" else "continue"
          push(sprintf("%s;", source_action))
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqIf)) {
          push("{")
          depth <<- depth + 1L
          emit_condition_value(statement@condition)
          push("if (condition_value != 0) {")
          depth <<- depth + 1L
          emit_statement_block(statement@consequent, result_target)
          depth <<- depth - 1L
          push("} else {")
          depth <<- depth + 1L
          emit_statement_block(statement@alternative, result_target)
          depth <<- depth - 1L
          push("}")
          depth <<- depth - 1L
          push("}")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqSwitch)) {
          selector_name <- emit_context$source_name_by_value_id[[
            statement@selector_target@value_id
          ]]
          if (is.null(selector_name) || !nzchar(selector_name)) {
            tccq_abort(
              "backend.unbound_switch_selector",
              "A positional switch has no generated selector storage.",
              phase = "backend",
              path = sprintf("backend.%s.switch", backend@id),
              data = list(backend = backend@id, statement = statement@id)
            )
          }
          push("{")
          depth <<- depth + 1L
          selector_type <- emit_context$storage_type_by_value_id[[
            statement@selector_target@value_id
          ]]
          if (!identical(selector_type, statement@selector_target@storage_type)) {
            tccq_abort(
              "backend.switch_selector_type_mismatch",
              "A positional switch selector disagrees with its backend local type.",
              phase = "backend",
              path = sprintf("backend.%s.switch", backend@id),
              data = list(backend = backend@id, statement = statement@id)
            )
          }
          push(sprintf(
            "%s %s = %s;",
            source_scalar_type(selector_type, "c"),
            selector_name,
            expression_text(statement@selector, emit_context)
          ))
          for (position in seq_along(statement@alternatives)) {
            prefix <- if (position == 1L) "if" else "else if"
            push(sprintf("%s (%s == %d) {", prefix, selector_name, position))
            depth <<- depth + 1L
            emit_statement_block(
              statement@alternatives[[position]],
              result_target
            )
            depth <<- depth - 1L
            push("}")
          }
          depth <<- depth - 1L
          push("}")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqWhile)) {
          push("while (1) {")
          depth <<- depth + 1L
          push("{")
          depth <<- depth + 1L
          emit_condition_value(statement@condition)
          push("if (condition_value == 0) break;")
          depth <<- depth - 1L
          push("}")
          emit_statement_block(statement@body, result_target)
          depth <<- depth - 1L
          push("}")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqRepeat)) {
          push("while (1) {")
          depth <<- depth + 1L
          emit_statement_block(statement@body, result_target)
          depth <<- depth - 1L
          push("}")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqFor)) {
          iteration <- statement@iteration
          axis_name <- iteration@domain@axes[[1L]]
          extent <- extent_text(
            iteration@domain@shape@dims[[1L]],
            emit_context$extent_by_symbol
          )
          iterator_name <- emit_context$source_name_by_value_id[[
            statement@iterator@value_id
          ]]
          push(sprintf(
            "for (int %s = 0; %s < %s; ++%s) {",
            axis_name,
            axis_name,
            extent,
            axis_name
          ))
          depth <<- depth + 1L
          element_text <- if (S7::S7_inherits(iteration@element, TccqExpression)) {
            expression_text(iteration@element, emit_context)
          } else {
            index_term_text(iteration@element)
          }
          push(sprintf(
            "%s = %s;",
            iterator_name,
            element_text
          ))
          emit_statement_block(statement@body, result_target)
          depth <<- depth - 1L
          push("}")
          return(invisible(NULL))
        }
        tccq_abort(
          "backend.unsupported_statement",
          "The source backend cannot emit this neutral statement class.",
          phase = "backend",
          path = sprintf("backend.%s.statement", backend@id),
          data = list(backend = backend@id, statement = statement)
        )
      }
      if (structured_body) {
        push(sprintf("%s %s;", source_scalar_type(result@type, "c"), interface@result_name))
        emit_statement_block(body, interface@result_name)
        push(sprintf("return %s;", interface@result_name))
        if (!is.null(interface@error_channel)) {
          push("tccq_runtime_failure:")
          push("return 0;")
        }
        return(paste(c(lines, "}"), collapse = "\n"))
      }
      if (returns_buffer) {
        push(sprintf("if (%s < 0) {", interface@result_count_name))
        push(if (output_argument) "return;" else "return NULL;", depth + 1L)
        push("}")
        if (!output_argument) {
          push(sprintf(
            "double *%s = (double *)malloc(sizeof(double) * (size_t)%s);",
            interface@result_name,
            interface@result_count_name
          ))
          push(sprintf("if (%s == NULL) {", interface@result_name))
          push("return NULL;", depth + 1L)
          push("}")
        }
      }
      intermediate_buffer_names <- vapply(
        intermediate_plans[vapply(intermediate_nests, function(nest) {
          nest@storage@type@shape@rank > 0L
        }, logical(1)) & first_allocation_use],
        function(intermediate_plan) intermediate_plan$materialization_name,
        character(1)
      )
      for (buffer_name in intermediate_buffer_names) {
        push(sprintf("double *%s = NULL;", buffer_name))
      }
      for (position in seq_along(intermediate_plans)) {
        intermediate_plan <- intermediate_plans[[position]]
        intermediate <- intermediate_nests[[position]]
        materializes_buffer <- intermediate@storage@type@shape@rank > 0L
        if (!materializes_buffer && isTRUE(first_allocation_use[[position]])) {
          push(sprintf(
            "%s %s;",
            source_scalar_type(intermediate@storage@type, "c"),
            intermediate_plan$materialization_name
          ))
        }
        open_guards(intermediate_plan$guards)
        if (materializes_buffer && isTRUE(first_allocation_use[[position]])) {
          push(sprintf(
            "%s = (double *)malloc(sizeof(double) * (size_t)(%s));",
            intermediate_plan$materialization_name,
            buffer_size_text(intermediate, emit_context)
          ))
          push(sprintf("if (%s == NULL) {", intermediate_plan$materialization_name))
          if (!is.na(allocation_failure_status)) {
            push(sprintf(
              "*%s = %d;",
              interface@error_channel@source_name,
              allocation_failure_status
            ), depth + 1L)
            push("goto tccq_runtime_failure;", depth + 1L)
          } else {
            for (buffer_name in intermediate_buffer_names) {
              push(sprintf("free(%s);", buffer_name), depth + 1L)
            }
            if (returns_buffer) {
              if (!output_argument) {
                push(sprintf("free(%s);", interface@result_name), depth + 1L)
              }
              push(if (output_argument) "return;" else "return NULL;", depth + 1L)
            } else {
              push("return NAN;", depth + 1L)
            }
          }
          push("}")
        }
        for (axis in intermediate_plan$map_axes) {
          open_loop(axis)
        }
        if (!is.null(intermediate@reduction)) {
          emit_state_initialization(intermediate_plan)
          for (axis in intermediate_plan$reduce_axes) {
            open_loop(axis)
          }
          if (intermediate_plan$body_requires_statements) {
            emit_statement_block(
              intermediate_plan$body,
              result_target = "",
              after_statements = function() {
                emit_state_step(intermediate_plan)
              }
            )
          } else {
            emit_state_step(intermediate_plan)
          }
          for (axis in intermediate_plan$reduce_axes) {
            close_loop(axis)
          }
          emit_reduction_validity(intermediate_plan)
        }
        materialization_target <- if (materializes_buffer) {
          sprintf(
            "%s[%s]",
            intermediate_plan$materialization_name,
            intermediate_plan$output_index
          )
        } else {
          intermediate_plan$materialization_name
        }
        if (is.null(intermediate@reduction) && intermediate_plan$body_requires_statements) {
          emit_statement_block(intermediate_plan$body, materialization_target)
        } else {
          push(sprintf(
            "%s = %s;",
            materialization_target,
            intermediate_plan$value_text
          ))
        }
        for (axis in intermediate_plan$map_axes) {
          close_loop(axis)
        }
        close_guards(intermediate_plan$guards)
      }
      for (axis in plan$map_axes) {
        open_loop(axis)
      }
      if (!is.null(result_nest@reduction)) {
        emit_state_initialization(plan)
        for (axis in plan$reduce_axes) {
          open_loop(axis)
        }
        if (plan$body_requires_statements) {
          emit_statement_block(
            plan$body,
            result_target = "",
            after_statements = function() {
              emit_state_step(plan)
            }
          )
        } else {
          emit_state_step(plan)
        }
        for (axis in plan$reduce_axes) {
          close_loop(axis)
        }
        emit_reduction_validity(plan)
      }
      if (returns_buffer) {
        output_target <- sprintf("%s[%s]", interface@result_name, plan$output_index)
        if (!is.null(result_nest@reduction)) {
          push(sprintf("%s = %s;", output_target, plan$value_text))
        } else if (plan$body_requires_statements) {
          emit_statement_block(plan$body, output_target)
        } else {
          push(sprintf("%s = %s;", output_target, plan$value_text))
        }
      }
      for (axis in plan$map_axes) {
        close_loop(axis)
      }
      for (buffer_name in intermediate_buffer_names) {
        push(sprintf("free(%s);", buffer_name))
      }
      if (returns_buffer) {
        push(if (output_argument) "return;" else sprintf("return %s;", interface@result_name))
      } else if (!is.null(result_nest@reduction)) {
        push(sprintf("return %s;", plan$value_text))
      } else if (plan$body_requires_statements) {
        push(sprintf("%s %s;", source_scalar_type(result@type, "c"), interface@result_name))
        emit_statement_block(plan$body, interface@result_name)
        push(sprintf("return %s;", interface@result_name))
      } else {
        push(sprintf("return %s;", plan$value_text))
      }
      if (!is.null(interface@error_channel)) {
        push("tccq_runtime_failure:")
        for (buffer_name in intermediate_buffer_names) {
          push(sprintf("free(%s);", buffer_name))
        }
        if (returns_buffer) {
          if (!output_argument) {
            push(sprintf("free(%s);", interface@result_name))
          }
          push(if (output_argument) "return;" else "return NULL;")
        } else {
          push("return 0;")
        }
      }
      paste(c(lines, "}"), collapse = "\n")
    }

    emit_fortran_source <- function(interface, nests, result, formals, body = NULL) {
      structured_body <- S7::S7_inherits(body, TccqValueBlock)
      result_nest <- if (structured_body) NULL else nests[[length(nests)]]
      intermediate_nests <- if (structured_body) list() else nests[-length(nests)]
      parameter_names <- vapply(
        interface@parameters,
        function(binding) binding@source_name,
        character(1)
      )
      extent_names <- vapply(
        interface@extents,
        function(binding) binding@source_name,
        character(1)
      )
      emit_context <- new_emit_context(interface, formals, "fortran")
      emit_context <- register_dim_symbols(emit_context)
      intermediate_plans <- lapply(
        intermediate_nests,
        loop_plan,
        interface = interface,
        emit_context = emit_context
      )
      intermediate_allocation_ids <- vapply(
        intermediate_nests,
        function(nest) nest@storage@allocation@id,
        character(1)
      )
      first_allocation_use <- !duplicated(intermediate_allocation_ids)
      plan <- if (structured_body) NULL else loop_plan(interface, result_nest, emit_context)
      returns_buffer <- result@type@shape@rank > 0L
      domain_parameter_names <- c(
        extent_names,
        if (nzchar(interface@result_count_name)) interface@result_count_name
      )
      status_name <- if (is.null(interface@error_channel)) {
        character()
      } else {
        interface@error_channel@source_name
      }
      value_declarations <- unlist(Map(function(value, parameter_name) {
        if (value@type@shape@rank == 0L) {
          sprintf("  %s, value :: %s", source_scalar_type(value@type, "fortran"), parameter_name)
        } else {
          sprintf("  real(c_double), intent(in) :: %s(*)", parameter_name)
        }
      }, formals, parameter_names), use.names = FALSE)
      header <- if (returns_buffer) {
        c(
          sprintf(
            "subroutine %s(%s) &",
            interface@symbol,
            paste(
              c(parameter_names, domain_parameter_names, status_name, interface@result_name),
              collapse = ", "
            )
          ),
          sprintf("  bind(c, name = \"%s\")", interface@symbol)
        )
      } else if (length(domain_parameter_names) > 0L) {
        c(
          sprintf(
            "function %s(%s) &",
            interface@symbol,
            paste(c(parameter_names, domain_parameter_names, status_name), collapse = ", ")
          ),
          sprintf("  bind(c, name = \"%s\") result(%s)", interface@symbol, interface@result_name)
        )
      } else {
        sprintf(
          "function %s(%s) bind(c, name = \"%s\") result(%s)",
          interface@symbol,
          paste(c(parameter_names, status_name), collapse = ", "),
          interface@symbol,
          interface@result_name
        )
      }
      lines <- c(
        header,
        "  use iso_c_binding, only: c_double, c_int",
        "  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan",
        "  implicit none",
        if (length(domain_parameter_names) > 0L) {
          sprintf("  integer(c_int), value :: %s", paste(domain_parameter_names, collapse = ", "))
        },
        value_declarations,
        if (length(status_name) > 0L) {
          sprintf("  integer(c_int), intent(out) :: %s", status_name)
        },
        if (returns_buffer) {
          sprintf("  real(c_double), intent(out) :: %s(*)", interface@result_name)
        } else {
          sprintf(
            "  %s :: %s",
            source_scalar_type(result@type, "fortran"),
            interface@result_name
          )
        },
        if (!structured_body && !is.null(result_nest@reduction)) {
          unlist(lapply(plan$reduction@state@components, function(component) {
            sprintf(
              "  %s :: %s",
              source_scalar_type(component@target@storage_type, "fortran"),
              emit_context$source_name_by_value_id[[component@target@value_id]]
            )
          }), use.names = FALSE)
        },
        unlist(Map(function(intermediate, intermediate_plan) {
          if (is.null(intermediate@reduction)) {
            return(character())
          }
          unlist(lapply(intermediate_plan$reduction@state@components, function(component) {
            sprintf(
              "  %s :: %s",
              source_scalar_type(component@target@storage_type, "fortran"),
              emit_context$source_name_by_value_id[[component@target@value_id]]
            )
          }), use.names = FALSE)
        }, intermediate_nests, intermediate_plans)),
        unlist(Map(function(intermediate, intermediate_plan, first_use) {
          if (!isTRUE(first_use)) {
            return(character())
          }
          if (intermediate@storage@type@shape@rank == 0L) {
            return(sprintf(
              "  %s :: %s",
              source_scalar_type(intermediate@storage@type, "fortran"),
              intermediate_plan$materialization_name
            ))
          }
          if (length(intermediate@guards) > 0L) {
            return(sprintf(
              "  real(c_double), allocatable :: %s(:)",
              intermediate_plan$materialization_name
            ))
          }
          sprintf(
            "  real(c_double) :: %s(%s)",
            intermediate_plan$materialization_name,
            buffer_size_text(intermediate, emit_context)
          )
        }, intermediate_nests, intermediate_plans, as.list(first_allocation_use))),
        if (length(interface@index_names) > 0L) {
          sprintf("  integer(c_int) :: %s", paste(interface@index_names, collapse = ", "))
        },
        "  integer(c_int), parameter :: tccq_na_logical = -huge(0_c_int) - 1_c_int"
      )
      depth <- 1L
      push <- function(text, level = depth) {
        lines <<- c(lines, paste0(strrep("  ", level), text))
      }
      if (length(status_name) > 0L) {
        push(sprintf("%s = 0_c_int", status_name))
      }
      condition_failure_status <- if (is.null(interface@error_channel)) {
        NA_integer_
      } else {
        match(
          "runtime.invalid_logical_condition",
          vapply(
            interface@error_channel@diagnostics,
            function(diagnostic) diagnostic@code,
            character(1)
          )
        )
      }
      reduction_failure_status <- if (is.null(interface@error_channel)) {
        NA_integer_
      } else {
        match(
          "runtime.reduction_has_no_value",
          vapply(
            interface@error_channel@diagnostics,
            function(diagnostic) diagnostic@code,
            character(1)
          )
        )
      }
      emit_state_initialization <- function(reduction_plan) {
        for (component in reduction_plan$reduction@state@components) {
          source_name <- emit_context$source_name_by_value_id[[component@target@value_id]]
          push(sprintf(
            "%s = %s",
            source_name,
            literal_text(component@identity, "fortran")
          ))
        }
      }
      emit_state_step <- function(reduction_plan) {
        condition <- reduction_plan$reduction@condition
        if (!is.null(condition)) {
          push("block")
          depth <<- depth + 1L
          push("integer(c_int) :: condition_value")
          emit_condition_value(condition)
          push("if (condition_value /= 0_c_int) then")
          depth <<- depth + 1L
        }
        for (assignment in reduction_plan$reduction@updates) {
          source_name <- emit_context$source_name_by_value_id[[assignment@target@value_id]]
          push(sprintf(
            "%s = %s",
            source_name,
            expression_text(assignment@value, emit_context)
          ))
        }
        if (!is.null(condition)) {
          depth <<- depth - 1L
          push("end if")
          depth <<- depth - 1L
          push("end block")
        }
      }
      emit_reduction_validity <- function(reduction_plan) {
        validity <- reduction_plan$reduction@valid
        if (is.null(validity)) {
          return(invisible(NULL))
        }
        if (is.na(reduction_failure_status)) {
          tccq_abort(
            "backend.missing_error_channel",
            "A fallible reduction requires a callable error channel.",
            phase = "backend",
            path = sprintf("backend.%s.error_channel", backend@id)
          )
        }
        push(sprintf(
          "if ((%s) == 0_c_int) then",
          expression_text(validity, emit_context)
        ))
        push(sprintf(
          "%s = %d_c_int",
          status_name,
          reduction_failure_status
        ), depth + 1L)
        if (!returns_buffer) {
          zero <- if (result@type@base %in% c("logical", "integer")) {
            "0_c_int"
          } else {
            "0.0_c_double"
          }
          push(sprintf("%s = %s", interface@result_name, zero), depth + 1L)
        }
        push("return", depth + 1L)
        push("end if")
        invisible(NULL)
      }
      emit_condition_value <- function(condition) {
        if (is.na(condition_failure_status)) {
          tccq_abort(
            "backend.missing_error_channel",
            "A generated control condition requires a callable error channel.",
            phase = "backend",
            path = sprintf("backend.%s.error_channel", backend@id)
          )
        }
        push(sprintf("condition_value = %s", expression_text(condition, emit_context)))
        push("if (condition_value == tccq_na_logical) then")
        push(sprintf(
          "%s = %d_c_int",
          status_name,
          condition_failure_status
        ), depth + 1L)
        if (!returns_buffer) {
          push(sprintf(
            "%s = %s",
            interface@result_name,
            if (result@type@base %in% c("logical", "integer")) "0_c_int" else "0.0_c_double"
          ), depth + 1L)
        }
        push("return", depth + 1L)
        push("end if")
      }
      finish_source <- function() {
        completed_lines <- c(
          lines,
          sprintf("end %s %s", if (returns_buffer) "subroutine" else "function", interface@symbol)
        )
        wrap_line <- function(line) {
          pieces <- character()
          while (nchar(line) > 100L) {
            break_at <- max(gregexpr(" ", substr(line, 1L, 100L))[[1L]])
            if (break_at <= 1L) {
              break
            }
            pieces <- c(pieces, paste0(substr(line, 1L, break_at - 1L), " &"))
            line <- paste0("    ", substr(line, break_at + 1L, nchar(line)))
          }
          c(pieces, line)
        }
        paste(unlist(lapply(completed_lines, wrap_line), use.names = FALSE), collapse = "\n")
      }
      open_loop <- function(axis) {
        extent <- extent_text(axis@extent, emit_context$extent_by_symbol)
        push(sprintf("do %s = 0, %s - 1", axis@name, extent))
        depth <<- depth + 1L
      }
      close_loop <- function(axis) {
        depth <<- depth - 1L
        push("end do")
      }
      open_guards <- function(guards) {
        for (guard in guards) {
          push("block")
          depth <<- depth + 1L
          push("integer(c_int) :: condition_value")
          emit_condition_value(guard@condition)
          comparison <- if (isTRUE(guard@selected)) "/= 0_c_int" else "== 0_c_int"
          push(sprintf("if (condition_value %s) then", comparison))
          depth <<- depth + 1L
        }
      }
      close_guards <- function(guards) {
        for (guard in rev(guards)) {
          depth <<- depth - 1L
          push("end if")
          depth <<- depth - 1L
          push("end block")
        }
      }
      emit_statement <- NULL
      emit_statement_block <- function(block, result_target, after_statements = NULL) {
        push("block")
        depth <<- depth + 1L
        for (local_target in block@locals) {
          local_name <- emit_context$source_name_by_value_id[[local_target@value_id]]
          if (is.null(local_name) || !nzchar(local_name)) {
            tccq_abort(
              "backend.unbound_write_target",
              "A typed local has no generated source name.",
              phase = "backend",
              path = sprintf("backend.%s.local", backend@id),
              data = list(backend = backend@id, value_id = local_target@value_id)
            )
          }
          local_storage_type <- emit_context$storage_type_by_value_id[[local_target@value_id]]
          if (!identical(local_storage_type, local_target@storage_type)) {
            tccq_abort(
              "backend.local_storage_type_mismatch",
              "A block local disagrees with its backend interface storage type.",
              phase = "backend",
              path = sprintf("backend.%s.local", backend@id),
              data = list(backend = backend@id, value_id = local_target@value_id)
            )
          }
          push(sprintf(
            "%s :: %s",
            source_scalar_type(local_storage_type, "fortran"),
            local_name
          ))
        }
        for (statement in block@statements) {
          emit_statement(statement, result_target)
        }
        if (!is.null(after_statements)) {
          after_statements()
        }
        depth <<- depth - 1L
        push("end block")
        invisible(NULL)
      }
      emit_statement <- function(statement, result_target) {
        if (S7::S7_inherits(statement, TccqAssignment)) {
          target <- if (statement@target@kind %in% c("local", "cell")) {
            emit_context$source_name_by_value_id[[statement@target@value_id]]
          } else {
            result_target
          }
          if (is.null(target) || !nzchar(target)) {
            tccq_abort(
              "backend.unbound_write_target",
              "A typed write target has no generated source destination.",
              phase = "backend",
              path = sprintf("backend.%s.target", backend@id),
              data = list(backend = backend@id, value_id = statement@target@value_id)
            )
          }
          push(sprintf("%s = %s", target, expression_text(statement@value, emit_context)))
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqLoopTransfer)) {
          source_action <- if (identical(statement@action, "break")) "exit" else "cycle"
          push(source_action)
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqIf)) {
          push("block")
          depth <<- depth + 1L
          push("integer(c_int) :: condition_value")
          emit_condition_value(statement@condition)
          push("if (condition_value /= 0_c_int) then")
          depth <<- depth + 1L
          emit_statement_block(statement@consequent, result_target)
          depth <<- depth - 1L
          push("else")
          depth <<- depth + 1L
          emit_statement_block(statement@alternative, result_target)
          depth <<- depth - 1L
          push("end if")
          depth <<- depth - 1L
          push("end block")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqSwitch)) {
          selector_name <- emit_context$source_name_by_value_id[[
            statement@selector_target@value_id
          ]]
          if (is.null(selector_name) || !nzchar(selector_name)) {
            tccq_abort(
              "backend.unbound_switch_selector",
              "A positional switch has no generated selector storage.",
              phase = "backend",
              path = sprintf("backend.%s.switch", backend@id),
              data = list(backend = backend@id, statement = statement@id)
            )
          }
          push("block")
          depth <<- depth + 1L
          selector_type <- emit_context$storage_type_by_value_id[[
            statement@selector_target@value_id
          ]]
          if (!identical(selector_type, statement@selector_target@storage_type)) {
            tccq_abort(
              "backend.switch_selector_type_mismatch",
              "A positional switch selector disagrees with its backend local type.",
              phase = "backend",
              path = sprintf("backend.%s.switch", backend@id),
              data = list(backend = backend@id, statement = statement@id)
            )
          }
          push(sprintf(
            "%s :: %s",
            source_scalar_type(selector_type, "fortran"),
            selector_name
          ))
          push(sprintf(
            "%s = %s",
            selector_name,
            expression_text(statement@selector, emit_context)
          ))
          for (position in seq_along(statement@alternatives)) {
            prefix <- if (position == 1L) "if" else "else if"
            push(sprintf(
              "%s (%s == %d_c_int) then",
              prefix,
              selector_name,
              position
            ))
            depth <<- depth + 1L
            emit_statement_block(
              statement@alternatives[[position]],
              result_target
            )
            depth <<- depth - 1L
          }
          push("end if")
          depth <<- depth - 1L
          push("end block")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqWhile)) {
          push("do")
          depth <<- depth + 1L
          push("block")
          depth <<- depth + 1L
          push("integer(c_int) :: condition_value")
          emit_condition_value(statement@condition)
          push("if (condition_value == 0_c_int) exit")
          depth <<- depth - 1L
          push("end block")
          emit_statement_block(statement@body, result_target)
          depth <<- depth - 1L
          push("end do")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqRepeat)) {
          push("do")
          depth <<- depth + 1L
          emit_statement_block(statement@body, result_target)
          depth <<- depth - 1L
          push("end do")
          return(invisible(NULL))
        }
        if (S7::S7_inherits(statement, TccqFor)) {
          iteration <- statement@iteration
          axis_name <- iteration@domain@axes[[1L]]
          extent <- extent_text(
            iteration@domain@shape@dims[[1L]],
            emit_context$extent_by_symbol
          )
          iterator_name <- emit_context$source_name_by_value_id[[
            statement@iterator@value_id
          ]]
          push(sprintf(
            "do %s = 0_c_int, %s - 1_c_int",
            axis_name,
            extent
          ))
          depth <<- depth + 1L
          element_text <- if (S7::S7_inherits(iteration@element, TccqExpression)) {
            expression_text(iteration@element, emit_context)
          } else {
            index_term_text(iteration@element)
          }
          push(sprintf(
            "%s = %s",
            iterator_name,
            element_text
          ))
          emit_statement_block(statement@body, result_target)
          depth <<- depth - 1L
          push("end do")
          return(invisible(NULL))
        }
        tccq_abort(
          "backend.unsupported_statement",
          "The source backend cannot emit this neutral statement class.",
          phase = "backend",
          path = sprintf("backend.%s.statement", backend@id),
          data = list(backend = backend@id, statement = statement)
        )
      }
      if (structured_body) {
        emit_statement_block(body, interface@result_name)
        return(finish_source())
      }
      for (position in seq_along(intermediate_plans)) {
        intermediate_plan <- intermediate_plans[[position]]
        intermediate <- intermediate_nests[[position]]
        materializes_buffer <- intermediate@storage@type@shape@rank > 0L
        open_guards(intermediate_plan$guards)
        if (
          materializes_buffer &&
            isTRUE(first_allocation_use[[position]]) &&
            length(intermediate_plan$guards) > 0L
        ) {
          push(sprintf(
            "allocate(%s(%s))",
            intermediate_plan$materialization_name,
            buffer_size_text(intermediate, emit_context)
          ))
        }
        for (axis in intermediate_plan$map_axes) {
          open_loop(axis)
        }
        if (!is.null(intermediate@reduction)) {
          emit_state_initialization(intermediate_plan)
          for (axis in intermediate_plan$reduce_axes) {
            open_loop(axis)
          }
          if (intermediate_plan$body_requires_statements) {
            emit_statement_block(
              intermediate_plan$body,
              result_target = "",
              after_statements = function() {
                emit_state_step(intermediate_plan)
              }
            )
          } else {
            emit_state_step(intermediate_plan)
          }
          for (axis in intermediate_plan$reduce_axes) {
            close_loop(axis)
          }
          emit_reduction_validity(intermediate_plan)
        }
        materialization_target <- if (materializes_buffer) {
          sprintf(
            "%s(%s + 1)",
            intermediate_plan$materialization_name,
            intermediate_plan$output_index
          )
        } else {
          intermediate_plan$materialization_name
        }
        if (is.null(intermediate@reduction) && intermediate_plan$body_requires_statements) {
          emit_statement_block(intermediate_plan$body, materialization_target)
        } else {
          push(sprintf(
            "%s = %s",
            materialization_target,
            intermediate_plan$value_text
          ))
        }
        for (axis in intermediate_plan$map_axes) {
          close_loop(axis)
        }
        close_guards(intermediate_plan$guards)
      }
      for (axis in plan$map_axes) {
        open_loop(axis)
      }
      if (!is.null(result_nest@reduction)) {
        emit_state_initialization(plan)
        for (axis in plan$reduce_axes) {
          open_loop(axis)
        }
        if (plan$body_requires_statements) {
          emit_statement_block(
            plan$body,
            result_target = "",
            after_statements = function() {
              emit_state_step(plan)
            }
          )
        } else {
          emit_state_step(plan)
        }
        for (axis in plan$reduce_axes) {
          close_loop(axis)
        }
        emit_reduction_validity(plan)
      }
      if (returns_buffer) {
        output_target <- sprintf("%s(%s + 1)", interface@result_name, plan$output_index)
        if (!is.null(result_nest@reduction)) {
          push(sprintf("%s = %s", output_target, plan$value_text))
        } else if (plan$body_requires_statements) {
          emit_statement_block(plan$body, output_target)
        } else {
          push(sprintf("%s = %s", output_target, plan$value_text))
        }
      }
      for (axis in plan$map_axes) {
        close_loop(axis)
      }
      for (position in seq_along(intermediate_nests)) {
        intermediate <- intermediate_nests[[position]]
        if (
          intermediate@storage@type@shape@rank > 0L &&
            isTRUE(first_allocation_use[[position]]) &&
            length(intermediate@guards) > 0L
        ) {
          intermediate_name <- intermediate_plans[[position]]$materialization_name
          push(sprintf(
            "if (allocated(%s)) deallocate(%s)",
            intermediate_name,
            intermediate_name
          ))
        }
      }
      if (!returns_buffer) {
        if (!is.null(result_nest@reduction)) {
          push(sprintf("%s = %s", interface@result_name, plan$value_text))
        } else if (plan$body_requires_statements) {
          emit_statement_block(plan$body, interface@result_name)
        } else {
          push(sprintf("%s = %s", interface@result_name, plan$value_text))
        }
      }
      finish_source()
    }

    emit_call_wrapper_source <- function(interface, result, formals) {
      symbol <- interface@symbol
      wrapper_symbol <- paste0(symbol, "_call")
      parameter_names <- vapply(
        interface@parameters,
        function(binding) binding@source_name,
        character(1)
      )
      extent_symbols <- vapply(
        interface@extents,
        function(binding) binding@symbol,
        character(1)
      )
      extent_names <- vapply(
        interface@extents,
        function(binding) binding@source_name,
        character(1)
      )
      extent_name_by_symbol <- as.list(stats::setNames(
        extent_names,
        extent_symbols
      ))
      result_rank <- result@type@shape@rank
      returns_buffer <- result_rank > 0L

      kernel_parameters <- unlist(Map(function(value, parameter_name) {
        if (value@type@shape@rank == 0L) {
          sprintf("%s %s", source_scalar_type(value@type, "c"), parameter_name)
        } else {
          sprintf("const double *%s", parameter_name)
        }
      }, formals, parameter_names), use.names = FALSE)
      kernel_parameters <- c(
        kernel_parameters,
        if (length(extent_names) > 0L) sprintf("int %s", extent_names),
        if (nzchar(interface@result_count_name)) sprintf("int %s", interface@result_count_name),
        if (!is.null(interface@error_channel)) {
          sprintf("int *%s", interface@error_channel@source_name)
        }
      )
      prototype <- if (identical(interface@result_placement, "output_argument")) {
        sprintf(
          "extern void %s(%s);",
          symbol,
          paste(c(kernel_parameters, sprintf("double *%s", interface@result_name)), collapse = ", ")
        )
      } else if (returns_buffer) {
        sprintf("extern double *%s(%s);", symbol, paste(kernel_parameters, collapse = ", "))
      } else {
        sprintf(
          "extern %s %s(%s);",
          source_scalar_type(result@type, "c"),
          symbol,
          paste(kernel_parameters, collapse = ", ")
        )
      }

      wrapper_parameters <- sprintf("SEXP %s_arg", parameter_names)
      lines <- c(
        "#include <R.h>",
        "#include <Rinternals.h>",
        "#include <limits.h>",
        "#include <stdlib.h>",
        "#include <string.h>",
        "",
        prototype,
        "",
        sprintf("SEXP %s(%s) {", wrapper_symbol, paste(wrapper_parameters, collapse = ", ")),
        "  int protect_count = 0;",
        if (!is.null(interface@error_channel)) {
          sprintf("  int %s = 0;", interface@error_channel@source_name)
        }
      )

      for (position in seq_along(formals)) {
        value <- formals[[position]]
        parameter_name <- parameter_names[[position]]
        rank <- value@type@shape@rank
        if (identical(value@type@base, "logical")) {
          lines <- c(
            lines,
            sprintf(
              "  SEXP %s_logical = PROTECT(Rf_coerceVector(%s_arg, LGLSXP));",
              parameter_name,
              parameter_name
            ),
            "  ++protect_count;",
            sprintf("  if (XLENGTH(%s_logical) < 1) {", parameter_name),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"scalar logical arguments must have length at least one\");",
            "  }",
            sprintf("  int %s_logical_value = LOGICAL(%s_logical)[0];", parameter_name, parameter_name),
            sprintf("  if (%s_logical_value == NA_LOGICAL) {", parameter_name),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"missing values are not allowed in scalar logical conditions\");",
            "  }",
            sprintf("  int %s = %s_logical_value != 0;", parameter_name, parameter_name)
          )
          next
        }
        lines <- c(
          lines,
          sprintf(
            "  SEXP %s_real = PROTECT(Rf_coerceVector(%s_arg, REALSXP));",
            parameter_name,
            parameter_name
          ),
          "  ++protect_count;"
        )
        if (rank == 0L) {
          lines <- c(
            lines,
            sprintf("  if (XLENGTH(%s_real) < 1) {", parameter_name),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"scalar arguments must have length at least one\");",
            "  }",
            sprintf("  double %s = REAL(%s_real)[0];", parameter_name, parameter_name)
          )
          next
        }
        if (rank > 1L) {
          lines <- c(
            lines,
            sprintf(
              "  SEXP %s_dim = PROTECT(Rf_getAttrib(%s_arg, R_DimSymbol));",
              parameter_name,
              parameter_name
            ),
            "  ++protect_count;",
            sprintf(
              "  if (TYPEOF(%s_dim) != INTSXP || XLENGTH(%s_dim) != %d) {",
              parameter_name,
              parameter_name,
              rank
            ),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"array arguments must have the declared rank\");",
            "  }"
          )
        }
        lines <- c(lines, sprintf("  const double *%s = REAL(%s_real);", parameter_name, parameter_name))
      }

      bound_symbols <- character()
      for (position in seq_along(formals)) {
        value <- formals[[position]]
        parameter_name <- parameter_names[[position]]
        rank <- value@type@shape@rank
        if (rank == 0L) {
          next
        }
        for (axis in seq_len(rank)) {
          dim <- value@type@shape@dims[[axis]]
          axis_source <- if (rank == 1L) {
            sprintf("XLENGTH(%s_real)", parameter_name)
          } else {
            sprintf("(R_xlen_t)INTEGER(%s_dim)[%d]", parameter_name, axis - 1L)
          }
          if (identical(dim@kind, "constant")) {
            lines <- c(
              lines,
              sprintf("  if (%s != %d) {", axis_source, dim@value),
              "    UNPROTECT(protect_count);",
              sprintf(
                "    Rf_error(\"argument `%s` must have declared extent %d on axis %d\");",
                parameter_name,
                dim@value,
                axis
              ),
              "  }"
            )
            next
          }
          if (!identical(dim@kind, "symbol")) {
            tccq_abort(
              "backend.unsupported_formal_extent",
              "Declared formals must have constant or symbolic extents.",
              phase = "backend",
              path = sprintf("backend.%s.wrapper", backend@id),
              data = list(backend = backend@id, kind = dim@kind)
            )
          }
          extent_name <- extent_name_by_symbol[[dim@label]]
          if (!dim@label %in% bound_symbols) {
            lines <- c(
              lines,
              sprintf("  if (%s > INT_MAX) {", axis_source),
              "    UNPROTECT(protect_count);",
              "    Rf_error(\"generated call exceeds the current int extent ABI\");",
              "  }",
              sprintf("  int %s = (int)(%s);", extent_name, axis_source)
            )
            bound_symbols <- c(bound_symbols, dim@label)
          } else {
            lines <- c(
              lines,
              sprintf("  if ((R_xlen_t)%s != %s) {", extent_name, axis_source),
              "    UNPROTECT(protect_count);",
              sprintf("    Rf_error(\"arguments disagree on declared extent `%s`\");", dim@label),
              "  }"
            )
          }
        }
      }
      unbound_symbols <- setdiff(extent_symbols, bound_symbols)
      if (length(unbound_symbols) > 0L) {
        tccq_abort(
          "backend.unbound_extent_symbol",
          "Extent symbols must be bindable from declared formal shapes.",
          phase = "backend",
          path = sprintf("backend.%s.wrapper", backend@id),
          data = list(backend = backend@id, symbols = unbound_symbols)
        )
      }

      call_arguments <- c(parameter_names, extent_names)
      if (!is.null(interface@error_channel)) {
        call_arguments <- c(
          call_arguments,
          sprintf("&%s", interface@error_channel@source_name)
        )
      }
      emit_status_check <- function() {
        if (is.null(interface@error_channel)) {
          return(character())
        }
        status_name <- interface@error_channel@source_name
        c(
          sprintf("  if (%s != 0) {", status_name),
          sprintf("    SEXP runtime_status = PROTECT(Rf_ScalarInteger(%s));", status_name),
          "    ++protect_count;",
          sprintf(
            "    SEXP runtime_status_class = PROTECT(Rf_mkString(%s));",
            encodeString(TCCQ_RUNTIME_STATUS_CLASS, quote = "\"")
          ),
          "    ++protect_count;",
          "    Rf_classgets(runtime_status, runtime_status_class);",
          "    UNPROTECT(protect_count);",
          "    return runtime_status;",
          "  }"
        )
      }
      if (returns_buffer) {
        lines <- c(lines, "  R_xlen_t result_element_count = 1;")
        result_extent_vars <- sprintf("result_extent_%04d", seq_len(result_rank))
        for (axis in seq_len(result_rank)) {
          extent <- extent_text(interface@result_dims[[axis]], extent_name_by_symbol)
          lines <- c(
            lines,
            sprintf("  int %s = %s;", result_extent_vars[[axis]], extent),
            sprintf("  if (%s < 0) {", result_extent_vars[[axis]]),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"result extents must be non-negative\");",
            "  }",
            sprintf("  result_element_count *= (R_xlen_t)%s;", result_extent_vars[[axis]])
          )
        }
        lines <- c(
          lines,
          "  if (result_element_count > INT_MAX) {",
          "    UNPROTECT(protect_count);",
          "    Rf_error(\"generated call exceeds the current int length ABI\");",
          "  }",
          sprintf("  int %s = (int)result_element_count;", interface@result_count_name),
          "  SEXP output_sexp = PROTECT(Rf_allocVector(REALSXP, result_element_count));",
          "  ++protect_count;"
        )
        if (result_rank > 1L) {
          lines <- c(
            lines,
            sprintf("  SEXP output_dim = PROTECT(Rf_allocVector(INTSXP, %d));", result_rank),
            "  ++protect_count;",
            vapply(seq_len(result_rank), function(axis) {
              sprintf("  INTEGER(output_dim)[%d] = %s;", axis - 1L, result_extent_vars[[axis]])
            }, character(1)),
            "  Rf_setAttrib(output_sexp, R_DimSymbol, output_dim);"
          )
        }
        status_argument <- if (is.null(interface@error_channel)) {
          character()
        } else {
          utils::tail(call_arguments, 1L)
        }
        if (length(status_argument) > 0L) {
          call_arguments <- utils::head(call_arguments, -1L)
        }
        call_arguments <- c(call_arguments, interface@result_count_name, status_argument)
        if (identical(interface@result_placement, "output_argument")) {
          lines <- c(
            lines,
            sprintf(
              "  %s(%s);",
              symbol,
              paste(c(call_arguments, "REAL(output_sexp)"), collapse = ", ")
            ),
            emit_status_check()
          )
        } else {
          lines <- c(
            lines,
            sprintf(
              "  double *%s = %s(%s);",
              interface@result_name,
              symbol,
              paste(call_arguments, collapse = ", ")
            ),
            emit_status_check(),
            sprintf("  if (%s == NULL) {", interface@result_name),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"generated kernel returned NULL\");",
            "  }",
            sprintf(
              "  memcpy(REAL(output_sexp), %s, sizeof(double) * (size_t)result_element_count);",
              interface@result_name
            ),
            sprintf("  free(%s);", interface@result_name)
          )
        }
        lines <- c(lines, "  UNPROTECT(protect_count);", "  return output_sexp;")
      } else {
        scalar_result_type <- source_scalar_type(result@type, "c")
        scalar_constructor <- switch(
          result@type@base,
          logical = "Rf_ScalarLogical",
          integer = "Rf_ScalarInteger",
          "Rf_ScalarReal"
        )
        lines <- c(
          lines,
          sprintf(
            "  %s output_value = %s(%s);",
            scalar_result_type,
            symbol,
            paste(call_arguments, collapse = ", ")
          ),
          emit_status_check(),
          "  UNPROTECT(protect_count);",
          sprintf("  return %s(output_value);", scalar_constructor)
        )
      }

      paste(c(lines, "}"), collapse = "\n")
    }

    compile_with_rtinycc <- function(plan, interface, result, formals) {
      symbol <- interface@symbol
      extent_symbols <- vapply(
        interface@extents,
        function(binding) binding@symbol,
        character(1)
      )
      if (!requireNamespace("Rtinycc", quietly = TRUE)) {
        diagnostic <- tccq_diagnostic(
          "backend.rtinycc_unavailable",
          "`Rtinycc` is required for jit mode.",
          phase = "backend",
          path = "backend.rtinycc",
          data = list(backend = backend@id, program = program@name)
        )
        plan@diagnostics <- c(plan@diagnostics, list(diagnostic))
        return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
      }

      returns_buffer <- result@type@shape@rank > 0L
      output_argument <- identical(interface@result_placement, "output_argument")
      ffi_arg_types <- lapply(formals, function(value) {
        if (value@type@shape@rank > 0L) {
          return("numeric_array")
        }
        if (identical(value@type@base, "logical")) "i32" else "f64"
      })
      if (length(interface@extents) > 0L) {
        ffi_arg_types <- c(ffi_arg_types, rep(list("i32"), length(interface@extents)))
      }
      if (returns_buffer) {
        ffi_arg_types <- c(ffi_arg_types, list("i32"))
        if (output_argument) {
          ffi_return <- "void"
        } else {
          result_count_position <- length(ffi_arg_types)
          ffi_return <- list(type = "numeric_array", length_arg = result_count_position, free = TRUE)
        }
      } else {
        ffi_return <- if (result@type@base %in% c("logical", "integer")) "i32" else "f64"
      }
      if (!is.null(interface@error_channel)) {
        ffi_arg_types <- c(ffi_arg_types, list("integer_array"))
      }
      if (output_argument) {
        ffi_arg_types <- c(ffi_arg_types, list("numeric_array"))
      }

      ffi <- Rtinycc::tcc_ffi()
      ffi <- do.call(Rtinycc::tcc_bind, c(
        list(ffi),
        stats::setNames(list(list(args = ffi_arg_types, returns = ffi_return)), symbol)
      ))
      ffi <- Rtinycc::tcc_source(ffi, plan@products@attrs$source)
      compiled <- tryCatch(
        Rtinycc::tcc_compile(ffi),
        error = identity
      )
      if (inherits(compiled, "error")) {
        diagnostic <- tccq_diagnostic(
          "backend.rtinycc_compile_failed",
          conditionMessage(compiled),
          phase = "backend",
          path = "backend.rtinycc.compile",
          data = list(backend = backend@id, program = program@name)
        )
        plan@diagnostics <- c(plan@diagnostics, list(diagnostic))
        return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
      }

      evaluate_result_dim <- function(dim, bound_extents) {
        if (identical(dim@kind, "constant")) {
          return(dim@value)
        }
        base <- bound_extents[[dim@label]]
        if (is.null(base)) {
          tccq_abort(
            "runtime.unbound_extent",
            "A result extent symbol is not bound by any argument shape.",
            phase = "runtime",
            path = "callable.extents",
            data = list(symbol = dim@label)
          )
        }
        if (identical(dim@kind, "affine")) base + dim@value else base
      }
      callable <- function(...) {
        arguments <- list(...)
        if (length(arguments) != length(formals)) {
          tccq_abort(
            "runtime.invalid_argument_count",
            "Callable received the wrong number of arguments.",
            phase = "runtime",
            path = "callable.arguments",
            data = list(expected = length(formals), actual = length(arguments))
          )
        }
        bound_extents <- list()
        for (position in seq_along(arguments)) {
          shape <- formals[[position]]@type@shape
          argument <- arguments[[position]]
          if (shape@rank == 0L) {
            if (identical(formals[[position]]@type@base, "logical")) {
              condition <- as.logical(argument)
              if (length(condition) == 0L || is.na(condition[[1L]])) {
                tccq_abort(
                  "runtime.invalid_logical_condition",
                  "Scalar logical arguments must contain one non-missing value.",
                  phase = "runtime",
                  path = "callable.arguments",
                  data = list(argument = position)
                )
              }
              arguments[[position]] <- as.integer(condition[[1L]])
            }
            next
          }
          actual_dims <- if (shape@rank == 1L) length(argument) else dim(argument)
          if (length(actual_dims) != shape@rank) {
            tccq_abort(
              "runtime.invalid_rank",
              "Array arguments must have the declared rank.",
              phase = "runtime",
              path = "callable.arguments",
              data = list(
                argument = position,
                expected = shape@rank,
                actual = length(actual_dims)
              )
            )
          }
          for (axis in seq_len(shape@rank)) {
            actual_extent <- as.integer(actual_dims[[axis]])
            declared <- shape@dims[[axis]]
            if (identical(declared@kind, "constant")) {
              if (actual_extent != declared@value) {
                tccq_abort(
                  "runtime.incompatible_dimensions",
                  "An argument extent disagrees with its declared constant extent.",
                  phase = "runtime",
                  path = "callable.arguments",
                  data = list(argument = position, axis = axis, expected = declared@value, actual = actual_extent)
                )
              }
              next
            }
            existing <- bound_extents[[declared@label]]
            if (is.null(existing)) {
              bound_extents[[declared@label]] <- actual_extent
            } else if (!identical(existing, actual_extent)) {
              tccq_abort(
                "runtime.incompatible_dimensions",
                "Arguments disagree on a declared extent symbol.",
                phase = "runtime",
                path = "callable.arguments",
                data = list(argument = position, axis = axis, symbol = declared@label, expected = existing, actual = actual_extent)
              )
            }
          }
        }
        extent_values <- vapply(extent_symbols, function(symbol_name) {
          value <- bound_extents[[symbol_name]]
          if (is.null(value)) {
            tccq_abort(
              "runtime.unbound_extent",
              "An extent symbol is not bound by any argument shape.",
              phase = "runtime",
              path = "callable.extents",
              data = list(symbol = symbol_name)
            )
          }
          value
        }, integer(1), USE.NAMES = FALSE)
        call_arguments <- c(arguments, as.list(extent_values))
        result_dim_values <- NULL
        if (returns_buffer) {
          result_dim_values <- vapply(interface@result_dims, function(dim) {
            as.integer(evaluate_result_dim(dim, bound_extents))
          }, integer(1))
          if (any(result_dim_values < 0L)) {
            tccq_abort(
              "runtime.negative_extent",
              "Result extents must be non-negative.",
              phase = "runtime",
              path = "callable.extents",
              data = list(dims = result_dim_values)
            )
          }
          call_arguments <- c(call_arguments, list(as.integer(prod(result_dim_values))))
        }
        output_value <- if (output_argument) {
          numeric(prod(result_dim_values))
        } else {
          NULL
        }
        runtime_status <- NULL
        if (!is.null(interface@error_channel)) {
          runtime_status <- integer(1L)
          call_arguments <- c(call_arguments, list(runtime_status))
        }
        if (output_argument) {
          call_arguments <- c(call_arguments, list(output_value))
        }
        call_result <- do.call(compiled[[symbol]], call_arguments)
        if (!is.null(runtime_status) && runtime_status[[1L]] != 0L) {
          status <- runtime_status[[1L]]
          diagnostics <- interface@error_channel@diagnostics
          if (status < 1L || status > length(diagnostics)) {
            tccq_abort(
              "runtime.unknown_generated_status",
              "Generated code returned an unknown runtime status.",
              phase = "runtime",
              path = "callable.status",
              data = list(status = status)
            )
          }
          tccq_abort_diagnostic(diagnostics[[status]])
        }
        value <- if (output_argument) output_value else call_result
        if (identical(result@type@base, "logical") && result@type@shape@rank == 0L) {
          value <- as.logical(value)
        }
        if (result@type@shape@rank > 1L) {
          dim(value) <- result_dim_values
        }
        value
      }

      attrs <- product_attrs(plan)
      attrs$compiled <- compiled
      attrs$callable <- callable
      plan <- set_product_attrs(plan, attrs)
      plan <- set_product_artifact(plan, "jit_callable", jit_callable_artifact(symbol, callable))
      tccq_result(success = TRUE, value = plan, diagnostics = list())
    }

    compile_shared_library <- function(plan, source, interface, result, formals) {
      symbol <- interface@symbol
      wrapper_symbol <- paste0(symbol, "_call")
      wrapper_source <- emit_call_wrapper_source(interface, result, formals)
      source_extension <- switch(
        source_language,
        c = ".c",
        fortran = ".f90",
        tccq_abort(
          "backend.unsupported_source_language",
          "Shared-library compilation does not support this source language.",
          phase = "backend",
          path = sprintf("backend.%s.shared_library.source_language", backend@id),
          data = list(backend = backend@id, source_language = source_language)
        )
      )
      build_dir <- tempfile(sprintf("tccq_%s_", source_language))
      source_filename <- paste0(symbol, source_extension)
      wrapper_filename <- paste0(wrapper_symbol, ".c")
      source_path <- file.path(build_dir, source_filename)
      wrapper_source_path <- file.path(build_dir, wrapper_filename)
      library_path <- file.path(
        build_dir,
        paste0(sub("\\.[^.]*$", "", source_filename), .Platform$dynlib.ext)
      )

      write_result <- tryCatch({
        dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
        writeLines(source, source_path, useBytes = TRUE)
        writeLines(wrapper_source, wrapper_source_path, useBytes = TRUE)
        NULL
      }, error = identity)
      attrs <- product_attrs(plan)
      attrs$source_path <- source_path
      attrs$wrapper_source <- wrapper_source
      attrs$wrapper_source_path <- wrapper_source_path
      attrs$wrapper_symbol <- wrapper_symbol
      plan <- set_product_attrs(plan, attrs)
      plan <- set_product_artifact(plan, "source", source_artifact(symbol, source, path = source_path))
      plan <- set_product_artifact(plan, "wrapper_source", source_artifact(wrapper_symbol, wrapper_source, path = wrapper_source_path))
      if (inherits(write_result, "error")) {
        diagnostic <- tccq_diagnostic(
          "backend.shared_library_write_failed",
          conditionMessage(write_result),
          phase = "backend",
          path = sprintf("backend.%s.shared_library.write", backend@id),
          data = list(
            backend = backend@id,
            program = program@name,
            source_language = source_language,
            source_path = source_path,
            wrapper_source_path = wrapper_source_path
          )
        )
        plan@diagnostics <- c(plan@diagnostics, list(diagnostic))
        return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
      }

      old_working_directory <- getwd()
      on.exit(setwd(old_working_directory), add = TRUE)
      setwd(build_dir)
      compile_output <- tryCatch(
        suppressWarnings(system2(
          file.path(R.home("bin"), "R"),
          c("CMD", "SHLIB", basename(source_path), basename(wrapper_source_path)),
          stdout = TRUE,
          stderr = TRUE
        )),
        error = identity
      )
      compile_status <- if (inherits(compile_output, "error")) {
        1L
      } else {
        status <- attr(compile_output, "status")
        if (is.null(status)) 0L else as.integer(status)
      }
      compile_log <- if (inherits(compile_output, "error")) {
        conditionMessage(compile_output)
      } else {
        paste(as.character(compile_output), collapse = "\n")
      }
      if (!identical(compile_status, 0L) || !file.exists(library_path)) {
        diagnostic <- tccq_diagnostic(
          "backend.shared_library_compile_failed",
          "Shared-library compilation failed.",
          phase = "backend",
          path = sprintf("backend.%s.shared_library.compile", backend@id),
          data = list(
            backend = backend@id,
            program = program@name,
            source_language = source_language,
            source_path = source_path,
            wrapper_source_path = wrapper_source_path,
            shared_library_path = library_path,
            status = compile_status,
            output = compile_log
          )
        )
        plan@diagnostics <- c(plan@diagnostics, list(diagnostic))
        return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
      }

      attrs <- product_attrs(plan)
      attrs$shared_library_path <- library_path
      attrs$shared_library_output <- compile_log
      plan <- set_product_attrs(plan, attrs)
      plan <- set_product_artifact(plan, "shared_library", shared_library_artifact(symbol, library_path))
      shared_library <- tryCatch(
        dyn.load(library_path),
        error = identity
      )
      if (inherits(shared_library, "error")) {
        diagnostic <- tccq_diagnostic(
          "backend.shared_library_load_failed",
          conditionMessage(shared_library),
          phase = "backend",
          path = sprintf("backend.%s.shared_library.load", backend@id),
          data = list(
            backend = backend@id,
            program = program@name,
            shared_library_path = library_path
          )
        )
        plan@diagnostics <- c(plan@diagnostics, list(diagnostic))
        return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
      }
      native_symbol <- tryCatch(
        getNativeSymbolInfo(wrapper_symbol, PACKAGE = shared_library),
        error = identity
      )
      if (inherits(native_symbol, "error")) {
        diagnostic <- tccq_diagnostic(
          "backend.shared_library_symbol_missing",
          conditionMessage(native_symbol),
          phase = "backend",
          path = sprintf("backend.%s.shared_library.symbol", backend@id),
          data = list(
            backend = backend@id,
            program = program@name,
            shared_library_path = library_path,
            symbol = wrapper_symbol
          )
        )
        plan@diagnostics <- c(plan@diagnostics, list(diagnostic))
        return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
      }
      callable <- function(...) {
        arguments <- list(...)
        if (length(arguments) != length(formals)) {
          tccq_abort(
            "runtime.invalid_argument_count",
            "Callable received the wrong number of arguments.",
            phase = "runtime",
            path = "callable.arguments",
            data = list(expected = length(formals), actual = length(arguments))
          )
        }
        value <- do.call(.Call, c(list(native_symbol), arguments))
        if (inherits(value, TCCQ_RUNTIME_STATUS_CLASS)) {
          status <- as.integer(value[[1L]])
          diagnostics <- interface@error_channel@diagnostics
          if (status < 1L || status > length(diagnostics)) {
            tccq_abort(
              "runtime.unknown_generated_status",
              "Generated code returned an unknown runtime status.",
              phase = "runtime",
              path = "callable.status",
              data = list(status = status)
            )
          }
          tccq_abort_diagnostic(diagnostics[[status]])
        }
        value
      }
      attrs$shared_library <- shared_library
      attrs$native_symbol <- native_symbol
      attrs$callable <- callable
      plan <- set_product_attrs(plan, attrs)
      plan <- set_product_artifact(plan, "native_callable", native_callable_artifact(symbol, callable))
      tccq_result(success = TRUE, value = plan, diagnostics = list())
    }

    result <- result_value()
    formals <- formal_values()
    lowered_diagnostic <- validate_lowered_program(result, formals)
    if (!is.null(lowered_diagnostic)) {
      plan <- diagnostic_plan(list(lowered_diagnostic))
      return(tccq_result(success = FALSE, value = plan, diagnostics = list(lowered_diagnostic)))
    }
    structured_body <- if (
      !is.null(program@schedule) &&
        S7::S7_inherits(program@schedule@body, TccqValueBlock)
    ) {
      program@schedule@body
    } else {
      NULL
    }
    if (is.null(structured_body)) {
      loop_nest_result <- tccq_program_loop_nests(program)
      if (!loop_nest_result@success) {
        plan <- diagnostic_plan(loop_nest_result@diagnostics)
        return(tccq_result(
          success = FALSE,
          value = plan,
          diagnostics = loop_nest_result@diagnostics
        ))
      }
      nests <- loop_nest_result@value
      nest <- nests[[length(nests)]]
    } else {
      nests <- list()
      nest <- NULL
    }

    symbol <- source_symbol()
    interface_result <- tryCatch(
      backend_function_interface(
        symbol,
        nests,
        result,
        formals,
        body = structured_body
      ),
      tccq_error = identity
    )
    if (inherits(interface_result, "tccq_error")) {
      diagnostic <- tccq_condition_diagnostic(interface_result)
      plan <- diagnostic_plan(list(diagnostic))
      return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
    }
    function_interface <- interface_result
    source_result <- tryCatch(
      switch(
        source_language,
        c = emit_c_source(
          function_interface,
          nests,
          result,
          formals,
          body = structured_body
        ),
        fortran = emit_fortran_source(
          function_interface,
          nests,
          result,
          formals,
          body = structured_body
        ),
        tccq_abort(
          "backend.unsupported_source_language",
          "The source printer does not support this target language.",
          phase = "backend",
          path = sprintf("backend.%s.source_language", backend@id),
          data = list(backend = backend@id, source_language = source_language)
        )
      ),
      tccq_error = identity
    )
    if (inherits(source_result, "tccq_error")) {
      diagnostic <- tccq_condition_diagnostic(source_result)
      plan <- diagnostic_plan(list(diagnostic))
      return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
    }
    source <- source_result
    input_bridge_kind <- function(value) {
      if (identical(value@type@shape@rank, 0L)) "sexp_to_scalar" else "sexp_to_buffer"
    }
    output_bridge_kind <- function(value) {
      if (identical(value@type@shape@rank, 0L)) "scalar_to_sexp" else "buffer_to_sexp"
    }
    bridges <- c(
      Map(function(value, bridge_index) {
        tccq_bridge_plan(
          id = sprintf("bridge_input_%04d", bridge_index),
          kind = input_bridge_kind(value),
          from_space = "r",
          to_space = "host",
          from_type = value@type,
          to_type = value@type,
          effect = tccq_effect(reads = TRUE, allocates = TRUE)
        )
      }, formals, seq_along(formals)),
      list(tccq_bridge_plan(
        id = "bridge_output_0001",
        kind = output_bridge_kind(result),
        from_space = "host",
        to_space = "r",
        from_type = result@type,
        to_type = result@type,
        effect = tccq_effect(reads = TRUE, allocates = TRUE)
      ))
    )
    plan <- tccq_backend_plan(
      id = sprintf("%s.%s.plan", backend@id, context@mode),
      backend_id = backend@id,
      family = backend@family,
      mode = context@mode,
      target = source_language,
      capabilities = backend@capabilities,
      regions = program@regions,
      bridges = bridges,
      products = tccq_backend_products(
        function_interface = function_interface,
        body = if (is.null(structured_body)) nest@body else structured_body,
        loop_nest = nest,
        loop_nests = nests,
        storage_plan = program@storage_plan,
        artifacts = list(source = source_artifact(symbol, source)),
        attrs = list(source = source)
      ),
      attrs = list(
        driver = backend@driver,
        source_language = source_language,
        symbol = symbol
      )
    )

    if (isTRUE(execute_with_rtinycc) && identical(context@mode, "jit")) {
      return(compile_with_rtinycc(plan, function_interface, result, formals))
    }
    if (identical(context@mode, "shared_library")) {
      return(compile_shared_library(plan, source, function_interface, result, formals))
    }
    tccq_result(success = TRUE, value = plan, diagnostics = list())
  }
}
