TCCQ_BACKEND_MODES <- c("source", "jit", "shared_library", "object_mode")
TCCQ_BACKEND_FAMILIES <- c("c", "fortran", "graph", "object", "device", "analysis")
TCCQ_BACKEND_CAPABILITIES <- c(
  "source",
  "jit",
  "shared_library",
  "object_mode",
  "r_api",
  "native",
  "openmp",
  "blas",
  "lapack",
  "host_memory",
  "device_memory",
  "stablehlo",
  "xla",
  "pjrt",
  "graph_trace",
  "autodiff",
  "shape_polymorphic",
  "raw_bytes",
  "buffer_bridge",
  "kernel",
  "parallel",
  "device"
)
TCCQ_RUNTIME_MODES <- c("release", "checked", "trace", "debug")
TCCQ_BRIDGE_KINDS <- c(
  "sexp_to_scalar",
  "scalar_to_sexp",
  "sexp_to_buffer",
  "buffer_to_sexp",
  "host_to_device",
  "device_to_host",
  "layout_convert",
  "tile_materialize",
  "boundary"
)
TCCQ_SAFEPOINT_KINDS <- c(
  "region_entry",
  "region_exit",
  "loop_backedge",
  "tile_boundary",
  "reduction_chunk",
  "boundary_call",
  "debug_break"
)
TCCQ_DEBUG_SITE_KINDS <- c("value", "region", "bridge", "safepoint", "control", "call")
TCCQ_BACKEND_FUNCTION_KINDS <- c("scalar", "map", "reduction")
TCCQ_BACKEND_FUNCTION_ABIS <- c("c", "fortran_bind_c")
TCCQ_BACKEND_RESULT_PLACEMENTS <- c("return", "output_argument")
TCCQ_BACKEND_ARTIFACT_KINDS <- c("source", "shared_library", "jit_callable", "native_callable")

#' Runtime instrumentation policy
#'
#' @param mode Runtime mode.
#' @param allow_interrupts Whether generated execution may observe interrupts.
#' @param check_interval Polling interval for generated loops or chunks.
#' @param emit_debug_sites Whether backend plans should preserve debug sites.
#' @param attrs Structured runtime-policy attributes.
#' @export
TccqRuntimePolicy <- S7::new_class(
  "TccqRuntimePolicy",
  package = "tccquickr",
  properties = list(
    mode = S7::class_character,
    allow_interrupts = S7::class_logical,
    check_interval = S7::class_integer,
    emit_debug_sites = S7::class_logical,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@mode) != 1L || is.na(self@mode) || !self@mode %in% TCCQ_RUNTIME_MODES) {
      problems <- c(problems, "@mode must be one supported runtime mode")
    }
    if (length(self@allow_interrupts) != 1L || is.na(self@allow_interrupts)) {
      problems <- c(problems, "@allow_interrupts must be a single TRUE/FALSE value")
    }
    if (
      length(self@check_interval) != 1L ||
        is.na(self@check_interval) ||
        self@check_interval <= 0L
    ) {
      problems <- c(problems, "@check_interval must be one positive integer")
    }
    if (length(self@emit_debug_sites) != 1L || is.na(self@emit_debug_sites)) {
      problems <- c(problems, "@emit_debug_sites must be a single TRUE/FALSE value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Source span for IR/debug metadata
#'
#' @param file Source file, or empty string if unknown.
#' @param line Starting line, or `NA_integer_` if unknown.
#' @param column Starting column, or `NA_integer_` if unknown.
#' @param end_line Ending line, or `NA_integer_` if unknown.
#' @param end_column Ending column, or `NA_integer_` if unknown.
#' @param label Human-readable source label.
#' @export
TccqSourceSpan <- S7::new_class(
  "TccqSourceSpan",
  package = "tccquickr",
  properties = list(
    file = S7::class_character,
    line = S7::class_integer,
    column = S7::class_integer,
    end_line = S7::class_integer,
    end_column = S7::class_integer,
    label = S7::class_character
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@file) != 1L || is.na(self@file)) {
      problems <- c(problems, "@file must be a single string")
    }
    if (length(self@label) != 1L || is.na(self@label)) {
      problems <- c(problems, "@label must be a single string")
    }
    line_values <- list(
      line = self@line,
      column = self@column,
      end_line = self@end_line,
      end_column = self@end_column
    )
    for (field_name in names(line_values)) {
      field_value <- line_values[[field_name]]
      if (length(field_value) != 1L || (!is.na(field_value) && field_value <= 0L)) {
        problems <- c(
          problems,
          sprintf("@%s must be one positive integer or NA", field_name)
        )
      }
    }
    if (!is.na(self@line) && !is.na(self@end_line) && self@end_line < self@line) {
      problems <- c(problems, "@end_line must not precede @line")
    }
    if (
      !is.na(self@line) &&
        !is.na(self@end_line) &&
        identical(self@line, self@end_line) &&
        !is.na(self@column) &&
        !is.na(self@end_column) &&
        self@end_column < self@column
    ) {
      problems <- c(problems, "@end_column must not precede @column on the same line")
    }
    if (length(problems) > 0L) problems
  }
)

#' Debug site in an IR or backend plan
#'
#' @param id Stable debug-site id.
#' @param kind Debug-site kind.
#' @param source Optional source span.
#' @param attrs Structured debug-site attributes.
#' @export
TccqDebugSite <- S7::new_class(
  "TccqDebugSite",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    kind = S7::class_character,
    source = S7::new_union(NULL, TccqSourceSpan),
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (
      length(self@kind) != 1L ||
        is.na(self@kind) ||
        !self@kind %in% TCCQ_DEBUG_SITE_KINDS
    ) {
      problems <- c(problems, "@kind must be one supported debug-site kind")
    }
    if (length(problems) > 0L) problems
  }
)

#' Generated-program safepoint
#'
#' Safepoints are backend-plan markers where long-running generated code may
#' check interrupts, update trace state, or stop at a debugger boundary. A
#' safepoint can require the R C API only in host/R-API-capable regions.
#'
#' @param id Stable safepoint id.
#' @param kind Safepoint kind.
#' @param region_id Region that owns the safepoint.
#' @param requires_rapi Whether the safepoint needs the R C API.
#' @param attrs Structured safepoint attributes.
#' @export
TccqSafepoint <- S7::new_class(
  "TccqSafepoint",
  package = "tccquickr",
  properties = list(
    id = S7::class_character,
    kind = S7::class_character,
    region_id = S7::class_character,
    requires_rapi = S7::class_logical,
    attrs = S7::class_list
  ),
  validator = function(self) {
    problems <- character()
    if (length(self@id) != 1L || is.na(self@id) || !nzchar(self@id)) {
      problems <- c(problems, "@id must be a single non-empty string")
    }
    if (
      length(self@kind) != 1L ||
        is.na(self@kind) ||
        !self@kind %in% TCCQ_SAFEPOINT_KINDS
    ) {
      problems <- c(problems, "@kind must be one supported safepoint kind")
    }
    if (length(self@region_id) != 1L || is.na(self@region_id) || !nzchar(self@region_id)) {
      problems <- c(problems, "@region_id must be a single non-empty string")
    }
    if (length(self@requires_rapi) != 1L || is.na(self@requires_rapi)) {
      problems <- c(problems, "@requires_rapi must be a single TRUE/FALSE value")
    }
    if (length(problems) > 0L) problems
  }
)

#' Backend planning context
#'
#' @param mode Requested backend mode.
#' @param target Requested target language or runtime, or `any`.
#' @param allow_boundary Whether explicit boundary/object-mode plans are allowed.
#' @param required_capabilities Capabilities that candidate backends must expose.
#' @param runtime Runtime instrumentation policy.
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
    runtime = TccqRuntimePolicy,
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

#' Backend function interface
#'
#' A backend function interface is the source-level callable boundary consumed
#' by C, Rtinycc, Fortran, and later source printers. It records the generated
#' symbol, ABI, formal parameter mapping, result placement, length/index
#' variables, and reduction accumulator name before any concrete syntax is
#' emitted.
#'
#' @param symbol Generated function symbol.
#' @param source_language Source language consumed by the printer.
#' @param kind Function shape: scalar, map, or reduction.
#' @param abi Callable ABI.
#' @param parameter_names Generated parameter names.
#' @param parameter_value_ids Lowered value ids corresponding to parameters.
#' @param result_value_id Lowered result value id.
#' @param result_placement Whether the result is returned or passed by output argument.
#' @param result_name Generated result/output variable name, or empty string.
#' @param needs_length Whether the callable receives a length parameter.
#' @param length_name Generated length parameter name, or empty string.
#' @param index_name Generated loop index name, or empty string.
#' @param accumulator_name Generated reduction accumulator name, or empty string.
#' @param attrs Structured interface metadata.
#' @export
TccqBackendFunctionInterface <- S7::new_class(
  "TccqBackendFunctionInterface",
  package = "tccquickr",
  properties = list(
    symbol = S7::class_character,
    source_language = S7::class_character,
    kind = S7::class_character,
    abi = S7::class_character,
    parameter_names = S7::class_character,
    parameter_value_ids = S7::class_character,
    result_value_id = S7::class_character,
    result_placement = S7::class_character,
    result_name = S7::class_character,
    needs_length = S7::class_logical,
    length_name = S7::class_character,
    index_name = S7::class_character,
    accumulator_name = S7::class_character,
    attrs = S7::class_list
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
      length_name = self@length_name,
      index_name = self@index_name,
      accumulator_name = self@accumulator_name
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
    if (length(self@needs_length) != 1L || is.na(self@needs_length)) {
      problems <- c(problems, "@needs_length must be a single TRUE/FALSE value")
    }
    if (length(self@parameter_names) != length(self@parameter_value_ids)) {
      problems <- c(problems, "@parameter_names and @parameter_value_ids must have the same length")
    }
    if (anyNA(self@parameter_names) || any(!nzchar(self@parameter_names))) {
      problems <- c(problems, "@parameter_names must contain non-empty strings")
    }
    if (anyNA(self@parameter_value_ids) || any(!nzchar(self@parameter_value_ids))) {
      problems <- c(problems, "@parameter_value_ids must contain non-empty value ids")
    }
    if (isTRUE(self@needs_length) && !nzchar(self@length_name)) {
      problems <- c(problems, "@length_name must be non-empty when @needs_length is TRUE")
    }
    if (
      length(self@kind) == 1L &&
        !is.na(self@kind) &&
        self@kind %in% c("map", "reduction") &&
        !nzchar(self@index_name)
    ) {
      problems <- c(problems, "map and reduction interfaces must have an index name")
    }
    if (identical(self@kind, "reduction") && !nzchar(self@accumulator_name)) {
      problems <- c(problems, "reduction interfaces must have an accumulator name")
    }
    if (identical(self@kind, "scalar") && isTRUE(self@needs_length)) {
      problems <- c(problems, "scalar interfaces must not require a length parameter")
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
#' @param safepoints Generated-program safepoints.
#' @param debug_sites Debug metadata retained for generated code.
#' @param diagnostics Diagnostics attached to this plan.
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
    safepoints = S7::class_list,
    debug_sites = S7::class_list,
    diagnostics = S7::class_list,
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
    safepoints_are_tccq_safepoints <- vapply(
      self@safepoints,
      S7::S7_inherits,
      logical(1),
      class = TccqSafepoint
    )
    debug_sites_are_tccq_debug_sites <- vapply(
      self@debug_sites,
      S7::S7_inherits,
      logical(1),
      class = TccqDebugSite
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
    if (!all(safepoints_are_tccq_safepoints)) {
      problems <- c(problems, "@safepoints must contain only <TccqSafepoint> values")
    }
    if (!all(debug_sites_are_tccq_debug_sites)) {
      problems <- c(problems, "@debug_sites must contain only <TccqDebugSite> values")
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
#' Fortran, graph/device, and object-mode paths must report their constraints
#' through the same contract.
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
      attrs = list(
        driver = backend@driver,
        runtime = context@runtime
      )
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

#' Construct a runtime instrumentation policy
#'
#' @param mode Runtime mode.
#' @param allow_interrupts Whether generated execution may observe interrupts.
#' @param check_interval Polling interval for generated loops or chunks.
#' @param emit_debug_sites Whether backend plans should preserve debug sites.
#' @param attrs Structured runtime-policy attributes.
#' @export
tccq_runtime_policy <- function(
  mode = "release",
  allow_interrupts = TRUE,
  check_interval = 4096L,
  emit_debug_sites = FALSE,
  attrs = list()
) {
  .tccq_check_character_scalar(mode, "mode")
  if (!mode %in% TCCQ_RUNTIME_MODES) {
    tccq_abort(
      "schema.invalid_runtime_mode",
      "`mode` is not a supported runtime mode.",
      phase = "schema",
      path = "runtime.mode",
      data = list(mode = mode, supported = TCCQ_RUNTIME_MODES)
    )
  }
  .tccq_check_logical_scalar(allow_interrupts, "allow_interrupts")
  check_interval <- .tccq_check_positive_integer(check_interval, "check_interval")
  .tccq_check_logical_scalar(emit_debug_sites, "emit_debug_sites")
  .tccq_check_list(attrs, "attrs")

  TccqRuntimePolicy(
    mode = mode,
    allow_interrupts = allow_interrupts,
    check_interval = check_interval,
    emit_debug_sites = emit_debug_sites,
    attrs = attrs
  )
}

#' Construct a source span
#'
#' @param file Source file, or empty string if unknown.
#' @param line Starting line, or `NA_integer_` if unknown.
#' @param column Starting column, or `NA_integer_` if unknown.
#' @param end_line Ending line, or `NA_integer_` if unknown.
#' @param end_column Ending column, or `NA_integer_` if unknown.
#' @param label Human-readable source label.
#' @export
tccq_source_span <- function(
  file = "",
  line = NA_integer_,
  column = NA_integer_,
  end_line = NA_integer_,
  end_column = NA_integer_,
  label = ""
) {
  .tccq_check_character_or_empty(file, "file")
  line <- .tccq_check_optional_positive_integer(line, "line")
  column <- .tccq_check_optional_positive_integer(column, "column")
  end_line <- .tccq_check_optional_positive_integer(end_line, "end_line")
  end_column <- .tccq_check_optional_positive_integer(end_column, "end_column")
  .tccq_check_character_or_empty(label, "label")

  TccqSourceSpan(
    file = file,
    line = line,
    column = column,
    end_line = end_line,
    end_column = end_column,
    label = label
  )
}

#' Construct a debug site
#'
#' @param id Stable debug-site id.
#' @param kind Debug-site kind.
#' @param source Optional source span.
#' @param attrs Structured debug-site attributes.
#' @export
tccq_debug_site <- function(id, kind, source = NULL, attrs = list()) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_DEBUG_SITE_KINDS) {
    tccq_abort(
      "schema.invalid_debug_site_kind",
      "`kind` is not a supported debug-site kind.",
      phase = "schema",
      path = "debug_site.kind",
      data = list(kind = kind, supported = TCCQ_DEBUG_SITE_KINDS)
    )
  }
  .tccq_check_optional_s7(source, TccqSourceSpan, "TccqSourceSpan", "source")
  .tccq_check_list(attrs, "attrs")

  TccqDebugSite(id = id, kind = kind, source = source, attrs = attrs)
}

#' Construct a safepoint
#'
#' @param id Stable safepoint id.
#' @param kind Safepoint kind.
#' @param region_id Region that owns the safepoint.
#' @param requires_rapi Whether the safepoint needs the R C API.
#' @param attrs Structured safepoint attributes.
#' @export
tccq_safepoint <- function(
  id,
  kind,
  region_id,
  requires_rapi = FALSE,
  attrs = list()
) {
  .tccq_check_character_scalar(id, "id")
  .tccq_check_character_scalar(kind, "kind")
  if (!kind %in% TCCQ_SAFEPOINT_KINDS) {
    tccq_abort(
      "schema.invalid_safepoint_kind",
      "`kind` is not a supported safepoint kind.",
      phase = "schema",
      path = "safepoint.kind",
      data = list(kind = kind, supported = TCCQ_SAFEPOINT_KINDS)
    )
  }
  .tccq_check_character_scalar(region_id, "region_id")
  .tccq_check_logical_scalar(requires_rapi, "requires_rapi")
  .tccq_check_list(attrs, "attrs")

  TccqSafepoint(
    id = id,
    kind = kind,
    region_id = region_id,
    requires_rapi = requires_rapi,
    attrs = attrs
  )
}

#' Construct a backend planning context
#'
#' @param mode Requested backend mode.
#' @param target Requested target language or runtime, or `any`.
#' @param allow_boundary Whether explicit boundary/object-mode plans are allowed.
#' @param required_capabilities Capabilities that candidate backends must expose.
#' @param runtime Runtime instrumentation policy.
#' @param attrs Structured backend-context attributes.
#' @export
tccq_backend_context <- function(
  mode = "source",
  target = "any",
  allow_boundary = FALSE,
  required_capabilities = character(),
  runtime = tccq_runtime_policy(),
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
  .tccq_check_s7(runtime, TccqRuntimePolicy, "TccqRuntimePolicy", "runtime")
  .tccq_check_list(attrs, "attrs")

  TccqBackendContext(
    mode = mode,
    target = target,
    allow_boundary = allow_boundary,
    required_capabilities = required_capabilities,
    runtime = runtime,
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

#' Construct a backend function interface
#'
#' @param symbol Generated function symbol.
#' @param source_language Source language consumed by the printer.
#' @param kind Function shape: scalar, map, or reduction.
#' @param abi Callable ABI.
#' @param parameter_names Generated parameter names.
#' @param parameter_value_ids Lowered value ids corresponding to parameters.
#' @param result_value_id Lowered result value id.
#' @param result_placement Whether the result is returned or passed by output argument.
#' @param result_name Generated result/output variable name, or empty string.
#' @param needs_length Whether the callable receives a length parameter.
#' @param length_name Generated length parameter name, or empty string.
#' @param index_name Generated loop index name, or empty string.
#' @param accumulator_name Generated reduction accumulator name, or empty string.
#' @param attrs Structured interface metadata.
#' @export
tccq_backend_function_interface <- function(
  symbol,
  source_language,
  kind,
  abi = "",
  parameter_names = character(),
  parameter_value_ids = character(),
  result_value_id,
  result_placement = "return",
  result_name = "",
  needs_length = FALSE,
  length_name = "",
  index_name = "",
  accumulator_name = "",
  attrs = list()
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
  if (!is.character(parameter_names) || anyNA(parameter_names) || any(!nzchar(parameter_names))) {
    tccq_abort(
      "schema.invalid_backend_function_parameters",
      "`parameter_names` must contain non-empty strings.",
      phase = "schema",
      path = "backend_function.parameter_names"
    )
  }
  if (
    !is.character(parameter_value_ids) ||
      anyNA(parameter_value_ids) ||
      any(!nzchar(parameter_value_ids))
  ) {
    tccq_abort(
      "schema.invalid_backend_function_parameters",
      "`parameter_value_ids` must contain non-empty strings.",
      phase = "schema",
      path = "backend_function.parameter_value_ids"
    )
  }
  if (length(parameter_names) != length(parameter_value_ids)) {
    tccq_abort(
      "schema.invalid_backend_function_parameters",
      "`parameter_names` and `parameter_value_ids` must have the same length.",
      phase = "schema",
      path = "backend_function.parameters"
    )
  }
  .tccq_check_character_scalar(result_value_id, "result_value_id")
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
  .tccq_check_logical_scalar(needs_length, "needs_length")
  .tccq_check_character_or_empty(length_name, "length_name")
  .tccq_check_character_or_empty(index_name, "index_name")
  .tccq_check_character_or_empty(accumulator_name, "accumulator_name")
  .tccq_check_list(attrs, "attrs")

  TccqBackendFunctionInterface(
    symbol = symbol,
    source_language = source_language,
    kind = kind,
    abi = abi,
    parameter_names = parameter_names,
    parameter_value_ids = parameter_value_ids,
    result_value_id = result_value_id,
    result_placement = result_placement,
    result_name = result_name,
    needs_length = needs_length,
    length_name = length_name,
    index_name = index_name,
    accumulator_name = accumulator_name,
    attrs = attrs
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
#' @param safepoints Generated-program safepoints.
#' @param debug_sites Debug metadata retained for generated code.
#' @param diagnostics Diagnostics attached to this plan.
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
  safepoints = list(),
  debug_sites = list(),
  diagnostics = list(),
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
  .tccq_check_list_of(safepoints, TccqSafepoint, "TccqSafepoint", "safepoints")
  .tccq_check_list_of(debug_sites, TccqDebugSite, "TccqDebugSite", "debug_sites")
  .tccq_check_list_of(diagnostics, TccqDiagnostic, "TccqDiagnostic", "diagnostics")
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
    safepoints = safepoints,
    debug_sites = debug_sites,
    diagnostics = diagnostics,
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
        attrs = list(
          driver = backend@driver,
          runtime = context@runtime
        )
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
    region_kinds = c("host", "kernel", "parallel"),
    memory_spaces = c("r", "host"),
    capabilities = c(
      "source",
      "shared_library",
      "native",
      "r_api",
      "openmp",
      "blas",
      "lapack",
      "host_memory",
      "buffer_bridge",
      "kernel",
      "parallel"
    ),
    uses_rapi = TRUE,
    attrs = list(role = "quickr_fortran"),
    prepare = new_lowered_backend_prepare("fortran")
  )
}

#' Anvil-style graph backend descriptor
#'
#' This descriptor captures graph tracing, primitive legality, StableHLO/XLA
#' lowering, PJRT execution, and host/device movement without making those
#' choices the core IR.
#'
#' @export
tccq_anvil_graph_backend <- function() {
  tccq_backend_spec(
    id = "anvil_graph",
    family = "graph",
    target = "stablehlo",
    driver = "anvil-xla-pjrt",
    modes = c("source", "jit"),
    region_kinds = c("kernel", "parallel", "device"),
    memory_spaces = c("host", "device"),
    capabilities = c(
      "source",
      "jit",
      "stablehlo",
      "xla",
      "pjrt",
      "graph_trace",
      "autodiff",
      "shape_polymorphic",
      "host_memory",
      "device_memory",
      "buffer_bridge",
      "kernel",
      "parallel",
      "device"
    ),
    uses_rapi = FALSE,
    attrs = list(role = "anvil_graph")
  )
}

#' R object-call backend descriptor
#'
#' This backend describes ordinary R call evaluation as a first-class backend
#' family. It is not the meaning of opaque calls; it is one implementation
#' family that may account for them when a host/R boundary is requested.
#'
#' @export
tccq_r_object_backend <- function() {
  tccq_backend_spec(
    id = "r_object",
    family = "object",
    target = "r",
    driver = "R",
    modes = "object_mode",
    region_kinds = "host",
    memory_spaces = "r",
    capabilities = c("object_mode", "r_api", "host_memory"),
    uses_rapi = TRUE,
    attrs = list(role = "r_call_evaluation")
  )
}

#' Core backend suite
#' Core backend descriptors
#'
#' @param include_rtinycc Whether to include the `Rtinycc` TinyCC descriptor.
#' @export
tccq_core_backends <- function(include_rtinycc = TRUE) {
  backends <- list(
    c = tccq_c_backend(),
    fortran = tccq_fortran_backend(),
    graph = tccq_anvil_graph_backend(),
    object = tccq_r_object_backend()
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
  all_backend_plans_succeeded <- all(vapply(results, function(result) result@success, logical(1)))
  tccq_result(
    success = all_backend_plans_succeeded,
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
        attrs = list(driver = backend@driver, runtime = context@runtime)
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
      all_values <- c(list(result), formals)
      unsupported_bases <- setdiff(
        unique(vapply(all_values, function(value) value@type@base, character(1))),
        "double"
      )
      if (length(unsupported_bases) > 0L) {
        return(tccq_diagnostic(
          "backend.unsupported_type",
          "This initial source printer supports only double values.",
          phase = "backend",
          path = sprintf("backend.%s.type", backend@id),
          data = list(base = unsupported_bases)
        ))
      }
      unsupported_ranks <- unique(vapply(
        all_values,
        function(value) value@type@shape@rank,
        integer(1)
      ))
      unsupported_ranks <- setdiff(unsupported_ranks, c(0L, 1L))
      if (length(unsupported_ranks) > 0L) {
        return(tccq_diagnostic(
          "backend.unsupported_rank",
          "This initial source printer supports only scalar and rank-one values.",
          phase = "backend",
          path = sprintf("backend.%s.rank", backend@id),
          data = list(rank = unsupported_ranks)
        ))
      }
      NULL
    }

    expression_operation <- function(expression) {
      operation <- expression@attrs$operation
      if (S7::S7_inherits(operation, TccqLoweredOperation)) operation else NULL
    }

    expression_is_reduction <- function(expression) {
      operation <- expression_operation(expression)
      identical(expression@kind, "operation") &&
        !is.null(operation) &&
        identical(operation@family, "reduction") &&
        length(expression@inputs) == 1L
    }

    backend_function_interface <- function(symbol, source_expression, result, formals) {
      parameter_names <- c_identifier("input", seq_along(formals))
      parameter_value_ids <- vapply(formals, function(value) value@id, character(1))
      kind <- if (expression_is_reduction(source_expression)) {
        "reduction"
      } else if (result@type@shape@rank == 1L) {
        "map"
      } else {
        "scalar"
      }
      needs_length <- kind %in% c("map", "reduction")
      result_placement <- if (identical(source_language, "fortran") && identical(kind, "map")) {
        "output_argument"
      } else {
        "return"
      }
      result_name <- if (identical(source_language, "fortran") || identical(kind, "map")) {
        "output"
      } else {
        ""
      }
      tccq_backend_function_interface(
        symbol = symbol,
        source_language = source_language,
        kind = kind,
        abi = if (identical(source_language, "fortran")) "fortran_bind_c" else "c",
        parameter_names = parameter_names,
        parameter_value_ids = parameter_value_ids,
        result_value_id = result@id,
        result_placement = result_placement,
        result_name = result_name,
        needs_length = needs_length,
        length_name = if (needs_length) "length_0001" else "",
        index_name = if (needs_length) "index_0001" else "",
        accumulator_name = if (identical(kind, "reduction")) "accumulator_0001" else "",
        attrs = list(result_type = result@type)
      )
    }

    expression_text <- function(expression, parameter_by_value_id, index_name, language) {
      if (identical(expression@kind, "reference")) {
        parameter_name <- parameter_by_value_id[[expression@value_id]]
        if (expression@type@shape@rank == 0L) {
          return(parameter_name)
        }
        if (identical(language, "fortran")) {
          return(sprintf("%s(%s)", parameter_name, index_name))
        }
        return(sprintf("%s[%s]", parameter_name, index_name))
      }
      if (identical(expression@kind, "literal")) {
        literal <- expression@literal
        if (identical(expression@type@base, "integer")) {
          return(sprintf("%d", as.integer(literal@value)))
        }
        return(formatC(as.numeric(literal@value), digits = 17L, format = "fg"))
      }

      child_expression_text <- function(child_expression) {
        expression_text(child_expression, parameter_by_value_id, index_name, language)
      }
      inputs <- vapply(expression@inputs, child_expression_text, character(1))
      render_result <- tccq_op_render(
        expression@resolved_op@implementation,
        inputs,
        tccq_op_render_context(
          language = language,
          backend_id = backend@id,
          attrs = list(value_id = expression@value_id, op = expression@op)
        )
      )
      if (!render_result@success) {
        tccq_abort_diagnostic(render_result@diagnostics[[1L]])
      }
      render_result@value
    }

    literal_text <- function(literal, language) {
      if (identical(literal@type@base, "integer")) {
        return(sprintf("%d", as.integer(literal@value)))
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

    reduction_identity_text <- function(reduction_spec, result_type, language) {
      identity_result <- tccq_reduction_identity(reduction_spec, result_type)
      if (!identity_result@success) {
        tccq_abort_diagnostic(identity_result@diagnostics[[1L]])
      }
      literal_text(identity_result@value, language)
    }

    reduction_combine_text <- function(reduction_spec, accumulator, value, language) {
      combine_result <- tccq_reduction_combine(
        reduction_spec,
        accumulator,
        value,
        tccq_op_render_context(
          language = language,
          backend_id = backend@id,
          attrs = list(reducer = reduction_spec@name)
        )
      )
      if (!combine_result@success) {
        tccq_abort_diagnostic(combine_result@diagnostics[[1L]])
      }
      combine_result@value
    }

    emit_c_source <- function(interface, source_expression, result, formals) {
      symbol <- interface@symbol
      parameter_names <- interface@parameter_names
      parameter_by_value_id <- as.list(stats::setNames(
        parameter_names,
        interface@parameter_value_ids
      ))
      scalar_parameter <- function(parameter_name) {
        sprintf("double %s", parameter_name)
      }
      vector_parameter <- function(parameter_name) {
        sprintf("const double *%s", parameter_name)
      }
      parameter_declarations <- Map(function(value, parameter_name) {
        if (value@type@shape@rank == 0L) {
          scalar_parameter(parameter_name)
        } else {
          vector_parameter(parameter_name)
        }
      }, formals, parameter_names)
      if (identical(interface@kind, "reduction")) {
        lowered_operation <- expression_operation(source_expression)
        reduction_spec <- if (!is.null(lowered_operation)) lowered_operation@reduction else NULL
        if (!S7::S7_inherits(reduction_spec, TccqReductionSpec)) {
          tccq_abort(
            "backend.invalid_reduction",
            "Reduction expressions must carry a <TccqReductionSpec>.",
            phase = "backend",
            path = sprintf("backend.%s.reducer", backend@id),
            data = list(backend = backend@id)
          )
        }
        length_name <- interface@length_name
        index_name <- interface@index_name
        accumulator_name <- interface@accumulator_name
        identity <- reduction_identity_text(reduction_spec, result@type, "c")
        reduced_expression <- expression_text(
          source_expression@inputs[[1L]],
          parameter_by_value_id,
          index_name,
          "c"
        )
        combined_expression <- reduction_combine_text(
          reduction_spec,
          accumulator_name,
          reduced_expression,
          "c"
        )
        signature_declarations <- c(unlist(parameter_declarations), sprintf("int %s", length_name))
        return(paste(c(
          "#include <math.h>",
          "#include <stddef.h>",
          "",
          sprintf(
            "double %s(%s) {",
            symbol,
            paste(signature_declarations, collapse = ", ")
          ),
          sprintf("  double %s = %s;", accumulator_name, identity),
          sprintf("  for (int %s = 0; %s < %s; ++%s) {", index_name, index_name, length_name, index_name),
          sprintf("    %s = %s;", accumulator_name, combined_expression),
          "  }",
          sprintf("  return %s;", accumulator_name),
          "}"
        ), collapse = "\n"))
      }

      if (identical(interface@kind, "scalar")) {
        expression <- expression_text(source_expression, parameter_by_value_id, NULL, "c")
        return(paste(c(
          "#include <math.h>",
          "",
          sprintf("double %s(%s) {", symbol, paste(unlist(parameter_declarations), collapse = ", ")),
          sprintf("  return %s;", expression),
          "}"
        ), collapse = "\n"))
      }

      length_name <- interface@length_name
      index_name <- interface@index_name
      result_name <- interface@result_name
      expression <- expression_text(source_expression, parameter_by_value_id, index_name, "c")
      signature_declarations <- c(unlist(parameter_declarations), sprintf("int %s", length_name))
      paste(c(
        "#include <math.h>",
        "#include <stddef.h>",
        "#include <stdlib.h>",
        "",
        sprintf(
          "double *%s(%s) {",
          symbol,
          paste(signature_declarations, collapse = ", ")
        ),
        sprintf("  if (%s < 0) {", length_name),
        "    return NULL;",
        "  }",
        sprintf("  double *%s = (double *)malloc(sizeof(double) * (size_t)%s);", result_name, length_name),
        sprintf("  if (%s == NULL) {", result_name),
        "    return NULL;",
        "  }",
        sprintf("  for (int %s = 0; %s < %s; ++%s) {", index_name, index_name, length_name, index_name),
        sprintf("    %s[%s] = %s;", result_name, index_name, expression),
        "  }",
        sprintf("  return %s;", result_name),
        "}"
      ), collapse = "\n")
    }

    emit_fortran_source <- function(interface, source_expression, result, formals) {
      symbol <- interface@symbol
      parameter_names <- interface@parameter_names
      parameter_by_value_id <- as.list(stats::setNames(
        parameter_names,
        interface@parameter_value_ids
      ))

      if (identical(interface@kind, "reduction")) {
        lowered_operation <- expression_operation(source_expression)
        reduction_spec <- if (!is.null(lowered_operation)) lowered_operation@reduction else NULL
        if (!S7::S7_inherits(reduction_spec, TccqReductionSpec)) {
          tccq_abort(
            "backend.invalid_reduction",
            "Reduction expressions must carry a <TccqReductionSpec>.",
            phase = "backend",
            path = sprintf("backend.%s.reducer", backend@id),
            data = list(backend = backend@id)
          )
        }
        length_name <- interface@length_name
        index_name <- interface@index_name
        result_name <- interface@result_name
        declarations <- Map(function(value, parameter_name) {
          if (value@type@shape@rank == 0L) {
            sprintf("  real(c_double), value :: %s", parameter_name)
          } else {
            sprintf("  real(c_double), intent(in) :: %s(%s)", parameter_name, length_name)
          }
        }, formals, parameter_names)
        identity <- reduction_identity_text(reduction_spec, result@type, "fortran")
        reduced_expression <- expression_text(
          source_expression@inputs[[1L]],
          parameter_by_value_id,
          index_name,
          "fortran"
        )
        combined_expression <- reduction_combine_text(
          reduction_spec,
          result_name,
          reduced_expression,
          "fortran"
        )
        return(paste(c(
          sprintf(
            "function %s(%s) bind(c, name = \"%s\") result(%s)",
            symbol,
            paste(c(parameter_names, length_name), collapse = ", "),
            symbol,
            result_name
          ),
          "  use iso_c_binding, only: c_double, c_int",
          "  implicit none",
          sprintf("  integer(c_int), value :: %s", length_name),
          unlist(declarations),
          sprintf("  real(c_double) :: %s", result_name),
          sprintf("  integer(c_int) :: %s", index_name),
          sprintf("  %s = %s", result_name, identity),
          sprintf("  do %s = 1, %s", index_name, length_name),
          sprintf("    %s = %s", result_name, combined_expression),
          "  end do",
          sprintf("end function %s", symbol)
        ), collapse = "\n"))
      }

      if (identical(interface@kind, "scalar")) {
        declarations <- Map(function(value, parameter_name) {
          sprintf("  real(c_double), value :: %s", parameter_name)
        }, formals, parameter_names)
        result_name <- interface@result_name
        expression <- expression_text(source_expression, parameter_by_value_id, NULL, "fortran")
        return(paste(c(
          sprintf(
            "function %s(%s) bind(c, name = \"%s\") result(%s)",
            symbol,
            paste(parameter_names, collapse = ", "),
            symbol,
            result_name
          ),
          "  use iso_c_binding, only: c_double",
          "  implicit none",
          unlist(declarations),
          sprintf("  real(c_double) :: %s", result_name),
          sprintf("  %s = %s", result_name, expression),
          sprintf("end function %s", symbol)
        ), collapse = "\n"))
      }

      length_name <- interface@length_name
      index_name <- interface@index_name
      result_name <- interface@result_name
      declarations <- Map(function(value, parameter_name) {
        if (value@type@shape@rank == 0L) {
          sprintf("  real(c_double), value :: %s", parameter_name)
        } else {
          sprintf("  real(c_double), intent(in) :: %s(%s)", parameter_name, length_name)
        }
      }, formals, parameter_names)
      expression <- expression_text(source_expression, parameter_by_value_id, index_name, "fortran")
      paste(c(
        sprintf(
          "subroutine %s(%s, %s, %s) bind(c, name = \"%s\")",
          symbol,
          paste(parameter_names, collapse = ", "),
          length_name,
          result_name,
          symbol
        ),
        "  use iso_c_binding, only: c_double, c_int",
        "  implicit none",
        sprintf("  integer(c_int), value :: %s", length_name),
        unlist(declarations),
        sprintf("  real(c_double), intent(out) :: %s(%s)", result_name, length_name),
        sprintf("  integer(c_int) :: %s", index_name),
        sprintf("  do %s = 1, %s", index_name, length_name),
        sprintf("    %s(%s) = %s", result_name, index_name, expression),
        "  end do",
        sprintf("end subroutine %s", symbol)
      ), collapse = "\n")
    }

    emit_call_wrapper_source <- function(interface, result, formals) {
      symbol <- interface@symbol
      wrapper_symbol <- paste0(symbol, "_call")
      parameter_names <- interface@parameter_names
      length_name <- interface@length_name
      result_name <- interface@result_name

      c_parameter_type <- function(value) {
        if (value@type@shape@rank == 0L) "double" else "const double *"
      }
      kernel_parameters <- unname(Map(function(value, parameter_name) {
        sprintf("%s %s", c_parameter_type(value), parameter_name)
      }, formals, parameter_names))
      if (isTRUE(interface@needs_length)) {
        kernel_parameters <- c(kernel_parameters, sprintf("int %s", length_name))
      }
      if (identical(interface@result_placement, "output_argument")) {
        kernel_parameters <- c(kernel_parameters, sprintf("double *%s", result_name))
        prototype <- sprintf("extern void %s(%s);", symbol, paste(kernel_parameters, collapse = ", "))
      } else if (result@type@shape@rank == 1L) {
        prototype <- sprintf("extern double *%s(%s);", symbol, paste(kernel_parameters, collapse = ", "))
      } else {
        prototype <- sprintf("extern double %s(%s);", symbol, paste(kernel_parameters, collapse = ", "))
      }

      wrapper_parameters <- unname(vapply(
        parameter_names,
        function(parameter_name) sprintf("SEXP %s_arg", parameter_name),
        character(1)
      ))
      lines <- c(
        "#include <R.h>",
        "#include <Rinternals.h>",
        "#include <R_ext/Utils.h>",
        "#include <limits.h>",
        "#include <stdlib.h>",
        "#include <string.h>",
        "",
        prototype,
        "",
        sprintf("SEXP %s(%s) {", wrapper_symbol, paste(wrapper_parameters, collapse = ", ")),
        "  int protect_count = 0;",
        "  R_xlen_t element_count = 1;"
      )

      vector_parameter_names <- parameter_names[vapply(
        formals,
        function(value) value@type@shape@rank == 1L,
        logical(1)
      )]
      if (length(vector_parameter_names) > 0L) {
        first_vector_name <- vector_parameter_names[[1L]]
        lines <- c(
          lines,
          sprintf(
            "  SEXP %s_real = PROTECT(Rf_coerceVector(%s_arg, REALSXP));",
            first_vector_name,
            first_vector_name
          ),
          "  ++protect_count;",
          sprintf("  element_count = XLENGTH(%s_real);", first_vector_name),
          "  if (element_count > INT_MAX) {",
          "    UNPROTECT(protect_count);",
          "    Rf_error(\"generated call exceeds the current int length ABI\");",
          "  }"
        )
      }

      parameter_setup <- unlist(Map(function(value, parameter_name) {
        if (value@type@shape@rank == 1L && parameter_name %in% vector_parameter_names[[1L]]) {
          return(c(sprintf("  const double *%s = REAL(%s_real);", parameter_name, parameter_name)))
        }
        if (value@type@shape@rank == 1L) {
          return(c(
            sprintf("  SEXP %s_real = PROTECT(Rf_coerceVector(%s_arg, REALSXP));", parameter_name, parameter_name),
            "  ++protect_count;",
            sprintf("  if (XLENGTH(%s_real) != element_count) {", parameter_name),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"vector arguments must have the same length\");",
            "  }",
            sprintf("  const double *%s = REAL(%s_real);", parameter_name, parameter_name)
          ))
        }
        c(
          sprintf("  SEXP %s_real = PROTECT(Rf_coerceVector(%s_arg, REALSXP));", parameter_name, parameter_name),
          "  ++protect_count;",
          sprintf("  if (XLENGTH(%s_real) < 1) {", parameter_name),
          "    UNPROTECT(protect_count);",
          "    Rf_error(\"scalar arguments must have length at least one\");",
          "  }",
          sprintf("  double %s = REAL(%s_real)[0];", parameter_name, parameter_name)
        )
      }, formals, parameter_names))
      lines <- c(lines, parameter_setup)

      argument_names <- parameter_names
      if (isTRUE(interface@needs_length)) {
        lines <- c(lines, sprintf("  int %s = (int)element_count;", length_name))
        argument_names <- c(argument_names, length_name)
      }

      if (result@type@shape@rank == 1L) {
        lines <- c(lines, "  SEXP output_sexp = PROTECT(Rf_allocVector(REALSXP, element_count));")
        lines <- c(lines, "  ++protect_count;")
        if (identical(interface@result_placement, "output_argument")) {
          call_arguments <- c(argument_names, "REAL(output_sexp)")
          lines <- c(lines, sprintf("  %s(%s);", symbol, paste(call_arguments, collapse = ", ")))
        } else {
          call_arguments <- argument_names
          lines <- c(
            lines,
            sprintf("  double *%s = %s(%s);", result_name, symbol, paste(call_arguments, collapse = ", ")),
            sprintf("  if (%s == NULL) {", result_name),
            "    UNPROTECT(protect_count);",
            "    Rf_error(\"generated kernel returned NULL\");",
            "  }",
            sprintf(
              "  memcpy(REAL(output_sexp), %s, sizeof(double) * (size_t)element_count);",
              result_name
            ),
            sprintf("  free(%s);", result_name)
          )
        }
        lines <- c(lines, "  UNPROTECT(protect_count);", "  return output_sexp;")
      } else {
        call_arguments <- argument_names
        lines <- c(
          lines,
          sprintf("  double output_value = %s(%s);", symbol, paste(call_arguments, collapse = ", ")),
          "  UNPROTECT(protect_count);",
          "  return Rf_ScalarReal(output_value);"
        )
      }

      paste(c(lines, "}"), collapse = "\n")
    }

    compile_with_rtinycc <- function(plan, interface, result, formals) {
      symbol <- interface@symbol
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

      ffi_arg_types <- lapply(formals, function(value) {
        if (value@type@shape@rank == 0L) "f64" else "numeric_array"
      })
      needs_length_argument <- interface@needs_length
      if (result@type@shape@rank == 1L) {
        ffi_arg_types <- c(ffi_arg_types, list("i32"))
        ffi_return <- list(type = "numeric_array", length_arg = length(ffi_arg_types), free = TRUE)
      } else {
        if (needs_length_argument) {
          ffi_arg_types <- c(ffi_arg_types, list("i32"))
        }
        ffi_return <- "f64"
      }

      ffi <- Rtinycc::tcc_ffi()
      ffi <- do.call(Rtinycc::tcc_bind, c(
        list(ffi),
        stats::setNames(list(list(args = ffi_arg_types, returns = ffi_return)), symbol)
      ))
      ffi <- Rtinycc::tcc_source(ffi, plan@attrs$source)
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
        vector_argument_positions <- which(vapply(
          formals,
          function(value) value@type@shape@rank == 1L,
          logical(1)
        ))
        element_count <- 1L
        if (length(vector_argument_positions) > 0L) {
          vector_lengths <- vapply(arguments[vector_argument_positions], length, integer(1))
          if (length(unique(vector_lengths)) != 1L) {
            tccq_abort(
              "runtime.incompatible_lengths",
              "Vector arguments must have the same length.",
              phase = "runtime",
              path = "callable.arguments",
              data = list(lengths = vector_lengths)
            )
          }
          element_count <- vector_lengths[[1L]]
        }
        call_arguments <- arguments
        if (needs_length_argument) {
          call_arguments <- c(call_arguments, list(as.integer(element_count)))
        }
        do.call(compiled[[symbol]], call_arguments)
      }

      attrs <- plan@attrs
      attrs$compiled <- compiled
      attrs$callable <- callable
      attrs$artifacts$jit_callable <- jit_callable_artifact(symbol, callable)
      plan@attrs <- attrs
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
      attrs <- plan@attrs
      attrs$source_path <- source_path
      attrs$wrapper_source <- wrapper_source
      attrs$wrapper_source_path <- wrapper_source_path
      attrs$wrapper_symbol <- wrapper_symbol
      attrs$artifacts$source <- source_artifact(symbol, source, path = source_path)
      attrs$artifacts$wrapper_source <- source_artifact(wrapper_symbol, wrapper_source, path = wrapper_source_path)
      plan@attrs <- attrs
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

      attrs <- plan@attrs
      attrs$shared_library_path <- library_path
      attrs$shared_library_output <- compile_log
      attrs$artifacts$shared_library <- shared_library_artifact(symbol, library_path)
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
        plan@attrs <- attrs
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
        plan@attrs <- attrs
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
        do.call(.Call, c(list(native_symbol), arguments))
      }
      attrs$shared_library <- shared_library
      attrs$native_symbol <- native_symbol
      attrs$callable <- callable
      attrs$artifacts$native_callable <- native_callable_artifact(symbol, callable)
      plan@attrs <- attrs
      tccq_result(success = TRUE, value = plan, diagnostics = list())
    }

    result <- result_value()
    formals <- formal_values()
    lowered_diagnostic <- validate_lowered_program(result, formals)
    if (!is.null(lowered_diagnostic)) {
      plan <- diagnostic_plan(list(lowered_diagnostic))
      return(tccq_result(success = FALSE, value = plan, diagnostics = list(lowered_diagnostic)))
    }
    source_expression_result <- tccq_expression_tree(program)
    if (!source_expression_result@success) {
      plan <- diagnostic_plan(source_expression_result@diagnostics)
      return(tccq_result(
        success = FALSE,
        value = plan,
        diagnostics = source_expression_result@diagnostics
      ))
    }
    source_expression <- source_expression_result@value

    symbol <- source_symbol()
    function_interface <- backend_function_interface(symbol, source_expression, result, formals)
    source_result <- tryCatch(
      switch(
        source_language,
        c = emit_c_source(function_interface, source_expression, result, formals),
        fortran = emit_fortran_source(function_interface, source_expression, result, formals),
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
      attrs = list(
        driver = backend@driver,
        runtime = context@runtime,
        source_language = source_language,
        symbol = symbol,
        function_interface = function_interface,
        source = source,
        artifacts = list(source = source_artifact(symbol, source)),
        expression = source_expression,
        storage_plan = program@storage_plan
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
