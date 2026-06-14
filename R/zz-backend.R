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
  s7contract::impl_trait(
    TccqBackend,
    TccqBackendSpec,
    methods = list(
      prepare = function(backend, program, context) {
        .tccq_backend_spec_prepare(backend, program, context)
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
  prepare = .tccq_backend_lowering_absent
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

    emit_c_source <- function(symbol, source_expression, result, formals) {
      parameter_names <- c_identifier("input", seq_along(formals))
      parameter_by_value_id <- as.list(stats::setNames(
        parameter_names,
        vapply(formals, function(value) value@id, character(1))
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
      if (result@type@shape@rank == 0L) {
        expression <- expression_text(source_expression, parameter_by_value_id, NULL, "c")
        return(paste(c(
          "#include <math.h>",
          "",
          sprintf("double %s(%s) {", symbol, paste(unlist(parameter_declarations), collapse = ", ")),
          sprintf("  return %s;", expression),
          "}"
        ), collapse = "\n"))
      }

      length_name <- "length_0001"
      index_name <- "index_0001"
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
        sprintf("  double *output = (double *)malloc(sizeof(double) * (size_t)%s);", length_name),
        "  if (output == NULL) {",
        "    return NULL;",
        "  }",
        sprintf("  for (int %s = 0; %s < %s; ++%s) {", index_name, index_name, length_name, index_name),
        sprintf("    output[%s] = %s;", index_name, expression),
        "  }",
        "  return output;",
        "}"
      ), collapse = "\n")
    }

    emit_fortran_source <- function(symbol, source_expression, result, formals) {
      parameter_names <- c_identifier("input", seq_along(formals))
      parameter_by_value_id <- as.list(stats::setNames(
        parameter_names,
        vapply(formals, function(value) value@id, character(1))
      ))
      if (result@type@shape@rank == 0L) {
        declarations <- Map(function(value, parameter_name) {
          sprintf("  real(c_double), value :: %s", parameter_name)
        }, formals, parameter_names)
        expression <- expression_text(source_expression, parameter_by_value_id, NULL, "fortran")
        return(paste(c(
          sprintf("function %s(%s) bind(c, name = \"%s\") result(output)", symbol, paste(parameter_names, collapse = ", "), symbol),
          "  use iso_c_binding, only: c_double",
          "  implicit none",
          unlist(declarations),
          "  real(c_double) :: output",
          sprintf("  output = %s", expression),
          sprintf("end function %s", symbol)
        ), collapse = "\n"))
      }

      length_name <- "length_0001"
      index_name <- "index_0001"
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
          "subroutine %s(%s, %s, output) bind(c, name = \"%s\")",
          symbol,
          paste(parameter_names, collapse = ", "),
          length_name,
          symbol
        ),
        "  use iso_c_binding, only: c_double, c_int",
        "  implicit none",
        sprintf("  integer(c_int), value :: %s", length_name),
        unlist(declarations),
        sprintf("  real(c_double), intent(out) :: output(%s)", length_name),
        sprintf("  integer(c_int) :: %s", index_name),
        sprintf("  do %s = 1, %s", index_name, length_name),
        sprintf("    output(%s) = %s", index_name, expression),
        "  end do",
        sprintf("end subroutine %s", symbol)
      ), collapse = "\n")
    }

    compile_with_rtinycc <- function(plan, symbol, result, formals) {
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
      if (result@type@shape@rank == 1L) {
        ffi_arg_types <- c(ffi_arg_types, list("i32"))
        ffi_return <- list(type = "numeric_array", length_arg = length(ffi_arg_types), free = TRUE)
      } else {
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
        if (result@type@shape@rank == 1L) {
          call_arguments <- c(call_arguments, list(as.integer(element_count)))
        }
        do.call(compiled[[symbol]], call_arguments)
      }

      attrs <- plan@attrs
      attrs$compiled <- compiled
      attrs$callable <- callable
      plan@attrs <- attrs
      tccq_result(success = TRUE, value = plan, diagnostics = list())
    }

    result <- result_value()
    formals <- formal_values()
    lowered_diagnostic <- validate_lowered_program(result, formals)
    if (!is.null(lowered_diagnostic)) {
      plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(lowered_diagnostic))
      return(tccq_result(success = FALSE, value = plan, diagnostics = list(lowered_diagnostic)))
    }
    source_expression_result <- tccq_expression_tree(program)
    if (!source_expression_result@success) {
      plan <- .tccq_backend_empty_plan(backend, context, diagnostics = source_expression_result@diagnostics)
      return(tccq_result(
        success = FALSE,
        value = plan,
        diagnostics = source_expression_result@diagnostics
      ))
    }
    source_expression <- source_expression_result@value

    symbol <- source_symbol()
    source_result <- tryCatch(
      switch(
        source_language,
        c = emit_c_source(symbol, source_expression, result, formals),
        fortran = emit_fortran_source(symbol, source_expression, result, formals),
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
      plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(diagnostic))
      return(tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic)))
    }
    source <- source_result
    bridges <- c(
      Map(function(value, bridge_index) {
        tccq_bridge_plan(
          id = sprintf("bridge_input_%04d", bridge_index),
          kind = "sexp_to_buffer",
          from_space = "r",
          to_space = "host",
          from_type = value@type,
          to_type = value@type,
          effect = tccq_effect(reads = TRUE, allocates = TRUE)
        )
      }, formals, seq_along(formals)),
      list(tccq_bridge_plan(
        id = "bridge_output_0001",
        kind = "buffer_to_sexp",
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
        source = source,
        expression = source_expression,
        storage_plan = program@storage_plan
      )
    )

    if (isTRUE(execute_with_rtinycc) && identical(context@mode, "jit")) {
      return(compile_with_rtinycc(plan, symbol, result, formals))
    }
    tccq_result(success = TRUE, value = plan, diagnostics = list())
  }
}

.tccq_backend_spec_prepare <- function(backend, program, context) {
  mode_diagnostic <- .tccq_backend_mode_diagnostic(backend, context)
  if (!is.null(mode_diagnostic)) {
    plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(mode_diagnostic))
    return(tccq_result(success = FALSE, value = plan, diagnostics = list(mode_diagnostic)))
  }

  target_diagnostic <- .tccq_backend_target_diagnostic(backend, context)
  if (!is.null(target_diagnostic)) {
    plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(target_diagnostic))
    return(tccq_result(success = FALSE, value = plan, diagnostics = list(target_diagnostic)))
  }

  capability_diagnostic <- .tccq_backend_capability_diagnostic(backend, context)
  if (!is.null(capability_diagnostic)) {
    plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(capability_diagnostic))
    return(tccq_result(success = FALSE, value = plan, diagnostics = list(capability_diagnostic)))
  }

  result <- backend@prepare(backend, program, context)
  .tccq_check_s7(result, TccqResult, "TccqResult", "backend result")
  result
}

.tccq_backend_lowering_absent <- function(backend, program, context) {
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
  plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(diagnostic))
  tccq_result(success = FALSE, value = plan, diagnostics = list(diagnostic))
}

.tccq_backend_empty_plan <- function(backend, context, diagnostics = list()) {
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

.tccq_backend_capability_diagnostic <- function(backend, context) {
  missing <- setdiff(context@required_capabilities, backend@capabilities)
  if (length(missing) == 0L) {
    return(NULL)
  }
  tccq_diagnostic(
    "backend.missing_capability",
    "Backend does not expose all requested capabilities.",
    phase = "backend",
    path = "backend_context.required_capabilities",
    data = list(
      backend = backend@id,
      requested = context@required_capabilities,
      missing = missing,
      supported = backend@capabilities
    )
  )
}

.tccq_backend_mode_diagnostic <- function(backend, context) {
  if (context@mode %in% backend@modes) {
    return(NULL)
  }
  tccq_diagnostic(
    "backend.unsupported_mode",
    "Backend does not support the requested mode.",
    phase = "backend",
    path = "backend_context.mode",
    data = list(
      backend = backend@id,
      requested = context@mode,
      supported = backend@modes
    )
  )
}

.tccq_backend_target_diagnostic <- function(backend, context) {
  if (identical(context@target, "any") || identical(context@target, backend@target)) {
    return(NULL)
  }
  tccq_diagnostic(
    "backend.unsupported_target",
    "Backend does not support the requested target.",
    phase = "backend",
    path = "backend_context.target",
    data = list(
      backend = backend@id,
      requested = context@target,
      supported = backend@target
    )
  )
}
