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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
  )
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
    attrs = list(role = "generic_c")
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
    attrs = list(runtime = "tinycc")
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
    attrs = list(role = "quickr_fortran")
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
  tccq_result(all(vapply(results, function(result) result@ok, logical(1))), plan_set, diagnostics)
}

.tccq_backend_spec_prepare <- function(backend, program, context) {
  mode_diagnostic <- .tccq_backend_mode_diagnostic(backend, context)
  if (!is.null(mode_diagnostic)) {
    plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(mode_diagnostic))
    return(tccq_result(FALSE, value = plan, diagnostics = list(mode_diagnostic)))
  }

  target_diagnostic <- .tccq_backend_target_diagnostic(backend, context)
  if (!is.null(target_diagnostic)) {
    plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(target_diagnostic))
    return(tccq_result(FALSE, value = plan, diagnostics = list(target_diagnostic)))
  }

  capability_diagnostic <- .tccq_backend_capability_diagnostic(backend, context)
  if (!is.null(capability_diagnostic)) {
    plan <- .tccq_backend_empty_plan(backend, context, diagnostics = list(capability_diagnostic))
    return(tccq_result(FALSE, value = plan, diagnostics = list(capability_diagnostic)))
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
  tccq_result(FALSE, value = plan, diagnostics = list(diagnostic))
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
