# Repo Scope And Rules

## Prologue

Always ask yourself before landing a change what other changes would have made that change easier to land and other changes to land long term. 
Sometimes as needed, you can call into the user or a PI/GPT/reviewer fellow for ideas and task to accomplish and ask them this question with current state of the project, avenues you see. Having several perspectives and long term maintenability is important ! Ambiguities should be avoided. Code sprawl and bloat too. Allignmment among actors is a must.

write C as a BSD kernel programmer rather than a Java programmer that failed upwards
write R as a r-lib programmer rather than a Python programmer that failed upwards

## Scope

`tccquickr` is a hard-reset experimental compiler core for a declared subset of
R. The current package is not a general R-to-C compiler and should not pretend
to be one. It is the typed semantic foundation that lowering and backend work
must justify itself against; the minimal source/backend paths exist only as
typed-IR consumers and contract pressure.

This repo is responsible for:

- declared-R frontend analysis rooted in `declare(type(...))`
- S7 schemas for compiler values: dimensions, shapes, types, effects, bindings,
  IR values, programs, diagnostics, backend plans, and phase results
- `s7contract` interfaces for internal compiler protocols, especially pass
  execution and backend planning
- classed diagnostics and result values instead of branching on error strings
- symbolic shape, effect, legality, and pass-pipeline work before backend work
- operation signatures, implementation metadata, neutral expression lowering,
  and typed source-planning consumers
- compiler-facing tests for schema validity, frontend diagnostics, and contract
  behavior
- the known failing apotheosis suite documented in `README.Rmd`

This repo is not currently responsible for:

- re-owning TinyCC runtime or FFI behavior that belongs in `Rtinycc`
- target-specific compiler architecture hidden in emitted C, Fortran, CUDA, or
  graph strings
- shared-library compilation outside an explicit backend plan
- a JIT cache
- fake compatibility shims for deleted compiler paths
- vignettes, ADR sprawl, or proof scaffolding ahead of a stable semantic core

Backends are allowed only as typed consumers. They should make the core stricter
by exposing missing semantics, not become a shortcut around the core.

## Current Architecture

The active architecture is the small typed core:

- `R/aaa-schema.R`: S7 classes and constructors for compiler schemas.
- `R/zz-backend.R`: generic backend descriptors, bridge plans, and
  backend-planning contracts.
- `R/conditions.R`: diagnostic/result values and classed compiler conditions.
- `R/contracts.R`: `s7contract` protocol for compiler passes.
- `R/frontend.R`: declaration extraction, `codetools`-assisted call discovery,
  and operation-registry diagnostics.
- `R/ops.R`: operation signatures, implementation traits, registries,
  elementwise/reduction specs, and operation source render contracts.
- `R/lowering.R`: typed lowering from the declared subset into values, regions,
  fusion groups, and storage plans.
- `R/z-expression.R`: backend-neutral expression trees, the `TccqLoopNest`
  with-loop plan (SAC lineage), and the loop-nest planner consumed by source
  printers.
- `R/utils.R`: small schema validation helpers.
- `docs/root.md`: the current root direction.

The intended growth path is:

1. Parse declarations from a deliberately narrow R subset.
2. Represent formals, values, effects, diagnostics, and pass outputs as S7
   objects.
3. Build a typed program graph with stable value identity.
4. Add rank and symbolic-shape constraints before lowering operations.
5. Add explicit effects, boundaries, and legality diagnostics.
6. Add array domains, reducers, matrix operations, and fusion only as typed IR
   concepts.
7. Keep generic backend planning ahead of target source printing.
8. Keep C, Fortran, Rtinycc, graph, and object-call paths downstream of the
   typed program and neutral expression handoff.

Do not reintroduce the deleted backend stack to get a demo. The demo is the
semantic core becoming strong enough that a backend is boring.

## Typed OOP And Contract Discipline

The project style is functional OOP with explicit typed values, generics,
interfaces, traits, and gradual runtime contracts. Do not let the codebase
collapse into a pile of private `.something()` functions that encode the real
architecture by convention.

Rules:

- Prefer S7 classes for compiler data and S7 generics for compiler behavior.
- Prefer `s7contract` interfaces for structural protocols between phases and
  pass-like components.
- Use explicit traits when an implementation must opt into behavior rather than
  merely having a compatible method shape.
- Use gradual interface typing: specify argument and return contracts on
  protocol requirements as soon as the expected shape is known.
- Operation/function/kernel support must go through typed implementation
  declarations. Use `TccqOpSignature`, `TccqOpImpl`,
  `TccqOpImplementation`, and `TccqOpRegistry`; do not hardcode local
  allowed/not-allowed vectors in the frontend.
- Argument-count and result-type rules belong in `TccqOpSignature`. Elementwise,
  reduction, and future operation families should carry signatures rather than
  growing category-specific disguised type checks.
- Result-domain and shape compatibility rules belong in `TccqDomainPolicy`
  carried by `TccqOpSignature`. Do not bury scalar broadcast, common-domain,
  recycling, or scalar-result behavior inside anonymous result-type closures or
  backend printers.
- Functions should mostly be constructors, pure transformations, generic
  methods, protocol runners, or small local helpers in service of one of those.
- A private helper is acceptable only when it is truly local glue. It is not
  acceptable for a helper to become the hidden owner of a compiler concept,
  protocol, type rule, legality rule, or pass contract.
- Do not add package-level `.tccq_*` functions for one-use predicates,
  constant-returning name sets, source-name cleanup, or disguised type checks.
  Inline one-use logic, store fixed facts as constants, use local closures
  inside one exported transformation, or promote the concept to typed S7/S7
  contract machinery.
- If a helper starts needing a name from the compiler vocabulary, promote the
  concept into an S7 class, S7 generic, `s7contract` interface, explicit trait,
  or classed diagnostic.
- Avoid untyped list protocols. Lists may hold collections, but their elements
  must be checked at constructor or interface boundaries.
- Avoid stringly dispatch. Stable strings are fine as data fields such as
  diagnostic codes, operation names, or region kinds; they are not a substitute
  for typed values and protocols.
- Mutating S7 objects in place should be exceptional. Prefer functional
  transformations that return new typed values.
- Tests should exercise the typed boundary: constructor validation, interface
  conformance, trait behavior, classed diagnostics, and pass return contracts.

## Semantic Staging Rule

Keep as much semantic information as possible in the R-level compiler before
emitting anything.

That means:

- decide legality, shape constraints, type constraints, effects, ownership,
  aliasing, mutation barriers, reducers, and boundary behavior in typed program
  values before backend design
- prefer explicit S7 schemas and classed diagnostics over ad hoc lists, strings,
  source-substring checks, or hidden target-side branches
- treat calls without an implementation as structured diagnostics until there
  is an explicit operation, effect, or boundary model
- use `codetools` for R structure inspection where it is enough; do not reach
  for source editing libraries when the task is semantic transformation
- let `s7contract` express protocol boundaries between compiler passes
- introduce proof artifacts only for small, stable pass laws; before that, use
  executable tests and structured diagnostics

This is the closest useful lesson from SAC / `sac2c` for this repo: the
compiler knows the array program before it becomes C, so the important
reasoning should happen before C emission. It is also the useful lesson from
Hermes and Numba: phase boundaries, effects, verification, and type inference
belong in the compiler architecture, not in the emitter.

## Current Semantic Commitments

For the reset core, keep these rules explicit:

- `declare(type(...))` is the entry point to the accepted subset.
- R source should be understood through parsed language objects. The C-like
  surface syntax is notation over calls: assignment is `<-`, indexing is `[`,
  subset replacement is parsed as assignment over an indexing call and evaluates
  through `[<-` or another replacement function, operators are calls, blocks
  are `{`, grouping is `(`, flow control is `if`/`for`/`while`/`repeat`/
  `break`/`next`, and function definitions are calls that construct closures.
- The call root does not flatten R's evaluator. Preserve whether a call is a
  special form, builtin, closure call, replacement evaluation, group generic,
  method dispatch, promise-forcing site, or lexical-environment dependency as
  typed facts before backend planning.
- `TccqCall` is the normal frontend root, not an incidental helper. Every call
  records its structural kind, arity, argument tags, original expression, and
  attributes so later passes can attach promise semantics, lexical environment
  requirements, dispatch rules, effects, domains, implementations, and backend
  feasibility.
- `TccqCallSemantics` is the next abstraction boundary after `TccqCall`. Use it
  for evaluator facts such as special vs builtin vs closure, eager vs lazy vs
  special promise forcing, group-generic/S3/replacement dispatch, lexical-scope
  dependence, and control/replacement flags. Do not rediscover those facts in
  backend code or operation predicates.
- `TccqCallIndex` is the current analysis handoff: stable call ids plus
  one-to-one `TccqCall` and `TccqCallSemantics` lists. `TccqProgram@call_index`
  should be consumed by later lowering passes before adding new AST walkers.
- An opaque call is still an operation candidate. Do not treat opacity as an
  R call-evaluation boundary. Object-mode/R-call evaluation is one backend family,
  not the semantic meaning of unknown calls.
- Type declarations currently model base scalar/storage type plus
  rank/symbolic dimensions. `raw` is an R atomic base type; `buffer` is the
  internal opaque storage-facing base type, not an R vector type.
- Matrix and array structure is represented by `TccqShape` rank and dimensions:
  rank 2 is a matrix, rank N is an array. Do not introduce separate matrix or
  array type families unless behavior proves shape rank is insufficient.
- Physical representation is separate from semantic type. Today layout is one
  fixed convention — dense contiguous column-major — hardcoded in the shared
  loop-nest linearizer for every backend. A layout or tile value re-enters the
  schema only as a consumed input to that linearizer, when layout becomes an
  actual choice.
- Scalar special values use `TccqLiteral`, with distinct representations for
  finite values, typed `NA`, `NaN`, `Inf`, and `-Inf`.
- Code sections use `TccqRegion`. `kernel`, `parallel`, and `device` regions
  must not touch the R C API or contain boundary effects; `device` regions must
  declare device memory space.
- Compiler internals are typed with S7 classes, not loose stringly lists.
- Errors are classed conditions carrying `TccqDiagnostic` values.
- Phase APIs return `TccqResult` or typed program values; they do not smuggle
  failure through messages.
- The apotheosis examples in `README.Rmd` are expected to fail today, but they
  must fail with structured diagnostics. Keep both minimal probes and composite
  targets; minimal probes isolate one higher-level idea, while programs such as
  logistic-gradient and Viterbi force ideas to compose.
- Calls without a current implementation are diagnostics, not implicit fallback
  and not invalid R. Use `frontend.unimplemented_call` style diagnostics to
  say what the current registry/context cannot implement.
- Supported calls are contextual: an operation may be supported by an R C API
  implementation, pure C implementation, Fortran implementation, or explicit
  boundary implementation. The frontend asks an operation registry rather than
  owning the support policy. New implementation targets enter the registry
  together with their first real implementation.
- `TccqOpSignature` is the shared operation contract for arity, result-domain
  policy, and result type. `TccqElementwiseSpec` and `TccqReductionSpec` carry
  signatures; reductions add reducer identity and combine behavior. Do not add
  another result-type helper, shape predicate, or source-name switch when a
  signature, domain policy, or implementation trait is the actual concept.
- Lowered operation values must carry a `TccqLoweredOperation` payload that
  preserves the selected implementation, signature, domain policy, operation
  family, and optional reducer facts. Later fusion, access, legality, storage,
  and backend passes should consume that payload instead of rediscovering
  behavior from operation names, value ranks, or backend-local predicates.
- Backend support is generic. Use `TccqBackendSpec`, `TccqBackend`,
  `TccqBackendContext`, `TccqBackendPlan`, `TccqBackendPlanSet`, and explicit
  bridge metadata. `Rtinycc` is one backend descriptor for a
  C/TinyCC path; it is not the architecture.
- Backend families exist in the suite only with a real lowering behind them:
  today generic C, Rtinycc/TinyCC C, and quickr-style Fortran. Divergence
  between families should produce typed capability or legality diagnostics,
  not target-specific hacks. Do not ship descriptor placeholders whose
  capability strings promise behavior nothing implements; a new family
  (graph/StableHLO, device, R call evaluation) enters together with its first
  real lowering.
- Backend planning must make representation transitions explicit through
  `TccqBridgePlan` values. The bridge kind must match the representation, so
  scalar values use scalar bridges and vector/array values use buffer bridges.
  Do not hide `SEXP -> scalar`, `scalar -> SEXP`, `SEXP -> buffer`,
  `buffer -> SEXP`, or object-mode boundaries inside emitted strings.
- Backend planning must also make generated callable shape explicit through
  `TccqBackendFunctionInterface`. Do not make C, Rtinycc, Fortran, or later
  source printers independently infer scalar/map/reduction/axis-reduction
  shape, generated parameter mapping, ABI, result placement, generated result
  names, iteration domain, per-axis input extent parameters, total input
  element-count parameter, per-axis result extent parameters, result-count
  parameter, index variable, or reduction accumulator names.
- Source backends consume exactly one iteration abstraction: `TccqLoopNest`,
  the SAC-style with-loop. It carries ordered `TccqLoopAxis` values (`map`
  produces output positions, `reduce` folds into an accumulator), a body whose
  references carry typed `TccqAccess`/`TccqIndexExpr` affine access maps, an
  optional reducer with identity, and an output access. Scalar programs, maps,
  full and per-axis reductions, `%*%` contractions, and interior stencils are
  instances of this one value. Do not reintroduce per-family printer cases,
  linear element-count ABIs, or string-built index arithmetic; new iteration
  behavior must extend the loop nest and its typed accesses.
- The generated ABI passes one `int` extent parameter per symbolic dimension
  plus a result element-count parameter for non-scalar results. Boundary
  wrappers and JIT callables bind each extent symbol from the first argument
  shape declaring it and check every other occurrence; they must not assume
  arguments share one shape. Affine dimensions (`n - 2`) are `TccqDim` facts
  rendered by the emitters, never precomputed strings.
- Operation families are elementwise (`TccqElementwiseSpec`), reduction
  (`TccqReductionSpec`), and contraction (`TccqContractionSpec`, which owns a
  signature, a reducer, and a combine operation). Reductions and contractions
  must produce the program result under the single-nest lowerer; other
  compositions are `lowering.unsupported_composition` diagnostics until
  multi-nest programs are modeled.
- Backend planning must make concrete products explicit through
  `TccqBackendProducts` and `TccqBackendArtifact`. Function interfaces,
  expression trees, storage plans, generated source, shared libraries, wrappers,
  native callables, and JIT callables belong under the typed products payload,
  not scattered across backend plan `attrs`.
- Shared-library execution from R must cross through an explicit wrapper or
  callable artifact. Do not make user-facing execution depend on ad hoc
  `dyn.load()` / `.Call()` glue outside the backend plan.
- Interrupt and debugger support (polling policy, safepoints, debug sites) is
  deferred schema: it enters together with the emitter behavior that writes
  polls or debug anchors into generated code, never as passive plan metadata.
- The next acceptable failure point should move deeper through the typed IR,
  not sideways into compatibility glue.
- Matrix operations, reductions, domains, views, mutation, fusion, and storage
  planning must first appear as typed IR concepts.
- Fusion-specific operation facts belong in `TccqFusionContract`: lowered
  operations, result operation, operation signatures, domain policies, reducer
  facts, and storage strategy must not be scattered across fusion `attrs`.
- Storage reuse must be derived from typed planning facts such as
  `TccqStorageLifetime`, storage compatibility, aliasing, materialization,
  layout, and memory space. Do not group temporaries by role alone or hide
  reuse assumptions in source printers.
- Source printers consume `TccqExpression` trees built from lowered programs.
  Do not make C, Fortran, TinyCC, CUDA, or graph printers rediscover expression
  semantics by walking raw values and switching on operation strings.
- Operation-specific target spelling belongs to typed implementations through
  `tccq_op_render()` and `TccqOpRenderContext`, not backend-local `if`/`switch`
  ladders over operation names.
- No new backend family should be added unless it makes the typed IR represent
  the apotheosis suite more honestly.

## Docs And Tests Rules

- Never manually write `.Rd` files.
- Generate `.Rd` files from source documentation using `roxygen2`.
- `README.Rmd` is the source for `README.md`.
- Keep docs aligned with the current typed-core architecture.
- Remove stale references to deleted legacy paths or transitional naming.
- Prefer schema, diagnostic, pass-contract, and structured IR checks over
  brittle source-substring assertions.
- Run `make test1` for fast test feedback.
- Run `R CMD check --no-manual` before committing broad package changes.
- Keep `README.md` rendered from `README.Rmd` when the README changes.
- Keep `.sync/` ignored. It is a local research cache for Hermes, Numba,
  quickr, anvl, Simple, and s7contract, not package source.
- Do not add vignettes or broad docs until the semantic core has more than one
  real layer to explain.
