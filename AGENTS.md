# Repo Scope And Rules

## Prologue

Always ask yourself before landing a change what other changes would have made that change easier to land and other changes to land long term. 
Sometimes as needed, you can call into the user or a PI/GPT/reviewer fellow for ideas and task to accomplish and ask them this question with current state of the project, avenues you see. Having several perspectives and long term maintenability is important ! Ambiguities should be avoided. Code sprawl and bloat too. Allignmment among actors is a must.

write C as a BSD kernel programmer rather than a Java programmer that failed upwards
write R as a r-lib programmer rather than a Python programmer that failed upwards

## Scope

`tccquickr` is a hard-reset experimental compiler core for a declared subset of
R. The current package is not a working R-to-C compiler and should not pretend
to be one. It is the typed semantic foundation that future lowering and backend
work must justify itself against.

This repo is responsible for:

- declared-R frontend analysis rooted in `declare(type(...))`
- S7 schemas for compiler values: dimensions, shapes, types, effects, bindings,
  IR values, programs, diagnostics, backend plans, and phase results
- `s7contract` interfaces for internal compiler protocols, especially pass
  execution and backend planning
- classed diagnostics and result values instead of branching on error strings
- symbolic shape, effect, legality, and pass-pipeline work before backend work
- compiler-facing tests for schema validity, frontend diagnostics, and contract
  behavior
- the known failing apotheosis suite documented in `README.Rmd`

This repo is not currently responsible for:

- C emission
- TinyCC integration
- shared-library compilation
- a JIT cache
- fake compatibility shims for deleted compiler paths
- vignettes, ADR sprawl, or proof scaffolding ahead of a stable semantic core

Backends may come back later, but only after the typed core can explain the
program it is lowering.

## Current Architecture

The active architecture is the small typed core:

- `R/aaa-schema.R`: S7 classes and constructors for compiler schemas.
- `R/zz-backend.R`: generic backend descriptors, bridge plans, safepoints, debug
  metadata, runtime policies, and backend-planning contracts.
- `R/conditions.R`: diagnostic/result values and classed compiler conditions.
- `R/contracts.R`: `s7contract` protocol for compiler passes.
- `R/frontend.R`: declaration extraction, `codetools`-assisted call discovery,
  and operation-registry diagnostics.
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
7. Add generic backend planning before target code emission.
8. Add C emission only after the above model is coherent and tested.

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
  declarations. Use `TccqOpImpl`, `TccqOpImplementation`, and
  `TccqOpRegistry`; do not hardcode local allowed/not-allowed vectors in the
  frontend.
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
- Physical representation is separate from semantic type. Use `TccqLayout` for
  order/strides/contiguity and `TccqTile` for rectangular partition metadata.
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
  implementation, pure C implementation, Fortran implementation, Mojo
  implementation, CUDA/device implementation, or explicit boundary
  implementation. The frontend asks an operation registry rather than owning
  the support policy.
- Backend support is generic. Use `TccqBackendSpec`, `TccqBackend`,
  `TccqBackendContext`, `TccqBackendPlan`, `TccqBackendPlanSet`, and explicit
  bridge/safepoint/debug metadata. `Rtinycc` is one backend descriptor for a
  C/TinyCC path; it is not the architecture.
- Keep several backend families visible while shaping the IR: generic C,
  Rtinycc/TinyCC C, quickr-style Fortran, anvil-style graph/StableHLO/XLA or
  device paths, and R call evaluation. Divergence between those families should
  produce typed capability or legality diagnostics, not target-specific hacks.
- Backend planning must make representation transitions explicit through
  `TccqBridgePlan` values. Do not hide `SEXP -> buffer`, `buffer -> SEXP`,
  host/device transfer, layout conversion, tile materialization, or object-mode
  boundaries inside emitted strings.
- Interrupt and debugger support belong in runtime policy, safepoints, and debug
  sites. Host/R-API regions may use direct R interrupt checks; pure kernel,
  parallel, and device regions need chunking, polling, or host orchestration
  safepoints instead.
- The next acceptable failure point should move deeper through the typed IR,
  not sideways into compatibility glue.
- Matrix operations, reductions, domains, views, mutation, fusion, and storage
  planning must first appear as typed IR concepts.
- No backend should be added until the typed IR can represent the apotheosis
  suite honestly.

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
