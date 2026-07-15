# Architecture

The typed core of `tccquickr`, phase by phase. The root direction lives in
[root.md](root.md); repo rules live in [AGENTS.md](../AGENTS.md). The README
demonstrates the working surface; this document explains the representation
behind it.

## Core model

The compiler state is a typed value graph, not source text. `TccqType` combines
a base type with a `TccqShape`; the shape carries rank and symbolic dimensions,
so rank 2 is a matrix and rank N is an array. `TccqLiteral` gives finite
values, typed `NA`, `NaN`, `Inf`, and `-Inf` their own representation.
`TccqValue` is the IR value: it names an operation, inputs, type, effects, and
attributes. `TccqBranch` inherits from `TccqValue` and adds the condition,
consequent, alternative, and originating `TccqCallSemantics` needed to preserve
R's lazy `if` evaluation. `TccqProgram` collects formals, values, regions,
result, and diagnostics.

Physical layout is currently a fixed convention, not a value: every array is a
dense, contiguous, column-major R buffer, and the shared loop-nest emitter
hardcodes that convention in its stride computation (C reads `x[linear]`,
Fortran reads `x(linear + 1)` over the same linearization). A layout value
returns to the schema when layout becomes a choice — row-major foreign
buffers, transpose elision, blocked/tiled storage — and it returns as a
consumed input to the linearizer, not as a passive annotation. The same rule
holds for tiling metadata.

## Call root

The semantic root is that R programs are language objects made of calls, names,
constants, promises, and environments. The C-like surface syntax is notation.
Parsed assignment is a call to `<-`; indexing is a call to `[`, `[[`, or `$`;
subset replacement is parsed as assignment over an indexing call and evaluates
through a replacement function such as `[<-`, `[[<-`, `$<-`, or another
replacement function; arithmetic and logical operators are calls with operator
names; blocks are calls to `{`; grouping is a call to `(`; flow control is
represented as calls such as `if`, `for`, `while`, `repeat`, `break`, and
`next`; function definitions are calls that construct closures.

This call-root view does not erase R's evaluator. Some calls are special forms
with non-standard promise forcing, some are builtins, some are closures with
lexical environments, and some dispatch through S3, S4, or S7. The compiler's
first job is to preserve those distinctions as typed call facts rather than
falling back to a C-shaped syntax tree. Only after the call has a signature,
effect, forcing policy, dispatch model, domain, and region can a backend claim
that it implements the call.

That means `TccqCall` is not a narrow frontend convenience. It is the first
normal form of the program. A call records its name, structural kind, arity,
argument tags, source expression, and attributes. Later passes may attach
signatures, promise-forcing rules, lexical environment requirements, dispatch
rules, effects, domains, implementations, and backend feasibility. An opaque
call is still an operation candidate; it is not automatically R call evaluation
and it is not illegal merely because we do not yet know how to lower it.

The next layer is `TccqCallSemantics`. It records facts the evaluator would care
about before any backend exists: whether the call resolves to a special form,
builtin, closure, compiler directive, or unknown function; whether arguments are
forced eagerly, lazily, specially, through replacement evaluation, or by the
compiler; whether dispatch is ordinary, S3, primitive S3, group-generic, or
replacement dispatch; and whether lexical scope is part of the call contract.
Frontend analysis stores both observed calls and these semantics in a
`TccqCallIndex`, then attaches that typed index to `TccqProgram@call_index` so
lowering has checked facts available rather than digging through untyped
attributes. The lowerer consumes those facts as a kernel-entry barrier: a
registry implementation replaces a call, so lazy closure forcing is compatible
when arguments are pure and an unbound name is registry vocabulary rather than
invalid R. Because every kernel value is an unclassed declared atomic, S3
dispatch resolves statically to the generic's default method (`mean` is
legal); the barrier fires — a classed `lowering.semantics_barrier` diagnostic
even when the registry claims the name — only when the generic has no default
method, because R itself could not evaluate that call on declared atomic
arguments. The current expression lowerer still
reconstructs value dependencies from the function body for a very small
subset, but operation availability is no longer a yes/no string predicate.
Each lowerable call is resolved to a typed implementation record before a
value is created.

## Operations

Function/operator support is contextual. The frontend does not own a fixed
allowed/not-allowed list. It asks a typed registry. A `TccqOpImpl` describes one
implementation of an operation, and that implementation must explicitly opt into
the `TccqOpImplementation` trait. A `TccqOpRegistry` is the visible set of
implementations, and `TccqOpContext` describes the requested target, region
kind, memory space, and whether R C API or boundary implementations are allowed.

An operation can be supported by several implementations: R C API evaluation,
pure C, Fortran, or an explicit boundary. Those choices are different because
they have different effects and region constraints, and new implementation
targets enter the registry together with their first real implementation.

The operation registry answers implementation questions, not grammar questions.
If a call has no implementation in the current registry and context, analysis
returns a classed `frontend.unimplemented_call` diagnostic. That diagnostic says
what is missing from the compiler model; it does not mean the call is not R.

`TccqResolvedOp` is the handoff from registry query to lowering. It records the
observed call, selected `TccqOpImpl`, target, region kind, memory space, purity,
boundary status, R C API usage, and effect. `TccqLoweredOperation` is the next
payload: it keeps the lowered family, resolved operation, signature, domain
policy, and optional reducer identity together on the operation value. Fusion,
storage, and backend planning consume that typed payload instead of
rediscovering semantics from `TccqValue@op` or a handful of loose attributes.
Operation implementations may also expose a source renderer through
`tccq_op_render()`. That renderer is an implementation capability, not a
backend-local switch over operation strings.

`TccqOpSignature` is the shared operation contract for arity, result-domain
policy, and result typing. It is deliberately smaller than a full type system,
but it is the right owner for facts such as "`sqrt` accepts one input, uses the
common elementwise domain, and returns double" or "this reducer accepts one
array expression and returns a scalar". A `TccqDomainPolicy` computes the result
shape from input types before target source generation, so scalar broadcast,
common elementwise domains, and scalar reducer results are typed operation facts
rather than hidden closure checks. Elementwise, reduction, and future operation
families carry signatures rather than each growing private arity predicates,
shape predicates, and result-type helpers.

Elementwise calls follow operation metadata too. A call lowers as elementwise
only when the resolved operation carries a `TccqElementwiseSpec`, which carries
a `TccqOpSignature`. The default registry provides a small numeric elementwise
surface for arithmetic, unary negation, powers, `sqrt`, and `exp`, but another
registry can add a source-rendered call such as `square(x)` without changing the
lowerer or printers.

Reducers follow the same rule. A reducer is not recognized because the lowerer
knows the text `sum`; a call lowers as a reduction only when the resolved
operation carries a `TccqReductionSpec`. That spec carries a `TccqOpSignature`
for the call shape, result-domain policy, and result type, then adds the
identity literal and accumulator combine expression needed by source printers.
The default registry models base `sum` as one such implementation, and another
registry can model a different fold surface through the same contract without
changing the lowerer or the C and Fortran printers.

Contractions are the third operation family. A call lowers as a contraction
only when the resolved operation carries a `TccqContractionSpec`, which owns
the shared signature, the reducer folded along the contracted axes, the
elementwise combine operation applied to aligned elements, and the pair of
operand dimensions that contract. The default registry models `%*%`
(contracting dims 2 and 1), `crossprod` (1 and 1), and `tcrossprod` (2 and 2)
for rank-2-by-rank-1 and rank-2-by-rank-2 inputs with a typed
contracted-dimension compatibility rule; transposition is an operand
axis-order fact in the nest plan, never a materialized transpose, so all
three are instances of the same loop-nest plan as maps and reductions.
Reducers may additionally carry a finalizer applied to the folded accumulator
once the reduce loops close — `mean`, `colMeans`, and `rowMeans` divide by
the reduced element count — rendered through `tccq_reduction_finalize()`.

## Current lowering boundary

The first lowering pass is deliberately small. It handles scalar and rank-N
elementwise expressions, full-domain rank-N reductions, per-axis reductions
such as `colSums()` and `rowSums()`, `%*%` contractions, and rank-1 interior
slices such as `x[2:(n - 1L)]` whose bounds are affine in declared dimension
symbols. Slice extents are typed affine dimensions (`n - 2` is a `TccqDim`
fact, not a printed string), and slices themselves disappear into typed affine
accesses rather than materializing values. Programs compose across loop
nests: every non-root reduction or contraction becomes its own fusion group
and nest, materializing a named scalar for rank-0 results and a temporary
buffer otherwise, consumed by later nests through ordinary typed accesses —
so `x / sum(x)`, `colSums(x) + 1`, `(x %*% w) + y`, and
`cs <- colSums(x); cs / sum(cs)` all lower to ordered nest sequences.
Extraction is keyed by value id, so a value consumed twice materializes once.
Declared dimension symbols are scalar values in the body: `n` in
`colSums(x) / n` lowers to a `dim_symbol` value that the emitters render from
the extent parameter the ABI already passes, widened to double. Rank-mixed
elementwise operands follow R's recycling rule with GNU-R as the oracle:
operands of rank two or more must agree exactly (non-conformable arrays are
refusals, as in R), and a shorter operand whose dimensions provably divide
the host's recycles over the host's column-major element order through a
typed `recycle` access — a modulo-linear index the emitters render, which is
also why recycled *subtrees* are correct for free: pointwise operations
commute with recycling.
Lowering returns a `TccqLoweringPlan`, then embeds the plan into
`TccqProgram` as values, regions, fusion groups, and storage facts. Each
operation value carries one `TccqLoweredOperation` payload that owns the shared
`TccqOpSignature`, result-domain policy, selected implementation, and optional
reducer facts used to type the call. The fusion-group partition is the typed
record of the composition decision: one `map_reduce` group per scalar
intermediate over its input domain, then the main group over the result
domain, each preserving operation signatures and domain policies and deriving
its target and effect from the resolved operations. Later fusion, access,
legality, storage, and backend passes should consume those contracts rather
than infer operation behavior from names, ranks, or emitted source. This is
not a general legality pass and it is not the place where new language
coverage should sprawl.

Top-level local assignment is currently modeled as single-assignment binding.
In `a <- expr`, the local symbol `a` becomes a name for the lowered value of
`expr`; it is not treated as mutation. Rebinding a local name, rebinding a
formal, or mutating through a formal such as `x[i] <- value` produces a classed
lowering diagnostic. That is intentionally strict until mutation barriers,
aliasing, materialization, and view semantics are represented in the
middle-end.

The important constraint is that lowering failure is not frontend failure. A
registered opaque operation can be a valid analyzed operation even when the
current lowerer cannot emit it. Backend planning then reports that no lowered
program is available for the requested backend. This preserves the difference
between "R operation not modeled", "operation modeled but not lowerable here",
and "operation lowerable but unsupported by this backend".

The minimal storage plan marks formals, literals, temporaries, and the output
explicitly so printers can consume a typed plan. It records each slot's
definition and last use as a `TccqStorageLifetime`, then only groups same-type
temporary slots for reuse when those typed lifetimes do not overlap. This is
still conservative: aliasing, mutation, materialization, layout, views, and
device memory need explicit passes before storage reuse can become aggressive.

Source printers consume an ordered sequence of `TccqLoopNest` values built
from the lowered program: backend-neutral with-loops in the SAC lineage,
intermediates first, the result nest last. Each nest carries ordered
`TccqLoopAxis` values (`map` axes produce output positions, `reduce` axes fold
into an accumulator), a `TccqExpression` body whose references carry typed
`TccqAccess` maps of affine `TccqIndexExpr` values, an optional reducer with
its identity, and an output access; intermediate nests additionally name the
scalar or temporary buffer their result materializes. Scalar programs,
elementwise maps, full and per-axis reductions, contractions, stencils, and
scalar- or buffer-intermediate compositions are all sequences of this one
value, so C, Fortran, and Rtinycc share a single generic per-nest emitter
instead of per-family printer cases; the C emitter owns buffer allocation and
free discipline, and Fortran declares automatic arrays. A branch-valued nest
body remains a typed `TccqExpression` carrying its `TccqBranch`; both source
families emit a conditional assignment, preserving that exactly one arm is
evaluated.
When a printer reaches an operation node, it asks the resolved implementation
to render through `tccq_op_render()` for a `TccqOpRenderContext`.

The generated callable boundary is a second explicit plan value:
`TccqBackendFunctionInterface`. It records the source symbol, scalar or
loop-nest shape, ABI, parameter names, lowered parameter value ids, result
value id, parameter and result `TccqType` values, result placement, the
loop-nest iteration domain, one extent
parameter per symbolic dimension (`n` becomes `int extent_n`, deduplicated
across arguments), per-axis loop index names, typed result dimensions, the
result element-count parameter, and the reduction accumulator name. C,
Rtinycc, and Fortran consume that same interface before concrete source is
emitted, and the generated R boundary wrappers bind each extent symbol from
the first argument shape that declares it, then check every other occurrence —
so `matrix_vector(x = double(n, p), w = double(p))` validates that `ncol(x)`
equals `length(w)` instead of assuming all arguments share one shape. A C
kernel returns an allocated pointer while a Fortran `bind(c)` kernel exposes
an output argument, but that difference is already explicit in the function
interface before either printer runs.

Backend products are also explicit. `TccqBackendProducts` carries the typed
function interface, loop nest, expression tree, storage plan, and concrete
`TccqBackendArtifact` values. Source mode records a source artifact carrying
the generated source. Shared-library mode writes that same source artifact plus
a generated C `.Call` wrapper to disk, invokes `R CMD SHLIB`, records the
resulting shared library as another artifact, and attaches a native callable
artifact when the wrapper symbol can be loaded. Rtinycc JIT mode keeps the same
source and function interface, then attaches a callable artifact after TinyCC
compilation. The architectural boundary is the typed expression, typed function
interface, typed storage plan, and typed product set.

## Control flow

Control flow is entering as structured IR. A pure scalar `if` with an explicit
`else` and identically typed arms becomes `TccqBranch`. The condition must be a
scalar logical, and its backend interface preserves that type as C `bool`,
Fortran `logical(c_bool)`, or TinyCC `bool`; wrappers reject a missing condition
instead of mapping it to a Boolean. The current loop-nest planner accepts the
branch as a result expression and emits statement-valued control. Pure branches
may nest in result arms because both source emitters recursively assign through
the same target. A branch used as another branch's condition stops until a
typed temporary can be scheduled before the outer branch; branch-local
reductions and contractions stop until their loop nests can be scheduled inside
the selected arm.

`for`, `while`, `repeat`, `break`, `next`, `switch`, vectorized `ifelse`, and
idiomatic R surfaces such as `Map`, `lapply`, `vapply`, `apply`, `Reduce`, and
`Filter` are not just more function names to whitelist. They describe regions,
exits, dominance, carried loop state, effect ordering, reducer legality,
allocation, and possible boundary regions.

The intended representation is a small set of typed control nodes over the same
value/effect/domain model. A `for` loop over `1:n`, a `Reduce("+", x)`, and a
`sum(x)` can all become reduction regions when their reducer implementation,
identity, missing-value policy, and effects are known. `ifelse` is not ordinary
branching when it is vectorized; it is a select over a domain with recycling and
missing-value rules. `switch` is a dispatch boundary until the selector type
and case set are known. `break` and `next` are structured exits, not hidden
`goto` strings.

## Regions and bridges

Executable code is grouped into `TccqRegion` values. A region records where code
may run and what effects are legal. A `host` region is ordinary host
orchestration. A `kernel` region is an R-API-free scalar or array kernel. A
`parallel` region is an R-API-free parallel kernel. A `device` region is
device-side code using device memory.

R C API evaluation is one possible backend implementation family, not the
semantic meaning of an opaque call and not a universal escape mode. When
selected it is a boundary that touches the R API, so it cannot silently appear
inside pure, parallel, or device regions.

Bridges are a typed backend-plan layer. They represent transitions such as
`SEXP -> scalar`, `scalar -> SEXP`, `SEXP -> C buffer`, `C buffer -> SEXP`,
and explicit R call-evaluation boundaries. These are plan values, not
target-side string glue, and their kind must match the shape of the value being
bridged. Host/device transfer, layout conversion, and tile materialization
bridges return alongside the passes that need them.

## Backend planning

Backends are implementation choices behind a trait. A `TccqBackendSpec`
describes a backend family, target, driver, supported modes, region kinds,
memory spaces, capabilities, and a planning function. The compiler calls the
`TccqBackend` trait and receives a `TccqBackendPlan`, not emitted code. The plan
contains assigned regions, bridges, diagnostics, capabilities, and backend
attributes.

Rtinycc comes for free only in the sense that it is one current driver for C
source and TinyCC JIT modes. It should not decide the IR. The generic C
descriptor, the Rtinycc descriptor, and the quickr-style Fortran descriptor
print source from the same `TccqLoopNest` through the same
`TccqBackendFunctionInterface` handoff, and source, shared-library, and JIT
products are recorded as `TccqBackendArtifact` values. A backend that cannot
lower a program reports typed planning diagnostics instead of vetoing the
suite, so `tccq_compile()` succeeds when at least one backend produces a
working plan. The C/Fortran split (return-pointer vs output-argument ABI,
0- vs 1-based indexing) is the live cross-family pressure on the IR; a new
backend family (graph/StableHLO, device, R call evaluation) enters the suite
together with its first real lowering, not as a capability list ahead of one.

Runtime instrumentation (interrupt polling policy, safepoints at loop
backedges and reduction chunks, debug sites tying generated code back to
source) is a real future concern, but it enters the schema only together with
an emitter that writes polls or debug anchors into generated code. The earlier
`TccqRuntimePolicy`/`TccqSafepoint`/`TccqDebugSite` classes were deleted
because nothing populated or emitted them; the design intent lives here until
the consuming pass exists.

## Fusion

Fusion is a transformation over a typed value graph. It is not a source rewrite
and not a backend shortcut. `TccqDomain` names the iteration space,
`TccqAccess` describes how a value maps onto that domain, and
`TccqFusionGroup` represents a candidate fused group over a domain, values,
outputs, accesses, target, region kind, and effects. `TccqFusionContract`
records the typed result value, lowered operation payloads, operation
signatures, domain policies, optional operation carried by the result, and
storage strategy for that group. This distinction matters for control values:
a `TccqBranch` remains the result without being misrepresented as whichever
ordinary operation happened to occur in one arm.

Simple `f(g(x))` fusion is just one case: a pure single-use producer and pure
consumer over the same domain with compatible implementations and domain
policies. Map-reduce, stencil, tile, and device fusion are extensions of the
same domain/access/region model.

Fusion stops at explicit barriers: R call-evaluation boundaries, unknown
effects, mutation, incompatible layouts, unmodeled missing-value semantics,
illegal region effects, or domain mismatches without an explicit
broadcast/recycle rule.

## Diagnostics

Failure is a value. Phases return `TccqResult` values and attach
`TccqDiagnostic` objects. Conditions carry diagnostics as payloads. The goal is
not to make programs we cannot explain appear to compile; it is to make every
failure specific enough that the next typed concept to add is obvious.
