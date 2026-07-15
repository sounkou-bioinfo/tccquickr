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
result, diagnostics, and one ordered `TccqProgramSchedule`.

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
symbols. It also handles scalar loop-carried cells updated by `while` and
`repeat` loops after explicit initialization, including nested procedural `if`
statements and nearest-loop `break`/`next` transfers over typed blocks. Slice
extents are typed
affine dimensions (`n - 2` is a `TccqDim`
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

Top-level execution is modeled by `TccqProgramSchedule`, whose contiguous
`TccqEvaluationStep` values account for every executable form after declarations
are removed. Each step names its lowered value and complete reachable effect;
an assignment step additionally owns a `TccqLocalBinding` with the local name,
value type, value id, and the same one-based statement position. This is the
single source of top-level evaluation order, including unbound expression
statements. Steps also record the exact local bindings read during symbol
resolution. This distinction is required because two lexical names may alias
one lowered value, so value identity cannot prove local dominance. The schedule
constructor validates value references, binding types, definition-before-use
dominance, complete effects, and the final result before loop-nest planning.

Each local read is a `TccqBindingReference` leaf. It retains the exact
`TccqLocalBinding` and points to the bound storage value, but expression and
effect traversal stop at the reference. Reading `s` therefore does not imply
re-evaluating the graph that defined `s`. The evaluated expression id of an
assignment may differ from the storage value id it binds: in `b <- a`, the step
evaluates a reference to `a` while `b` aliases the resulting value.

The loop-nest planner consumes schedule steps in order before it plans the
returned expression. Non-fusible descendants such as reductions and
contractions are therefore materialized at the evaluation's control path. A
value defined before a later branch is computed once without that branch's
guards, while a conditional definition retains its own selected-arm guards
when later consumers reuse it. Rebinding a local name, rebinding a formal, or
mutating through a formal such as `x[i] <- value` produces a classed lowering
diagnostic. That remains intentionally strict until mutation barriers, aliasing,
materialization, and view semantics are represented in the middle-end.

One local-definition optimization now consumes this schedule. The storage
planner counts actual `TccqBindingReference` occurrences rather than the
schedule's deduplicated binding-use set. It marks a definition non-materialized
only when that occurrence is unique and belongs to the immediately following
step, producer and consumer are pure elementwise trees with identical result
shapes, and neither tree may write, allocate, cross a boundary, error, or warn.
The loop-nest planner then substitutes the dominating typed expression at that
one read. Duplicate reads, later reads, control, reductions, contractions, and
observable effects retain a storage boundary. The decision is
`TccqStorageSlot@materialized`; it is not repeated in attrs or emitter logic.

The important constraint is that lowering failure is not frontend failure. A
registered opaque operation can be a valid analyzed operation even when the
current lowerer cannot emit it. Backend planning then reports that no lowered
program is available for the requested backend. This preserves the difference
between "R operation not modeled", "operation modeled but not lowerable here",
and "operation lowerable but unsupported by this backend".

The minimal storage plan marks formals, literals, temporaries, and the output
explicitly so printers can consume a typed plan. It records each slot's
definition and last use as a `TccqStorageLifetime`. Lifetime analysis derives
value order from `TccqProgramSchedule`, then walks the complete expression
evaluated by each step. It stops at a
`TccqBindingReference` after extending the referenced storage lifetime through
that consumer. This prevents a dependent contraction from overwriting its
input while permitting reuse after an intervening scalar reduction has
finished. Every materialized temporary owns a `TccqStorageAllocation`; sharing
that typed identity is the storage-reuse decision. The current first-fit rule
shares only exact-type, rank-positive, control-independent host allocations
whose lifetimes do not overlap. Exact type includes shape, while layout remains
the one fixed dense column-major convention. Scalars and guarded buffers retain
distinct allocations. A non-materialized slot is an expression fact and owns
no allocation. Mutation, aliases beyond immutable local references, layout
choices, views, and device memory remain barriers until their own typed facts
exist.

Source printers consume an ordered sequence of `TccqLoopNest` values built
from the lowered program: backend-neutral with-loops in the SAC lineage,
intermediates first, the result nest last. Each nest carries ordered
`TccqLoopAxis` values (`map` axes produce output positions, `reduce` axes fold
into an accumulator), a value-expression or typed-statement-block body whose
references carry `TccqExpressionReference` payloads. That payload separates the
expression value id from its logical source value id and owns its optional
symbol, lexical binding, slice offsets, and typed `TccqAccess` map of affine
`TccqIndexExpr` values. It does not identify physical storage; that remains a
`TccqStorageAllocation`. `TccqAccess` is also closed: a recycle access owns the
typed consumer shape whose axis order is linearized, while every other access
has no consumer shape. Each nest also carries an optional reducer with its
identity and an output access; intermediate nests additionally name the scalar
or temporary buffer their result materializes. `TccqExpression` has no open
metadata channel: every expression carries a typed effect, and an operation
expression carries the full `TccqLoweredOperation` that owns its resolution,
signature, domain policy, family, and reducer or contraction facts. Printers
follow that payload to `tccq_op_render()` rather than accepting a second
resolved-operation field.
Scalar expression programs, elementwise maps, full and per-axis reductions, contractions,
stencils, control-valued results, and scalar- or buffer-intermediate
compositions are all sequences of this one value, so C, Fortran, and Rtinycc
share a single generic per-nest emitter instead of per-family printer cases;
the C emitter emits one allocation and free per `TccqStorageAllocation`, while
Fortran declares one automatic array for each unconditional allocation and one
guarded allocatable array for each selected-path allocation. A branch-valued
nest body is a typed `TccqValueBlock` containing
`TccqAssignment` and `TccqConditional` statements over `TccqWriteTarget`
destinations. Both source families consume that block, preserving that exactly
one arm is evaluated.
When a printer reaches an operation node, it asks the resolved implementation
to render through `tccq_op_render()` for a `TccqOpRenderContext`.

The generated callable boundary is a second explicit plan value:
`TccqBackendFunctionInterface`. It records the source symbol, scalar or
loop-nest or structured shape, ABI, result identity and type, result placement, and the
loop-nest iteration domain. Each ABI parameter or scalar local is one
`TccqBackendValueBinding` containing its generated name, neutral value id, and
source type. Each `TccqBackendAllocationBinding` pairs one generated name with
one physical allocation and all materialized slots sharing it. Each
`TccqBackendExtentBinding` pairs a symbolic dimension with its generated ABI
parameter (`n` becomes `int extent_n`, deduplicated across arguments). Per-axis
loop index names, typed result dimensions, and the result element-count
parameter complete the interface. No mapping depends on synchronized parallel
vectors. C,
Rtinycc, and Fortran consume that same interface before concrete source is
emitted, and the generated R boundary wrappers bind each extent symbol from
the first argument shape that declares it, then check every other occurrence —
so `matrix_vector(x = double(n, p), w = double(p))` validates that `ncol(x)`
equals `length(w)` instead of assuming all arguments share one shape. An
unguarded C kernel may return an allocated pointer, while Fortran and a C
kernel with a typed error channel use caller-owned output arguments. That
choice is explicit in the function interface before any printer or FFI layer
runs, so status is inspected before a buffer is converted or copied.

Backend products are also explicit. `TccqBackendProducts` carries the typed
function interface, loop nest, expression tree or statement block, storage
plan, and concrete `TccqBackendArtifact` values. Source mode records a source
artifact carrying the generated source. Shared-library mode writes that same
source artifact plus a generated C `.Call` wrapper to disk, invokes
`R CMD SHLIB`, records the
resulting shared library as another artifact, and attaches a native callable
artifact when the wrapper symbol can be loaded. Rtinycc JIT mode keeps the same
source and function interface, then attaches a callable artifact after TinyCC
compilation. The architectural boundary is the typed expression, typed function
interface, typed storage plan, and typed product set.

## Control flow

Control flow is entering as structured IR. A pure scalar `if` with an explicit
`else` and identically typed arms becomes `TccqBranch`. The condition must be a
scalar logical. Generated logical values use R-compatible three-state integers
as C `int`, Fortran `integer(c_int)`, or TinyCC `i32`. Comparison
implementations preserve `NA` and `NaN`, and typed callable error channels turn
a missing control condition into `runtime.invalid_logical_condition` instead of
mapping it to false. The current loop-nest planner accepts the
branch as a result and lowers it into a `TccqValueBlock`. Pure branches may
nest in result arms because both source emitters recursively assign through the
same typed target. A branch used directly as another branch's condition or below a
pure elementwise operation first writes a block-owned target, and its consumer
reads that target through an ordinary typed access. `TccqWriteTarget` retains
the full semantic value type and separately records the scalar storage type
written in one loop iteration. `TccqBackendFunctionInterface` assigns stable
names to those locals; C compound blocks and Fortran `block` constructs declare
them at the owning statement block. Each block effect is the conservative union
of its statement effects, including control normalized below a pure operation.
`TccqBlock` owns lexical scope, ordered statements, and their exact effect
without claiming a result. `TccqValueBlock` is its stricter value-producing
subtype and explicitly names the write target produced on every terminal path.
A reducer or contraction whose operand contains control reads that block-local
target and combines it before the lexical block closes. A scalar branch-local
reduction or contraction is an intermediate `TccqLoopNest` carrying an ordered
`TccqLoopGuard` path. C and Fortran nest those guards around the same neutral
plan, including outer-to-inner nested branch paths. Branch-local array
intermediates use the same typed guard and storage-slot facts: C allocates a
nullable owned buffer inside the selected path, and Fortran allocates an
allocatable array there. Both clean up after the final consumer. When a prior
`TccqLocalBinding` owns the materialization schedule, later uses reuse that
schedule even across different consumer paths; without a definition boundary,
incompatible paths remain a classed loop-nest diagnostic.

Sequential recurrence uses abstract `TccqLoop` over a plain `TccqBlock`.
`TccqWhile` adds a condition evaluated before every iteration; `TccqRepeat`
enters the body directly. Loop-carried state is explicit `TccqCell` storage:
mutable by contract and distinct from immutable `TccqLocalBinding` definitions.
Conditions and assignments reuse `TccqExpression`, so the C and Fortran
printers consume one neutral program body and do not recover recurrence from
source names. `TccqLoopTransfer` records a validated `break` or `next` action
against its originating call semantics and always targets the nearest enclosing
loop. It is control completion rather than an ordinary effect, so a
transformation must not move work across it merely because its `TccqEffect` is
empty. This slice is scalar, requires cells to be initialized before loop
entry, and admits
procedural `TccqIf` statements whose arms are general typed blocks. A source
`if` without `else` has an explicit empty alternative. `TccqConditional` is the
stricter value-producing subtype used by expression loop nests; its retained
source branch may have a broader effect after guarded work has been extracted,
while the inherited statement effect remains exact for the normalized arms.
Sequential control does not use `TccqLoopNest`, which remains the data-parallel
iteration abstraction.
The structured body is the mutually exclusive body form of
`TccqProgramSchedule`; it does not compete with the schedule for ownership of
top-level order. Schedule construction verifies exact cell, local, and result
targets, graph-consistent expression and target types, and initialization
dominance before a backend sees the body.

`for`, `switch`, vectorized `ifelse`, and idiomatic R surfaces such as `Map`,
`lapply`,
`vapply`, `apply`, `Reduce`, and `Filter` are not just more function names to
whitelist. They describe regions, exits, dominance, carried loop state, effect
ordering, reducer legality, allocation, and possible boundary regions.

Scalar numeric comparisons are ordinary typed elementwise implementations, not
control nodes. Their signatures currently require scalar operands and return a
scalar logical value; this gives structured control a backend-neutral condition
without falsely promising vector comparison. The current C and Fortran
implementations preserve scalar numeric missingness in the logical result.

The intended representation is a small set of typed control nodes over the same
value/effect/domain model. A `for` loop over `1:n`, a `Reduce("+", x)`, and a
`sum(x)` can all become reduction regions when their reducer implementation,
identity, missing-value policy, and effects are known. `ifelse` is not ordinary
branching when it is vectorized; it is a select over a domain with recycling and
missing-value rules. `switch` is a dispatch boundary until the selector type
and case set are known. `break` and `next` are structured transfer statements,
not hidden `goto` strings; labeled and nonlocal transfers remain outside the
current model.

`TccqProgramSchedule` remains the sole top-level ordering boundary. Expression
programs use its linear SSA steps; sequential recurrence uses its typed body
form with an optional while-header condition, loop body, backedge through
cells, and explicit transfer statements; it does not become another mode hidden
inside `TccqLoopNest`.

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

Simple `f(g(x))` fusion is just one case. The first implemented case requires
one exact, adjacent lexical use; pure elementwise producer and consumer trees;
the same iteration shape; and effects that cannot write, allocate, cross a
boundary, error, or warn. Purity alone is not a reordering proof. Map-reduce,
stencil, tile, and device fusion are extensions of the same
domain/access/region model.

Fusion stops at explicit barriers: R call-evaluation boundaries, unknown or
observable effects, mutation, multiple or non-adjacent reads, control,
incompatible layouts, unmodeled missing-value semantics, illegal region
effects, or domain mismatches without an explicit broadcast/recycle rule.

## Diagnostics

Failure is a value. Phases return `TccqResult` values and attach
`TccqDiagnostic` objects. Conditions carry diagnostics as payloads. The goal is
not to make programs we cannot explain appear to compile; it is to make every
failure specific enough that the next typed concept to add is obvious.
