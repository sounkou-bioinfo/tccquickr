
# tccquickr

`tccquickr` is being hard-reset as a typed R transformation core.

The new root is declared R programs, not source editing. Compiler values
are S7 objects, compiler protocols are expressed with `s7contract`
interfaces and traits, and failures are classed diagnostics rather than
string checks. Shape, layout, effects, regions, operation
implementations, and fusion candidates are modeled before a backend
printer decides syntax. Backend planning is now a typed layer too:
Rtinycc is represented as one backend descriptor for a C/TinyCC path,
not as the compiler architecture. Minimal C, Fortran, and Rtinycc paths
now consume a lowered program for a tiny elementwise subset, but the
semantic contract remains the R-level typed program. Fake backward
compatibility is not part of the reset.

## Core model

The compiler state is a typed value graph, not source text. `TccqType`
combines a base type with a `TccqShape`; the shape carries rank and
symbolic dimensions, so rank 2 is a matrix and rank N is an array.
`TccqLayout` records physical order, strides, offset, and contiguity,
while `TccqTile` records rectangular partition metadata. `TccqLiteral`
gives finite values, typed `NA`, `NaN`, `Inf`, and `-Inf` their own
representation. `TccqValue` is the IR value: it names an operation,
inputs, type, effects, layout, tile, and attributes. `TccqProgram`
collects formals, values, regions, result, and diagnostics.

The split is intentional. Shape says what a value means. Layout says how
it is stored. Tile says how it is partitioned. None of those should be
hidden inside target-specific code generation.

## Call root

The semantic root is that R programs are language objects made of calls,
names, constants, promises, and environments. The C-like surface syntax
is notation. Parsed assignment is a call to `<-`; indexing is a call to
`[`, `[[`, or `$`; subset replacement is parsed as assignment over an
indexing call and evaluates through a replacement function such as
`[<-`, `[[<-`, `$<-`, or another replacement function; arithmetic and
logical operators are calls with operator names; blocks are calls to
`{`; grouping is a call to `(`; flow control is represented as calls
such as `if`, `for`, `while`, `repeat`, `break`, and `next`; function
definitions are calls that construct closures.

This call-root view does not erase R’s evaluator. Some calls are special
forms with non-standard promise forcing, some are builtins, some are
closures with lexical environments, and some dispatch through S3, S4, or
S7. The compiler’s first job is to preserve those distinctions as typed
call facts rather than falling back to a C-shaped syntax tree. Only
after the call has a signature, effect, forcing policy, dispatch model,
domain, and region can a backend claim that it implements the call.

That means `TccqCall` is not a narrow frontend convenience. It is the
first normal form of the program. A call records its name, structural
kind, arity, argument tags, source expression, and attributes. Later
passes may attach signatures, promise-forcing rules, lexical environment
requirements, dispatch rules, effects, domains, implementations, and
backend feasibility. An opaque call is still an operation candidate; it
is not automatically R call evaluation and it is not illegal merely
because we do not yet know how to lower it.

The next layer is `TccqCallSemantics`. It records facts the evaluator
would care about before any backend exists: whether the call resolves to
a special form, builtin, closure, compiler directive, or unknown
function; whether arguments are forced eagerly, lazily, specially,
through replacement evaluation, or by the compiler; whether dispatch is
ordinary, S3, primitive S3, group-generic, or replacement dispatch; and
whether lexical scope is part of the call contract. Frontend analysis
stores both observed calls and these semantics in a `TccqCallIndex`,
then attaches that typed index to `TccqProgram@call_index` so lowering
has checked facts available rather than digging through untyped
attributes. The current expression lowerer still reconstructs value
dependencies from the function body for a very small subset, but
operation availability is no longer a yes/no string predicate. Each
lowerable call is resolved to a typed implementation record before a
value is created.

## Operations

Function/operator support is contextual. The frontend does not own a
fixed allowed/not-allowed list. It asks a typed registry. A `TccqOpImpl`
describes one implementation of an operation, and that implementation
must explicitly opt into the `TccqOpImplementation` trait. A
`TccqOpRegistry` is the visible set of implementations, and
`TccqOpContext` describes the requested target, region kind, memory
space, and whether R C API or boundary implementations are allowed.

An operation can be supported by several implementations: R C API
evaluation, pure C, Fortran, Mojo, CUDA/device code, or an explicit
boundary. Those choices are different because they have different
effects and region constraints.

The operation registry answers implementation questions, not grammar
questions. If a call has no implementation in the current registry and
context, analysis returns a classed `frontend.unimplemented_call`
diagnostic. That diagnostic says what is missing from the compiler
model; it does not mean the call is not R.

`TccqResolvedOp` is the handoff from registry query to lowering. It
records the observed call, selected `TccqOpImpl`, target, region kind,
memory space, purity, boundary status, R C API usage, and effect.
`TccqLoweredOperation` is the next payload: it keeps the lowered family,
resolved operation, signature, domain policy, and optional reducer
identity together on the operation value. Fusion, storage, and backend
planning consume that typed payload instead of rediscovering semantics
from `TccqValue@op` or a handful of loose attributes. Operation
implementations may also expose a source renderer through
`tccq_op_render()`. That renderer is an implementation capability, not a
backend-local switch over operation strings.

`TccqOpSignature` is the shared operation contract for arity,
result-domain policy, and result typing. It is deliberately smaller than
a full type system, but it is the right owner for facts such as “`sqrt`
accepts one input, uses the common elementwise domain, and returns
double” or “this reducer accepts one array expression and returns a
scalar”. A `TccqDomainPolicy` computes the result shape from input types
before target source generation, so scalar broadcast, common elementwise
domains, and scalar reducer results are typed operation facts rather
than hidden closure checks. Elementwise, reduction, and future operation
families carry signatures rather than each growing private arity
predicates, shape predicates, and result-type helpers.

Elementwise calls follow operation metadata too. A call lowers as
elementwise only when the resolved operation carries a
`TccqElementwiseSpec`, which carries a `TccqOpSignature`. The default
registry provides a small numeric elementwise surface for arithmetic,
unary negation, powers, `sqrt`, and `exp`, but another registry can add
a source-rendered call such as `square(x)` without changing the lowerer
or printers.

Reducers follow the same rule. A reducer is not recognized because the
lowerer knows the text `sum`; a call lowers as a reduction only when the
resolved operation carries a `TccqReductionSpec`. That spec carries a
`TccqOpSignature` for the call shape, result-domain policy, and result
type, then adds the identity literal and accumulator combine expression
needed by source printers. The default registry models base `sum` as one
such implementation, and another registry can model a different fold
surface through the same contract without changing the lowerer or the C
and Fortran printers.

## Current lowering boundary

The first lowering pass is deliberately small. It handles scalar and
rank-N contiguous elementwise expressions, full-domain rank-N
reductions, and rank-2 single-axis reductions such as `colSums()` and
`rowSums()` when the resolved operations carry the corresponding typed
specs. Source backends currently linearize non-scalar elementwise and
reduction domains as contiguous buffers while preserving rank and shape
in `TccqType`, `TccqDomain`, bridge plans, and backend products. It
returns a `TccqLoweringPlan`, then embeds the plan into `TccqProgram` as
values, regions, fusion groups, and storage facts. Each operation value
carries one `TccqLoweredOperation` payload that owns the shared
`TccqOpSignature`, result-domain policy, selected implementation, and
optional reduction facts used to type the call. The generated fusion
group preserves operation signatures and domain policies, then derives
its target and effect from the resolved operations. Later fusion,
access, legality, storage, and backend passes should consume those
contracts rather than infer operation behavior from names, ranks, or
emitted source. This is not a general legality pass and it is not the
place where new language coverage should sprawl.

Top-level local assignment is currently modeled as single-assignment
binding. In `a <- expr`, the local symbol `a` becomes a name for the
lowered value of `expr`; it is not treated as mutation. Rebinding a
local name, rebinding a formal, or mutating through a formal such as
`x[i] <- value` produces a classed lowering diagnostic. That is
intentionally strict until mutation barriers, aliasing, materialization,
and view semantics are represented in the middle-end.

The important constraint is that lowering failure is not frontend
failure. A registered opaque operation can be a valid analyzed operation
even when the current lowerer cannot emit it. Backend planning then
reports that no lowered program is available for the requested backend.
This preserves the difference between “R operation not modeled”,
“operation modeled but not lowerable here”, and “operation lowerable but
unsupported by this backend”.

The minimal storage plan marks formals, literals, temporaries, and the
output explicitly so printers can consume a typed plan. It records each
slot’s definition and last use as a `TccqStorageLifetime`, then only
groups same-type temporary slots for reuse when those typed lifetimes do
not overlap. This is still conservative: aliasing, mutation,
materialization, layout, views, and device memory need explicit passes
before storage reuse can become aggressive.

Source printers now consume a `TccqExpression` tree built from the
lowered program result. That tree preserves reference leaves, literal
leaves, operation nodes, result types, and selected operation
implementations. C, Fortran, and Rtinycc therefore share one neutral
expression handoff before syntax printing; the printers should not walk
the raw value graph or rediscover operation semantics themselves. When a
printer reaches an operation node, it asks the resolved implementation
to render through `tccq_op_render()` for a `TccqOpRenderContext`.

The generated callable boundary is a second explicit plan value:
`TccqBackendFunctionInterface`. It records the source symbol,
scalar/map/reduction/axis-reduction shape, ABI, parameter names, lowered
parameter value ids, result value id, result placement, iteration
domain, per-axis input extent parameter names, total input element-count
name, per-axis result extent names, result element-count name, index
name, and reduction accumulator name needed by generated loops. C,
Rtinycc, and Fortran consume that same interface before concrete source
is emitted. A C map or axis-reduce kernel can return an allocated
pointer while a Fortran `bind(c)` map or axis-reduce kernel can expose
an output argument, but that difference is already explicit in the
function interface before either printer runs.

Backend products are also explicit. `TccqBackendProducts` carries the
typed function interface, expression tree, storage plan, and concrete
`TccqBackendArtifact` values. Source mode records a source artifact
carrying the generated source. Shared-library mode writes that same
source artifact plus a generated C `.Call` wrapper to disk, invokes
`R CMD SHLIB`, records the resulting shared library as another artifact,
and attaches a native callable artifact when the wrapper symbol can be
loaded. Rtinycc JIT mode keeps the same source and function interface,
then attaches a callable artifact after TinyCC compilation. The
architectural boundary is the typed expression, typed function
interface, typed storage plan, and typed product set.

## Control flow

Control flow has to become structured IR. `for`, `while`, `repeat`,
`break`, `next`, `switch`, scalar conditionals, vectorized `ifelse`, and
idiomatic R surfaces such as `Map`, `lapply`, `vapply`, `apply`,
`Reduce`, and `Filter` are not just more function names to whitelist.
They describe regions, exits, dominance, carried loop state, effect
ordering, reducer legality, allocation, and possible boundary regions.

The intended representation is a small set of typed control nodes over
the same value/effect/domain model. A `for` loop over `1:n`, a
`Reduce("+", x)`, and a `sum(x)` can all become reduction regions when
their reducer implementation, identity, missing-value policy, and
effects are known. `ifelse` is not ordinary branching when it is
vectorized; it is a select over a domain with recycling and
missing-value rules. `switch` is a dispatch boundary until the selector
type and case set are known. `break` and `next` are structured exits,
not hidden `goto` strings.

## Regions and bridges

Executable code is grouped into `TccqRegion` values. A region records
where code may run and what effects are legal. A `host` region is
ordinary host orchestration. A `kernel` region is an R-API-free scalar
or array kernel. A `parallel` region is an R-API-free parallel kernel. A
`device` region is device-side code using device memory.

R C API evaluation is the equivalent of Numba object mode: it should
always be available as an explicit boundary implementation, but it is a
boundary. It touches the R API and therefore cannot silently appear
inside pure, parallel, or device regions.

Bridges are a typed backend-plan layer. They represent transitions such
as `SEXP -> scalar`, `scalar -> SEXP`, `SEXP -> C buffer`,
`C buffer -> SEXP`, `host -> device`, `device -> host`, layout
conversion, tile materialization, and explicit R call-evaluation
boundaries. These are plan values, not target-side string glue, and
their kind must match the shape of the value being bridged.

## Backend planning

Backends are implementation choices behind a trait. A `TccqBackendSpec`
describes a backend family, target, driver, supported modes, region
kinds, memory spaces, capabilities, and a planning function. The
compiler calls the `TccqBackend` trait and receives a `TccqBackendPlan`,
not emitted code. The plan contains assigned regions, bridges,
safepoints, debug sites, diagnostics, capabilities, and backend
attributes.

Rtinycc comes for free only in the sense that it is one current driver
for C source and TinyCC JIT modes. It should not decide the IR. The
generic C descriptor, the Rtinycc descriptor, and the quickr-style
Fortran descriptor now print source from the same lowered elementwise
program through the same `TccqExpression` and
`TccqBackendFunctionInterface` handoffs, and source, shared-library, and
JIT products are recorded as `TccqBackendArtifact` values. The
anvil-style graph/StableHLO/XLA descriptor and the R call-evaluation
descriptor still report typed planning diagnostics until their
corresponding lowerings exist. Those different families pressure the
same typed representation from different directions, reducing the chance
that the IR is secretly reward-hacked for one concrete backend.

Runtime concerns are part of backend planning. `TccqRuntimePolicy` says
whether we are in release, checked, trace, or debug mode; whether
interrupts may be observed; and how often generated loops or chunks
should poll. `TccqSafepoint` marks region entries, loop backedges, tile
boundaries, reduction chunks, boundary calls, and debugger stops.
`TccqDebugSite` keeps source and IR metadata attached to generated code
so debugging is a backend-plan feature, not an afterthought in emitted
C.

## Fusion

Fusion is a transformation over a typed value graph. It is not a source
rewrite and not a backend shortcut. `TccqDomain` names the iteration
space, `TccqAccess` describes how a value maps onto that domain, and
`TccqFusionGroup` represents a candidate fused group over a domain,
values, outputs, accesses, target, region kind, and effects.
`TccqFusionContract` records the lowered operation payloads, operation
signatures, domain policies, result operation, and storage strategy for
that group.

Simple `f(g(x))` fusion is just one case: a pure single-use producer and
pure consumer over the same domain with compatible implementations and
domain policies. Map-reduce, stencil, tile, and device fusion are
extensions of the same domain/access/region model.

Fusion stops at explicit barriers: R C API/object-mode calls, unknown
effects, mutation, incompatible layouts, unmodeled missing-value
semantics, illegal region effects, or domain mismatches without an
explicit broadcast/recycle rule.

## Diagnostics

Failure is a value. Phases return `TccqResult` values and attach
`TccqDiagnostic` objects. Conditions carry diagnostics as payloads. The
goal is not to make programs we cannot explain appear to compile; it is
to make every failure specific enough that the next typed concept to add
is obvious.

## Apotheosis suite

The current targets are not hidden package functions. They are explicit
apotheosis examples: small probes that exercise one compiler idea at a
time, and larger programs that force those ideas to compose. Rank-N
elementwise maps, full-domain rank-N map-reduce probes, and rank-2
single-axis sum probes now pass through typed IR and source planning,
while the remaining probes are still expected to fail with structured
diagnostics. Making each failure move deeper through the same typed IR
is the first serious milestone.

## Minimal probes

These examples are intentionally small. Each unsupported probe should
move from structured failure to typed IR before we add more surface
area.

``` r
map_chain <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  exp(sqrt(x) + y)
}

map_reduce <- function(x, y) {
  declare(type(x = double(n), y = double(n)))
  sum(exp(x) * y)
}

matrix_reduce <- function(x, y) {
  declare(type(x = double(n, p), y = double(n, p)))
  sum(exp(x) * y)
}

matrix_map <- function(x, y) {
  declare(type(x = double(n, p), y = double(n, p)))
  sqrt(x) + y
}

matrix_vector <- function(x, w) {
  declare(type(x = double(n, p), w = double(p)))
  x %*% w
}

raw_buffer_roundtrip <- function(bytes, scratch) {
  declare(type(bytes = raw(n), scratch = buffer(n)))
  bytes
}

tiled_stencil_1d <- function(x) {
  declare(type(x = double(n)))
  x[1:(n - 2L)] + x[2:(n - 1L)] + x[3:n]
}

control_flow_probe <- function(x, flag) {
  declare(type(x = double(n), flag = logical()))
  out <- 0
  i <- 1L
  repeat {
    if (i > n) {
      break
    }
    out <- out + ifelse(flag, x[i], -x[i])
    i <- i + 1L
  }
  switch(if (flag) "sum" else "neg", sum = out, neg = -out)
}

apply_reduce_probe <- function(x) {
  declare(type(x = double(n, p)))
  Reduce(`+`, lapply(seq_len(p), function(j) sum(x[, j])))
}
```

## Composite targets

The larger examples force shapes, domains, layouts, reductions, regions,
operation implementations, bridges, and fusion planning to interact.

``` r
logistic_gradient <- function(x, y, w, lambda) {
  declare(type(
    x = double(n, p),
    y = double(n),
    w = double(p),
    lambda = double()
  ))

  mu <- colMeans(x)
  sigma <- sqrt(colSums((x - mu)^2) / (n - 1L))
  z <- (x - mu) / sigma
  eta <- z %*% w
  prob <- 1 / (1 + exp(-eta))
  grad <- crossprod(z, prob - y) / n + lambda * w
  w - 0.01 * grad
}

viterbi_decode <- function(init, trans, emit) {
  declare(type(
    init = double(k),
    trans = double(k, k),
    emit = double(t, k)
  ))

  score <- init + emit[1L, ]
  back <- matrix(0L, nrow = t, ncol = k)
  for (step in 2:t) {
    next_score <- score
    for (state in 1:k) {
      candidate <- score + trans[, state]
      best <- which.max(candidate)
      next_score[state] <- candidate[best] + emit[step, state]
      back[step, state] <- best
    }
    score <- next_score
  }
  list(score = score, back = back)
}

tccq_analyze(logistic_gradient)
tccq_compile(logistic_gradient)

tccq_analyze(viterbi_decode)
tccq_compile(viterbi_decode)
```

The expected state is structured failure: calls without implementations
should be reported as classed diagnostics until the frontend, typed IR,
region planner, operation implementations, bridges, and backend
feasibility checks can account for them honestly.
