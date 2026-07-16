# Conceptual Landscape and Roadmap

## Purpose

The governing principle of this project is conceptual control. It is not
necessary for one person to retain every implementation line, but it is
necessary to retain a precise model of what the compiler means, where each fact
is established, which transformations may change it, and which boundaries are
allowed to reject it. Generated code, benchmarks, and passing examples are not
substitutes for that model. They are evidence that the model survives contact
with execution.

This document is the map for that model. It is not an ADR log and does not try
to justify every past edit. It records the current thesis, the load-bearing
abstractions, the questions that should drive the next increments, and the
tests by which a new concept earns a place in the core.

## Project Thesis

`tccquickr` is a compiler core for a declared subset of R. The subset is narrow
by design, but its semantics must be honest. R source arrives as language
objects: symbols and constants are atoms, while operators, assignment,
indexing, replacement, blocks, control forms, and ordinary invocation are
represented as calls. The familiar C-like notation must not cause the compiler
to forget lazy arguments, lexical environments, replacement evaluation,
method dispatch, special forms, attributes, or R's array rules.

The compiler therefore starts with R semantics and progressively proves when a
program can use a simpler execution model. An opaque call remains a candidate
operation whose implementations may be supplied by R evaluation, the R C API,
pure C, Fortran, a device kernel, or another backend. Opacity does not itself
mean fallback, failure, or object mode. Backend feasibility is a conclusion of
analysis over typed facts, not a label attached during parsing.

The core is successful when target emission becomes mechanical. A backend
should receive types, shapes, effects, accesses, iteration domains, storage
decisions, bridges, and callable interfaces that have already been made
explicit. A printer should choose target spelling and ABI syntax. It should not
be the first component to discover what the program does.

## Semantic Map

The conceptual flow is:

```text
R language objects
  -> calls and evaluator semantics
  -> declared bindings, types, shapes, and literals
  -> typed program values and control/effect facts
  -> legality, regions, domains, accesses, and storage
  -> neutral expressions and ordered loop nests
  -> backend capability and bridge plans
  -> typed artifacts, source, libraries, wrappers, and callables
```

Each arrow is a contract boundary. Earlier layers preserve information; later
layers may consume or refine it but must not reconstruct it from operation
names or emitted strings. Failure at a boundary is a classed diagnostic with
typed context. A phase returns a typed value or `TccqResult`, never a message
that another phase must parse.

Several distinctions keep this map coherent. Semantic type is distinct from
physical representation: an R double array is not a C pointer, a Fortran
array, a `SEXP`, or device memory. Shape is distinct from layout: matrix and
array meaning currently comes from rank and dimensions, while dense contiguous
column-major layout is one physical convention. Operation meaning is distinct
from implementation: `sum` is a reduction contract, while pure C, Fortran, or
an R call are implementations with capabilities and effects. A region is a
legality boundary, not merely an emitter hint. A bridge is an explicit
representation transition, not wrapper code discovered after source emission.

## Load-Bearing Concepts

`declare(type(...))` establishes the accepted entry contract. Base storage
type, rank, symbolic dimensions, and special literals provide the first facts
on which inference can build. `raw` remains an R atomic vector type; `buffer`
is opaque compiler storage. Matrices and arrays remain shaped values rather
than separate nominal type families until a behavior demonstrates that rank
and dimensions are insufficient.

`TccqCall`, `TccqCallSemantics`, and `TccqCallIndex` retain the parsed call and
the evaluator properties that later phases must respect. `TccqOpSignature`
owns arity, result domain, and result type. Operation family values refine that
contract with elementwise, reduction, contraction, iteration, or subscript
facts. `TccqOpImpl` and its explicit implementation trait answer whether one
target can realize those semantics in one context. There is no global list of
allowed R spellings.

The implementation language follows the same boundaries. S7 classes own
compiler data and validate it at construction. S7 generics own behavior that
varies by compiler value. `s7contract` interfaces describe structural phase
protocols, while explicit traits mark behavior that an implementation must opt
into. Contracts become more precise as the program gains facts. A private
helper may connect these objects locally, but it must not become the hidden
owner of a type rule, legality rule, operation family, or phase protocol.

The typed program graph gives values stable identity. Effects, cells,
assignments, branches, loops, transfers, and indexed reads represent the
procedural subset without pretending it is pure SSA. `TccqAccess` and
`TccqIndexExpr` express how a value is read. `TccqLoopNest` is the common
iteration handoff for scalar, map, reduction, contraction, stencil, and
materialized intermediate work. Storage plans describe materialization,
lifetime, alias constraints, memory space, and reuse independently of target
declarations.

Backend specs and plans consume this semantic core. Function interfaces,
bridges, neutral bodies, loop nests, storage plans, source, libraries, wrappers,
and callables are typed products. Generic C, Rtinycc/TinyCC, and quickr-style
Fortran are valuable precisely because agreement among different target
families makes backend-specific cheating harder.

## Conceptual Landscape

R itself supplies the frontend representation. `codetools` is sufficient for
structural discovery while parsed language objects remain the authority for
transformation. A source editor or concrete syntax tree solves a different
problem and would add dependency and conceptual weight without improving the
semantic model.

SAC contributes the strongest array lesson: reason about complete array
domains and with-loops before lowering to scalar target code. That supports
fusion, tiling, parallel placement, and storage decisions at the level where
they are meaningful. The project should borrow this semantic stance without
copying a mature compiler's full machinery before a local program demands it.

Hermes and Static Hermes provide pressure for explicit phases, verifier-backed
IR boundaries, control-flow integrity, effects, and progressive typing. Numba
demonstrates useful specialization from inferred types and the practical need
for multiple execution strategies. Its object mode should be understood here
as one possible backend implementation, not as the semantic category for every
operation the native subset does not yet know.

Quickr matters because its Fortran path exposes assumptions that a C-only core
could hide. Anvl is relevant for operation declarations and implementation
selection. Simple-Rust is useful as a compact reference for graph construction,
optimization, and code generation, but an R compiler cannot inherit its type
or evaluation model merely because its implementation is approachable.

Porffor is useful evidence that an intentionally incomplete dynamic-language
AOT compiler can still explore serious native execution. Its explicit semantic
analysis, typed runtime categories, precompiled built-ins, low-level IR, and
IR-to-C path are worth studying. It also supplies a warning: when most effort
accumulates in target-oriented code generation, semantic decisions can become
embedded in instruction construction. `tccquickr` should preserve a stronger
neutral handoff so that C is one consumer rather than the shape of the core.

## Near-Term Roadmap

The next horizon is semantic closure for the already declared subset. Calls,
bindings, loops, `if`, positional `switch`, reductions, contractions, exact
indexed reads, and affine slices now have typed footholds. The next increments
should be chosen by programs that expose a missing semantic fact, not by syntax
coverage counts. Dynamic indexing, replacement calls, richer argument
matching, views, mutation, promises, dispatch, and lexical capture should enter
only with explicit evaluator and effect consequences.

Type and shape work should move from local result rules toward a small
constraint system. It must explain symbolic equality, affine extents,
rank-polymorphic signatures, R recycling, zero extents, scalar promotion,
special-value propagation, and result inference. A constraint is useful when
it can produce either a proved fact consumed downstream or a diagnostic that
identifies the unresolved relation. A growing collection of predicates is not
a solver.

Control and effects must become strong enough to support optimization. The
compiler needs explicit answers for dominance, initialization, loop-carried
state, early transfer, call purity, allocation, mutation, aliasing, promise
forcing, environment access, and boundary effects. This does not require a
generic framework in advance. The first control-flow or pass abstraction
should arrive with a transformation that uses it and a verifier that can reject
an invalid result.

The first optimization pipeline should operate entirely on typed neutral
values. Canonicalization and constant folding can simplify facts without
changing domains. Dead-value elimination and common-subexpression elimination
require stable identity and effect proofs. Fusion must compare iteration
domains, access maps, reducer boundaries, effects, and materialization needs;
it is not merely recognizing `f(g(x))`. Storage reuse follows liveness,
compatibility, aliasing, layout, memory space, and escape behavior. Loop
interchange, tiling, parallelization, and device placement come later because
they require the same facts plus target capability and cost information.

Backend planning should then become a legality and selection problem over
those neutral artifacts. A backend declares supported representations,
operations, effects, regions, control constructs, memory spaces, and bridge
kinds. Legalization either produces a typed backend plan or explains the first
unsupported fact. New backends such as CUDA, Mojo, a graph IR, or R evaluation
enter only with one real lowering and an end-to-end probe. Descriptor-only
backends create false confidence and are excluded.

Interrupts, debugging, profiling, caching, and source maps belong after the
execution model can carry them honestly. A safepoint must identify where
polling is legal and an emitter must write the poll. A debug site must map a
neutral operation or control point to generated code. A cache key must include
the semantic program, backend capabilities, ABI, and relevant runtime state.
Passive metadata for future behavior would only make plans look more complete
than they are.

## Executable Milestones

Development should alternate between minimal probes and composite programs. A
minimal probe isolates one law: repeated symbolic extents, a loop-carried
maximum, an affine access, a non-fusible effect, an alias barrier, a reduction
finalizer, or a representation bridge. The probe should assert typed IR and a
specific structured failure before it asserts source text. Once the fact is
represented, the same neutral artifact should be exercised by at least two
materially different backend families.

The Viterbi program is the primary control, indexing, and state pressure test.
It requires shaped score tables, nested iteration, dynamic predecessor reads,
max and argmax state, backpointer writes, boundary initialization, and a
traceback phase. Logistic-gradient code pressures elementwise fusion,
reductions, matrix operations, broadcasting, and temporary reuse. Stencils
pressure affine interior domains and halo legality. Small map, reduce,
contraction, diagonal, and zero-extent probes remain necessary because a large
program can obscure which proof is absent.

The useful measure of progress is that these programs fail later and more
precisely. Moving a failure from frontend string policy into shape inference,
effect legality, storage planning, or backend capability is progress when the
new diagnostic corresponds to a real missing proof. Emitting more C while the
core cannot explain the program is not.

## Immediate Experiments

The first experiment is now established: `TccqProgramOptimization` is an
explicit transformation-and-verification trait, and
`TccqDeadStoreOptimization` is its first implementation. It removes only
provably unobserved assignments from structured neutral control, rebuilds
typed blocks and region facts to a fixed point, and verifies the exact expected
program at the compiler boundary before backend planning. It intentionally
preserves the source graph and storage plan rather than pretending to perform
graph-level dead-value elimination. This is enough to establish the
program-to-program boundary without inventing a pass manager before pass
composition exists.

The second experiment should generalize proven access beyond an iterator cell
without claiming full R indexing. An affine scalar selector can be admitted
when range constraints prove every access in bounds. This will test whether
dimension constraints, domains, and accesses compose cleanly before mutation
or views complicate ownership.

The third experiment should make one nontrivial fusion and one deliberate
fusion refusal executable. The accepted case should eliminate a materialized
temporary across compatible elementwise domains. The refused case should be
blocked by a typed reduction, effect, alias, or domain fact. Storage lifetime
and reuse should then be derived from the transformed program rather than
encoded by a printer.

The fourth experiment should carve the smallest Viterbi recurrence that forces
max-with-argmax state and a backpointer write. It need not compile the complete
algorithm. Its purpose is to identify whether the next missing concept is a
reducer with structured state, dynamic access proof, mutation region, or
multi-result operation. The answer should determine the next schema change.

## Decision Discipline

A concept enters the core when it is required to state R semantics, shared by
multiple operations or phases, forced by divergent backends, or needed to
explain an apotheosis diagnostic. It should arrive with a constructor boundary,
typed behavior, an invariant, and an executable consumer. Otherwise it remains
a local implementation detail or a documented question.

This rule excludes passive schemas, fake compatibility layers, target-shaped
IR, operation-name switches in printers, generic metadata bags, helper-owned
type systems, and source-substring tests as architectural evidence. It also
prevents the opposite failure: building a universal compiler framework before
the declared R subset has supplied concrete semantic pressure.

The roadmap itself should change when the conceptual map changes. Completed
experiments belong in tests and current architecture documentation, not in an
ever-growing decision diary. The durable artifact is a small set of concepts
that a maintainer can explain, challenge, and use to predict the consequences
of the next change.
