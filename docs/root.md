# Root Direction

The compiler core starts from declared R and turns parsed language objects into
typed S7 values. Declarations, call facts, value types, dimensions, effects,
diagnostics, operation implementations, backend plans, loop nests, and backend
products are compiler data first. Backend source exists only after those facts
are explicit enough that C, Rtinycc, and Fortran consume the same semantic
handoff.

Neutral expressions are closed typed values rather than metadata bags. Every
`TccqExpression` owns its effect, references own
`TccqExpressionReference`, and operation nodes own the complete
`TccqLoweredOperation` selected by lowering. The latter is the one path to the
resolved implementation used by source printers. A recycling `TccqAccess` owns
its `TccqShape` consumer order directly; access metadata is not a secondary
shape protocol.

Composite operation definitions now enter the same handoff instead of adding a
new backend convention. `TccqOpBody` owns one typed call index and one
parameterized R expression. Lowering expands it through the registry, checks
its signature result and declared effect envelope, and returns the resulting
primitive value graph. C, Rtinycc, and Fortran therefore compile the same
expanded expression and never recognize the composite operation's spelling.
Bodies are currently pure elementwise expressions without control,
replacement, special forcing, free-symbol capture, defaults, or `...`; richer
body structure must first acquire a typed middle-end representation.

The current useful center is the typed loop-nest path. Elementwise maps,
reductions, axis reductions, contractions, intermediate materialization,
dimension symbols, and rank-mixed recycling are represented before printing.
Each `TccqLoopNest` owns one typed materialized storage slot and reductions own
a closed `TccqReductionPlan`, including typed state with one or more scalar
components. Generated local and intermediate names are backend-interface
bindings to those values; parameters and scalar locals bind name, value
identity, and type together, allocation bindings own
all slots sharing one physical allocation, and extent bindings pair one
dimension symbol with one ABI name. None of these mappings is a parallel-array
convention. The storage slot is the single owner
of a nest result's identity and type. A materialized temporary slot additionally
owns a `TccqStorageAllocation`, and non-overlapping same-typed buffer lifetimes
may share that physical identity. C, TinyCC, and Fortran consume the same
allocation identity; reuse is not reconstructed from generated names.
TinyCC and Fortran execution now both exercise composite kernels, including the
logistic-gradient apotheosis, so the next failures should come from semantics
that are not yet modeled rather than from target-specific source shortcuts.

The first control value is now real rather than a syntax exception. A pure R
`if` becomes `TccqBranch`, retains its special-form forcing semantics, joins
identically typed arms, and reaches C, TinyCC, and Fortran as a conditional
statement over a typed logical ABI parameter. The loop-nest planner converts
value-producing control into a neutral `TccqValueBlock` of typed assignments and
conditionals while retaining neutral expressions inside those statements. Pure
branches may occur in either result arm, directly as another branch's
condition, or below pure elementwise operations. Control-valued operands are
normalized left to right into block-owned write targets before their consumers.
Each target separates the full semantic value type from the scalar storage type
written in one loop iteration, and C and Fortran declare that storage in the
lexical source block that owns it. Each block names the target produced by every
terminal path, so reductions and contractions can consume conditional elements
inside the reducer scope. Every neutral reference has a
`TccqExpressionReference`: the logical source value, optional lexical binding
and slice, and eventual typed domain access are one payload rather than
expression attributes. This source identity is not a physical allocation and
cannot be used as a storage-reuse hint. A reduction or contraction selected
inside a branch arm becomes an intermediate loop nest with an ordered typed
guard path;
nested paths preserve outer-to-inner arm selection in every source backend.
The same path scopes materialized storage. `TccqProgramSchedule` records every
top-level evaluation in contiguous statement order, including unbound
expression statements, and assignment steps own their typed `TccqLocalBinding`.
The planner schedules each step's non-fusible descendants before analyzing
later uses. A reduction defined before a branch is unguarded even when both arms
consume it; a conditional definition retains only its own guards. C represents
guarded arrays as nullable owned buffers, while Fortran uses guarded allocatable
arrays. Both allocate only in the selected definition path and clean up safely
after the final consumer. A shared materialization without a typed definition
remains a refusal when its uses imply incompatible paths.

Sequential recurrence now has a separate honest representation. Abstract
`TccqLoop` owns the typed body shared by concrete `TccqWhile` and `TccqRepeat`
statements. `TccqFor` adds a scalar iteration cell and one
`TccqIterationPlan`. That plan evaluates its source once, owns a rank-1
`TccqDomain`, and represents the current element either as a stored-vector
`TccqExpression` with `TccqAccess` or as a virtual affine `TccqIndexExpr`.
Virtual iteration is selected through typed operation metadata rather than a
printer spelling. The first such implementation is `seq_len(n)` for a declared
dimension `n`; its source is a `TccqDimensionReference`, so a scalar binding
named `n` cannot masquerade as that extent. `TccqIterationSpec` owns its unary
signature, extent argument, and first value. Its iterator is
initialized inside the body but not after a potentially empty domain.
Within exact virtual iterations, a scalar extraction with one selector per
source axis is a `TccqIndexedValue`, not an untyped special case. It retains
ordered iterator targets, selector cells, iteration proofs, the selected
subscript implementation, evaluator semantics, and one typed `extract`
access. Every source dimension must match its selector's rank-1 iteration
domain. The access domain owns the unique iteration axes, so a diagonal read
can map one axis onto multiple source dimensions without pretending there are
multiple loops. `TccqArity` represents the subscript's open argument-count
contract without imposing a fake maximum rank. Assignment to a selector
iterator invalidates that proof for the complete body, and stored-vector
iteration never creates it. Unproven selectors, rank mismatches, and per-axis
dimension mismatches remain diagnostics. Tagged subscript arguments, including
`drop`, also remain diagnostics until their R argument-matching semantics are
modeled. C, TinyCC, and Fortran consume the same neutral indexed expression
through their column-major access linearizers.
Loop-carried variables become mutable `TccqCell` storage rather
than fake SSA bindings or a special `TccqLoopNest` mode. `TccqLoopTransfer`
represents nearest-loop `break` and `next` completion without pretending it is
a read/write effect. `TccqControlCompletion` keeps normal, break, and continue
outcomes separate from effects. Blocks compose those facts in evaluation order,
nested loops consume their own transfers, and initialization joins consider
only arms that can reach the next statement. Cell reads use the same neutral expressions as the array
path, assignments carry write effects, and nested procedural branches are
`TccqIf` statements over typed arm blocks. `TccqConditional` is its stricter
value-producing subtype; the shared parent keeps printers independent of that
distinction while exact local effects remain separate from the retained source
branch effect. Positional statement `switch` is `TccqSwitch`; it owns one
scalar integer selector expression, a typed local selector target with stable
identity, ordered typed arm blocks, and evaluator semantics. The shared function
interface plans one local selector binding, so every backend evaluates it
exactly once.
Unmatched positions retain normal completion, while arm `break` and `next`
still target the surrounding R loop. C uses ordered conditional arms rather
than a native C `switch`, whose `break` semantics would be wrong here.
Character selection, numeric-double coercion and warning behavior, missing
alternatives, and value-producing `switch` are not yet accepted. C, TinyCC,
and Fortran consume the same structured program body.
That body is the mutually exclusive
structured form of `TccqProgramSchedule`, preserving one owner for top-level
order and result identity. A scalar cell can be first defined inside a loop
when the definition dominates every normally reachable read; definitions are
not assumed after a loop that may execute zero times. Numeric
comparisons preserve missing logical results, and every generated control test
reports them through a typed callable error channel; labeled or nonlocal exits,
arbitrary scalar extents, range/list/computed `for` iterables, and array-carried
state remain explicit gaps. A
controlled array result
uses caller-owned output storage, so C, Rtinycc, and Fortran inspect that error
channel without first converting a returned buffer pointer.

The first schedule-aware fusion is deliberately narrow. A local elementwise
definition can remain an expression only when it has one exact lexical read in
the immediately following elementwise evaluation, both trees have the same
iteration shape, and their typed effects permit reordering. Warning and error
effects are barriers, so `sqrt` remains eager and materialized while a silent
`exp` chain can become one nest. The storage slot's `materialized` field is the
single optimization decision consumed by the neutral loop planner; C, TinyCC,
and Fortran do not rediscover it.

The first storage-reuse decision is similarly narrow. Lifetime propagation
derives order from the typed program schedule and accounts for complete
consumer expressions and lexical binding references.
Only exact-type host buffers outside control paths can share one typed physical
allocation, and only when the earlier lifetime ends before the later
definition. Directly dependent or simultaneously live buffers remain distinct.

The remaining north-star pressure is richer loop and evaluator structure.
Viterbi and smaller probes around value-producing and character `switch`,
richer `for` iterables, replacement calls, dispatch, promises, side effects,
and interruption should fail with structured diagnostics until the compiler
has typed facts for those concepts.
The accepted direct-vector and declared-extent unit-sequence `for` slices do
not yet cover arbitrary scalar extents, general ranges, lists, or computed
iterables.
The goal is not to accept more R by fallback; it is to make each new accepted
program deepen the shared representation consumed by every backend.
