# Root Direction

The compiler core starts from declared R and turns parsed language objects into
typed S7 values. Declarations, call facts, value types, dimensions, effects,
diagnostics, operation implementations, backend plans, loop nests, and backend
products are compiler data first. Backend source exists only after those facts
are explicit enough that C, Rtinycc, and Fortran consume the same semantic
handoff.

The current useful center is the typed loop-nest path. Elementwise maps,
reductions, axis reductions, contractions, intermediate materialization,
dimension symbols, and rank-mixed recycling are represented before printing.
Each `TccqLoopNest` owns one typed materialized storage slot and reductions own
a separate typed scalar accumulator target. Generated local and intermediate
names are backend-interface bindings to those values; they are not loop-nest
attributes or operation-name conventions. The storage slot is the single owner
of a nest result's identity and type.
TinyCC and Fortran execution now both exercise composite kernels, including the
logistic-gradient apotheosis, so the next failures should come from semantics
that are not yet modeled rather than from target-specific source shortcuts.

The first control value is now real rather than a syntax exception. A pure R
`if` becomes `TccqBranch`, retains its special-form forcing semantics, joins
identically typed arms, and reaches C, TinyCC, and Fortran as a conditional
statement over a typed logical ABI parameter. The loop-nest planner converts
value-producing control into a neutral `TccqBlock` of typed assignments and
conditionals while retaining neutral expressions inside those statements. Pure
branches may occur in either result arm, directly as another branch's
condition, or below pure elementwise operations. Control-valued operands are
normalized left to right into block-owned write targets before their consumers.
Each target separates the full semantic value type from the scalar storage type
written in one loop iteration, and C and Fortran declare that storage in the
lexical source block that owns it. Each block names the target produced by every
terminal path, so reductions and contractions can consume conditional elements
inside the reducer scope. A reduction or contraction selected inside a
branch arm becomes an intermediate loop nest with an ordered typed guard path;
nested paths preserve outer-to-inner arm selection in every source backend.
The same path scopes materialized storage. A top-level assignment is a typed
`TccqLocalBinding` definition carrying its executable statement position, so
the planner schedules its non-fusible descendants before analyzing later uses.
A reduction defined before a branch is unguarded even when both arms consume
it; a conditional definition retains only its own guards. C represents guarded
arrays as nullable owned buffers, while Fortran uses guarded allocatable arrays.
Both allocate only in the selected definition path and clean up safely after
the final consumer. A shared materialization without a typed definition remains
a refusal when its uses imply incompatible paths.

The remaining north-star pressure is loop and evaluator structure. Viterbi and
smaller probes around `switch`, loops, `repeat`, `break`, `next`, replacement
calls, dispatch, promises, side effects, and interruption should fail with
structured diagnostics until the compiler has typed facts for those concepts.
The goal is not to accept more R by fallback; it is to make each new accepted
program deepen the shared representation consumed by every backend.
