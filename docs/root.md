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
TinyCC and Fortran execution now both exercise composite kernels, including the
logistic-gradient apotheosis, so the next failures should come from semantics
that are not yet modeled rather than from target-specific source shortcuts.

The first control value is now real rather than a syntax exception. A pure R
`if` becomes `TccqBranch`, retains its special-form forcing semantics, joins
identically typed arms, and reaches C, TinyCC, and Fortran as a conditional
statement over a typed logical ABI parameter. The loop-nest planner converts
value-producing control into a neutral `TccqBlock` of typed assignments and
conditionals while retaining neutral expressions inside those statements. Pure
branches may occur in either result arm or directly as another branch's
condition; the latter is assigned to an interface-owned typed scalar local
before the outer branch. Branch-local reductions and control nested inside an
ordinary operation stop with structured diagnostics until branch-local
scheduling and expression normalization exist.

The remaining north-star pressure is loop and evaluator structure. Viterbi and
smaller probes around `switch`, loops, `repeat`, `break`, `next`, replacement
calls, dispatch, promises, side effects, and interruption should fail with
structured diagnostics until the compiler has typed facts for those concepts.
The goal is not to accept more R by fallback; it is to make each new accepted
program deepen the shared representation consumed by every backend.
