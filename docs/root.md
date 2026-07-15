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

The remaining north-star pressure is control and evaluator structure. Viterbi
and smaller probes around `if`, `switch`, loops, `repeat`, `break`, `next`,
replacement calls, dispatch, promises, side effects, and interruption should
fail with structured diagnostics until the compiler has typed facts for those
concepts. The goal is not to accept more R by fallback; it is to make each new
accepted program deepen the shared representation consumed by every backend.
