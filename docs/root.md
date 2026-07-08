# Root Direction

The compiler core starts from values with schemas:

1. Parse declarations from a deliberately narrow R subset.
2. Represent formals, values, effects, and diagnostics as S7 objects.
3. Express opt-in protocols (operation implementations, backends) as
   `s7contract` traits; a pass-runner protocol returns only when the pipeline
   has more than one caller for it.
4. Grow type, rank, symbolic-shape, effect, and legality analysis before any
   backend work.
5. Treat unsupported R as a classed diagnostic, not as implicit fallback.

The first north-star programs are the apotheosis examples in `README.Rmd`.
Minimal probes should isolate one higher-level idea at a time. Composite
targets such as logistic-gradient and Viterbi should remain known-failing until
the compiler can model their declarations, domains, reductions, matrix
operations, regions, bridges, operation implementations, and fusion boundaries
honestly.
