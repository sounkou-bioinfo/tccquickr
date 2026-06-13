# Root Direction

The compiler core starts from values with schemas:

1. Parse declarations from a deliberately narrow R subset.
2. Represent formals, values, effects, and diagnostics as S7 objects.
3. Run passes through `s7contract` interfaces.
4. Grow type, rank, symbolic-shape, effect, and legality analysis before any
   backend work.
5. Treat unsupported R as a classed diagnostic, not as implicit fallback.

The first north-star program is `tccq_apotheosis_kernel()`. It should remain
known-failing until the compiler can model its declarations, domains,
reductions, matrix operations, and fusion boundaries honestly.
