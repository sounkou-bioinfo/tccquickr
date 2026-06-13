
# tccquickr

`tccquickr` is being hard-reset as a typed R transformation core.

The new root is:

- declared R programs, not source editing;
- S7 schemas for compiler values;
- `s7contract` interfaces for internal compiler protocols;
- classed diagnostic values instead of stringly errors;
- shape-aware analysis before any C emission;
- no fake backward compatibility while the semantic core is rebuilt.

The current target program is `tccq_apotheosis_kernel()`: a
logistic-gradient array kernel with symbolic shapes, reductions,
normalization, matrix-vector multiply, and scalar/vector fusion
pressure. It fails today by design. Making that program pass through a
typed IR is the first serious milestone.
