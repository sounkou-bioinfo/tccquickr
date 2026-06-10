
<!-- 0004-recon-and-jit-cleanup.md is generated from 0004-recon-and-jit-cleanup.Rmd. Do not edit the .md. -->

# 0004 — Recon and JIT cleanup

- Status: accepted
- Date: 2026-06-10

## Decision

1.  **Remove the R-bytecode reconnaissance** from `tccq_analyze()`: the
    `compiler::cmpfun()` + `compiler::disassemble()` path and opcode
    extraction. Keep the AST/formals analysis and recommendations.
2.  **Keep `tccq_jit()`** (the per-signature compile cache) but record
    that its “exact” mode currently emits **shape guards, not
    specialized codegen**, and make closing that gap an explicit
    follow-up.

## Why — bytecode recon

`tccq_analyze_compiler()` compiles the function with the `compiler`
package, disassembles it, and collects opcode *names into advisory
strings that nothing consumes*. This is a misapplied numba analogy:

- numba reads CPython bytecode because Python gives it no typed AST at
  runtime;
- R is homoiconic — the AST and formals are right there, and the typed
  frontend already has more semantic information than bytecode would.

Round-tripping through bytecode walks *backward* into Python’s
constraint instead of using R’s advantage. It adds a `compiler`-package
dependency surface, brittle tests (`opcodes >= 1`), and no signal the
compiler uses. The AST facts (`call_names`, `dynamic_calls`,
`superassignments`, `maybe_free_symbols`) are the useful,
target-relevant part — keep those.

## Why — JIT gap

`tccq_jit(exact = TRUE)` keys a cache on observed shapes and threads a
`specialization` list into the module. But in the C target that list
only feeds `tccq_c_emit_specialization_check()`, which emits runtime
guards (`Rf_error("argument specialization mismatch")`). It does **not**
bake constant extents into the kernel loops. So today’s “specialization”
is *shape assertion*, not specialization. That is fine as a correctness
guard, but it is not the optimization the name implies, and it should
not be reported as done.

Real constant-extent specialization belongs in the LIR work
([0002](0002-lowered-ir-seam.md)): `lir_for` and `lir_index` should be
able to carry known-constant extents so the printer/compiler can unroll,
fold, and size allocations statically. Until then, `tccq_jit` is a
compile cache with guards.

## What this rejects

- **Bytecode-driven analysis as a compiler input.** Rejected; use the
  AST.
- **Claiming `tccq_jit` specializes codegen.** Rejected wording; it
  guards shape.

## Consequences

- `tccq_analyze()` loses its `compiler` field; `print.tccq_analysis` and
  the recommendations drop the compiler branch; the test drops bytecode
  assertions. Docs regenerated via roxygen2 (never hand-edit `.Rd`).
- A follow-up (tracked in LIR work) wires constant extents from
  `specialization` into LIR so `tccq_jit(exact=TRUE)` produces genuinely
  specialized kernels.
