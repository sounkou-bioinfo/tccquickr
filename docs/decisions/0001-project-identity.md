
<!-- 0001-project-identity.md is generated from 0001-project-identity.Rmd. Do not edit the .md. -->

# 0001 — Project identity: AOT optimizing transpiler

- Status: accepted
- Date: 2026-06-10

## Decision

`tccquickr` is an **ahead-of-time optimizing transpiler**. It lowers a
declared subset of R to a typed IR, optimizes at the semantically
meaningful level, and emits source for a real optimizing compiler (C
first, via GCC/Clang `-O2`, `-march`/`-mavx2` for hot kernels).

TinyCC is **not** the performance backend. It is kept for two specific
jobs:

1.  fast in-memory compilation for the dev/iteration loop, and
2.  an in-memory linker/relocator that loads precompiled native object
    files (the SIMD-object workflow in
    `../tinycc-simd-stencils-design.md`).

## Why

The TinyCC SIMD reconnaissance (`docs/tinycc-simd-stencils-design.md`)
settled a fork the project had been straddling:

- TinyCC cannot practically compile `<immintrin.h>` / native intrinsics.
- SIMDe-through-TinyCC is a correctness fallback, ~far slower than
  native.
- TinyCC scalar loops measured ~5×+ slower than a full GCC AVX2 kernel.
- Native SIMD *must* come from GCC/Clang precompiled objects, loaded
  into a TinyCC state and relocated.

So “fast in-memory TinyCC JIT” and “competitive numeric kernels” pull in
opposite directions. You cannot serve both from one pipeline. We choose
performance: optimize in the IR, emit clean C, and let a real compiler
do instruction selection, vectorization, and register allocation — the
things TinyCC is not built to do.

This also matches the codebase’s stated instinct (the Semantic Staging
Rule): keep meaning high, treat emitted source as a consumed artifact.

## What this rejects

- **TinyCC as the optimizing code generator.** Rejected. It is
  dev-loop + linker.
- **Intrinsic-sized extern shims** as the acceleration strategy.
  Rejected (already shown to lose inlining, register allocation,
  scheduling).
- **SIMDe-under-TinyCC as an acceleration path.** Rejected; fallback
  only.
- **Privileging interactive latency over kernel quality.** Rejected. A
  signature cache (`tccq_jit`) is welcome, but it caches AOT-compiled
  artifacts; it does not justify a weaker codegen path.

## Consequences

- The compiler must produce C that a real compiler optimizes well:
  explicit loops, `restrict`-friendly pointers, no R C API in hot
  regions (the existing boundary/kernel split already commits to this —
  keep it).
- Compile latency is acceptable to trade for kernel quality. The dev
  loop uses TinyCC; release/benchmark uses GCC/Clang via the `shlib`
  backend or a native object path.
- Retargeting beyond C (see [0003](0003-target-and-backend-roadmap.md))
  becomes a realistic goal *because* we are already emitting source for
  an external optimizer rather than owning codegen — but only after the
  lowered IR seam in [0002](0002-lowered-ir-seam.md) exists.
