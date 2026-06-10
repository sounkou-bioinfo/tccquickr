
<!-- 0003-target-and-backend-roadmap.md is generated from 0003-target-and-backend-roadmap.Rmd. Do not edit the .md. -->

# 0003 — Target and backend roadmap

- Status: accepted
- Date: 2026-06-10

## Decision

**C is the only target language we build now.** Other targets (Fortran,
Rust, Mojo) and device targets (CUDA, OpenCL/SPIR-V, OpenGL compute,
Vulkan compute) are *constraints on the LIR design*
([0002](0002-lowered-ir-seam.md)), not work items yet. We keep them in
view so C-first lowering does not foreclose them; we do not write
speculative backends.

A target is added only when it earns its keep against a concrete
workload.

## Two axes, do not conflate them

1.  **Target language** — what the printer emits: C, Fortran, Rust,
    Mojo, …
2.  **Backend** — how that source/object becomes a callable: TinyCC
    in-memory, `R CMD SHLIB`/GCC, precompiled native object +
    relocation, remote device queue.

`callme`-style SHLIB is a *backend*, not a target. CUDA is *both* a
target language and (with a host/device split) a backend deployment
model. Keeping the axes separate is what lets one LIR feed many of each.

## What each future target demands of LIR (so we don’t paint ourselves in)

LIR must be able to *represent* these even though only C consumes it
today. The point is to avoid C-isms leaking into LIR that would block
them later.

| Target                                               | What it needs LIR to already model                                                                                                                                                                                                                                                                   |
|------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **C** (now)                                          | counted loops, affine indexing, alloc/load/store, owned/borrowed spans, intrinsic math calls, opaque boundary regions                                                                                                                                                                                |
| **Fortran**                                          | column-major as an attribute of layout, not baked indexing; 1-based vs 0-based as a printer choice; whole-array ops survive as loop nests; no pointer aliasing assumptions unless declared                                                                                                           |
| **Rust**                                             | ownership/borrow already first-class in LIR (we have it); bounds-safety facts so `get_unchecked` is justified, not guessed; lifetimes map to our borrowed-view spans                                                                                                                                 |
| **Mojo**                                             | same loop/owned model; SIMD width as a parameter of a vectorizable loop, not hardcoded                                                                                                                                                                                                               |
| **CUDA / OpenCL / SPIR-V / Vulkan / OpenGL compute** | a loop nest carrying an explicit, side-effect-free **parallel/elementwise domain** (the kernelizer already produces `domain`/`producer` — preserve that into LIR); explicit host/device buffer boundaries; no hidden R C API inside the parallel region (the boundary split already guarantees this) |

The common requirement across all of them: **the parallel/elementwise
domain and the ownership/aliasing facts must survive into LIR as data**,
not be reconstructed from C text. That is exactly what
[0002](0002-lowered-ir-seam.md) buys.

## Backend roadmap (ordered)

1.  **TinyCC in-memory** (have) — dev loop, fast compile, correctness.
2.  **`R CMD SHLIB` / system compiler** (have) — release/benchmark
    quality; `-O2 -march=native` lives here. This is where AOT
    performance is realized.
3.  **Precompiled native SIMD object + TinyCC relocation** (designed,
    not built) — `tinycc-simd-stencils-design.md` Stage 1; GCC/Clang
    builds the kernel, TinyCC loads/relocates it, glue calls it.
4.  **Symbol-value patching + stencil instantiation** — Stages 2–3 of
    that doc, only if medium-granularity copy-and-patch beats full
    kernels on real cases.
5.  **Device backends** — only after a numeric workload exists that
    justifies the host/device buffer-lifetime and dispatch machinery.
    `RsimdDispatch` is the reference for CPU feature probing/dispatch;
    reuse its patterns, do not fold it in prematurely.

## What this rejects

- **Inventing extra target IRs per language.** Rejected. One LIR, many
  printers.
- **Building any device backend now.** Rejected; deferred behind a real
  workload.
- **Letting C-specific assumptions (row-major, 0-based, pointer
  aliasing) become load-bearing in LIR.** Rejected; those are printer
  choices. See the table.
- **Folding `RsimdDispatch` in before it’s needed.** Rejected;
  reference, reuse configure/dispatch patterns when a SIMD backend is
  real.

## Consequences

- LIR review gains one standing question: *“does this node assume C?”*
  If yes, push the assumption down into the printer.
- The roadmap is demand-driven. No target ships without a workload that
  proves it. This keeps the “lots of ideas” surface
  (Fortran/Mojo/CUDA/GL/Vulkan) from becoming sprawl: they are recorded
  as constraints, scheduled by need.
