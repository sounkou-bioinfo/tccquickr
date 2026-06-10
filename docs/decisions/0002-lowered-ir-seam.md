
<!-- 0002-lowered-ir-seam.md is generated from 0002-lowered-ir-seam.Rmd. Do not edit the .md. -->

# 0002 — Insert a lowered IR; the C target becomes a printer

- Status: accepted
- Date: 2026-06-10

## Decision

Introduce an explicit **lowered IR** (“LIR”) between the middle-end and
the C target. LIR is target-neutral but execution-shaped: explicit
loops, index arithmetic, allocations, loads/stores, scalar ops, and
explicit boundary regions. The C target is reduced to a **dumb printer
of LIR** — it makes no ownership, legality, or optimization decisions.

Concretely:

- New module `R/tccq_lir.R`: LIR node constructors + a validator.
- New pass `lower_to_lir`: middle-end IR/kernel form → LIR, consuming
  the existing storage/allocation/protection/boundary plans.
- `R/tccq_target_c_rapi.R` shrinks from a ~2,900-line decision engine to
  a printer that walks LIR and emits C. No `identical(node, ...)`-driven
  semantic branching; structural emission only.

## Why

`tccq_target_c_rapi.R` is currently **2,915 lines — ~36% of the
package** — with 363 shape-keyed branches and 122 hits on
ownership/materialize/borrow/alias/ legal/specialize/copy/protect. That
directly violates the Semantic Staging Rule: the C emitter is where the
compiler *re-discovers* semantics per node shape. This is the single
thing blocking every other goal:

- **Retargeting** (Fortran/Rust/devices,
  [0003](0003-target-and-backend-roadmap.md)) is impossible while
  lowering decisions live inside a C string builder. A second backend
  today means duplicating ~2,900 lines of decisions.
- **SSA / dataflow optimization** has nowhere to live. LIR is the form
  on which real passes (CSE, licm, bounds-check elision, fusion
  legality) operate.
- **Testability**: LIR is a data structure you can assert on; emitted C
  strings are not.

## The seam

    R subset
      → tccq_frontend            (typed)
      → tccq IR + middle-end     (validate, effects, index_normalize, shape_domains,
                                  kernelize, fusion, boundary, storage/alloc/protect)
      → LIR  (lower_to_lir)      ← NEW: explicit loops/indices/allocs/mem/boundary
      → printer                  ← C today; Fortran/Rust/... later
      → backend                  TinyCC (dev) | shlib/GCC (release) | native .o

LIR is the contract. Everything above it reasons about R-array
semantics; everything below it is mechanical translation.

## LIR shape (initial)

Target-neutral nodes, roughly:

- region: `lir_func`, `lir_block`
- memory: `lir_alloc`, `lir_load`, `lir_store`, `lir_view` (borrowed
  span)
- control: `lir_for` (counted), `lir_if`
- value: `lir_const`, `lir_param`, `lir_temp`, `lir_binop`, `lir_unop`,
  `lir_call` (intrinsic math), `lir_index` (affine address)
- boundary: `lir_boundary` (opaque R-eval region; printers must
  round-trip it unchanged, never inline across it)
- meta carried as fields, not as target syntax: ownership
  (`owned`/`borrowed`), element type, rank, known extents (for
  specialization, see [0004](0004-recon-and-jit-cleanup.md)).

LIR must be expressible without naming C. If a node can only be
understood as C text, it belongs in the printer, not in LIR.

## What this rejects

- **Keeping semantics in the C emitter.** Rejected; that is the mess
  being cleaned up.
- **A second target backend before LIR exists.** Rejected; build the
  seam first.
- **A maximally abstract IR that no backend has exercised.** Rejected.
  LIR is shaped by what C needs first;
  [0003](0003-target-and-backend-roadmap.md) lists the device
  constraints we keep in view so C-first does not paint us into a
  corner, but we do not build device nodes speculatively.

## Consequences and migration

This is a rebuild of the *bottom* of the stack only. The frontend, IR,
and the 11 middle-end passes are kept (they are working and tested).

Migration is incremental and stays green at every step:

1.  Land `tccq_lir.R` (constructors + validator) with no pipeline
    wiring.
2.  Add `lower_to_lir` covering the simplest surface (scalar + pointwise
    vector), gated behind an internal flag; the existing emitter stays
    default.
3.  Add a `print_c_from_lir` printer for that surface; differential-test
    it against the existing emitter and against direct R.
4.  Expand LIR coverage surface by surface (reducers → views/slices →
    stores → loops → matrix → boundary), retiring the corresponding
    branches of the old emitter as each surface reaches parity.
5.  When LIR covers the supported subset, delete the legacy decision
    branches and `tccq_target_c_rapi.R` becomes the printer.

No backward-compat shim is required (per project owner): once a surface
reaches parity in LIR, the old path for that surface is removed, not
kept in parallel.
