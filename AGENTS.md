# Repo Scope And Rules

## Scope

`tccquickr` is the experimental compiler and transformation package built on
 top of `Rtinycc` and directly referencing
<https://github.com/sounkou-bioinfo/Rtinycc>.

This repo is responsible for:

- the `tccq_*` compiler path
- the parser, IR, lowering, validation, middle-end passes, and C target codegen
- transpiler-specific tests, docs, and examples
- experimentation around the declared R-to-C subset and its semantics
- backend-neutral architecture work, with `Rtinycc` used as the current TinyCC
  backend/runtime layer rather than re-owned here

This repo should depend on `Rtinycc` for TinyCC runtime, FFI compilation,
and low-level execution support rather than re-owning that functionality.

## Current `tccq` Direction

Treat `tccq_*` as the package's current compiler architecture.

The intended split is:

- frontend: parse `declare(type(...))` and lower a typed R subset
- middle-end: explicit kernel IR, fusion rewrites, legality barriers,
  materialization decisions, and later allocation planning
- target: C + R C API emission
- backend: source-only or TinyCC via `Rtinycc`

Do not introduce parallel replacement paths lightly. New compiler work should
land in `tccq_*` unless the task is explicitly about a temporary migration or
compatibility shim.

### Assignment, slicing, and indexed-write semantics

For `tccq`, keep the semantics explicit:

- `a <- expr` is a local binding, not mutation
- `x[i]` is an indexed read
- `x[lo:hi]` is a contiguous slice expression
- `a[i] <- v` and `a[lo:hi] <- v` are mutation barriers

Conservative milestone rules:

- plain assignment does not itself force mutation semantics
- indexed assignment currently requires an owned local vector binding
- direct mutation of formals such as `x[i] <- v` should be rejected until an
  explicit copy-on-write design is implemented
- slicing may stay view-like in IR, but writes, fallback boundaries, and return
  paths may force materialization
- do not fuse across `store_index` or `store_range`
- rebinding an already-bound local name is currently rejected until explicit
  reassignment semantics are designed
- boundary nodes should stay explicit legality barriers; do not silently fall
  back through random codegen paths

## Rules

- Never manually write `.Rd` files.
- Generate `.Rd` files from source documentation using `roxygen2`.
