# Repo Scope And Rules

## Scope

`tccquickr` is the experimental compiler-front-end package built on top of
`Rtinycc` and directly referencing
<https://github.com/sounkou-bioinfo/Rtinycc>.

This repo is responsible for:

- `tcc_quick()` and `tcc_quick_ops()`
- the parser, IR, lowering, validation, and code-generation pipeline
- transpiler-specific tests, docs, and examples
- experimentation around the R-to-C subset and its semantics
- the fresh `tccq2_*` compiler path, where backend-neutral middle-end work
  should land first

This repo should depend on `Rtinycc` for TinyCC runtime, FFI compilation,
and low-level execution support rather than re-owning that functionality.

## Fresh `tccq2` Direction

Treat `tccq2_*` as the preferred path for new compiler architecture work.

The intended split is:

- frontend: parse `declare(type(...))` and lower a typed R subset
- middle-end: explicit kernel IR, fusion rewrites, legality barriers,
  materialization decisions, and later allocation planning
- target: C + R C API emission
- backend: source-only or TinyCC via `Rtinycc`

Do not wire new middle-end ideas directly into `tcc_quick*` unless the task is
specifically about preserving or fixing the older prototype.

### Assignment, slicing, and indexed-write semantics

For `tccq2`, keep the semantics explicit:

- `a <- expr` is a local binding / rebinding, not mutation
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
- boundary nodes should stay explicit legality barriers; do not silently fall
  back through random codegen paths

## Rules

- Never manually write `.Rd` files.
- Generate `.Rd` files from source documentation using `roxygen2`.
