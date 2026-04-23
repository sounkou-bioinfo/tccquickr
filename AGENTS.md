# Repo Scope And Rules

## Scope

`tccquickr` is the experimental compiler and transformation package built on
`Rtinycc`.

This repo is responsible for:

- the `tccq_*` compiler path
- typed frontend parsing and lowering for the declared R subset
- IR design, validation, middle-end passes, and legality checks
- C target emission through the R C API
- compiler-facing tests, examples, and design docs
- backend-neutral architecture work, with `Rtinycc` used as the current TinyCC
  runtime/backend layer rather than re-owned here

This repo should depend on `Rtinycc` for TinyCC runtime, FFI compilation, and
low-level execution support rather than re-implementing that functionality.

## Current Architecture

Treat `tccq_*` as the only active compiler architecture.

The intended split is:

- frontend: `declare(type(...))` parsing plus typed lowering from R AST to IR
- middle-end: validation, effects, kernelization, fusion, boundary handling,
  storage planning, allocation planning, and protection planning
- target: C + R C API emission
- backend: source-only output, TinyCC via `Rtinycc`, and later other C
  compilation/loading backends such as system-compiler or `callme`-style paths

Do not introduce parallel replacement paths lightly. New compiler work should
land in `tccq_*` unless the task is explicitly about a temporary migration or
compatibility shim.

Prefer one primary target language first: C. If we want more deployment modes,
add more C backends before inventing extra target IRs.

## Semantic Staging Rule

Keep as much semantic information as possible in the R-level compiler before
emitting C.

That means:

- decide legality, ownership, aliasing, view semantics, mutation barriers, and
  fallback boundaries in IR or middle-end plans
- prefer explicit IR nodes and explicit plans over target-side special cases
- treat emitted C as a target artifact that consumes compiler decisions, not as
  the place where the compiler first discovers semantics
- when a choice matters for correctness or optimization, model it in the
  middle-end rather than hiding it in code generation branches

This is the closest useful lesson from SAC / `sac2c` for this repo: the
compiler knows the array program before it becomes C, so the important
reasoning should happen before C emission.

## Current Semantic Commitments

For `tccq`, keep these rules explicit:

- `a <- expr` is a local binding, not mutation
- `x[i]` is an indexed read
- `x[lo:hi]` is a contiguous slice/view expression
- `a[i] <- v` and `a[lo:hi] <- v` are mutation barriers
- direct mutation of formals such as `x[i] <- v` is rejected for now
- rebinding an already-bound local name is rejected for now
- do not fuse across `store_index`, `store_range`, or explicit boundary nodes
- unsupported calls only cross into fallback through explicit boundary nodes
- views may stay borrowed in IR, but writes, boundary crossing, and return paths
  may force materialization

## Docs And Tests Rules

- Never manually write `.Rd` files.
- Generate `.Rd` files from source documentation using `roxygen2`.
- `README.Rmd` is the source for `README.md`.
- Keep docs aligned with the current `tccq_*` architecture.
- Remove stale references to deleted legacy paths or transitional `tccq2_*`
  naming.
- Prefer semantic/runtime tests and structured IR/plan checks over brittle
  source-substring assertions.
