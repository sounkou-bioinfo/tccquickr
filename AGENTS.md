# Repo Scope And Rules

## Prologue

Always ask yourself before landing a change what other changes would have made that change easier to land and other changes to land long term. 
Sometimes as needed, you can call into the user or a PI/GPT/reviewer fellow for ideas and task to accomplish and ask them this question with current state of the project, avenues you see. Having several perspectives and long term maintenability is important ! Ambiguities should be avoided. Code sprawl and bloat too. Allignmment among actors is a must.

write C as a BSD kernel programmer rather than a Java programmer that failed upwards
write R as a r-lib programmer rather than a Python programmer that failed upwards

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
- middle-end: validation, effects, kernelization, fusion, reducer handling,
  boundary handling, storage planning, allocation planning, and protection
  planning
- target: C + R C API emission
- backend: source-only output, TinyCC via `Rtinycc`, or shared-library
  compilation through `R CMD SHLIB`, with room for further C-only backends such
  as other system-compiler or `callme`-style paths

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
- comparison and logical vector code should stay explicit in IR rather than
  being hidden in target-only lowering
- reducers should go through the reducer registry / fold path rather than
  adding one-off codegen-only special cases
- limited `Reduce(FUN, x)` lowering is acceptable only for recognized reducer
  surfaces within the current subset; do not reason about it as full base-R
  `Reduce()` semantics yet
- do not fuse across `store_index`, `store_range`, or explicit boundary nodes
- unsupported calls only cross into fallback through explicit boundary nodes
- views may stay borrowed in IR, but writes, boundary crossing, and return paths
  may force materialization

## Docs And Tests Rules

- Never manually write `.Rd` files.
- Generate `.Rd` files from source documentation using `roxygen2`.
- `README.Rmd` is the source for `README.md`.
- Keep docs aligned with the current `tccq_*` architecture.
- Remove stale references to deleted legacy paths or transitional naming.
- Prefer semantic/runtime tests and structured IR/plan checks over brittle
  source-substring assertions.
- When adding language coverage, extend the generated/differential validation
  suite so compiled behavior is compared against direct R evaluation over many
  programmatically constructed cases.
- Treat corpus growth as part of the architecture work: generated tests come
  first, and harvested real-world/base-R-style cases can be added on top.
