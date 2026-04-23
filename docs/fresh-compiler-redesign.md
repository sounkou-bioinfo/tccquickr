# `tccquickr` compiler architecture note

This document is no longer a "fresh reset" note. It describes the current
`tccq_*` compiler direction.

## Core design rule

`tccquickr` has more semantic information while it is still reasoning about R
code than after it has emitted C.

So the compiler should keep important meaning in the frontend and middle-end:

- typed values and ranks
- local bindings versus mutation
- aliases versus owned locals versus views
- legality barriers and fallback boundaries
- fusion opportunities
- materialization points
- storage, allocation, and protection plans

The C target should mostly consume those decisions. It should not be the place
where the compiler first infers ownership, legality, or optimization strategy
from ad hoc expression shapes.

This is the most useful architectural lesson to borrow from SAC / `sac2c` for
`tccquickr`: reason about the array program while it is still a compiler IR,
then lower that understanding into C.

## Current split

### Frontend

Responsibilities:

- parse `declare(type(...))`
- collect formals and declared types
- lower the supported R subset into typed IR
- reject unsupported semantics early when possible

Current entry points:

- `tccq_frontend()`
- `tccq_lower_module()`

### Middle-end

Responsibilities:

- validate IR shape and effects
- convert expressions into explicit kernel IR
- lower recognized reducers into explicit fold form
- fuse legal producer/fold patterns
- collect and validate explicit boundary nodes
- derive conservative storage, allocation, and protection plans

Current default pass chain:

1. `validate_ir`
2. `effects`
3. `kernelize`
4. `fusion`
5. `boundary_collect`
6. `boundary_validate`
7. `storage_plan`
8. `allocation_plan`
9. `protect_plan`

Important current IR concepts:

- expression nodes such as `var`, `binary`, `call1`, `reduce`
- statement/program nodes such as `bind`, `store_index`, `store_range`,
  `program`
- kernel nodes such as `domain`, `producer`, `materialize`, `fold`,
  `scalar_kernel`, `kernel_program`
- explicit view/slice nodes such as `view1`
- explicit boundary nodes such as `boundary_call`

### Target

Responsibilities:

- emit C for the current kernel/program form
- own `SEXP`, `PROTECT`, `UNPROTECT`, `Rf_allocVector`, and `Rf_eval`
- turn explicit compiler decisions into target code

Current target:

- `tccq_target_c_rapi()`

### Backend

Responsibilities:

- compile or return target artifacts
- stay ignorant of R-level lowering details
- load/return the emitted entry point

Current backends:

- `tccq_backend_source()`
- `tccq_backend_tinycc()`
- `tccq_backend_shlib()` for shared-library compilation through `R CMD SHLIB`,
  in the same general deployment space as [`callme`](https://github.com/coolbutuseless/callme)

The compile seam now also validates backend capabilities against:

- target requirements
- compile context fields such as headers, include paths, and libraries
- explicit boundary APIs used by the module

Planned direction:

- keep the main target in C
- add more C compilation/loading backends before adding new target languages
- extend beyond the current TinyCC and `R CMD SHLIB` paths only when the extra
  deployment mode is genuinely useful

## Current semantic model

### Bindings and mutation

- `a <- expr` creates a local binding
- `x[i]` is an indexed read
- `x[lo:hi]` lowers to an explicit contiguous view/slice form
- `a[i] <- v` and `a[lo:hi] <- v` are mutation barriers
- direct formal mutation is rejected for now
- rebinding a local name is rejected for now

### Ownership and materialization

The compiler currently distinguishes, conservatively, between:

- borrowed formals
- owned local vectors
- alias locals such as `y <- x`
- view locals such as `y <- x[lo:hi]`

Writes, boundary crossing, and returns may force materialization. That decision
belongs in compiler plans, not only in C emission branches.

### Boundaries

Unsupported calls do not silently disappear into codegen.

- `fallback = "hard"` rejects them
- `fallback = "auto"` lowers them to explicit `boundary_call` / `r_eval` nodes

Boundary nodes are legality barriers and should remain explicit.

## Reference projects

### `quickr`

Useful for:

- declared-subset compiler comparisons
- compile-boundary ergonomics
- keeping the compiler/runtime split readable

### `anvil`

Useful for:

- explicit backend thinking
- transformation-stage separation
- capability-oriented architecture instincts

### SAC / `sac2c`

Most relevant ideas for this repo:

- explicit array/kernel IR
- legality-driven fusion
- storage/materialization decisions represented before target emission
- reuse opportunities based on shape/index semantics, not on string-level C
  accidents

The reuse and with-loop machinery in `sac2c` is not something to copy
literally, but it is a strong reminder that high-value optimization decisions
belong in compiler analysis, not in backend folklore.

## Current limits

`tccq_*` is still intentionally narrow.

Current supported core:

- declared scalar and vector inputs
- scalar and elementwise arithmetic
- comparison and logical vector expressions
- unary math calls such as `sin()` and `exp()`
- generic fold-style reducers: `sum`, `prod`, `min`, `max`, `mean`, `any`, and
  `all`
- limited `Reduce(FUN, x)` lowering for recognized reducer surfaces
- local bindings
- scalar indexed reads
- contiguous slices/views
- local indexed and range writes
- explicit fallback boundaries
- source-only, TinyCC-backed, and shared-library (`R CMD SHLIB`) compilation
  modes
- generated differential validation over programmatically constructed cases

## Architecture closeout checklist

Implemented now:

- [x] explicit fold IR for more than one reducer surface
- [x] comparison/logical vector expressions needed for reducer idioms such as
  `any(x > 0)` and `all((x > 0) & (y > 0))`
- [x] limited `Reduce(FUN, x)` lowering for recognized reducer operators
- [x] cross-backend generated validation against direct R evaluation

Still open:

- [ ] richer semantic allocation/reuse planning before C emission
- [ ] clearer middle-end ownership of boundary argument materialization
- [ ] more systematic view/index normalization before C emission
- [ ] axis-wise reductions and matrix-aware lowering
- [ ] larger harvested validation corpus in addition to generated tests

Current important limits:

- scalar/vector ranks only in the current C target
- only conservative view handling today
- no explicit copy-on-write model for formal mutation yet
- allocation planning exists but is still conservative
- boundary argument materialization should become a clearer middle-end decision
- view/index normalization is still much smaller than SAC-style loop metadata
- axis reductions / full `apply`-family semantics still require rank-aware IR
- limited `Reduce(FUN, x)` lowering is not yet a claim of full base-R
  `Reduce()` compatibility

## Highest-value next steps

1. enrich allocation/materialization planning so codegen consumes a clearer plan
2. make boundary argument materialization more explicitly middle-end owned
3. normalize view/index metadata more systematically before C emission
4. add rank-aware axis-reduction IR before broader `apply`-family lowering
5. grow validation from generated cases into a larger maintained corpus

## Validation strategy

The current validation direction is intentionally executable rather than merely
aspirational.

Implemented now:

- explicit tinytests for core semantics and backends
- generated differential tests comparing compiled results against direct R
  evaluation
- cross-backend comparisons between TinyCC and `R CMD SHLIB`

Still to grow:

- larger generated grammars over the supported subset
- pass-by-pass translation validation for middle-end rewrites
- harvested corpora from real array-oriented/base-R-style programs where the
  supported subset overlaps

## Backend note: TinyCC and `callme`

After looking at [`callme`](https://github.com/coolbutuseless/callme), the
useful lesson is backend shape rather than frontend semantics.

`callme` already demonstrates a practical C-only compilation/loading workflow:

- accept complete C source
- pass through compiler, preprocessor, and linker flags
- compile through `R CMD SHLIB`
- load the resulting shared library and create `.Call` wrappers

That fits `tccquickr` as a backend idea, not as a replacement compiler model.
The `tccq_*` frontend and middle-end should still decide semantics; a
`callme`-style path would simply be another way to compile the emitted C.
