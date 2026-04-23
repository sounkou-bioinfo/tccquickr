# `sac2c` study notes for `tccquickr`

These are internal architecture notes from a reread of `sac2c`, its tests,
selected implementation files, and the SaC publications page.

The goal is not to copy `sac2c`. The goal is to separate:

- ideas that are still highly relevant to `tccq_*`, from
- delivery mechanisms in `sac2c` that are too large, macro-heavy, or backend-
  specific for `tccquickr`.

## Sources reread

### Local repo sources

- `/root/Rtinycc/.sync/sac2c/README.md`
- `/root/Rtinycc/.sync/sac2c/doc/sac.texi`
- `/root/Rtinycc/.sync/sac2c/doc/schedule_design.txt`
- `/root/Rtinycc/.sync/sac2c/doc/distributed_fold_design.txt`
- `/root/Rtinycc/.sync/sac2c/doc/ref-counting-methods.txt`
- `/root/Rtinycc/.sync/sac2c/src/runtime/m4/README`
- `/root/Rtinycc/.sync/sac2c/src/runtime/m4/icm.m4`
- `/root/Rtinycc/.sync/sac2c/src/runtime/essentials_h/std_gen.h.m4`
- `/root/Rtinycc/.sync/sac2c/src/runtime/essentials_h/std.h`
- `/root/Rtinycc/.sync/sac2c/src/runtime/essentials_h/icm.h`
- `/root/Rtinycc/.sync/sac2c/src/runtime/essentials_h/wl.h`
- `/root/Rtinycc/.sync/sac2c/include/sac.h`
- `/root/Rtinycc/.sync/sac2c/src/libsac2c/codegen/icm2c_wl.c`
- `/root/Rtinycc/.sync/sac2c/src/libsac2c/memory/ReuseWithArrays.c`
- `/root/Rtinycc/.sync/sac2c/src/libsac2c/wltransform/wlidxs.c`
- `/root/Rtinycc/.sync/sac2c/tests/reusecand/test-wrci-rc.sac`
- `/root/Rtinycc/.sync/sac2c/tests/reusecand/test-wrci-prc.sac`
- `/root/Rtinycc/.sync/sac2c/tests/with-loops/test-aud-fold.sac`
- `/root/Rtinycc/.sync/sac2c/tests/with-loops/test-aud-genarray.sac`
- `/root/Rtinycc/.sync/sac2c/tests/with-loops/test-aud-modarray.sac`
- `/root/Rtinycc/.sync/sac2c/tests/wlir/test-wlir-shadow.sac`
- `/root/Rtinycc/.sync/sac2c/tests/README.md`

### Paper/publication sources

- `https://www.sac-home.org/publications`
- raw/xhtml publication exports from the same site
- selected publication PDFs downloaded from sac-home for spot-checking

## Bottom line

The strongest SaC lesson for `tccquickr` is still:

> make shape, domain, index, reuse, and materialization facts first-class
> compiler artifacts before emitting C.

The strongest counter-lesson is:

> do not copy `sac2c`'s M4 + CPP macro architecture to get those benefits.

For `tccquickr`, the right synthesis is:

- emulate the **semantic staging**,
- not the **macro delivery mechanism**.

## Paper notes

## Most relevant historical papers

### 1. *On Code Generation for Multi-generator With-loops in Sac* (2000)

Main idea:

- with-loops have map-like semantics
- legal execution order is more flexible than naïve loop lowering suggests
- code generation is partly a schedule choice, not just a syntax translation

Why it matters now:

- `tccq` needs an explicit notion of domain and schedule metadata
- view/range lowering should not be discovered ad hoc in the C emitter
- later multidimensional work should keep logical domain and low-level loop
  schedule separate

### 2. *Single Assignment C — Efficient Support for High-level Array Operations in a Functional Setting* (2003)

Main idea:

- SaC combines functional semantics with efficient array programming
- the core language ideas are shape-carrying array types plus with-loops
- runtime competitiveness depends on shape inference and high-level
  transformations that remove intermediate arrays

Why it matters now:

- validates the overall `tccq` direction: explicit typed array IR first, C later
- reinforces that optimization should happen at the array-program level, not as
  isolated peepholes in emitted C

### 3. *With-loop Scalarization: Merging Nested Array Operations* (2004)

Main idea:

- nested array operations create temporary arrays
- scalarization pushes work inward and reduces those temporaries
- with-loops are the right internal representation for expressing that rewrite

Why it matters now:

- closely matches `producer` / `materialize` / `fold` thinking in `tccq`
- strongly supports keeping scalarization-like rewrites as middle-end IR passes
- suggests future work on nested view/index/materialize rewrites before codegen

### 4. *Implicit Memory Management for Sac* (2004)

Main idea:

- functional array languages want implicit memory management
- reference counting fits this better than tracing GC when timely reclaim and
  in-place updates matter
- optimizing memory management is central to array-language performance

Why it matters now:

- reminds us that copy/borrow/materialize semantics are core language semantics
- supports building explicit ownership and reuse plans rather than relying on
  emitted-C accidents
- especially relevant to alias locals, views, mutation barriers, and return
  strategies in `tccq`

### 5. *With-loop Fusion for Data Locality and Parallelism* (2006)

Main idea:

- fuse with-loops not only to remove intermediates, but also to improve data
  locality and parallel behavior
- traversal sharing is itself an optimization target

Why it matters now:

- reinforces that fusion is more than temporary elimination
- suggests future `tccq` fusion should reason about shared domains and reuse of
  traversal structure, not only local producer-consumer pairs

### 6. *Shape Cliques* (2006)

Main idea:

- infer sets of arrays that are known to have equal shapes
- use those equivalence classes to enable more optimizations and remove checks

Why it matters now:

- the cleanest paper-level argument for adding a shape/domain equivalence pass
  to `tccq`
- would help zip legality, slice/view reasoning, fusion legality, and later
  multidimensional work

### 7. *Index Vector Elimination: Making Index Vectors Affordable* (2007)

Main idea:

- index vectors are elegant at the source level but expensive if they survive
  too long
- compilers should reduce index-vector and offset-computation overhead through
  high-level rewrites

Why it matters now:

- directly relevant to `x[i]`, `x[lo:hi]`, views, offset math, and later rank-
  aware indexing
- strongly argues for a real `index_normalize` / offset-planning pass before C
  emission

### 8. *Extended Memory Reuse: An Optimisation for Reducing Memory Allocations* (2018)

Main idea:

- extend lifetimes in a controlled way so deallocation + reallocation can become
  direct reuse
- reduce allocator pressure without losing the benefits of reference-counted,
  in-place-capable semantics

Why it matters now:

- the best paper-level motivation for moving from a simple storage plan to a
  real reuse plan in `tccq`
- especially relevant for loops, mutate-then-return locals, and eventual
  multidimensional kernels

## Important newer papers, but probably phase 2 for `tccq`

### *In-Place-Folding of Non-Scalar Hyper-Planes of Multi-Dimensional Arrays* (2022)

Main idea:

- do non-scalar folds in place over multidimensional hyperplanes
- keep one result allocation while avoiding scalarization of the fold operator

Why it matters:

- very relevant later for axis reductions and rank-aware IR
- probably premature until `tccq` has real multidimensional kernel/domain
  support

### *Parallel Scan as a Multidimensional Array Problem* (2022)

Main idea:

- use rank-polymorphic reshapes and recursive sub-array views to expose
  different parallel scan strategies

Why it matters:

- good evidence that shape-guided decomposition belongs in the language/IR
- more relevant after `tccq` grows axis-aware and rank-aware operations

### *Rank-Polymorphism for Shape-Guided Blocking* (2023)

Main idea:

- encode blocking structure in array shape and rank-polymorphic combinators
- let reshapes and rank structure express what imperative blocking often does
  with extra loops and explicit index math

Why it matters:

- promising long-term direction for future matrix/tensor lowering
- not the next step; requires richer rank-aware IR first

## What the macro-heavy C implementation actually is

The heavy implementation is real, deliberate, and layered.

## Layers

### 1. Facade / assembly layer

File:

- `include/sac.h`

Role:

- one umbrella include that pulls together generated headers, runtime macro
  bodies, backend-specific support, tracing, profiling, RC, CUDA, MT, distmem,
  and other features

Consequence:

- this is less a normal header and more a macro/runtime assembly point

### 2. Macro-dispatch DSL

Files:

- `src/runtime/m4/README`
- `src/runtime/m4/icm.m4`

Role:

- define an M4 DSL for mapping tagged name-tuples to concrete macro variants
- `pat(...)` and `rule(...)` describe compile-time dispatch rules
- wildcard tags such as `*SHP`, `*HID`, `*UNQ`, `*CBT` expand to many
  specialization cases

Consequence:

- a large part of the compiler/runtime contract is encoded as generated CPP
  dispatch, not just handwritten C

### 3. Generated dispatch catalogs

File:

- `src/runtime/essentials_h/std_gen.h.m4`

Role:

- enumerate many specialization mappings for runtime operations like descriptor
  access, reads/writes, shape queries, parameter passing, etc.

Consequence:

- `sac2c` pushes a lot of representation-specific choice into macro dispatch
  tables rather than into plain runtime functions or small codegen decisions

### 4. Low-level tuple and token-glue utilities

File:

- `src/runtime/essentials_h/icm.h`

Role:

- provide tuple item extraction (`Item0`, `Item1`, ...)
- provide large token-concatenation families (`CAT0`, `CAT1`, ...)

Consequence:

- the macro system depends on positional tuple access and layered token
  concatenation to resolve variants

### 5. Runtime macro bodies

Files:

- `src/runtime/essentials_h/std.h`
- `src/runtime/essentials_h/wl.h`
- related runtime headers

Role:

- implement actual runtime semantics selected by the generated macro layer
- descriptor handling, RC modes, shape/size metadata, reads/writes, WL schedule
  helpers, shape factors, offset updates, no-op grids, and loop helpers

Consequence:

- the runtime contract is highly macroized and tightly coupled to descriptor and
  backend semantics

### 6. Codegen bridge

File:

- `src/libsac2c/codegen/icm2c_wl.c`

Role:

- emit C that calls the runtime macro layer for with-loop scheduling and index
  handling
- generated C is still structured by these macro abstractions rather than plain,
  directly readable C loops alone

Consequence:

- `sac2c`'s generated C is deeply shaped by its runtime macro architecture

## Why it became so heavy

The macro system is solving real problems:

- specialization over shape knowledge and array representation classes
- multiple backends and runtime modes
- descriptor-heavy arrays with RC and uniqueness concerns
- shared low-level code patterns across many generated cases
- with-loop scheduling and offset logic reused across many compilation paths

So the macro system is not accidental. It is the mechanism used to scale one
compiler/runtime stack across many cases.

## Why `tccquickr` should not copy that mechanism

For `tccquickr`, the macro system would be the wrong level to import.

Reasons:

- `tccquickr` already has a good place for semantics: R-level IR and passes
- `tccquickr` is not trying to be SaC's full systems/compiler/runtime stack
- the M4 + CPP approach is difficult to debug, difficult to read, and expensive
  to preprocess
- it would move semantic decisions away from the explicit middle-end we are
  trying to build

The part to copy is the **separation of concerns**, not the specific macro
engineering.

## Implementation evidence from source files

## Reuse lessons from `ReuseWithArrays.c`

This file is one of the most useful concrete implementation references.

Core idea:

- reuse is not guessed opportunistically
- it is inferred conservatively under explicit access-pattern constraints

Important constraints in the file/comments:

- `modarray` loops can possibly reuse their source array
- `genarray` / `modarray` loops can possibly reuse arrays seen in the WL body
  only if those arrays satisfy strict conditions
- candidate arrays must not occur on the left-hand side
- right-hand-side uses must look like selection by the current with-loop index
  or a valid prefix-based nested index pattern
- invalid index use moves the array into a no-reuse set

Key lesson for `tccq`:

- reuse should be a separate proof obligation
- reuse legality belongs in explicit compiler analysis, not in C-emission
  heuristics

## Offset/index lessons from `wlidxs.c`

This file annotates with-loops with offset/index variables that can be reused.

Important details:

- result arrays from `genarray` / `modarray` may get WL offset variables
- if shapes are known equal, existing offset variables can be reused
- this sharing is explicitly justified by shape equality checks
- folds do not inherently get offsets because they do not guarantee a canonical
  iteration order
- but if folds are fused with array-producing loops, they may still exploit
  existing offsets

Key lesson for `tccq`:

- explicit index/offset metadata deserves its own middle-end pass
- shape equality can justify reuse of normalized traversal metadata
- folds and materialized arrays should not automatically be treated as having
  the same canonical traversal properties

## Scheduling lessons from `schedule_design.txt`

This file is a strong reminder that several concepts should stay separate:

- logical iteration domain
- schedule range
- stride-adjusted schedule range
- flat offset into storage

The `with2` scheduling note decomposes lowering into:

- `wlseg`
- `wlstride`
- `wlgrid`

Key lesson for `tccq`:

- later multidimensional work should avoid collapsing domain, schedule, and flat
  offset into one ad hoc structure
- even the current vector-only compiler benefits from keeping extent and base
  offset ideas explicit

## Fold aliasing lesson from `distributed_fold_design.txt`

The distributed fold note highlights that folds become tricky when accumulator
and inputs can alias.

Key lesson for `tccq`:

- when more advanced fold lowering arrives, aliasing of accumulator/result/input
  storage must be an explicit concern
- future in-place fold strategies must be planned, not assumed

## Test lessons from `tests/reusecand`, `tests/with-loops`, `tests/wlir`

The tests are useful not because of their exact framework style, but because of
what they choose to pin down.

### `tests/reusecand`

Examples:

- `test-wrci-rc.sac`
- `test-wrci-prc.sac`

These check that particular reuse candidates are identified by compilation.

Lesson:

- some tests should validate compiler decisions, not only final numerics
- in `tccq`, prefer structured plan checks over grepping emitted strings when
  possible

### `tests/with-loops`

Examples:

- `test-aud-fold.sac`
- `test-aud-genarray.sac`
- `test-aud-modarray.sac`

These show that one with-loop domain can naturally drive multiple outputs and
multiple operators.

Lesson:

- a future `tccq` multi-result kernel form would be very natural and very
  SaC-like

### `tests/wlir/test-wlir-shadow.sac`

This is about shadowed dependence in nested WL structures.

Lesson:

- name shadowing and dependence depth are worth making explicit in tests
- if `tccq` gets more nested-domain rewriting, dependence metadata will matter

## What `tccq` should copy

- semantic staging: keep important meaning in IR and plans before C emission
- explicit domain/index/shape facts
- conservative reuse inference based on access patterns
- explicit fusion/scalarization as middle-end rewrites
- the idea that one traversal can drive multiple results
- strong small tests that pin down specific compiler claims

## What `tccq` should avoid

- importing the giant M4 + CPP dispatch machinery
- pushing ownership/shape/reuse meaning into macro names instead of IR facts
- coupling the compiler too tightly to a large runtime descriptor model
- using target-C cleverness to discover semantics that should have been decided
  earlier

## Concrete implications for `tccq`

## High-value next middle-end passes

### 1. `tccq_pass_index_normalize()`

Responsibility:

- normalize views/indexing into explicit metadata such as:
  - base object
  - lower bound
  - upper bound
  - extent
  - maybe later stride
  - maybe later flat-offset seed/step

Why:

- aligns with `wlidxs` and IVE-style lessons
- reduces codegen-side heuristics
- should prevent more slice/view extent bugs of the kind already observed

### 2. `tccq_pass_shape_equiv()`

Responsibility:

- infer equivalence classes or explicit facts such as:
  - same extent
  - same domain
  - same shape class

Why:

- aligns with `Shape Cliques`
- would help fusion legality, zip legality, and future offset reuse

### 3. `tccq_pass_reuse_plan()`

Responsibility:

- move beyond `owned` / `alias` / `view` into explicit reuse decisions, e.g.:
  - reusable destination candidate
  - blocked by write
  - blocked by boundary
  - blocked by access pattern
  - copy-on-write required
  - return materialization required

Why:

- aligns with `ReuseWithArrays.c`, implicit memory management, and EMR
- would make allocation and materialization planning more than a conservative
  placeholder

### 4. Future multi-result kernel IR

Responsibility:

- let one domain drive more than one result:
  - multiple materialized arrays
  - multiple folds
  - array + fold combinations

Why:

- matches real SaC with-loop structure
- would matter for one-pass analytics and richer array kernels

## Concrete test ideas for `tccq`

- plan-level tests for reuse candidates and reuse blockers
- tests for shape/domain equivalence facts
- tests for nested views and offset reuse/normalization
- multi-result kernel witness tests once such IR exists
- dependence/shadowing tests if nested kernel/domain rewriting grows

## Suggested reading order for future work

If revisiting the papers in depth, a good order is:

1. *Single Assignment C — Efficient Support for High-level Array Operations in a Functional Setting* (2003)
2. *On Code Generation for Multi-generator With-loops in Sac* (2000)
3. *With-loop Scalarization: Merging Nested Array Operations* (2004)
4. *With-loop Fusion for Data Locality and Parallelism* (2006)
5. *Shape Cliques* (2006)
6. *Index Vector Elimination: Making Index Vectors Affordable* (2007)
7. *Implicit Memory Management for Sac* (2004)
8. *Extended Memory Reuse* (2018)
9. *In-Place-Folding of Non-Scalar Hyper-Planes of Multi-Dimensional Arrays* (2022)
10. *Parallel Scan as a Multidimensional Array Problem* (2022)
11. *Rank-Polymorphism for Shape-Guided Blocking* (2023)

## Current recommendation for `tccq`

The next serious SAC-inspired step is not “more fusion” in the abstract.

It is:

- explicit index normalization,
- explicit shape/domain equivalence facts,
- and a real reuse/materialization plan.

That is the part of SaC that best matches the current gap between `tccq`'s
architecture and its implementation depth.
