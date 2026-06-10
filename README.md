
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tccquickr

An experimental, **optimizing R-to-C transpiler** for a small, declared
subset of R, built on top of
[`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc).

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml)
[![tccquickr status
badge](https://sounkou-bioinfo.r-universe.dev/tccquickr/badges/version)](https://sounkou-bioinfo.r-universe.dev/tccquickr/badges/version)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

You annotate a function’s argument types with `declare(type(...))`;
`tccq_compile()` lowers it to a typed IR, runs middle-end optimization
passes, emits C, and returns a compiled closure you call like any R
function.

``` r
sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

compiled <- tccq_compile(sum_kernel)
compiled(seq(-2, 2, length.out = 10), seq(1, 3, length.out = 10))
#> [1] 48.90504
```

See `vignette("getting-started")` for a tour, and
`vignette("the-r-subset")` for the exact accepted subset, the boundary
model, and the optimization passes.

## The framework

`tccquickr` is a transformation pipeline, not a single “R expression to
C string” step. Meaning is kept high and lowered deliberately:

    declare()'d R  ->  typed IR  ->  middle-end passes  ->  [lowered IR]  ->  C target  ->  backend
                       (ranks,        (shape domains,        (target-        (R C API)     (TinyCC /
                        effects)       kernelize, fusion,     neutral seam)                 SHLIB / source)
                                       storage/alloc plans)

- **`tccquickr`** owns the frontend, IR, passes, C target, and
  backend-neutral orchestration.
- **`Rtinycc`** owns the TinyCC toolchain, libtcc runtime, and FFI.
  TinyCC is one backend, not the design.

Codegen is organized around a **three-region split**, explicit in the
IR:

- **pure-C kernel** — plain C math/loops, no `Rf_*` in the hot path; the
  only region optimization targets (including future copy-and-patch
  kernel work);
- **C-API wrapper** — argument/type checks and boundary plumbing;
- **boundary** — every unsupported/dynamic call as an explicit `r_eval`
  node and legality barrier.

The current target is C; the lowered-IR seam exists so the same
decisions can later drive other targets (Fortran/Rust/devices).
Direction is recorded in the architecture decision records under
[`docs/decisions/`](docs/decisions/).

## Goals

- Optimize R array programs at a **semantically meaningful** level
  (ranks, ownership, shape domains, the elementwise domain) *before*
  committing to any target syntax — the lesson borrowed from SAC /
  `sac2c`.
- Keep unsupported semantics **explicit** as boundaries rather than
  hiding them in target special cases.
- Stay **target-neutral** at the IR so retargeting does not mean a
  rewrite.
- Be **honest**: the accepted subset and the correctness claims are
  executable and checked, not aspirational.

## Verification

Correctness has three layers, with three ground truths (see [ADR
0005](docs/decisions/0005-conformance-and-verification.md)):

- **Grammar coverage** — the accepted subset is a computed map probed
  against the frontend, grounded in R’s grammar
  ([`docs/r-subset-grammar.md`](docs/r-subset-grammar.md)).
- **Conformance** — generated in-subset programs are diffed against the
  R interpreter as oracle, with a tracked pass number that must not
  regress ([`docs/conformance.md`](docs/conformance.md)).
- **Proof** — middle-end/lowering transformations are proved
  meaning-preserving in **Lean 4** (`proofs/`), checked live in the docs
  via the [`leanknit`](https://github.com/sounkou-bioinfo/leanknit)
  engine.

## Key functions

- `tccq_compile()` — compile a declared function (modes: `"compile"`,
  `"code"`, `"ir"`; fallbacks: `"hard"`, `"auto"`).
- `tccq_jit()` — per-signature compile cache.
- `tccq_analyze()` — fast pre-compilation reconnaissance of a function’s
  **AST** (call patterns, dynamic calls, superassignments, free symbols)
  with recommendations.
- `tccq_backend_source()` / `tccq_backend_tinycc()` /
  `tccq_backend_shlib()` — return C, compile in memory via `Rtinycc`, or
  compile via `R CMD SHLIB`.

## Installation

``` r
install.packages(
  "tccquickr",
  repos = c(
    "https://sounkou-bioinfo.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

## Related projects and influences

- [`quickr`](https://github.com/t-kalinowski/quickr) — declared-subset R
  compiler.
- [`anvil`](https://github.com/r-xla/anvl) — explicit transformation
  architecture.
- [`callme`](https://github.com/coolbutuseless/callme) — `.Call()`
  compile/load.
- [`SAC` / `sac2c`](https://sac-home.org/) — explicit array IR, fusion,
  and allocation/materialization discipline (the main inspiration).

## Development

`README.Rmd` generates `README.md`; `roxygen2` generates the man pages;
docs are generated from `.Rmd` (`tools/render_docs.R`); `tinytest` lives
under `inst/tinytest`; a `Makefile` drives build/check/test/docs.
