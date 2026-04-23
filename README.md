
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tccquickr

Experimental backend-neutral R code transformation framework on top of
`Rtinycc`.

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml)
[![tccquickr status
badge](https://sounkou-bioinfo.r-universe.dev/tccquickr/badges/version)](https://sounkou-bioinfo.r-universe.dev/tccquickr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Abstract

`tccquickr` is an experimental compiler and transformation framework for
a small, declared R subset.

The current design direction is centered on the `tccq_*` path:

- frontend parsing and typed lowering for `declare(type(...))`-annotated
  R
- explicit middle-end IR for producers, materialization, folds,
  statements, and legality barriers
- C emission through the R C API
- swappable C backends, with source-only output, TinyCC via
  [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc), and
  shared-library compilation through `R CMD SHLIB`, in the same general
  deployment space as
  [`callme`](https://github.com/coolbutuseless/callme)

The package depends on
[`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) for the
underlying TinyCC toolchain, libtcc runtime, FFI compilation pipeline,
and pointer/runtime support.

Conceptually, the project sits near several adjacent efforts:

- [`quickr`](https://github.com/t-kalinowski/quickr) as a useful R
  subset compiler comparison
- [`anvil`](https://github.com/r-xla/anvil) as a broader transformation
  and backend framework comparison
- [`callme`](https://github.com/coolbutuseless/callme) as a useful
  reference for C-only shared-library compilation/loading workflows
- [`SAC` / `sac2c`](https://sac-home.org/about%3Asac) as the main
  inspiration for explicit array IR, fusion, and allocation reduction

## Scope

`tccquickr` is where the transformation framework lives.

The main moving parts are:

- `tccq_compile()`
- backend factories such as `tccq_backend_source()`,
  `tccq_backend_tinycc()`, and `tccq_backend_shlib()`
- typed frontend parsing and lowering
- middle-end kernel IR and rewrite passes
- C + R C API target emission
- tinytest coverage for generated code and compiled behavior

[`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) remains
responsible for:

- bundled TinyCC build and installation
- `tcc_ffi()`, `tcc_bind()`, and `tcc_compile()`
- low-level state and CLI helpers
- pointer, struct, callback, and declarative FFI support

Shared-library deployment through `R CMD SHLIB` is a separate C backend
path in `tccquickr`, closer in spirit to
[`callme`](https://github.com/coolbutuseless/callme) than to `Rtinycc`’s
TinyCC runtime responsibilities.

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

## Current `tccq` compiler

`tccq_*` is the current compiler architecture.

The main design rule is that the compiler knows more while it is still
working with typed R-level IR than after it has emitted C. So legality,
alias/view semantics, mutation barriers, fallback boundaries, and
materialization choices should live in the frontend or middle-end
whenever possible, with the C target mostly consuming those decisions.

### Current status

Implemented now:

- [x] declared scalar and vector inputs
- [x] `double`, `integer`, and `logical` typing
- [x] scalar arithmetic
- [x] vector elementwise arithmetic
- [x] unary math calls such as `sin()`, `cos()`, `exp()`, `log()`, and
  `sqrt()`
- [x] `sum(...)` as a fused reduction
- [x] explicit kernel IR nodes for `domain`, `producer`, `materialize`,
  `fold`, and `scalar_kernel`
- [x] statement/program IR for local bindings and writes
- [x] explicit contiguous view IR via `view1`
- [x] scalar indexed reads `x[i]`
- [x] contiguous range slices `x[lo:hi]`
- [x] local indexed writes `y[i] <- v` and local range writes
  `y[lo:hi] <- v`
- [x] explicit fallback boundary nodes in `fallback = "auto"`
- [x] conservative storage, allocation, and protect planning passes
- [x] compile-time backend/target capability checks for context fields
  and boundary APIs
- [x] source-only, TinyCC-backed, and shared-library (`R CMD SHLIB`)
  backends

Planned next:

- [ ] richer semantic allocation/reuse planning before C emission
- [ ] clearer middle-end ownership of boundary argument materialization
- [ ] more systematic view/index normalization before target codegen
- [ ] broader indexing forms such as gather/scatter/filter
- [ ] additional C backends beyond the current source-only,
  TinyCC-backed, and shared-library path

### Current architecture split

- [x] frontend: parse `declare(type(...))` and lower a small R subset
- [x] middle-end: validate IR, infer effects, kernelize, fuse, handle
  boundaries, and derive conservative plans
- [x] target: emit C using the R C API
- [x] backend: return source or compile emitted C through TinyCC or
  `R CMD SHLIB`
- [ ] middle-end: richer plan-driven allocation/reuse and view/index
  normalization
- [ ] backend expansion toward other C compilation/loading paths such as
  additional system compiler or `callme`-style workflows

### Backend selection

`tccq_compile()` accepts explicit backend objects:

- `tccq_backend_source()` returns emitted C without compiling it
- `tccq_backend_tinycc()` compiles emitted C in memory through `Rtinycc`
- `tccq_backend_shlib()` compiles emitted C through `R CMD SHLIB`

That shared-library path is in the same general deployment space as
[`callme`](https://github.com/coolbutuseless/callme): a C-only
compile/load workflow around `.Call()` entry points. Internally,
`tccq_compile()` validates backend capabilities against the target,
compile context, and explicit boundary APIs before compilation.

### Kernel IR concepts

`producer`  
an element computation over an index domain

`materialize(producer)`  
allocate an output vector and write producer elements into it

`fold(op, domain, elem)`  
reduce a producer-like element expression over a loop domain

These are the concepts that let the package move from “fusion hidden in
code emission” toward “fusion represented in IR”.

### Assignment and slicing rules

The current compiler treats these forms differently:

- `a <- expr` is a local binding, not mutation
- `x[i]` is an indexed read with checked 1-based indexing
- `x[lo:hi]` is a contiguous slice expression
- `a[i] <- v` and `a[lo:hi] <- v` are mutation barriers

For the current milestone, indexed writes are only allowed into local
vector bindings. Direct formal mutation such as `x[i] <- v` is rejected.

### Example: inspect fresh IR

``` r
fresh_sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}

fresh_vec_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sin(x) + y * y
}

sum_module <- tccq_compile(fresh_sum_kernel, mode = "ir")
vec_module <- tccq_compile(fresh_vec_kernel, mode = "ir")

sum_module
#> <tccq_module>
#>   entry: tccq_entry 
#>   formals:
#>    - x : double[NA] 
#>    - y : double[NA] 
#>   ir: program 
#>   kernel: fold 
#>   result type: double
vec_module
#> <tccq_module>
#>   entry: tccq_entry 
#>   formals:
#>    - x : double[NA] 
#>    - y : double[NA] 
#>   ir: program 
#>   kernel: materialize 
#>   result type: double[NA]
```

The reduction path is represented as a `fold` over an explicit
`producer`. The vector return path is represented as
`materialize(producer)`.

### Example: explicit fusion rewrite

Before fusion, `kernelize` represents the reduction as a fold over a
materialized producer. The fusion pass rewrites that to a fold over the
producer directly.

``` r
mod0 <- tccquickr:::tccq_frontend(fresh_sum_kernel)
mod0 <- tccquickr:::tccq_lower_module(mod0)

before_fusion <- tccquickr:::tccq_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq_pass_validate_ir(),
    tccquickr:::tccq_pass_effects(),
    tccquickr:::tccq_pass_kernelize()
  )
)

after_fusion <- tccquickr:::tccq_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq_pass_validate_ir(),
    tccquickr:::tccq_pass_effects(),
    tccquickr:::tccq_pass_kernelize(),
    tccquickr:::tccq_pass_fusion()
  )
)

before_fusion
#> <tccq_module>
#>   entry: tccq_entry 
#>   formals:
#>    - x : double[NA] 
#>    - y : double[NA] 
#>   ir: program 
#>   kernel: fold 
#>   result type: double
after_fusion
#> <tccq_module>
#>   entry: tccq_entry 
#>   formals:
#>    - x : double[NA] 
#>    - y : double[NA] 
#>   ir: program 
#>   kernel: fold 
#>   result type: double
```

This is still a small rewrite system, but the important point is that
fusion is now expressed as a middle-end transformation rather than being
buried entirely inside C string emission.

### Example: explicit boundary fallback

Boundary nodes stay explicit legality barriers, but `fallback = "auto"`
now lowers unsupported calls to explicit `r_eval` boundary nodes instead
of failing immediately during lowering.

``` r
fallback_kernel <- function(x) {
  declare(type(x = double()))
  floor(x)
}

fallback_ir <- tccq_compile(fallback_kernel, mode = "ir", fallback = "auto")
fallback_ir$ir$result$tag
#> [1] "boundary_call"
fallback_ir$ir$result$api
#> [1] "r_eval"
```

In `fallback = "hard"` mode, the same unsupported call is rejected.

``` r
tryCatch(
  tccq_compile(fallback_kernel, mode = "ir", fallback = "hard"),
  error = function(e) e$message
)
#> [1] "unsupported call in fresh compiler: floor. Add a lowerer case or route it through an explicit boundary node."
```

### Example: assignments and indexed writes

``` r
assign_kernel <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

assign_src <- tccq_compile(assign_kernel, mode = "code")
tccq_c_block(assign_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif

static R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_i, SEXP arg_v) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *p_x = REAL_RO(arg_x);
  if (TYPEOF(arg_i) != INTSXP) {
    Rf_error("argument %s has wrong R type", "i");
  }
  if (XLENGTH(arg_i) < 1) {
    Rf_error("scalar argument %s is empty", "i");
  }
  int v_i = INTEGER_RO(arg_i)[0];
  if (TYPEOF(arg_v) != REALSXP) {
    Rf_error("argument %s has wrong R type", "v");
  }
  if (XLENGTH(arg_v) < 1) {
    Rf_error("scalar argument %s is empty", "v");
  }
  double v_v = REAL_RO(arg_v)[0];
  int tccq_nprotect = 0;
  R_xlen_t n_y = n_x;
  SEXP loc_y = R_NilValue;
  double *p_y = (double *) p_x;
  int own_y = 0;
  if (!own_y) {
    loc_y = PROTECT(Rf_allocVector(REALSXP, n_y));
    ++tccq_nprotect;
    double *tmp_y = REAL(loc_y);
    for (R_xlen_t i = 0; i < n_y; ++i) tmp_y[i] = p_y[i];
    p_y = tmp_y;
    own_y = 1;
  }
  R_xlen_t j_y = tccq_checked_index1((R_xlen_t)(v_i), n_y, "y");
  p_y[j_y] = (double)(v_v);
  if (!own_y) {
    loc_y = PROTECT(Rf_allocVector(REALSXP, n_y));
    ++tccq_nprotect;
    double *tmp_y = REAL(loc_y);
    for (R_xlen_t i = 0; i < n_y; ++i) tmp_y[i] = p_y[i];
    p_y = tmp_y;
    own_y = 1;
  }
  UNPROTECT(tccq_nprotect);
  return loc_y;
}
```

The local `y <- x` binding now starts as an alias to the formal input.
The first indexed write materializes `y` into owned local storage, so
mutate-then- return allocates only once while still keeping direct
writes to caller-owned formal vectors out of scope for this milestone.

### Example: slicing and reduction

``` r
slice_sum_kernel <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  sum(y)
}

slice_sum_src <- tccq_compile(slice_sum_kernel, mode = "code")
tccq_c_block(slice_sum_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif

static R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_lo, SEXP arg_hi) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *p_x = REAL_RO(arg_x);
  if (TYPEOF(arg_lo) != INTSXP) {
    Rf_error("argument %s has wrong R type", "lo");
  }
  if (XLENGTH(arg_lo) < 1) {
    Rf_error("scalar argument %s is empty", "lo");
  }
  int v_lo = INTEGER_RO(arg_lo)[0];
  if (TYPEOF(arg_hi) != INTSXP) {
    Rf_error("argument %s has wrong R type", "hi");
  }
  if (XLENGTH(arg_hi) < 1) {
    Rf_error("scalar argument %s is empty", "hi");
  }
  int v_hi = INTEGER_RO(arg_hi)[0];
  int tccq_nprotect = 0;
  R_xlen_t lo_n_y = tccq_checked_index1((R_xlen_t)(v_lo), n_x, "x");
  R_xlen_t hi_n_y = tccq_checked_index1((R_xlen_t)(v_hi), n_x, "x");
  if (hi_n_y < lo_n_y) { Rf_error("decreasing slices are not supported"); }
  R_xlen_t n_n_y = hi_n_y - lo_n_y + 1;
  R_xlen_t n_y = n_n_y;
  SEXP loc_y = R_NilValue;
  double *p_y = (double *) (p_x + lo_n_y);
  int own_y = 0;
  R_xlen_t n_out = n_y;
  double acc = 0.0;
  for (R_xlen_t i = 0; i < n_out; ++i) {
    acc += (double)(p_y[i]);
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
  ++tccq_nprotect;
  REAL(out)[0] = acc;
  UNPROTECT(tccq_nprotect);
  return out;
}
```

The slice bind now lowers to an explicit `view1` node. In the current
target, that means the compiler can bind a pointer/length view and
reduce over it without allocating a copied slice first.

### Example: direct formal mutation is rejected

``` r
direct_formal_mutation <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  x[1] <- v
  x
}

tryCatch(
  tccq_compile(direct_formal_mutation, mode = "code"),
  error = function(e) e$message
)
#> [1] "indexed assignment currently requires a local vector binding. Write y <- x; y[i] <- value; y instead of mutating formal 'x' directly."
```

### Example: generate C source

``` r
sum_src <- tccq_compile(fresh_sum_kernel, mode = "code")
tccq_c_block(sum_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif

static R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_y) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *p_x = REAL_RO(arg_x);
  if (TYPEOF(arg_y) != REALSXP) {
    Rf_error("argument %s has wrong R type", "y");
  }
  R_xlen_t n_y = XLENGTH(arg_y);
  const double *p_y = REAL_RO(arg_y);
  int tccq_nprotect = 0;
  R_xlen_t n_out = n_x;
  if (n_y != n_out) {
    Rf_error("vector length mismatch for %s", "y");
  }
  double acc = 0.0;
  for (R_xlen_t i = 0; i < n_out; ++i) {
    acc += (double)(((((sin((double)(p_x[i]))) + (p_y[i]))) * (p_y[i])));
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
  ++tccq_nprotect;
  REAL(out)[0] = acc;
  UNPROTECT(tccq_nprotect);
  return out;
}
```

The generated C contains one reduction loop and no intermediate vector
allocation for the reduction itself.

### Example: vector materialization kernel

``` r
vec_src <- tccq_compile(fresh_vec_kernel, mode = "code")
tccq_c_block(vec_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif

static R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_y) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *p_x = REAL_RO(arg_x);
  if (TYPEOF(arg_y) != REALSXP) {
    Rf_error("argument %s has wrong R type", "y");
  }
  R_xlen_t n_y = XLENGTH(arg_y);
  const double *p_y = REAL_RO(arg_y);
  int tccq_nprotect = 0;
  R_xlen_t n_out = n_x;
  if (n_y != n_out) {
    Rf_error("vector length mismatch for %s", "y");
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n_out));
  ++tccq_nprotect;
  double *p_out = REAL(out);
  for (R_xlen_t i = 0; i < n_out; ++i) {
    p_out[i] = (double)(((sin((double)(p_x[i]))) + (((p_y[i]) * (p_y[i])))));
  }
  UNPROTECT(tccq_nprotect);
  return out;
}
```

This path allocates one output vector and fills it in one loop.

### Optional: compile through TinyCC

``` r
if (requireNamespace("Rtinycc", quietly = TRUE)) {
  compiled_sum <- tccq_compile(fresh_sum_kernel)
  compiled_vec <- tccq_compile(fresh_vec_kernel)
  compiled_fallback <- tccq_compile(fallback_kernel, fallback = "auto")
  compiled_assign <- tccq_compile(assign_kernel)
  compiled_slice_sum <- tccq_compile(slice_sum_kernel)

  x <- as.double(seq(-2, 2, length.out = 10))
  y <- as.double(seq(1, 3, length.out = 10))

  list(
    compiled_sum = compiled_sum(x, y),
    compiled_vec_head = unname(compiled_vec(x, y)[1:4]),
    compiled_fallback = compiled_fallback(42),
    compiled_assign = unname(compiled_assign(c(1, 2, 3), 2L, 10)),
    compiled_slice_sum = compiled_slice_sum(c(1, 2, 3, 4), 2L, 4L)
  )
}
#> $compiled_sum
#> [1] 48.90504
#> 
#> $compiled_vec_head
#> [1] 0.09070257 0.49394330 1.19022755 2.15940797
#> 
#> $compiled_fallback
#> [1] 42
#> 
#> $compiled_assign
#> [1]  1 10  3
#> 
#> $compiled_slice_sum
#> [1] 9
```

## Related projects and influences

`tccquickr` is not trying to be identical to any one upstream project,
but its current direction is easier to understand relative to a few
concrete references.

### `quickr`

[`quickr`](https://github.com/t-kalinowski/quickr) is a useful
comparison for a declared R subset compiler. It demonstrates that a
strict, typed subset of R can be lowered aggressively and made fast.

### `anvil`

[`anvil`](https://github.com/r-xla/anvil) is a useful comparison for
explicit transformation architecture and backend thinking. It is broader
than the goals here, but it is a good reference point for separating
frontend and backend concerns.

### `SAC` and `sac2c`

[`SAC`](https://sac-home.org/about%3Asac) and the
[`sac2c`](https://gitlab.sac-home.org/sac-group/sac2c) compiler are the
main optimization inspiration for the current path here:

- explicit array/kernel IR
- fusion as an IR rewrite rather than a codegen accident
- allocation reduction and materialization discipline
- legality-driven optimization boundaries

## Status

This package is experimental.

The split from [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc)
is about separation of concerns:

- [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) provides the
  TinyCC / FFI runtime layer
- `tccquickr` provides the compiler and transformation framework
- `tccq_*` is the active architecture for explicit IR, mutation
  barriers, slicing/indexing semantics, and backend-neutral compilation
  flow

## Development

`tccquickr` follows the same repo conventions as the parent package:

- `README.Rmd` as the source of the GitHub README
- `roxygen2` for documentation generation
- `tinytest` under `inst/tinytest`
- a simple package-local `Makefile` for build/check/test tasks
