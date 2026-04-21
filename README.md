
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

The current design direction is centered on the fresh `tccq2_*` path:

- frontend parsing and typed lowering for `declare(type(...))`-annotated
  R
- explicit middle-end IR for producers, materialization, folds,
  statements, and legality barriers
- C emission through the R C API
- swappable backends, with source-only output and TinyCC via
  [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc)

The package depends on
[`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) for the
underlying TinyCC toolchain, libtcc runtime, FFI compilation pipeline,
and pointer/runtime support.

## Scope

`tccquickr` is where the transformation framework lives.

The main moving parts are:

- `tccq2_compile()`
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

## Fresh `tccq2` scaffold

The fresh path is deliberately small in the first milestones.

Milestone 1 / 2 supports:

- declared scalar and vector inputs
- `double`, `integer`, and `logical` typing
- scalar arithmetic
- vector elementwise arithmetic
- unary math calls such as `sin()`, `cos()`, `exp()`, `log()`, and
  `sqrt()`
- `sum(...)` as a fused reduction
- explicit kernel IR nodes for `producer`, `materialize`, and `fold`
- statement blocks with local bindings
- scalar indexed reads `x[i]`
- contiguous range slices `x[lo:hi]`
- local indexed writes `y[i] <- v` and local range writes
  `y[lo:hi] <- v`

The intended split is:

- frontend: parse `declare(type(...))` and lower a small R subset
- middle-end: validate, kernelize, and later perform explicit fusion and
  allocation planning
- target: emit C using the R C API
- backend: either return source or compile through TinyCC via `Rtinycc`

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

The fresh compiler treats these forms differently:

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

sum_module <- tccq2_compile(fresh_sum_kernel, mode = "ir")
vec_module <- tccq2_compile(fresh_vec_kernel, mode = "ir")

list(
  sum_kernel_tag = sum_module$kernel$tag,
  sum_domain_tag = sum_module$kernel$domain$tag,
  sum_elem_tag = sum_module$kernel$elem$tag,
  vec_kernel_tag = vec_module$kernel$tag,
  vec_producer_tag = vec_module$kernel$producer$tag
)
#> $sum_kernel_tag
#> [1] "fold"
#> 
#> $sum_domain_tag
#> [1] "domain"
#> 
#> $sum_elem_tag
#> [1] "producer"
#> 
#> $vec_kernel_tag
#> [1] "materialize"
#> 
#> $vec_producer_tag
#> [1] "producer"
```

The reduction path is represented as a `fold` over an explicit
`producer`. The vector return path is represented as
`materialize(producer)`.

### Example: explicit fusion rewrite

Before fusion, `kernelize` represents the reduction as a fold over a
materialized producer. The fusion pass rewrites that to a fold over the
producer directly.

``` r
mod0 <- tccquickr:::tccq2_frontend(fresh_sum_kernel)
mod0 <- tccquickr:::tccq2_lower_module(mod0)

before_fusion <- tccquickr:::tccq2_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq2_pass_validate_ir(),
    tccquickr:::tccq2_pass_effects(),
    tccquickr:::tccq2_pass_kernelize()
  )
)

after_fusion <- tccquickr:::tccq2_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq2_pass_validate_ir(),
    tccquickr:::tccq2_pass_effects(),
    tccquickr:::tccq2_pass_kernelize(),
    tccquickr:::tccq2_pass_fusion()
  )
)

list(
  before = before_fusion$kernel$elem$tag,
  after = after_fusion$kernel$elem$tag
)
#> $before
#> [1] "materialize"
#> 
#> $after
#> [1] "producer"
```

This is still a small rewrite system, but the important point is that
fusion is now expressed as a middle-end transformation rather than being
buried entirely inside C string emission.

### Example: legality barriers

Boundary nodes are explicit legality barriers. The fresh compiler does
not yet fallback through them; it refuses to compile them.

``` r
boundary_ir <- tccquickr:::tccq2_ir_boundary(
  kind = "rf_call",
  reason = "unsupported R call",
  input = tccquickr:::tccq2_ir_var("x", tccquickr:::tccq2_type_vector("double")),
  type = tccquickr:::tccq2_type_vector("double")
)

boundary_module <- tccquickr:::tccq2_module(
  entry = "tccq2_entry",
  formal_names = c("x"),
  types = list(x = tccquickr:::tccq2_type_vector("double")),
  expr = quote(x),
  ir = boundary_ir
)

tryCatch(
  tccquickr:::tccq2_run_passes(boundary_module),
  error = function(e) e$message
)
#> [1] "boundary nodes are explicit legality barriers in tccq2; later milestones may lower them, but milestone 2 refuses to compile them"
```

### Example: assignments and indexed writes

``` r
assign_kernel <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}

assign_src <- tccq2_compile(assign_kernel, mode = "code")
cat(assign_src)
#> #include <R.h>
#> #include <Rinternals.h>
#> #include <Rmath.h>
#> #include <math.h>
#> 
#> #ifndef REAL_RO
#> #define REAL_RO(x) REAL(x)
#> #endif
#> #ifndef INTEGER_RO
#> #define INTEGER_RO(x) INTEGER(x)
#> #endif
#> #ifndef LOGICAL_RO
#> #define LOGICAL_RO(x) LOGICAL(x)
#> #endif
#> 
#> static R_xlen_t tccq2_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
#>   if (idx < 1 || idx > len) {
#>     Rf_error("index out of bounds for %s", name);
#>   }
#>   return idx - 1;
#> }
#> 
#> SEXP tccq2_entry(SEXP arg_x, SEXP arg_i, SEXP arg_v) {
#>   if (TYPEOF(arg_x) != REALSXP) {
#>     Rf_error("argument %s has wrong R type", "x");
#>   }
#>   R_xlen_t n_x = XLENGTH(arg_x);
#>   const double *p_x = REAL_RO(arg_x);
#>   if (TYPEOF(arg_i) != INTSXP) {
#>     Rf_error("argument %s has wrong R type", "i");
#>   }
#>   if (XLENGTH(arg_i) < 1) {
#>     Rf_error("scalar argument %s is empty", "i");
#>   }
#>   int v_i = INTEGER_RO(arg_i)[0];
#>   if (TYPEOF(arg_v) != REALSXP) {
#>     Rf_error("argument %s has wrong R type", "v");
#>   }
#>   if (XLENGTH(arg_v) < 1) {
#>     Rf_error("scalar argument %s is empty", "v");
#>   }
#>   double v_v = REAL_RO(arg_v)[0];
#>   int tccq2_nprotect = 0;
#>   R_xlen_t n_y = n_x;
#>   SEXP loc_y = PROTECT(Rf_allocVector(REALSXP, n_y));
#>   ++tccq2_nprotect;
#>   double *p_y = REAL(loc_y);
#>   for (R_xlen_t i = 0; i < n_y; ++i) {
#>     p_y[i] = (double)(p_x[i]);
#>   }
#>   R_xlen_t j_y = tccq2_checked_index1((R_xlen_t)(v_i), n_y, "y");
#>   p_y[j_y] = (double)(v_v);
#>   R_xlen_t n_out = n_y;
#>   SEXP out = PROTECT(Rf_allocVector(REALSXP, n_out));
#>   ++tccq2_nprotect;
#>   double *p_out = REAL(out);
#>   for (R_xlen_t i = 0; i < n_out; ++i) {
#>     p_out[i] = (double)(p_y[i]);
#>   }
#>   UNPROTECT(tccq2_nprotect);
#>   return out;
#> }
```

The local `y <- x` binding is materialized into owned local storage
before the indexed write. That keeps direct writes to caller-owned
formal vectors out of this milestone.

### Example: slicing and reduction

``` r
slice_sum_kernel <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  sum(x[lo:hi])
}

slice_sum_src <- tccq2_compile(slice_sum_kernel, mode = "code")
cat(slice_sum_src)
#> #include <R.h>
#> #include <Rinternals.h>
#> #include <Rmath.h>
#> #include <math.h>
#> 
#> #ifndef REAL_RO
#> #define REAL_RO(x) REAL(x)
#> #endif
#> #ifndef INTEGER_RO
#> #define INTEGER_RO(x) INTEGER(x)
#> #endif
#> #ifndef LOGICAL_RO
#> #define LOGICAL_RO(x) LOGICAL(x)
#> #endif
#> 
#> static R_xlen_t tccq2_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
#>   if (idx < 1 || idx > len) {
#>     Rf_error("index out of bounds for %s", name);
#>   }
#>   return idx - 1;
#> }
#> 
#> SEXP tccq2_entry(SEXP arg_x, SEXP arg_lo, SEXP arg_hi) {
#>   if (TYPEOF(arg_x) != REALSXP) {
#>     Rf_error("argument %s has wrong R type", "x");
#>   }
#>   R_xlen_t n_x = XLENGTH(arg_x);
#>   const double *p_x = REAL_RO(arg_x);
#>   if (TYPEOF(arg_lo) != INTSXP) {
#>     Rf_error("argument %s has wrong R type", "lo");
#>   }
#>   if (XLENGTH(arg_lo) < 1) {
#>     Rf_error("scalar argument %s is empty", "lo");
#>   }
#>   int v_lo = INTEGER_RO(arg_lo)[0];
#>   if (TYPEOF(arg_hi) != INTSXP) {
#>     Rf_error("argument %s has wrong R type", "hi");
#>   }
#>   if (XLENGTH(arg_hi) < 1) {
#>     Rf_error("scalar argument %s is empty", "hi");
#>   }
#>   int v_hi = INTEGER_RO(arg_hi)[0];
#>   int tccq2_nprotect = 0;
#>   R_xlen_t lo_fold = tccq2_checked_index1((R_xlen_t)(v_lo), n_x, "x");
#>   R_xlen_t hi_fold = tccq2_checked_index1((R_xlen_t)(v_hi), n_x, "x");
#>   if (hi_fold < lo_fold) { Rf_error("decreasing slices are not supported"); }
#>   R_xlen_t n_fold = hi_fold - lo_fold + 1;
#>   double acc = 0.0;
#>   for (R_xlen_t i = 0; i < n_fold; ++i) {
#>     acc += (double)(p_x[lo_fold + i]);
#>   }
#>   SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
#>   ++tccq2_nprotect;
#>   REAL(out)[0] = acc;
#>   UNPROTECT(tccq2_nprotect);
#>   return out;
#> }
```

### Example: direct formal mutation is rejected

``` r
direct_formal_mutation <- function(x, v) {
  declare(type(x = double(NA)), type(v = double()))
  x[1] <- v
  x
}

tryCatch(
  tccq2_compile(direct_formal_mutation, mode = "code"),
  error = function(e) e$message
)
#> [1] "indexed assignment currently requires a local vector binding. Write y <- x; y[i] <- value; y instead of mutating formal 'x' directly."
```

### Example: generate C source

``` r
sum_src <- tccq2_compile(fresh_sum_kernel, mode = "code")
cat(sum_src)
#> #include <R.h>
#> #include <Rinternals.h>
#> #include <Rmath.h>
#> #include <math.h>
#> 
#> #ifndef REAL_RO
#> #define REAL_RO(x) REAL(x)
#> #endif
#> #ifndef INTEGER_RO
#> #define INTEGER_RO(x) INTEGER(x)
#> #endif
#> #ifndef LOGICAL_RO
#> #define LOGICAL_RO(x) LOGICAL(x)
#> #endif
#> 
#> static R_xlen_t tccq2_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
#>   if (idx < 1 || idx > len) {
#>     Rf_error("index out of bounds for %s", name);
#>   }
#>   return idx - 1;
#> }
#> 
#> SEXP tccq2_entry(SEXP arg_x, SEXP arg_y) {
#>   if (TYPEOF(arg_x) != REALSXP) {
#>     Rf_error("argument %s has wrong R type", "x");
#>   }
#>   R_xlen_t n_x = XLENGTH(arg_x);
#>   const double *p_x = REAL_RO(arg_x);
#>   if (TYPEOF(arg_y) != REALSXP) {
#>     Rf_error("argument %s has wrong R type", "y");
#>   }
#>   R_xlen_t n_y = XLENGTH(arg_y);
#>   const double *p_y = REAL_RO(arg_y);
#>   int tccq2_nprotect = 0;
#>   R_xlen_t n_out = n_x;
#>   if (n_y != n_out) {
#>     Rf_error("vector length mismatch for %s", "y");
#>   }
#>   double acc = 0.0;
#>   for (R_xlen_t i = 0; i < n_out; ++i) {
#>     acc += (double)(((((sin((double)(p_x[i]))) + (p_y[i]))) * (p_y[i])));
#>   }
#>   SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
#>   ++tccq2_nprotect;
#>   REAL(out)[0] = acc;
#>   UNPROTECT(tccq2_nprotect);
#>   return out;
#> }
```

The generated C contains one reduction loop and no intermediate vector
allocation for the reduction itself.

### Example: vector materialization kernel

``` r
vec_src <- tccq2_compile(fresh_vec_kernel, mode = "code")
cat(vec_src)
#> #include <R.h>
#> #include <Rinternals.h>
#> #include <Rmath.h>
#> #include <math.h>
#> 
#> #ifndef REAL_RO
#> #define REAL_RO(x) REAL(x)
#> #endif
#> #ifndef INTEGER_RO
#> #define INTEGER_RO(x) INTEGER(x)
#> #endif
#> #ifndef LOGICAL_RO
#> #define LOGICAL_RO(x) LOGICAL(x)
#> #endif
#> 
#> static R_xlen_t tccq2_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
#>   if (idx < 1 || idx > len) {
#>     Rf_error("index out of bounds for %s", name);
#>   }
#>   return idx - 1;
#> }
#> 
#> SEXP tccq2_entry(SEXP arg_x, SEXP arg_y) {
#>   if (TYPEOF(arg_x) != REALSXP) {
#>     Rf_error("argument %s has wrong R type", "x");
#>   }
#>   R_xlen_t n_x = XLENGTH(arg_x);
#>   const double *p_x = REAL_RO(arg_x);
#>   if (TYPEOF(arg_y) != REALSXP) {
#>     Rf_error("argument %s has wrong R type", "y");
#>   }
#>   R_xlen_t n_y = XLENGTH(arg_y);
#>   const double *p_y = REAL_RO(arg_y);
#>   int tccq2_nprotect = 0;
#>   R_xlen_t n_out = n_x;
#>   if (n_y != n_out) {
#>     Rf_error("vector length mismatch for %s", "y");
#>   }
#>   SEXP out = PROTECT(Rf_allocVector(REALSXP, n_out));
#>   ++tccq2_nprotect;
#>   double *p_out = REAL(out);
#>   for (R_xlen_t i = 0; i < n_out; ++i) {
#>     p_out[i] = (double)(((sin((double)(p_x[i]))) + (((p_y[i]) * (p_y[i])))));
#>   }
#>   UNPROTECT(tccq2_nprotect);
#>   return out;
#> }
```

This path allocates one output vector and fills it in one loop.

### Optional: compile through TinyCC

``` r
if (requireNamespace("Rtinycc", quietly = TRUE)) {
  compiled_sum <- tccq2_compile(fresh_sum_kernel)
  compiled_vec <- tccq2_compile(fresh_vec_kernel)

  x <- as.double(seq(-2, 2, length.out = 10))
  y <- as.double(seq(1, 3, length.out = 10))

  list(
    compiled_sum = compiled_sum(x, y),
    compiled_vec_head = unname(compiled_vec(x, y)[1:4])
  )
}
#> $compiled_sum
#> [1] 48.90504
#> 
#> $compiled_vec_head
#> [1] 0.09070257 0.49394330 1.19022755 2.15940797
```

## Status

This package is experimental.

The split from [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc)
is about separation of concerns:

- [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) provides the
  TinyCC / FFI runtime layer
- `tccquickr` provides the compiler and transformation framework
- `tccq2_*` is the active architecture for explicit IR, mutation
  barriers, slicing/indexing semantics, and backend-neutral compilation
  flow

## Development

`tccquickr` follows the same repo conventions as the parent package:

- `README.Rmd` as the source of the GitHub README
- `roxygen2` for documentation generation
- `tinytest` under `inst/tinytest`
- a simple package-local `Makefile` for build/check/test tasks
