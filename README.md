
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tccquickr

Experimental `tcc_quick()` front-end work on top of `Rtinycc`, plus a
fresh backend-neutral compiler scaffold under `tccq2_*`.

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml)
[![tccquickr status
badge](https://sounkou-bioinfo.r-universe.dev/tccquickr/badges/version)](https://sounkou-bioinfo.r-universe.dev/tccquickr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Abstract

`tccquickr` contains two related pieces of work:

- the existing experimental `tcc_quick()` prototype and its older parser
  / IR / lowering / code-generation path
- a fresh-start `tccq2_*` compiler scaffold that keeps backend choice
  explicit and starts introducing middle-end concepts directly

The package is intentionally scoped as the compiler-front-end
experiment. It depends on
[`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) for the
underlying TinyCC toolchain, libtcc runtime, FFI compilation pipeline,
and pointer/runtime support.

## Scope

`tccquickr` currently contains:

- `tcc_quick()`
- `tcc_quick_ops()`
- the older internal IR and lowering/codegen helpers
- the fresh `tccq2_compile()` scaffold
- transpiler-focused `tinytest` coverage

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
#>   R_xlen_t n_out = n_x;
#>   if (n_y != n_out) {
#>     Rf_error("vector length mismatch for %s", "y");
#>   }
#>   double acc = 0.0;
#>   for (R_xlen_t i = 0; i < n_out; ++i) {
#>     acc += (double)(((((sin((double)(p_x[i]))) + (p_y[i]))) * (p_y[i])));
#>   }
#>   SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
#>   REAL(out)[0] = acc;
#>   UNPROTECT(1);
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
#>   R_xlen_t n_out = n_x;
#>   if (n_y != n_out) {
#>     Rf_error("vector length mismatch for %s", "y");
#>   }
#>   SEXP out = PROTECT(Rf_allocVector(REALSXP, n_out));
#>   double *p_out = REAL(out);
#>   for (R_xlen_t i = 0; i < n_out; ++i) {
#>     p_out[i] = (double)(((sin((double)(p_x[i]))) + (((p_y[i]) * (p_y[i])))));
#>   }
#>   UNPROTECT(1);
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

This package is experimental. The split from
[`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) is primarily
about semantic clarity and package soundness:

- [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) should read as
  the TinyCC/FFI package
- `tccquickr` should carry the separate R-to-C lowering experiment
- the fresh `tccq2_*` path is where explicit producer/fold/materialize
  ideas can be grown without disturbing the older prototype

## Development

`tccquickr` follows the same repo conventions as the parent package:

- `README.Rmd` as the source of the GitHub README
- `roxygen2` for documentation generation
- `tinytest` under `inst/tinytest`
- a simple package-local `Makefile` for build/check/test tasks
