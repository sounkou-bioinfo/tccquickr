
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tccquickr: an R-to-C transformation framework

Experimental backend-neutral R-to-C transformation framework on top of
`Rtinycc`.

<!-- badges: start -->

[![R-CMD-check](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sounkou-bioinfo/tccquickr/actions/workflows/R-CMD-check.yaml)
[![tccquickr status
badge](https://sounkou-bioinfo.r-universe.dev/tccquickr/badges/version)](https://sounkou-bioinfo.r-universe.dev/tccquickr/badges/version)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## What this package is

`tccquickr` is an experimental R-to-C transformation framework for a
small, declared subset of R.

The package is centered on `tccq_compile()`, which takes
`declare(type(...))`-annotated R code, lowers it into explicit typed IR,
applies middle-end rewrites, emits C through the R C API, and then
either:

- returns generated C source,
- compiles it in memory through TinyCC via `Rtinycc`, or
- compiles it as a shared library through `R CMD SHLIB`.

The public entry points are:

- `tccq_compile()`
- `tccq_backend_source()`
- `tccq_backend_tinycc()`
- `tccq_backend_shlib()`

Responsibility is split deliberately:

- `tccquickr` owns the frontend, IR, passes, C target, and
  backend-neutral orchestration
- [`Rtinycc`](https://github.com/sounkou-bioinfo/Rtinycc) owns the
  TinyCC toolchain, libtcc runtime, FFI pipeline, and low-level native
  runtime helpers

That means `tccquickr` is the transformation layer. TinyCC is one
backend, not the whole design.

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

## Current scope

### Implemented today

- declared scalar, vector, and matrix inputs
- `double`, `integer`, and `logical` typing
- scalar and elementwise arithmetic
- comparison and logical vector expressions
- unary math calls such as `sin()`, `cos()`, `exp()`, `log()`, and
  `sqrt()`
- explicit kernel IR nodes such as `producer`, `materialize`, `fold`,
  and `scalar_kernel`
- statement/program IR for local bindings and writes
- scalar indexed reads `x[i]`
- contiguous slices `x[lo:hi]`
- local indexed writes `y[i] <- v` and local range writes
  `y[lo:hi] <- v`
- scalar matrix reads `x[i, j]`
- scalar, row, column, rectangle, and full-matrix local writes
- scalar `matrix(data, nrow, ncol)` construction
- fold-style reducers `sum`, `prod`, `min`, `max`, `mean`, `any`, and
  `all`
- limited `Reduce(FUN, x)` lowering for recognized reducer surfaces
- explicit boundary nodes in `fallback = "auto"`
- source-only, TinyCC, and shared-library (`R CMD SHLIB`) backends
- compile-time backend/target capability validation
- hand-written and generated differential validation, including
  cross-backend checks

### Intentionally still limited

- full base-R `Reduce()` compatibility
- axis-wise reductions such as `rowSums()` / `colSums()`
- broader `apply`-family lowering
- richer reuse/allocation planning before C emission
- broader indexing forms such as gather/scatter/filter
- larger harvested validation corpora from real array-oriented R code

## Quick tour

### Example 1: a reduction kernel

Start with a kernel that reduces a vector expression to one scalar.

``` r
sum_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sum((sin(x) + y) * y)
}
```

### Inspect the IR

``` r
sum_module <- tccq_compile(sum_kernel, mode = "ir")
sum_module
#> <tccq_module>
#>   entry: tccq_entry 
#>   formals:
#>    - x : double[NA] 
#>    - y : double[NA] 
#>   ir: program 
#>   kernel: fold 
#>   result type: double
```

This path lowers to an explicit `fold` over a `producer`.

### Generate C

<details>
<summary>
Show generated C for <code>sum_kernel</code>
</summary>

``` r
sum_src <- tccq_compile(sum_kernel, mode = "code")
tccq_c_block(sum_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <limits.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif
#ifndef TCCQ_UNUSED
# if defined(__GNUC__)
#  define TCCQ_UNUSED __attribute__((unused))
# else
#  define TCCQ_UNUSED
# endif
#endif

static TCCQ_UNUSED R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

static TCCQ_UNUSED int tccq_lgl_not(int x) {
  return x == NA_LOGICAL ? NA_LOGICAL : (!x);
}

static TCCQ_UNUSED int tccq_int_idiv(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER || b == 0) return NA_INTEGER;
  int q = a / b;
  int r = a % b;
  if (r != 0 && ((a < 0) != (b < 0))) --q;
  return q;
}

static TCCQ_UNUSED int tccq_int_checked(long long x) {
  if (x > INT_MAX || x <= INT_MIN) return NA_INTEGER;
  return (int)x;
}

static TCCQ_UNUSED int tccq_int_add(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a + (long long)b);
}

static TCCQ_UNUSED int tccq_int_sub(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a - (long long)b);
}

static TCCQ_UNUSED int tccq_int_mul(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a * (long long)b);
}

static TCCQ_UNUSED int tccq_int_neg(int a) {
  if (a == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked(-((long long)a));
}

static TCCQ_UNUSED int tccq_lgl_and(int a, int b) {
  if (a == 0 || b == 0) return 0;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 1;
}

static TCCQ_UNUSED int tccq_lgl_or(int a, int b) {
  if (a == 1 || b == 1) return 1;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 0;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_y) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *tccq_arg_ptr_cache_x = NULL;
  #define p_x (tccq_arg_ptr_cache_x == NULL ? (tccq_arg_ptr_cache_x = REAL_RO(arg_x)) : tccq_arg_ptr_cache_x)
  if (TYPEOF(arg_y) != REALSXP) {
    Rf_error("argument %s has wrong R type", "y");
  }
  R_xlen_t n_y = XLENGTH(arg_y);
  const double *tccq_arg_ptr_cache_y = NULL;
  #define p_y (tccq_arg_ptr_cache_y == NULL ? (tccq_arg_ptr_cache_y = REAL_RO(arg_y)) : tccq_arg_ptr_cache_y)
  int tccq_nprotect = 0;
  R_xlen_t n_out = n_y;
  if (n_x != n_out) {
    Rf_error("vector length mismatch in shared shape domain");
  }
  double acc = 0.0;
  for (R_xlen_t i = 0; i < n_out; ++i) {
    double v = (double)(((double)(((double)(sin((double)(p_x[i]))) + (double)(p_y[i]))) * (double)(p_y[i])));
    if (R_IsNA(v)) { acc = NA_REAL; break; }
    if (R_IsNaN(v)) { acc = R_NaN; break; }
    acc += v;
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
  ++tccq_nprotect;
  REAL(out)[0] = (double) (acc);
  UNPROTECT(tccq_nprotect);
  return out;
}
```

</details>

### Compile and run it

``` r
compiled_sum_kernel <- tccq_compile(sum_kernel)
x <- as.double(seq(-2, 2, length.out = 10))
y <- as.double(seq(1, 3, length.out = 10))
compiled_sum_kernel(x, y)
#> [1] 48.90504
```

### Example 2: a vector-return kernel

Now take a kernel that produces a whole vector result.

``` r
vec_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  sin(x) + y * y
}
```

### Inspect the IR

``` r
vec_module <- tccq_compile(vec_kernel, mode = "ir")
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

This path lowers to `materialize(producer)`.

### Generate C

<details>
<summary>
Show generated C for <code>vec_kernel</code>
</summary>

``` r
vec_src <- tccq_compile(vec_kernel, mode = "code")
tccq_c_block(vec_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <limits.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif
#ifndef TCCQ_UNUSED
# if defined(__GNUC__)
#  define TCCQ_UNUSED __attribute__((unused))
# else
#  define TCCQ_UNUSED
# endif
#endif

static TCCQ_UNUSED R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

static TCCQ_UNUSED int tccq_lgl_not(int x) {
  return x == NA_LOGICAL ? NA_LOGICAL : (!x);
}

static TCCQ_UNUSED int tccq_int_idiv(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER || b == 0) return NA_INTEGER;
  int q = a / b;
  int r = a % b;
  if (r != 0 && ((a < 0) != (b < 0))) --q;
  return q;
}

static TCCQ_UNUSED int tccq_int_checked(long long x) {
  if (x > INT_MAX || x <= INT_MIN) return NA_INTEGER;
  return (int)x;
}

static TCCQ_UNUSED int tccq_int_add(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a + (long long)b);
}

static TCCQ_UNUSED int tccq_int_sub(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a - (long long)b);
}

static TCCQ_UNUSED int tccq_int_mul(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a * (long long)b);
}

static TCCQ_UNUSED int tccq_int_neg(int a) {
  if (a == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked(-((long long)a));
}

static TCCQ_UNUSED int tccq_lgl_and(int a, int b) {
  if (a == 0 || b == 0) return 0;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 1;
}

static TCCQ_UNUSED int tccq_lgl_or(int a, int b) {
  if (a == 1 || b == 1) return 1;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 0;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_y) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *tccq_arg_ptr_cache_x = NULL;
  #define p_x (tccq_arg_ptr_cache_x == NULL ? (tccq_arg_ptr_cache_x = REAL_RO(arg_x)) : tccq_arg_ptr_cache_x)
  if (TYPEOF(arg_y) != REALSXP) {
    Rf_error("argument %s has wrong R type", "y");
  }
  R_xlen_t n_y = XLENGTH(arg_y);
  const double *tccq_arg_ptr_cache_y = NULL;
  #define p_y (tccq_arg_ptr_cache_y == NULL ? (tccq_arg_ptr_cache_y = REAL_RO(arg_y)) : tccq_arg_ptr_cache_y)
  int tccq_nprotect = 0;
  R_xlen_t n_out = n_x;
  if (n_y != n_out) {
    Rf_error("vector length mismatch in shared shape domain");
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n_out));
  ++tccq_nprotect;
  double *p_out = REAL(out);
  for (R_xlen_t i = 0; i < n_out; ++i) {
    p_out[i] = (double)(((double)(sin((double)(p_x[i]))) + (double)(((double)(p_y[i]) * (double)(p_y[i])))));
  }
  UNPROTECT(tccq_nprotect);
  return out;
}
```

</details>

### Compile and run it

``` r
compiled_vec_kernel <- tccq_compile(vec_kernel)
unname(compiled_vec_kernel(x, y)[1:4])
#> [1] 0.09070257 0.49394330 1.19022755 2.15940797
```

## Backend selection

`tccq_compile()` can target different C backends explicitly.

``` r
tccq_compile(f, backend = tccq_backend_source())
tccq_compile(f, backend = tccq_backend_tinycc())
tccq_compile(f, backend = tccq_backend_shlib())
```

The roles are:

- `tccq_backend_source()` returns emitted C and does not compile it
- `tccq_backend_tinycc()` compiles emitted C in memory through `Rtinycc`
- `tccq_backend_shlib()` compiles emitted C through `R CMD SHLIB` and
  loads it through `.Call()`

The shared-library route is in the same general deployment space as
[`callme`](https://github.com/coolbutuseless/callme): a C-only
compile/load workflow around shared objects and `.Call()` entry points.

Before compilation, `tccq_compile()` validates backend capabilities
against the selected target, compile context, and any explicit boundary
APIs.

## Current semantic model

The current subset is small on purpose. The key rules are explicit and
are meant to live in IR and middle-end plans rather than appearing
accidentally in the C emitter.

### Bindings, reads, slices, and writes

- `a <- expr` is a local binding, not mutation
- `x[i]` is a checked 1-based indexed read
- `x[lo:hi]` is a contiguous slice/view expression
- `a[i] <- v` and `a[lo:hi] <- v` are mutation barriers
- direct mutation of formals such as `x[i] <- v` is rejected for now

### Example: local bind, then indexed write

``` r
assign_kernel <- function(x, i, v) {
  declare(type(x = double(NA)), type(i = integer()), type(v = double()))
  y <- x
  y[i] <- v
  y
}
```

Compile and run it:

``` r
compiled_assign_kernel <- tccq_compile(assign_kernel)
unname(compiled_assign_kernel(c(1, 2, 3), 2L, 10))
#> [1]  1 10  3
```

Show the generated C:

<details>
<summary>
Show generated C for <code>assign_kernel</code>
</summary>

``` r
assign_src <- tccq_compile(assign_kernel, mode = "code")
tccq_c_block(assign_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <limits.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif
#ifndef TCCQ_UNUSED
# if defined(__GNUC__)
#  define TCCQ_UNUSED __attribute__((unused))
# else
#  define TCCQ_UNUSED
# endif
#endif

static TCCQ_UNUSED R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

static TCCQ_UNUSED int tccq_lgl_not(int x) {
  return x == NA_LOGICAL ? NA_LOGICAL : (!x);
}

static TCCQ_UNUSED int tccq_int_idiv(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER || b == 0) return NA_INTEGER;
  int q = a / b;
  int r = a % b;
  if (r != 0 && ((a < 0) != (b < 0))) --q;
  return q;
}

static TCCQ_UNUSED int tccq_int_checked(long long x) {
  if (x > INT_MAX || x <= INT_MIN) return NA_INTEGER;
  return (int)x;
}

static TCCQ_UNUSED int tccq_int_add(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a + (long long)b);
}

static TCCQ_UNUSED int tccq_int_sub(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a - (long long)b);
}

static TCCQ_UNUSED int tccq_int_mul(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a * (long long)b);
}

static TCCQ_UNUSED int tccq_int_neg(int a) {
  if (a == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked(-((long long)a));
}

static TCCQ_UNUSED int tccq_lgl_and(int a, int b) {
  if (a == 0 || b == 0) return 0;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 1;
}

static TCCQ_UNUSED int tccq_lgl_or(int a, int b) {
  if (a == 1 || b == 1) return 1;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 0;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_i, SEXP arg_v) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *tccq_arg_ptr_cache_x = NULL;
  #define p_x (tccq_arg_ptr_cache_x == NULL ? (tccq_arg_ptr_cache_x = REAL_RO(arg_x)) : tccq_arg_ptr_cache_x)
  if (TYPEOF(arg_i) != INTSXP) {
    Rf_error("argument %s has wrong R type", "i");
  }
  R_xlen_t n_i = XLENGTH(arg_i);
  if (n_i < 1) {
    Rf_error("scalar argument %s is empty", "i");
  }
  if (n_i != 1) {
    Rf_error("scalar value %s has runtime length %lld; vector-valued scalar use is not supported", "i", (long long)n_i);
  }
  int v_i = INTEGER_RO(arg_i)[0];
  if (TYPEOF(arg_v) != REALSXP) {
    Rf_error("argument %s has wrong R type", "v");
  }
  R_xlen_t n_v = XLENGTH(arg_v);
  if (n_v < 1) {
    Rf_error("scalar argument %s is empty", "v");
  }
  if (n_v != 1) {
    Rf_error("scalar value %s has runtime length %lld; vector-valued scalar use is not supported", "v", (long long)n_v);
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
  R_xlen_t lo_y_2_d1 = 0;
  R_xlen_t n_y_2_d1 = 0;
  if (!(((int)(v_i) == NA_INTEGER))) {
    R_xlen_t raw_y_2_d1 = (R_xlen_t)(v_i);
    lo_y_2_d1 = tccq_checked_index1(raw_y_2_d1, n_y, "y");
    n_y_2_d1 = 1;
  }
  double rhs_y_2 = (double)(v_v);
  for (R_xlen_t i = 0; i < n_y_2_d1; ++i) {
    p_y[lo_y_2_d1 + i] = rhs_y_2;
  }
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

</details>

Here `y <- x` starts as an alias to the formal input. The first indexed
write forces materialization into owned local storage.

### Example: slice, then reduce

``` r
slice_sum_kernel <- function(x, lo, hi) {
  declare(type(x = double(NA)), type(lo = integer()), type(hi = integer()))
  y <- x[lo:hi]
  sum(y)
}
```

Compile and run it:

``` r
compiled_slice_sum_kernel <- tccq_compile(slice_sum_kernel)
compiled_slice_sum_kernel(c(1, 2, 3, 4), 2L, 4L)
#> [1] 9
```

Show the generated C:

<details>
<summary>
Show generated C for <code>slice_sum_kernel</code>
</summary>

``` r
slice_sum_src <- tccq_compile(slice_sum_kernel, mode = "code")
tccq_c_block(slice_sum_src)
```

``` c
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <limits.h>

#ifndef REAL_RO
#define REAL_RO(x) REAL(x)
#endif
#ifndef INTEGER_RO
#define INTEGER_RO(x) INTEGER(x)
#endif
#ifndef LOGICAL_RO
#define LOGICAL_RO(x) LOGICAL(x)
#endif
#ifndef TCCQ_UNUSED
# if defined(__GNUC__)
#  define TCCQ_UNUSED __attribute__((unused))
# else
#  define TCCQ_UNUSED
# endif
#endif

static TCCQ_UNUSED R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {
  if (idx < 1 || idx > len) {
    Rf_error("index out of bounds for %s", name);
  }
  return idx - 1;
}

static TCCQ_UNUSED int tccq_lgl_not(int x) {
  return x == NA_LOGICAL ? NA_LOGICAL : (!x);
}

static TCCQ_UNUSED int tccq_int_idiv(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER || b == 0) return NA_INTEGER;
  int q = a / b;
  int r = a % b;
  if (r != 0 && ((a < 0) != (b < 0))) --q;
  return q;
}

static TCCQ_UNUSED int tccq_int_checked(long long x) {
  if (x > INT_MAX || x <= INT_MIN) return NA_INTEGER;
  return (int)x;
}

static TCCQ_UNUSED int tccq_int_add(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a + (long long)b);
}

static TCCQ_UNUSED int tccq_int_sub(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a - (long long)b);
}

static TCCQ_UNUSED int tccq_int_mul(int a, int b) {
  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked((long long)a * (long long)b);
}

static TCCQ_UNUSED int tccq_int_neg(int a) {
  if (a == NA_INTEGER) return NA_INTEGER;
  return tccq_int_checked(-((long long)a));
}

static TCCQ_UNUSED int tccq_lgl_and(int a, int b) {
  if (a == 0 || b == 0) return 0;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 1;
}

static TCCQ_UNUSED int tccq_lgl_or(int a, int b) {
  if (a == 1 || b == 1) return 1;
  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;
  return 0;
}

SEXP tccq_entry(SEXP arg_x, SEXP arg_lo, SEXP arg_hi) {
  if (TYPEOF(arg_x) != REALSXP) {
    Rf_error("argument %s has wrong R type", "x");
  }
  R_xlen_t n_x = XLENGTH(arg_x);
  const double *tccq_arg_ptr_cache_x = NULL;
  #define p_x (tccq_arg_ptr_cache_x == NULL ? (tccq_arg_ptr_cache_x = REAL_RO(arg_x)) : tccq_arg_ptr_cache_x)
  if (TYPEOF(arg_lo) != INTSXP) {
    Rf_error("argument %s has wrong R type", "lo");
  }
  R_xlen_t n_lo = XLENGTH(arg_lo);
  if (n_lo < 1) {
    Rf_error("scalar argument %s is empty", "lo");
  }
  if (n_lo != 1) {
    Rf_error("scalar value %s has runtime length %lld; vector-valued scalar use is not supported", "lo", (long long)n_lo);
  }
  int v_lo = INTEGER_RO(arg_lo)[0];
  if (TYPEOF(arg_hi) != INTSXP) {
    Rf_error("argument %s has wrong R type", "hi");
  }
  R_xlen_t n_hi = XLENGTH(arg_hi);
  if (n_hi < 1) {
    Rf_error("scalar argument %s is empty", "hi");
  }
  if (n_hi != 1) {
    Rf_error("scalar value %s has runtime length %lld; vector-valued scalar use is not supported", "hi", (long long)n_hi);
  }
  int v_hi = INTEGER_RO(arg_hi)[0];
  int tccq_nprotect = 0;
  R_xlen_t off_n_y = 0;
  R_xlen_t n_n_y = n_x;
  int missing_n_y = 0;
  if (!missing_n_y) {
    R_xlen_t rel_lo_n_y_1 = tccq_checked_index1((R_xlen_t)(v_lo), n_n_y, "x");
    R_xlen_t rel_hi_n_y_1 = tccq_checked_index1((R_xlen_t)(v_hi), n_n_y, "x");
    if (rel_hi_n_y_1 < rel_lo_n_y_1) { Rf_error("decreasing slices are not supported"); }
    off_n_y = off_n_y + rel_lo_n_y_1;
    n_n_y = rel_hi_n_y_1 - rel_lo_n_y_1 + 1;
  }
  R_xlen_t n_y = n_n_y;
  SEXP loc_y = R_NilValue;
  double *p_y = (double *) (p_x + off_n_y);
  int own_y = 0;
  R_xlen_t n_out = n_y;
  double acc = 0.0;
  for (R_xlen_t i = 0; i < n_out; ++i) {
    double v = (double)(p_y[i]);
    if (R_IsNA(v)) { acc = NA_REAL; break; }
    if (R_IsNaN(v)) { acc = R_NaN; break; }
    acc += v;
  }
  SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));
  ++tccq_nprotect;
  REAL(out)[0] = (double) (acc);
  UNPROTECT(tccq_nprotect);
  return out;
}
```

</details>

This slice bind lowers to an explicit `view1` node. In the current
target, that lets the compiler reduce over the slice extent without
first copying the slice into a fresh vector.

### Reducers and reducer-style idioms

Reducers are lowered through explicit fold machinery, not through
one-off `sum()`-only code generation branches.

Supported direct reducers include:

- `sum`, `prod`, `min`, `max`, `mean`
- `any`, `all`

Recognized `Reduce(FUN, x)` surfaces currently include
reducer-equivalent forms such as:

- `Reduce(`+`, x)`
- `Reduce(`\*`, x)`
- `Reduce(`&`, x)`
- `Reduce(`\|`, x)`
- `Reduce(min, x)`
- `Reduce(max, x)`

``` r
logic_reduce_kernel <- function(x, y) {
  declare(type(x = double(NA), y = double(NA)))
  all((x > 0) & (y > 0))
}

compiled_logic_reduce_kernel <- tccq_compile(logic_reduce_kernel)
compiled_logic_reduce_kernel(c(1, 2, 3), c(4, 5, 6))
#> [1] TRUE
```

``` r
reduce_kernel <- function(x) {
  declare(type(x = double(NA)))
  Reduce(`+`, x)
}

compiled_reduce_kernel <- tccq_compile(reduce_kernel)
compiled_reduce_kernel(c(1, 2, 3, 4))
#> [1] 10
```

This is still intentionally narrower than full base-R `Reduce()`
semantics.

### Unsupported calls and explicit boundaries

Unsupported calls are not supposed to disappear silently into
target-specific special cases. The default `fallback = "hard"` rejects
unsupported calls. With explicit `fallback = "auto"`, they lower to
inspectable boundary nodes.

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
fallback_ir$boundary_diagnostics[[1]]$original_call
#> [1] "floor(x)"
```

In default hard-fallback mode, the same unsupported call is rejected.

``` r
tryCatch(
  tccq_compile(fallback_kernel, mode = "ir"),
  error = function(e) e$message
)
#> [1] "unsupported call in tccq: floor. Add a lowerer case or route it through an explicit boundary node."
```

### Direct formal mutation is rejected

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
#> [1] "indexed assignment currently requires a local vector binding. Write y <- x; y[...] <- value; y instead of mutating formal 'x' directly."
```

## Architecture

`tccquickr` is organized as an explicit transformation pipeline rather
than as a single direct “R expression to C string” step.

### Pipeline

- **frontend**: parse `declare(type(...))` and lower a small R subset
  into typed IR
- **middle-end**: validate IR, infer effects, normalize access paths,
  derive explicit shape/domain facts, lower reducers, kernelize, fuse,
  handle explicit boundaries, and derive conservative storage/protection
  plans
- **target**: emit C through the R C API
- **backend**: either return source or compile the emitted C through
  TinyCC or `R CMD SHLIB`

### Kernel IR concepts

`producer`  
an element computation over an index domain

`materialize(producer)`  
allocate an output vector and write producer elements into it

`fold(op, domain, elem)`  
reduce a producer-like element expression over a loop domain

These are the concepts that make fusion and materialization decisions
explicit in IR instead of hiding them inside the C emitter.

### Fusion is an explicit rewrite

Before fusion, the middle-end now makes access chains and shape/domain
facts explicit, then `kernelize` represents the reduction as a fold over
a materialized producer. The fusion pass rewrites that to a fold over
the producer directly.

``` r
mod0 <- tccquickr:::tccq_frontend(sum_kernel)
mod0 <- tccquickr:::tccq_lower_module(mod0)

before_fusion <- tccquickr:::tccq_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq_pass_validate_ir(),
    tccquickr:::tccq_pass_effects(),
    tccquickr:::tccq_pass_index_normalize(),
    tccquickr:::tccq_pass_shape_domains(),
    tccquickr:::tccq_pass_kernelize()
  )
)

after_fusion <- tccquickr:::tccq_run_passes(
  mod0,
  passes = list(
    tccquickr:::tccq_pass_validate_ir(),
    tccquickr:::tccq_pass_effects(),
    tccquickr:::tccq_pass_index_normalize(),
    tccquickr:::tccq_pass_shape_domains(),
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

That rewrite system is still small, but the important architectural
point is that fusion lives in the middle-end rather than being
discovered accidentally while printing C.

## Validation

Validation is part of the architecture, not an afterthought.

The current package combines:

- hand-written runtime tests for reducers, slicing, views, boundaries,
  storage behavior, and backends
- generated differential tests that construct typed R functions
  programmatically and compare compiled behavior against direct R
  evaluation
- cross-backend checks between TinyCC and `R CMD SHLIB`

The goal is not to claim formal proof. The goal is to keep the
transformation pipeline honest with executable witness tests across IR,
emitted C, and runtime behavior.

## Related projects and influences

These are reference points, not templates that `tccquickr` is trying to
copy exactly.

- [`quickr`](https://github.com/t-kalinowski/quickr): useful comparison
  for a declared R subset compiler
- [`anvil`](https://github.com/r-xla/anvl): useful comparison for
  explicit transformation architecture and backend thinking
- [`callme`](https://github.com/coolbutuseless/callme): useful reference
  for a shared-library compile/load workflow around `.Call()`
- [`SAC`](https://sac-home.org/) and
  [`sac2c`](https://gitlab.sac-home.org/sac-group/sac2c): main
  inspiration for explicit array IR, fusion, and
  allocation/materialization discipline

## Development

`tccquickr` follows the same basic repo conventions as the parent
package:

- `README.Rmd` is the source of `README.md`
- `roxygen2` generates documentation
- `tinytest` lives under `inst/tinytest`
- a simple package-local `Makefile` drives common build/check/test tasks
