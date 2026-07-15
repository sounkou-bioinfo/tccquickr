
# tccquickr

`tccquickr` compiles a declared subset of R into typed IR and then into
C, Fortran, and TinyCC-JIT kernels. Programs enter through
`declare(type(...))`, become S7 value graphs with symbolic shapes and
effects, and lower into one or more backend-neutral loop nests —
SAC-style with-loops — before any target syntax exists. Unsupported R is
a classed diagnostic, never an implicit fallback.

Every chunk in this README executes when the document is knit, so the
claims below are checked against the installed package, not maintained
by hand.

## Install

``` r
install.packages(
  "tccquickr",
  repos = c("https://sounkou-bioinfo.r-universe.dev", getOption("repos"))
)
# or
remotes::install_github("sounkou-bioinfo/tccquickr")
```

## From declared R to a running kernel

A matrix-vector product, declared with symbolic dimensions:

``` r
library(tccquickr)

matrix_vector <- function(x, w) {
  declare(type(x = double(n, p), w = double(p)))
  x %*% w
}
```

The analysis lowers this into a typed loop nest — one `map` axis over
`n`, one `reduce` axis over `p` folding with `sum` — and the C backend
prints that nest:

``` r
program <- tccq_analyze(matrix_vector, strict = TRUE)@value
c_plan <- tccq_plan_backend(
  program,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
cat(c_plan@value@products@attrs$source)
#> #include <math.h>
#> #include <stdbool.h>
#> #include <stddef.h>
#> #include <stdlib.h>
#> 
#> double *tccq_c_9794934(const double *input_0001, const double *input_0002, int extent_n, int extent_p, int result_count_0001) {
#>   if (result_count_0001 < 0) {
#>     return NULL;
#>   }
#>   double *output = (double *)malloc(sizeof(double) * (size_t)result_count_0001);
#>   if (output == NULL) {
#>     return NULL;
#>   }
#>   for (int axis_0001 = 0; axis_0001 < extent_n; ++axis_0001) {
#>     double accumulator_0001 = 0.0;
#>     for (int axis_0002 = 0; axis_0002 < extent_p; ++axis_0002) {
#>       accumulator_0001 = accumulator_0001 + (input_0001[axis_0001 + axis_0002 * extent_n] * input_0002[axis_0002]);
#>     }
#>     output[axis_0001] = accumulator_0001;
#>   }
#>   return output;
#> }
```

The extent parameters come from the declared dimension symbols, so the
generated boundary knows that `ncol(x)` and `length(w)` are the same `p`
and checks it. TinyCC compiles the same source in-process and the
callable agrees with R:

``` r
jit_plan <- tccq_plan_backend(
  program,
  tccq_rtinycc_backend(),
  tccq_backend_context(mode = "jit", target = "c")
)
matvec <- jit_plan@value@products@attrs$callable

set.seed(1)
x <- matrix(rnorm(12), nrow = 3)
w <- rnorm(4)
all.equal(matvec(x, w), drop(x %*% w))
#> [1] TRUE
```

Stencils work the same way: affine slice bounds become typed affine
extents (`n - 2` is a dimension fact, not a printed string), and the
loop nest shifts accesses instead of materializing slices:

``` r
tiled_stencil_1d <- function(x) {
  declare(type(x = double(n)))
  x[1:(n - 2L)] + x[2:(n - 1L)] + x[3:n]
}

stencil_jit <- tccq_plan_backend(
  tccq_analyze(tiled_stencil_1d, strict = TRUE)@value,
  tccq_rtinycc_backend(),
  tccq_backend_context(mode = "jit", target = "c")
)
stencil <- stencil_jit@value@products@attrs$callable
v <- c(1, 2, 4, 8, 16, 32, 64)
all.equal(stencil(v), v[1:5] + v[2:6] + v[3:7])
#> [1] TRUE
```

Programs are no longer limited to one loop nest. A scalar reduction
feeding later work becomes its own all-reduce nest. Its reducer folds
through a typed scalar accumulator, then writes a distinct typed
materialized scalar slot that the final nest reads. The backend
interface assigns source names to both values; the loop nest contains no
target names or generic metadata bag. This SAC-style composition is
visible in the emitted source:

``` r
normalize <- function(x) {
  declare(type(x = double(n)))
  s <- sum(x)
  x / s
}

normalize_plan <- tccq_plan_backend(
  tccq_analyze(normalize, strict = TRUE)@value,
  tccq_c_backend(),
  tccq_backend_context(mode = "source", target = "c")
)
cat(normalize_plan@value@products@attrs$source)
#> #include <math.h>
#> #include <stdbool.h>
#> #include <stddef.h>
#> #include <stdlib.h>
#> 
#> double *tccq_c_960543494(const double *input_0001, int extent_n, int result_count_0001) {
#>   if (result_count_0001 < 0) {
#>     return NULL;
#>   }
#>   double *output = (double *)malloc(sizeof(double) * (size_t)result_count_0001);
#>   if (output == NULL) {
#>     return NULL;
#>   }
#>   double intermediate_0001;
#>   double accumulator_0001 = 0.0;
#>   for (int axis_0001 = 0; axis_0001 < extent_n; ++axis_0001) {
#>     accumulator_0001 = accumulator_0001 + input_0001[axis_0001];
#>   }
#>   intermediate_0001 = accumulator_0001;
#>   for (int axis_0001 = 0; axis_0001 < extent_n; ++axis_0001) {
#>     output[axis_0001] = (input_0001[axis_0001] / intermediate_0001);
#>   }
#>   return output;
#> }
```

``` r
normalize_jit <- tccq_plan_backend(
  tccq_analyze(normalize, strict = TRUE)@value,
  tccq_rtinycc_backend(),
  tccq_backend_context(mode = "jit", target = "c")
)
normalize_kernel <- normalize_jit@value@products@attrs$callable
v <- c(1, 2, 3, 4)
all.equal(normalize_kernel(v), v / sum(v))
#> [1] TRUE
```

## The declared subset today

**Types.** `double` formals may have any rank with symbolic or constant
dimensions. Scalar `logical` formals are also carried through the
generated ABI as C `bool`, Fortran `logical(c_bool)`, and TinyCC `bool`;
other declared base types analyze but do not reach the source backends
yet.

**Elementwise operations.** `+`, `-`, `*`, `/`, `^`, `sqrt`, and `exp`
use scalar broadcast over any rank. Registries can add rendered
operations without touching the printers. Every neutral operation
expression owns its complete `TccqLoweredOperation` and typed effect; it
does not duplicate the selected implementation or hide pass facts in
expression attributes.

**Reductions and contractions.** `sum` and `mean` fold full rank-N
domains; `colSums`, `rowSums`, `colMeans`, and `rowMeans` fold selected
axes. Custom reducers use `TccqReductionSpec`, including an optional
accumulator finalizer. `%*%`, `crossprod`, and `tcrossprod` use a typed
`TccqContractionSpec`, so contracted dimensions and transposition remain
axis facts rather than target-source conventions.

**Branches.** A pure `if` with a scalar logical condition, an explicit
`else`, and identical arm types lowers to `TccqBranch`. The branch
retains the special forcing facts from `TccqCallSemantics`; C and
Fortran emit conditional statements, so the unselected arm is not
evaluated, and native call boundaries reject a missing condition as R
does. Loop-nest lowering represents control as a typed `TccqBlock`
containing `TccqConditional` and `TccqAssignment` values; arithmetic in
each assignment remains a neutral `TccqExpression`. Pure branches may
nest in result arms, directly as another branch’s condition, or under
pure elementwise operations. The normalizer evaluates each
control-valued operand into a block-owned write target before its
consumer. That target retains the full semantic array type while
carrying the scalar storage type actually written in each loop
iteration; the shared function interface assigns its generated name.
Every block names the write target produced by its terminal paths. A
reducer or contraction can therefore consume a conditional element from
a block-local target while the target is still in scope. A reduction or
contraction selected inside an arm becomes an intermediate loop nest
carrying an ordered typed guard path, so nested branches execute and
materialize only the selected nests. The guard path is the storage
execution scope: extraction rejects an unscheduled value reached through
incompatible paths, C uses nullable owned buffers, and Fortran uses
guarded allocatable arrays.

**Slices and bindings.** Rank-1 `x[a:b]` accepts bounds affine in
declared dimension symbols. Every executable top-level form is a typed
`TccqEvaluationStep` in one contiguous `TccqProgramSchedule`, including
expression statements whose values are not bound. Assignment steps carry
a single-assignment `TccqLocalBinding`; the schedule, not value ids or
source text, owns R evaluation order. Each neutral reference carries one
typed `TccqExpressionReference` that distinguishes the expression value
from the logical source value it reads and owns its optional symbol,
lexical binding, slice offsets, and affine `TccqAccess`. That logical
source is distinct from a later physical `TccqStorageAllocation`. Each
step retains the exact local bindings read, because different lexical
names may alias one lowered value. The loop-nest planner sees those
reads as `TccqBindingReference` leaves rather than walking back into and
re-evaluating their definition graphs. It visits schedule steps before
the returned expression. A reduction defined before a later `if`
therefore materializes once without that `if`’s guards; when the
definition itself is conditional, its intermediate nests retain the
definition’s selected-arm guards. Later consumers reuse that schedule
rather than moving evaluation to a use site.

The first local fusion rule is intentionally stricter than textual
`f(g(x))`. A definition is kept as an expression only when it has one
exact lexical read in the immediately following elementwise step,
producer and consumer have the same iteration shape, and neither typed
effect may write, allocate, cross a boundary, error, or warn. The
resulting `TccqStorageSlot@materialized = FALSE` is consumed by the
shared loop-nest planner, so C, TinyCC, and Fortran all remove the same
intermediate. Duplicate reads, later reads, branches, reductions,
contractions, and warning-capable operations such as `sqrt` remain
materialized. Purity alone is not treated as a proof that eager R
evaluation can be reordered.

**Composition.** Non-root reductions and contractions become
intermediate nests — named scalars for rank-0 results and materialized
temporary buffers otherwise — so `x / sum(x)`, `colSums(x) + 1`,
`(x %*% w) + y`, and `cs <- colSums(x); cs / sum(cs)` compile as ordered
nest sequences. A value consumed twice materializes once. Every nest
carries the typed storage slot that receives its result, and every
reducer carries a separate typed scalar accumulator target.
`TccqBackendFunctionInterface` binds generated names to those values for
C, TinyCC, and Fortran.

A materialized temporary slot owns a `TccqStorageAllocation`. Two
same-typed buffer slots share that physical identity only when
schedule-derived, complete expression liveness proves that the first
dies before the second definition and neither slot is control-dependent.
A direct consumer therefore overlaps its producer, while a buffer
reduced to a retained scalar can be reused by a later buffer. The shared
allocation is consumed by all source paths: C emits one allocation and
one free, and Fortran emits one automatic array. Non-materialized
expressions own no allocation, and guarded buffers remain distinct.

**Dimensions and recycling.** A declared dimension symbol is a scalar in
the body: `colSums(x) / n` reads the extent parameter already carried by
the ABI. Rank-mixed operands follow R’s recycling rule with GNU-R as the
oracle. A shorter operand recycles over column-major order through a
typed modulo-linear access only when divisibility is provable. That
`TccqAccess` owns the typed consumer shape used to linearize the logical
axis order; it does not pass dimensions to the printer through metadata.
Non-conformable arrays are refused as R refuses them.

Compilation succeeds when at least one backend produces a working plan;
a backend that cannot lower a program reports typed feasibility
diagnostics without vetoing the suite.

## Apotheosis suite

The targets are explicit probes, each exercising one compiler idea, plus
composite programs that force the ideas together. The table below is
computed at knit time by running every probe through `tccq_compile()`; a
probe either compiles through the core backends or names the diagnostic
where it stops.

``` r
probes <- list(
  map_chain = function(x, y) {
    declare(type(x = double(n), y = double(n)))
    exp(sqrt(x) + y)
  },
  map_reduce = function(x, y) {
    declare(type(x = double(n), y = double(n)))
    sum(exp(x) * y)
  },
  matrix_reduce = function(x, y) {
    declare(type(x = double(n, p), y = double(n, p)))
    sum(exp(x) * y)
  },
  matrix_map = function(x, y) {
    declare(type(x = double(n, p), y = double(n, p)))
    sqrt(x) + y
  },
  column_sums = function(x) {
    declare(type(x = double(n, p)))
    colSums(x)
  },
  matrix_vector = function(x, w) {
    declare(type(x = double(n, p), w = double(p)))
    x %*% w
  },
  matrix_multiply = function(x, y) {
    declare(type(x = double(n, p), y = double(p, q)))
    x %*% y
  },
  tiled_stencil_1d = function(x) {
    declare(type(x = double(n)))
    x[1:(n - 2L)] + x[2:(n - 1L)] + x[3:n]
  },
  scalar_composition = function(x, y) {
    declare(type(x = double(n), y = double(n)))
    (x - sum(x)) / sum(y * y)
  },
  array_composition = function(x, w) {
    declare(type(x = double(n, p), w = double(p)))
    cs <- colSums(x) * w
    cs / sum(cs)
  },
  column_means_chain = function(x, y) {
    declare(type(x = double(n, p), y = double(p, q)))
    m <- x %*% y
    sqrt(exp(colSums(m) / n))
  },
  logistic_forward_pass = function(x, w) {
    declare(type(x = double(n, p), w = double(p)))
    mu <- colSums(x) / n
    sigma <- sqrt(colSums((x - mu)^2) / (n - 1))
    z <- (x - mu) / sigma
    eta <- z %*% w
    1 / (1 + exp(-eta))
  },
  raw_buffer_roundtrip = function(bytes, scratch) {
    declare(type(bytes = raw(n), scratch = buffer(n)))
    bytes
  },
  conditional_map = function(x, flag) {
    declare(type(x = double(n), flag = logical()))
    if (flag) x else -x
  },
  nested_conditional_map = function(x, primary, secondary) {
    declare(type(x = double(n), primary = logical(), secondary = logical()))
    if (primary) if (secondary) x else -x else x
  },
  computed_condition_map = function(x, primary, secondary) {
    declare(type(x = double(n), primary = logical(), secondary = logical()))
    if (if (primary) secondary else primary) x else -x
  },
  conditional_composition = function(x, flag) {
    declare(type(x = double(n), flag = logical()))
    exp((if (flag) x else -x) + 1)
  },
  conditional_reduce = function(x, flag) {
    declare(type(x = double(n), flag = logical()))
    sum(if (flag) x else -x)
  },
  selected_reduce = function(x, flag) {
    declare(type(x = double(n), flag = logical()))
    if (flag) sum(x) else 0
  },
  control_flow_probe = function(x, flag) {
    declare(type(x = double(n), flag = logical()))
    out <- 0
    i <- 1L
    repeat {
      if (i > n) {
        break
      }
      out <- out + ifelse(flag, x[i], -x[i])
      i <- i + 1L
    }
    switch(if (flag) "sum" else "neg", sum = out, neg = -out)
  },
  apply_reduce_probe = function(x) {
    declare(type(x = double(n, p)))
    Reduce(`+`, lapply(seq_len(p), function(j) sum(x[, j])))
  },
  logistic_gradient = function(x, y, w, lambda) {
    declare(type(
      x = double(n, p),
      y = double(n),
      w = double(p),
      lambda = double()
    ))
    mu <- colMeans(x)
    sigma <- sqrt(colSums((x - mu)^2) / (n - 1L))
    z <- (x - mu) / sigma
    eta <- z %*% w
    prob <- 1 / (1 + exp(-eta))
    grad <- crossprod(z, prob - y) / n + lambda * w
    w - 0.01 * grad
  },
  viterbi_decode = function(init, trans, emit) {
    declare(type(
      init = double(k),
      trans = double(k, k),
      emit = double(t, k)
    ))
    score <- init + emit[1L, ]
    back <- matrix(0L, nrow = t, ncol = k)
    for (step in 2:t) {
      next_score <- score
      for (state in 1:k) {
        candidate <- score + trans[, state]
        best <- which.max(candidate)
        next_score[state] <- candidate[best] + emit[step, state]
        back[step, state] <- best
      }
      score <- next_score
    }
    list(score = score, back = back)
  }
)
```

``` r
probe_status <- function(fn) {
  compiled <- tccq_compile(fn, strict = FALSE)
  if (compiled@success) {
    return("compiles through C, Fortran, and TinyCC JIT")
  }
  diagnostic <- compiled@diagnostics[[1L]]
  sprintf("`%s`", diagnostic@code)
}
knitr::kable(data.frame(
  probe = names(probes),
  status = vapply(probes, probe_status, character(1)),
  row.names = NULL
))
```

| probe                   | status                                      |
|:------------------------|:--------------------------------------------|
| map_chain               | compiles through C, Fortran, and TinyCC JIT |
| map_reduce              | compiles through C, Fortran, and TinyCC JIT |
| matrix_reduce           | compiles through C, Fortran, and TinyCC JIT |
| matrix_map              | compiles through C, Fortran, and TinyCC JIT |
| column_sums             | compiles through C, Fortran, and TinyCC JIT |
| matrix_vector           | compiles through C, Fortran, and TinyCC JIT |
| matrix_multiply         | compiles through C, Fortran, and TinyCC JIT |
| tiled_stencil_1d        | compiles through C, Fortran, and TinyCC JIT |
| scalar_composition      | compiles through C, Fortran, and TinyCC JIT |
| array_composition       | compiles through C, Fortran, and TinyCC JIT |
| column_means_chain      | compiles through C, Fortran, and TinyCC JIT |
| logistic_forward_pass   | compiles through C, Fortran, and TinyCC JIT |
| raw_buffer_roundtrip    | `backend.unsupported_type`                  |
| conditional_map         | compiles through C, Fortran, and TinyCC JIT |
| nested_conditional_map  | compiles through C, Fortran, and TinyCC JIT |
| computed_condition_map  | compiles through C, Fortran, and TinyCC JIT |
| conditional_composition | compiles through C, Fortran, and TinyCC JIT |
| conditional_reduce      | compiles through C, Fortran, and TinyCC JIT |
| selected_reduce         | compiles through C, Fortran, and TinyCC JIT |
| control_flow_probe      | `frontend.unimplemented_call`               |
| apply_reduce_probe      | `frontend.unimplemented_call`               |
| logistic_gradient       | compiles through C, Fortran, and TinyCC JIT |
| viterbi_decode          | `frontend.unimplemented_call`               |

The failing rows are the roadmap: every one must move deeper through the
same typed IR — loop regions and recurrences for Viterbi, general
indexing and access regions, `raw`/`buffer` bridges, and apply-family
folds. Failures must stay specific enough that the next typed concept to
add is obvious.

## Design

The compiler core is typed all the way down: S7 schemas for values,
shapes, effects, regions, and plans; `s7contract` interfaces between
phases; classed diagnostics instead of error strings; neutral expression
and statement values; and one `TccqLoopNest` iteration abstraction
consumed by every source backend. The full phase-by-phase story — call
semantics, operation registries, lowering, loop nests, bridges, backend
planning, fusion — lives in
[docs/architecture.md](docs/architecture.md), with the root direction in
[docs/root.md](docs/root.md) and repo rules in [AGENTS.md](AGENTS.md).
