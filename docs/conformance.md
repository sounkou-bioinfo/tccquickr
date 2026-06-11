
<!-- conformance.md is generated from .Rmd. Do not edit the .md. -->

# Conformance against R

This is the layer-2 number from [ADR
0005](decisions/0005-conformance-and-verification.md): the **Porffor
discipline**, where the R interpreter itself is the ground-truth oracle.

A generator emits programs over the in-subset grammar
(`R/tccq_conformance.R`). Each program is compiled by `tccq` **and**
evaluated by R on several random inputs, and the results are diffed
(numeric tolerance, NA/NaN positions must match). The generator only
emits constructs that are `core` in the [grammar coverage
map](r-subset-grammar.md), so the contract is exact:

> **passed == total, always.** A drop is a real regression, and the
> gating test `inst/tinytest/test_tccq_conformance.R` fails CI.

## Result

**40 / 40** generated programs match the R interpreter (seed `20260611`,
4 random inputs each, TinyCC backend).

| status | count |
|:-------|------:|
| pass   |    40 |
| fail   |     0 |
| error  |     0 |

A sample of the generated programs:

| Generated program  | Status |
|:-------------------|:-------|
| –1                 | pass   |
| all(-x \> sin(-1)) | pass   |
| all(-1/-1 == 2)    | pass   |
| sum(y/–1)          | pass   |
| mean(2)            | pass   |
| y                  | pass   |
| abs(-1)            | pass   |
| exp(-sin(2))       | pass   |
| -2 + (2 + x) + -1  | pass   |
| -exp(-x)           | pass   |

## Bounded-exhaustive check

Random sampling can miss cases. The harness also runs a
**bounded-exhaustive** pass: every depth-1 elementwise kernel (a leaf, a
unary op of a leaf, or a binary op of two leaves, over `{x, y, 1, 2}`)
is compiled and diffed against R. This is *complete* up to depth 1, not
merely sampled.

    #> Warning in sqrt(x): NaNs produced
    #> Warning in sqrt(x): NaNs produced
    #> Warning in sqrt(x): NaNs produced
    #> Warning in sqrt(y): NaNs produced
    #> Warning in sqrt(y): NaNs produced
    #> Warning in sqrt(y): NaNs produced

**92 / 92** depth-1 elementwise kernels match the R interpreter.

## Growing the suite

This generated corpus is the seed. Per ADR 0005 it grows by (1) widening
the generator as the `core` surface grows, (2) deepening the
bounded-exhaustive pass, and (3) harvesting in-subset fragments from
`r-svn/tests/` (`arith.R`, `any-all.R`, `primitives.R`). The number only
goes up as coverage widens; it must never drop for a fixed seed.
