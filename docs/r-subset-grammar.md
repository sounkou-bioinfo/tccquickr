
<!-- r-subset-grammar.md is generated from .Rmd. Do not edit the .md. -->

# Accepted R subset — grammar coverage

This is the **mental map** for what `tccquickr` compiles, grounded in
R’s own grammar (the yacc grammar
[`src/main/gram.y`](https://github.com/r-devel/r-svn/blob/master/src/main/gram.y)).
It is the layer-1 spec boundary from [ADR
0005](decisions/0005-conformance-and-verification.md).

**It is computed, not hand-written.** Each row below is a real probe: a
minimal `declare(type(...))` function exercising one grammar production
is handed to the compiler, and the disposition records what actually
happens. The table cannot drift from the implementation — if support
changes, regenerating this document changes the table
(`tools/render_docs.R`, backed by `R/tccq_grammar.R`).

Dispositions:

- **core** — compiles with `fallback = "hard"`: a pure-C kernel, no
  `r_eval`.
- **boundary** — rejected in hard mode, but lowers under
  `fallback = "auto"` to an explicit `r_eval` boundary node.
- **rejected** — neither mode accepts it (an honest gap, or out of
  scope).

Current coverage: **30 core**, **5 boundary**, **11 rejected** (of 46
probed productions).

| Production                        | gram.y rule                                        | Probe                                                                 | Disposition |
|:----------------------------------|:---------------------------------------------------|:----------------------------------------------------------------------|:------------|
| add                               | expr: expr ‘+’ expr                                | x + 1                                                                 | core        |
| comparison !=                     | expr: expr NE expr                                 | x != 0                                                                | core        |
| comparison \<                     | expr: expr LT expr                                 | x \< 1                                                                | core        |
| comparison \<=                    | expr: expr LE expr                                 | x \<= 0                                                               | core        |
| comparison ==                     | expr: expr EQ expr                                 | x == 0                                                                | core        |
| comparison \>                     | expr: expr GT expr                                 | x \> 0                                                                | core        |
| comparison \>=                    | expr: expr GE expr                                 | x \>= 0                                                               | core        |
| divide                            | expr: expr ‘/’ expr                                | x / 2                                                                 | core        |
| fold reducer call                 | expr: SYMBOL_FUNCTION_CALL ‘(’ … ‘)’               | sum(x)                                                                | core        |
| for loop + indexed write          | expr: FOR forcond expr_or_assign                   | out \<- x; for (i in 1:n) { out\[i\] \<- out\[i\] \* 2 }; out         | core        |
| if / else (scalar)                | expr: IF ifcond expr_or_assign ELSE expr_or_assign | if (n \> 0L) n else -n                                                | core        |
| integer constant                  | expr: NUM_CONST                                    | x + 1L                                                                | core        |
| integer divide                    | expr: expr SPECIAL expr                            | x %/% 2                                                               | core        |
| local assignment                  | expr: expr LEFT_ASSIGN expr                        | a \<- x + 1; a                                                        | core        |
| logical and (&)                   | expr: expr AND expr                                | (x \> 0) & (x \< 1)                                                   | core        |
| logical or (\|)                   | expr: expr OR expr                                 | (x \> 0) \| (x \< 1)                                                  | core        |
| multiply                          | expr: expr ’\*’ expr                               | x \* 3                                                                | core        |
| numeric constant                  | expr: NUM_CONST                                    | x \* 2                                                                | core        |
| parenthesised                     | expr: ‘(’ expr_or_assign ‘)’                       | (x + 1) \* 2                                                          | core        |
| pipe (\|\>)                       | expr: expr PIPE expr                               | x \|\> sin()                                                          | core        |
| power                             | expr: expr ‘^’ expr                                | x ^ 2                                                                 | core        |
| range slice (\[)                  | expr: expr ‘\[’ subscript ’\]’                     | x\[1:n\]                                                              | core        |
| single index (\[)                 | expr: expr ‘\[’ subscript ’\]’                     | x\[1L\]                                                               | core        |
| subtract                          | expr: expr ‘-’ expr                                | x - 1                                                                 | core        |
| symbol                            | expr: SYMBOL                                       | x                                                                     | core        |
| unary math call                   | expr: SYMBOL_FUNCTION_CALL ‘(’ … ‘)’               | sin(x)                                                                | core        |
| unary minus                       | expr: ‘-’ expr                                     | -x                                                                    | core        |
| unary not                         | expr: ‘!’ expr                                     | !(x \> 0)                                                             | core        |
| unary plus                        | expr: ‘+’ expr                                     | +x                                                                    | core        |
| vectorized ifelse                 | expr: SYMBOL_FUNCTION_CALL ‘(’ … ‘)’               | ifelse(x \> 0, x, -x)                                                 | core        |
| double index (\[\[)               | expr: expr LBB subscript ‘\]’ ‘\]’                 | x\[\[1L\]\]                                                           | boundary    |
| logical and (&&)                  | expr: expr AND2 expr                               | (n \> 0L) && (n \< 9L)                                                | boundary    |
| logical or (\|\|)                 | expr: expr OR2 expr                                | (n \> 0L) \|\| (n \< 9L)                                              | boundary    |
| modulo                            | expr: expr SPECIAL expr                            | x %% 2                                                                | boundary    |
| unknown call                      | expr: SYMBOL_FUNCTION_CALL ‘(’ … ‘)’               | qux(x)                                                                | boundary    |
| anonymous function                | expr: FUNCTION ‘(’ formlist ‘)’ …                  | (function(z) z)(x)                                                    | rejected    |
| break                             | expr: BREAK                                        | out \<- x; for (i in 1:n) { if (i \> 0L) break; out\[i\] \<- 0 }; out | rejected    |
| colon sequence                    | expr: expr ‘:’ expr                                | 1:n                                                                   | rejected    |
| dollar ($) |expr: expr '$’ SYMBOL | x\$a                                               | rejected                                                              |             |
| namespaced call (::)              | expr: SYMBOL NS_GET SYMBOL                         | base::sin(x)                                                          | rejected    |
| next                              | expr: NEXT                                         | out \<- x; for (i in 1:n) { if (i \> 0L) next; out\[i\] \<- 0 }; out  | rejected    |
| NULL                              | expr: NULL_CONST                                   | NULL                                                                  | rejected    |
| repeat loop                       | expr: REPEAT expr_or_assign                        | repeat { break }; x                                                   | rejected    |
| slot (@)                          | expr: expr ‘@’ SYMBOL                              | <x@a>                                                                 | rejected    |
| string constant                   | expr: STR_CONST                                    | “hello”                                                               | rejected    |
| while loop                        | expr: WHILE cond expr_or_assign                    | while (n \> 0L) { }; x                                                | rejected    |

## Reading the gaps

The rejected and boundary rows are the honest edges of the subset, not
oversights to hide:

- `%%` (modulo) currently lowers to a **boundary** while `%/%` is
  **core** — the two integer-division operators are not yet at parity in
  the binary-op lowering (`R/tccq_lower.R`).
- `&&` / `||` are **boundary**: scalar short-circuit logic is not part
  of the elementwise kernel surface.
- **`if/else` is core** for the scalar case: `if (cond) yes else no`
  with a scalar-logical condition and same-typed scalar branches
  compiles to a guarded C ternary (an `NA` condition errors like R’s
  `if`).
- **`ifelse()` is core**: the vectorized, elementwise conditional
  compiles to a per-element select that propagates `NA` (matching R,
  except that the degenerate all-`NA` case returns a typed `NA` rather
  than R’s logical-`NA` quirk).
- Still **not covered** for conditionals: differently-typed branches,
  and `if` used as a statement around assignments.
- `while`, `repeat`, `next`, `break`, `switch`, `::`, `$`, `@`, and
  anonymous functions are **rejected** today — the remaining loop forms,
  multi-way dispatch, namespaced calls, and list/S4 access are future
  work, not part of the current numeric-kernel core.
- `:` is only accepted inside a `for` head and as a slice subscript, not
  yet as a free integer-sequence producer.

These align with the limits recorded in
[`fresh-compiler-redesign.md`](fresh-compiler-redesign.md); this table
is the executable, always-current version of that prose.
