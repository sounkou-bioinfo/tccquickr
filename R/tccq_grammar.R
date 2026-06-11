# tccq_grammar.R - empirical grammar coverage probe
# SPDX-License-Identifier: GPL-3.0-or-later
#
# The accepted R subset is defined against R's own grammar, the yacc grammar
# src/main/gram.y:
#   https://github.com/r-devel/r-svn/blob/master/src/main/gram.y
# Rather than maintain a hand-written coverage
# table that drifts from the implementation, we *probe* the frontend: for each
# grammar production we compile a minimal declared function and record what the
# compiler actually does with it.

# Classify one construct by compiling a minimal declared function that uses it.
#
# - "core"     : compiles with fallback = "hard" (pure-C kernel, no r_eval).
# - "boundary" : rejected hard, but lowers under fallback = "auto" to an
#                explicit r_eval boundary node.
# - "rejected" : neither mode accepts it.
# - "error"    : the probe snippet could not even be constructed/parsed.
#' @keywords internal
#' @noRd
tccq_grammar_probe_one <- function(example, decl = "x = double(NA), n = integer()") {
  src <- sprintf("function(x, n) { declare(type(%s)); %s }", decl, example)
  fn <- tryCatch(eval(parse(text = src)[[1L]], envir = globalenv()), error = function(e) NULL)
  if (is.null(fn)) {
    return("error")
  }
  hard <- tryCatch({
    tccq_compile(fn, mode = "code", fallback = "hard")
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(hard)) {
    return("core")
  }
  auto <- tryCatch({
    tccq_compile(fn, mode = "code", fallback = "auto")
    TRUE
  }, error = function(e) FALSE)
  if (isTRUE(auto)) {
    return("boundary")
  }
  "rejected"
}

# The grammar productions we map, each with a representative probe. `rule` quotes
# the relevant gram.y production; `example` is spliced into a declared function.
#' @keywords internal
#' @noRd
tccq_grammar_productions <- function() {
  rows <- list(
    c("numeric constant", "expr: NUM_CONST", "x * 2", ""),
    c("integer constant", "expr: NUM_CONST", "x + 1L", "x = integer(NA), n = integer()"),
    c("string constant", "expr: STR_CONST", "\"hello\"", ""),
    c("NULL", "expr: NULL_CONST", "NULL", ""),
    c("symbol", "expr: SYMBOL", "x", ""),
    c("parenthesised", "expr: '(' expr_or_assign ')'", "(x + 1) * 2", ""),
    c("unary minus", "expr: '-' expr", "-x", ""),
    c("unary plus", "expr: '+' expr", "+x", ""),
    c("unary not", "expr: '!' expr", "!(x > 0)", ""),
    c("colon sequence", "expr: expr ':' expr", "1:n", ""),
    c("add", "expr: expr '+' expr", "x + 1", ""),
    c("subtract", "expr: expr '-' expr", "x - 1", ""),
    c("multiply", "expr: expr '*' expr", "x * 3", ""),
    c("divide", "expr: expr '/' expr", "x / 2", ""),
    c("power", "expr: expr '^' expr", "x ^ 2", ""),
    c("modulo", "expr: expr SPECIAL expr", "x %% 2", ""),
    c("integer divide", "expr: expr SPECIAL expr", "x %/% 2", ""),
    c("comparison <", "expr: expr LT expr", "x < 1", ""),
    c("comparison >", "expr: expr GT expr", "x > 0", ""),
    c("comparison ==", "expr: expr EQ expr", "x == 0", ""),
    c("comparison !=", "expr: expr NE expr", "x != 0", ""),
    c("comparison <=", "expr: expr LE expr", "x <= 0", ""),
    c("comparison >=", "expr: expr GE expr", "x >= 0", ""),
    c("logical and (&)", "expr: expr AND expr", "(x > 0) & (x < 1)", ""),
    c("logical or (|)", "expr: expr OR expr", "(x > 0) | (x < 1)", ""),
    c("logical and (&&)", "expr: expr AND2 expr", "(n > 0L) && (n < 9L)", ""),
    c("logical or (||)", "expr: expr OR2 expr", "(n > 0L) || (n < 9L)", ""),
    c("pipe (|>)", "expr: expr PIPE expr", "x |> sin()", ""),
    c("local assignment", "expr: expr LEFT_ASSIGN expr", "a <- x + 1; a", ""),
    c("unary math call", "expr: SYMBOL_FUNCTION_CALL '(' ... ')'", "sin(x)", ""),
    c("fold reducer call", "expr: SYMBOL_FUNCTION_CALL '(' ... ')'", "sum(x)", ""),
    c("unknown call", "expr: SYMBOL_FUNCTION_CALL '(' ... ')'", "qux(x)", ""),
    c("namespaced call (::)", "expr: SYMBOL NS_GET SYMBOL", "base::sin(x)", ""),
    c("if / else (scalar)", "expr: IF ifcond expr_or_assign ELSE expr_or_assign", "if (n > 0L) n else -n", ""),
    c("for loop + indexed write", "expr: FOR forcond expr_or_assign", "out <- x; for (i in 1:n) { out[i] <- out[i] * 2 }; out", ""),
    c("while loop", "expr: WHILE cond expr_or_assign", "while (n > 0L) { }; x", ""),
    c("repeat loop", "expr: REPEAT expr_or_assign", "repeat { break }; x", ""),
    c("next", "expr: NEXT", "out <- x; for (i in 1:n) { if (i > 0L) next; out[i] <- 0 }; out", ""),
    c("break", "expr: BREAK", "out <- x; for (i in 1:n) { if (i > 0L) break; out[i] <- 0 }; out", ""),
    c("single index ([)", "expr: expr '[' subscript ']'", "x[1L]", ""),
    c("range slice ([)", "expr: expr '[' subscript ']'", "x[1:n]", ""),
    c("double index ([[)", "expr: expr LBB subscript ']' ']'", "x[[1L]]", ""),
    c("dollar ($)", "expr: expr '$' SYMBOL", "x$a", ""),
    c("slot (@)", "expr: expr '@' SYMBOL", "x@a", ""),
    c("anonymous function", "expr: FUNCTION '(' formlist ')' ...", "(function(z) z)(x)", "")
  )
  df <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(df) <- c("production", "rule", "example", "decl")
  df$decl[!nzchar(df$decl)] <- "x = double(NA), n = integer()"
  df
}

# Compute the coverage table: productions + the disposition the frontend gives
# each. This is the source of the generated docs/r-subset-grammar.md.
#' @keywords internal
#' @noRd
tccq_grammar_coverage <- function() {
  p <- tccq_grammar_productions()
  p$disposition <- vapply(
    seq_len(nrow(p)),
    function(i) tccq_grammar_probe_one(p$example[[i]], p$decl[[i]]),
    character(1)
  )
  p
}
