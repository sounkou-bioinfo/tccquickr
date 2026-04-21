# tccq2_utils.R - fresh compiler utility helpers
# SPDX-License-Identifier: GPL-3.0-or-later

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

tccq2_abort <- function(..., call. = FALSE) {
  stop(paste0(...), call. = call.)
}

tccq2_assert <- function(cond, ..., call. = FALSE) {
  if (!isTRUE(cond)) {
    tccq2_abort(..., call. = call.)
  }
  invisible(TRUE)
}

tccq2_is_call_to <- function(expr, name) {
  is.call(expr) && identical(tccq2_call_head_name(expr), name)
}

tccq2_call_head_name <- function(expr) {
  if (!is.call(expr)) {
    return(NULL)
  }
  head <- expr[[1L]]
  if (is.symbol(head)) {
    return(as.character(head))
  }
  NULL
}

tccq2_symbol_name <- function(x, what = "symbol") {
  if (is.symbol(x)) {
    return(as.character(x))
  }
  tccq2_abort("expected ", what, ", got ", typeof(x))
}

tccq2_c_ident <- function(name) {
  out <- gsub("[^A-Za-z0-9_]", "_", as.character(name))
  out <- gsub("_+", "_", out)
  if (!grepl("^[A-Za-z_]", out)) {
    out <- paste0("_", out)
  }
  out
}

tccq2_c_string <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x)
  paste0("\"", x, "\"")
}

tccq2_unique <- function(x) {
  x[!duplicated(x)]
}

tccq2_indent <- function(lines, n = 2L) {
  pad <- paste(rep(" ", n), collapse = "")
  paste0(pad, lines)
}

tccq2_flatten_lines <- function(...) {
  parts <- list(...)
  unlist(parts, recursive = FALSE, use.names = FALSE)
}

tccq2_require_namespace <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tccq2_abort("package '", pkg, "' is required for this operation")
  }
  invisible(TRUE)
}
