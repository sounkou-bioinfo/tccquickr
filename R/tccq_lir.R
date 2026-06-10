# tccq_lir.R - lowered IR: the target-neutral, execution-shaped seam
# SPDX-License-Identifier: GPL-3.0-or-later
#
# LIR sits between the middle-end (R-array semantics) and the printers
# (C today; Fortran/Rust/... later). See docs/decisions/0002-lowered-ir-seam.md.
#
# Invariant: an LIR node must be expressible without naming C. Ownership,
# aliasing, element types, ranks, and known extents are carried as data so that
# printers are mechanical and so device/parallel backends can read the
# elementwise domain rather than reconstructing it from emitted text
# (docs/decisions/0003-target-and-backend-roadmap.md).
#
# This module is intentionally standalone: constructors + validator + traversal,
# with no pipeline wiring yet. lower_to_lir and the LIR-driven C printer land
# surface by surface against differential tests.

# Vocabularies ----------------------------------------------------------------

# Element types are deliberately small and target-neutral. Layout (row/column
# major) and base index (0/1) are printer choices, never encoded here.
tccq_lir_elts <- c("double", "int", "logical", "xlen", "ptr", "void")

# Ownership of a storage-bearing value. "owned" means this region allocates and
# may be freed/returned; "borrowed" means it aliases storage owned elsewhere and
# must not be freed or mutated unless a barrier already forced materialization.
tccq_lir_ownerships <- c("owned", "borrowed")

# Node constructor ------------------------------------------------------------

tccq_lir <- function(tag, ...) {
  structure(
    c(list(tag = tag), list(...)),
    class = c(paste0("tccq_lir_", tag), "tccq_lir_node")
  )
}

tccq_is_lir <- function(x) inherits(x, "tccq_lir_node")

# Regions ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
tccq_lir_func <- function(name, params, body, ret = NULL) {
  tccq_lir("func", name = as.character(name), params = params, body = body, ret = ret)
}

#' @keywords internal
#' @noRd
tccq_lir_block <- function(stmts = list()) {
  tccq_lir("block", stmts = stmts)
}

# Values ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
tccq_lir_const <- function(value, elt) {
  tccq_lir("const", value = value, elt = elt)
}

#' @keywords internal
#' @noRd
tccq_lir_param <- function(name, elt, rank = 0L, extent = NULL, own = "borrowed") {
  tccq_lir(
    "param",
    name = as.character(name), elt = elt, rank = as.integer(rank),
    extent = extent, own = own
  )
}

#' @keywords internal
#' @noRd
tccq_lir_temp <- function(name, elt) {
  tccq_lir("temp", name = as.character(name), elt = elt)
}

#' @keywords internal
#' @noRd
tccq_lir_binop <- function(op, lhs, rhs, elt) {
  tccq_lir("binop", op = as.character(op), lhs = lhs, rhs = rhs, elt = elt)
}

#' @keywords internal
#' @noRd
tccq_lir_unop <- function(op, x, elt) {
  tccq_lir("unop", op = as.character(op), x = x, elt = elt)
}

#' @keywords internal
#' @noRd
tccq_lir_call <- function(fun, args, elt) {
  tccq_lir("call", fun = as.character(fun), args = args, elt = elt)
}

# Affine address into a storage-bearing value. `terms` is a list of integer or
# value-node offsets summed to form the linear index; layout/base are resolved
# by the printer per target.
#' @keywords internal
#' @noRd
tccq_lir_index <- function(base, terms, elt) {
  tccq_lir("index", base = base, terms = terms, elt = elt)
}

# Memory ----------------------------------------------------------------------

#' @keywords internal
#' @noRd
tccq_lir_alloc <- function(elt, extent, own = "owned") {
  tccq_lir("alloc", elt = elt, extent = extent, own = own)
}

#' @keywords internal
#' @noRd
tccq_lir_load <- function(addr, elt) {
  tccq_lir("load", addr = addr, elt = elt)
}

#' @keywords internal
#' @noRd
tccq_lir_store <- function(addr, value) {
  tccq_lir("store", addr = addr, value = value)
}

# Borrowed contiguous span of an existing value: offset/extent into `base`.
#' @keywords internal
#' @noRd
tccq_lir_view <- function(base, offset, extent, elt) {
  tccq_lir("view", base = base, offset = offset, extent = extent, elt = elt, own = "borrowed")
}

# Control ---------------------------------------------------------------------

# Counted loop. `extent` may carry a known-constant trip count for unrolling /
# static sizing (this is where tccq_jit constant specialization lands; see
# docs/decisions/0004-recon-and-jit-cleanup.md).
#' @keywords internal
#' @noRd
tccq_lir_for <- function(var, lo, hi, body, extent = NULL) {
  tccq_lir("for", var = as.character(var), lo = lo, hi = hi, body = body, extent = extent)
}

#' @keywords internal
#' @noRd
tccq_lir_if <- function(cond, then, els = NULL) {
  tccq_lir("if", cond = cond, then = then, els = els)
}

# Boundary --------------------------------------------------------------------

# Opaque R-eval region. Printers must round-trip it unchanged and never inline,
# reorder, or fuse across it. This is the legality barrier from the high IR,
# preserved into LIR as data.
#' @keywords internal
#' @noRd
tccq_lir_boundary <- function(reason, inputs = list(), outputs = list(), elt = "void") {
  tccq_lir("boundary", reason = as.character(reason), inputs = inputs, outputs = outputs, elt = elt)
}

# Validation ------------------------------------------------------------------

# Structural legality only. Type/shape correctness is the middle-end's job;
# by the time we reach LIR those facts are assumed and merely carried.
#' @keywords internal
#' @noRd
tccq_lir_validate <- function(node, path = "<root>") {
  if (!tccq_is_lir(node)) {
    tccq_abort("LIR validate: expected an LIR node at ", path, ", got ", class(node)[[1L]])
  }

  check_elt <- function(elt, where) {
    if (is.null(elt) || !is.character(elt) || length(elt) != 1L || !(elt %in% tccq_lir_elts)) {
      tccq_abort("LIR validate: bad element type at ", where, ": ", deparse(elt))
    }
  }
  check_own <- function(own, where) {
    if (!identical(own, "owned") && !identical(own, "borrowed")) {
      tccq_abort("LIR validate: bad ownership at ", where, ": ", deparse(own))
    }
  }
  child <- function(x, where) tccq_lir_validate(x, where)
  children <- function(xs, where) {
    if (!is.list(xs)) tccq_abort("LIR validate: expected a list at ", where)
    for (i in seq_along(xs)) child(xs[[i]], paste0(where, "[", i, "]"))
  }

  tag <- node$tag
  here <- paste0(path, "/", tag)

  switch(
    tag,
    func = {
      children(node$params, paste0(here, ".params"))
      child(node$body, paste0(here, ".body"))
      if (!is.null(node$ret)) child(node$ret, paste0(here, ".ret"))
    },
    block = children(node$stmts, paste0(here, ".stmts")),
    const = check_elt(node$elt, here),
    param = {
      check_elt(node$elt, here)
      check_own(node$own, here)
    },
    temp = check_elt(node$elt, here),
    binop = {
      check_elt(node$elt, here)
      child(node$lhs, paste0(here, ".lhs"))
      child(node$rhs, paste0(here, ".rhs"))
    },
    unop = {
      check_elt(node$elt, here)
      child(node$x, paste0(here, ".x"))
    },
    call = {
      check_elt(node$elt, here)
      children(node$args, paste0(here, ".args"))
    },
    index = {
      check_elt(node$elt, here)
      child(node$base, paste0(here, ".base"))
      children(node$terms[vapply(node$terms, tccq_is_lir, logical(1))], paste0(here, ".terms"))
    },
    alloc = {
      check_elt(node$elt, here)
      check_own(node$own, here)
      child(node$extent, paste0(here, ".extent"))
    },
    load = {
      check_elt(node$elt, here)
      child(node$addr, paste0(here, ".addr"))
    },
    store = {
      child(node$addr, paste0(here, ".addr"))
      child(node$value, paste0(here, ".value"))
    },
    view = {
      check_elt(node$elt, here)
      child(node$base, paste0(here, ".base"))
      child(node$offset, paste0(here, ".offset"))
      child(node$extent, paste0(here, ".extent"))
    },
    `for` = {
      child(node$lo, paste0(here, ".lo"))
      child(node$hi, paste0(here, ".hi"))
      child(node$body, paste0(here, ".body"))
    },
    `if` = {
      child(node$cond, paste0(here, ".cond"))
      child(node$then, paste0(here, ".then"))
      if (!is.null(node$els)) child(node$els, paste0(here, ".els"))
    },
    boundary = {
      check_elt(node$elt, here)
      children(node$inputs, paste0(here, ".inputs"))
      children(node$outputs, paste0(here, ".outputs"))
    },
    tccq_abort("LIR validate: unknown node tag '", tag, "' at ", path)
  )

  invisible(TRUE)
}

# Traversal -------------------------------------------------------------------

# Pre-order walk over LIR child nodes. `f` is called on each node; return value
# is ignored (use for collection via side effects, in the style of tccq_ir_walk).
#' @keywords internal
#' @noRd
tccq_lir_walk <- function(node, f) {
  if (!tccq_is_lir(node)) {
    return(invisible(NULL))
  }
  f(node)
  for (nm in names(node)) {
    if (identical(nm, "tag")) next
    val <- node[[nm]]
    if (tccq_is_lir(val)) {
      tccq_lir_walk(val, f)
    } else if (is.list(val)) {
      for (el in val) {
        if (tccq_is_lir(el)) tccq_lir_walk(el, f)
      }
    }
  }
  invisible(NULL)
}
