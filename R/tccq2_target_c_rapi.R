# tccq2_target_c_rapi.R - C + R C API target for the fresh compiler
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_target_c_rapi <- function() {
  structure(
    list(
      name = "c_rapi",
      entry_spec = function(module, ctx = list()) {
        list(
          args = rep("sexp", length(module$formal_names)),
          returns = "sexp"
        )
      },
      emit = function(module, ctx = list()) {
        tccq2_c_emit_module(module, ctx)
      }
    ),
    class = "tccq2_target"
  )
}

tccq2_c_emit_module <- function(module, ctx = list()) {
  tccq2_module_validate(module)
  if (is.null(module$kernel)) {
    tccq2_abort("module has no kernel; run middle-end passes before codegen")
  }

  sym <- tccq2_c_symbol_table(module)
  params <- paste(
    paste0("SEXP ", vapply(module$formal_names, function(nm) sym[[nm]]$arg, character(1))),
    collapse = ", "
  )

  lines <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <Rmath.h>",
    "#include <math.h>",
    "",
    "#ifndef REAL_RO",
    "#define REAL_RO(x) REAL(x)",
    "#endif",
    "#ifndef INTEGER_RO",
    "#define INTEGER_RO(x) INTEGER(x)",
    "#endif",
    "#ifndef LOGICAL_RO",
    "#define LOGICAL_RO(x) LOGICAL(x)",
    "#endif",
    "",
    paste0("SEXP ", module$entry, "(", params, ") {"),
    tccq2_indent(tccq2_c_emit_argument_setup(module, sym), 2L),
    tccq2_indent(tccq2_c_emit_kernel(module, sym), 2L),
    "}"
  )

  paste(lines, collapse = "\n")
}

tccq2_c_symbol_table <- function(module) {
  out <- list()
  for (nm in module$formal_names) {
    id <- tccq2_c_ident(nm)
    out[[nm]] <- list(
      arg = paste0("arg_", id),
      ptr = paste0("p_", id),
      len = paste0("n_", id),
      val = paste0("v_", id),
      type = module$types[[nm]]
    )
  }
  out
}

tccq2_c_emit_argument_setup <- function(module, sym) {
  lines <- character()

  for (nm in module$formal_names) {
    s <- sym[[nm]]
    type <- s$type
    sexptype <- tccq2_sexptype_for_mode(type$mode)
    cname <- tccq2_c_string(nm)

    lines <- c(
      lines,
      paste0("if (TYPEOF(", s$arg, ") != ", sexptype, ") {"),
      paste0("  Rf_error(\"argument %s has wrong R type\", ", cname, ");"),
      "}"
    )

    if (type$rank == 0L) {
      lines <- c(
        lines,
        paste0("if (XLENGTH(", s$arg, ") < 1) {"),
        paste0("  Rf_error(\"scalar argument %s is empty\", ", cname, ");"),
        "}",
        paste0(
          tccq2_c_scalar_type_for_mode(type$mode), " ", s$val,
          " = ", tccq2_c_ro_accessor(type$mode), "(", s$arg, ")[0];"
        )
      )
    } else if (type$rank == 1L) {
      lines <- c(
        lines,
        paste0("R_xlen_t ", s$len, " = XLENGTH(", s$arg, ");"),
        paste0(
          "const ", tccq2_c_scalar_type_for_mode(type$mode), " *", s$ptr,
          " = ", tccq2_c_ro_accessor(type$mode), "(", s$arg, ");"
        )
      )
    } else {
      tccq2_abort(
        "C target milestone 1 supports scalar/vector only; argument ", nm,
        " has rank ", type$rank
      )
    }
  }

  lines
}

tccq2_c_ro_accessor <- function(mode) {
  switch(
    mode,
    double = "REAL_RO",
    integer = "INTEGER_RO",
    logical = "LOGICAL_RO",
    raw = "RAW",
    tccq2_abort("unsupported accessor mode: ", mode)
  )
}

tccq2_c_rw_accessor <- function(mode) {
  switch(
    mode,
    double = "REAL",
    integer = "INTEGER",
    logical = "LOGICAL",
    raw = "RAW",
    tccq2_abort("unsupported writable accessor mode: ", mode)
  )
}

tccq2_c_emit_domain_length <- function(domain, sym) {
  if (!identical(domain$tag, "domain")) {
    tccq2_abort("expected domain node for loop length emission")
  }

  vec_vars <- domain$vars %||% character()
  if (!length(vec_vars)) {
    return("R_xlen_t n_out = 1;")
  }

  first <- vec_vars[[1L]]
  lines <- paste0("R_xlen_t n_out = ", sym[[first]]$len, ";")

  if (length(vec_vars) > 1L) {
    for (nm in vec_vars[-1L]) {
      lines <- c(
        lines,
        paste0("if (", sym[[nm]]$len, " != n_out) {"),
        paste0("  Rf_error(\"vector length mismatch for %s\", ", tccq2_c_string(nm), ");"),
        "}"
      )
    }
  }

  lines
}

tccq2_c_emit_kernel <- function(module, sym) {
  kernel <- module$kernel

  switch(
    kernel$tag,
    scalar_kernel = tccq2_c_emit_kernel_scalar(kernel, sym),
    materialize = tccq2_c_emit_kernel_materialize(kernel, sym),
    fold = tccq2_c_emit_kernel_fold(kernel, sym),
    tccq2_abort("unknown kernel tag: ", kernel$tag)
  )
}

tccq2_c_emit_kernel_scalar <- function(kernel, sym) {
  mode <- kernel$type$mode
  out_sexp <- tccq2_sexptype_for_mode(mode)
  expr <- tccq2_c_emit_expr(kernel$expr, sym, idx = NULL)

  c(
    paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", 1));"),
    tccq2_c_emit_assign_scalar("out", mode, "0", expr),
    "UNPROTECT(1);",
    "return out;"
  )
}

tccq2_c_emit_kernel_materialize <- function(kernel, sym) {
  mode <- kernel$type$mode
  out_sexp <- tccq2_sexptype_for_mode(mode)
  producer <- kernel$producer
  expr <- producer$elem
  elem <- tccq2_c_emit_expr(expr, sym, idx = "i")

  c(
    tccq2_c_emit_domain_length(producer$domain, sym),
    paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", n_out));"),
    paste0(tccq2_c_scalar_type_for_mode(mode), " *p_out = ", tccq2_c_rw_accessor(mode), "(out);"),
    "for (R_xlen_t i = 0; i < n_out; ++i) {",
    paste0("  p_out[i] = (", tccq2_c_scalar_type_for_mode(mode), ")(", elem, ");"),
    "}",
    "UNPROTECT(1);",
    "return out;"
  )
}

tccq2_c_emit_kernel_fold <- function(kernel, sym) {
  producer <- kernel$elem
  expr <- producer$elem
  elem <- tccq2_c_emit_expr(expr, sym, idx = "i")
  if (!identical(kernel$op, "sum")) {
    tccq2_abort("fresh C target only supports sum fold")
  }

  c(
    tccq2_c_emit_domain_length(kernel$domain, sym),
    "double acc = 0.0;",
    "for (R_xlen_t i = 0; i < n_out; ++i) {",
    paste0("  acc += (double)(", elem, ");"),
    "}",
    "SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));",
    "REAL(out)[0] = acc;",
    "UNPROTECT(1);",
    "return out;"
  )
}

tccq2_c_emit_assign_scalar <- function(out, mode, index, expr) {
  switch(
    mode,
    double = paste0("REAL(", out, ")[", index, "] = (double)(", expr, ");"),
    integer = paste0("INTEGER(", out, ")[", index, "] = (int)(", expr, ");"),
    logical = paste0("LOGICAL(", out, ")[", index, "] = (int)(", expr, ");"),
    raw = paste0("RAW(", out, ")[", index, "] = (Rbyte)(", expr, ");"),
    tccq2_abort("unsupported scalar assign mode: ", mode)
  )
}

tccq2_c_emit_expr <- function(node, sym, idx = NULL) {
  if (!is.list(node) || is.null(node$tag)) {
    tccq2_abort("expected IR node in C expression emitter")
  }

  switch(
    node$tag,
    const = tccq2_c_const_literal(node$value, node$type$mode),
    var = tccq2_c_emit_var(node, sym, idx),
    unary = tccq2_c_emit_unary(node, sym, idx),
    binary = tccq2_c_emit_binary(node, sym, idx),
    call1 = tccq2_c_emit_call1(node, sym, idx),
    len = tccq2_c_emit_len(node, sym),
    reduce = tccq2_abort("nested reduce is not supported in milestone 1"),
    tccq2_abort("unsupported IR tag in C expression emitter: ", node$tag)
  )
}

tccq2_c_emit_var <- function(node, sym, idx = NULL) {
  s <- sym[[node$name]]
  if (is.null(s)) {
    tccq2_abort("unknown C symbol for variable: ", node$name)
  }

  if (node$type$rank == 0L) {
    return(s$val)
  }

  if (is.null(idx)) {
    tccq2_abort("vector variable '", node$name, "' requires an index in C expression emission")
  }

  paste0(s$ptr, "[", idx, "]")
}

tccq2_c_emit_unary <- function(node, sym, idx = NULL) {
  x <- tccq2_c_emit_expr(node$x, sym, idx)
  switch(
    node$op,
    `-` = paste0("(-(", x, "))"),
    tccq2_abort("unsupported unary op: ", node$op)
  )
}

tccq2_c_emit_binary <- function(node, sym, idx = NULL) {
  lhs <- tccq2_c_emit_expr(node$lhs, sym, idx)
  rhs <- tccq2_c_emit_expr(node$rhs, sym, idx)

  if (identical(node$op, "^")) {
    return(paste0("pow((double)(", lhs, "), (double)(", rhs, "))"))
  }

  paste0("((", lhs, ") ", node$op, " (", rhs, "))")
}

tccq2_c_emit_call1 <- function(node, sym, idx = NULL) {
  x <- tccq2_c_emit_expr(node$x, sym, idx)
  fun <- switch(
    node$fun,
    sin = "sin",
    cos = "cos",
    tan = "tan",
    exp = "exp",
    log = "log",
    sqrt = "sqrt",
    abs = "fabs",
    tccq2_abort("unsupported C math function: ", node$fun)
  )
  paste0(fun, "((double)(", x, "))")
}

tccq2_c_emit_len <- function(node, sym) {
  if (!identical(node$x$tag, "var")) {
    tccq2_abort("length() currently supports direct variables only")
  }
  s <- sym[[node$x$name]]
  if (is.null(s)) {
    tccq2_abort("unknown C symbol for length(): ", node$x$name)
  }
  if (node$x$type$rank == 0L) {
    return("1")
  }
  paste0("(int)", s$len)
}
