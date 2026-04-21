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
    "static R_xlen_t tccq2_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {",
    "  if (idx < 1 || idx > len) {",
    "    Rf_error(\"index out of bounds for %s\", name);",
    "  }",
    "  return idx - 1;",
    "}",
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
      name = nm,
      kind = "formal",
      arg = paste0("arg_", id),
      sexp = paste0("arg_", id),
      ptr = paste0("p_", id),
      len = paste0("n_", id),
      val = paste0("v_", id),
      type = module$types[[nm]]
    )
  }

  locals <- if (!is.null(module$ir)) tccq2_ir_program_locals(module$ir) else list()
  for (nm in names(locals)) {
    id <- tccq2_c_ident(nm)
    out[[nm]] <- list(
      name = nm,
      kind = "local",
      arg = NULL,
      sexp = paste0("loc_", id),
      ptr = paste0("p_", id),
      len = paste0("n_", id),
      val = paste0("v_", id),
      type = locals[[nm]]
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
        "C target supports scalar/vector only in this milestone; argument ", nm,
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

tccq2_c_emit_common_length_named <- function(expr, sym, len_name) {
  if (identical(expr$tag, "slice_range")) {
    setup <- tccq2_c_emit_slice_range_indices(expr, sym, prefix = len_name)
    return(c(setup, paste0("R_xlen_t ", len_name, " = n_", len_name, ";")))
  }

  vec_vars <- tccq2_ir_vector_vars(expr)
  if (!length(vec_vars)) {
    tccq2_abort("vector expression has no vector inputs; cannot infer length")
  }

  first <- vec_vars[[1L]]
  lines <- paste0("R_xlen_t ", len_name, " = ", sym[[first]]$len, ";")

  if (length(vec_vars) > 1L) {
    for (nm in vec_vars[-1L]) {
      lines <- c(
        lines,
        paste0("if (", sym[[nm]]$len, " != ", len_name, ") {"),
        paste0("  Rf_error(\"vector length mismatch for %s\", ", tccq2_c_string(nm), ");"),
        "}"
      )
    }
  }

  lines
}

tccq2_c_emit_common_length <- function(module, sym, expr) {
  tccq2_c_emit_common_length_named(expr, sym, "n_out")
}

tccq2_c_expr_length <- function(expr, sym) {
  if (identical(expr$tag, "var") && expr$type$rank == 1L) {
    return(sym[[expr$name]]$len)
  }
  if (identical(expr$tag, "slice_range")) {
    tccq2_abort("slice length as inline expression is not supported; materialize/bind it first")
  }
  vec_vars <- tccq2_ir_vector_vars(expr)
  if (length(vec_vars) == 1L) {
    return(sym[[vec_vars[[1L]]]]$len)
  }
  tccq2_abort("cannot infer inline vector length for expression")
}

tccq2_c_emit_kernel <- function(module, sym) {
  kernel <- module$kernel
  c(
    "int tccq2_nprotect = 0;",
    switch(
      kernel$tag,
      scalar_kernel = tccq2_c_emit_kernel_scalar(kernel, sym),
      materialize = tccq2_c_emit_kernel_materialize(kernel, sym),
      fold = tccq2_c_emit_kernel_fold(kernel, sym),
      kernel_program = tccq2_c_emit_kernel_program(kernel, sym),
      tccq2_abort("unknown kernel tag: ", kernel$tag)
    )
  )
}

tccq2_c_emit_kernel_scalar <- function(kernel, sym) {
  mode <- kernel$type$mode
  out_sexp <- tccq2_sexptype_for_mode(mode)
  expr <- tccq2_c_emit_expr(kernel$expr, sym, idx = NULL)

  c(
    paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", 1));"),
    "++tccq2_nprotect;",
    tccq2_c_emit_assign_scalar("out", mode, "0", expr),
    "UNPROTECT(tccq2_nprotect);",
    "return out;"
  )
}

tccq2_c_emit_kernel_materialize <- function(kernel, sym) {
  mode <- kernel$type$mode
  out_sexp <- tccq2_sexptype_for_mode(mode)
  producer <- kernel$producer
  expr <- producer$elem

  if (identical(expr$tag, "slice_range")) {
    return(tccq2_c_emit_materialize_slice_range(expr, sym, out_name = "out", out_ptr = "p_out"))
  }

  elem <- tccq2_c_emit_expr(expr, sym, idx = "i")

  c(
    tccq2_c_emit_domain_length(producer$domain, sym),
    paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", n_out));"),
    "++tccq2_nprotect;",
    paste0(tccq2_c_scalar_type_for_mode(mode), " *p_out = ", tccq2_c_rw_accessor(mode), "(out);"),
    "for (R_xlen_t i = 0; i < n_out; ++i) {",
    paste0("  p_out[i] = (", tccq2_c_scalar_type_for_mode(mode), ")(", elem, ");"),
    "}",
    "UNPROTECT(tccq2_nprotect);",
    "return out;"
  )
}

tccq2_c_emit_kernel_fold <- function(kernel, sym) {
  producer <- if (identical(kernel$elem$tag, "materialize")) kernel$elem$producer else kernel$elem
  expr <- producer$elem

  if (!identical(kernel$op, "sum")) {
    tccq2_abort("fresh C target only supports sum fold")
  }

  if (identical(expr$tag, "slice_range")) {
    setup <- tccq2_c_emit_slice_range_indices(expr, sym, prefix = "fold")
    elem <- tccq2_c_emit_slice_range_elem(expr, sym, idx = "i", lo_name = "lo_fold")
    loop_len <- "n_fold"
  } else {
    setup <- tccq2_c_emit_domain_length(kernel$domain, sym)
    elem <- tccq2_c_emit_expr(expr, sym, idx = "i")
    loop_len <- "n_out"
  }

  c(
    setup,
    "double acc = 0.0;",
    paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
    paste0("  acc += (double)(", elem, ");"),
    "}",
    "SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));",
    "++tccq2_nprotect;",
    "REAL(out)[0] = acc;",
    "UNPROTECT(tccq2_nprotect);",
    "return out;"
  )
}

tccq2_c_emit_kernel_program <- function(kernel, sym) {
  c(
    unlist(lapply(kernel$stmts, tccq2_c_emit_stmt, sym = sym), use.names = FALSE),
    switch(
      kernel$result_kernel$tag,
      scalar_kernel = tccq2_c_emit_kernel_scalar(kernel$result_kernel, sym),
      materialize = tccq2_c_emit_kernel_materialize(kernel$result_kernel, sym),
      fold = tccq2_c_emit_kernel_fold(kernel$result_kernel, sym),
      tccq2_abort("unsupported program result kernel: ", kernel$result_kernel$tag)
    )
  )
}

tccq2_c_emit_stmt <- function(stmt, sym) {
  switch(
    stmt$tag,
    bind = tccq2_c_emit_stmt_bind(stmt, sym),
    store_index = tccq2_c_emit_stmt_store_index(stmt, sym),
    store_range = tccq2_c_emit_stmt_store_range(stmt, sym),
    tccq2_abort("unsupported statement tag: ", stmt$tag)
  )
}

tccq2_c_emit_stmt_bind <- function(stmt, sym) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local")) {
    tccq2_abort("bind target must be a local symbol: ", stmt$name)
  }

  type <- stmt$type

  if (type$rank == 0L) {
    expr <- tccq2_c_emit_expr(stmt$value, sym, idx = NULL)
    return(paste0(
      tccq2_c_scalar_type_for_mode(type$mode), " ", s$val,
      " = (", tccq2_c_scalar_type_for_mode(type$mode), ")(", expr, ");"
    ))
  }

  if (type$rank != 1L) {
    tccq2_abort("local bind currently supports scalar/vector only")
  }

  if (identical(stmt$value$tag, "slice_range")) {
    return(tccq2_c_emit_bind_slice_range(stmt, sym))
  }

  len_lines <- tccq2_c_emit_common_length_named(stmt$value, sym, s$len)
  elem <- tccq2_c_emit_expr(stmt$value, sym, idx = "i")

  c(
    len_lines,
    paste0("SEXP ", s$sexp, " = PROTECT(Rf_allocVector(", tccq2_sexptype_for_mode(type$mode), ", ", s$len, "));"),
    "++tccq2_nprotect;",
    paste0(tccq2_c_scalar_type_for_mode(type$mode), " *", s$ptr, " = ", tccq2_c_rw_accessor(type$mode), "(", s$sexp, ");"),
    paste0("for (R_xlen_t i = 0; i < ", s$len, "; ++i) {"),
    paste0("  ", s$ptr, "[i] = (", tccq2_c_scalar_type_for_mode(type$mode), ")(", elem, ");"),
    "}"
  )
}

tccq2_c_emit_stmt_store_index <- function(stmt, sym) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local")) {
    tccq2_abort("indexed assignment target must be a local vector: ", stmt$name)
  }
  if (s$type$rank != 1L) {
    tccq2_abort("indexed assignment target must be a vector: ", stmt$name)
  }

  index <- tccq2_c_emit_expr(stmt$index, sym, idx = NULL)
  value <- tccq2_c_emit_expr(stmt$value, sym, idx = NULL)
  ctype <- tccq2_c_scalar_type_for_mode(s$type$mode)
  id <- tccq2_c_ident(stmt$name)

  c(
    paste0("R_xlen_t j_", id, " = tccq2_checked_index1((R_xlen_t)(", index, "), ", s$len, ", ", tccq2_c_string(stmt$name), ");"),
    paste0(s$ptr, "[j_", id, "] = (", ctype, ")(", value, ");")
  )
}

tccq2_c_emit_stmt_store_range <- function(stmt, sym) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local")) {
    tccq2_abort("range assignment target must be a local vector: ", stmt$name)
  }
  if (s$type$rank != 1L) {
    tccq2_abort("range assignment target must be a vector: ", stmt$name)
  }
  if (!tccq2_is_scalar_rhs_for_assignment(stmt$value)) {
    tccq2_abort("fresh compiler currently supports scalar RHS range assignment only")
  }

  id <- tccq2_c_ident(stmt$name)
  start <- tccq2_c_emit_expr(stmt$start, sym, idx = NULL)
  stop <- tccq2_c_emit_expr(stmt$stop, sym, idx = NULL)
  ctype <- tccq2_c_scalar_type_for_mode(s$type$mode)
  rhs_value <- tccq2_c_emit_expr(stmt$value, sym, idx = NULL)

  c(
    paste0("R_xlen_t lo_", id, " = tccq2_checked_index1((R_xlen_t)(", start, "), ", s$len, ", ", tccq2_c_string(stmt$name), ");"),
    paste0("R_xlen_t hi_", id, " = tccq2_checked_index1((R_xlen_t)(", stop, "), ", s$len, ", ", tccq2_c_string(stmt$name), ");"),
    paste0("if (hi_", id, " < lo_", id, ") { Rf_error(\"decreasing ranges are not supported in indexed assignment\"); }"),
    paste0("R_xlen_t n_rng_", id, " = hi_", id, " - lo_", id, " + 1;"),
    paste0("for (R_xlen_t i = 0; i < n_rng_", id, "; ++i) {"),
    paste0("  ", s$ptr, "[lo_", id, " + i] = (", ctype, ")(", rhs_value, ");"),
    "}"
  )
}

tccq2_c_emit_slice_range_indices <- function(node, sym, prefix) {
  if (!identical(node$x$tag, "var")) {
    tccq2_abort("slice_range currently supports direct variable base only")
  }

  base <- sym[[node$x$name]]
  start <- tccq2_c_emit_expr(node$start, sym, idx = NULL)
  stop <- tccq2_c_emit_expr(node$stop, sym, idx = NULL)

  c(
    paste0("R_xlen_t lo_", prefix, " = tccq2_checked_index1((R_xlen_t)(", start, "), ", base$len, ", ", tccq2_c_string(node$x$name), ");"),
    paste0("R_xlen_t hi_", prefix, " = tccq2_checked_index1((R_xlen_t)(", stop, "), ", base$len, ", ", tccq2_c_string(node$x$name), ");"),
    paste0("if (hi_", prefix, " < lo_", prefix, ") { Rf_error(\"decreasing slices are not supported\"); }"),
    paste0("R_xlen_t n_", prefix, " = hi_", prefix, " - lo_", prefix, " + 1;")
  )
}

tccq2_c_emit_slice_range_elem <- function(node, sym, idx, lo_name) {
  if (!identical(node$x$tag, "var")) {
    tccq2_abort("slice_range currently supports direct variable base only")
  }
  base <- sym[[node$x$name]]
  paste0(base$ptr, "[", lo_name, " + ", idx, "]")
}

tccq2_c_emit_materialize_slice_range <- function(node, sym, out_name, out_ptr) {
  mode <- node$type$mode
  ctype <- tccq2_c_scalar_type_for_mode(mode)
  setup <- tccq2_c_emit_slice_range_indices(node, sym, prefix = out_name)
  elem <- tccq2_c_emit_slice_range_elem(node, sym, idx = "i", lo_name = paste0("lo_", out_name))

  c(
    setup,
    paste0("SEXP ", out_name, " = PROTECT(Rf_allocVector(", tccq2_sexptype_for_mode(mode), ", n_", out_name, "));"),
    "++tccq2_nprotect;",
    paste0(ctype, " *", out_ptr, " = ", tccq2_c_rw_accessor(mode), "(", out_name, ");"),
    paste0("for (R_xlen_t i = 0; i < n_", out_name, "; ++i) {"),
    paste0("  ", out_ptr, "[i] = (", ctype, ")(", elem, ");"),
    "}",
    "UNPROTECT(tccq2_nprotect);",
    paste0("return ", out_name, ";")
  )
}

tccq2_c_emit_bind_slice_range <- function(stmt, sym) {
  s <- sym[[stmt$name]]
  mode <- stmt$type$mode
  ctype <- tccq2_c_scalar_type_for_mode(mode)
  setup <- tccq2_c_emit_slice_range_indices(stmt$value, sym, prefix = s$len)
  elem <- tccq2_c_emit_slice_range_elem(stmt$value, sym, idx = "i", lo_name = paste0("lo_", s$len))

  c(
    setup,
    paste0("R_xlen_t ", s$len, " = n_", s$len, ";"),
    paste0("SEXP ", s$sexp, " = PROTECT(Rf_allocVector(", tccq2_sexptype_for_mode(mode), ", ", s$len, "));"),
    "++tccq2_nprotect;",
    paste0(ctype, " *", s$ptr, " = ", tccq2_c_rw_accessor(mode), "(", s$sexp, ");"),
    paste0("for (R_xlen_t i = 0; i < ", s$len, "; ++i) {"),
    paste0("  ", s$ptr, "[i] = (", ctype, ")(", elem, ");"),
    "}"
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
    index = tccq2_c_emit_index(node, sym, idx),
    slice_range = tccq2_c_emit_slice_range_expr(node, sym, idx),
    reduce = tccq2_abort("nested reduce is not supported in milestone 1"),
    boundary = tccq2_abort("boundary nodes are not emitted directly"),
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

tccq2_c_emit_index <- function(node, sym, idx = NULL) {
  if (!identical(node$x$tag, "var")) {
    tccq2_abort("x[i] currently supports direct variable base only")
  }
  base <- sym[[node$x$name]]
  index <- tccq2_c_emit_expr(node$index, sym, idx = NULL)
  paste0(
    base$ptr,
    "[tccq2_checked_index1((R_xlen_t)(", index, "), ",
    base$len,
    ", ",
    tccq2_c_string(node$x$name),
    ")]"
  )
}

tccq2_c_emit_slice_range_expr <- function(node, sym, idx = NULL) {
  if (is.null(idx)) {
    tccq2_abort("slice_range requires an element index in expression emission")
  }
  if (!identical(node$x$tag, "var")) {
    tccq2_abort("slice_range currently supports direct variable base only")
  }

  base <- sym[[node$x$name]]
  start <- tccq2_c_emit_expr(node$start, sym, idx = NULL)
  paste0(
    base$ptr,
    "[tccq2_checked_index1((R_xlen_t)(", start, "), ",
    base$len,
    ", ",
    tccq2_c_string(node$x$name),
    ") + ",
    idx,
    "]"
  )
}
