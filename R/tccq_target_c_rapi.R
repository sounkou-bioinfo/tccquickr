# tccq_target_c_rapi.R - C + R C API target for tccq
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_target_c_rapi <- function() {
  structure(
    list(
      name = "c_rapi",
      capabilities = tccq_target_capabilities(
        c = TRUE,
        r_api = TRUE,
        boundary_apis = "r_eval",
        source_artifact = "c"
      ),
      entry_spec = function(module, ctx = list()) {
        list(
          args = rep("sexp", length(module$formal_names)),
          returns = "sexp"
        )
      },
      emit = function(module, ctx = list()) {
        tccq_c_emit_module(module, ctx)
      }
    ),
    class = "tccq_target"
  )
}

tccq_c_emit_module <- function(module, ctx = list()) {
  tccq_module_validate(module)
  if (is.null(module$kernel)) {
    tccq_abort("module has no kernel; run middle-end passes before codegen")
  }

  sym <- tccq_c_symbol_table(module)
  params <- paste(
    paste0("SEXP ", vapply(module$formal_names, function(nm) sym[[nm]]$arg, character(1))),
    collapse = ", "
  )

  lines <- c(
    "#include <R.h>",
    "#include <Rinternals.h>",
    "#include <Rmath.h>",
    "#include <math.h>",
    "#include <limits.h>",
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
    "#ifndef TCCQ_UNUSED",
    "# if defined(__GNUC__)",
    "#  define TCCQ_UNUSED __attribute__((unused))",
    "# else",
    "#  define TCCQ_UNUSED",
    "# endif",
    "#endif",
    "",
    "static TCCQ_UNUSED R_xlen_t tccq_checked_index1(R_xlen_t idx, R_xlen_t len, const char *name) {",
    "  if (idx < 1 || idx > len) {",
    "    Rf_error(\"index out of bounds for %s\", name);",
    "  }",
    "  return idx - 1;",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_lgl_not(int x) {",
    "  return x == NA_LOGICAL ? NA_LOGICAL : (!x);",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_int_idiv(int a, int b) {",
    "  if (a == NA_INTEGER || b == NA_INTEGER || b == 0) return NA_INTEGER;",
    "  int q = a / b;",
    "  int r = a % b;",
    "  if (r != 0 && ((a < 0) != (b < 0))) --q;",
    "  return q;",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_int_checked(long long x) {",
    "  if (x > INT_MAX || x <= INT_MIN) return NA_INTEGER;",
    "  return (int)x;",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_int_add(int a, int b) {",
    "  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;",
    "  return tccq_int_checked((long long)a + (long long)b);",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_int_sub(int a, int b) {",
    "  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;",
    "  return tccq_int_checked((long long)a - (long long)b);",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_int_mul(int a, int b) {",
    "  if (a == NA_INTEGER || b == NA_INTEGER) return NA_INTEGER;",
    "  return tccq_int_checked((long long)a * (long long)b);",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_int_neg(int a) {",
    "  if (a == NA_INTEGER) return NA_INTEGER;",
    "  return tccq_int_checked(-((long long)a));",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_lgl_and(int a, int b) {",
    "  if (a == 0 || b == 0) return 0;",
    "  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;",
    "  return 1;",
    "}",
    "",
    "static TCCQ_UNUSED int tccq_lgl_or(int a, int b) {",
    "  if (a == 1 || b == 1) return 1;",
    "  if (a == NA_LOGICAL || b == NA_LOGICAL) return NA_LOGICAL;",
    "  return 0;",
    "}",
    "",
    paste0("SEXP ", module$entry, "(", params, ") {"),
    tccq_indent(tccq_c_emit_argument_setup(module, sym), 2L),
    tccq_indent(tccq_c_emit_kernel(module, sym), 2L),
    "}"
  )

  paste(lines, collapse = "\n")
}

tccq_c_symbol_table <- function(module) {
  out <- list()

  for (nm in module$formal_names) {
    id <- tccq_c_ident(nm)
    out[[nm]] <- list(
      name = nm,
      kind = "formal",
      arg = paste0("arg_", id),
      sexp = paste0("arg_", id),
      ptr = paste0("p_", id),
      len = paste0("n_", id),
      nrow = paste0("nr_", id),
      ncol = paste0("nc_", id),
      val = paste0("v_", id),
      type = module$types[[nm]]
    )
  }

  locals <- if (!is.null(module$ir)) tccq_ir_program_locals(module$ir) else list()
  for (nm in names(locals)) {
    id <- tccq_c_ident(nm)
    out[[nm]] <- list(
      name = nm,
      kind = "local",
      arg = NULL,
      sexp = paste0("loc_", id),
      ptr = paste0("p_", id),
      len = paste0("n_", id),
      nrow = paste0("nr_", id),
      ncol = paste0("nc_", id),
      val = paste0("v_", id),
      type = locals[[nm]]
    )
  }

  c_names <- vapply(out, function(s) tccq_c_ident(s$name), character(1))
  if (anyDuplicated(c_names)) {
    dup <- unique(c_names[duplicated(c_names)])
    tccq_abort(
      "tccq names collide after C identifier normalization: ",
      paste(dup, collapse = ", "),
      ". Rename formals/locals to distinct C identifiers."
    )
  }

  out
}

tccq_c_emit_scalar_value_length_check <- function(s, value_vars, label) {
  if (!s$name %in% value_vars) {
    return(character())
  }
  c(
    paste0("if (", s$len, " != 1) {"),
    paste0("  Rf_error(\"scalar value %s has runtime length %lld; vector-valued scalar use is not supported\", ", tccq_c_string(label), ", (long long)", s$len, ");"),
    "}"
  )
}

tccq_c_emit_argument_setup <- function(module, sym) {
  lines <- character()
  scalar_value_vars <- tccq_c_module_scalar_value_vars(module)

  for (nm in module$formal_names) {
    s <- sym[[nm]]
    type <- s$type
    sexptype <- tccq_sexptype_for_mode(type$mode)
    cname <- tccq_c_string(nm)

    lines <- c(
      lines,
      paste0("if (TYPEOF(", s$arg, ") != ", sexptype, ") {"),
      paste0("  Rf_error(\"argument %s has wrong R type\", ", cname, ");"),
      "}"
    )

    if (type$rank == 0L) {
      lines <- c(
        lines,
        paste0("R_xlen_t ", s$len, " = XLENGTH(", s$arg, ");"),
        paste0("if (", s$len, " < 1) {"),
        paste0("  Rf_error(\"scalar argument %s is empty\", ", cname, ");"),
        "}",
        tccq_c_emit_scalar_value_length_check(s, scalar_value_vars, nm),
        paste0(
          tccq_c_scalar_type_for_mode(type$mode), " ", s$val,
          " = ", tccq_c_ro_accessor(type$mode), "(", s$arg, ")[0];"
        )
      )
    } else if (type$rank == 1L) {
      cache <- paste0("tccq_arg_ptr_cache_", tccq_c_ident(nm))
      lines <- c(
        lines,
        paste0("R_xlen_t ", s$len, " = XLENGTH(", s$arg, ");"),
        paste0("const ", tccq_c_scalar_type_for_mode(type$mode), " *", cache, " = NULL;"),
        paste0(
          "#define ", s$ptr, " (", cache, " == NULL ? (", cache, " = ",
          tccq_c_ro_accessor(type$mode), "(", s$arg, ")) : ", cache, ")"
        )
      )
    } else if (type$rank == 2L) {
      cache <- paste0("tccq_arg_ptr_cache_", tccq_c_ident(nm))
      dim_name <- paste0("dim_", tccq_c_ident(nm))
      lines <- c(
        lines,
        paste0("SEXP ", dim_name, " = Rf_getAttrib(", s$arg, ", R_DimSymbol);"),
        paste0("if (TYPEOF(", dim_name, ") != INTSXP || LENGTH(", dim_name, ") != 2) {"),
        paste0("  Rf_error(\"matrix argument %s must have integer dim attribute of length 2\", ", cname, ");"),
        "}",
        paste0("R_xlen_t ", s$nrow, " = (R_xlen_t)INTEGER(", dim_name, ")[0];"),
        paste0("R_xlen_t ", s$ncol, " = (R_xlen_t)INTEGER(", dim_name, ")[1];"),
        paste0("R_xlen_t ", s$len, " = XLENGTH(", s$arg, ");"),
        paste0("if (", s$nrow, " < 0 || ", s$ncol, " < 0 || ", s$len, " != ", s$nrow, " * ", s$ncol, ") {"),
        paste0("  Rf_error(\"matrix argument %s has inconsistent dimensions\", ", cname, ");"),
        "}",
        paste0("const ", tccq_c_scalar_type_for_mode(type$mode), " *", cache, " = NULL;"),
        paste0(
          "#define ", s$ptr, " (", cache, " == NULL ? (", cache, " = ",
          tccq_c_ro_accessor(type$mode), "(", s$arg, ")) : ", cache, ")"
        )
      )
    } else {
      tccq_abort(
        "C target supports scalar/vector/matrix only in this milestone; argument ", nm,
        " has rank ", type$rank
      )
    }
  }

  lines
}

tccq_c_ro_accessor <- function(mode) {
  switch(
    mode,
    double = "REAL_RO",
    integer = "INTEGER_RO",
    logical = "LOGICAL_RO",
    raw = "RAW",
    tccq_abort("unsupported accessor mode: ", mode)
  )
}

tccq_c_rw_accessor <- function(mode) {
  switch(
    mode,
    double = "REAL",
    integer = "INTEGER",
    logical = "LOGICAL",
    raw = "RAW",
    tccq_abort("unsupported writable accessor mode: ", mode)
  )
}

tccq_c_emit_shape_domain_length <- function(shape_domain, sym, module, len_name, preferred_names = NULL) {
  if (is.null(shape_domain) || is.null(module$shape_facts)) {
    return(NULL)
  }

  witnesses <- tccq_shape_domain_witness_names(module$shape_facts, shape_domain)
  if (!is.null(preferred_names)) {
    preferred_names <- unique(preferred_names)
    witnesses <- intersect(witnesses, preferred_names)
    if (!length(preferred_names) || !setequal(witnesses, preferred_names)) {
      return(NULL)
    }
  }
  witnesses <- witnesses[witnesses %in% names(sym)]
  if (!length(witnesses)) {
    return(NULL)
  }

  first <- witnesses[[1L]]
  lines <- paste0("R_xlen_t ", len_name, " = ", sym[[first]]$len, ";")

  if (length(witnesses) > 1L) {
    for (nm in witnesses[-1L]) {
      lines <- c(
        lines,
        paste0("if (", sym[[nm]]$len, " != ", len_name, ") {"),
        "  Rf_error(\"vector length mismatch in shared shape domain\");",
        "}"
      )
      if (sym[[first]]$type$rank == 2L && sym[[nm]]$type$rank == 2L) {
        lines <- c(
          lines,
          paste0("if (", sym[[nm]]$nrow, " != ", sym[[first]]$nrow, " || ", sym[[nm]]$ncol, " != ", sym[[first]]$ncol, ") {"),
          "  Rf_error(\"matrix dimension mismatch in shared shape domain\");",
          "}"
        )
      }
    }
  }

  lines
}

tccq_c_emit_domain_length <- function(domain, sym, module = NULL, len_name = "n_out") {
  if (!identical(domain$tag, "domain")) {
    tccq_abort("expected domain node for loop length emission")
  }

  preferred <- domain$vars %||% character()
  from_shape <- tccq_c_emit_shape_domain_length(domain$shape_domain %||% NULL, sym, module, len_name, preferred_names = preferred)
  if (!is.null(from_shape)) {
    return(from_shape)
  }

  vec_vars <- domain$vars %||% character()
  if (!length(vec_vars)) {
    return(paste0("R_xlen_t ", len_name, " = 1;"))
  }

  first <- vec_vars[[1L]]
  lines <- paste0("R_xlen_t ", len_name, " = ", sym[[first]]$len, ";")

  if (length(vec_vars) > 1L) {
    for (nm in vec_vars[-1L]) {
      lines <- c(
        lines,
        paste0("if (", sym[[nm]]$len, " != ", len_name, ") {"),
        paste0("  Rf_error(\"vector length mismatch for %s\", ", tccq_c_string(nm), ");"),
        "}"
      )
    }
  }

  lines
}

tccq_c_emit_scalar_length_setup <- function(node, sym, module, len_name) {
  if (identical(node$tag, "const")) {
    return(paste0("R_xlen_t ", len_name, " = 1;"))
  }

  if (identical(node$tag, "var")) {
    s <- sym[[node$name]]
    if (is.null(s)) {
      tccq_abort("unknown C symbol for scalar length: ", node$name)
    }
    return(paste0("R_xlen_t ", len_name, " = ", s$len, ";"))
  }

  if (identical(node$tag, "boundary_call")) {
    prefix <- tccq_c_ident(len_name)
    return(c(
      tccq_c_emit_boundary_call(node, sym, module, expect_sexp = FALSE, prefix = prefix),
      paste0("R_xlen_t ", len_name, " = XLENGTH(val_", prefix, ");")
    ))
  }

  if (node$tag %in% c("unary", "call1")) {
    return(c(
      tccq_c_emit_scalar_length_setup(node$x, sym, module, paste0(len_name, "_x")),
      paste0("R_xlen_t ", len_name, " = ", len_name, "_x;")
    ))
  }

  if (identical(node$tag, "binary")) {
    lhs_name <- paste0(len_name, "_lhs")
    rhs_name <- paste0(len_name, "_rhs")
    return(c(
      tccq_c_emit_scalar_length_setup(node$lhs, sym, module, lhs_name),
      tccq_c_emit_scalar_length_setup(node$rhs, sym, module, rhs_name),
      paste0("if (", lhs_name, " != 1 && ", rhs_name, " != 1 && ", lhs_name, " != ", rhs_name, ") {"),
      "  Rf_error(\"scalar expression length mismatch\");",
      "}",
      paste0("R_xlen_t ", len_name, " = ", lhs_name, " != 1 ? ", lhs_name, " : ", rhs_name, ";")
    ))
  }

  expr <- tccq_c_emit_expr(node, sym, idx = NULL)
  ctype <- tccq_c_scalar_type_for_mode(node$type$mode)
  c(
    paste0(ctype, " ", len_name, "_value = (", ctype, ")(", expr, ");"),
    paste0("(void)", len_name, "_value;"),
    paste0("R_xlen_t ", len_name, " = 1;")
  )
}

tccq_c_emit_matrix_dim_setup <- function(expr, sym, nr_name, nc_name) {
  if (is.null(expr$type) || expr$type$rank != 2L) {
    tccq_abort("matrix dimension setup requires a rank-2 expression")
  }

  if (identical(expr$tag, "var")) {
    s <- sym[[expr$name]]
    return(c(
      paste0("R_xlen_t ", nr_name, " = ", s$nrow, ";"),
      paste0("R_xlen_t ", nc_name, " = ", s$ncol, ";")
    ))
  }

  if (identical(expr$tag, "matrix_fill")) {
    nr <- tccq_c_emit_expr(expr$nrow, sym, idx = NULL)
    nc <- tccq_c_emit_expr(expr$ncol, sym, idx = NULL)
    return(c(
      paste0("R_xlen_t ", nr_name, " = (R_xlen_t)(", nr, ");"),
      paste0("R_xlen_t ", nc_name, " = (R_xlen_t)(", nc, ");"),
      paste0("if (", nr_name, " < 0 || ", nc_name, " < 0) {"),
      "  Rf_error(\"matrix dimensions must be non-negative\");",
      "}"
    ))
  }

  if (expr$tag %in% c("unary", "call1")) {
    return(tccq_c_emit_matrix_dim_setup(expr$x, sym, nr_name, nc_name))
  }

  if (identical(expr$tag, "binary")) {
    lhs_matrix <- !is.null(expr$lhs$type) && expr$lhs$type$rank == 2L
    rhs_matrix <- !is.null(expr$rhs$type) && expr$rhs$type$rank == 2L
    if (lhs_matrix && rhs_matrix) {
      lhs_nr <- paste0(nr_name, "_lhs")
      lhs_nc <- paste0(nc_name, "_lhs")
      rhs_nr <- paste0(nr_name, "_rhs")
      rhs_nc <- paste0(nc_name, "_rhs")
      return(c(
        tccq_c_emit_matrix_dim_setup(expr$lhs, sym, lhs_nr, lhs_nc),
        tccq_c_emit_matrix_dim_setup(expr$rhs, sym, rhs_nr, rhs_nc),
        paste0("if (", lhs_nr, " != ", rhs_nr, " || ", lhs_nc, " != ", rhs_nc, ") {"),
        "  Rf_error(\"matrix dimension mismatch in composite expression\");",
        "}",
        paste0("R_xlen_t ", nr_name, " = ", lhs_nr, ";"),
        paste0("R_xlen_t ", nc_name, " = ", lhs_nc, ";")
      ))
    }
    if (lhs_matrix) {
      return(tccq_c_emit_matrix_dim_setup(expr$lhs, sym, nr_name, nc_name))
    }
    if (rhs_matrix) {
      return(tccq_c_emit_matrix_dim_setup(expr$rhs, sym, nr_name, nc_name))
    }
  }

  if (identical(expr$tag, "boundary_call")) {
    tccq_abort("rank-2 boundary_call dimension setup is not supported yet")
  }

  tccq_abort("cannot infer matrix dimensions for expression tag: ", expr$tag)
}

tccq_c_emit_matrix_length_setup <- function(expr, sym, len_name, nr_name = paste0("nr_", len_name), nc_name = paste0("nc_", len_name)) {
  c(
    tccq_c_emit_matrix_dim_setup(expr, sym, nr_name, nc_name),
    paste0("if (", nc_name, " != 0 && ", nr_name, " > R_XLEN_T_MAX / ", nc_name, ") {"),
    "  Rf_error(\"matrix dimensions exceed R length limits\");",
    "}",
    paste0("R_xlen_t ", len_name, " = ", nr_name, " * ", nc_name, ";")
  )
}

tccq_c_emit_common_length_named <- function(expr, sym, len_name, module = NULL) {
  emit_length <- function(node, target) {
    if (is.null(node$type) || node$type$rank == 0L) {
      return(tccq_c_emit_scalar_length_setup(node, sym, module, target))
    }

    if (node$type$rank == 2L) {
      return(tccq_c_emit_matrix_length_setup(node, sym, target))
    }

    from_shape <- tccq_c_emit_shape_domain_length(
      tccq_ir_shape_domain(node),
      sym,
      module,
      target,
      preferred_names = tccq_ir_vector_vars(node)
    )
    if (!is.null(from_shape) && !node$tag %in% c("slice_range", "view1") && !tccq_c_expr_length_needs_eval(node)) {
      return(from_shape)
    }

    switch(
      node$tag,
      var = paste0("R_xlen_t ", target, " = ", sym[[node$name]]$len, ";"),
      vector_fill = {
        n <- tccq_c_emit_expr(node$length, sym, idx = NULL)
        c(
          paste0("R_xlen_t ", target, " = (R_xlen_t)(", n, ");"),
          paste0("if (", target, " < 0) { Rf_error(\"vector length must be non-negative\"); }")
        )
      },
      matrix_view = tccq_c_emit_matrix_view_length_setup(node, sym, target),
      slice_range = {
        prefix <- paste0(target, "_view")
        c(
          tccq_c_emit_slice_range_indices(node, sym, prefix = prefix),
          paste0("R_xlen_t ", target, " = n_", prefix, ";")
        )
      },
      view1 = {
        prefix <- paste0(target, "_view")
        c(
          tccq_c_emit_slice_range_indices(node, sym, prefix = prefix),
          paste0("R_xlen_t ", target, " = n_", prefix, ";")
        )
      },
      unary = c(
        emit_length(node$x, paste0(target, "_x")),
        paste0("R_xlen_t ", target, " = ", target, "_x;")
      ),
      call1 = c(
        emit_length(node$x, paste0(target, "_x")),
        paste0("R_xlen_t ", target, " = ", target, "_x;")
      ),
      boundary_call = {
        prefix <- tccq_c_ident(target)
        c(
          tccq_c_emit_boundary_call(node, sym, module, expect_sexp = FALSE, prefix = prefix),
          paste0("R_xlen_t ", target, " = XLENGTH(val_", prefix, ");")
        )
      },
      index = {
        if (!is.null(node$type) && node$type$rank == 1L) {
          emit_length(node$index, target)
        } else {
          paste0("R_xlen_t ", target, " = 1;")
        }
      },
      binary = {
        left_name <- paste0(target, "_lhs")
        right_name <- paste0(target, "_rhs")
        left_vec <- node$lhs$type$rank > 0L
        right_vec <- node$rhs$type$rank > 0L

        c(
          emit_length(node$lhs, left_name),
          emit_length(node$rhs, right_name),
          if (left_vec && right_vec) c(
            paste0("if (", left_name, " != ", right_name, ") {"),
            "  Rf_error(\"vector length mismatch in composite expression\");",
            "}",
            paste0("R_xlen_t ", target, " = ", left_name, ";")
          ) else if (left_vec) {
            paste0("R_xlen_t ", target, " = ", left_name, ";")
          } else if (right_vec) {
            paste0("R_xlen_t ", target, " = ", right_name, ";")
          } else {
            paste0("R_xlen_t ", target, " = 1;")
          }
        )
      },
      tccq_abort("cannot infer vector length for expression tag: ", node$tag)
    )
  }

  emit_length(expr, len_name)
}

tccq_c_emit_common_length <- function(module, sym, expr) {
  tccq_c_emit_common_length_named(expr, sym, "n_out", module = module)
}

tccq_c_expr_length <- function(expr, sym) {
  if (identical(expr$tag, "var") && expr$type$rank == 1L) {
    return(sym[[expr$name]]$len)
  }
  if (expr$tag %in% c("slice_range", "view1")) {
    tccq_abort("slice/view length as inline expression is not supported; materialize/bind it first")
  }
  vec_vars <- tccq_ir_vector_vars(expr)
  if (length(vec_vars) == 1L) {
    return(sym[[vec_vars[[1L]]]]$len)
  }
  tccq_abort("cannot infer inline vector length for expression")
}

tccq_c_expr_length_needs_eval <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(FALSE)
  }

  if (is.null(node$type) || node$type$rank == 0L) {
    return(!node$tag %in% c("var", "const"))
  }

  switch(
    node$tag,
    var = FALSE,
    unary = tccq_c_expr_length_needs_eval(node$x),
    call1 = tccq_c_expr_length_needs_eval(node$x),
    binary = tccq_c_expr_length_needs_eval(node$lhs) || tccq_c_expr_length_needs_eval(node$rhs),
    index = tccq_c_expr_length_needs_eval(node$index),
    index2 = tccq_c_expr_length_needs_eval(node$row) || tccq_c_expr_length_needs_eval(node$col),
    matrix_fill = TRUE,
    vector_fill = tccq_c_expr_length_needs_eval(node$length),
    matrix_view = TRUE,
    slice_range = tccq_c_expr_length_needs_eval(node$start) || tccq_c_expr_length_needs_eval(node$stop),
    view1 = tccq_c_expr_length_needs_eval(node$start) || tccq_c_expr_length_needs_eval(node$stop),
    boundary_call = TRUE,
    len = tccq_c_expr_length_needs_eval(node$x),
    FALSE
  )
}

tccq_c_expr_length_data_vars <- function(node, module = NULL) {
  if (!is.list(node) || is.null(node$tag)) {
    return(character())
  }

  if (is.null(node$type) || node$type$rank == 0L) {
    return(tccq_c_expr_data_vars(node, module = module))
  }

  switch(
    node$tag,
    var = character(),
    unary = tccq_c_expr_length_data_vars(node$x, module = module),
    call1 = tccq_c_expr_length_data_vars(node$x, module = module),
    binary = tccq_unique(c(tccq_c_expr_length_data_vars(node$lhs, module = module), tccq_c_expr_length_data_vars(node$rhs, module = module))),
    index = if (!is.null(node$type) && node$type$rank == 1L) {
      tccq_c_expr_length_data_vars(node$index, module = module)
    } else {
      tccq_unique(c(tccq_c_expr_data_vars(node$x, module = module), tccq_c_expr_data_vars(node$index, module = module)))
    },
    index2 = tccq_unique(c(tccq_c_expr_data_vars(node$row, module = module), tccq_c_expr_data_vars(node$col, module = module))),
    matrix_fill = tccq_unique(c(tccq_c_expr_data_vars(node$nrow, module = module), tccq_c_expr_data_vars(node$ncol, module = module))),
    vector_fill = tccq_c_expr_data_vars(node$length, module = module),
    matrix_view = tccq_unique(c(tccq_c_expr_data_vars(node$x, module = module), tccq_c_subscripts_data_vars(node$subscripts, module = module))),
    slice_range = tccq_unique(c(tccq_c_expr_data_vars(node$start, module = module), tccq_c_expr_data_vars(node$stop, module = module))),
    view1 = tccq_unique(c(tccq_c_expr_data_vars(node$start, module = module), tccq_c_expr_data_vars(node$stop, module = module))),
    boundary_call = tccq_unique(unlist(Map(
      function(arg, i) tccq_c_boundary_arg_data_vars(arg, module, node, i),
      node$args %||% list(),
      seq_along(node$args %||% list())
    ), use.names = FALSE)),
    len = tccq_c_expr_length_data_vars(node$x, module = module),
    character()
  )
}

tccq_c_boundary_arg_data_vars <- function(arg, module, boundary, arg_index) {
  plan <- if (!is.null(module)) tccq_c_boundary_arg_plan(module, boundary, arg_index) else NULL
  strategy <- plan$strategy %||% NULL

  if (is.list(arg) && identical(arg$tag, "var") && !is.null(arg$type) && arg$type$rank > 0L) {
    if (!is.null(strategy) && strategy %in% c("pass_formal", "pass_local", "pass_source")) {
      return(character())
    }
    if (identical(strategy, "materialize_local")) {
      return(arg$name)
    }
  }

  tccq_c_expr_data_vars(arg, module = module)
}

tccq_c_expr_data_vars <- function(node, module = NULL) {
  if (!is.list(node) || is.null(node$tag)) {
    return(character())
  }

  switch(
    node$tag,
    const = character(),
    var = if (!is.null(node$type) && node$type$rank > 0L) node$name else character(),
    unary = tccq_c_expr_data_vars(node$x, module = module),
    call1 = tccq_c_expr_data_vars(node$x, module = module),
    binary = tccq_unique(c(tccq_c_expr_data_vars(node$lhs, module = module), tccq_c_expr_data_vars(node$rhs, module = module))),
    reduce = tccq_c_expr_data_vars(node$x, module = module),
    arg_reduce = tccq_c_expr_data_vars(node$x, module = module),
    len = tccq_c_expr_length_data_vars(node$x, module = module),
    index = tccq_unique(c(tccq_c_expr_data_vars(node$x, module = module), tccq_c_expr_data_vars(node$index, module = module))),
    index2 = tccq_unique(c(tccq_c_expr_data_vars(node$x, module = module), tccq_c_expr_data_vars(node$row, module = module), tccq_c_expr_data_vars(node$col, module = module))),
    matrix_fill = tccq_unique(c(tccq_c_expr_data_vars(node$value, module = module), tccq_c_expr_data_vars(node$nrow, module = module), tccq_c_expr_data_vars(node$ncol, module = module))),
    vector_fill = tccq_unique(c(tccq_c_expr_data_vars(node$value, module = module), tccq_c_expr_data_vars(node$length, module = module))),
    matrix_view = tccq_unique(c(tccq_c_expr_data_vars(node$x, module = module), tccq_c_subscripts_data_vars(node$subscripts, module = module))),
    slice_range = tccq_unique(c(tccq_c_expr_data_vars(node$x, module = module), tccq_c_expr_data_vars(node$start, module = module), tccq_c_expr_data_vars(node$stop, module = module))),
    view1 = tccq_unique(c(tccq_c_expr_data_vars(node$x, module = module), tccq_c_expr_data_vars(node$start, module = module), tccq_c_expr_data_vars(node$stop, module = module))),
    boundary_call = tccq_unique(unlist(Map(
      function(arg, i) tccq_c_boundary_arg_data_vars(arg, module, node, i),
      node$args %||% list(),
      seq_along(node$args %||% list())
    ), use.names = FALSE)),
    character()
  )
}

tccq_c_subscript_data_vars <- function(sub, module = NULL) {
  if (identical(sub$kind, "index")) {
    return(tccq_c_expr_data_vars(sub$index, module = module))
  }
  if (identical(sub$kind, "range")) {
    return(tccq_unique(c(
      tccq_c_expr_data_vars(sub$start, module = module),
      tccq_c_expr_data_vars(sub$stop, module = module)
    )))
  }
  character()
}

tccq_c_subscripts_data_vars <- function(subscripts, module = NULL) {
  tccq_unique(unlist(lapply(subscripts %||% list(), tccq_c_subscript_data_vars, module = module), use.names = FALSE))
}

tccq_c_subscript_scalar_value_vars <- function(sub) {
  if (identical(sub$kind, "index")) {
    return(tccq_c_expr_scalar_value_vars(sub$index))
  }
  if (identical(sub$kind, "range")) {
    return(tccq_unique(c(tccq_c_expr_scalar_value_vars(sub$start), tccq_c_expr_scalar_value_vars(sub$stop))))
  }
  character()
}

tccq_c_subscripts_scalar_value_vars <- function(subscripts) {
  tccq_unique(unlist(lapply(subscripts %||% list(), tccq_c_subscript_scalar_value_vars), use.names = FALSE))
}

tccq_c_stmt_data_vars <- function(stmt, module) {
  if (!is.list(stmt) || is.null(stmt$tag)) {
    return(character())
  }

  switch(
    stmt$tag,
    bind = {
      kind <- tccq_c_storage_kind(module, stmt$name)
      if (kind %in% c("alias", "view")) {
        value <- stmt$value
        if (is.list(value) && value$tag %in% c("slice_range", "view1")) {
          return(tccq_unique(c(tccq_c_expr_data_vars(value$start, module = module), tccq_c_expr_data_vars(value$stop, module = module))))
        }
        return(character())
      }
      tccq_c_expr_data_vars(stmt$value, module = module)
    },
    store_index = tccq_unique(c(stmt$name, tccq_c_expr_data_vars(stmt$access, module = module), tccq_c_expr_data_vars(stmt$index, module = module), tccq_c_expr_data_vars(stmt$value, module = module))),
    store_range = tccq_unique(c(stmt$name, tccq_c_expr_data_vars(stmt$access, module = module), tccq_c_expr_data_vars(stmt$start, module = module), tccq_c_expr_data_vars(stmt$stop, module = module), tccq_c_expr_data_vars(stmt$value, module = module))),
    store_index2 = tccq_unique(c(stmt$name, tccq_c_expr_data_vars(stmt$row, module = module), tccq_c_expr_data_vars(stmt$col, module = module), tccq_c_expr_data_vars(stmt$value, module = module))),
    store_access = tccq_unique(c(stmt$name, tccq_c_expr_data_vars(stmt$access, module = module), tccq_c_subscripts_data_vars(stmt$subscripts, module = module), tccq_c_expr_data_vars(stmt$value, module = module))),
    for_loop = tccq_unique(c(tccq_c_expr_data_vars(stmt$start, module = module), tccq_c_expr_data_vars(stmt$stop, module = module), unlist(lapply(stmt$body %||% list(), tccq_c_stmt_data_vars, module = module), use.names = FALSE))),
    character()
  )
}

tccq_c_access_scalar_value_vars <- function(node) {
  plan <- tccq_ir_normalized_access(node)
  if (is.null(plan)) {
    return(character())
  }

  vars <- character()
  for (step in plan$steps %||% list()) {
    if (identical(step$kind, "slice")) {
      vars <- c(vars, tccq_c_expr_scalar_value_vars(step$start), tccq_c_expr_scalar_value_vars(step$stop))
    } else if (identical(step$kind, "index")) {
      vars <- c(vars, tccq_c_expr_scalar_value_vars(step$index))
    }
  }
  tccq_unique(vars)
}

tccq_c_expr_length_scalar_value_vars <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(character())
  }

  if (!is.null(node$type) && node$type$rank == 0L) {
    return(switch(
      node$tag,
      const = character(),
      var = character(),
      unary = tccq_c_expr_length_scalar_value_vars(node$x),
      call1 = tccq_c_expr_length_scalar_value_vars(node$x),
      binary = tccq_unique(c(tccq_c_expr_length_scalar_value_vars(node$lhs), tccq_c_expr_length_scalar_value_vars(node$rhs))),
      index = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$index))),
      index2 = tccq_unique(c(tccq_c_expr_scalar_value_vars(node$row), tccq_c_expr_scalar_value_vars(node$col))),
      arg_reduce = tccq_c_expr_length_scalar_value_vars(node$x),
      boundary_call = character(),
      len = tccq_c_expr_length_scalar_value_vars(node$x),
      tccq_c_expr_scalar_value_vars(node)
    ))
  }

  switch(
    node$tag,
    var = character(),
    unary = tccq_c_expr_length_scalar_value_vars(node$x),
    call1 = tccq_c_expr_length_scalar_value_vars(node$x),
    binary = tccq_unique(c(tccq_c_expr_length_scalar_value_vars(node$lhs), tccq_c_expr_length_scalar_value_vars(node$rhs))),
    vector_fill = tccq_c_expr_scalar_value_vars(node$length),
    matrix_view = tccq_c_subscripts_scalar_value_vars(node$subscripts),
    slice_range = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$start), tccq_c_expr_scalar_value_vars(node$stop))),
    view1 = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$start), tccq_c_expr_scalar_value_vars(node$stop))),
    index = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$index))),
    index2 = tccq_unique(c(tccq_c_expr_scalar_value_vars(node$row), tccq_c_expr_scalar_value_vars(node$col))),
    boundary_call = character(),
    len = tccq_c_expr_length_scalar_value_vars(node$x),
    character()
  )
}

tccq_c_expr_scalar_value_vars <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(character())
  }

  switch(
    node$tag,
    const = character(),
    var = if (!is.null(node$type) && node$type$rank == 0L) node$name else character(),
    unary = tccq_c_expr_scalar_value_vars(node$x),
    call1 = tccq_c_expr_scalar_value_vars(node$x),
    binary = tccq_unique(c(tccq_c_expr_scalar_value_vars(node$lhs), tccq_c_expr_scalar_value_vars(node$rhs))),
    reduce = tccq_c_expr_scalar_value_vars(node$x),
    arg_reduce = tccq_c_expr_scalar_value_vars(node$x),
    len = tccq_c_expr_length_scalar_value_vars(node$x),
    index = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$index))),
    index2 = tccq_unique(c(tccq_c_expr_scalar_value_vars(node$row), tccq_c_expr_scalar_value_vars(node$col))),
    matrix_fill = tccq_unique(c(tccq_c_expr_scalar_value_vars(node$value), tccq_c_expr_scalar_value_vars(node$nrow), tccq_c_expr_scalar_value_vars(node$ncol))),
    vector_fill = tccq_unique(c(tccq_c_expr_scalar_value_vars(node$value), tccq_c_expr_scalar_value_vars(node$length))),
    matrix_view = tccq_c_subscripts_scalar_value_vars(node$subscripts),
    slice_range = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$start), tccq_c_expr_scalar_value_vars(node$stop))),
    view1 = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$start), tccq_c_expr_scalar_value_vars(node$stop))),
    boundary_call = tccq_unique(unlist(lapply(node$args %||% list(), tccq_c_expr_scalar_value_vars), use.names = FALSE)),
    character()
  )
}

tccq_c_stmt_scalar_value_vars <- function(stmt) {
  if (!is.list(stmt) || is.null(stmt$tag)) {
    return(character())
  }

  switch(
    stmt$tag,
    store_index = tccq_unique(c(tccq_c_access_scalar_value_vars(stmt$access), tccq_c_expr_scalar_value_vars(stmt$index), tccq_c_expr_scalar_value_vars(stmt$value))),
    store_range = tccq_unique(c(tccq_c_access_scalar_value_vars(stmt$access), tccq_c_expr_scalar_value_vars(stmt$start), tccq_c_expr_scalar_value_vars(stmt$stop), tccq_c_expr_scalar_value_vars(stmt$value))),
    store_index2 = tccq_unique(c(tccq_c_expr_scalar_value_vars(stmt$row), tccq_c_expr_scalar_value_vars(stmt$col), tccq_c_expr_scalar_value_vars(stmt$value))),
    store_access = tccq_unique(c(tccq_c_access_scalar_value_vars(stmt$access), tccq_c_subscripts_scalar_value_vars(stmt$subscripts), tccq_c_expr_scalar_value_vars(stmt$value))),
    for_loop = tccq_unique(c(tccq_c_expr_scalar_value_vars(stmt$start), tccq_c_expr_scalar_value_vars(stmt$stop), unlist(lapply(stmt$body %||% list(), tccq_c_stmt_scalar_value_vars), use.names = FALSE))),
    character()
  )
}

tccq_c_stmt_eager_scalar_value_vars <- function(stmt) {
  if (!is.list(stmt) || !identical(stmt$tag, "bind") || is.null(stmt$type) || stmt$type$rank == 0L) {
    return(character())
  }

  if (is.list(stmt$value) && stmt$value$tag %in% c("slice_range", "view1", "index")) {
    return(tccq_c_access_scalar_value_vars(stmt$value))
  }

  if (is.list(stmt$value) && identical(stmt$value$tag, "var")) {
    return(character())
  }

  tccq_c_expr_scalar_value_vars(stmt$value)
}

tccq_c_module_scalar_value_vars <- function(module) {
  ir <- module$ir
  if (is.null(ir) || !identical(ir$tag, "program")) {
    return(tccq_c_expr_scalar_value_vars(ir))
  }

  value_vars <- tccq_unique(c(
    tccq_c_expr_scalar_value_vars(ir$result),
    unlist(lapply(ir$stmts %||% list(), tccq_c_stmt_scalar_value_vars), use.names = FALSE),
    unlist(lapply(ir$stmts %||% list(), tccq_c_stmt_eager_scalar_value_vars), use.names = FALSE)
  ))

  repeat {
    before <- value_vars
    for (stmt in ir$stmts %||% list()) {
      if (!identical(stmt$tag, "bind") || !stmt$name %in% value_vars) {
        next
      }
      value_vars <- tccq_unique(c(value_vars, tccq_c_expr_scalar_value_vars(stmt$value)))
    }
    if (setequal(before, value_vars)) {
      break
    }
  }

  value_vars
}

tccq_c_module_data_vars <- function(module) {
  ir <- module$ir
  if (is.null(ir) || !identical(ir$tag, "program")) {
    return(tccq_c_expr_data_vars(ir))
  }

  barrier_vars <- unlist(lapply(
    module$storage_plan$write_barriers %||% list(),
    function(x) c(x$materialize_views %||% character(), if (isTRUE(x$copy_target)) x$target else character())
  ), use.names = FALSE)

  data <- tccq_unique(c(
    unlist(lapply(ir$stmts %||% list(), tccq_c_stmt_data_vars, module = module), use.names = FALSE),
    tccq_c_expr_data_vars(ir$result, module = module),
    barrier_vars
  ))

  repeat {
    before <- data
    for (stmt in ir$stmts %||% list()) {
      if (!identical(stmt$tag, "bind") || !stmt$name %in% data) {
        next
      }
      kind <- tccq_c_storage_kind(module, stmt$name)
      if (identical(kind, "alias") && identical(stmt$value$tag, "var") && stmt$value$type$rank > 0L) {
        data <- tccq_unique(c(data, stmt$value$name))
      } else if (identical(kind, "view")) {
        plan <- tccq_ir_normalized_access(stmt$value)
        base_name <- plan$base_name %||% if (is.list(stmt$value$x) && identical(stmt$value$x$tag, "var")) stmt$value$x$name else NULL
        if (!is.null(base_name)) {
          data <- tccq_unique(c(data, base_name))
        }
      }
    }
    if (setequal(before, data)) {
      break
    }
  }

  data
}

tccq_c_storage_binding <- function(module, name) {
  module$storage_plan$bindings[[name]] %||% list(kind = "owned")
}

tccq_c_storage_kind <- function(module, name) {
  tccq_c_storage_binding(module, name)$kind %||% "owned"
}

tccq_c_owned_flag <- function(sym_entry) {
  paste0("own_", tccq_c_ident(sym_entry$name))
}

tccq_c_scalar_sexp_alloc <- function(mode, sexp_name, expr) {
  switch(
    mode,
    double = c(
      paste0("SEXP ", sexp_name, " = PROTECT(Rf_allocVector(REALSXP, 1));"),
      "++tccq_nprotect;",
      paste0("REAL(", sexp_name, ")[0] = (double) (", expr, ");")
    ),
    integer = c(
      paste0("SEXP ", sexp_name, " = PROTECT(Rf_allocVector(INTSXP, 1));"),
      "++tccq_nprotect;",
      paste0("INTEGER(", sexp_name, ")[0] = (int) (", expr, ");")
    ),
    logical = c(
      paste0("SEXP ", sexp_name, " = PROTECT(Rf_allocVector(LGLSXP, 1));"),
      "++tccq_nprotect;",
      paste0("LOGICAL(", sexp_name, ")[0] = (int) (", expr, ");")
    ),
    raw = c(
      paste0("SEXP ", sexp_name, " = PROTECT(Rf_allocVector(RAWSXP, 1));"),
      "++tccq_nprotect;",
      paste0("RAW(", sexp_name, ")[0] = (Rbyte) (", expr, ");")
    ),
    xlen = c(
      paste0("R_xlen_t ", sexp_name, "_xlen = (R_xlen_t)(", expr, ");"),
      paste0("int ", sexp_name, "_fits_int = ", sexp_name, "_xlen >= (R_xlen_t)INT_MIN && ", sexp_name, "_xlen <= (R_xlen_t)INT_MAX;"),
      paste0("SEXP ", sexp_name, " = PROTECT(Rf_allocVector(", sexp_name, "_fits_int ? INTSXP : REALSXP, 1));"),
      "++tccq_nprotect;",
      paste0("if (", sexp_name, "_fits_int) {"),
      paste0("  INTEGER(", sexp_name, ")[0] = (int)", sexp_name, "_xlen;"),
      "} else {",
      paste0("  REAL(", sexp_name, ")[0] = (double)", sexp_name, "_xlen;"),
      "}"
    ),
    tccq_abort("unsupported scalar boxing mode: ", mode)
  )
}

tccq_c_boundary_arg_plan <- function(module, boundary, arg_index) {
  boundary_id <- boundary$boundary_id %||% NULL
  if (is.null(boundary_id)) {
    return(NULL)
  }
  plans <- module$storage_plan$boundary_args[[as.character(boundary_id)]] %||% NULL
  if (is.null(plans) || length(plans) < arg_index) {
    return(NULL)
  }
  plans[[arg_index]]
}

tccq_c_emit_boundary_arg <- function(arg, sym, module, name, boundary = NULL, arg_index = NULL) {
  plan <- if (!is.null(boundary) && !is.null(arg_index)) tccq_c_boundary_arg_plan(module, boundary, arg_index) else NULL

  if (identical(arg$tag, "var")) {
    s <- sym[[arg$name]]
    if (is.null(s)) {
      tccq_abort("unknown symbol in boundary arg: ", arg$name)
    }
    if (arg$type$rank > 0L) {
      strategy <- plan$strategy %||% NULL
      if (!is.null(strategy) && strategy %in% c("pass_formal", "pass_local", "pass_source")) {
        sexp_name <- plan$sexp %||% arg$name
        sexp_sym <- sym[[sexp_name]]
        if (is.null(sexp_sym)) {
          tccq_abort("unknown planned boundary arg SEXP: ", sexp_name)
        }
        return(list(lines = character(), sexp = sexp_sym$sexp))
      }
      if (identical(strategy, "materialize_local")) {
        return(list(lines = c(tccq_c_emit_materialize_local(arg$name, sym, module)), sexp = s$sexp))
      }
      if (identical(s$kind, "formal")) {
        return(list(lines = character(), sexp = s$sexp))
      }
      return(list(lines = c(tccq_c_emit_materialize_local(arg$name, sym, module)), sexp = s$sexp))
    }
    if (identical(s$kind, "formal")) {
      return(list(lines = character(), sexp = s$sexp))
    }
    expr <- tccq_c_emit_expr(arg, sym, idx = NULL)
    return(list(lines = tccq_c_scalar_sexp_alloc(arg$type$mode, name, expr), sexp = name))
  }

  if (identical(arg$type$rank, 0L)) {
    expr <- tccq_c_emit_expr(arg, sym, idx = NULL)
    return(list(lines = tccq_c_scalar_sexp_alloc(arg$type$mode, name, expr), sexp = name))
  }

  tccq_abort("boundary args currently support direct vars or scalar expressions only")
}

tccq_c_emit_boundary_call <- function(node, sym, module, expect_sexp = TRUE, prefix = "boundary") {
  if (!identical(node$api, "r_eval")) {
    tccq_abort("only r_eval boundary emission is supported in this milestone")
  }

  call_expr <- node$metadata$call_expr
  head <- tccq_call_head_name(call_expr)
  if (is.null(head)) {
    tccq_abort("r_eval boundary emission currently requires a simple function symbol head")
  }

  prefix <- tccq_c_ident(prefix)
  call_name <- paste0("call_", prefix)
  val_name <- paste0("val_", prefix)

  arg_lines <- character()
  arg_sexps <- character()
  for (i in seq_along(node$args)) {
    tmp <- tccq_c_emit_boundary_arg(
      node$args[[i]],
      sym,
      module,
      paste0("arg_", prefix, "_", i),
      boundary = node,
      arg_index = i
    )
    arg_lines <- c(arg_lines, tmp$lines)
    arg_sexps[[i]] <- tmp$sexp
  }

  lang_line <- switch(
    as.character(length(arg_sexps)),
    `0` = paste0("SEXP ", call_name, " = PROTECT(Rf_lang1(Rf_install(\"HEAD\")));"),
    `1` = paste0("SEXP ", call_name, " = PROTECT(Rf_lang2(Rf_install(\"HEAD\"), ", arg_sexps[[1L]], "));"),
    `2` = paste0("SEXP ", call_name, " = PROTECT(Rf_lang3(Rf_install(\"HEAD\"), ", arg_sexps[[1L]], ", ", arg_sexps[[2L]], "));"),
    `3` = paste0("SEXP ", call_name, " = PROTECT(Rf_lang4(Rf_install(\"HEAD\"), ", arg_sexps[[1L]], ", ", arg_sexps[[2L]], ", ", arg_sexps[[3L]], "));"),
    tccq_abort("r_eval boundary emission currently supports up to 3 arguments")
  )
  lang_line <- sub("HEAD", head, lang_line, fixed = TRUE)

  c(
    arg_lines,
    lang_line,
    "++tccq_nprotect;",
    paste0("SEXP ", val_name, " = PROTECT(Rf_eval(", call_name, ", R_GlobalEnv));"),
    "++tccq_nprotect;",
    if (isTRUE(expect_sexp)) c("UNPROTECT(tccq_nprotect);", paste0("return ", val_name, ";")) else character()
  )
}

tccq_c_emit_copy_local <- function(name, sym, module, prefix = NULL) {
  s <- sym[[name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank < 1L) {
    tccq_abort("copy_local requires a local array: ", name)
  }

  id <- tccq_c_ident(prefix %||% name)
  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  c(
    "{",
    paste0("  SEXP copy_", id, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(s$type$mode), ", ", s$len, "));"),
    "  ++tccq_nprotect;",
    if (s$type$rank == 2L) tccq_indent(tccq_c_emit_matrix_dim_attrib(paste0("copy_", id), s$nrow, s$ncol, paste0("copy_", id)), 2L) else character(),
    paste0("  ", ctype, " *tmp_", id, " = ", tccq_c_rw_accessor(s$type$mode), "(copy_", id, ");"),
    paste0("  for (R_xlen_t i = 0; i < ", s$len, "; ++i) tmp_", id, "[i] = ", s$ptr, "[i];"),
    paste0("  ", s$sexp, " = copy_", id, ";"),
    paste0("  ", s$ptr, " = tmp_", id, ";"),
    paste0("  ", tccq_c_owned_flag(s), " = 1;"),
    "}"
  )
}

tccq_c_emit_materialize_local <- function(name, sym, module) {
  s <- sym[[name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank < 1L) {
    tccq_abort("materialize_local requires a local array: ", name)
  }

  own_flag <- tccq_c_owned_flag(s)
  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  c(
    paste0("if (!", own_flag, ") {"),
    paste0("  ", s$sexp, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(s$type$mode), ", ", s$len, "));"),
    "  ++tccq_nprotect;",
    if (s$type$rank == 2L) tccq_indent(tccq_c_emit_matrix_dim_attrib(s$sexp, s$nrow, s$ncol, tccq_c_ident(name)), 2L) else character(),
    paste0("  ", ctype, " *tmp_", tccq_c_ident(name), " = ", tccq_c_rw_accessor(s$type$mode), "(", s$sexp, ");"),
    paste0("  for (R_xlen_t i = 0; i < ", s$len, "; ++i) tmp_", tccq_c_ident(name), "[i] = ", s$ptr, "[i];"),
    paste0("  ", s$ptr, " = tmp_", tccq_c_ident(name), ";"),
    paste0("  ", own_flag, " = 1;"),
    "}"
  )
}

tccq_c_emit_kernel <- function(module, sym) {
  kernel <- module$kernel
  c(
    "int tccq_nprotect = 0;",
    switch(
      kernel$tag,
      scalar_kernel = tccq_c_emit_kernel_scalar(kernel, sym, module),
      materialize = tccq_c_emit_kernel_materialize(kernel, sym, module),
      fold = tccq_c_emit_kernel_fold(kernel, sym, module),
      kernel_program = tccq_c_emit_kernel_program(kernel, sym, module),
      tccq_abort("unknown kernel tag: ", kernel$tag)
    )
  )
}

tccq_c_emit_kernel_scalar <- function(kernel, sym, module) {
  if (identical(kernel$expr$tag, "boundary_call")) {
    return(tccq_c_emit_boundary_call(kernel$expr, sym, module))
  }

  mode <- kernel$type$mode
  out_sexp <- tccq_sexptype_for_mode(mode)

  if (identical(kernel$expr$tag, "arg_reduce")) {
    return(c(
      tccq_c_emit_arg_reduce_scalar_lines(kernel$expr, sym, module, "res_arg_reduce"),
      paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", 1));"),
      "++tccq_nprotect;",
      tccq_c_emit_assign_scalar("out", mode, "0", "res_arg_reduce"),
      "UNPROTECT(tccq_nprotect);",
      "return out;"
    ))
  }

  if (identical(kernel$expr$tag, "len")) {
    return(c(
      tccq_c_emit_len_setup(kernel$expr, sym, module, "n_len"),
      "if (n_len <= (R_xlen_t)INT_MAX) {",
      "  SEXP out = PROTECT(Rf_allocVector(INTSXP, 1));",
      "  ++tccq_nprotect;",
      "  INTEGER(out)[0] = (int)n_len;",
      "  UNPROTECT(tccq_nprotect);",
      "  return out;",
      "}",
      "SEXP out = PROTECT(Rf_allocVector(REALSXP, 1));",
      "++tccq_nprotect;",
      "REAL(out)[0] = (double)n_len;",
      "UNPROTECT(tccq_nprotect);",
      "return out;"
    ))
  }

  if (identical(kernel$expr$tag, "index") && !is.null(tccq_ir_normalized_access(kernel$expr))) {
    expr <- tccq_c_emit_access_elem(kernel$expr, sym, prefix = "scalar_idx", idx = NULL)
    return(c(
      tccq_c_emit_access_setup(kernel$expr, sym, prefix = "scalar_idx"),
      paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", 1));"),
      "++tccq_nprotect;",
      tccq_c_emit_assign_scalar("out", mode, "0", expr),
      "UNPROTECT(tccq_nprotect);",
      "return out;"
    ))
  }

  expr <- tccq_c_emit_expr(kernel$expr, sym, idx = NULL)

  if (identical(mode, "xlen")) {
    return(c(
      tccq_c_scalar_sexp_alloc("xlen", "out", expr),
      "UNPROTECT(tccq_nprotect);",
      "return out;"
    ))
  }

  c(
    paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", 1));"),
    "++tccq_nprotect;",
    tccq_c_emit_assign_scalar("out", mode, "0", expr),
    "UNPROTECT(tccq_nprotect);",
    "return out;"
  )
}

tccq_c_result_reuse_plan <- function(module) {
  plan <- module$alloc_plan$reuse$result_buffer %||% NULL
  if (is.null(plan) || !identical(plan$strategy, "reuse_owned_local_result")) {
    return(NULL)
  }
  plan
}

tccq_c_emit_reuse_result_buffer <- function(expr, sym, module, reuse) {
  # The allocation plan only selects pointwise, non-boundary expressions here.
  # That invariant keeps the writeback loop allocation-free while it borrows the
  # reused buffer's protected SEXP/data pointer.
  s <- sym[[reuse$name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank != 1L) {
    tccq_abort("invalid result reuse local: ", reuse$name %||% "<NULL>")
  }

  mode <- expr$type$mode
  ctype <- tccq_c_scalar_type_for_mode(mode)
  elem <- tccq_c_emit_expr(expr, sym, idx = "i")

  c(
    tccq_c_emit_common_length_named(expr, sym, "n_out", module = module),
    paste0("if (n_out != ", s$len, ") {"),
    "  Rf_error(\"reused output length mismatch\");",
    "}",
    paste0("SEXP out = ", s$sexp, ";"),
    paste0(ctype, " *p_out = ", s$ptr, ";"),
    "for (R_xlen_t i = 0; i < n_out; ++i) {",
    paste0("  p_out[i] = (", ctype, ")(", elem, ");"),
    "}",
    "UNPROTECT(tccq_nprotect);",
    "return out;"
  )
}

tccq_c_emit_kernel_materialize <- function(kernel, sym, module) {
  mode <- kernel$type$mode
  out_sexp <- tccq_sexptype_for_mode(mode)
  producer <- kernel$producer
  expr <- producer$elem

  if (identical(expr$tag, "boundary_call")) {
    return(tccq_c_emit_boundary_call(expr, sym, module))
  }

  if (identical(expr$tag, "var") && expr$type$rank > 0L) {
    s <- sym[[expr$name]]
    if (!is.null(s) && identical(s$kind, "local")) {
      if (identical(tccq_c_storage_kind(module, expr$name), "owned")) {
        return(c(
          "UNPROTECT(tccq_nprotect);",
          paste0("return ", s$sexp, ";")
        ))
      }
      return(c(
        tccq_c_emit_materialize_local(expr$name, sym, module),
        "UNPROTECT(tccq_nprotect);",
        paste0("return ", s$sexp, ";")
      ))
    }
  }

  if (expr$tag %in% c("slice_range", "view1")) {
    return(tccq_c_emit_materialize_slice_range(expr, sym, out_name = "out", out_ptr = "p_out"))
  }

  reuse <- tccq_c_result_reuse_plan(module)
  if (!is.null(reuse)) {
    return(tccq_c_emit_reuse_result_buffer(expr, sym, module, reuse))
  }

  elem <- tccq_c_emit_expr(expr, sym, idx = "i")
  rank2 <- !is.null(expr$type) && expr$type$rank == 2L
  length_setup <- if (isTRUE(rank2)) {
    tccq_c_emit_matrix_length_setup(expr, sym, "n_out", "nr_out", "nc_out")
  } else {
    tccq_c_emit_common_length_named(expr, sym, "n_out", module = module)
  }
  dim_setup <- if (isTRUE(rank2)) tccq_c_emit_matrix_dim_attrib("out", "nr_out", "nc_out", "out") else character()

  c(
    length_setup,
    paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", n_out));"),
    "++tccq_nprotect;",
    dim_setup,
    paste0(tccq_c_scalar_type_for_mode(mode), " *p_out = ", tccq_c_rw_accessor(mode), "(out);"),
    "for (R_xlen_t i = 0; i < n_out; ++i) {",
    paste0("  p_out[i] = (", tccq_c_scalar_type_for_mode(mode), ")(", elem, ");"),
    "}",
    "UNPROTECT(tccq_nprotect);",
    "return out;"
  )
}

tccq_c_emit_kernel_fold <- function(kernel, sym, module) {
  producer <- if (identical(kernel$elem$tag, "materialize")) kernel$elem$producer else kernel$elem
  expr <- producer$elem
  tccq_reducer_spec_get(kernel$op)

  if (expr$tag %in% c("slice_range", "view1")) {
    setup <- tccq_c_emit_slice_range_indices(expr, sym, prefix = "fold")
    elem <- tccq_c_emit_slice_range_elem(expr, sym, idx = "i", prefix = "fold")
    loop_len <- "n_fold"
  } else {
    setup <- tccq_c_emit_common_length_named(expr, sym, "n_out", module = module)
    elem <- tccq_c_emit_expr(expr, sym, idx = "i")
    loop_len <- "n_out"
  }

  c(
    setup,
    if (identical(kernel$surface %||% kernel$op, "Reduce")) c(
      paste0("if (", loop_len, " == 0) {"),
      "  UNPROTECT(tccq_nprotect);",
      "  return R_NilValue;",
      "}"
    ),
    tccq_c_emit_reducer_fold_lines(kernel$op, result_type = kernel$type, input_type = expr$type, elem = elem, loop_len = loop_len),
    "UNPROTECT(tccq_nprotect);",
    "return out;"
  )
}

tccq_c_emit_missing_check <- function(expr, type) {
  switch(
    type$mode,
    double = paste0("ISNAN((double)(", expr, "))"),
    integer = paste0("((int)(", expr, ") == NA_INTEGER)"),
    logical = paste0("((int)(", expr, ") == NA_LOGICAL)"),
    raw = "0",
    xlen = "0",
    tccq_abort("unsupported missing-value check mode: ", type$mode)
  )
}

tccq_c_emit_na_value_for_mode <- function(mode) {
  switch(
    mode,
    double = "NA_REAL",
    integer = "NA_INTEGER",
    logical = "NA_LOGICAL",
    raw = "(Rbyte)0",
    tccq_abort("unsupported NA value mode: ", mode)
  )
}

tccq_c_emit_reducer_fold_lines <- function(op, result_type, input_type, elem, loop_len) {
  missing_check <- tccq_c_emit_missing_check(elem, input_type)

  if (op %in% c("sum", "prod") && identical(input_type$mode, "double")) {
    return(switch(
      op,
      sum = c(
        "double acc = 0.0;",
        paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
        paste0("  double v = (double)(", elem, ");"),
        "  if (R_IsNA(v)) { acc = NA_REAL; break; }",
        "  if (R_IsNaN(v)) { acc = R_NaN; break; }",
        "  acc += v;",
        "}",
        tccq_c_scalar_sexp_alloc("double", "out", "acc")
      ),
      prod = c(
        "double acc = 1.0;",
        paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
        paste0("  double v = (double)(", elem, ");"),
        "  if (R_IsNA(v)) { acc = NA_REAL; break; }",
        "  if (R_IsNaN(v)) { acc = R_NaN; break; }",
        "  acc *= v;",
        "}",
        tccq_c_scalar_sexp_alloc("double", "out", "acc")
      )
    ))
  }

  if (op %in% c("min", "max", "mean") && identical(input_type$mode, "double")) {
    return(switch(
      op,
      min = c(
        "double acc = R_PosInf;",
        "int seen_nan = 0;",
        "int seen_na = 0;",
        paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
        paste0("  double v = (double)(", elem, ");"),
        "  if (R_IsNA(v)) { seen_na = 1; continue; }",
        "  if (R_IsNaN(v)) { seen_nan = 1; continue; }",
        "  if (v < acc) acc = v;",
        "}",
        "double min_res = seen_na ? NA_REAL : (seen_nan ? R_NaN : acc);",
        tccq_c_scalar_sexp_alloc("double", "out", "min_res")
      ),
      max = c(
        "double acc = R_NegInf;",
        "int seen_nan = 0;",
        "int seen_na = 0;",
        paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
        paste0("  double v = (double)(", elem, ");"),
        "  if (R_IsNA(v)) { seen_na = 1; continue; }",
        "  if (R_IsNaN(v)) { seen_nan = 1; continue; }",
        "  if (v > acc) acc = v;",
        "}",
        "double max_res = seen_na ? NA_REAL : (seen_nan ? R_NaN : acc);",
        tccq_c_scalar_sexp_alloc("double", "out", "max_res")
      ),
      mean = c(
        "double acc = 0.0;",
        "int seen_nan = 0;",
        "int seen_na = 0;",
        paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
        paste0("  double v = (double)(", elem, ");"),
        "  if (R_IsNA(v)) { seen_na = 1; continue; }",
        "  if (R_IsNaN(v)) { seen_nan = 1; continue; }",
        "  acc += v;",
        "}",
        paste0("double mean_res = seen_na ? NA_REAL : (seen_nan ? R_NaN : (", loop_len, " == 0 ? R_NaN : (acc / (double)", loop_len, ")));"),
        tccq_c_scalar_sexp_alloc("double", "out", "mean_res")
      )
    ))
  }

  switch(
    op,
    sum = c(
      "double acc = 0.0;",
      paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
      paste0("  if (", missing_check, ") { acc = NA_REAL; break; }"),
      paste0("  acc += (double)(", elem, ");"),
      "}",
      tccq_c_scalar_sexp_alloc("double", "out", "acc")
    ),
    prod = c(
      "double acc = 1.0;",
      paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
      paste0("  if (", missing_check, ") { acc = NA_REAL; break; }"),
      paste0("  acc *= (double)(", elem, ");"),
      "}",
      tccq_c_scalar_sexp_alloc("double", "out", "acc")
    ),
    min = c(
      "double acc = R_PosInf;",
      paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
      paste0("  if (", missing_check, ") { acc = NA_REAL; break; }"),
      paste0("  double v = (double)(", elem, ");"),
      "  if (v < acc) acc = v;",
      "}",
      tccq_c_scalar_sexp_alloc("double", "out", "acc")
    ),
    max = c(
      "double acc = R_NegInf;",
      paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
      paste0("  if (", missing_check, ") { acc = NA_REAL; break; }"),
      paste0("  double v = (double)(", elem, ");"),
      "  if (v > acc) acc = v;",
      "}",
      tccq_c_scalar_sexp_alloc("double", "out", "acc")
    ),
    mean = c(
      "double acc = 0.0;",
      paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
      paste0("  if (", missing_check, ") { acc = NA_REAL; break; }"),
      paste0("  acc += (double)(", elem, ");"),
      "}",
      paste0("double mean_res = ISNAN(acc) ? acc : (", loop_len, " == 0 ? R_NaN : (acc / (double)", loop_len, "));"),
      tccq_c_scalar_sexp_alloc("double", "out", "mean_res")
    ),
    any = c(
      "int acc = 0;",
      "int seen_na = 0;",
      paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
      paste0("  int v = (int)(", elem, ");"),
      "  if (v == NA_LOGICAL) { seen_na = 1; } else if (v) { acc = 1; break; }",
      "}",
      "int any_res = acc ? 1 : (seen_na ? NA_LOGICAL : 0);",
      tccq_c_scalar_sexp_alloc("logical", "out", "any_res")
    ),
    all = c(
      "int acc = 1;",
      "int seen_na = 0;",
      paste0("for (R_xlen_t i = 0; i < ", loop_len, "; ++i) {"),
      paste0("  int v = (int)(", elem, ");"),
      "  if (v == NA_LOGICAL) { seen_na = 1; } else if (!v) { acc = 0; break; }",
      "}",
      "int all_res = (!acc) ? 0 : (seen_na ? NA_LOGICAL : 1);",
      tccq_c_scalar_sexp_alloc("logical", "out", "all_res")
    ),
    tccq_abort("unsupported reducer in C fold emission: ", op)
  )
}

tccq_c_emit_kernel_program <- function(kernel, sym, module) {
  stmt_lines <- unlist(
    Map(
      function(stmt, idx) tccq_c_emit_stmt(stmt, sym = sym, module = module, stmt_index = idx),
      kernel$stmts,
      seq_along(kernel$stmts)
    ),
    use.names = FALSE
  )

  c(
    stmt_lines,
    switch(
      kernel$result_kernel$tag,
      scalar_kernel = tccq_c_emit_kernel_scalar(kernel$result_kernel, sym, module),
      materialize = tccq_c_emit_kernel_materialize(kernel$result_kernel, sym, module),
      fold = tccq_c_emit_kernel_fold(kernel$result_kernel, sym, module),
      tccq_abort("unsupported program result kernel: ", kernel$result_kernel$tag)
    )
  )
}

tccq_c_bind_reuse_plan <- function(module, name) {
  plan <- module$alloc_plan$reuse$bindings[[name]] %||% NULL
  if (is.null(plan) || !identical(plan$strategy, "reuse_owned_local_bind")) {
    return(NULL)
  }
  plan
}

tccq_c_emit_matrix_dim_attrib <- function(sexp, nr, nc, prefix) {
  c(
    paste0("if (", nr, " > INT_MAX || ", nc, " > INT_MAX) {"),
    "  Rf_error(\"matrix dimensions exceed R integer dim limits\");",
    "}",
    paste0("SEXP dim_", prefix, " = PROTECT(Rf_allocVector(INTSXP, 2));"),
    "++tccq_nprotect;",
    paste0("INTEGER(dim_", prefix, ")[0] = (int)", nr, ";"),
    paste0("INTEGER(dim_", prefix, ")[1] = (int)", nc, ";"),
    paste0("Rf_setAttrib(", sexp, ", R_DimSymbol, dim_", prefix, ");")
  )
}

tccq_c_emit_matrix_fill_bind <- function(stmt, sym, module) {
  s <- sym[[stmt$name]]
  mode <- stmt$type$mode
  ctype <- tccq_c_scalar_type_for_mode(mode)
  prefix <- tccq_c_ident(stmt$name)
  value <- tccq_c_emit_expr(stmt$value$value, sym, idx = NULL)
  nr <- tccq_c_emit_expr(stmt$value$nrow, sym, idx = NULL)
  nc <- tccq_c_emit_expr(stmt$value$ncol, sym, idx = NULL)

  c(
    paste0("R_xlen_t ", s$nrow, " = (R_xlen_t)(", nr, ");"),
    paste0("R_xlen_t ", s$ncol, " = (R_xlen_t)(", nc, ");"),
    paste0("if (", s$nrow, " < 0 || ", s$ncol, " < 0) {"),
    "  Rf_error(\"matrix dimensions must be non-negative\");",
    "}",
    paste0("if (", s$ncol, " != 0 && ", s$nrow, " > R_XLEN_T_MAX / ", s$ncol, ") {"),
    "  Rf_error(\"matrix dimensions exceed R length limits\");",
    "}",
    paste0("R_xlen_t ", s$len, " = ", s$nrow, " * ", s$ncol, ";"),
    paste0("SEXP ", s$sexp, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(mode), ", ", s$len, "));"),
    "++tccq_nprotect;",
    tccq_c_emit_matrix_dim_attrib(s$sexp, s$nrow, s$ncol, prefix),
    paste0(ctype, " *", s$ptr, " = ", tccq_c_rw_accessor(mode), "(", s$sexp, ");"),
    paste0("int ", tccq_c_owned_flag(s), " = 1;"),
    paste0(ctype, " fill_", prefix, " = (", ctype, ")(", value, ");"),
    paste0("for (R_xlen_t i = 0; i < ", s$len, "; ++i) {"),
    paste0("  ", s$ptr, "[i] = fill_", prefix, ";"),
    "}"
  )
}

tccq_c_emit_reuse_bind_buffer <- function(stmt, sym, module, reuse) {
  # Bind reuse is also restricted to pointwise, non-boundary expressions so this
  # loop does not allocate while writing through the source local's buffer.
  s <- sym[[stmt$name]]
  source <- sym[[reuse$source]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank != 1L) {
    tccq_abort("invalid bind reuse target: ", stmt$name %||% "<NULL>")
  }
  if (is.null(source) || !identical(source$kind, "local") || source$type$rank != 1L) {
    tccq_abort("invalid bind reuse source: ", reuse$source %||% "<NULL>")
  }

  ctype <- tccq_c_scalar_type_for_mode(stmt$type$mode)
  elem <- tccq_c_emit_expr(stmt$value, sym, idx = "i")

  c(
    tccq_c_emit_common_length_named(stmt$value, sym, s$len, module = module),
    paste0("if (", s$len, " != ", source$len, ") {"),
    "  Rf_error(\"reused local length mismatch\");",
    "}",
    paste0("SEXP ", s$sexp, " = ", source$sexp, ";"),
    paste0(ctype, " *", s$ptr, " = ", source$ptr, ";"),
    "/* Reused bind target intentionally aliases the source owned buffer. */",
    paste0("int ", tccq_c_owned_flag(s), " = 1;"),
    paste0("for (R_xlen_t i = 0; i < ", s$len, "; ++i) {"),
    paste0("  ", s$ptr, "[i] = (", ctype, ")(", elem, ");"),
    "}"
  )
}

tccq_c_emit_stmt <- function(stmt, sym, module, stmt_index = NULL) {
  switch(
    stmt$tag,
    bind = tccq_c_emit_stmt_bind(stmt, sym, module),
    store_index = tccq_c_emit_stmt_store_index(stmt, sym, module, stmt_index = stmt_index),
    store_range = tccq_c_emit_stmt_store_range(stmt, sym, module, stmt_index = stmt_index),
    store_index2 = tccq_c_emit_stmt_store_index2(stmt, sym, module, stmt_index = stmt_index),
    store_access = tccq_c_emit_stmt_store_access(stmt, sym, module, stmt_index = stmt_index),
    for_loop = tccq_c_emit_stmt_for_loop(stmt, sym, module),
    tccq_abort("unsupported statement tag: ", stmt$tag)
  )
}

tccq_c_emit_stmt_for_loop <- function(stmt, sym, module) {
  s <- sym[[stmt$var]]
  if (is.null(s)) {
    tccq_abort("unknown for-loop variable: ", stmt$var)
  }
  id <- tccq_c_ident(stmt$var)
  start <- tccq_c_emit_expr(stmt$start, sym, idx = NULL)
  stop <- tccq_c_emit_expr(stmt$stop, sym, idx = NULL)
  start_missing <- tccq_c_emit_missing_check(start, stmt$start$type)
  stop_missing <- tccq_c_emit_missing_check(stop, stmt$stop$type)
  body <- unlist(lapply(stmt$body %||% list(), tccq_c_emit_stmt, sym = sym, module = module, stmt_index = NULL), use.names = FALSE)
  c(
    "{",
    paste0("  R_xlen_t ", s$len, " = 1;"),
    paste0("  if (", start_missing, " || ", stop_missing, ") { Rf_error(\"for-loop bounds must not be missing\"); }"),
    paste0("  R_xlen_t start_", id, " = (R_xlen_t)(", start, ");"),
    paste0("  R_xlen_t stop_", id, " = (R_xlen_t)(", stop, ");"),
    paste0("  R_xlen_t by_", id, " = start_", id, " <= stop_", id, " ? 1 : -1;"),
    paste0("  R_xlen_t ", s$val, " = start_", id, ";"),
    paste0("  for (;;) {"),
    paste0("    if ((by_", id, " > 0 && ", s$val, " > stop_", id, ") || (by_", id, " < 0 && ", s$val, " < stop_", id, ")) break;"),
    tccq_indent(body, 4L),
    paste0("    if (", s$val, " == stop_", id, ") break;"),
    paste0("    ", s$val, " += by_", id, ";"),
    "  }",
    "}"
  )
}

tccq_c_emit_len_setup <- function(node, sym, module, len_name) {
  if (!identical(node$tag, "len")) {
    tccq_abort("expected length node for length setup")
  }

  x <- node$x
  if (is.null(x$type) || x$type$rank == 0L) {
    return(tccq_c_emit_scalar_length_setup(x, sym, module, len_name))
  }

  if (identical(x$tag, "var")) {
    s <- sym[[x$name]]
    if (is.null(s)) {
      tccq_abort("unknown C symbol for length(): ", x$name)
    }
    return(paste0("R_xlen_t ", len_name, " = ", s$len, ";"))
  }

  tccq_c_emit_common_length_named(x, sym, len_name, module = module)
}

tccq_c_emit_reducer_scalar_lines <- function(node, sym, module, target) {
  if (!identical(node$tag, "reduce")) {
    tccq_abort("expected reduce node for scalar reducer emission")
  }
  x <- node$x
  len_name <- paste0("n_", tccq_c_ident(target), "_reduce")
  elem <- tccq_c_emit_expr(x, sym, idx = "i")
  missing <- tccq_c_emit_missing_check(elem, x$type)
  ctype <- tccq_c_scalar_type_for_mode(node$type$mode)

  inner <- switch(
    node$op,
    sum = c(
      tccq_c_emit_common_length_named(x, sym, len_name, module = module),
      "double acc = 0.0;",
      paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
      paste0("  if (", missing, ") { acc = NA_REAL; break; }"),
      paste0("  acc += (double)(", elem, ");"),
      "}",
      paste0(target, " = (", ctype, ")acc;")
    ),
    prod = c(
      tccq_c_emit_common_length_named(x, sym, len_name, module = module),
      "double acc = 1.0;",
      paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
      paste0("  if (", missing, ") { acc = NA_REAL; break; }"),
      paste0("  acc *= (double)(", elem, ");"),
      "}",
      paste0(target, " = (", ctype, ")acc;")
    ),
    min = c(
      tccq_c_emit_common_length_named(x, sym, len_name, module = module),
      "double acc = R_PosInf;",
      paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
      paste0("  if (", missing, ") { acc = NA_REAL; break; }"),
      paste0("  double v = (double)(", elem, ");"),
      "  if (v < acc) acc = v;",
      "}",
      paste0(target, " = (", ctype, ")acc;")
    ),
    max = c(
      tccq_c_emit_common_length_named(x, sym, len_name, module = module),
      "double acc = R_NegInf;",
      paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
      paste0("  if (", missing, ") { acc = NA_REAL; break; }"),
      paste0("  double v = (double)(", elem, ");"),
      "  if (v > acc) acc = v;",
      "}",
      paste0(target, " = (", ctype, ")acc;")
    ),
    mean = c(
      tccq_c_emit_common_length_named(x, sym, len_name, module = module),
      "double acc = 0.0;",
      paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
      paste0("  if (", missing, ") { acc = NA_REAL; break; }"),
      paste0("  acc += (double)(", elem, ");"),
      "}",
      paste0("double mean_res = ISNAN(acc) ? acc : (", len_name, " == 0 ? R_NaN : (acc / (double)", len_name, "));"),
      paste0(target, " = (", ctype, ")mean_res;")
    ),
    any = c(
      tccq_c_emit_common_length_named(x, sym, len_name, module = module),
      "int acc = 0;",
      "int seen_na = 0;",
      paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
      paste0("  int v = (int)(", elem, ");"),
      "  if (v == NA_LOGICAL) { seen_na = 1; } else if (v) { acc = 1; break; }",
      "}",
      paste0(target, " = (acc ? 1 : (seen_na ? NA_LOGICAL : 0));")
    ),
    all = c(
      tccq_c_emit_common_length_named(x, sym, len_name, module = module),
      "int acc = 1;",
      "int seen_na = 0;",
      paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
      paste0("  int v = (int)(", elem, ");"),
      "  if (v == NA_LOGICAL) { seen_na = 1; } else if (!v) { acc = 0; break; }",
      "}",
      paste0(target, " = ((!acc) ? 0 : (seen_na ? NA_LOGICAL : 1));")
    ),
    tccq_abort("unsupported scalar reducer emission: ", node$op)
  )

  c(
    paste0(ctype, " ", target, ";"),
    "{",
    tccq_indent(inner, 2L),
    "}"
  )
}

tccq_c_emit_arg_reduce_scalar_lines <- function(node, sym, module, target) {
  if (!identical(node$tag, "arg_reduce") || !identical(node$op, "which.max")) {
    tccq_abort("unsupported arg reducer emission")
  }
  x <- node$x
  len_name <- paste0("n_", tccq_c_ident(target), "_arg_reduce")
  elem <- tccq_c_emit_expr(x, sym, idx = "i")
  missing <- tccq_c_emit_missing_check(elem, x$type)
  inner <- c(
    tccq_c_emit_common_length_named(x, sym, len_name, module = module),
    "R_xlen_t best = 0;",
    "int seen = 0;",
    "double best_val = R_NegInf;",
    paste0("for (R_xlen_t i = 0; i < ", len_name, "; ++i) {"),
    paste0("  if (", missing, ") continue;"),
    paste0("  double v = (double)(", elem, ");"),
    "  if (!seen || v > best_val) { best_val = v; best = i + 1; seen = 1; }",
    "}",
    paste0(target, " = (!seen || best > (R_xlen_t)INT_MAX) ? NA_INTEGER : (int)best;")
  )

  c(
    paste0("int ", target, ";"),
    "{",
    tccq_indent(inner, 2L),
    "}"
  )
}

tccq_c_emit_stmt_bind <- function(stmt, sym, module) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local")) {
    tccq_abort("bind target must be a local symbol: ", stmt$name)
  }

  type <- stmt$type
  scalar_value_vars <- tccq_c_module_scalar_value_vars(module)

  if (type$rank == 0L) {
    if (identical(stmt$value$tag, "reduce")) {
      return(c(
        tccq_c_emit_reducer_scalar_lines(stmt$value, sym, module, s$val),
        paste0("R_xlen_t ", s$len, " = 1;"),
        tccq_c_emit_scalar_value_length_check(s, scalar_value_vars, stmt$name)
      ))
    }

    if (identical(stmt$value$tag, "arg_reduce")) {
      return(c(
        tccq_c_emit_arg_reduce_scalar_lines(stmt$value, sym, module, s$val),
        paste0("R_xlen_t ", s$len, " = 1;"),
        tccq_c_emit_scalar_value_length_check(s, scalar_value_vars, stmt$name)
      ))
    }

    if (identical(stmt$value$tag, "len")) {
      len_name <- paste0(s$val, "_len")
      return(c(
        tccq_c_emit_len_setup(stmt$value, sym, module, len_name),
        paste0("R_xlen_t ", s$len, " = 1;"),
        tccq_c_emit_scalar_value_length_check(s, scalar_value_vars, stmt$name),
        paste0(
          tccq_c_scalar_type_for_mode(type$mode), " ", s$val,
          " = (", tccq_c_scalar_type_for_mode(type$mode), ")(", len_name, ");"
        )
      ))
    }

    if (identical(stmt$value$tag, "boundary_call")) {
      prefix <- tccq_c_ident(s$val)
      val_name <- paste0("val_", prefix)
      common <- c(
        tccq_c_emit_boundary_call(stmt$value, sym, module, expect_sexp = FALSE, prefix = prefix),
        paste0("R_xlen_t ", s$len, " = XLENGTH(", val_name, ");"),
        paste0("if (", s$len, " < 1) {"),
        "  Rf_error(\"boundary scalar result is empty\");",
        "}",
        tccq_c_emit_scalar_value_length_check(s, scalar_value_vars, stmt$name)
      )
      if (identical(type$mode, "xlen")) {
        return(c(
          common,
          paste0("R_xlen_t ", s$val, ";"),
          paste0("if (TYPEOF(", val_name, ") == INTSXP) {"),
          paste0("  int tmp_", prefix, " = INTEGER(", val_name, ")[0];"),
          paste0("  if (tmp_", prefix, " == NA_INTEGER) Rf_error(\"boundary xlen scalar result is NA\");"),
          paste0("  ", s$val, " = (R_xlen_t)tmp_", prefix, ";"),
          paste0("} else if (TYPEOF(", val_name, ") == REALSXP) {"),
          paste0("  double tmp_", prefix, " = REAL(", val_name, ")[0];"),
          paste0("  if (!R_FINITE(tmp_", prefix, ") || floor(tmp_", prefix, ") != tmp_", prefix, " || tmp_", prefix, " < -(double)R_XLEN_T_MAX || tmp_", prefix, " > (double)R_XLEN_T_MAX) {"),
          "    Rf_error(\"boundary xlen scalar result is not an integer-like finite length\");",
          "  }",
          paste0("  ", s$val, " = (R_xlen_t)tmp_", prefix, ";"),
          "} else {",
          "  Rf_error(\"boundary xlen scalar result has wrong R type\");",
          "}"
        ))
      }
      return(c(
        common,
        paste0("if (TYPEOF(", val_name, ") != ", tccq_sexptype_for_mode(type$mode), ") {"),
        "  Rf_error(\"boundary scalar result has wrong R type\");",
        "}",
        paste0(
          tccq_c_scalar_type_for_mode(type$mode), " ", s$val,
          " = ", tccq_c_ro_accessor(type$mode), "(", val_name, ")[0];"
        )
      ))
    }

    if (identical(stmt$value$tag, "index") && !is.null(tccq_ir_normalized_access(stmt$value))) {
      expr <- tccq_c_emit_access_elem(stmt$value, sym, prefix = s$val, idx = NULL)
      return(c(
        tccq_c_emit_access_setup(stmt$value, sym, prefix = s$val),
        paste0("R_xlen_t ", s$len, " = 1;"),
        tccq_c_emit_scalar_value_length_check(s, scalar_value_vars, stmt$name),
        paste0(
          tccq_c_scalar_type_for_mode(type$mode), " ", s$val,
          " = (", tccq_c_scalar_type_for_mode(type$mode), ")(", expr, ");"
        )
      ))
    }

    expr <- tccq_c_emit_expr(stmt$value, sym, idx = NULL)
    return(c(
      tccq_c_emit_scalar_length_setup(stmt$value, sym, module, s$len),
      tccq_c_emit_scalar_value_length_check(s, scalar_value_vars, stmt$name),
      paste0(
        tccq_c_scalar_type_for_mode(type$mode), " ", s$val,
        " = (", tccq_c_scalar_type_for_mode(type$mode), ")(", expr, ");"
      )
    ))
  }

  storage_kind <- tccq_c_storage_kind(module, stmt$name)
  own_flag <- tccq_c_owned_flag(s)
  ctype <- tccq_c_scalar_type_for_mode(type$mode)

  if (type$rank == 2L) {
    if (identical(stmt$value$tag, "matrix_fill")) {
      return(tccq_c_emit_matrix_fill_bind(stmt, sym, module))
    }
    if (identical(storage_kind, "alias") && identical(stmt$value$tag, "var")) {
      base <- sym[[stmt$value$name]]
      ptr_init <- if (stmt$name %in% tccq_c_module_data_vars(module)) {
        paste0("(", ctype, " *) ", base$ptr)
      } else {
        "NULL"
      }
      return(c(
        paste0("R_xlen_t ", s$len, " = ", base$len, ";"),
        paste0("R_xlen_t ", s$nrow, " = ", base$nrow, ";"),
        paste0("R_xlen_t ", s$ncol, " = ", base$ncol, ";"),
        paste0("SEXP ", s$sexp, " = R_NilValue;"),
        paste0(ctype, " *", s$ptr, " = ", ptr_init, ";"),
        paste0("int ", own_flag, " = 0;")
      ))
    }
    if (identical(storage_kind, "owned")) {
      elem <- tccq_c_emit_expr(stmt$value, sym, idx = "i")
      return(c(
        tccq_c_emit_matrix_length_setup(stmt$value, sym, s$len, s$nrow, s$ncol),
        paste0("SEXP ", s$sexp, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(type$mode), ", ", s$len, "));"),
        "++tccq_nprotect;",
        tccq_c_emit_matrix_dim_attrib(s$sexp, s$nrow, s$ncol, tccq_c_ident(stmt$name)),
        paste0(ctype, " *", s$ptr, " = ", tccq_c_rw_accessor(type$mode), "(", s$sexp, ");"),
        paste0("int ", own_flag, " = 1;"),
        paste0("for (R_xlen_t i = 0; i < ", s$len, "; ++i) {"),
        paste0("  ", s$ptr, "[i] = (", ctype, ")(", elem, ");"),
        "}"
      ))
    }
    tccq_abort("local matrix bind currently supports matrix(), direct aliases, or owned pointwise matrix expressions only")
  }

  if (type$rank != 1L) {
    tccq_abort("local bind currently supports scalar/vector/matrix only")
  }

  reuse <- tccq_c_bind_reuse_plan(module, stmt$name)
  if (!is.null(reuse)) {
    return(tccq_c_emit_reuse_bind_buffer(stmt, sym, module, reuse))
  }

  if (identical(storage_kind, "alias") && identical(stmt$value$tag, "var")) {
    base <- sym[[stmt$value$name]]
    ptr_init <- if (stmt$name %in% tccq_c_module_data_vars(module)) {
      paste0("(", ctype, " *) ", base$ptr)
    } else {
      "NULL"
    }
    return(c(
      paste0("R_xlen_t ", s$len, " = ", base$len, ";"),
      paste0("SEXP ", s$sexp, " = R_NilValue;"),
      paste0(ctype, " *", s$ptr, " = ", ptr_init, ";"),
      paste0("int ", own_flag, " = 0;")
    ))
  }

  if (stmt$value$tag %in% c("slice_range", "view1")) {
    return(tccq_c_emit_bind_slice_range(stmt, sym, module))
  }

  len_lines <- tccq_c_emit_common_length_named(stmt$value, sym, s$len, module = module)
  elem <- tccq_c_emit_expr(stmt$value, sym, idx = "i")

  c(
    len_lines,
    paste0("SEXP ", s$sexp, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(type$mode), ", ", s$len, "));"),
    "++tccq_nprotect;",
    paste0(ctype, " *", s$ptr, " = ", tccq_c_rw_accessor(type$mode), "(", s$sexp, ");"),
    paste0("int ", own_flag, " = 1;"),
    paste0("for (R_xlen_t i = 0; i < ", s$len, "; ++i) {"),
    paste0("  ", s$ptr, "[i] = (", ctype, ")(", elem, ");"),
    "}"
  )
}

tccq_c_emit_materialize_before_write <- function(module, sym, stmt_index) {
  if (is.null(stmt_index)) {
    return(character())
  }

  barrier <- module$storage_plan$write_barriers[[as.character(stmt_index)]] %||% NULL
  if (is.null(barrier)) {
    return(character())
  }

  views <- barrier$materialize_views %||% character()
  lines <- unlist(lapply(views, tccq_c_emit_materialize_local, sym = sym, module = module), use.names = FALSE)

  if (isTRUE(barrier$copy_target)) {
    lines <- c(
      lines,
      tccq_c_emit_copy_local(
        barrier$target,
        sym,
        module,
        prefix = paste0(barrier$target, "_escape_", stmt_index)
      )
    )
  }

  lines
}

tccq_c_emit_store_subscript_setup <- function(sub, sym, dim_expr, prefix, label) {
  lo <- paste0("lo_", prefix)
  n <- paste0("n_", prefix)

  if (identical(sub$kind, "index")) {
    index <- tccq_c_emit_expr(sub$index, sym, idx = NULL)
    raw <- paste0("raw_", prefix)
    missing <- tccq_c_emit_missing_check(index, sub$index$type)
    return(list(
      lines = c(
        paste0("R_xlen_t ", lo, " = 0;"),
        paste0("R_xlen_t ", n, " = 0;"),
        paste0("if (!(", missing, ")) {"),
        paste0("  R_xlen_t ", raw, " = (R_xlen_t)(", index, ");"),
        paste0("  ", lo, " = tccq_checked_index1(", raw, ", ", dim_expr, ", ", tccq_c_string(label), ");"),
        paste0("  ", n, " = 1;"),
        "}"
      ),
      lo = lo,
      n = n
    ))
  }

  if (identical(sub$kind, "range")) {
    start <- tccq_c_emit_expr(sub$start, sym, idx = NULL)
    stop <- tccq_c_emit_expr(sub$stop, sym, idx = NULL)
    hi <- paste0("hi_", prefix)
    return(list(
      lines = c(
        paste0("R_xlen_t ", lo, " = tccq_checked_index1((R_xlen_t)(", start, "), ", dim_expr, ", ", tccq_c_string(label), ");"),
        paste0("R_xlen_t ", hi, " = tccq_checked_index1((R_xlen_t)(", stop, "), ", dim_expr, ", ", tccq_c_string(label), ");"),
        paste0("if (", hi, " < ", lo, ") { Rf_error(\"decreasing ranges are not supported in indexed assignment\"); }"),
        paste0("R_xlen_t ", n, " = ", hi, " - ", lo, " + 1;")
      ),
      lo = lo,
      n = n
    ))
  }

  if (identical(sub$kind, "all")) {
    return(list(
      lines = c(
        paste0("R_xlen_t ", lo, " = 0;"),
        paste0("R_xlen_t ", n, " = ", dim_expr, ";")
      ),
      lo = lo,
      n = n
    ))
  }

  tccq_abort("unsupported assignment subscript kind: ", sub$kind)
}

tccq_c_emit_stmt_store_access <- function(stmt, sym, module, stmt_index = NULL) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank < 1L) {
    tccq_abort("indexed assignment target must be a local array-like value: ", stmt$name)
  }
  if (!stmt$value$type$rank %in% c(0L, 1L)) {
    tccq_abort("indexed assignment currently supports scalar or vector RHS values")
  }

  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  id <- tccq_c_ident(stmt$name)
  tmp_id <- paste0(id, if (!is.null(stmt_index)) paste0("_", stmt_index) else "")
  rhs_scalar <- identical(as.integer(stmt$value$type$rank), 0L)
  rhs_value <- if (rhs_scalar) tccq_c_emit_expr(stmt$value, sym, idx = NULL) else NULL
  rhs_setup <- if (rhs_scalar) character() else tccq_c_emit_common_length_named(stmt$value, sym, paste0("n_rhs_", tmp_id), module = module)
  common <- c(
    tccq_c_emit_materialize_before_write(module, sym, stmt_index),
    if (!identical(tccq_c_storage_kind(module, stmt$name), "owned")) tccq_c_emit_materialize_local(stmt$name, sym, module)
  )

  if (s$type$rank == 1L) {
    if (!is.null(stmt$access)) {
      final_kind <- tccq_c_access_final_kind(stmt$access)
      if (!final_kind %in% c("index", "slice")) {
        tccq_abort("nested vector assignment requires a final index or slice access")
      }
      prefix <- paste0("store_", tmp_id)
      rhs_elem <- if (rhs_scalar) paste0("rhs_", tmp_id) else tccq_c_emit_expr(stmt$value, sym, idx = "i")
      return(c(
        common,
        tccq_c_emit_access_setup(stmt$access, sym, prefix = prefix),
        rhs_setup,
        if (rhs_scalar) paste0(ctype, " rhs_", tmp_id, " = (", ctype, ")(", rhs_value, ");") else c(
          paste0("if (n_rhs_", tmp_id, " != n_", prefix, ") {"),
          "  Rf_error(\"vector RHS length must match indexed assignment extent\");",
          "}"
        ),
        paste0("for (R_xlen_t i = 0; i < n_", prefix, "; ++i) {"),
        paste0("  ", s$ptr, "[off_", prefix, " + i] = (", ctype, ")(", rhs_elem, ");"),
        "}"
      ))
    }

    if (length(stmt$subscripts) != 1L) {
      tccq_abort("vector assignment expects exactly one subscript")
    }
    setup <- tccq_c_emit_store_subscript_setup(stmt$subscripts[[1L]], sym, s$len, paste0(tmp_id, "_d1"), stmt$name)
    rhs_elem <- if (rhs_scalar) paste0("rhs_", tmp_id) else tccq_c_emit_expr(stmt$value, sym, idx = "i")
    return(c(
      common,
      setup$lines,
      rhs_setup,
      if (rhs_scalar) paste0(ctype, " rhs_", tmp_id, " = (", ctype, ")(", rhs_value, ");") else c(
        paste0("if (n_rhs_", tmp_id, " != ", setup$n, ") {"),
        "  Rf_error(\"vector RHS length must match indexed assignment extent\");",
        "}"
      ),
      paste0("for (R_xlen_t i = 0; i < ", setup$n, "; ++i) {"),
      paste0("  ", s$ptr, "[", setup$lo, " + i] = (", ctype, ")(", rhs_elem, ");"),
      "}"
    ))
  }

  if (s$type$rank == 2L) {
    if (length(stmt$subscripts) != 2L) {
      tccq_abort("matrix assignment expects exactly two subscripts")
    }
    row <- tccq_c_emit_store_subscript_setup(stmt$subscripts[[1L]], sym, s$nrow, paste0(tmp_id, "_row"), stmt$name)
    col <- tccq_c_emit_store_subscript_setup(stmt$subscripts[[2L]], sym, s$ncol, paste0(tmp_id, "_col"), stmt$name)
    rhs_elem <- if (rhs_scalar) paste0("rhs_", tmp_id) else tccq_c_emit_expr(stmt$value, sym, idx = paste0("rhs_i_", tmp_id))
    return(c(
      common,
      row$lines,
      col$lines,
      rhs_setup,
      paste0("R_xlen_t n_lhs_", tmp_id, " = ", row$n, " * ", col$n, ";"),
      if (rhs_scalar) paste0(ctype, " rhs_", tmp_id, " = (", ctype, ")(", rhs_value, ");") else c(
        paste0("if (n_rhs_", tmp_id, " != n_lhs_", tmp_id, ") {"),
        "  Rf_error(\"vector RHS length must match indexed assignment extent\");",
        "}"
      ),
      paste0("for (R_xlen_t cc_", tmp_id, " = 0; cc_", tmp_id, " < ", col$n, "; ++cc_", tmp_id, ") {"),
      paste0("  for (R_xlen_t rr_", tmp_id, " = 0; rr_", tmp_id, " < ", row$n, "; ++rr_", tmp_id, ") {"),
      if (!rhs_scalar) paste0("    R_xlen_t rhs_i_", tmp_id, " = rr_", tmp_id, " + cc_", tmp_id, " * ", row$n, ";") else character(),
      paste0("    ", s$ptr, "[(", row$lo, " + rr_", tmp_id, ") + (", col$lo, " + cc_", tmp_id, ") * ", s$nrow, "] = (", ctype, ")(", rhs_elem, ");"),
      "  }",
      "}"
    ))
  }

  tccq_abort("generic assignment C emission currently supports rank-1 vectors and rank-2 matrices; rank ", s$type$rank, " is planned for array support")
}

tccq_c_emit_stmt_store_index2 <- function(stmt, sym, module, stmt_index = NULL) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank != 2L) {
    tccq_abort("matrix indexed assignment target must be a local matrix: ", stmt$name)
  }

  row <- tccq_c_emit_expr(stmt$row, sym, idx = NULL)
  col <- tccq_c_emit_expr(stmt$col, sym, idx = NULL)
  value <- tccq_c_emit_expr(stmt$value, sym, idx = NULL)
  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  id <- tccq_c_ident(stmt$name)
  tmp_id <- paste0(id, if (!is.null(stmt_index)) paste0("_", stmt_index) else "")

  c(
    tccq_c_emit_materialize_before_write(module, sym, stmt_index),
    if (!identical(tccq_c_storage_kind(module, stmt$name), "owned")) tccq_c_emit_materialize_local(stmt$name, sym, module),
    paste0("R_xlen_t row_", tmp_id, " = tccq_checked_index1((R_xlen_t)(", row, "), ", s$nrow, ", ", tccq_c_string(stmt$name), ");"),
    paste0("R_xlen_t col_", tmp_id, " = tccq_checked_index1((R_xlen_t)(", col, "), ", s$ncol, ", ", tccq_c_string(stmt$name), ");"),
    paste0(s$ptr, "[row_", tmp_id, " + col_", tmp_id, " * ", s$nrow, "] = (", ctype, ")(", value, ");")
  )
}

tccq_c_emit_stmt_store_access_index <- function(stmt, sym, module, stmt_index = NULL) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank != 1L) {
    tccq_abort("indexed assignment target must be a local vector: ", stmt$name)
  }
  if (is.null(stmt$access) || !identical(tccq_c_access_final_kind(stmt$access), "index")) {
    tccq_abort("nested indexed assignment requires a normalized final index access")
  }

  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  id <- tccq_c_ident(stmt$name)
  tmp_id <- paste0(id, if (!is.null(stmt_index)) paste0("_", stmt_index) else "")
  prefix <- paste0("store_", tmp_id)
  value <- tccq_c_emit_expr(stmt$value, sym, idx = NULL)

  c(
    tccq_c_emit_materialize_before_write(module, sym, stmt_index),
    if (!identical(tccq_c_storage_kind(module, stmt$name), "owned")) tccq_c_emit_materialize_local(stmt$name, sym, module),
    tccq_c_emit_access_setup(stmt$access, sym, prefix = prefix),
    paste0(s$ptr, "[off_", prefix, "] = (", ctype, ")(", value, ");")
  )
}

tccq_c_emit_stmt_store_access_range <- function(stmt, sym, module, stmt_index = NULL) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank != 1L) {
    tccq_abort("range assignment target must be a local vector: ", stmt$name)
  }
  if (is.null(stmt$access) || !identical(tccq_c_access_final_kind(stmt$access), "slice")) {
    tccq_abort("nested range assignment requires a normalized final slice access")
  }
  if (!tccq_is_scalar_rhs_for_assignment(stmt$value)) {
    tccq_abort("tccq currently supports scalar RHS range assignment only")
  }

  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  id <- tccq_c_ident(stmt$name)
  tmp_id <- paste0(id, if (!is.null(stmt_index)) paste0("_", stmt_index) else "")
  prefix <- paste0("store_", tmp_id)
  rhs_value <- tccq_c_emit_expr(stmt$value, sym, idx = NULL)

  c(
    tccq_c_emit_materialize_before_write(module, sym, stmt_index),
    if (!identical(tccq_c_storage_kind(module, stmt$name), "owned")) tccq_c_emit_materialize_local(stmt$name, sym, module),
    tccq_c_emit_access_setup(stmt$access, sym, prefix = prefix),
    paste0(ctype, " rhs_", tmp_id, " = (", ctype, ")(", rhs_value, ");"),
    paste0("for (R_xlen_t i = 0; i < n_", prefix, "; ++i) {"),
    paste0("  ", s$ptr, "[off_", prefix, " + i] = rhs_", tmp_id, ";"),
    "}"
  )
}

tccq_c_emit_stmt_store_index <- function(stmt, sym, module, stmt_index = NULL) {
  if (!is.null(stmt$access)) {
    return(tccq_c_emit_stmt_store_access_index(stmt, sym, module, stmt_index = stmt_index))
  }

  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local")) {
    tccq_abort("indexed assignment target must be a local vector: ", stmt$name)
  }
  if (s$type$rank != 1L) {
    tccq_abort("indexed assignment target must be a vector: ", stmt$name)
  }

  index <- tccq_c_emit_expr(stmt$index, sym, idx = NULL)
  value <- tccq_c_emit_expr(stmt$value, sym, idx = NULL)
  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  id <- tccq_c_ident(stmt$name)
  tmp_id <- paste0(id, if (!is.null(stmt_index)) paste0("_", stmt_index) else "")

  c(
    tccq_c_emit_materialize_before_write(module, sym, stmt_index),
    if (!identical(tccq_c_storage_kind(module, stmt$name), "owned")) tccq_c_emit_materialize_local(stmt$name, sym, module),
    paste0("R_xlen_t j_", tmp_id, " = tccq_checked_index1((R_xlen_t)(", index, "), ", s$len, ", ", tccq_c_string(stmt$name), ");"),
    paste0(s$ptr, "[j_", tmp_id, "] = (", ctype, ")(", value, ");")
  )
}

tccq_c_emit_stmt_store_range <- function(stmt, sym, module, stmt_index = NULL) {
  if (!is.null(stmt$access)) {
    return(tccq_c_emit_stmt_store_access_range(stmt, sym, module, stmt_index = stmt_index))
  }

  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local")) {
    tccq_abort("range assignment target must be a local vector: ", stmt$name)
  }
  if (s$type$rank != 1L) {
    tccq_abort("range assignment target must be a vector: ", stmt$name)
  }
  if (!tccq_is_scalar_rhs_for_assignment(stmt$value)) {
    tccq_abort("tccq currently supports scalar RHS range assignment only")
  }

  id <- tccq_c_ident(stmt$name)
  tmp_id <- paste0(id, if (!is.null(stmt_index)) paste0("_", stmt_index) else "")
  start <- tccq_c_emit_expr(stmt$start, sym, idx = NULL)
  stop <- tccq_c_emit_expr(stmt$stop, sym, idx = NULL)
  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  rhs_value <- tccq_c_emit_expr(stmt$value, sym, idx = NULL)

  c(
    tccq_c_emit_materialize_before_write(module, sym, stmt_index),
    if (!identical(tccq_c_storage_kind(module, stmt$name), "owned")) tccq_c_emit_materialize_local(stmt$name, sym, module),
    paste0("R_xlen_t lo_", tmp_id, " = tccq_checked_index1((R_xlen_t)(", start, "), ", s$len, ", ", tccq_c_string(stmt$name), ");"),
    paste0("R_xlen_t hi_", tmp_id, " = tccq_checked_index1((R_xlen_t)(", stop, "), ", s$len, ", ", tccq_c_string(stmt$name), ");"),
    paste0("if (hi_", tmp_id, " < lo_", tmp_id, ") { Rf_error(\"decreasing ranges are not supported in indexed assignment\"); }"),
    paste0("R_xlen_t n_rng_", tmp_id, " = hi_", tmp_id, " - lo_", tmp_id, " + 1;"),
    paste0(ctype, " rhs_", tmp_id, " = (", ctype, ")(", rhs_value, ");"),
    paste0("for (R_xlen_t i = 0; i < n_rng_", tmp_id, "; ++i) {"),
    paste0("  ", s$ptr, "[lo_", tmp_id, " + i] = rhs_", tmp_id, ";"),
    "}"
  )
}

tccq_c_access_plan <- function(node) {
  plan <- tccq_ir_normalized_access(node)
  if (is.null(plan) || is.null(plan$base_name)) {
    tccq_abort(
      "tccq currently supports x[i] / x[lo:hi] only when the base is a direct variable ",
      "or a normalized access chain. Bind the composite vector expression first."
    )
  }
  plan
}

tccq_c_access_base_symbol <- function(node, sym) {
  plan <- tccq_c_access_plan(node)
  base <- sym[[plan$base_name]]
  if (is.null(base)) {
    tccq_abort("unknown normalized access base symbol: ", plan$base_name)
  }
  base
}

tccq_c_access_final_kind <- function(node) {
  plan <- tccq_c_access_plan(node)
  if (!length(plan$steps)) {
    return("base")
  }
  plan$steps[[length(plan$steps)]]$kind
}

tccq_c_emit_access_setup <- function(node, sym, prefix) {
  plan <- tccq_c_access_plan(node)
  base <- tccq_c_access_base_symbol(node, sym)
  lines <- c(
    paste0("R_xlen_t off_", prefix, " = 0;"),
    paste0("R_xlen_t n_", prefix, " = ", base$len, ";"),
    paste0("int missing_", prefix, " = 0;")
  )

  if (!length(plan$steps)) {
    return(lines)
  }

  for (i in seq_along(plan$steps)) {
    step <- plan$steps[[i]]
    if (identical(step$kind, "slice")) {
      start <- tccq_c_emit_expr(step$start, sym, idx = NULL)
      stop <- tccq_c_emit_expr(step$stop, sym, idx = NULL)
      lines <- c(
        lines,
        paste0("if (!missing_", prefix, ") {"),
        paste0("  R_xlen_t rel_lo_", prefix, "_", i, " = tccq_checked_index1((R_xlen_t)(", start, "), n_", prefix, ", ", tccq_c_string(plan$base_name), ");"),
        paste0("  R_xlen_t rel_hi_", prefix, "_", i, " = tccq_checked_index1((R_xlen_t)(", stop, "), n_", prefix, ", ", tccq_c_string(plan$base_name), ");"),
        paste0("  if (rel_hi_", prefix, "_", i, " < rel_lo_", prefix, "_", i, ") { Rf_error(\"decreasing slices are not supported\"); }"),
        paste0("  off_", prefix, " = off_", prefix, " + rel_lo_", prefix, "_", i, ";"),
        paste0("  n_", prefix, " = rel_hi_", prefix, "_", i, " - rel_lo_", prefix, "_", i, " + 1;"),
        "}"
      )
    } else if (identical(step$kind, "index")) {
      index <- tccq_c_emit_expr(step$index, sym, idx = NULL)
      raw <- paste0("raw_idx_", prefix, "_", i)
      missing <- tccq_c_emit_missing_check(index, step$index$type)
      lines <- c(
        lines,
        paste0("if (!missing_", prefix, ") {"),
        paste0("  if (", missing, ") {"),
        paste0("    missing_", prefix, " = 1;"),
        paste0("    n_", prefix, " = 0;"),
        "  } else {",
        paste0("    R_xlen_t ", raw, " = (R_xlen_t)(", index, ");"),
        paste0("    R_xlen_t rel_idx_", prefix, "_", i, " = tccq_checked_index1(", raw, ", n_", prefix, ", ", tccq_c_string(plan$base_name), ");"),
        paste0("    off_", prefix, " = off_", prefix, " + rel_idx_", prefix, "_", i, ";"),
        paste0("    n_", prefix, " = 1;"),
        "  }",
        "}"
      )
    } else {
      tccq_abort("unsupported normalized access step kind: ", step$kind)
    }
  }

  lines
}

tccq_c_emit_access_elem <- function(node, sym, prefix, idx = NULL) {
  base <- tccq_c_access_base_symbol(node, sym)
  final_kind <- tccq_c_access_final_kind(node)

  if (identical(final_kind, "index")) {
    return(paste0("(missing_", prefix, " ? ", tccq_c_emit_na_value_for_mode(node$type$mode), " : ", base$ptr, "[off_", prefix, "])"))
  }

  if (is.null(idx)) {
    tccq_abort("normalized slice/view access requires an element index in C emission")
  }

  paste0(base$ptr, "[off_", prefix, " + ", idx, "]")
}

tccq_c_emit_slice_range_indices <- function(node, sym, prefix) {
  if (!identical(tccq_c_access_final_kind(node), "slice")) {
    tccq_abort("slice/view length emission requires a normalized slice result")
  }
  tccq_c_emit_access_setup(node, sym, prefix)
}

tccq_c_emit_slice_range_elem <- function(node, sym, idx, prefix) {
  if (!identical(tccq_c_access_final_kind(node), "slice")) {
    tccq_abort("slice/view element emission requires a normalized slice result")
  }
  tccq_c_emit_access_elem(node, sym, prefix, idx = idx)
}

tccq_c_emit_materialize_slice_range <- function(node, sym, out_name, out_ptr) {
  mode <- node$type$mode
  ctype <- tccq_c_scalar_type_for_mode(mode)
  setup <- tccq_c_emit_slice_range_indices(node, sym, prefix = out_name)
  elem <- tccq_c_emit_slice_range_elem(node, sym, idx = "i", prefix = out_name)

  c(
    setup,
    paste0("SEXP ", out_name, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(mode), ", n_", out_name, "));"),
    "++tccq_nprotect;",
    paste0(ctype, " *", out_ptr, " = ", tccq_c_rw_accessor(mode), "(", out_name, ");"),
    paste0("for (R_xlen_t i = 0; i < n_", out_name, "; ++i) {"),
    paste0("  ", out_ptr, "[i] = (", ctype, ")(", elem, ");"),
    "}",
    "UNPROTECT(tccq_nprotect);",
    paste0("return ", out_name, ";")
  )
}

tccq_c_emit_bind_slice_range <- function(stmt, sym, module) {
  s <- sym[[stmt$name]]
  mode <- stmt$type$mode
  ctype <- tccq_c_scalar_type_for_mode(mode)
  setup <- tccq_c_emit_slice_range_indices(stmt$value, sym, prefix = s$len)
  base <- tccq_c_access_base_symbol(stmt$value, sym)
  own_flag <- tccq_c_owned_flag(s)

  if (identical(tccq_c_storage_kind(module, stmt$name), "view")) {
    ptr_init <- if (stmt$name %in% tccq_c_module_data_vars(module)) {
      paste0("(", ctype, " *) (", base$ptr, " + off_", s$len, ")")
    } else {
      "NULL"
    }
    return(c(
      setup,
      paste0("R_xlen_t ", s$len, " = n_", s$len, ";"),
      paste0("SEXP ", s$sexp, " = R_NilValue;"),
      paste0(ctype, " *", s$ptr, " = ", ptr_init, ";"),
      paste0("int ", own_flag, " = 0;")
    ))
  }

  elem <- tccq_c_emit_slice_range_elem(stmt$value, sym, idx = "i", prefix = s$len)
  c(
    setup,
    paste0("R_xlen_t ", s$len, " = n_", s$len, ";"),
    paste0("SEXP ", s$sexp, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(mode), ", ", s$len, "));"),
    "++tccq_nprotect;",
    paste0(ctype, " *", s$ptr, " = ", tccq_c_rw_accessor(mode), "(", s$sexp, ");"),
    paste0("int ", own_flag, " = 1;"),
    paste0("for (R_xlen_t i = 0; i < ", s$len, "; ++i) {"),
    paste0("  ", s$ptr, "[i] = (", ctype, ")(", elem, ");"),
    "}"
  )
}

tccq_c_emit_assign_scalar <- function(out, mode, index, expr) {
  switch(
    mode,
    double = paste0("REAL(", out, ")[", index, "] = (double)(", expr, ");"),
    integer = paste0("INTEGER(", out, ")[", index, "] = (int)(", expr, ");"),
    logical = paste0("LOGICAL(", out, ")[", index, "] = (int)(", expr, ");"),
    raw = paste0("RAW(", out, ")[", index, "] = (Rbyte)(", expr, ");"),
    xlen = tccq_abort("xlen scalar assignment requires dynamic boxing"),
    tccq_abort("unsupported scalar assign mode: ", mode)
  )
}

tccq_c_emit_expr <- function(node, sym, idx = NULL) {
  if (!is.list(node) || is.null(node$tag)) {
    tccq_abort("expected IR node in C expression emitter")
  }

  switch(
    node$tag,
    const = tccq_c_const_literal(node$value, node$type$mode),
    var = tccq_c_emit_var(node, sym, idx),
    unary = tccq_c_emit_unary(node, sym, idx),
    binary = tccq_c_emit_binary(node, sym, idx),
    call1 = tccq_c_emit_call1(node, sym, idx),
    len = tccq_c_emit_len(node, sym),
    index = tccq_c_emit_index(node, sym, idx),
    index2 = tccq_c_emit_index2(node, sym),
    matrix_view = tccq_c_emit_matrix_view_expr(node, sym, idx),
    matrix_fill = {
      if (is.null(idx)) {
        tccq_abort("matrix_fill nodes require a loop index when emitted as element expressions")
      }
      tccq_c_emit_expr(node$value, sym, idx = NULL)
    },
    vector_fill = {
      if (is.null(idx)) {
        tccq_abort("vector_fill nodes require a loop index when emitted as element expressions")
      }
      tccq_c_emit_expr(node$value, sym, idx = NULL)
    },
    slice_range = tccq_c_emit_slice_range_expr(node, sym, idx),
    view1 = tccq_c_emit_slice_range_expr(node, sym, idx),
    reduce = tccq_abort("nested reduce is not supported in milestone 1"),
    arg_reduce = tccq_abort("nested arg reducer is not supported without scalar hoisting"),
    boundary_call = tccq_abort("boundary_call nodes are not emitted directly in this milestone"),
    boundary = tccq_abort("boundary nodes are not emitted directly"),
    tccq_abort("unsupported IR tag in C expression emitter: ", node$tag)
  )
}

tccq_c_emit_var <- function(node, sym, idx = NULL) {
  s <- sym[[node$name]]
  if (is.null(s)) {
    tccq_abort("unknown C symbol for variable: ", node$name)
  }

  if (node$type$rank == 0L) {
    return(s$val)
  }

  if (is.null(idx)) {
    tccq_abort("vector variable '", node$name, "' requires an index in C expression emission")
  }

  paste0(s$ptr, "[", idx, "]")
}

tccq_c_emit_unary <- function(node, sym, idx = NULL) {
  x <- tccq_c_emit_expr(node$x, sym, idx)
  switch(
    node$op,
    `-` = {
      if (identical(node$type$mode, "double")) {
        # TinyCC's ARM64 backend can abort in its generic floating unary-minus
        # path (`load()` mixed int/float register assertion). Emit floating
        # negation as a binary multiply so it uses the backend's ordinary
        # floating binary-op path instead.
        paste0("((-1.0) * (double)(", x, "))")
      } else if (identical(node$type$mode, "integer")) {
        paste0("tccq_int_neg((int)(", x, "))")
      } else {
        paste0("(-(", x, "))")
      }
    },
    `!` = paste0("tccq_lgl_not((int)(", x, "))"),
    tccq_abort("unsupported unary op: ", node$op)
  )
}

tccq_c_emit_double_operand <- function(expr, type) {
  if (type$mode %in% c("integer", "logical")) {
    missing <- tccq_c_emit_missing_check(expr, type)
    return(paste0("((", missing, ") ? NA_REAL : (double)(", expr, "))"))
  }
  paste0("(double)(", expr, ")")
}

tccq_c_emit_binary <- function(node, sym, idx = NULL) {
  lhs <- tccq_c_emit_expr(node$lhs, sym, idx)
  rhs <- tccq_c_emit_expr(node$rhs, sym, idx)

  if (identical(node$op, "^")) {
    lhs_d <- tccq_c_emit_double_operand(lhs, node$lhs$type)
    rhs_d <- tccq_c_emit_double_operand(rhs, node$rhs$type)
    return(paste0("pow(", lhs_d, ", ", rhs_d, ")"))
  }

  if (identical(node$op, "/")) {
    lhs_d <- tccq_c_emit_double_operand(lhs, node$lhs$type)
    rhs_d <- tccq_c_emit_double_operand(rhs, node$rhs$type)
    return(paste0("(", lhs_d, " / ", rhs_d, ")"))
  }

  if (identical(node$op, "%/%")) {
    if (identical(node$type$mode, "double")) {
      lhs_d <- tccq_c_emit_double_operand(lhs, node$lhs$type)
      rhs_d <- tccq_c_emit_double_operand(rhs, node$rhs$type)
      return(paste0("floor(", lhs_d, " / ", rhs_d, ")"))
    }
    return(paste0("tccq_int_idiv((int)(", lhs, "), (int)(", rhs, "))"))
  }

  if (node$op %in% c("+", "-", "*") && identical(node$type$mode, "double")) {
    lhs_d <- tccq_c_emit_double_operand(lhs, node$lhs$type)
    rhs_d <- tccq_c_emit_double_operand(rhs, node$rhs$type)
    return(paste0("(", lhs_d, " ", node$op, " ", rhs_d, ")"))
  }

  if (node$op %in% c("+", "-", "*") && identical(node$type$mode, "integer")) {
    fun <- switch(
      node$op,
      `+` = "tccq_int_add",
      `-` = "tccq_int_sub",
      `*` = "tccq_int_mul"
    )
    return(paste0(fun, "((int)(", lhs, "), (int)(", rhs, "))"))
  }

  if (identical(node$op, "&")) {
    return(paste0("tccq_lgl_and((int)(", lhs, "), (int)(", rhs, "))"))
  }

  if (identical(node$op, "|")) {
    return(paste0("tccq_lgl_or((int)(", lhs, "), (int)(", rhs, "))"))
  }

  if (node$op %in% c("<", "<=", ">", ">=", "==", "!=")) {
    lhs_na <- tccq_c_emit_missing_check(lhs, node$lhs$type)
    rhs_na <- tccq_c_emit_missing_check(rhs, node$rhs$type)
    return(paste0(
      "((", lhs_na, " || ", rhs_na, ") ? NA_LOGICAL : (((", lhs, ") ", node$op, " (", rhs, ")) ? 1 : 0))"
    ))
  }

  paste0("((", lhs, ") ", node$op, " (", rhs, "))")
}

tccq_c_emit_call1 <- function(node, sym, idx = NULL) {
  x <- tccq_c_emit_expr(node$x, sym, idx)
  fun <- switch(
    node$fun,
    sin = "sin",
    cos = "cos",
    tan = "tan",
    exp = "exp",
    log = "log",
    sqrt = "sqrt",
    abs = "fabs",
    tccq_abort("unsupported C math function: ", node$fun)
  )
  paste0(fun, "((double)(", x, "))")
}

tccq_c_emit_len <- function(node, sym) {
  if (node$x$type$rank == 0L) {
    if (identical(node$x$tag, "const")) {
      return("1")
    }
    if (identical(node$x$tag, "var")) {
      s <- sym[[node$x$name]]
      if (is.null(s)) {
        tccq_abort("unknown C symbol for length(): ", node$x$name)
      }
      return(paste0("(R_xlen_t)", s$len))
    }
    tccq_abort("scalar length expression should have been hoisted before C emission")
  }
  if (!identical(node$x$tag, "var")) {
    tccq_abort("non-variable vector length should have been hoisted before C emission")
  }
  s <- sym[[node$x$name]]
  if (is.null(s)) {
    tccq_abort("unknown C symbol for length(): ", node$x$name)
  }
  paste0("(R_xlen_t)", s$len)
}

tccq_c_matrix_view_kind <- function(node) {
  kinds <- vapply(node$subscripts %||% list(), `[[`, character(1), "kind")
  if (identical(kinds, c("all", "index"))) {
    return("column")
  }
  if (identical(kinds, c("index", "all"))) {
    return("row")
  }
  tccq_abort("matrix view C emission currently supports x[, j] or x[i, ]")
}

tccq_c_emit_matrix_view_length_setup <- function(node, sym, len_name) {
  if (!identical(node$x$tag, "var")) {
    tccq_abort("matrix view currently supports direct matrix variable base only")
  }
  base <- sym[[node$x$name]]
  kind <- tccq_c_matrix_view_kind(node)
  if (identical(kind, "column")) {
    return(paste0("R_xlen_t ", len_name, " = ", base$nrow, ";"))
  }
  paste0("R_xlen_t ", len_name, " = ", base$ncol, ";")
}

tccq_c_emit_matrix_view_expr <- function(node, sym, idx = NULL) {
  if (is.null(idx)) {
    tccq_abort("matrix view requires an element index in C expression emission")
  }
  if (!identical(node$x$tag, "var")) {
    tccq_abort("matrix view currently supports direct matrix variable base only")
  }

  base <- sym[[node$x$name]]
  kind <- tccq_c_matrix_view_kind(node)

  if (identical(kind, "column")) {
    col_sub <- node$subscripts[[2L]]
    col <- tccq_c_emit_expr(col_sub$index, sym, idx = NULL)
    missing <- tccq_c_emit_missing_check(col, col_sub$index$type)
    return(paste0(
      "((", missing, ") ? ", tccq_c_emit_na_value_for_mode(node$type$mode), " : ",
      base$ptr,
      "[", idx, " + tccq_checked_index1((R_xlen_t)(", col, "), ", base$ncol, ", ", tccq_c_string(node$x$name), ") * ", base$nrow, "])"
    ))
  }

  row_sub <- node$subscripts[[1L]]
  row <- tccq_c_emit_expr(row_sub$index, sym, idx = NULL)
  missing <- tccq_c_emit_missing_check(row, row_sub$index$type)
  paste0(
    "((", missing, ") ? ", tccq_c_emit_na_value_for_mode(node$type$mode), " : ",
    base$ptr,
    "[tccq_checked_index1((R_xlen_t)(", row, "), ", base$nrow, ", ", tccq_c_string(node$x$name), ") + ", idx, " * ", base$nrow, "])"
  )
}

tccq_c_emit_index <- function(node, sym, idx = NULL) {
  if (!identical(node$x$tag, "var")) {
    tccq_abort("x[i] currently supports direct variable base only")
  }
  base <- sym[[node$x$name]]
  if (!is.null(node$type) && node$type$rank == 1L) {
    if (is.null(idx)) {
      tccq_abort("gather x[i] requires an element index in C expression emission")
    }
    index <- tccq_c_emit_expr(node$index, sym, idx = idx)
    missing <- tccq_c_emit_missing_check(index, node$index$type)
    return(paste0(
      "((", missing, ") ? ", tccq_c_emit_na_value_for_mode(node$type$mode), " : ",
      base$ptr,
      "[tccq_checked_index1((R_xlen_t)(", index, "), ",
      base$len,
      ", ",
      tccq_c_string(node$x$name),
      ")])"
    ))
  }

  index <- tccq_c_emit_expr(node$index, sym, idx = NULL)
  missing <- tccq_c_emit_missing_check(index, node$index$type)
  paste0(
    "((", missing, ") ? ", tccq_c_emit_na_value_for_mode(node$type$mode), " : ",
    base$ptr,
    "[tccq_checked_index1((R_xlen_t)(", index, "), ",
    base$len,
    ", ",
    tccq_c_string(node$x$name),
    ")])"
  )
}

tccq_c_emit_index2 <- function(node, sym) {
  if (!identical(node$x$tag, "var")) {
    tccq_abort("x[i, j] currently supports direct matrix variable base only")
  }
  base <- sym[[node$x$name]]
  row <- tccq_c_emit_expr(node$row, sym, idx = NULL)
  col <- tccq_c_emit_expr(node$col, sym, idx = NULL)
  row_missing <- tccq_c_emit_missing_check(row, node$row$type)
  col_missing <- tccq_c_emit_missing_check(col, node$col$type)
  paste0(
    "((", row_missing, " || ", col_missing, ") ? ", tccq_c_emit_na_value_for_mode(node$type$mode), " : ",
    base$ptr,
    "[tccq_checked_index1((R_xlen_t)(", row, "), ", base$nrow, ", ", tccq_c_string(node$x$name), ") + ",
    "tccq_checked_index1((R_xlen_t)(", col, "), ", base$ncol, ", ", tccq_c_string(node$x$name), ") * ", base$nrow,
    "])"
  )
}

tccq_c_emit_slice_range_expr <- function(node, sym, idx = NULL) {
  if (is.null(idx)) {
    tccq_abort("view requires an element index in expression emission")
  }
  if (!identical(node$x$tag, "var")) {
    tccq_abort("view currently supports direct variable base only")
  }

  base <- sym[[node$x$name]]
  start <- tccq_c_emit_expr(node$start, sym, idx = NULL)
  paste0(
    base$ptr,
    "[tccq_checked_index1((R_xlen_t)(", start, "), ",
    base$len,
    ", ",
    tccq_c_string(node$x$name),
    ") + ",
    idx,
    "]"
  )
}
