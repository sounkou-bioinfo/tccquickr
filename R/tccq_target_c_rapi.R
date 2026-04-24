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
    } else {
      tccq_abort(
        "C target supports scalar/vector only in this milestone; argument ", nm,
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

tccq_c_emit_common_length_named <- function(expr, sym, len_name, module = NULL) {
  emit_length <- function(node, target) {
    if (is.null(node$type) || node$type$rank == 0L) {
      return(tccq_c_emit_scalar_length_setup(node, sym, module, target))
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
    slice_range = tccq_c_expr_length_needs_eval(node$start) || tccq_c_expr_length_needs_eval(node$stop),
    view1 = tccq_c_expr_length_needs_eval(node$start) || tccq_c_expr_length_needs_eval(node$stop),
    boundary_call = TRUE,
    len = tccq_c_expr_length_needs_eval(node$x),
    FALSE
  )
}

tccq_c_expr_length_data_vars <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(character())
  }

  if (is.null(node$type) || node$type$rank == 0L) {
    return(tccq_c_expr_data_vars(node))
  }

  switch(
    node$tag,
    var = character(),
    unary = tccq_c_expr_length_data_vars(node$x),
    call1 = tccq_c_expr_length_data_vars(node$x),
    binary = tccq_unique(c(tccq_c_expr_length_data_vars(node$lhs), tccq_c_expr_length_data_vars(node$rhs))),
    slice_range = tccq_unique(c(tccq_c_expr_data_vars(node$start), tccq_c_expr_data_vars(node$stop))),
    view1 = tccq_unique(c(tccq_c_expr_data_vars(node$start), tccq_c_expr_data_vars(node$stop))),
    boundary_call = tccq_unique(unlist(lapply(node$args %||% list(), tccq_c_expr_data_vars), use.names = FALSE)),
    len = tccq_c_expr_length_data_vars(node$x),
    character()
  )
}

tccq_c_expr_data_vars <- function(node) {
  if (!is.list(node) || is.null(node$tag)) {
    return(character())
  }

  switch(
    node$tag,
    const = character(),
    var = if (!is.null(node$type) && node$type$rank > 0L) node$name else character(),
    unary = tccq_c_expr_data_vars(node$x),
    call1 = tccq_c_expr_data_vars(node$x),
    binary = tccq_unique(c(tccq_c_expr_data_vars(node$lhs), tccq_c_expr_data_vars(node$rhs))),
    reduce = tccq_c_expr_data_vars(node$x),
    len = tccq_c_expr_length_data_vars(node$x),
    index = tccq_unique(c(tccq_c_expr_data_vars(node$x), tccq_c_expr_data_vars(node$index))),
    slice_range = tccq_unique(c(tccq_c_expr_data_vars(node$x), tccq_c_expr_data_vars(node$start), tccq_c_expr_data_vars(node$stop))),
    view1 = tccq_unique(c(tccq_c_expr_data_vars(node$x), tccq_c_expr_data_vars(node$start), tccq_c_expr_data_vars(node$stop))),
    boundary_call = tccq_unique(unlist(lapply(node$args %||% list(), tccq_c_expr_data_vars), use.names = FALSE)),
    character()
  )
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
          return(tccq_unique(c(tccq_c_expr_data_vars(value$start), tccq_c_expr_data_vars(value$stop))))
        }
        return(character())
      }
      tccq_c_expr_data_vars(stmt$value)
    },
    store_index = tccq_unique(c(stmt$name, tccq_c_expr_data_vars(stmt$index), tccq_c_expr_data_vars(stmt$value))),
    store_range = tccq_unique(c(stmt$name, tccq_c_expr_data_vars(stmt$start), tccq_c_expr_data_vars(stmt$stop), tccq_c_expr_data_vars(stmt$value))),
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
    slice_range = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$start), tccq_c_expr_scalar_value_vars(node$stop))),
    view1 = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$start), tccq_c_expr_scalar_value_vars(node$stop))),
    index = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$index))),
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
    len = tccq_c_expr_length_scalar_value_vars(node$x),
    index = tccq_unique(c(tccq_c_access_scalar_value_vars(node), tccq_c_expr_scalar_value_vars(node$index))),
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
    store_index = tccq_unique(c(tccq_c_expr_scalar_value_vars(stmt$index), tccq_c_expr_scalar_value_vars(stmt$value))),
    store_range = tccq_unique(c(tccq_c_expr_scalar_value_vars(stmt$start), tccq_c_expr_scalar_value_vars(stmt$stop), tccq_c_expr_scalar_value_vars(stmt$value))),
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
    function(x) x$materialize_views %||% character()
  ), use.names = FALSE)

  data <- tccq_unique(c(
    unlist(lapply(ir$stmts %||% list(), tccq_c_stmt_data_vars, module = module), use.names = FALSE),
    tccq_c_expr_data_vars(ir$result),
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

tccq_c_emit_boundary_arg <- function(arg, sym, module, name) {
  if (identical(arg$tag, "var")) {
    s <- sym[[arg$name]]
    if (is.null(s)) {
      tccq_abort("unknown symbol in boundary arg: ", arg$name)
    }
    if (arg$type$rank > 0L) {
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
    tmp <- tccq_c_emit_boundary_arg(node$args[[i]], sym, module, paste0("arg_", prefix, "_", i))
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

tccq_c_emit_materialize_local <- function(name, sym, module) {
  s <- sym[[name]]
  if (is.null(s) || !identical(s$kind, "local") || s$type$rank != 1L) {
    tccq_abort("materialize_local requires a local vector: ", name)
  }

  own_flag <- tccq_c_owned_flag(s)
  ctype <- tccq_c_scalar_type_for_mode(s$type$mode)
  c(
    paste0("if (!", own_flag, ") {"),
    paste0("  ", s$sexp, " = PROTECT(Rf_allocVector(", tccq_sexptype_for_mode(s$type$mode), ", ", s$len, "));"),
    "  ++tccq_nprotect;",
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

  elem <- tccq_c_emit_expr(expr, sym, idx = "i")

  c(
    tccq_c_emit_common_length_named(expr, sym, "n_out", module = module),
    paste0("SEXP out = PROTECT(Rf_allocVector(", out_sexp, ", n_out));"),
    "++tccq_nprotect;",
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

tccq_c_emit_stmt <- function(stmt, sym, module, stmt_index = NULL) {
  switch(
    stmt$tag,
    bind = tccq_c_emit_stmt_bind(stmt, sym, module),
    store_index = tccq_c_emit_stmt_store_index(stmt, sym, module, stmt_index = stmt_index),
    store_range = tccq_c_emit_stmt_store_range(stmt, sym, module, stmt_index = stmt_index),
    tccq_abort("unsupported statement tag: ", stmt$tag)
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

tccq_c_emit_stmt_bind <- function(stmt, sym, module) {
  s <- sym[[stmt$name]]
  if (is.null(s) || !identical(s$kind, "local")) {
    tccq_abort("bind target must be a local symbol: ", stmt$name)
  }

  type <- stmt$type
  scalar_value_vars <- tccq_c_module_scalar_value_vars(module)

  if (type$rank == 0L) {
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

  if (type$rank != 1L) {
    tccq_abort("local bind currently supports scalar/vector only")
  }

  storage_kind <- tccq_c_storage_kind(module, stmt$name)
  own_flag <- tccq_c_owned_flag(s)
  ctype <- tccq_c_scalar_type_for_mode(type$mode)

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
  views <- barrier$materialize_views %||% character()
  if (!length(views)) {
    return(character())
  }

  unlist(lapply(views, tccq_c_emit_materialize_local, sym = sym, module = module), use.names = FALSE)
}

tccq_c_emit_stmt_store_index <- function(stmt, sym, module, stmt_index = NULL) {
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
    paste0("R_xlen_t n_", prefix, " = ", base$len, ";")
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
        paste0("R_xlen_t rel_lo_", prefix, "_", i, " = tccq_checked_index1((R_xlen_t)(", start, "), n_", prefix, ", ", tccq_c_string(plan$base_name), ");"),
        paste0("R_xlen_t rel_hi_", prefix, "_", i, " = tccq_checked_index1((R_xlen_t)(", stop, "), n_", prefix, ", ", tccq_c_string(plan$base_name), ");"),
        paste0("if (rel_hi_", prefix, "_", i, " < rel_lo_", prefix, "_", i, ") { Rf_error(\"decreasing slices are not supported\"); }"),
        paste0("off_", prefix, " = off_", prefix, " + rel_lo_", prefix, "_", i, ";"),
        paste0("n_", prefix, " = rel_hi_", prefix, "_", i, " - rel_lo_", prefix, "_", i, " + 1;")
      )
    } else if (identical(step$kind, "index")) {
      index <- tccq_c_emit_expr(step$index, sym, idx = NULL)
      lines <- c(
        lines,
        paste0("R_xlen_t rel_idx_", prefix, "_", i, " = tccq_checked_index1((R_xlen_t)(", index, "), n_", prefix, ", ", tccq_c_string(plan$base_name), ");"),
        paste0("off_", prefix, " = off_", prefix, " + rel_idx_", prefix, "_", i, ";"),
        paste0("n_", prefix, " = 1;")
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
    return(paste0(base$ptr, "[off_", prefix, "]"))
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
    slice_range = tccq_c_emit_slice_range_expr(node, sym, idx),
    view1 = tccq_c_emit_slice_range_expr(node, sym, idx),
    reduce = tccq_abort("nested reduce is not supported in milestone 1"),
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
    `-` = paste0("(-(", x, "))"),
    `!` = paste0("tccq_lgl_not((int)(", x, "))"),
    tccq_abort("unsupported unary op: ", node$op)
  )
}

tccq_c_emit_binary <- function(node, sym, idx = NULL) {
  lhs <- tccq_c_emit_expr(node$lhs, sym, idx)
  rhs <- tccq_c_emit_expr(node$rhs, sym, idx)

  if (identical(node$op, "^")) {
    return(paste0("pow((double)(", lhs, "), (double)(", rhs, "))"))
  }

  if (identical(node$op, "/")) {
    return(paste0("((double)(", lhs, ") / (double)(", rhs, "))"))
  }

  if (node$op %in% c("+", "-", "*") && identical(node$type$mode, "double")) {
    return(paste0("((double)(", lhs, ") ", node$op, " (double)(", rhs, "))"))
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

tccq_c_emit_index <- function(node, sym, idx = NULL) {
  if (!identical(node$x$tag, "var")) {
    tccq_abort("x[i] currently supports direct variable base only")
  }
  base <- sym[[node$x$name]]
  index <- tccq_c_emit_expr(node$index, sym, idx = NULL)
  paste0(
    base$ptr,
    "[tccq_checked_index1((R_xlen_t)(", index, "), ",
    base$len,
    ", ",
    tccq_c_string(node$x$name),
    ")]"
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
