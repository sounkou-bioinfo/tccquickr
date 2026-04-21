# tccq_print.R - compact printers for fresh compiler objects
# SPDX-License-Identifier: GPL-3.0-or-later

#' Format a `tccq` type
#'
#' @param x A `tccq_type` object.
#' @param ... Unused.
#' @return A compact character representation.
#' @exportS3Method format tccq_type
format.tccq_type <- function(x, ...) {
  tccq_type_to_string(x)
}

#' Print a `tccq` type
#'
#' @param x A `tccq_type` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @exportS3Method print tccq_type
print.tccq_type <- function(x, ...) {
  cat("<tccq_type>", format(x, ...), "\n")
  invisible(x)
}

tccq_kernel_result_tag <- function(kernel) {
  if (is.null(kernel) || is.null(kernel$tag)) {
    return("<none>")
  }

  if (identical(kernel$tag, "kernel_program")) {
    result_kernel <- kernel$result_kernel
    if (is.null(result_kernel) || is.null(result_kernel$tag)) {
      return("kernel_program -> <none>")
    }
    return(paste0("kernel_program -> ", result_kernel$tag))
  }

  kernel$tag
}

#' Print a `tccq` module summary
#'
#' @param x A `tccq_module` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @exportS3Method print tccq_module
print.tccq_module <- function(x, ...) {
  tccq_module_validate(x)

  cat("<tccq_module>\n")
  cat("  entry:", x$entry, "\n")

  if (length(x$formal_names)) {
    cat("  formals:\n")
    for (nm in x$formal_names) {
      cat("   -", nm, ":", format(x$types[[nm]]), "\n")
    }
  } else {
    cat("  formals: <none>\n")
  }

  if (!is.null(x$ir) && !is.null(x$ir$tag)) {
    cat("  ir:", x$ir$tag, "\n")
  }

  if (!is.null(x$kernel)) {
    cat("  kernel:", tccq_kernel_result_tag(x$kernel), "\n")
  }

  locals <- if (!is.null(x$ir)) tccq_ir_program_locals(x$ir) else list()
  if (length(locals)) {
    cat("  locals:\n")
    for (nm in names(locals)) {
      cat("   -", nm, ":", format(locals[[nm]]), "\n")
    }
  }

  if (!is.null(x$ir) && identical(x$ir$tag, "program") && length(x$ir$stmts)) {
    stmt_tags <- vapply(x$ir$stmts, `[[`, character(1), "tag")
    cat("  statements: ", length(x$ir$stmts), " [", paste(stmt_tags, collapse = ", "), "]\n", sep = "")
  }

  result_type <- NULL
  if (!is.null(x$kernel) && !is.null(x$kernel$type)) {
    result_type <- x$kernel$type
  } else if (!is.null(x$ir) && !is.null(x$ir$type)) {
    result_type <- x$ir$type
  }
  if (!is.null(result_type)) {
    cat("  result type:", format(result_type), "\n")
  }
  invisible(x)
}
