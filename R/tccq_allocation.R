# tccq_allocation.R - conservative allocation and output-reuse planning
# SPDX-License-Identifier: GPL-3.0-or-later

# A final vector expression can be written back into an owned local only when the
# expression is strictly pointwise over that same local.  This deliberately
# excludes indexed reads, slices, reducers, and boundary calls; those can observe
# non-current elements or cross semantic barriers, so they need a fresh result
# until the middle-end has a richer dependence model.
tccq_allocation_expr_pointwise_over <- function(expr, name) {
  if (!is.list(expr) || is.null(expr$tag)) {
    return(TRUE)
  }

  switch(
    expr$tag,
    const = TRUE,
    var = {
      if (!is.null(expr$type) && expr$type$rank > 0L) {
        identical(expr$name, name)
      } else {
        TRUE
      }
    },
    unary = tccq_allocation_expr_pointwise_over(expr$x, name),
    call1 = tccq_allocation_expr_pointwise_over(expr$x, name),
    binary = tccq_allocation_expr_pointwise_over(expr$lhs, name) &&
      tccq_allocation_expr_pointwise_over(expr$rhs, name),
    len = TRUE,
    index = FALSE,
    slice_range = FALSE,
    view1 = FALSE,
    reduce = FALSE,
    boundary_call = FALSE,
    FALSE
  )
}

tccq_allocation_result_expr <- function(module) {
  kernel <- module$kernel
  if (is.null(kernel)) {
    return(NULL)
  }
  result_kernel <- if (identical(kernel$tag, "kernel_program")) kernel$result_kernel else kernel
  if (is.null(result_kernel) || !identical(result_kernel$tag, "materialize")) {
    return(NULL)
  }
  result_kernel$producer$elem %||% NULL
}

tccq_allocation_result_reuse_plan <- function(module) {
  expr <- tccq_allocation_result_expr(module)
  if (is.null(expr) || is.null(expr$type) || expr$type$rank != 1L) {
    return(NULL)
  }
  if (expr$tag %in% c("var", "boundary_call", "slice_range", "view1")) {
    return(NULL)
  }

  vector_vars <- tccq_ir_vector_vars(expr)
  if (length(vector_vars) != 1L) {
    return(NULL)
  }

  name <- vector_vars[[1L]]
  bindings <- module$storage_plan$bindings %||% list()
  binding <- bindings[[name]] %||% NULL
  if (is.null(binding) || !identical(binding$kind, "owned")) {
    return(NULL)
  }
  if (!identical(binding$type$mode, expr$type$mode) || binding$type$rank != 1L) {
    return(NULL)
  }
  if (!tccq_allocation_expr_pointwise_over(expr, name)) {
    return(NULL)
  }

  list(
    strategy = "reuse_owned_local_result",
    name = name,
    mode = expr$type$mode,
    reason = "pointwise_final_expression"
  )
}

tccq_allocation_output_plan <- function(kernel) {
  switch(
    kernel$tag,
    scalar_kernel = list(kind = "scalar_result", protect = TRUE),
    materialize = list(kind = "vector_result", protect = TRUE),
    fold = list(kind = "scalar_result", protect = TRUE),
    kernel_program = list(kind = "program_result", protect = TRUE),
    tccq_abort("unknown kernel tag for allocation plan: ", kernel$tag)
  )
}

tccq_allocation_plan <- function(module) {
  kernel <- module$kernel
  list(
    output = tccq_allocation_output_plan(kernel),
    temporaries = list(),
    reuse = list(result_buffer = tccq_allocation_result_reuse_plan(module))
  )
}
