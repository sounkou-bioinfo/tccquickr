# tccq2_pass.R - tiny pass manager and first middle-end passes
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_pass <- function(name, run, requires = character(), provides = character()) {
  structure(
    list(name = name, run = run, requires = requires, provides = provides),
    class = "tccq2_pass"
  )
}

tccq2_run_passes <- function(module, passes = tccq2_default_passes()) {
  tccq2_module_validate(module)
  facts <- module$facts %||% list()

  for (pass in passes) {
    missing <- setdiff(pass$requires, names(facts))
    if (length(missing)) {
      tccq2_abort(
        "pass '", pass$name, "' missing required facts: ",
        paste(missing, collapse = ", ")
      )
    }

    result <- pass$run(module, facts)
    if (!inherits(result, "tccq2_module")) {
      tccq2_abort("pass '", pass$name, "' did not return a tccq2_module")
    }
    module <- result

    for (provided in pass$provides) {
      facts[[provided]] <- TRUE
    }
    module$facts <- facts
  }

  module
}

tccq2_default_passes <- function() {
  list(
    tccq2_pass_validate_ir(),
    tccq2_pass_effects(),
    tccq2_pass_kernelize(),
    tccq2_pass_fusion(),
    tccq2_pass_legality_barriers(),
    tccq2_pass_allocation_plan(),
    tccq2_pass_protect_plan()
  )
}

tccq2_pass_validate_ir <- function() {
  tccq2_pass(
    name = "validate_ir",
    provides = "ir_valid",
    run = function(module, facts) {
      if (is.null(module$ir)) {
        tccq2_abort("validate_ir requires module$ir")
      }
      tccq2_ir_validate(module$ir)
      module
    }
  )
}

tccq2_pass_effects <- function() {
  tccq2_pass(
    name = "effects",
    requires = "ir_valid",
    provides = "effects_known",
    run = function(module, facts) {
      has_impure <- FALSE
      tccq2_ir_walk(module$ir, function(n) {
        effect <- n$effect %||% "pure"
        if (!effect %in% c("pure", "boundary")) {
          has_impure <<- TRUE
        }
      })
      if (has_impure) {
        tccq2_abort("fresh compiler milestone 2 does not accept impure IR")
      }
      module
    }
  )
}

tccq2_pass_kernelize <- function() {
  tccq2_pass(
    name = "kernelize",
    requires = c("ir_valid", "effects_known"),
    provides = "kernel_ir",
    run = function(module, facts) {
      ir <- module$ir
      kernel <- if (identical(ir$tag, "reduce")) {
        domain <- tccq2_kernel_domain_from_expr(ir$x)
        producer <- tccq2_ir_producer(domain = domain, elem = ir$x, type = ir$x$type)
        tccq2_ir_fold(
          op = ir$op,
          domain = domain,
          elem = tccq2_ir_materialize(producer = producer, type = ir$x$type),
          init = 0.0,
          type = ir$type
        )
      } else if (ir$type$rank > 0L) {
        domain <- tccq2_kernel_domain_from_expr(ir)
        producer <- tccq2_ir_producer(domain = domain, elem = ir, type = ir$type)
        tccq2_ir_materialize(producer = producer, type = ir$type)
      } else {
        tccq2_ir_scalar_kernel(ir)
      }
      tccq2_module_with(module, kernel = kernel)
    }
  )
}

tccq2_pass_fusion <- function() {
  tccq2_pass(
    name = "fusion",
    requires = "kernel_ir",
    provides = "fusion_done",
    run = function(module, facts) {
      tccq2_module_with(module, kernel = tccq2_fuse_kernel(module$kernel))
    }
  )
}

tccq2_pass_legality_barriers <- function() {
  tccq2_pass(
    name = "legality_barriers",
    requires = "fusion_done",
    provides = "legality_checked",
    run = function(module, facts) {
      if (tccq2_kernel_has_boundary(module$kernel)) {
        tccq2_abort(
          "boundary nodes are explicit legality barriers in tccq2; ",
          "later milestones may lower them, but milestone 2 refuses to compile them"
        )
      }
      module
    }
  )
}

tccq2_pass_allocation_plan <- function() {
  tccq2_pass(
    name = "allocation_plan",
    requires = "legality_checked",
    provides = "alloc_plan",
    run = function(module, facts) {
      kernel <- module$kernel
      output <- switch(
        kernel$tag,
        scalar_kernel = list(kind = "scalar_result", protect = TRUE),
        materialize = list(kind = "vector_result", protect = TRUE),
        fold = list(kind = "scalar_result", protect = TRUE),
        tccq2_abort("unknown kernel tag for allocation plan: ", kernel$tag)
      )
      tccq2_module_with(
        module,
        alloc_plan = list(output = output, temporaries = list(), reuse = list())
      )
    }
  )
}

tccq2_pass_protect_plan <- function() {
  tccq2_pass(
    name = "protect_plan",
    requires = "alloc_plan",
    provides = "protect_plan",
    run = function(module, facts) {
      # Milestone 1 has one allocated return SEXP and no materialized temps.
      tccq2_module_with(module, protect_plan = list(n_protect = 1L))
    }
  )
}
