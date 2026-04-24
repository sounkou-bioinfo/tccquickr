# tccq_pass.R - tiny pass manager and first middle-end passes
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_pass <- function(name, run, requires = character(), provides = character()) {
  structure(
    list(name = name, run = run, requires = requires, provides = provides),
    class = "tccq_pass"
  )
}

tccq_run_passes <- function(module, passes = tccq_default_passes()) {
  tccq_module_validate(module)
  facts <- module$facts %||% list()

  for (pass in passes) {
    missing <- setdiff(pass$requires, names(facts))
    if (length(missing)) {
      tccq_abort(
        "pass '", pass$name, "' missing required facts: ",
        paste(missing, collapse = ", ")
      )
    }

    result <- pass$run(module, facts)
    if (!inherits(result, "tccq_module")) {
      tccq_abort("pass '", pass$name, "' did not return a tccq_module")
    }
    module <- result

    for (provided in pass$provides) {
      facts[[provided]] <- TRUE
    }
    module$facts <- facts
  }

  module
}

tccq_default_passes <- function() {
  list(
    tccq_pass_validate_ir(),
    tccq_pass_effects(),
    tccq_pass_index_normalize(),
    tccq_pass_shape_domains(),
    tccq_pass_kernelize(),
    tccq_pass_fusion(),
    tccq_pass_boundary_collect(),
    tccq_pass_boundary_validate(),
    tccq_pass_storage_plan(),
    tccq_pass_allocation_plan(),
    tccq_pass_protect_plan()
  )
}

tccq_pass_validate_ir <- function() {
  tccq_pass(
    name = "validate_ir",
    provides = "ir_valid",
    run = function(module, facts) {
      if (is.null(module$ir)) {
        tccq_abort("validate_ir requires module$ir")
      }
      tccq_ir_validate(module$ir)
      module
    }
  )
}

tccq_pass_effects <- function() {
  tccq_pass(
    name = "effects",
    requires = "ir_valid",
    provides = "effects_known",
    run = function(module, facts) {
      tccq_ir_walk(module$ir, function(n) {
        eff <- n$effect %||% "pure"
        if (identical(eff, "pure")) {
          return(invisible(NULL))
        }
        if (identical(eff, "write") && n$tag %in% c("program", "kernel_program", "store_index", "store_range")) {
          return(invisible(NULL))
        }
        if (eff %in% c("boundary", "r_eval")) {
          return(invisible(NULL))
        }
        tccq_abort("unsupported effect in tccq: ", eff, " on node ", n$tag)
      })
      module
    }
  )
}

tccq_pass_index_normalize <- function() {
  tccq_pass(
    name = "index_normalize",
    requires = c("ir_valid", "effects_known"),
    provides = "index_normalized",
    run = function(module, facts) {
      ir <- tccq_ir_attach_normalized_access(module$ir)
      ir <- tccq_ir_hoist_access_program(ir, reserved_names = module$formal_names)
      ir <- tccq_ir_attach_normalized_access(ir)
      tccq_module_with(module, ir = ir)
    }
  )
}

tccq_pass_shape_domains <- function() {
  tccq_pass(
    name = "shape_domains",
    requires = c("ir_valid", "effects_known", "index_normalized"),
    provides = "shape_domains_known",
    run = function(module, facts) {
      analysis <- tccq_shape_analyze_module(module)
      tccq_module_with(module, ir = analysis$ir, shape_facts = analysis$shape_facts)
    }
  )
}

tccq_pass_kernelize <- function() {
  tccq_pass(
    name = "kernelize",
    requires = c("ir_valid", "effects_known", "index_normalized", "shape_domains_known"),
    provides = "kernel_ir",
    run = function(module, facts) {
      ir <- module$ir

      make_kernel <- function(expr) {
        if (identical(expr$tag, "reduce")) {
          domain <- tccq_kernel_domain_from_expr(expr$x, module = module)
          producer <- tccq_ir_producer(domain = domain, elem = expr$x, type = expr$x$type)
          tccq_ir_fold(
            op = expr$op,
            domain = domain,
            elem = tccq_ir_materialize(producer = producer, type = expr$x$type),
            init = 0.0,
            type = expr$type,
            surface = expr$surface %||% expr$op
          )
        } else if (expr$type$rank > 0L) {
          domain <- tccq_kernel_domain_from_expr(expr, module = module)
          producer <- tccq_ir_producer(domain = domain, elem = expr, type = expr$type)
          tccq_ir_materialize(producer = producer, type = expr$type)
        } else {
          tccq_ir_scalar_kernel(expr)
        }
      }

      kernel <- if (identical(ir$tag, "program") && length(ir$stmts)) {
        tccq_ir_kernel_program(ir$stmts, make_kernel(ir$result))
      } else if (identical(ir$tag, "program")) {
        make_kernel(ir$result)
      } else {
        make_kernel(ir)
      }

      tccq_module_with(module, kernel = kernel)
    }
  )
}

tccq_pass_fusion <- function() {
  tccq_pass(
    name = "fusion",
    requires = "kernel_ir",
    provides = "fusion_done",
    run = function(module, facts) {
      kernel <- module$kernel
      if (identical(kernel$tag, "kernel_program")) {
        kernel$result_kernel <- tccq_fuse_kernel(kernel$result_kernel, module = module)
      } else {
        kernel <- tccq_fuse_kernel(kernel, module = module)
      }
      tccq_module_with(module, kernel = kernel)
    }
  )
}

tccq_pass_boundary_collect <- function() {
  tccq_pass(
    name = "boundary_collect",
    requires = "fusion_done",
    provides = "boundaries_collected",
    run = function(module, facts) {
      ir <- tccq_boundary_annotate_ids(module$ir)
      kernel <- tccq_boundary_annotate_ids(module$kernel)
      tccq_module_with(module, ir = ir, kernel = kernel, boundary_context = tccq_boundary_context(kernel))
    }
  )
}

tccq_pass_boundary_validate <- function() {
  tccq_pass(
    name = "boundary_validate",
    requires = "boundaries_collected",
    provides = "legality_checked",
    run = function(module, facts) {
      boundaries <- tccq_boundary_nodes(module$kernel)
      fallback <- module$fallback %||% "hard"
      for (node in boundaries) {
        if (!isTRUE(tccq_boundary_supported(node, fallback = fallback))) {
          tccq_abort(
            "boundary node '", node$api, ":", node$name,
            "' is not supported in fallback = '", fallback, "'"
          )
        }
      }
      module
    }
  )
}

tccq_pass_storage_plan <- function() {
  tccq_pass(
    name = "storage_plan",
    requires = c("legality_checked", "shape_domains_known"),
    provides = "storage_plan",
    run = function(module, facts) {
      tccq_module_with(module, storage_plan = tccq_storage_plan(module))
    }
  )
}

tccq_pass_allocation_plan <- function() {
  tccq_pass(
    name = "allocation_plan",
    requires = c("legality_checked", "storage_plan"),
    provides = "alloc_plan",
    run = function(module, facts) {
      tccq_module_with(module, alloc_plan = tccq_allocation_plan(module))
    }
  )
}

tccq_pass_protect_plan <- function() {
  tccq_pass(
    name = "protect_plan",
    requires = "alloc_plan",
    provides = "protect_plan",
    run = function(module, facts) {
      tccq_module_with(module, protect_plan = list(n_protect = 1L))
    }
  )
}
