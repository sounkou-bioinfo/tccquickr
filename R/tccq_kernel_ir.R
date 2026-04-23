# tccq_kernel_ir.R - explicit kernel IR for producer/fold/materialize nodes
# SPDX-License-Identifier: GPL-3.0-or-later

tccq_ir_domain <- function(vars, type = tccq_type_scalar("integer")) {
  vars <- tccq_unique(as.character(vars))
  tccq_node("domain", vars = vars, type = type)
}

tccq_ir_producer <- function(domain, elem, type = elem$type) {
  tccq_node("producer", domain = domain, elem = elem, type = type)
}

tccq_ir_materialize <- function(producer, type = producer$type) {
  tccq_node("materialize", producer = producer, type = type)
}

tccq_ir_fold <- function(op, domain, elem, init = NULL, type = tccq_type_scalar("double"), surface = op) {
  tccq_node("fold", op = op, domain = domain, elem = elem, init = init, surface = surface, type = type)
}

tccq_ir_scalar_kernel <- function(expr) {
  tccq_node("scalar_kernel", expr = expr, type = expr$type)
}

tccq_kernel_domain_from_expr <- function(expr) {
  vars <- tccq_ir_vector_vars(expr)
  tccq_ir_domain(vars)
}
