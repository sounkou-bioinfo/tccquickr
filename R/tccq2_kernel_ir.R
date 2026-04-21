# tccq2_kernel_ir.R - explicit kernel IR for producer/fold/materialize nodes
# SPDX-License-Identifier: GPL-3.0-or-later

tccq2_ir_domain <- function(vars, type = tccq2_type_scalar("integer")) {
  vars <- tccq2_unique(as.character(vars))
  tccq2_node("domain", vars = vars, type = type)
}

tccq2_ir_producer <- function(domain, elem, type = elem$type) {
  tccq2_node("producer", domain = domain, elem = elem, type = type)
}

tccq2_ir_materialize <- function(producer, type = producer$type) {
  tccq2_node("materialize", producer = producer, type = type)
}

tccq2_ir_fold <- function(op, domain, elem, init = NULL, type = tccq2_type_scalar("double")) {
  tccq2_node("fold", op = op, domain = domain, elem = elem, init = init, type = type)
}

tccq2_ir_scalar_kernel <- function(expr) {
  tccq2_node("scalar_kernel", expr = expr, type = expr$type)
}

tccq2_kernel_domain_from_expr <- function(expr) {
  vars <- tccq2_ir_vector_vars(expr)
  tccq2_ir_domain(vars)
}
