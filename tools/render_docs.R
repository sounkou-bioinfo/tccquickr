#!/usr/bin/env Rscript
# Render every docs/**/*.Rmd to a github_document .md.
#
# Lean chunks are checked live via the leanknit engine against proofs/, so a
# document cannot render while claiming a proof that does not check
# (docs/decisions/0005-conformance-and-verification.md). Run from the repo root:
#
#   Rscript tools/render_docs.R            # render all
#   Rscript tools/render_docs.R docs/x.Rmd # render specific files

root <- normalizePath(getwd())
proofs <- file.path(root, "proofs")
if (requireNamespace("leanknit", quietly = TRUE) && dir.exists(proofs)) {
  library(leanknit)
  options(leanknit.project = proofs)
}

args <- commandArgs(trailingOnly = TRUE)
rmds <- if (length(args)) {
  args
} else {
  list.files("docs", pattern = "[.]Rmd$", recursive = TRUE, full.names = TRUE)
}

for (rmd in rmds) {
  message("Rendering ", rmd)
  rmarkdown::render(rmd, output_format = "github_document", quiet = TRUE)
}
message("Rendered ", length(rmds), " document(s).")
