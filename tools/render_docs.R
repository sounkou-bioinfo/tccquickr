#!/usr/bin/env Rscript
# Render every docs/**/*.Rmd to a github_document .md.
#
#   Rscript tools/render_docs.R            # render all
#   Rscript tools/render_docs.R docs/x.Rmd # render specific files

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
