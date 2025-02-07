#!/usr/bin/env Rscript
#
# Download documentation for Census surveys

DOCUMENTATION_DIR <- "documentation"

sources <- read.csv("external/documentation-sources.csv")

apply(
  sources,
  1,
  function(row) {
    directory <- file.path(DOCUMENTATION_DIR, row["DESTINATION"])
    output_path <- file.path(directory, basename(row["SOURCE"]))
    if (file.exists(output_path)) return()
    dir.create(directory, recursive = TRUE, showWarnings = FALSE)
    download.file(row["SOURCE"], output_path)
  }
)
