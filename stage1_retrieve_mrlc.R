#!/usr/bin/env Rscript
#
# Download the main Census geographies from TIGER/Line

library(pbapply)

source("util.R")

# Large files may timeout
options(timeout = 60 * 20)

MRLC_ZIPS = list(
  landcover = c(
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_land_cover_l48_20210604.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2011_land_cover_l48_20210604.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_land_cover_l48_20210604.zip"
  ),
  imperviousness = c(
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_impervious_l48_20210604.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2011_impervious_l48_20210604.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_impervious_l48_20210604.zip"
  )
)

MRLC_DIR <- "external/mrlc/"

FINISHED_MARKER_NAME = "complete"

dir.create(MRLC_DIR, showWarnings = FALSE, recursive = TRUE)

for (dataset in names(MRLC_ZIPS)) {
  output_directory <- file.path(MRLC_DIR, dataset)
  finished_marker <- file.path(output_directory, FINISHED_MARKER_NAME)
  if (file.exists(finished_marker)) {
    message(sprintf("Skipping %s", dataset))
  } else {
    message(sprintf("Downloading %s", dataset))
    for (url in MRLC_ZIPS[[dataset]]) {
      download_zip(url, output_directory, temp_file = "temp_mrlc.zip")
    }
    file.create(finished_marker)
  }
}
