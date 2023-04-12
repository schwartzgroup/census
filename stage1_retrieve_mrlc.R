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
    "https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2001_Land_Cover_AK_20200724.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/HI_landcover_wimperv_9-30-08_se5.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/PR_landcover_wimperv_10-28-08_se5.zip",

    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2011_land_cover_l48_20210604.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2011_Land_Cover_AK_20200724.zip"
  ),
  imperviousness = c(
    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2001_impervious_l48_20210604.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2001_Impervious_AK_20200724.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/HI_masked_imperv_9-30-08.zip",
    "https://s3-us-west-2.amazonaws.com/mrlc/PR_masked_imperv_10-25-08.zip",

    "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2011_impervious_l48_20210604.zip"
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
