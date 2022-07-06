#!/usr/bin/env Rscript
#
# Download the main Census geographies from TIGER/Line

library(httr)
library(pbapply)
library(xml2)

source("util.R")

# The bigger files (e.g. ZCTA5) take more than 60 seconds to download
options(timeout = 60 * 10)

TIGER_DOWNLOAD_TEMPLATES = list(
  
  # Hierarchical
  state = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2010/STATE/2000/tl_2010_us_state00.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/STATE/2010/tl_2010_us_state10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/STATE/tl_2020_us_state.zip"
  ),
  county = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2010/COUNTY/2000/tl_2010_us_county00.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/COUNTY/2010/tl_2010_us_county10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/COUNTY/tl_2020_us_county.zip"
  ),
  tract = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2010/TRACT/2000/tl_2010_{STATE_FIPS}_tract00.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/TRACT/2010/tl_2010_{STATE_FIPS}_tract10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/TRACT/tl_2020_{STATE_FIPS}_tract.zip"
  ),
  block_group = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2010/BG/2000/tl_2010_{STATE_FIPS}_bg00.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/BG/2010/tl_2010_{STATE_FIPS}_bg10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/BG/tl_2020_{STATE_FIPS}_bg.zip"
  ),
  block = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/2000/tl_2010_{STATE_FIPS}_tabblock00.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/2010/tl_2010_{STATE_FIPS}_tabblock10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_{STATE_FIPS}_tabblock20.zip"
  ),
  
  # Misc
  zcta5 = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5/2000/tl_2010_us_zcta500.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/ZCTA5/2010/tl_2010_us_zcta510.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/ZCTA520/tl_2020_us_zcta520.zip"
  ),
  puma = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2008/{STATE_FIPS}_{STATE_FULL_CAPS}/tl_2008_{STATE_FIPS}_puma500.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/PUMA5/2010/tl_2010_01_puma10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/PUMA/tl_2020_01_puma10.zip"
  ),
  
  # Multi-state
  uac = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2008/tl_2008_us_uac.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/UA/2010/tl_2010_us_uac10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/UAC/tl_2020_us_uac10.zip"
  ),
  csa = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2008/tl_2008_us_csa.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/CSA/2010/tl_2010_us_csa10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/CSA/tl_2020_us_csa.zip"
  ),
  cbsa = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2008/tl_2008_us_cbsa.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/CBSA/2010/tl_2010_us_cbsa10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/CBSA/tl_2020_us_cbsa.zip"
  ),
  metdiv = c(
    "2000" = "https://www2.census.gov/geo/tiger/TIGER2008/tl_2008_us_metdiv.zip",
    "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/METDIV/2010/tl_2010_us_metdiv10.zip",
    "2020" = "https://www2.census.gov/geo/tiger/TIGER2020/METDIV/tl_2020_us_metdiv.zip"
  ),
  
  # Roads
  roads = c(
      "2010" = "https://www2.census.gov/geo/tiger/TIGER2010/ROADS/*",
      "2010" = "https://www2.census.gov/geo/tiger/TIGER2020/ROADS/*"
  )
  
)

TIGER_DIR <- "external/tiger/"

FINISHED_MARKER_NAME = "complete"

dir.create(TIGER_DIR, showWarnings = FALSE, recursive = TRUE)

fill_tiger_template <- function(template) {
  
  # Fill in {STATE_FIPS}, {STATE_FULL_CAPS} with FIPS codes or state names
  if (any(sapply(
    c("{STATE_FIPS}", "{STATE_FULL_CAPS}"),
    grepl,
    template,
    fixed = TRUE
  ))) {
    return(sapply(
      STATES_DC_FIPS,
      function(fips_code) {
        state_full_caps <- gsub(
          " ",
          "_",
          toupper(
            subset(FIPS_DF, FIPS == as.numeric(fips_code))[["STATE"]][[1]]
          )
        )
        result <- gsub("{STATE_FIPS}", fips_code, template, fixed = TRUE)
        result <- gsub("{STATE_FULL_CAPS}", state_full_caps, result, fixed = TRUE)
        return(result)
      }
    ))
  }
  
  # Wildcard match - pull all files
  if (grepl("\\*$", template)) {
    root <- dirname(template)
    html <- content(GET(root), as = "parsed", encoding = "UTF-8")
    files <- sapply(
      xml_find_all(html, ".//table//tr//td//a"),
      xml_attr,
      "href"
    )
    files <- files[grepl(".zip$", files)]
    return(file.path(root, files))
  }
  
  return(template)
}

download_shp <- function(url, output_directory, temp_file = "temp.zip") {
  # message(sprintf("Downloading %s to %s", url, output_directory))
  if (file.exists(temp_file)) {
    file.remove(temp_file)
  }
  download.file(url, temp_file)
  unzip(temp_file, exdir = output_directory)
  file.remove(temp_file)
}

for (geography in names(TIGER_DOWNLOAD_TEMPLATES)) {
  years <- TIGER_DOWNLOAD_TEMPLATES[[geography]]
  for (year in names(years)) {
    output_directory <- file.path(TIGER_DIR, year, geography)
    finished_marker <- file.path(output_directory, FINISHED_MARKER_NAME)
    if (file.exists(finished_marker)) {
      message(sprintf("Skipping %s %s", year, geography))
    } else {
      message(sprintf("Downloading %s %s", year, geography))
      for (url in fill_tiger_template(years[[year]])) {
        download_shp(url, output_directory)
      }
      file.create(finished_marker)
    }
  }
}
