# Utilities for working with Census data
#
# Contact: Edgar Castro <edgar_castro@g.harvard.edu>

library(data.table)
library(pbapply)
library(tidycensus)
library(totalcensus)
library(tools)

# Setup -------------------------------------------------------------------

# Path to save tidycensus results to
TIDYCENSUS_CACHE <- "cache/tidycensus"

# Path to save totalcensus results to
TOTALCENSUS_CACHE <- "cache/totalcensus"

# Path where the Census API key will be loaded from
API_KEY_FILE <- "api_key.txt"

# Geographies that do not require a state to be specified
GEOGRAPHIES_NO_STATE <- c(
  "us", "region", "state", "county", "division", "place",
  "combined statistical area", "congressional district", "urban area",
  "public use microdata area", "zip code tabulation area", "zcta"
)

MAIN_GEOGRAPHIES <- c(
  "state", "county", "tract", "block group", "block", "zip code tabulation area"
)

FIPS_DF <- read.csv("external/fips.csv")

# FIPS codes for the 50 states + DC - FIPS codes are sorted by first by states
# alphabetically, so we can subset the FIPS codes to those at or before Wyoming
# ("56")
STATES_DC_FIPS <- sprintf("%02d", subset(FIPS_DF, FIPS <= 56)[["FIPS"]])

if (file.exists(API_KEY_FILE)) {
  suppressMessages(census_api_key(readLines(API_KEY_FILE)))
} else {
  cat("Paste your API key here", file = API_KEY_FILE)
  stop(sprintf("Please paste your Census API key into %s", API_KEY_FILE))
}

pboptions(type = "timer")

Sys.setenv(PATH_TO_CENSUS = TOTALCENSUS_CACHE)

# Functions for Stage 1 ---------------------------------------------------

### Formula manipulation ----

lhs <- function(f) {
  return(f[[2]])
}

rhs <- function(f) {
  return(f[[3]])
}

rhs_variables <- function(f) {
  return(all.vars(rhs(f)))
}

## Driver templates ----

.get_variables <- function(year, dataset) {
  # Return a data.frame-like object of all variables available in the given
  # year-dataset combination with the following columns:
  #
  # * name: character of variable names (e.g. "B01001_001")
  # * label: character describing the variable (e.g. "Estimate!!Total:")
  # * concept: character describing the variable category (e.g. "SEX BY AGE")
  #
  # These do not necessarily have to be formatted as in the above examples; the
  # explain(...) function will handle formatting by stripping punctuation and
  # standardizing the letter case.
}

.get <- function(geography,
                 year,
                 dataset, # "acs5" / "sf1" / "sf3"
                 needed_variables,
                 states = STATES_DC_FIPS, # TODO: check if states= agrees with the last run
                 check_variables = TRUE,
                 ...
                 ) { 
  # Return a data.frame-like object for all needed_variables in a given
  # geography-year-dataset combination, within the given states. If
  # check_variables is TRUE, first check needed_variables against
  # .get_variables(...) and raise an error if not all variables are available.
  #
  # The output should have a column of GEOIDs and all other columns should be
  # the needed_variables.
}

## tidycensus drivers ----

# Retrieve a list of available variables from the Census API via tidycensus
get_tidycensus_variables_cached <- function(year, dataset) {
  cache_directory <- file.path(TIDYCENSUS_CACHE, dataset, year)
  dir.create(cache_directory, showWarnings = FALSE, recursive = TRUE)
  
  variables_file <- file.path(cache_directory, "variables.csv.gz")
  if (file.exists(variables_file)) {
    return(fread(variables_file))
  } else {
    result <- load_variables(year, dataset)
    fwrite(result, variables_file)
    return(result)
  }
}

# Retrieve data from the Census API via tidycensus
get_tidycensus_cached <- function(geography,
                                  year,
                                  dataset, # "acs5" / "sf1" / "sf3"
                                  needed_variables,
                                  states = STATES_DC_FIPS, # TODO: check if states= agrees with the last run
                                  check_variables = TRUE,
                                  ...
                                  ) { 
  cache_directory <- file.path(TIDYCENSUS_CACHE, dataset, year, geography)
  dir.create(cache_directory, showWarnings = FALSE, recursive = TRUE)
  
  if (check_variables) {
    message("Checking variables")
    all_variables <- get_tidycensus_variables_cached(year, dataset)[["name"]]
    missing_variables <- setdiff(needed_variables, all_variables)
    if (length(missing_variables > 0)) {
      stop(sprintf(
        "Unavailable variables: %s",
        paste(missing_variables, collapse = ", ")
      ))
    }
  }
  
  # List of cached variables, minus GEOID
  cached_variables <- intersect(
    needed_variables,
    setdiff(
      gsub(".csv.gz", "", list.files(cache_directory)),
      "GEOID"
    )
  )
  
  # Variables that need to be fetched
  variables_to_fetch <- setdiff(needed_variables, cached_variables)
  
  # Function used for fetching
  if (dataset %in% c("acs5", "acs3", "acs1")) {
    fetch_function <- function(...) {
      return(get_acs(..., survey = dataset))
    }
    value_column <- "estimate"
  } else {
    fetch_function <- get_decennial
    fetch_function <- function(...) {
      return(get_decennial(..., sumfile = dataset))
    }
    value_column <- "value"
  }
  
  message(sprintf(
    "Existing variables: %s/%s (%0.2f%%)\nNeed to fetch %s",
    length(cached_variables),
    length(needed_variables),
    length(cached_variables) / length(needed_variables) * 2,
    length(variables_to_fetch)
  ))
  
  # Retrieve cached data
  if (length(cached_variables) > 0) {
    message("Loading cached data")
    cached_data <- pblapply(
      c("GEOID", cached_variables),
      function(variable) {
        # Warnings arise here when "GEOID" is not a column that is present
        suppressWarnings(fread(
          file.path(cache_directory, sprintf("%s.csv.gz", variable)),
          colClasses = list(character = "GEOID")
        ))
      }
    )
    message("Binding columns")
    cached_data <- do.call(cbind, cached_data)
  }
    
  # Retrieve new data
  if (length(variables_to_fetch) > 0) {
    message("Fetching new data")
  
    if (geography %in% GEOGRAPHIES_NO_STATE) {
        new_data <- as.data.table(suppressMessages(fetch_function(
            geography, variables_to_fetch, year = year, ...
        )))
      if (geography %in% c("zip code tabulation area", "zcta")) {
        new_data[, GEOID := tstrsplit(NAME, " ")[[2]]]
      }
      new_data <- dcast(
        new_data,
        GEOID ~ variable,
        value.var = value_column
      )
    } else {
      new_data <- dcast(
        rbindlist(pblapply(
          states,
          function(state) {
            suppressMessages(fetch_function(
              geography, variables_to_fetch, year = year, state = state
            ))
          }
        )),
        GEOID ~ variable,
        value.var = value_column
      )
    }
    
    # Cache new data
    message("Caching new data")
    invisible(pblapply(
      names(new_data),
      function(variable) {
        cache_file <- file.path(cache_directory, sprintf("%s.csv.gz", variable))
        if (!file.exists(cache_file)) {
          fwrite(subset(new_data, select = variable), cache_file)
        }
      }
    ))
    
    if (length(cached_variables) > 0) {
      # Both new data and cached data: merge
      data <- new_data[cached_data, on = list(GEOID)]
    } else {
      # Only new data
      data <- new_data
    }
  } else {
    # Only cached data
    data <- cached_data
  }
  
  return(data)
}

## totalcensus drivers ----

get_totalcensus_variables <- function(year, dataset) {
  if (dataset %in% c("sf1", "sf3")) {
    warning(sprintf(
      "Translating dataset \"%s\" to \"dec\" for totalcensus compatibility",
      dataset
    ))
    dataset <- "dec"
  }
  result <- search_tablecontents(dataset, 2009, view = FALSE)
  available <- complete.cases(result)
  return(result[available
                ][, list(name = reference,
                         label = table_content,
                         concept = table_name)])
}

get_totalcensus <- function(geography,
                            year,
                            dataset, # "acs5" / "sf1" / "sf3"
                            needed_variables,
                            states = states_DC, # TODO: check if states= agrees with the last run
                            check_variables = TRUE,
                            ...
                            ) { 
  if (dataset %in% c("sf1", "sf3")) {
    warning(sprintf(
      "Translating dataset \"%s\" to \"dec\" for totalcensus compatibility",
      dataset
    ))
    dataset <- "dec"
  }
  
  if (dataset == "acs5") {
    read_function <- read_acs5year
  } else if (dataset == "acs1") {
    read_function <- read_acs1year
  } else if (dataset == "dec") {
    read_function <- read_decennial
  } else {
    stop(sprintf("Dataset %s not supported", dataset))
  }
  
  if (geography %in% c("zip code tabulation area", "zcta")) {
    geography <- "860"
    states <- "US"
  }
  
  if (check_variables) {
    message("Checking variables")
    all_variables <- get_totalcensus_variables(year, dataset)[["name"]]
    missing_variables <- setdiff(needed_variables, all_variables)
    if (length(missing_variables > 0)) {
      stop(sprintf(
        "Unavailable variables: %s",
        paste(missing_variables, collapse = ", ")
      ))
    }
  }
  
  result <- read_function(
    year = year,
    states = states,
    table_contents = needed_variables,
    summary_level = geography
  )
  
  result[["GEOID"]] <- gsub("^[0-9]+US", "", result[["GEOID"]])
  
  return(subset(result, select = c("GEOID", needed_variables)))
}

## User-facing functions ----

# census_fetch_function should be a function returning a data.frame-like object
# with the GEOIDs in the first column and all other specified variables in other
# columns
get_census <- function(geography,
                       year,
                       dataset,
                       formulas,
                       census_fetch_function = get_tidycensus_cached,
                       states = STATES_DC_FIPS, ...) {
  needed_variables <- unique(unlist(lapply(formulas, rhs_variables)))
  
  data <- census_fetch_function(geography, year, dataset, needed_variables, ...)
  
  result <- data.table(
    year = year,
    dataset = dataset,
    GEOID = data[["GEOID"]]
  )
  
  message("Evaluating formulas")
  invisible(pblapply(
    1:length(formulas),
    function(i) {
      f <- formulas[[i]]
      variable <- lhs(f)
      values <- with(data, eval(rhs(f)))
      result[[variable]] <<- values
    }
  ))
  
  return(result)
}

# Fill in variables from a formula using definitions from the Census
explain <- function(year,
                    dataset,
                    formula,
                    # width = NULL,
                    delimiter = "$",
                    get_variables = get_tidycensus_variables_cached) {
  variables <- get_variables(year, dataset)
  # result <- format(formula, width = width)
  result <- deparse1(formula)
  sapply(
    rhs_variables(formula),
    function(variable) {
      definition <- subset(variables, name == variable)
      label <- sprintf(
        "%s%s%s",
        toTitleCase(tolower(definition[["concept"]])),
        delimiter,
        definition[["label"]]
      )
      label <- gsub("Estimate!!", "", label)
      label <- gsub(":$", "", label)
      label <- gsub(":*!!", delimiter, label)
      label <- sprintf("[%s]", label)
      result <<- gsub(variable, label, result, fixed = TRUE)
    }
  )
  return(result)
}

## Data retrieval ----

download_zip <- function(url, output_directory, temp_file = "temp.zip") {
  # message(sprintf("Downloading %s to %s", url, output_directory))
  if (file.exists(temp_file)) {
    file.remove(temp_file)
  }
  download.file(url, temp_file)
  tryCatch(
    system2("unzip", args = c("-d", output_directory, temp_file)),
    error = function(error) {
      message(error)
      unzip(temp_file, exdir = output_directory)
    }
  )
  file.remove(temp_file)
}
