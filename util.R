# Utilities for working with Census data
#
# Contact: Edgar Castro <edgar_castro@g.harvard.edu>

library(data.table)
library(pbapply)
library(tidycensus)
library(tools)

# Setup -------------------------------------------------------------------

# Path to save tidycensus results to
TIDYCENSUS_CACHE <- "cache/tidycensus"

# Path where the Census API key will be loaded from
API_KEY_FILE <- "api_key.txt"

# Geographies that do not require a state to be specified
GEOGRAPHIES_NO_STATE <- c(
  "us", "region", "state", "county", "division", "zip code tabulation area", "zcta"
)

# FIPS codes for the 50 states + DC
STATES_DC_FIPS <- c(
  "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16",
  "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
  "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
)

if (file.exists(API_KEY_FILE)) {
  suppressMessages(census_api_key(readLines(API_KEY_FILE)))
} else {
  cat("Paste your API key here", file = API_KEY_FILE)
  stop(sprintf("Please paste your Census API key into %s", API_KEY_FILE))
}

pboptions(type = "timer")

# Retrieval functions -----------------------------------------------------

lhs <- function(f) {
  return(f[[2]])
}

rhs <- function(f) {
  return(f[[3]])
}

rhs_variables <- function(f) {
  return(all.vars(rhs(f)))
}

# Retrieve a list of available variables from the Census API via tidycensus
get_variables_cached <- function(year, dataset) {
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
                                  check_variables = TRUE
                                  ) { 
  cache_directory <- file.path(TIDYCENSUS_CACHE, dataset, year, geography)
  dir.create(cache_directory, showWarnings = FALSE, recursive = TRUE)
  
  if (check_variables) {
    message("Checking variables")
    all_variables <- get_variables_cached(year, dataset)[["name"]]
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
  if (dataset == "acs5") {
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
      new_data <- dcast(
        as.data.table(suppressMessages(fetch_function(
            geography, variables_to_fetch, year = year
        ))),
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

# census_fetch_function should be a function returning a data.frame-like object
# with the GEOIDs in the first column and all other specified variables in other
# columns
get_census <- function(geography, year, dataset, formulas,
                       census_fetch_function = get_tidycensus_cached,
                       states = STATES_DC_FIPS, ...) {
  needed_variables <- unique(unlist(lapply(formulas, rhs_variables)))
  
  data <- census_fetch_function(geography, year, dataset, needed_variables, ...)
  
  result <- data.frame(GEOID = data[["GEOID"]])
  
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
explain <- function(year, dataset, formula, width = NULL, delimiter = "$") {
  variables <- get_variables_cached(year, dataset)
  result <- format(formula, width = width)
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
