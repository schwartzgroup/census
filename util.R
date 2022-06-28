library(data.table)
library(pbapply)
library(tidycensus)
library(tidyr)

# Setup -------------------------------------------------------------------

TIDYCENSUS_CACHE <- "cache/tidycensus"

API_KEY_FILE <- "api_key.txt"

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

# Functions ---------------------------------------------------------------

lhs <- function(f) {
  return(f[[2]])
}

rhs <- function(f) {
  return(f[[3]])
}

rhs_variables <- function(f) {
  return(all.vars(rhs(f)))
}

get_variables_cached <- function(year, dataset) {
  cache_directory <- file.path(TIDYCENSUS_CACHE, dataset, year)
  dir.create(cache_directory, showWarnings = FALSE, recursive = TRUE)
  
  variables_file <- file.path(cache_directory, "variables.csv.gz")
  if (file.exists(variables_file)) {
    return(fread(variables_file))
  } else {
    result <- load_variables(year, dataset)
    fwrite(result, variables_file)
  }
}

# Retrieve data from the Census API via tidycensus, caching tables
get_tidycensus_cached <- function(geography, year, dataset, needed_variables,
                                  states = STATES_DC_FIPS) { # TODO: check if states= agrees with the last run
  cache_directory <- file.path(TIDYCENSUS_CACHE, dataset, year, geography)
  dir.create(cache_directory, showWarnings = FALSE, recursive = TRUE)
  
  message("Checking variables")
  all_variables <- get_variables_cached(year, dataset)[["name"]]
  missing_variables <- setdiff(needed_variables, all_variables)
  if (length(missing_variables > 0)) {
    stop(sprintf(
      "Unavailable variables: %s",
      paste(missing_variables, collapse = ", ")
    ))
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
    fetch_function <- get_acs
  } else {
    fetch_function <- get_decennial
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
      value.var = "estimate"
    )
    
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

get_census <- function(geography, year, dataset, formulas,
                       census_fetch_function = get_tidycensus_cached,
                       states = STATES_DC_FIPS) {
  needed_variables <- unique(unlist(lapply(formulas, rhs_variables)))
  
  data <- census_fetch_function(geography, year, dataset, needed_variables)
  
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

get_census(
  geography = "block group",
  dataset = "acs5",
  year = 2019,
  formulas = c(
    population ~ B01001_001,
    pct_female ~ B01001_026 / B01001_001,
    pct_white ~ B02001_002 / B01001_001,
    pct_black ~ B02001_003 / B01001_001,
    pct_native ~ B02001_004 / B01001_001,
    pct_asian ~ B02001_005 / B01001_001,
    pct_two_or_more_races ~ B02001_008 / B01001_001,
    pct_hispanic_white ~ B03002_013 / B01001_001,
    pct_hispanic_black ~ B03002_014 / B01001_001,
    pct_hispanic_native ~ B03002_015 / B01001_001,
    pct_hispanic_asian ~ B03002_016 / B01001_001,
    pct_hispanic_two_or_more_races ~ B03002_019 / B01001_001,
    pct_non_hispanic_white ~ B03002_003 / B01001_001,
    pct_non_hispanic_black ~ B03002_004 / B01001_001,
    pct_non_hispanic_native ~ B03002_005 / B01001_001,
    pct_non_hispanic_asian ~ B03002_006 / B01001_001,
    pct_non_hispanic_two_or_more_races ~ B03002_009 / B01001_001,
    pct_hispanic ~ B03002_012 / B01001_001,
    pct_foreign_born ~ B05006_001 / B01001_001,
    
    # Compatibility with 1990 Decennial Census
    pct_pacific_islander ~ (B02001_005 + B02001_006) / B01001_001,
    pct_hispanic_pacific_islander ~ (B03002_016 + B03002_017) / B01001_001,
    pct_non_hispanic_pacific_islander ~ (B03002_006 + B03002_007) / B01001_001
  )
)

get_census(
  geography = "block group",
  dataset = "acs5",
  year = 2019,
  formulas = c(
    n_households ~ B11001_001,
    mean_household_size ~ B25010_001,
    pct_households_single_father ~ B11001_005 / B11001_001,
    pct_households_single_mother ~ B11001_006 / B11001_001,
    pct_public_assistance ~ B19057_002 / B11001_001
  )
)

get_census(
  geography = "block group",
  dataset = "acs5",
  year = 2020,
  formulas = c(
    n_occupied_housing_units ~ B25003_001,
    pct_renting ~ B25003_003 / B25003_001,
    pct_heating_utility_gas ~ B25040_002 / B25003_001,
    pct_heating_gas_tank ~ B25040_003 / B25003_001,
    pct_heating_electricity ~ B25040_004 / B25003_001,
    pct_heating_oil ~ B25040_005 / B25003_001,
    pct_heating_coal ~ B25040_006 / B25003_001,
    pct_heating_wood ~ B25040_007 / B25003_001,
    pct_heating_solar ~ B25040_008 / B25003_001,
    pct_heating_other ~ B25040_009 / B25003_001,
    pct_heating_none ~ B25040_001 / B25003_001
  )
)
