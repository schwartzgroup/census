#!/usr/bin/env Rscript
#
# General formula: \hat{y}_t = \sum_s{\frac{\left(\frac{A_st}{A_t}\right)z_t}{\sum_\tau\left(\frac{A_{s\tau}}{A_\tau}\right)}y_s}
# where:
#
# * s = The "source zone", i.e. the data set where the original geographies came
#       from, i.e. the 2000 Census.
# * t = The "target zone", i.e. the data set where the geographies that we want
#       to interpolate to came from, i.e. the 2010 Census.
# * \hat{y}_t = The estimated value of the given Census variable in the target
#       zone, i.e. the value according to 2010 Census geographies.
# * y_s = The value of the given Census variable in the source zone, i.e. the
#       original value in the original geographies (2000 Census).
# * z_t = The value of the given Census variable in the target zone, i.e. the
#       value in a 2010 Census geography.
# * z_\tau = Similar to z_t, but idexing target zone intersections with \tau.
# * A_st = The area of a single intersection between s and t, indexing source
#       zone intersections. There may be multiple, e.g. if a 2000 geography
#       intersects 3 different 2010 geographies.
# * A_s = The area of a geography from the source zone, i.e. a 2000 geography.
# * A_s\tau = Similar to A_st, but indexing target zone intersections instead.
#       There may be multiple, e.g. if a 2010 geography intersects 3 different
#       2000 geographies.
# * A_t = The area of a geography from the target zone, i.e. a 2010 geography.

library(data.table)
library(pbapply)
library(r2r) # r2r::hashmap(.) is used to greatly speed up lookups vs. subset(.)

# Hashmap factories -------------------------------------------------------

# Build a one-to-one hashmap from `data[, key_columns] to `data$value_column`.
# `key_columns` can either be one column or multiple columns. Each row of
# `data[, key_columns]` must be unique and map to exactly value from
# `data$value_column`; otherwise, only the last value from `data$value_column`
# is used for each `data[, key_columns]`.
#
# Examples:
#
#     mtcars$vehicle <- row.names(mtcars) # create key_column
#     m <- build_hashmap(mtcars, "vehicle", "mpg")
#     m[["Mazda RX4"]]
#
#     m <- build_hashmap(ChickWeight, c("Chick", "Time"), "weight")
#     m[[c(4, 2)]] # chick 4 at time 2
#
build_hashmap <- function(data, key_columns, value_column) {
  message(sprintf(
    "Building one-to-one hashmap from %s: (%s) -> %s",
    deparse(substitute(data)), paste(key_columns, collapse = ", "), value_column
  ))
  return(do.call(
    hashmap,
    pblapply(
      1:nrow(data),
      function(i) list(
        sapply(
          key_columns,
          function(key_column) data[[i, key_column]],
          USE.NAMES = FALSE
        ),
        data[[i, value_column]]
      )
    )
  ))
}

# Build a one-to-many hashmap from `data$key_column` to `data$value_column`. If
# a value in `data$key_column` appears more than once, then it will be mapped to
# a character vector containing each value in `data$value_column` that it
# appears with.
#
# Example:
#
#     m <- build_hashmap_one_to_many(chickwts, "feed", "weight")
#     m[["sunflower"]]
#     m[["soybean"]]
#
# Microbenchmark test:
# Unit: milliseconds
#                                                                                         expr      min       lq     mean   median       uq      max neval
#                    subset(intersections, get(target_id_name) == target_id)[[source_id_name]] 2.351056 2.648683 5.312952 2.836869 3.080089 70.47588   100
# unlist(intersections[get(target_id_name) == target_id, ..source_id_name], use.names = FALSE) 1.621920 1.845852 3.626208 1.942512 2.145664 95.51740   100
build_hashmap_one_to_many <- function(data, key_column, value_column) {
  message(sprintf(
    "Building one-to-many hashmap from %s: %s -> %s[]",
    deparse(substitute(data)), paste(key_column, collapse = ", "), value_column
  ))
  if (!inherits(data, "data.table")) data <- as.data.table(data)
  return(do.call(
    hashmap,
    pbapply(
      data[, list(x = list(.SD[[value_column]])), # every value_column for each key_column
                  by = key_column],
      1,
      unname # need to ensure the lists are unname
    )
  ))
}

# Calculate weights -------------------------------------------------------

calculate_tdw_weights <- function(source_geographies_path,
                                  target_geographies_path,
                                  intersections_path,
                                  source_id_name,
                                  target_id_name,
                                  area_metric,
                                  population_metric,
                                  weights_column_name) {
  message("Loading geographic information")
  source_geographies <- fread(
    source_geographies_path,
    colClasses = list(character = source_id_name)
  )
  target_geographies <- fread(
    target_geographies_path,
    colClasses = list(character = target_id_name)
  )
  intersections <- fread(
    intersections_path,
    colClasses = list(character = c(source_id_name, target_id_name))
  )
  
  message("Creating hash maps")
  
  # one-to-one hash maps
  target_areas_hashmap <- build_hashmap(
    target_geographies, target_id_name, area_metric
  )
  target_populations_hashmap <- build_hashmap(
    target_geographies, target_id_name, population_metric
  )
  intersection_areas_hashmap <- build_hashmap(
    intersections, c(source_id_name, target_id_name), area_metric
  )
  intersection_source_id_to_target_ids_hashmap <- build_hashmap_one_to_many(
    intersections, source_id_name, target_id_name
  )
  intersection_target_id_to_source_ids_hashmap <- build_hashmap_one_to_many(
    intersections, target_id_name, source_id_name
  )
  
  message("Calculating target-density weights")
  all_results <- rbindlist(pblapply(
    target_geographies[[target_id_name]],
    function(target_id) {
      source_ids <- intersection_target_id_to_source_ids_hashmap[[target_id]]
      if (is.null(source_ids)) return(NULL)
      
      A_t <- target_areas_hashmap[[target_id]]
      Z_t <- target_populations_hashmap[[target_id]]
      
      # t \mapsto \frac{\frac{A_st}{A_t}Z_t}{\sum_\tau\frac{A_{s\tau}}{A_\tau}Z_\tau}
      target_density_weights <- sapply(
        source_ids,
        function(source_id) {
          A_st <- intersection_areas_hashmap[[c(source_id, target_id)]]
          
          numerator <- A_st / A_t * Z_t
          
          denominator <- sum(unlist(sapply(
            intersection_source_id_to_target_ids_hashmap[[source_id]],
            function(target_id_2) {
              A_stau <- intersection_areas_hashmap[[c(source_id, target_id_2)]]
              A_tau <- target_areas_hashmap[[target_id_2]]
              Z_tau <- target_populations_hashmap[[target_id_2]]
              
              return(A_stau / A_tau * Z_tau)
            }
          )))
      
          return(numerator / denominator)
        }
      )
      
      result <- data.table(target_id, source_ids, target_density_weights)
      setnames(result, c(target_id_name, source_id_name, weights_column_name))
    }
  ))
    
  return(all_results)
}

x <- calculate_tdw_weights(
  source_geographies_path = "output/geography_attributes/2000_zcta5.csv.gz",
  source_id_name = "ZCTA5CE00",
  target_geographies_path = "output/geography_attributes/2010_zcta5.csv.gz",
  target_id_name = "ZCTA5CE10",
  intersections_path = "output/intersections/2000_zcta5_2010_zcta5.csv.gz",
  area_metric = "area_m2",
  population_metric = "population",
  weights_column_name = "tdw_raw"
)
  
calculate_tdw_weights(
  source_geographies_path = "output/geography_attributes/2000_tract.csv.gz",
  source_id_name = "CTIDFP00",
  target_geographies_path = "output/geography_attributes/2010_tract.csv.gz",
  target_id_name = "GEOID10",
  intersections_path = "output/intersections/2000_tract_2010_tract.csv.gz",
  area_metric = "area_m2",
  population_metric = "population",
  weights_column_name = "tdw_raw"
)

# Diagnostics -------------------------------------------------------------

library(ggplot2)
library(sf)
library(shadowtext)

tracts_ma_2000 <- st_read("external/tiger/2000/tract/tl_2010_25_tract00.shp")
tracts_ma_2000 <- cbind(tracts_ma_2000, st_coordinates(st_centroid(tracts_ma_2000)))

tracts_ma_2010 <- st_read("external/tiger/2010/tract/tl_2010_25_tract10.shp")

blocks_ma_2000 <- st_read("external/tiger/2000/block/tl_2010_25_tabblock00.shp")
block_pops_2000 <- fread(
  "output/tables/decennial/2000_block_sf1.csv.gz",
  colClasses = list(character = "GEOID"),
  select = c("GEOID", "population")
)
blocks_ma_2000 <- merge(
  subset(blocks_ma_2000, select = "BLKIDFP00"),
  block_pops_2000,
  by.x = "BLKIDFP00",
  by.y = "GEOID",
  all.x = TRUE
)
blocks_ma_2000$CTIDFP00 <- substr(blocks_ma_2000$BLKIDFP00, 1, 11)

test_weights <- calculate_tdw_weights(
  source_geographies_path = "output/geography_attributes/2000_tract.csv.gz",
  source_id_name = "CTIDFP00",
  target_geographies_path = "output/geography_attributes/2010_tract.csv.gz",
  target_id_name = "GEOID10",
  intersections_path = "output/intersections/2000_tract_2010_tract.csv.gz",
  area_metric = "area_m2",
  population_metric = "population",
  weights_column_name = "tdw_raw"
)
test_weights_sf <- merge(
  subset(tracts_ma_2000, select = c("CTIDFP00", "X", "Y")),
  test_weights,
  all.x = TRUE,
  by = "CTIDFP00"
)

tdw_total <- test_weights[, list(tdw_raw_total = sum(tdw_raw)), by = GEOID10
                          ][substr(GEOID10, 1, 2) == "25"]
hist(tdw_total$tdw_raw_total)
tdw_total[tdw_raw_total < 1]

target <- "25001010206" # 0.07068636
target <- "25027758102" # 0.53080687
target <- "25001010700" # 2.005351

(sampled <- tdw_total[sample(1:nrow(tdw_total), 1)])
target <- sampled$GEOID10

ggplot(subset(test_weights_sf, GEOID10 == target)) +
  geom_sf(aes(fill = tdw_raw), color = "black") +
  geom_sf(data = subset(tracts_ma_2010, GEOID10 == target), lwd = 2, color = "red", fill = NA) +
  geom_shadowtext(aes(x = X, y = Y, label = signif(tdw_raw, 3))) +
  scale_fill_viridis_c() +
  labs(title = paste("Weights for", target)) +
  theme_minimal()

(sampled <- tdw_total[sample(1:nrow(tdw_total), 1)])
target <- sampled$GEOID10
test_weights_sf
subset(blocks_ma_2000, CTIDFP00 %in% subset(test_weights_sf, GEOID10 == target)$CTIDFP00)
ggplot(subset(test_weights_sf, GEOID10 == target)) +
  geom_sf(aes(fill = population), data = subset(blocks_ma_2000, CTIDFP00 %in% subset(test_weights_sf, GEOID10 == target)$CTIDFP00), lwd = 0) +
  geom_sf(color = "black", fill = NA) +
  geom_sf(data = subset(tracts_ma_2010, GEOID10 == target), lwd = 2, color = "red", fill = NA) +
  geom_shadowtext(aes(x = X, y = Y, label = signif(tdw_raw, 3))) +
  scale_fill_viridis_c() +
  labs(title = paste("Weights for", target), fill = "Population\n(2010)") +
  theme_minimal()
