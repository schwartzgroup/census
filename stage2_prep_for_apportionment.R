#!/usr/bin/env Rscript

library(data.table)
library(exactextractr)
library(parallel)
library(pbapply)
library(sf)
library(terra)

# Masks a lot of the function names in `terra`, but we need it for
# `exactextract` - opted instead to lazy load with `raster::raster`
# install.packages("raster)

source("util.R")

terraOptions(tempdir = "/home/edgar/temp")
pboptions(type = "timer")

MRLC_CRS_CONUS <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# MRLC_CRS_AK = "+proj=aea +lat_0=50 +lon_0=-154 +lat_1=55 +lat_2=65 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# MRLC_CRS_HI = "+proj=aea +lat_0=3 +lon_0=-157 +lat_1=8 +lat_2=18 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# MRLC_CRS_PR = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

CELL_AREA_M <- 900 # 30x30 meters

NAD83_CONUS_ALBERS_CRS <- "epsg:5070"

ROAD_BUFFER_M <- 300

NLCD_CONUS <- "external/mrlc/landcover/nlcd_2001_land_cover_l48_20210604.img"
NLCD_WATER <- 11

IMPERVIOUSNESS_CONUS <- "external/mrlc/imperviousness/nlcd_2001_impervious_l48_20210604.img"
IMPERVIOUSNESS_THRESHOLD <- 5

DEFAULT_BLOCK_POPULATIONS <- "output/tables/decennial/2000_block_sf1.csv.gz"

NEAR_ROAD_VRT_PATH = "output/near_road_population/all.vrt"

BD_POPULATION_VRT_PATH = "output/dasymetrically_refined_population/all.vrt"

# Binary dasymetric refinement --------------------------------------------

calculate_dasymetrically_refined_population <- function(blocks,
                                                        roads, # character vector
                                                        population_csv = DEFAULT_BLOCK_POPULATIONS,
                                                        nlcd_raster = NLCD_CONUS,
                                                        imperviousness_raster = IMPERVIOUSNESS_CONUS,
                                                        raster_crs = MRLC_CRS_CONUS,
                                                        blocks_geoid = "BLKIDFP00",
                                                        population_geoid = "GEOID") {
  message("(1/12) Reading population data")
  population <- fread(
    population_csv,
    colClasses = list(character = population_geoid),
    select = c(population_geoid, "population")
  )
  
  message("(2/12) Reading and validating blocks")
  blocks <- vect(blocks)
  blocks <- makeValid(blocks)
  blocks <- project(blocks, raster_crs)
  blocks <- blocks[, blocks_geoid] # Fixes bug in pulling values as a data.frame
  
  # terra doesn't like GEOIDs - here we map them to small numbers which we will
  # later convert back into GEOIDs
  message("(3/12) Replacing GEOIDs with integers")
  geoid_mapping <- data.table(
    geoid_placeholder = values(blocks)[[blocks_geoid]],
    id = 1:nrow(blocks)
  )
  setnames(geoid_mapping, "geoid_placeholder", blocks_geoid)
  blocks <- merge(blocks, geoid_mapping, by = blocks_geoid, all.x = TRUE)
  
  message("(4/12) Reading and cropping MRLD NLCD")
  nlcd_cropped <- crop(rast(nlcd_raster), blocks)
  
  message("(5/12) Reading and cropping MRLD imperviousness")
  imperviousness_cropped <- crop(rast(imperviousness_raster), blocks)
  
  message("(6/12) Rasterizing blocks")
  blocks <- rasterize(blocks, nlcd_cropped, "id")
  
  message(sprintf("(7/12) Reading, reprojecting, and buffering roads by %s meters", ROAD_BUFFER_M))
  roads <- pblapply(roads, vect)
  roads <- pblapply(roads, project, NAD83_CONUS_ALBERS_CRS)
  roads <- pblapply(roads, buffer, ROAD_BUFFER_M)
  roads <- do.call(rbind, roads)
  
  message("(8/12) Rasterizing roads")
  roads <- rasterize(roads, nlcd_cropped)
  
  # all(c(
  #   dim(blocks) == dim(roads),
  #   dim(blocks) == dim(imperviousness_cropped),
  #   dim(blocks) == dim(nlcd_cropped)
  # ))
  # sum(is.na(values(blocks))) # MA: 33017031
  
  message("(9/12) Subtracting NLCD water bodies")
  blocks[nlcd_cropped == NLCD_WATER] <- NA
  # sum(is.na(values(blocks))) # MA: 40864221
  
  if (ext(imperviousness_cropped) != ext(nlcd_cropped)) {
    message("(DEBUG) Resizing imperviousness to match NLCD")
    imperviousness_cropped <- resample(imperviousness_cropped, nlcd_cropped)
  }
  
  message(sprintf("(10/12) Subtracting imperviousness < %s", IMPERVIOUSNESS_THRESHOLD))
  blocks[imperviousness_cropped < IMPERVIOUSNESS_THRESHOLD] <- NA
  # sum(is.na(values(blocks))) # MA: 58265899
  
  message(sprintf("(11/12) Subtracting > %s m from roads", ROAD_BUFFER_M))
  blocks[is.na(roads)] <- NA
  # sum(is.na(values(blocks))) # MA: 63359090
  
  message("(12/12) Spreading block population over grid cells")
  cell_ids <- values(blocks)[, "id"]
  
  cells_per_id <- data.table(id = cell_ids
                             )[!is.na(id)
                               ][, list(cells = .N), by = id]
  cells_per_id <- merge(cells_per_id, geoid_mapping, by = "id", all.x = TRUE)
  
  population_per_id <- merge(
    cells_per_id, population,
    by.x = blocks_geoid, by.y = population_geoid,
    all.x = TRUE
  )[, population_per_cell := population / cells]
  
  cell_populations <- merge(
    data.table(id = cell_ids), population_per_id,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )$population_per_cell
  
  values(blocks) <- cell_populations
  names(blocks) <- "population"
  
  tmpFiles(current = TRUE, remove = TRUE)
  return(blocks)
}

invisible(pblapply(
  setdiff(STATES_DC_FIPS, c("02", "15")),
  function(fips) {
    state <- subset(FIPS_DF, FIPS == fips)$STATE
    output_dir <- "output/dasymetrically_refined_population/"
    output_file <- file.path(output_dir, sprintf("2000_%s.tif", fips))
    temp_file <- file.path(output_dir, "temp.tif")
    
    if (file.exists(output_file)) {
      message(sprintf("Skipping: %s (%s)", fips, state))
    } else {
      message(sprintf("Calculating binary dasymetric population: %s (%s)", fips, state))
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      
      result <- calculate_dasymetrically_refined_population(
        blocks = sprintf("external/tiger/2000/block/tl_2010_%s_tabblock00.shp", fips),
        roads = Sys.glob(sprintf("external/tiger/2010/roads/tl_2010_%s*_roads.shp", fips))
      )
      
      writeRaster(result, temp_file)
      file.rename(temp_file, output_file)
    }
    
  }
))

message("Building VRT")
invisible(vrt(
  Sys.glob("output/dasymetrically_refined_population/*.tif"),
  "output/dasymetrically_refined_population/all.vrt",
  overwrite = TRUE
))

# Near-road areas ---------------------------------------------------------
# TODO: this is just a copy of the above section with the extra refinement
# removed. There's probably a better way to do this - maybe cache the road
# rasterization step and then split into further refinement vs population
# spread?

calculate_near_road_population <- function(blocks,
                                           roads, # character vector
                                           population_csv = DEFAULT_BLOCK_POPULATIONS,
                                           nlcd_raster = NLCD_CONUS,
                                           imperviousness_raster = IMPERVIOUSNESS_CONUS,
                                           raster_crs = MRLC_CRS_CONUS,
                                           blocks_geoid = "BLKIDFP00",
                                           population_geoid = "GEOID") {
  message("(1/10) Reading population data")
  population <- fread(
    population_csv,
    colClasses = list(character = population_geoid),
    select = c(population_geoid, "population")
  )
  
  message("(2/10) Reading and validating blocks")
  blocks <- vect(blocks)
  blocks <- makeValid(blocks)
  blocks <- project(blocks, raster_crs)
  blocks <- blocks[, blocks_geoid] # Fixes bug in pulling values as a data.frame
  
  # terra doesn't like GEOIDs - here we map them to small numbers which we will
  # later convert back into GEOIDs
  message("(3/10) Replacing GEOIDs with integers")
  geoid_mapping <- data.table(
    geoid_placeholder = values(blocks)[[blocks_geoid]],
    id = 1:nrow(blocks)
  )
  setnames(geoid_mapping, "geoid_placeholder", blocks_geoid)
  blocks <- merge(blocks, geoid_mapping, by = blocks_geoid, all.x = TRUE)
  
  message("(4/10) Reading and cropping MRLD NLCD")
  nlcd_cropped <- crop(rast(nlcd_raster), blocks)
  
  message("(5/10) Reading and cropping MRLD imperviousness")
  imperviousness_cropped <- crop(rast(imperviousness_raster), blocks)
  
  message("(6/10) Rasterizing blocks")
  blocks <- rasterize(blocks, nlcd_cropped, "id")
  
  message(sprintf("(7/10) Reading, reprojecting, and buffering roads by %s meters", ROAD_BUFFER_M))
  roads <- pblapply(roads, vect)
  roads <- pblapply(roads, project, NAD83_CONUS_ALBERS_CRS)
  roads <- pblapply(roads, buffer, ROAD_BUFFER_M)
  roads <- do.call(rbind, roads)
  
  message("(8/10) Rasterizing roads")
  roads <- rasterize(roads, nlcd_cropped)
  
  message(sprintf("(9/10) Subtracting > %s m from roads", ROAD_BUFFER_M))
  blocks[is.na(roads)] <- NA
  # sum(is.na(values(blocks))) # MA: 63359090
  
  message("(10/10) Spreading block population over grid cells")
  cell_ids <- values(blocks)[, "id"]
  
  cells_per_id <- data.table(id = cell_ids
  )[!is.na(id)
  ][, list(cells = .N), by = id]
  cells_per_id <- merge(cells_per_id, geoid_mapping, by = "id", all.x = TRUE)
  
  population_per_id <- merge(
    cells_per_id, population,
    by.x = blocks_geoid, by.y = population_geoid,
    all.x = TRUE
  )[, population_per_cell := population / cells]
  
  cell_populations <- merge(
    data.table(id = cell_ids), population_per_id,
    by = "id",
    all.x = TRUE,
    sort = FALSE
  )$population_per_cell
  
  values(blocks) <- cell_populations
  names(blocks) <- "population"
  
  tmpFiles(current = TRUE, remove = TRUE)
  return(blocks)
}

invisible(pblapply(
  setdiff(STATES_DC_FIPS, c("02", "15")),
  function(fips) {
    state <- subset(FIPS_DF, FIPS == fips)$STATE
    output_dir <- "output/near_road_population/"
    output_file <- file.path(output_dir, sprintf("2000_%s.tif", fips))
    temp_file <- file.path(output_dir, "temp.tif")
    
    if (file.exists(output_file)) {
      message(sprintf("Skipping: %s (%s)", fips, state))
    } else {
      message(sprintf("Calculating near-road population: %s (%s)", fips, state))
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      
      result <- calculate_near_road_population(
        blocks = sprintf("external/tiger/2000/block/tl_2010_%s_tabblock00.shp", fips),
        roads = Sys.glob(sprintf("external/tiger/2010/roads/tl_2010_%s*_roads.shp", fips))
      )
      
      writeRaster(result, temp_file)
      file.rename(temp_file, output_file)
    }
    
  }
))

message("Building VRT")
invisible(vrt(
  Sys.glob("output/near_road_population/*.tif"),
  "output/near_road_population/all.vrt",
  overwrite = TRUE
))

# Calculation of source and target attributes -----------------------------
# For hierarchical geographies, we could truncate GEOIDs, but will stick to
# spatial joins to stay consistent

calculate_attributes <- function(blocks,
                                 geographies,
                                 geographies_geoid,
                                 blocks_geoid = "BLKIDFP00",
                                 near_road_raster = NEAR_ROAD_VRT_PATH,
                                 population_raster = BD_POPULATION_VRT_PATH,
                                 population_csv = DEFAULT_BLOCK_POPULATIONS,
                                 population_geoid = "GEOID") {
  message("(1/11) Loading population data")
  block_population <- fread(
    population_csv,
    colClasses = list(character = population_geoid),
    select = c(population_geoid, "population")
  )
  
  message("(2/11) Loading vector data (sf::st_read)")
  if (length(blocks) > 1) {
    blocks <- pblapply(blocks, function(path) st_read(path, quiet = TRUE)[blocks_geoid])
    blocks <- pblapply(blocks, st_make_valid)
    message("> Merging blocks")
    blocks <- do.call(rbind, blocks)
  } else {
    blocks <- st_read(blocks, quiet = TRUE)[blocks_geoid]
    blocks <- st_make_valid(blocks)
  }
  if (length(geographies) > 1) {
    geographies <- pblapply(geographies, function(path) st_read(path, quiet = TRUE)[geographies_geoid])
    geographies <- pblapply(geographies, st_make_valid)
    message("> Merging geographies")
    geographies <- do.call(rbind, geographies)
  } else {
    geographies <- st_read(geographies, quiet = TRUE)[geographies_geoid]
    geographies <- st_make_valid(geographies)
  }
  
  message("(3/11) Reprojecting blocks")
  blocks <- st_transform(blocks, MRLC_CRS_CONUS)
  
  message("(4/11) Reprojecting input geographies")
  geographies <- st_transform(geographies, MRLC_CRS_CONUS)
  
  message("(5/11) Calculating block centroids")
  blocks_centroids <- st_centroid(blocks)
  
  message("(6/11) Assigning block populations")
  blocks_centroids <- merge(
    blocks_centroids, block_population,
    by.x = blocks_geoid, by.y = population_geoid,
    all.x = TRUE
  )
  
  message("(7/11) Performing point-in-polygon spatial join of block centroids")
  blocks_centroids <- st_join(blocks_centroids, geographies, st_within)
  
  # Unrefined population
  message("(8/11) Aggregating joined block populations to input geographies")
  geographies <- merge(
    geographies,
    as.data.table(st_drop_geometry(blocks_centroids)
    )[, list(population = sum(population)),
      by = geographies_geoid],
    by = geographies_geoid,
    all.x = TRUE
  )
  geographies$population <- ifelse(
    is.na(geographies$population),
    0,
    geographies$population
  )
  
  # Unrefined area
  message("(9/11) Calculating input geogrpahy areas")
  geographies$area_m2 <- as.numeric(st_area(geographies))
  
  # Near-road population and area
  message("(10/11) Extracting near-road populations and areas")
  near_road_results <- exact_extract(
    raster::raster(near_road_raster),
    geographies,
    c("sum", "count")
  )
  geographies$near_road_population <- near_road_results$sum
  geographies$near_road_area_m2 <- near_road_results$count * CELL_AREA_M
  
  # Binary dasymetrically-refined population and area
  message("(11/11) Extracting binary dasymetrically-refined populations and areas")
  bd_results <- exact_extract(
    raster::raster(population_raster),
    geographies,
    c("sum", "count")
  )
  geographies$bd_refined_population <- bd_results$sum
  geographies$bd_refined_area_m2 <- bd_results$count * CELL_AREA_M
  
  tmpFiles(current = TRUE, remove = TRUE)
  return(st_drop_geometry(geographies))
}

if (FALSE) {
  x <- calculate_attributes(
    blocks = Sys.glob("external/tiger/2000/block/*.shp"),
    geographies = Sys.glob("external/tiger/2000/tract/*.shp"),
    geographies_geoid = "CTIDFP00"
  )
  fwrite(x, "output/geography_attributes/2000_tract.csv.gz")
  
  x <- calculate_attributes(
    blocks = Sys.glob("external/tiger/2000/block/*.shp"),
    geographies = Sys.glob("external/tiger/2010/tract/*.shp"),
    geographies_geoid = "GEOID10"
  )
  fwrite(x, "output/geography_attributes/2010_tract.csv.gz")
  
  x <- calculate_attributes(
    blocks = Sys.glob("external/tiger/2000/block/*.shp"),
    geographies = Sys.glob("external/tiger/2000/zcta5/*.shp"),
    geographies_geoid = "ZCTA5CE00"
  )
  fwrite(x, "output/geography_attributes/2000_zcta5.csv.gz")
  
  # TODO
  x <- calculate_attributes(
    blocks = Sys.glob("external/tiger/2000/block/*.shp"),
    geographies = Sys.glob("external/tiger/2010/zcta5/*.shp"),
    geographies_geoid = "ZCTA5CE10"
  )
  fwrite(x, "output/geography_attributes/2010_zcta5.csv.gz")
  
  x <- calculate_attributes(
    blocks = Sys.glob("external/tiger/2010/block/*.shp"),
    geographies = Sys.glob("external/tiger/2000/block_group/*.shp"),
    geographies_geoid = "BKGPIDFP00"
  )
  fwrite(x, "output/geography_attributes/2000_block_group.csv.gz")
  
  x <- calculate_attributes(
    blocks = Sys.glob("external/tiger/2010/block/*.shp"),
    geographies = Sys.glob("external/tiger/2010/block_group/*.shp"),
    geographies_geoid = "GEOID10"
  )
  fwrite(x, "output/geography_attributes/2010_block_group.csv.gz")
}

# Calculation of intersections --------------------------------------------

calculate_intersections <- function(blocks,
                                    source_geographies,
                                    target_geographies,
                                    source_geoid,
                                    target_geoid = "GEOID10",
                                    population_csv = DEFAULT_BLOCK_POPULATIONS,
                                    blocks_geoid = "BLKIDFP00",
                                    near_road_raster = NEAR_ROAD_VRT_PATH,
                                    population_raster = BD_POPULATION_VRT_PATH,
                                    population_geoid = "GEOID") {
  message("(1/16) Loading population data")
  block_population <- fread(
    population_csv,
    colClasses = list(character = population_geoid),
    select = c(population_geoid, "population")
  )
  
  message("(2/16) Loading vector data (terra::vect)")
  if (length(blocks) > 1) {
    blocks <- pblapply(blocks, function(path) vect(path)[, blocks_geoid])
    blocks <- pblapply(blocks, makeValid)
    message("> Merging blocks")
    blocks <- do.call(rbind, blocks)
  } else {
    blocks <- vect(blocks)[, blocks_geoid]
    blocks <- makeValid(blocks)
  }
  if (length(source_geographies) > 1) {
    source_geographies <- pblapply(source_geographies, function(path) vect(path)[, source_geoid])
    source_geographies <- pblapply(source_geographies, makeValid)
    message("> Merging source geographies")
    source_geographies <- do.call(rbind, source_geographies)
  } else {
    source_geographies <- vect(source_geographies)[, source_geoid]
    source_geographies <- makeValid(source_geographies)
  }
  if (length(target_geographies) > 1) {
    target_geographies <- pblapply(target_geographies, function(path) vect(path)[, target_geoid])
    target_geographies <- pblapply(target_geographies, makeValid)
    message("> Merging target geographies")
    target_geographies <- do.call(rbind, target_geographies)
  } else {
    target_geographies <- vect(target_geographies)[, target_geoid]
    target_geographies <- makeValid(target_geographies)
  }
  
  message("(3/16) Validating input geographies")
  source_geographies <- makeValid(source_geographies)
  target_geographies <- makeValid(target_geographies)
  
  # Intersections and reprojections are faster with `terra`, so we'll do those
  # before handing the data over to `sf`
  message("(4/16) Calculating intersections")
  intersections <- intersect(source_geographies, target_geographies)
  
  message("(5/16) Reprojecting intersections")
  intersections <- project(intersections, MRLC_CRS_CONUS)
  
  message("(6/16) Converting intersections from `SpatVector`s to `sf`s")
  intersections <- st_as_sf(intersections)
  
  # Unrefined block population
  # Centroids and reprojections are faster in `terra`
  message("(7/16) Calculating block centroids")
  blocks_centroids <- centroids(blocks)
  
  message("(8/16) Reprojecting block centroids")
  blocks_centroids <- project(blocks_centroids, MRLC_CRS_CONUS)
  
  message("(9/16) Assigning block populations")
  blocks_centroids <- merge(
    blocks_centroids, block_population,
    by.x = blocks_geoid, by.y = population_geoid,
    all.x = TRUE
  )
  
  message("(10/16) Converting block centroids from `SpatVector`s to `sf`s")
  blocks_centroids <- st_as_sf(blocks_centroids)
  
  message("(11/16) Performing point-in-polygon spatial join of block centroids")
  blocks_centroids <- st_join(blocks_centroids["population"], intersections, st_within)
  
  message("(12/16) Aggregating joined block populations to intersections")
  intersections <- merge(
    intersections,
    as.data.table(st_drop_geometry(blocks_centroids)
    )[, list(population = sum(population)),
      by = c(source_geoid, target_geoid)],
    by = c(source_geoid, target_geoid),
    all.x = TRUE
  )
  intersections$population <- ifelse(
    is.na(intersections$population),
    0,
    intersections$population
  )
  
  # Unrefined area
  message("(13/16) Calculating intersection areas")
  intersections$area_m2 <- as.numeric(st_area(intersections))
  
  # Near-road population and area
  message("(14/16) Extracting near-road populations and areas")
  near_road_results <- exact_extract(
    raster::raster(near_road_raster),
    intersections,
    c("sum", "count")
  )
  intersections$near_road_population <- near_road_results$sum
  intersections$near_road_area_m2 <- near_road_results$count * CELL_AREA_M
  
  # Binary dasymetrically-refined population and area
  message("(15/16) Extracting binary dasymetrically-refined populations and areas")
  bd_results <- exact_extract(
    raster::raster(population_raster),
    intersections,
    c("sum", "count")
  )
  intersections$bd_refined_population <- bd_results$sum
  intersections$bd_refined_area_m2 <- bd_results$count * CELL_AREA_M
  
  message("(16/16) Aggregating multipolygons")
  intersections <- as.data.table(st_drop_geometry(intersections)
  )[, lapply(.SD, sum),
    by = c(source_geoid, target_geoid)]
  
  return(intersections)
}

if (FALSE) {
  x <- calculate_intersections(
    blocks = Sys.glob("external/tiger/2000/block/*.shp"),
    source_geographies = Sys.glob("external/tiger/2000/tract/*.shp"),
    source_geoid = "CTIDFP00",
    target_geographies = Sys.glob("external/tiger/2010/tract/*.shp"),
    target_geoid = "GEOID10"
  )
  fwrite(x, "output/intersections/2000_tract_2010_tract.csv.gz")
  
  x <- calculate_intersections(
    blocks = Sys.glob("external/tiger/2000/block/*.shp"),
    source_geographies = Sys.glob("external/tiger/2000/block_group/*.shp"),
    source_geoid = "BKGPIDFP00",
    target_geographies = Sys.glob("external/tiger/2010/tract/*.shp"),
    target_geoid = "GEOID10"
  )
  fwrite(x, "output/intersections/2000_block_group_2010_tract.csv.gz")
  
  x <- calculate_intersections(
    blocks = Sys.glob("external/tiger/2000/block/*.shp"),
    source_geographies = Sys.glob("external/tiger/2000/zcta5/*.shp"),
    source_geoid = "ZCTA5CE00",
    target_geographies = Sys.glob("external/tiger/2010/zcta5/*.shp"),
    target_geoid = "ZCTA5CE10"
  )
  fwrite(x, "output/intersections/2000_zcta5_2010_zcta5.csv.gz")
}

# DEPRECATED: per-state calculation of attributes -------------------------
# 
# calculate_attributes <- function(blocks,
#                                  geographies_shapefile,
#                                  geographies_geoid,
#                                  population_raster = BD_POPULATION_VRT_PATH,
#                                  population_csv = DEFAULT_BLOCK_POPULATIONS,
#                                  blocks_geoid = "BLKIDFP00",
#                                  population_geoid = "GEOID") {
#   message("(1/10) Loading population data")
#   block_population <- fread(
#     population_csv,
#     colClasses = list(character = population_geoid),
#     select = c(population_geoid, "population")
#   )
#   
#   message("(2/10) Loading vector data (sf::st_read)")
#   blocks <- st_read(blocks, quiet = TRUE)[blocks_geoid]
#   geographies <- st_read(geographies_shapefile, quiet = TRUE)[geographies_geoid]
#   
#   message("(3/10) Reprojecting blocks")
#   blocks <- st_transform(blocks, MRLC_CRS_CONUS)
#   
#   message("(4/10) Reprojecting input geographies")
#   geographies <- st_transform(geographies, MRLC_CRS_CONUS)
#   
#   message("(5/10) Calculating block centroids")
#   blocks_centroids <- st_centroid(blocks)
#   
#   message("(6/10) Assigning block populations")
#   blocks_centroids <- merge(
#     blocks_centroids, block_population,
#     by.x = blocks_geoid, by.y = population_geoid,
#     all.x = TRUE
#   )
#   
#   message("(7/10) Performing point-in-polygon spatial join of block centroids")
#   blocks_centroids <- st_join(blocks_centroids, geographies, st_within)
#   
#   # Unrefined population
#   message("(8/10) Aggregating joined block populations to intersections")
#   geographies <- merge(
#     geographies,
#     as.data.table(st_drop_geometry(blocks_centroids)
#     )[, list(population = sum(population)),
#       by = geographies_geoid],
#     by = geographies_geoid,
#     all.x = TRUE
#   )
#   geographies$population <- ifelse(
#     is.na(geographies$population),
#     0,
#     geographies$population
#   )
#   
#   # Unrefined area
#   message("(9/10) Calculating input geogrpahy areas")
#   geographies$area_m2 <- as.numeric(st_area(geographies))
#   
#   # Binary dasymetrically-refined population and area
#   message("(10/10) Extracting binary dasymetrically-refined populations and areas")
#   bd_results <- exact_extract(
#     raster::raster(population_raster),
#     geographies,
#     c("sum", "count")
#   )
#   geographies$bd_refined_population <- bd_results$sum
#   geographies$bd_refined_area_m2 <- bd_results$count * CELL_AREA_M
#   
#   tmpFiles(current = TRUE, remove = TRUE)
#   return(st_drop_geometry(geographies))
# }
# 
# invisible(pbapply(
#   expand.grid(
#     fips = setdiff(STATES_DC_FIPS, c("02", "15")),
#     census_year = c(2000, 2010)
#   ),
#   1,
#   function(row) {
#     fips <- row[["fips"]]
#     census_year <- row[["census_year"]]
#     
#     state <- subset(FIPS_DF, FIPS == fips)$STATE
#     if (census_year == 2000) {
#       suffix <- "00"
#       geoid <- "CTIDFP00"
#     } else if (census_year == 2010) {
#       suffix <- "10"
#       geoid <- "GEOID10"
#     } else {
#       stop("Not implemented")
#     }
#     
#     output_dir <- "output/geography_attributes/"
#     output_file <- file.path(output_dir, sprintf("%s_tract_%s.csv.gz", census_year, fips))
#     temp_file <- file.path(output_dir, "temp.csv.gz")
#     
#     if (file.exists(output_file)) {
#       message(sprintf("Skipping: %s (%s)", fips, state))
#     } else {
#       message(sprintf("Calculating intersections: %s (%s)", fips, state))
#       dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
#       
#       result <- calculate_attributes(
#         blocks = sprintf(
#           "external/tiger/%s/block/tl_2010_%s_tabblock%s.shp",
#           census_year, fips, suffix
#         ),
#         geographies_shapefile = sprintf(
#           "external/tiger/%s/tract/tl_2010_%s_tract%s.shp",
#           census_year, fips, suffix
#         ),
#         geographies_geoid = geoid
#       )
#       
#       fwrite(result, temp_file)
#       file.rename(temp_file, output_file)
#     }
#     
#   }
# ))

# DEPRECATED: per-state calculation of intersections ----------------------
# 
# calculate_intersections <- function(blocks,
#                                     population_raster,
#                                     source_geographies,
#                                     target_geographies,
#                                     source_geoid,
#                                     target_geoid = "GEOID10",
#                                     population_csv = DEFAULT_BLOCK_POPULATIONS,
#                                     blocks_geoid = "BLKIDFP00",
#                                     population_geoid = "GEOID") {
#   message("(1/9) Loading population data")
#   block_population <- fread(
#     population_csv,
#     colClasses = list(character = population_geoid),
#     select = c(population_geoid, "population")
#   )
#   
#   message("(2/9) Loading vector data (terra::vect)")
#   blocks <- vect(blocks)[, blocks_geoid]
#   source_geographies <- vect(source_geographies)[, source_geoid]
#   target_geographies <- vect(target_geographies)[, target_geoid]
#   
#   # Intersections and reprojections are faster with `terra`, so we'll do those
#   # before handing the data over to `sf`
#   message("(3/9) Calculating, reprojecting, and type converting intersections")
#   intersections <- intersect(source_geographies, target_geographies)
#   intersections <- project(intersections, MRLC_CRS_CONUS)
#   intersections <- st_as_sf(intersections)
#   
#   # Unrefined block population
#   # Centroids and reprojections are faster in `terra`
#   message("(4/9) Calculating, reprojecting, and type converting block centroids")
#   blocks_centroids <- centroids(blocks)
#   blocks_centroids <- project(blocks_centroids, MRLC_CRS_CONUS)
#   blocks_centroids <- merge(
#     blocks_centroids, block_population,
#     by.x = "BLKIDFP00", by.y = population_geoid,
#     all.x = TRUE
#   )
#   blocks_centroids <- st_as_sf(blocks_centroids)
#   
#   message("(5/9) Performing point-in-polygon spatial join of block centroids")
#   blocks_centroids <- st_join(blocks_centroids["population"], intersections, st_within)
#   
#   message("(6/9) Aggregating joined block populations to intersections")
#   intersections <- merge(
#     intersections,
#     as.data.table(st_drop_geometry(blocks_centroids)
#     )[, list(population = sum(population)),
#       by = c(source_geoid, target_geoid)],
#     by = c(source_geoid, target_geoid),
#     all.x = TRUE
#   )
#   intersections$population <- ifelse(
#     is.na(intersections$population),
#     0,
#     intersections$population
#   )
#   
#   # Unrefined area
#   message("(7/9) Calculating intersection areas")
#   intersections$area_m2 <- as.numeric(st_area(intersections))
#   
#   # Binary dasymetrically-refined population and area
#   message("(8/9) Extracting binary dasymetrically-refined populations and areas")
#   bd_results <- exact_extract(
#     raster::raster(population_raster),
#     intersections,
#     c("sum", "count")
#   )
#   intersections$bd_refined_population <- bd_results$sum
#   intersections$bd_refined_area_m2 <- bd_results$count * CELL_AREA_M
#   
#   message("(9/9) Aggregating MultiPolygons")
#   intersections <- as.data.table(st_drop_geometry(intersections)
#   )[, lapply(.SD, sum),
#     by = c(source_geoid, target_geoid)]
#   
#   return(intersections)
# }
# 
# invisible(pblapply(
#   setdiff(STATES_DC_FIPS, c("02", "15")),
#   function(fips) {
#     state <- subset(FIPS_DF, FIPS == fips)$STATE
#     output_dir <- "output/intersections/"
#     output_file <- file.path(output_dir, sprintf("2000_tract_2010_tract_%s.csv.gz", fips))
#     temp_file <- file.path(output_dir, "temp.csv.gz")
# 
#     if (file.exists(output_file)) {
#       message(sprintf("Skipping: %s (%s)", fips, state))
#     } else {
#       message(sprintf("Calculating intersections: %s (%s)", fips, state))
#       dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
# 
#       result <- calculate_intersections(
#         blocks = sprintf("external/tiger/2000/block/tl_2010_%s_tabblock00.shp", fips),
#         population_raster = sprintf("output/dasymetrically_refined_population/2000_%s.tif", fips),
#         source_geographies = sprintf("external/tiger/2000/tract/tl_2010_%s_tract00.shp", fips),
#         source_geoid = "CTIDFP00",
#         target_geographies = sprintf("external/tiger/2010/tract/tl_2010_%s_tract10.shp", fips),
#         target_geoid = "GEOID10"
#       )
# 
#       fwrite(result, temp_file)
#       file.rename(temp_file, output_file)
#     }
# 
#   }
# ))
