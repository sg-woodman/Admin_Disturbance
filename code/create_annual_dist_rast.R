# Title: Rasterize disturbance data
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Convert disturbance polygons to rasters

## Aerial disturbance data need to be converted to rasters for easier/quicker 
## processing. For each polygon file, they need to be separated by dist type and
## rasterized according to the year of disturbance. 


# Load packages -----------------------------------------------------------

library(sf)
library(terra)
library(raster)
library(tidyverse)
library(here)

# Functions ---------------------------------------------------------------

## Seprate sf vector so each layer is a year of data for a specific dist type
get_annual_dist_list <- function(df, filt_yr, crs, dist_type) {
  # Arguments
  ## df: spatial datafrome of disturbance with columns for "Event" and "Year"
  ## filt_yr: Minimum threshold year for filtering
  ## crs: EPSG number to transform polygons to 
  ## dist_type: vector of type of disturbance used to filter "Event" column
  out <- df %>% 
    # filter to years of interest
    filter(Year >= filt_yr) %>% 
    # filter to disturbance types of interest
    dplyr::filter(Event %in% dist_type) %>% 
    # reproject to CRS
    st_transform(crs) %>% 
    # group by Year and split to list object
    group_by(Year) %>% 
    group_split()
  return(out)
}

get_annual_dist_list_stand <- function(df, filt_yr, crs, dist) {
  # Arguments
  ## df: spatial datafrome of disturbance with columns for "Event" and "Year"
  ## filt_yr: Minimum threshold year for filtering
  ## crs: EPSG number to transform polygons to 
  ## dist_type: vector of type of disturbance used to filter "Event" column
  out <- df %>% 
    # filter to years of interest
    filter(Year >= filt_yr) %>% 
    # filter to disturbance types of interest
    dplyr::filter(dist_type1 %in% dist) %>% 
    # reproject to CRS
    st_transform(crs) %>% 
    # group by Year and split to list object
    group_by(Year) %>% 
    group_split()
  return(out)
}

get_annual_dist_list_bio <- function(df, filt_yr, crs, dist) {
  # Arguments
  ## df: spatial datafrome of disturbance with columns for "Event" and "Year"
  ## filt_yr: Minimum threshold year for filtering
  ## crs: EPSG number to transform polygons to 
  ## dist_type: vector of type of disturbance used to filter "Event" column
  out <- df %>% 
    # filter to years of interest
    filter(Year >= filt_yr) %>% 
    # filter to disturbance types of interest
    dplyr::filter(dist_type2 %in% dist) %>% 
    # reproject to CRS
    st_transform(crs) %>% 
    # group by Year and split to list object
    group_by(Year) %>% 
    group_split()
  return(out)
}

get_annual_dist_list_all_dist <- function(df, filt_yr, crs, dist) {
  # Arguments
  ## df: spatial datafrome of disturbance with columns for "Event" and "Year"
  ## filt_yr: Minimum threshold year for filtering
  ## crs: EPSG number to transform polygons to 
  ## dist_type: vector of type of disturbance used to filter "Event" column
  out <- df %>% 
    # filter to years of interest
    filter(Year >= filt_yr) %>% 
    # filter to disturbance types of interest
    dplyr::filter(all_dist %in% dist) %>% 
    # reproject to CRS
    st_transform(crs) %>% 
    # group by Year and split to list object
    group_by(Year) %>% 
    group_split()
  return(out)
}

## raster vecto where each year is a layer
rasterize_dist_year <- function(poly, rast, sum_type, dir, res) {
  # Arguments
  ## poly: polygon to be rasterized
  ## rast: raster with resolution to match
  ## sum_type: whether to sum instances of polygons at single pixel
  ### Count = sum overlapping polygons to get count
  ### PA = overwrite overlapping polygons, returns presence/absence
  ## dir: Name of directory to be save in, also used in file name
  ## res: resolution of rast used for naming
  
  # check polygon class and convert to terra class if needed
  v <- if (class(poly)[1] != "SpatVector") {terra::vect(poly)} else {poly}
  # check raster class and convert to terra class if needed
  r <- if (class(rast)[1] != "SpatRaster") {terra::rast(rast)} else {rast}
  # add count variable
  # for presence/absence, 1 will be overwritten (see sum in terra::rasterize)
  # for counting instances, 1 value for each pixel will be summed
  v$count <- 1
  # convert polygon to raster
  add <- if (sum_type == "PA") FALSE else TRUE
  r <- terra::rasterize(v, r, "count", background = 0, sum = add)
  # crop to aou
  cr_r <- crop(r, aou)
  # mask lakes 
  ma_r <- terra::mask(cr_r, aou_lakes_raster)
  # mask values outside aou
  aou_ma_r <- terra::mask(ma_r, crop(aou_raster, aou))
  # write raster to disk
  # change path when needed
  terra::writeRaster(aou_ma_r, paste0(here("data/processed/Annual_Raster/"),
                                      dir, "/", dir, "_", sum_type, "_", 
                                      unique(poly$Year), "_", res, "m.tif"), 
                     overwrite = T)
}



# Load data ---------------------------------------------------------------

## Disturbance polygons
all_dist <- st_read(here("data/processed/all_dist_2001-2019.gpkg"))

## Reference raster
r_250 <- rast(here("data/raw/raster_250m_template.tif"))
aou_lakes_raster <- rast(here("data/processed/aou_lakes_rast_250m.tif"))
aou_raster <- rast(here("data/processed/aou_rast_250m.tif"))

## Reference vectors
aou <- vect(here("data/raw/AoU.gpkg")) %>% 
  project(., crs(r_250))

### EPSG:3161
## Disturbance polygons
all_dist_3161 <- st_read(here("data/processed/all_dist_3161.gpkg"))

## Reference raster
r_250_3161 <- rast(here("data/raw/raster_250_tamplate_3161.tif"))
aou_lakes_raster_3161 <- rast(here("data/processed/aou_lakes_rast_250m_3161.tif"))
aou_raster_3161 <- rast(here("data/processed/aou_rast_250_3161.tif"))

## Reference vectors
aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))

## Reference raster
r_500_3161 <- rast(here("data/raw/raster_500_tamplate_3161.tif"))
aou_lakes_raster_3161_500 <- rast(here("data/processed/aou_lakes_rast_500m_3161.tif"))
aou_raster_3161_500 <- rast(here("data/processed/aou_rast_500_3161.tif"))

## Reference vectors
aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))

# Process -----------------------------------------------------------------

## Min year for each dist
all_dist %>% 
  st_set_geometry(NULL) %>% 
  group_by(Event) %>% 
  summarise(x = min(Year))

## Bark Beetle
dist_poly_bark <- get_annual_dist_list(all_dist, 2001, 4326, "Bark_Beetle")
dist_poly_bark %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Bark_Beetle", "250")

## Cold
dist_poly_cold <- get_annual_dist_list(all_dist, 2001, 4326, "Cold")
dist_poly_cold %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Cold", "250")

## Defoliators
dist_poly_defol <- get_annual_dist_list(all_dist, 2001, 4326, "Defoliators")
dist_poly_defol %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Defoliators", "250")

## Disease
dist_poly_disease <- get_annual_dist_list(all_dist, 2001, 4326, "Disease")
dist_poly_disease %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Disease", "250")

## Fire
dist_poly_fire <- get_annual_dist_list(all_dist, 2001, 4326, "Fire")
dist_poly_fire %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Fire", "250")

## Water stress
dist_poly_water <- get_annual_dist_list(all_dist, 2001, 4326, "Water_stress")
dist_poly_water %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Water_stress", "250")

## Wind
dist_poly_wind <- get_annual_dist_list(all_dist, 2001, 4326, "Wind")
dist_poly_wind %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Wind", "250")

## Winter_Precip
dist_poly_winter <- get_annual_dist_list(all_dist, 2001, 4326, "Winter_Precip")
dist_poly_winter %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Winter_Precip", "250")

## Stand Replacing
dist_poly_sr <- get_annual_dist_list_stand(all_dist, 2001, 4326, "SR")
dist_poly_sr %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "SR", "250")

## Non stand replacing
dist_poly_nsr <- get_annual_dist_list_stand(all_dist, 2001, 4326, "NSR")
dist_poly_nsr %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "NSR", "250")

## Biotic
dist_poly_bio <- get_annual_dist_list_bio(all_dist, 2001, 4326, "Biotic")
dist_poly_bio %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Biotic", "250")

## Abiotic
dist_poly_abio <- get_annual_dist_list_bio(all_dist, 2001, 4326, "Abiotic")
dist_poly_abio %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "Abiotic", "250")

## All dist
dist_poly_all_dist <- get_annual_dist_list_all_dist(all_dist, 2001, 4326, "All_Dist")
dist_poly_all_dist %>% 
  walk(rasterize_dist_year, rast = r_250, sum_type = "PA", "All_Dist", "250")


# 3161 projected data -----------------------------------------------------

## raster vecto where each year is a layer
rasterize_dist_year_3161 <- function(poly, rast, sum_type, dir, res) {
  # Arguments
  ## poly: polygon to be rasterized
  ## rast: raster with resolution to match
  ## sum_type: whether to sum instances of polygons at single pixel
  ### Count = sum overlapping polygons to get count
  ### PA = overwrite overlapping polygons, returns presence/absence
  ## dir: Name of directory to be save in, also used in file name
  ## res: resolution of rast used for naming
  
  # check polygon class and convert to terra class if needed
  v <- if (class(poly)[1] != "SpatVector") {terra::vect(poly)} else {poly}
  # check raster class and convert to terra class if needed
  r <- if (class(rast)[1] != "SpatRaster") {terra::rast(rast)} else {rast}
  # add count variable
  # for presence/absence, 1 will be overwritten (see sum in terra::rasterize)
  # for counting instances, 1 value for each pixel will be summed
  v$count <- 1
  # convert polygon to raster
  add <- if (sum_type == "PA") FALSE else TRUE
  r <- terra::rasterize(v, r, "count", background = 0, sum = add)
  # crop to aou
  cr_r <- crop(r, aou_3161)
  # mask lakes 
  ma_r <- terra::mask(cr_r, aou_lakes_raster_3161)
  # mask values outside aou
  aou_ma_r <- terra::mask(ma_r, crop(aou_raster_3161, aou_3161))
  # write raster to disk
  # change path when needed
  terra::writeRaster(aou_ma_r, paste0(here("data/processed/Annual_Raster/"),
                                      dir, "/", dir, "_", sum_type, "_", 
                                      unique(poly$Year), "_", res, "m.tif"), 
                     overwrite = T)
}

## Stand Replacing
dist_poly_sr_3161 <- get_annual_dist_list_stand(all_dist_3161, 2001, 3161, "SR")
dist_poly_sr_3161 %>% 
  walk(rasterize_dist_year_3161, rast = r_250_3161, sum_type = "PA", "SR", "250_3161")

## Non stand replacing
dist_poly_nsr_3161 <- get_annual_dist_list_stand(all_dist_3161, 2001, 3161, "NSR")
dist_poly_nsr_3161 %>% 
  walk(rasterize_dist_year_3161, rast = r_250_3161, sum_type = "PA", "NSR_3161", "250_3161")


## All dist
dist_poly_all_dist_3161 <- get_annual_dist_list_all_dist(all_dist_3161, 2001, 3161, "All_Dist")
dist_poly_all_dist_3161 %>% 
  walk(rasterize_dist_year_3161, rast = r_250_3161, sum_type = "PA", "All_Dist_3161", "250_3161")

## raster vecto where each year is a layer
rasterize_dist_year_3161_500 <- function(poly, rast, sum_type, dir, res) {
  # Arguments
  ## poly: polygon to be rasterized
  ## rast: raster with resolution to match
  ## sum_type: whether to sum instances of polygons at single pixel
  ### Count = sum overlapping polygons to get count
  ### PA = overwrite overlapping polygons, returns presence/absence
  ## dir: Name of directory to be save in, also used in file name
  ## res: resolution of rast used for naming
  
  # check polygon class and convert to terra class if needed
  v <- if (class(poly)[1] != "SpatVector") {terra::vect(poly)} else {poly}
  # check raster class and convert to terra class if needed
  r <- if (class(rast)[1] != "SpatRaster") {terra::rast(rast)} else {rast}
  # add count variable
  # for presence/absence, 1 will be overwritten (see sum in terra::rasterize)
  # for counting instances, 1 value for each pixel will be summed
  v$count <- 1
  # convert polygon to raster
  add <- if (sum_type == "PA") FALSE else TRUE
  r <- terra::rasterize(v, r, "count", background = 0, sum = add)
  # crop to aou
  cr_r <- crop(r, aou_3161)
  # mask lakes 
  ma_r <- terra::mask(cr_r, aou_lakes_raster_3161_500)
  # mask values outside aou
  aou_ma_r <- terra::mask(ma_r, crop(aou_raster_3161_500, aou_3161))
  # write raster to disk
  # change path when needed
  terra::writeRaster(aou_ma_r, paste0(here("data/processed/Annual_Raster/"),
                                      dir, "/", dir, "_", sum_type, "_", 
                                      unique(poly$Year), "_", res, "m.tif"), 
                     overwrite = T)
}

## Stand Replacing 500m
dist_poly_sr_3161 <- get_annual_dist_list_stand(all_dist_3161, 2001, 3161, "SR")
dist_poly_sr_3161 %>% 
  walk(rasterize_dist_year_3161_500, rast = r_500_3161, sum_type = "PA", "SR_3161_500", "500_3161")

## Non stand replacing
dist_poly_nsr_3161 <- get_annual_dist_list_stand(all_dist_3161, 2001, 3161, "NSR")
dist_poly_nsr_3161 %>% 
  walk(rasterize_dist_year_3161_500, rast = r_500_3161, sum_type = "PA", "NSR_3161_500", "500_3161")


## All dist
dist_poly_all_dist_3161 <- get_annual_dist_list_all_dist(all_dist_3161, 2001, 3161, "All_Dist")
dist_poly_all_dist_3161 %>% 
  walk(rasterize_dist_year_3161_500, rast = r_500_3161, sum_type = "PA", "All_Dist_3161_500", "500_3161")

