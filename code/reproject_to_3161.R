# Title: Project data to 3161
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Rasterize disturbance polygons

## For modelling having the data in metres rather than lat/long can be useful.
## This is likely important (though not critical) for creating the sdmTMB mesh.
## Using the resampled 250m C flux raster, all vectors need to be converted to 
## 3161 before rasterizing.  

# Packages ----------------------------------------------------------------
library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

## Rasters
cflux_250_ha <- rast(here("data/processed/aou_cflux_250m_ha.tif"))
cflux_30_ha <- rast(here("data/raw/aou_cflux_per_ha_30.tif"))

## Vectors
aou <- vect(here("data/raw/AoU.gpkg"))
admin_poly <- vect(here("data/processed/admin_zones.gpkg"))
aou_lakes <- vect(here("data/raw/AoU_Lakes.gpkg"))
all_dist <- vect(here("data/processed/all_dist_2001-2019.gpkg"))

# project from lat/long to metres
epsg_3161 <- rgdal::make_EPSG() %>% filter(code == 3161) %>% pull(prj4)



# Reproject ---------------------------------------------------------------

cflux_250_ha_3161 <- project(cflux_250_ha, epsg_3161)

cflux_30_ha_3161 <- project(cflux_30_ha, epsg_3161)

aou_3161 <- project(aou, epsg_3161)

aou_lakes_3161 <- project(aou_lakes, epsg_3161)

admin_poly_3161 <- project(admin_poly, epsg_3161)

all_dist_3161 <- project(all_dist, epsg_3161)



# Save output -------------------------------------------------------------

writeRaster(cflux_250_ha_3161, here("data/processed/aou_cflux_250m_ha_3161.tif"),
            overwrite = T)

writeRaster(cflux_30_ha_3161, here("data/processed/aou_cflux_30m_ha_3161.tif"),
            overwrite = T)

writeVector(aou_3161, here("data/processed/aou_3161.gpkg"))

writeVector(aou_lakes_3161, here("data/processed/aou_lakes_3161.gpkg"))

writeVector(admin_poly_3161, here("data/processed/admin_poly_3161.gpkg"))

writeVector(all_dist_3161, here("data/processed/all_dist_3161.gpkg"))
