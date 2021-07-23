# Title: Rasterize Admin Zones
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Rasterize AoU and lakes with

## Administration zones represent the type of land management for area in Ontario
## Here, these admin zones are rasterized for eventual stacking with C-flux and 
## disturbance rasters. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

aou <- vect(here("data/raw/AoU.gpkg"))
cflux_250 <- rast(here("data/processed/aou_cflux_250m.tif"))
aou_rast <- rast(here("data/processed/aou_rast_250m.tif"))
aou_lakes_rast <- rast( here("data/processed/aou_lakes_rast_250m.tif"))
admin_poly <- vect(here("data/processed/admin_zones.gpkg"))


# Rasterize admin zone ----------------------------------------------------

admin_raster <- rasterize(admin_poly, cflux_250, "zone_num")

admin_rast <- admin_raster %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast) %>% 
  # mask values outside aou
  terra::mask(., aou_rast)
plot(admin_rast)


# Save output -------------------------------------------------------------

writeRaster(admin_rast, here("data/processed/admin_zone_rast_250m.tif"))
