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
admin_poly <- vect(here("data/processed/admin_zones.gpkg"))

aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))
admin_poly_3161 <- vect(here("data/processed/admin_poly_3161.gpkg"))


cflux_250 <- rast(here("data/processed/aou_cflux_250m.tif"))
aou_rast <- rast(here("data/processed/aou_rast_250m.tif"))
aou_lakes_rast <- rast( here("data/processed/aou_lakes_rast_250m.tif"))

cflux_30 <- rast(here("data/raw/aou_cflux_per_ha_30.tif"))
aou_rast_30 <- rast(here("data/processed/aou_rast_30.tif"))
aou_lakes_rast_30 <- rast(here("data/processed/aou_lakes_rast_30m.tif"))

cflux_250_ha_3161 <- rast(here("data/processed/aou_cflux_250m_ha_3161.tif"))
aou_rast_3161 <- rast(here("data/processed/aou_rast_250_3161.tif"))
aou_lakes_rast_3161 <- rast(here("data/processed/aou_lakes_rast_250m_3161.tif"))


# Rasterize admin zone ----------------------------------------------------

## 250 m cflux raster
admin_raster <- rasterize(admin_poly, cflux_250, "zone_num")

admin_rast <- admin_raster %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast) %>% 
  # mask values outside aou
  terra::mask(., aou_rast)
plot(admin_rast)

## 30 M raster
sub_admin_poly <- subset(admin_poly, 
                         admin_poly$zone %in% c("General Use Area",
                                              "Enhanced Management Area",
                                              "Indian Reserve",
                                              "Provincial Park",
                                              "Conservation Reserve")) 

admin_raster_30 <- rasterize(sub_admin_poly, cflux_30, "zone_num")

admin_rast_30 <- admin_raster_30 %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_30) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_30)
plot(admin_rast)

## 250 m cflux raster epsg3161
admin_raster_3161 <- rasterize(admin_poly_3161, cflux_250_ha_3161, "zone_num")

admin_rast_3161 <- admin_raster_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161)
plot(admin_rast_3161)

# Save output -------------------------------------------------------------

writeRaster(admin_rast, here("data/processed/admin_zone_rast_250m.tif"))
writeRaster(admin_rast_30, here("data/processed/admin_zone_rast_30m.tif"))
writeRaster(admin_rast_3161, here("data/processed/admin_zone_rast_250m_3161.tif"))

