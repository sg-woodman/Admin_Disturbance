# Title: Downscale C-flux data
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Reduce resolution of 30m C-flux raster to 250m 

## Analyses on 30m rasters for the entire study area results in over 2 billion
## pixels. This makes any subsequebt analyses slow and impractical. Therefore, 
## downsampling to 250m is more appropriate. 250m was selected as this was the 
## resolution of the belowground C data. 



# Packages ----------------------------------------------------------------

library(tidyverse, warn.conflicts = F)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

aou <- vect(here("data/raw/AoU.gpkg"))
cflux_30 <- rast(here("data/raw/aou_cflux_30.tif"))
cflux_30_ha <- rast(here("data/processed/cflux_per_ha_30_aou.tif"))
r_250 <- rast(here("data/raw/raster_250m_template.tif")) # originally from MODIS pheno data

aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))
cflux_30_ha_3161 <- rast(here("data/processed/aou_cflux_30m_ha_3161.tif"))

land_cover_3161 <- rast("/Volumes/Backup/Large_Files/ON_Land_Cover.tif")



# Create raster template --------------------------------------------------

epsg_3161 <- rgdal::make_EPSG() %>% filter(code == 3161) %>% pull(prj4)

r_250_3161 <- rast(res = c(250,250),
                   extent = ext(cflux_30_ha_3161),
                   crs = epsg_3161, 
                   vals = 1)

writeRaster(r_250_3161, here("data/raw/raster_250_tamplate_3161.tif"))

# Resample raster ---------------------------------------------------------

cflux_250 <- terra::resample(cflux_30, r_250, method = "bilinear")

cflux_250 <- terra::crop(cflux_250, aou)

cflux_250_ha <- terra::resample(cflux_30_ha, r_250, method = "bilinear")

cflux_250_ha <- terra::crop(cflux_250_ha, aou)

cflux_250_ha_3161 <- terra::resample(cflux_30_ha_3161, r_250_3161, 
                                     method = "bilinear")

cflux_250_ha_3161 <- terra::crop(cflux_250_ha_3161, aou_3161)

land_cover_250_3161 <- terra::resample(land_cover_3161, r_250_3161, 
                                     method = "near")

land_cover_250_3161 <- terra::crop(land_cover_250_3161, aou_3161)
# Save output -------------------------------------------------------------

writeRaster(cflux_250, here("data/processed/aou_cflux_250m.tif"), overwrite = T)

writeRaster(cflux_250_ha, here("data/processed/aou_cflux_250m_ha.tif"), overwrite = T)

writeRaster(cflux_250_ha_3161, here("data/processed/aou_cflux_250m_ha_3161.tif"), overwrite = T)

writeRaster(land_cover_250_3161, here("data/processed/land_cover_250_3161.tif"), overwrite = T)
