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

mean_t_3161 <- rast(here("data/raw/mean_t_01_19.tif"))

human_footprint_3161 <- rast(here("data/raw/ontario_human_footprint.tif"))

harvest_3161 <- rast(here("data/raw/harvest_3161.tif"))

evi_3161 <- rast(here("data/raw/mean_evi_3161.tif"))

evi_sen_3161 <- rast(here("data/raw/evi_sen_slope_3161.tif"))

fri_overstory_3161 <- rast(here("data/processed/fri_overstory_3161.tif"))

fri_understory_3161 <- rast(here("data/processed/fri_understory_3161.tif"))


# Create raster template --------------------------------------------------

epsg_3161 <- rgdal::make_EPSG() %>% filter(code == 3161) %>% pull(prj4)

r_250_3161 <- rast(res = c(250,250),
                   extent = ext(cflux_30_ha_3161),
                   crs = epsg_3161, 
                   vals = 1)

writeRaster(r_250_3161, here("data/raw/raster_250_tamplate_3161.tif"))

r_500_3161 <- rast(res = c(500,500),
                   extent = ext(cflux_30_ha_3161),
                   crs = epsg_3161, 
                   vals = 1)

writeRaster(r_500_3161, here("data/raw/raster_500_tamplate_3161.tif"))

# Resample raster ---------------------------------------------------------


cflux_250 <- terra::resample(cflux_30, r_250, method = "bilinear")

cflux_250 <- terra::crop(cflux_250, aou)

cflux_250_ha <- terra::resample(cflux_30_ha, r_250, method = "bilinear")

cflux_250_ha <- terra::crop(cflux_250_ha, aou)

## 3161; 250m
cflux_250_ha_3161 <- terra::resample(cflux_30_ha_3161, r_250_3161, 
                                     method = "bilinear")

cflux_250_ha_3161 <- terra::crop(cflux_250_ha_3161, aou_3161)

land_cover_250_3161 <- terra::resample(land_cover_3161, r_250_3161, 
                                     method = "near")

land_cover_250_3161 <- terra::crop(land_cover_250_3161, aou_3161)

## 3161; 500m
cflux_500_ha_3161 <- terra::resample(cflux_30_ha_3161, r_500_3161, 
                                     method = "bilinear")

cflux_500_ha_3161 <- terra::crop(cflux_500_ha_3161, aou_3161)
##
land_cover_500_3161 <- terra::resample(land_cover_3161, r_500_3161, 
                                       method = "near")

land_cover_500_3161 <- terra::crop(land_cover_500_3161, aou_3161)
##
mean_t_500_3161 <- terra::resample(mean_t_3161, r_500_3161, 
                                       method = "bilinear")

mean_t_500_3161 <- terra::crop(mean_t_500_3161, aou_3161)
##
human_footprint_500_3161 <- terra::resample(human_footprint_3161, r_500_3161, 
                                   method = "bilinear")

human_footprint_500_3161 <- terra::crop(human_footprint_500_3161, aou_3161)
##
harvest_500_3161 <- terra::resample(harvest_3161, r_500_3161, 
                                            method = "near")

harvest_500_3161 <- terra::crop(harvest_500_3161, aou_3161)
##
evi_500_3161 <- terra::resample(evi_3161, r_500_3161, 
                                    method = "bilinear")

evi_500_3161 <- terra::crop(evi_500_3161, aou_3161)
##
evi_sen_500_3161 <- terra::resample(evi_sen_3161, r_500_3161, 
                                method = "bilinear")

evi_sen_500_3161 <- terra::crop(evi_sen_500_3161, aou_3161)
##
overstory_500_3161 <- terra::resample(fri_overstory_3161, r_500_3161, 
                                    method = "bilinear")

overstory_500_3161 <- terra::crop(overstory_500_3161, aou_3161)
##
understory_500_3161 <- terra::resample(fri_understory_3161, r_500_3161, 
                                      method = "bilinear")

understory_500_3161 <- terra::crop(understory_500_3161, aou_3161)
# Save output -------------------------------------------------------------

writeRaster(cflux_250, here("data/processed/aou_cflux_250m.tif"), overwrite = T)

writeRaster(cflux_250_ha, here("data/processed/aou_cflux_250m_ha.tif"), overwrite = T)

writeRaster(cflux_250_ha_3161, here("data/processed/aou_cflux_250m_ha_3161.tif"), overwrite = T)

writeRaster(land_cover_250_3161, here("data/processed/land_cover_250_3161.tif"), overwrite = T)

writeRaster(cflux_500_ha_3161, here("data/processed/aou_cflux_500m_ha_3161.tif"), overwrite = T)

writeRaster(land_cover_500_3161, here("data/processed/land_cover_500_3161.tif"), overwrite = T)

writeRaster(mean_t_500_3161, here("data/processed/mean_t_500_3161.tif"), overwrite = T)

writeRaster(human_footprint_500_3161, here("data/processed/human_footprint_500_3161.tif"), overwrite = T)

writeRaster(harvest_500_3161, here("data/processed/harvest_500_3161.tif"), overwrite = T)

writeRaster(evi_500_3161, here("data/processed/evi_500_3161.tif"), overwrite = T)

writeRaster(evi_sen_500_3161, here("data/processed/evi_sen_500_3161.tif"), overwrite = T)

writeRaster(overstory_500_3161, here("data/processed/overstory_500_3161.tif"), overwrite = T)

writeRaster(understory_500_3161, here("data/processed/understory_500_3161.tif"), overwrite = T)



## Resample harvest tp 30

cflux_30_ha_3161 <- rast(here("data/processed/aou_cflux_30m_ha_3161.tif"))


harvest_3161 <- rast(here("data/raw/harvest_3161.tif"))

harvest_30_3161 <- terra::resample(harvest_3161, cflux_30_ha_3161, method = "near")

harvest_30_3161[harvest_30_3161 > 0] <- NA

aou_3161 <- vect(here("data/processed/"))

harvest_30_3161_aou <- harvest_30_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161)
plot(harvest_30_3161)

writeRaster(harvest_30_3161, here("data/processed/harvest_mask_30_3161.tif"))

