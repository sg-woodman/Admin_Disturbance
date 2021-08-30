# Title: Clean FRI raster
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Remove lakes and crop to AoU 

## To ensure consitency across all raster datasets the same final processing
## steps are applied to all. 1) Remove pixels that are classified as lakes and 
## 2) crop to AoU. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

cflux_30 <- rast(here("data/raw/aou_cflux_per_ha_30.tif"))
aou <- vect(here("data/raw/AoU.gpkg"))
aou_rast <- rast(here("data/processed/aou_rast_30.tif"))
aou_lakes_rast <- rast(here("data/processed/aou_lakes_rast_30m.tif"))
fri_age_aou <- rast(here("data/processed/fri_age_aou.tif"))


# Clean C-flux ------------------------------------------------------------

cflux_30_aou <- cflux_30 %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast) %>% 
  # mask values outside aou
  terra::mask(., aou_rast) %>% 
  # mask to only vpixels with fri data
  terra::mask(., fri_age_aou)
plot(cflux_30_aou)

# Save output -------------------------------------------------------------

writeRaster(cflux_30_aou, here("data/processed/cflux_per_ha_30_aou.tif"))
