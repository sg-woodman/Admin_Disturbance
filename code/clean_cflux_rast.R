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
admin_rast_30 <- rast(here("data/processed/admin_zone_rast_30m.tif"))

cflux_250_ha_3161 <- rast(here("data/processed/aou_cflux_250m_ha_3161.tif"))
aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))
aou_rast_3161 <- rast(here("data/processed/aou_rast_250_3161.tif"))
aou_lakes_rast_3161 <- rast(here("data/processed/aou_lakes_rast_250m_3161.tif"))

# Clean C-flux ------------------------------------------------------------

cflux_30_aou <- cflux_30 %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast) %>% 
  # mask values outside aou
  terra::mask(., aou_rast) #%>% 
  # mask admin zone not in study (e.g. Far north protected)
  #terra::mask(., admin_rast_30)

plot(cflux_30_aou)

cflux_250_aou_3161 <- cflux_250_ha_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(cflux_250_aou_3161)

# Save output -------------------------------------------------------------

writeRaster(cflux_30_aou, here("data/processed/cflux_per_ha_30_aou.tif"), overwrite = T)

writeRaster(cflux_250_aou_3161, here("data/processed/cflux_per_ha_250_aou_3161.tif"), overwrite = T)



