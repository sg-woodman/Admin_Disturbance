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

cflux_500_ha_3161 <- rast(here("data/processed/aou_cflux_500m_ha_3161.tif"))
aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))
aou_rast_3161_500 <- rast(here("data/processed/aou_rast_500_3161.tif"))
aou_lakes_rast_3161_500 <- rast(here("data/processed/aou_lakes_rast_500m_3161.tif"))

mean_t_500_3161 <- rast(here("data/processed/mean_t_500_3161.tif"))

human_footprint_500_3161 <- rast(here("data/processed/human_footprint_500_3161.tif"))

harvest_500_3161 <- rast(here("data/processed/harvest_500_3161.tif"))

evi_500_3161 <-  rast(here("data/processed/evi_500_3161.tif"))

evi_sen_500_3161 <- rast(here("data/processed/evi_sen_500_3161.tif"))

overstory_500_3161 <- rast(here("data/processed/overstory_500_3161.tif"))

understory_500_3161 <- rast(here("data/processed/understory_500_3161.tif"))



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

cflux_500_aou_3161 <- cflux_500_ha_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(cflux_500_aou_3161)

mean_t_500_aou_3161 <- mean_t_500_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(mean_t_500_aou_3161)

human_footprint_500_aou_3161 <- human_footprint_500_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(human_footprint_500_aou_3161)

harvest_500_aou_3161 <- harvest_500_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(harvest_500_aou_3161)

evi_500_aou_3161 <- evi_500_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(evi_500_aou_3161)

evi_sen_500_aou_3161 <- evi_sen_500_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

overstory_500_aou_3161 <- overstory_500_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(overstory_500_aou_3161)

understory_500_aou_3161 <- understory_500_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161_500) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161_500) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(understory_500_aou_3161)




# Save output -------------------------------------------------------------

writeRaster(cflux_30_aou, here("data/processed/cflux_per_ha_30_aou.tif"), overwrite = T)

writeRaster(cflux_250_aou_3161, here("data/processed/cflux_per_ha_250_aou_3161.tif"), overwrite = T)

writeRaster(cflux_500_aou_3161, here("data/processed/cflux_per_ha_500_aou_3161.tif"), overwrite = T)

writeRaster(mean_t_500_aou_3161, here("data/processed/mean_t_500_aou_3161.tif"), overwrite = T)

writeRaster(human_footprint_500_aou_3161, here("data/processed/human_footprint_500_aou_3161.tif"), overwrite = T)

writeRaster(harvest_500_aou_3161, here("data/processed/harvest_500_aou_3161.tif"), overwrite = T)

writeRaster(evi_500_aou_3161, here("data/processed/evi_500_aou_3161.tif"), overwrite = T)

writeRaster(evi_sen_500_aou_3161, here("data/processed/evi_sen_500_aou_3161.tif"), overwrite = T)

writeRaster(overstory_500_aou_3161, here("data/processed/overstory_500_aou_3161.tif"), overwrite = T)

writeRaster(understory_500_aou_3161, here("data/processed/eoverstory_500_aou_3161.tif"), overwrite = T)
