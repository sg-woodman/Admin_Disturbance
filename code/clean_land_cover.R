# Title: Clean Land Cover raster
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

land_cover_250_3161 <- rast(here("data/processed/land_cover_250_3161.tif"))
aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))
aou_rast_3161 <- rast(here("data/processed/aou_rast_250_3161.tif"))
aou_lakes_rast_3161 <- rast(here("data/processed/aou_lakes_rast_250m_3161.tif"))

# Clean C-flux ------------------------------------------------------------



land_cover_aou_250_3161 <- land_cover_250_3161 %>% 
  terra::crop(., aou_3161) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast_3161) %>% 
  # mask values outside aou
  terra::mask(., aou_rast_3161) #%>% 
# mask admin zone not in study (e.g. Far north protected)
#terra::mask(., admin_rast_30)

plot(land_cover_aou_250_3161)

# Save output -------------------------------------------------------------


writeRaster(land_cover_aou_250_3161, here("data/processed/land_cover_aou_250_3161.tif"), overwrite = T)



