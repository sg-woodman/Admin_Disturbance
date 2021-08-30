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

fri_age <- rast(here("data/processed/fri_oyrorg_30.tif"))
aou <- vect(here("data/raw/AoU.gpkg"))
aou_rast <- rast(here("data/processed/aou_rast_30.tif"))
aou_lakes_rast <- rast(here("data/processed/aou_lakes_rast_30m.tif"))

# Clean raster ------------------------------------------------------------

fri_age_aou <- fri_age %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast) %>% 
  # mask values outside aou
  terra::mask(., aou_rast)

fri_age_aou[fri_age_aou == 0] <- NA

writeRaster(fri_age_aou, here("data/processed/fri_age_aou.tif"), overwrite = T)



