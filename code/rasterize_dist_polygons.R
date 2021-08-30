# Title: Rasterize disturbance polygons
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Rasterize disturbance polygons

## For pixel-based analyses vector data need to be rasterized. This allows for 
## all layers to be stacked and analysed. Here, each of the forest disturbance 
## polygons from Ontario GeoHub are rasterized to match the downsampled C-flux 
## data. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

aou <- vect(here("data/raw/AoU.gpkg"))
cflux_30 <- rast(here("data/raw/aou_cflux_per_ha_30.tif"))
aou_rast <- rast(here("data/processed/aou_rast_30.tif"))
aou_lakes_rast <- rast(here("data/processed/aou_lakes_rast_30m.tif"))
all_dist <- vect(here("data/processed/all_dist_2001-2019.gpkg"))
fri_age_aou <- rast(here("data/processed/fri_age_aou.tif"))

# Rasterize disturbance polygons ------------------------------------------

## Taking all disturbance events from 2001 to 2019, sum the number of events
## that occurred in a given pixel. 
all_dist_raster <- rasterize(all_dist, cflux_30, "count", 
                             background = 0, sum = T)
## Clean rasterized disturbance totals by croping, removing lakes, and 
## pixels that fall outside AoU
dist_rast <- all_dist_raster %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast) %>% 
  # mask values outside aou
  terra::mask(., aou_rast) %>% 
  # mask to only vpixels with fri data
  terra::mask(., fri_age_aou)
plot(dist_rast)

# Save output -------------------------------------------------------------

writeRaster(dist_rast, here("data/processed/total_dist_rast_2001-2019.tif"), 
            overwrite = T)

