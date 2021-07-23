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
cflux_250 <- rast(here("data/processed/aou_cflux_250m.tif"))
aou_rast <- rast(here("data/processed/aou_rast_250m.tif"))
aou_lakes_rast <- rast( here("data/processed/aou_lakes_rast_250m.tif"))
all_dist <- vect(here("data/processed/all_dist_2001-2019.gpkg"))

# Rasterize disturbance polygons ------------------------------------------

## Taking all disturbance events from 2001 to 2019, sum the number of events
## that occurred in a given pixel. 
all_dist_raster <- rasterize(all_dist, cflux_250, "count", 
                             background = 0, sum = T)
## Clean rasterized disturbance totals by croping, removing lakes, and 
## pixels that fall outside AoU
dist_rast <- all_dist_raster %>% 
  terra::crop(., aou) %>%
  # mask lakes 
  terra::mask(., aou_lakes_rast) %>% 
  # mask values outside aou
  terra::mask(., aou_rast)
plot(dist_rast)

# Save output -------------------------------------------------------------

writeRaster(dist_rast, here("data/processed/total_dist_rast_2001-2019.tif"), 
            overwrite = T)

