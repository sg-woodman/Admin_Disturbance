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
r_250 <- rast(here("data/raw/raster_250m_template.tif")) # originally from MODIS pheno data

# Resample raster ---------------------------------------------------------

cflux_250 <- terra::resample(cflux_30, r_250, method = "bilinear")

cflux_250 <- terra::crop(cflux_250, aou)

# Save output -------------------------------------------------------------

writeRaster(cflux_250, here("data/processed/aou_cflux_250.tif"), overwrite = T)

