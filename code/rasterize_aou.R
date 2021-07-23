# Title: Rasterize AoU Poly and Lakes
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Rasterize AoU and lakes with

## The Area of Undertaking (AoU) represents the area of Ontario that has been 
## surveyed for disturbance over the last several decades. All analyses need to 
## be bound within the AoU. Here, the AoU is resterized to provide a template of
## the extent of the analysis. Likewise, the analysis is only approporite for 
## pixels that fall on land. Therefore, the polygons of all lakes/waterbodies 
## that fall within the AoU are also rasterized for future masking. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

aou <- vect(here("data/raw/AoU.gpkg"))
aou_lakes <- vect(here("data/raw/AoU_Lakes.gpkg"))
cflux_250 <- rast(here("data/processed/aou_cflux_250.tif"))


# Functions ---------------------------------------------------------------

raster_aou <- function(pol, ras) {
  # Arguments: 
  # pol: polygon to rasterize
  # ras: raster to use as template for rasterizing
  # 
  ## add "count" to polygon of interest as single ID for polygons
  pol$count <- 1
  ## reproject polygons to raster crs
  p <- pol %>% 
    terra::project(., terra::crs(ras))
  ## rasterize, sum = F returns presence/absence
  r <- rasterize(p, ras, "count", background = 0, sum = F)
  ## crop to AoU extent
  out <- terra::crop(r, aou)
  return(out)
}



# Rasterize aou and lakes -------------------------------------------------

aou_rast <- raster_aou(aou, cflux_250)
plot(aou_rast)

aou_lakes_rast <- raster_aou(aou_lakes, cflux_250)
aou_lakes_rast[aou_lakes_rast == 1] <- NA
plot(aou_lakes_rast)



# Save outputs ------------------------------------------------------------
writeRaster(aou_rast, here("data/processed/aou_rast_250m.tif"))
writeRaster(aou_lakes_rast, here("data/processed/aou_lakes_rast_250m.tif"))




