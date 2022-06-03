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
cflux_30 <- rast(here("data/raw/aou_cflux_per_ha_30.tif"))

aou_3161 <- vect(here("data/processed/aou_3161.gpkg"))
aou_lakes_3161 <- vect(here("data/processed/aou_lakes_3161.gpkg"))
cflux_250_ha_3161 <- rast(here("data/processed/aou_cflux_250m_ha_3161.tif"))



# Functions ---------------------------------------------------------------

raster_aou <- function(pol, ras, aou) {
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

aou_rast <- raster_aou(aou, cflux_30, aou)
aou_rast[aou_rast == 0] <- NA
ext(aou_rast)
plot(aou_rast)

aou_lakes_rast <- raster_aou(aou_lakes, cflux_30, aou)
aou_lakes_rast[aou_lakes_rast == 1] <- NA
ext(aou_lakes_rast)
plot(aou_lakes_rast)

aou_rast_3161 <- raster_aou(aou_3161, cflux_250_ha_3161, aou_3161)
aou_rast_3161[aou_rast_3161 == 0] <- NA
ext(aou_rast_3161)
plot(aou_rast_3161)

aou_lakes_rast_3161 <- raster_aou(aou_lakes_3161, cflux_250_ha_3161, aou_3161)
aou_lakes_rast_3161[aou_lakes_rast_3161 == 1] <- NA
ext(aou_lakes_rast_3161)
plot(aou_lakes_rast_3161)

# Save outputs ------------------------------------------------------------
writeRaster(aou_rast, here("data/processed/aou_rast_30.tif"), overwrite = T)
writeRaster(aou_lakes_rast, here("data/processed/aou_lakes_rast_30m.tif"))

writeRaster(aou_rast_3161, here("data/processed/aou_rast_250_3161.tif"), overwrite = T)
writeRaster(aou_lakes_rast_3161, here("data/processed/aou_lakes_rast_250m_3161.tif"))

