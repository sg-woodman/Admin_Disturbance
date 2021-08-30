# Title: Extract C-flux in admin
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Summarize 30m C-flux data within admin polygons

## To summarize the flux of C from forest in Ontario we need to select the 
## appropriate spatial scale for the analysis. Here, we have selected 
## administrative zones available GeoHub. Using exactextractr we can calculate 
## the mean, median, sum, etc. of C-flux in each catchment. The Hansen C-flux 
## data comes in two forms: per pixel and per hectacre. First attempts used per 
## pixel and calculated per ha but once this approach was shown to be useful the
## per ha data was downloaded and extracted. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(raster)
library(sf)
library(exactextractr)

# Load data ---------------------------------------------------------------

cflux_ha_30 <- raster(here("data/processed/cflux_per_ha_30_aou.tif"))
admin_zone_nolakes <- 
  # generated and validated using QGIS (faster than R in this case)
  st_read(here("data/processed/admin_zone_lakes-removed_valid.gpkg")) %>% 
  # calculate area of each polygon in ha (1 m2 = 0.0001 ha)
  mutate(area_poly_ha = as.numeric(st_area(.)*0.0001))

# Extract  ----------------------------------------------------------------

## per ha extraction
cflux_admin_ha <- exact_extract(cflux_ha_30, 
                             admin_zone_nolakes,
                             c("mean", "sum", "median", "variance", "count"))

colnames(cflux_admin_ha) <- paste("cflux", colnames(cflux_admin_ha), sep="_")

cflux_admin_ha_df <- admin_zone_nolakes %>%
  st_set_geometry(NULL) %>%
  bind_cols(., cflux_admin_ha)

write_csv(cflux_admin_ha_df, here("data/processed/cflux_admin_ha_extract.csv"))
