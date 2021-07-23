# Title: Create Admin, Dist, C-flux df
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Rasterize disturbance polygons

## To model the effects of land administration and disturbance history on C-flux
## these datesets need to be combined. Using the 250m rasters of admin zones, 
## total disturbance, and C-flux generated using scripts in the code directory, 
## these datasets will be stacked and converted to a dataframe. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

cflux_250 <- rast(here("data/processed/aou_cflux_250m.tif"))
admin_rast_250 <- rast(here("data/processed/admin_zone_rast_250m.tif"))
dist_rast_250 <- rast(here("data/processed/total_dist_rast_2001-2019.tif"))
admin_zone_key <- read_csv(here("data/processed/admin_zone_key.csv"))

# Stack rasters -----------------------------------------------------------

cad_rast <- c(cflux_250, admin_rast_250, dist_rast_250)

cad_df <- terra::as.data.frame(cad_rast, xy = T, na.rm = T) %>% 
  rename(lat = y, lon = x, cflux = cflux_masked_aou) %>% 
  left_join(., admin_zone_key, by = c("zone_num"))

# Save output -------------------------------------------------------------

write_csv(cad_df, here("data/processed/cflux_admin_dist_df.csv"))

