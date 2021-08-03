# Title: Create Admin, Dist, C-flux df 
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Collate admin polygon data to create dataframe

## To model the effects of land administration and disturbance history on C-flux
## these datesets need to be combined. Using admin zone polygons as the spatail 
## scale, disturbance history, C-flux, and admin zone type are combined into a 
## single df.


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)

# Load data ---------------------------------------------------------------

admin_zone_nolakes <- 
  # generated and validated using QGIS (faster than R in this case)
  st_read(here("data/processed/admin_zone_lakes-removed_valid.gpkg")) %>% 
  # calculate area of each polygon in ha (1 m2 = 0.0001 ha)
  mutate(area_poly_ha = as.numeric(st_area(.)*0.0001))

cflux_admin_ha_df <- read_csv(here("data/processed/cflux_admin_ha_extract.csv"))

admin_dist_df <- read_csv(here("data/processed/dist_admin_extract.csv"))


# Constants ---------------------------------------------------------------

zone_to_remove <- c("National Park", "Wilderness Area", 
                    "National Marine Conservation Area", "Forest Reserve",
                    "National Wildlife Area", "Protected Area - Far North")


# Create df ---------------------------------------------------------------

admin_cflux_dist_ha_df <- admin_zone_nolakes %>% 
  st_centroid() %>% 
  mutate(lat = st_coordinates(.)[,2],
         lon = st_coordinates(.)[,1]) %>% 
  st_set_geometry(NULL) %>% 
  left_join(., cflux_admin_ha_df) %>% 
  left_join(., admin_dist_df) %>% 
  filter(!zone %in% zone_to_remove) %>% 
  mutate(
         # combine recommended provincial parks and recommended conservation 
         # reserves with standard equivalents
         zone = str_replace(zone, "Recommended ", ""),
         zone = factor(zone, 
                       levels = c("General Use Area", 
                                  "Enhanced Management Area",
                                  "Indian Reserve",
                                  "Provincial Park",
                                  "Conservation Reserve"))) %>% 
  na.omit() %>% 
  filter(cflux_mean != 0) %>% 
  mutate(zone = as.character(zone),
         zone = if_else(zone == "Indian Reserve", "Indigenous Reserve", zone),
         zone = as.factor(zone))

# Save output -------------------------------------------------------------

write_csv(admin_cflux_dist_ha_df, 
          here("data/processed/admin_cflux_dist_poly_ha_df.csv"))

# Explore - remove for proper explore script
admin_in_aou <- st_read(here("tmp/admin_zones_within_aou.gpkg")) %>% 
  st_set_geometry(NULL) %>% 
  pull(ID)

admin_zone <- st_read(here("data/processed/admin_zones_valid.gpkg")) %>% 
  mutate(full_area_poly_ha = as.numeric(st_area(.)*0.0001))

admin_zone %>% 
  st_set_geometry(NULL) %>% 
  left_join(., admin_zone_nolakes %>% 
              st_set_geometry(NULL)) %>% 
  mutate(water_area = full_area_poly_ha - area_poly_ha,
         prop_water = water_area/full_area_poly_ha,
         prop_land = area_poly_ha/full_area_poly_ha) %>% 
  filter(ID %in% admin_in_aou) %>% pull(prop_water) %>% hist

admin_cflux_dist_df %>% 
  mutate(diff = cflux_per_ha_cell - cflux_per_ha_poly) %>% view


colSums(is.na(admin_cflux_dist_df))



