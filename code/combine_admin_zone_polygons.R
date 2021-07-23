# Title: Combine admin zone polygons
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Combine forest disturbance polgons to one file

## Land administration zone across Ontario are stored as separate polygon files. 
## Here, we combine the 4 type of land admin zones that are within the AoU


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)
library(sf)

# Load data ---------------------------------------------------------------

crown_land <- st_read(here("data/processed/Crown_Land_Designations_AoU.gpkg"))
ind_mod <- st_read(here("data/processed/Indigenous_Land_Modified_clean_AoU.gpkg"))
ind_con <- st_read(here("data/processed/Indigenous_Land_Confirmed_clean_AoU.gpkg"))
nat_parks <- st_read(here("data/processed/National_Parks_clean_AoU.gpkg"))


# Clean polygons ----------------------------------------------------------

cr <- crown_land %>% 
  dplyr::select(OGF_ID, DESIGNATIO, geom) %>% 
  mutate(OGF_ID = paste0("CR_", OGF_ID)) %>% 
  rename(ID = OGF_ID, zone = DESIGNATIO) %>% 
  st_transform(4269)

im <- ind_mod %>% 
  mutate(ID = 1:nrow(.), 
         ID = paste0("IN-M_", ID),
         zone = "Indian Reserve") %>% 
  dplyr::select(ID, zone, geom) %>% 
  st_transform(4269)

ic <- ind_con %>% 
  mutate(ID = 1:nrow(.), 
         ID = paste0("IN-C_", ID),
         zone = "Indian Reserve") %>% 
  dplyr::select(ID, zone, geom) %>% 
  st_transform(4269)

np <- nat_parks %>% 
  dplyr::select(OGF_ID, PROTECTED_, geom) %>% 
  mutate(OGF_ID = paste0("NP_", OGF_ID)) %>% 
  rename(ID = OGF_ID, zone = PROTECTED_) %>% 
  st_transform(4269)

# Combine polygons --------------------------------------------------------

admin_poly <- bind_rows(cr, im, ic, np) %>% 
  # set NA as General Use Area
  mutate(zone = replace_na(zone, "General Use Area"),
         # set each admin zone as a number for future rasterizing
         zone_num = as.numeric(as.factor(zone))) %>% 
  vect(.)


# Create admin zone number key --------------------------------------------

admin_zone_key <- admin_poly[, c(2,3)] %>% 
  as.data.frame() %>% 
  distinct() %>% 
  arrange(zone)

# Save output -------------------------------------------------------------

writeVector(admin_poly, here("data/processed/admin_zones.gpkg"),
            filetype = "GPKG")

write_csv(admin_zone_key, here("data/processed/admin_zone_key.csv"))
