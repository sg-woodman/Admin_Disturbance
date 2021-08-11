# Title: Combine extracted disturbance
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Combine extracted disturbances within admin zones

## Calculating the area/proportion of each land administration zone that is 
## disturbed each year is slow and requires parallelizing on the Plant Science
## Cluster. Here, the separate runs of the intersection and area calculation 
## are combined to create a single df where the mean proportion of admin zones
## disturbed across 2001-2019 is calculated. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)

# Constants ---------------------------------------------------------------

zone_to_remove <- c("National Park", "Wilderness Area", 
                    "National Marine Conservation Area", "Forest Reserve",
                    "National Wildlife Area", "Protected Area - Far North")

# Load data ---------------------------------------------------------------

admin_dist_full_df <- list.files(path = here("data/processed/"),
           pattern = "admin_zone_dist_\\d.rds", 
           full.names = T) %>% 
  # load RDS objects
  map(readRDS) %>% 
  # select only the results (i.e. not errors) from second level of list
  map_depth(., 2, "result") %>% 
  # remove all elements that are NULL
  map_depth(., 2, compact) %>% 
  # bind all elements in to a df
  dplyr::bind_rows()
  
# Process dist in admin ---------------------------------------------------

## create skeleton df for all years and admin zones IDs
## used to join data so years without disturbance are included. 
skeleton_df <- expand_grid(Year = seq(2001, 2019, 1), 
                           ID = unique(admin_dist_full_df$ID)) %>% 
  left_join(., admin_dist_full_df %>% 
              dplyr::select(ID, zone, zone_num, area_ha) %>% 
              distinct())

admin_dist_df <- admin_dist_full_df %>% 
  group_by(ID, zone, Year) %>% 
  # calc mean area (constant), total area disturbed, and no. of events within
  # each unique zone and year
  summarise(area_ha = mean(area_ha),
            dist_area = sum(dist_area),
            count = sum(count),
            .groups = "drop") %>% 
  # calc prop are disturbed
  mutate(prop_dist = dist_area/area_ha,
         # if prop disturbed is greater than 1 replace with 1
         # occurs when multiple disturbances occur in a given year
         prop_dist = if_else(prop_dist > 1, 1, prop_dist),
         # calc binary count of dist for each year
         n_dist = if_else(count > 0, 1, 0)) %>% 
  # join to skeleton df
  full_join(., skeleton_df) %>% 
  # fill missing values created during join with 0
  mutate(dist_area = replace_na(dist_area, 0),
         prop_dist = replace_na(prop_dist, 0),
         n_dist = replace_na(n_dist, 0)) %>%
  group_by(ID, zone) %>% 
  summarise(area_ha = mean(area_ha),
            mean_prop_dist = mean(prop_dist, na.rm = T),
            med_prop_dist = median(prop_dist, na.rm = T),
            var_prop_dist = var(prop_dist, na.rm = T),
            min_prop_dist = min(prop_dist, na.rm = T),
            max_prop_dist = max(prop_dist, na.rm = T),
            sum_prop_dist = sum(prop_dist, na.rm = T),
            n_dist = sum(n_dist)) %>% 
  # remove admin zones that are not prevalent 
  filter(!zone %in% zone_to_remove) %>% 
  # combine recommended provincial parks and conservation areas with thier
  # full status equivalents
  mutate(zone = str_replace(zone, "Recommended ", ""),
         zone = as.factor(zone))

## proportion of each admin zone disturbed for each year from 2001 to 2019
## annual proportion disturbed is also needed to determine if there is 
## temporal synchrony of disturbance. 
admin_dist_df_year <- admin_dist_full_df %>% 
  group_by(ID, zone, Year) %>% 
  # calc mean area (constant), total area disturbed, and no. of events within
  # each unique zone and year
  summarise(area_ha = mean(area_ha),
            dist_area = sum(dist_area),
            count = sum(count),
            .groups = "drop") %>% 
  # calc prop are disturbed
  mutate(prop_dist = dist_area/area_ha,
         # if prop disturbed is greater than 1 replace with 1
         # occurs when multiple disturbances occur in a given year
         prop_dist = if_else(prop_dist > 1, 1, prop_dist),
         # calc binary count of dist for each year
         n_dist = if_else(count > 0, 1, 0)) %>% 
  # join to skeleton df
  full_join(., skeleton_df) %>% 
  # fill missing values created during join with 0
  mutate(dist_area = replace_na(dist_area, 0),
         prop_dist = replace_na(prop_dist, 0),
         n_dist = replace_na(n_dist, 0)) %>%
  # remove admin zones that are not prevalent 
  filter(!zone %in% zone_to_remove) %>% 
  # combine recommended provincial parks and conservation areas with thier
  # full status equivalents
  mutate(zone = str_replace(zone, "Recommended ", ""),
         zone = as.factor(zone))

# Save output -------------------------------------------------------------

write_csv(admin_dist_df, here("data/processed/dist_admin_extract.csv"))

write_csv(admin_dist_df_year, here("data/processed/dist_admin_extract_year.csv"))
