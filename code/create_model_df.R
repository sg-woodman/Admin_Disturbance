
# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)

# Load data ---------------------------------------------------------------

admin_250_df_3161 <- read_csv(here("data/processed/admin_250_3161_df.csv"))
cflux_250_df_3161 <- read_csv(here("data/processed/cflux_250_3161_df.csv"))
lc_250_df_3161 <-  read_csv(here("data/processed/landcover_250_3161_df.csv"))
dist_250_3161_df <- read_csv(here("data/processed/dist_250_3161_df.csv"))

## Key for admin zone numbers to names
key <- read_csv(here("data/processed/admin_zone_key.csv"))


# Make df -----------------------------------------------------------------

cad_df_3161 <- dist_250_3161_df %>% 
  left_join(., admin_250_df_3161 %>% 
              select(-c(x,y))) %>% 
  left_join(., cflux_250_df_3161 %>% 
              select(-c(x,y))) %>% 
  left_join(., lc_250_df_3161 %>% 
              select(-c(x,y))) %>% 
  left_join(., key) %>% 
  rename(cflux_ha = Hansen_cflux_ha_aou) %>% 
  mutate(
    # combine recommended provincial parks and recommended conservation 
    # reserves with standard equivalents
    zone = str_replace(zone, "Recommended ", "")) %>% 
  #filter(zone_num %in% c(1, 2, 4, 5, 10, 11, 12)) %>% 
  mutate(zone = relevel(as.factor(.$zone), "General Use Area"),
         LC = case_when(ON_Land_Cover == 1 ~ "Clear open water", 
                        ON_Land_Cover == 2 ~ "Turbid water",
                        ON_Land_Cover == 3 ~ "Shoreline",
                        ON_Land_Cover == 4 ~ "Mudflats",
                        ON_Land_Cover == 5 ~ "Marsh",
                        ON_Land_Cover == 6 ~ "Swamp",
                        ON_Land_Cover == 7 ~ "Fen",
                        ON_Land_Cover == 8 ~ "Bog",
                        ON_Land_Cover == 10 ~ "Heath",
                        ON_Land_Cover == 11 ~ "Sparse treed",
                        ON_Land_Cover == 12 ~ "Treed upland",
                        ON_Land_Cover == 13 ~ "Deciduous treed",
                        ON_Land_Cover == 14 ~ "Mixed treed",
                        ON_Land_Cover == 15 ~ "Coniferous treed",
                        ON_Land_Cover == 16 ~ "Plantations",
                        ON_Land_Cover == 17 ~ "Hedge rows",
                        ON_Land_Cover == 18 ~ "Disturbance",
                        ON_Land_Cover == 19 ~ "Open Cliff and Talus",
                        ON_Land_Cover == 20 ~ "Alvar",
                        ON_Land_Cover == 21 ~ "Sand Barren and Dune",
                        ON_Land_Cover == 22 ~ "Open Tallgrass Prairie",
                        ON_Land_Cover == 23 ~ "Tallgrass Savannah",
                        ON_Land_Cover == 24 ~ "Tallgrass Woodland",
                        ON_Land_Cover == 25 ~ "Sand Gravel Mine Tailings",
                        ON_Land_Cover == 26 ~ "Bedrock",
                        ON_Land_Cover == 27 ~ "Community Infrastructure",
                        ON_Land_Cover == 28 ~ "Agriculture",
                        ON_Land_Cover == -99 ~ "Other",
                        ON_Land_Cover == -9 ~ "Cloud",
                        TRUE ~ "NA"),
         Forest = case_when(ON_Land_Cover == 11 ~ "Treed",
                            ON_Land_Cover == 12 ~ "Treed",
                            ON_Land_Cover == 13 ~ "Treed",
                            ON_Land_Cover == 14 ~ "Treed",
                            ON_Land_Cover == 15 ~ "Treed",
                            ON_Land_Cover == 16 ~ "Treed",
                            ON_Land_Cover == 17 ~ "Treed",
                            TRUE ~ "Non-treed"))

cad_df_3161 %>% 
  group_by(LC) %>% tally()

cad_df_3161 %>% 
  group_by(Forest) %>% tally()


# Save output -------------------------------------------------------------
write_csv(cad_df_3161, here("data/processed/cflux_admin_dist_pixel_250m_df_3161.csv"))



## Other dist process code
## Reshape df so year is a single column
dist_rast_df_long <- dist_250_3161_df %>% 
  rename_with(.fn = ~ str_replace(.x, "count_", ""),
              .cols = starts_with("count_")) %>% 
  pivot_longer(cols = 4:22, names_to = "year", values_to = "dist")

## Calculate when the most recent disturbance occurred in a pixel
most_recent_dist <- dist_rast_df_long %>% 
  filter(dist != 0) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(cell) %>% 
  slice(which.max(year))

most_recent_dist %>% 
  ggplot() + 
  geom_histogram(aes(x = year), binwidth = 1)

most_recent_dist %>% 
  group_by(year) %>% 
  tally

## Count number of disturbance that occurred in a pixel during the record
n_dist <- dist_rast_df_long %>% 
  filter(dist != 0) %>% 
  mutate(year = as.numeric(year)) %>% 
  group_by(cell) %>% 
  summarise(total_dist = sum(dist))

n_dist %>% 
  ggplot() + 
  geom_histogram(aes(x = total_dist))

n_dist %>% 
  group_by(total_dist) %>% 
  tally

## Disturbane df where the year of most recent disturbance and number of 
## disturbances is included. 
dist_df <- dist_raster_df_3161 %>% 
  dplyr::select(cell, x, y) %>% 
  left_join(., most_recent_dist %>% 
              dplyr::select(-dist)) %>% 
  left_join(., n_dist) %>% 
  mutate(recent_dist_year = replace_na(year, 2000),
         total_dist = replace_na(total_dist, 0)) %>% 
  select(-year)

