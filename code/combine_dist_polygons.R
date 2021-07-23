# Title: Combine disturbance polygons
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Combine forest disturbance polgons to one file

## Forest disturbance polygons for Ontario are split by disturbance type. Here, 
## the separate disturbance polygons are combinded into a single .gpkg file with 
## the type of disturbnace and year as attributes


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(terra)

# Load data ---------------------------------------------------------------

raw_dist_files <- list.files(path = here("data/raw"), 
                             pattern = "Dist.gpkg", 
                             full.names = T) %>% 
  # do not include fire point data
  discard(., str_detect(., "Point"))


# Functions ---------------------------------------------------------------

combine_dist_polygons <- function(poly) {
  # Arguments 
  ## poly: spatial polygon dataframe to be combined
  ## needs to have columns in the correct position to work
  out <- poly %>% 
    # selects ID, Event, and Year column from Ontario disturbance data
    dplyr::select(c(1, 3, 4), geom) %>% 
    # rename to consistent names
    rename(ID = 1, Event = 2, Year = 3)
  return(out)
}

# Combine dist polygons ---------------------------------------------------

all_dist <- raw_dist_files %>%
  map(st_read) %>% 
  map(combine_dist_polygons) %>% 
  reduce(bind_rows) %>% 
  # filter to years of C flux data
  filter(between(Year, 2001, 2019)) %>% 
  # project to match project crs
  st_transform(., 4269) %>% 
  # create column with 1 in all rows for rasterizing
  mutate(count = 1)

# Save output -------------------------------------------------------------

st_write(all_dist, here("data/processed/all_dist_2001-2019.gpkg"), overwrite = T)

