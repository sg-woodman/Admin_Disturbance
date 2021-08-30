# Title: Extract forest age
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Use FRI raster of overstory age to calc age in admin zone

## The Ontario FRI data contains several useful variables of forests. Among these
## is overstory age. Due to it's large size and complex data the full FRI dataset
## could not be rasterized for all variables. Therefore, key variables, like 
## age, are rasterized separately. Here, the age of forests in admin zones is 
## extracted. Since the data is rasterized at 30 m (same as response variable: 
## cflux) the calculated age will be weighted by the size of the forest. 

## NOTE! Not all FRI datasets were available. Several along the eastern edge 
## of Ontario were missing and a couple in the middle were corrupted. Therefore
## the data needs to be investigated to ensure admin zones that are lacking data
## are not skewing the results.

# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(exactextractr)
library(raster)
library(sf)

# Load data ---------------------------------------------------------------

fri_age <- raster(here("data/processed/fri_oyrorg_30.tif"))
admin_zone_nolakes <- 
  # generated and validated using QGIS (faster than R in this case)
  st_read(here("data/processed/admin_zone_lakes-removed_valid.gpkg"))

# Extract -----------------------------------------------------------------

fri_age_admin <- exact_extract(fri_age, 
                                admin_zone_nolakes,
                                c("mean", "median", "min", "max", 
                                  "variance", "count"))


# Save output -------------------------------------------------------------

admin_forest_age_df <- admin_zone_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(ID, zone) %>% 
  bind_cols(., fri_age_admin) %>% 
  rename(mean_age = mean, median_age = median, min_age = min, max_age = max,
         variance_age = variance, count_age = count)

write_csv(admin_forest_age_df, here("data/processed/admin_forest_age.csv"))

