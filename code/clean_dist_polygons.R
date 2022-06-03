# Title: Clean disturbance data
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Reclassify disturbance data to aggregate types

## Aerial disturbance data collected by the Ontario government include several
## types of disturbances at a fine level of detail. Having so many types will
## increase the size of the analysis and reduce interpretability. Therefore, 
## disturbances are grouped according to similar base types. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(terra)
library(sf)


# Load Data ---------------------------------------------------------------

dist_poly_sf <- st_read(here("data/processed/all_dist_2001-2019.gpkg"))

# Constants ---------------------------------------------------------------

Defoliators <- c("Aspen Twoleaf Tier","Birch Casebearer","Birch Leafminer","Birch Skeletonizer",
                 "Bruce Spanworm","Cedar Leafminer","Cherry Scallop Shell Moth",
                 "Early Aspen Leafroller","Elm Spanworm","Fall Cankerworm", "Fall Webworm",
                 "Forest Tent Caterpillar","Gypsy Moth","Hemlock Looper","Hickory Leaf Roller",
                 "Introduced Pine Sawfly","Jack Pine Budworm","Larch Casebearer",
                 "Large Aspen Tortrix","Lesser Maple Spanworm","Maple Leaf Roller","Maple Leafcutter",
                 "Oak Defoliators Complex","Oak Leaf Roller","Oak Leafshredder","Other Insect",
                 "Pine False Webworm","Pink-striped Oakworm","Poplar Flea Beetle",
                 "Poplar Serpentine Leafminer","Redhumped Oakworm","Satin Moth","Spruce Budworm", 
                 "Spruce budworm", "Unknown Aspen Leafroller","Willow Leafminer")
Bark_Beetle <- c("Emerald Ash Borer", 
                 "Eastern Larch Beetle", "White-Spotted Sawyer Beetle", 
                 "Agrilus sp. Damage to Balsam Poplar", "Bronze Birch Borer", 
                 "Pine Engraver Beetle", "Hickory Bark Beetle", "Pine Shoot Beetle", 
                 "Hemlock Borer", "Misc Beetle Damage to Jack Pine",
                 "Bronze Poplar Borer")
Disease <- c("Brown spot needle blight of pine",
             "Septoria leaf spot and canker", "Armillaria root rot",
             "Ink spot of aspen", "Anthracnose", "Lophodermium needle cast of pine",
             "Beech bark disease", "Spruce needle rust", "Linospora leaf blight")
Water_Stress <- c("Scorch", "Drought", "Winter Drying")
Fire <- c("IFR", "PB", "OFR")
Cold <- c("Hail", "Snow", "Ice", "Cold", "Frost")
Blowdown <- c("Blowdown")


# Preprocessing -----------------------------------------------------------

dist_poly <- dist_poly_sf %>% 
  mutate(Event = case_when(Event %in% Defoliators ~ "Defoliators",
                           Event %in% Bark_Beetle ~ "Bark_Beetle",
                           Event %in% Disease ~ "Disease",
                           Event %in% Water_Stress ~ "Water_stress",
                           Event %in% Fire ~ "Fire",
                           Event %in% Cold ~ "Cold",
                           Event %in% Blowdown ~ "Blowdown")) %>% 
  vect(.)


# Save output -------------------------------------------------------------

writeVector(dist_poly, here("data/processed/cleaned_all_dist_2001-2019.gpkg"), 
            "GPKG", overwrite = T)


