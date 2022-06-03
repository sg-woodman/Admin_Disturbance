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


# Constants ---------------------------------------------------------------

## Dist types
`%notin%` <- Negate(`%in%`)
## Vector of disturbance event names from Ontario GeoHub and the author 
## defined catecories they are grouped in to. 
Defoliators <- c("Aspen Twoleaf Tier","Birch Casebearer","Birch Leafminer","Birch Skeletonizer",
                 "Bruce Spanworm","Cedar Leafminer","Cherry Scallop Shell Moth",
                 "Early Aspen Leafroller","Elm Spanworm","Fall Cankerworm", "Fall Webworm",
                 "Forest Tent Caterpillar","Gypsy Moth","Hemlock Looper","Hickory Leaf Roller",
                 "Introduced Pine Sawfly","Jack Pine Budworm","Larch Casebearer",
                 "Large Aspen Tortrix","Lesser Maple Spanworm","Maple Leaf Roller","Maple Leafcutter",
                 "Oak Defoliators Complex","Oak Leaf Roller","Oak Leafshredder","Other Insect",
                 "Pine False Webworm","Pink-striped Oakworm","Poplar Flea Beetle",
                 "Poplar Serpentine Leafminer","Redhumped Oakworm","Satin Moth","Spruce Budworm",
                 "Unknown Aspen Leafroller","Willow Leafminer") %>% as.vector()
Bark_Beetles <- c("Emerald Ash Borer", 
                  "Eastern Larch Beetle", "White-Spotted Sawyer Beetle", 
                  "Agrilus sp. Damage to Balsam Poplar", "Bronze Birch Borer", 
                  "Pine Engraver Beetle", "Hickory Bark Beetle", "Pine Shoot Beetle", 
                  "Hemlock Borer", "Misc Beetle Damage to Jack Pine",
                  "Bronze Poplar Borer")
Water_Stress <- c("Drought", "Winter Drying", "Scorch")
Winter_Precip <- c("Hail", "Snow", "Ice")
Cold_Temp <- c("Cold", "Frost")
Wind <- c("Blowdown", "Windstorm")
Fire = c("IFR", "PB", "OFR")
Disease <- c("Brown spot needle blight of pine",
             "Septoria leaf spot and canker", "Armillaria root rot",
             "Ink spot of aspen", "Anthracnose", "Lophodermium needle cast of pine",
             "Beech bark disease", "Spruce needle rust", "Linospora leaf blight")


NSR <- c("Aspen Twoleaf Tier","Birch Casebearer","Birch Leafminer","Birch Skeletonizer",
         "Bruce Spanworm","Cedar Leafminer","Cherry Scallop Shell Moth",
         "Early Aspen Leafroller","Elm Spanworm","Fall Cankerworm", "Fall Webworm",
         "Forest Tent Caterpillar","Gypsy Moth","Hemlock Looper","Hickory Leaf Roller",
         "Introduced Pine Sawfly","Jack Pine Budworm","Larch Casebearer",
         "Large Aspen Tortrix","Lesser Maple Spanworm","Maple Leaf Roller","Maple Leafcutter",
         "Oak Defoliators Complex","Oak Leaf Roller","Oak Leafshredder","Other Insect",
         "Pine False Webworm","Pink-striped Oakworm","Poplar Flea Beetle",
         "Poplar Serpentine Leafminer","Redhumped Oakworm","Satin Moth","Spruce Budworm", 
         "Spruce budworm", "Unknown Aspen Leafroller","Willow Leafminer",
         "Brown spot needle blight of pine",
         "Septoria leaf spot and canker", "Armillaria root rot",
         "Ink spot of aspen", "Anthracnose", "Lophodermium needle cast of pine",
         "Beech bark disease", "Spruce needle rust", "Linospora leaf blight",
         "Scorch", "Drought", "Winter Drying",
         "Hail", "Snow", "Ice", "Cold", "Frost", 
         "Windstorm")
SR <- c("Emerald Ash Borer", 
        "Eastern Larch Beetle", "White-Spotted Sawyer Beetle", 
        "Agrilus sp. Damage to Balsam Poplar", "Bronze Birch Borer", 
        "Pine Engraver Beetle", "Hickory Bark Beetle", "Pine Shoot Beetle", 
        "Hemlock Borer", "Misc Beetle Damage to Jack Pine",
        "Bronze Poplar Borer",
        "IFR", "PB", "OFR",
        "Blowdown")

Biotic <- c("Aspen Twoleaf Tier","Birch Casebearer","Birch Leafminer","Birch Skeletonizer",
            "Bruce Spanworm","Cedar Leafminer","Cherry Scallop Shell Moth",
            "Early Aspen Leafroller","Elm Spanworm","Fall Cankerworm", "Fall Webworm",
            "Forest Tent Caterpillar","Gypsy Moth","Hemlock Looper","Hickory Leaf Roller",
            "Introduced Pine Sawfly","Jack Pine Budworm","Larch Casebearer",
            "Large Aspen Tortrix","Lesser Maple Spanworm","Maple Leaf Roller","Maple Leafcutter",
            "Oak Defoliators Complex","Oak Leaf Roller","Oak Leafshredder","Other Insect",
            "Pine False Webworm","Pink-striped Oakworm","Poplar Flea Beetle",
            "Poplar Serpentine Leafminer","Redhumped Oakworm","Satin Moth","Spruce Budworm", 
            "Spruce budworm", "Unknown Aspen Leafroller","Willow Leafminer",
            "Brown spot needle blight of pine",
            "Septoria leaf spot and canker", "Armillaria root rot",
            "Ink spot of aspen", "Anthracnose", "Lophodermium needle cast of pine",
            "Beech bark disease", "Spruce needle rust", "Linospora leaf blight",
            "Emerald Ash Borer", 
            "Eastern Larch Beetle", "White-Spotted Sawyer Beetle", 
            "Agrilus sp. Damage to Balsam Poplar", "Bronze Birch Borer", 
            "Pine Engraver Beetle", "Hickory Bark Beetle", "Pine Shoot Beetle", 
            "Hemlock Borer", "Misc Beetle Damage to Jack Pine",
            "Bronze Poplar Borer")
Abiotic <- c("Scorch", "Drought", "Winter Drying",
             "Hail", "Snow", "Ice", "Cold", "Frost", 
             "Windstorm",
             "IFR", "PB", "OFR",
             "Blowdown")


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
  mutate(count = 1,
         all_dist = "All_Dist",
         dist_type1 = case_when(Event %in% NSR ~ "NSR",
                                Event %in% SR ~ "SR"),
         dist_type2 = case_when(Event %in% Biotic ~ "Biotic",
                                Event %in% Abiotic ~ "Abiotic"),
         Event = case_when(Event %in% Defoliators ~ "Defoliators",
                           Event %in% Bark_Beetles ~ "Bark_Beetle",
                           Event %in% Disease ~ "Disease",
                           Event %in% Water_Stress ~ "Water_stress",
                           Event %in% Fire ~ "Fire",
                           Event %in% Cold_Temp ~ "Cold",
                           Event %in% Winter_Precip ~ "Winter_Precip",
                           Event %in% Wind ~ "Wind"))

# Save output -------------------------------------------------------------

st_write(all_dist, 
         here("data/processed/all_dist_2001-2019.gpkg"), 
         delete_layer = TRUE)

