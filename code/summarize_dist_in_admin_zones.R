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

## Dist types
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
Water_Stress <- c("Drought", "Winter Drying", "Scorch")
Winter_Precip <- c("Hail", "Snow", "Ice")
Cold_Temp <- c("Cold", "Frost")
Wind <- c("Blowdown", "Windstorm")
Fire = c("IFR", "PB", "OFR")


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
  dplyr::bind_rows() %>% 
  mutate(Event = case_when(Event %in% Defoliators ~ "Defoliators",
                           Event %in% Bark_Beetle ~ "Bark_Beetle",
                           Event %in% Disease ~ "Disease",
                           Event %in% Water_Stress ~ "Water_stress",
                           Event %in% Fire ~ "Fire",
                           Event %in% Cold_Temp ~ "Cold",
                           Event %in% Winter_Precip ~ "Winter_Precip",
                           Event %in% Wind ~ "Wind"))
  
# Process dist in admin ---------------------------------------------------

## create skeleton df for all years and admin zones IDs
## used to join data so years without disturbance are included. 
skeleton_df <- expand_grid(Year = seq(2001, 2019, 1), 
                           ID = unique(admin_dist_full_df$ID), 
                           Event = unique(admin_dist_full_df$Event)) %>% 
  left_join(., admin_dist_full_df %>% 
              dplyr::select(ID, zone, zone_num, area_ha) %>% 
              distinct()) %>% 
  mutate(Event2 = case_when(Event %in% c("Defoliators", "Bark_Beetle", "Disease") ~ "Biotic", 
                            Event %in% c("Water_stress", "Fire", "Cold", "Winter_Precip", "Wind") ~ "Abiotic"))

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
  group_by(ID, zone, Event, Year) %>% 
  # calc mean area (constant), total area disturbed, and no. of events within
  # each unique zone and year
  summarise(area_ha = mean(area_ha),
            dist_area = sum(dist_area),
            count = sum(count),
            .groups = "drop") %>%
  # calc prop are disturbed
  mutate(Event2 = case_when(Event %in% c("Defoliators", "Bark_Beetle", "Disease") ~ "Biotic", 
                            Event %in% c("Water_stress", "Fire", "Cold", "Winter_Precip", "Wind") ~ "Abiotic"),
         prop_dist = dist_area/area_ha,
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
         zone = as.factor(zone)) %>% 
  arrange(ID, zone, Event, Year)

# Save output -------------------------------------------------------------

write_csv(admin_dist_df, here("data/processed/dist_admin_extract.csv"))

write_csv(admin_dist_df_year, here("data/processed/dist_admin_extract_year.csv"))


# Plot --------------------------------------------------------------------

# Mean prop dist
prop_fig <- admin_dist_df_year %>% 
  filter(prop_dist > 0) %>% 
  group_by(zone, Event2) %>% 
  summarise(mean_prop_dist = mean(prop_dist, na.rm = T),
            sd_prop_dist = sd(prop_dist, na.rm = T),
            n_prop_dist = n(),
            se_prop_dist = sd_prop_dist/sqrt(n_prop_dist)) %>%
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indigenous Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park"),
         zone = factor(zone, levels = c("General<br>Use Area", 
                                        "Enhanced<br>Management Area",
                                        "Indigenous<br>Reserve",
                                        "Provincial<br>Park",
                                        "Conservation<br>Reserve"
         ))) %>% 
  ggplot(aes(x = zone, y = mean_prop_dist, fill = Event2)) + 
  geom_col(width = 0.75, 
           position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin = mean_prop_dist - se_prop_dist, 
                    ymax = mean_prop_dist + se_prop_dist), 
                width =0.5,
                position = position_dodge(0.75)) + 
  scale_y_continuous(name = "Mean (±SE) proportion area disturbed<br>no disturbance removed",
                     # limits = c(0, 1),
                     # breaks = seq(0, 1, 0.1),
                     expand = c(0,0)) + 
  #facet_wrap(~Event, scales = "free_y") + 
  theme_classic() + 
  theme(text = element_text(size=12),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown(),
        axis.title.x = element_blank())


## Mean number of dist
n_fig <- admin_dist_df_year %>% 
  group_by(ID, zone, Event2) %>% 
  summarise(n_dist = sum(n_dist)) %>% 
  group_by(zone, Event2) %>% 
  summarise(num_dist = mean(n_dist, na.rm = T),
            sd_dist = sd(n_dist, na.rm = T),
            n_dist = n(),
            se_dist = sd_dist/sqrt(n_dist)) %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indigenous Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park"),
         zone = factor(zone, levels = c("General<br>Use Area", 
                                        "Enhanced<br>Management Area",
                                        "Indigenous<br>Reserve",
                                        "Provincial<br>Park",
                                        "Conservation<br>Reserve"
         ))) %>% 
  ggplot(aes(x = zone, y = num_dist, fill = Event2)) + 
  geom_col(width = 0.75, 
           position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin = num_dist - se_dist, 
                    ymax = num_dist + se_dist), 
                width = 0.5,
                position = position_dodge(0.75)) +
  scale_y_continuous(expand = c(0,0)) +
  ylab("Mean (±SE) number of years between<br>2001 and 2019 with disturbance") + 
  xlab("") + 
  #facet_wrap(~Event2, scales = "free_y") + 
  theme_classic() + 
  theme(text = element_text(size=12),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown(),
        axis.title.x = element_blank())  

ggpubr::ggarrange(prop_fig, n_fig, ncol=1, nrow=2, common.legend = TRUE, legend="top")
