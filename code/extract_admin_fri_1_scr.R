library(tidyverse)
library(sf) # geospatial analysis with points and polygons
library(dplyr) # data manipulation tools
library(purrr) # data manipulation tools
library(parallel)

#/scripts/csmit -m 100G -c 30 -b "/applications/R/R-3.6.0/bin/Rscript /home/sgw35/extract_admin_fri_1_scr.R"

zone_to_remove <- c("National Park", "Wilderness Area", 
                    "National Marine Conservation Area", "Forest Reserve",
                    "National Wildlife Area", "Protected Area - Far North")

print("load data")
admin_zone_nolakes <- st_read("/home/sgw35/admin_zone_lakes-removed_valid.gpkg") %>% 
  filter(!zone %in% zone_to_remove) %>% 
  mutate(area_m2 = as.numeric(st_area(.)))

fri <- st_read("/home/sgw35/fri_3.gpkg")

# fri_1 = 
# fri_2 = 567098 rows
# fri_3 = 

#fri[1:113420,]

print("make list")
admin_list <- admin_zone_nolakes %>% 
  group_by(ID) %>% 
  group_split()

print("setup")
#plan(multicore, workers = 30)

extract_dist <- safely(function(CATCH){
  #pre_area <- as.numeric(sf::st_area(CATCH)) # calculate area before cleaning to set max size of hole to fill
  
  out_dist <- sf::st_intersection(CATCH, fri) # select outbreak events that intersect land
  
  out_df <- out_dist %>% 
    dplyr::mutate(fri_area = as.numeric(sf::st_area(.))) %>% # calculate total catchment area
    sf::st_set_geometry(NULL) #remove geometry to force output as df
  out_df # output df
})

print("run")
output <- mclapply(admin_list, extract_dist, mc.cores = 30)

# output <- admin_list %>% # map extract dist function across catchments
#   future_map(extract_dist) #%>% 
# map("result") %>% 
# compact() %>% 
# dplyr::bind_rows(., .id = "column_label")

print("save")
saveRDS(output, "/home/sgw35/admin_zone_fri_3.rds")

# write.csv(output, "/home/sgw35/ForestDist/Abiotic_Dist_History.csv")