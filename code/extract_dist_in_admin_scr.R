library(sf) # geospatial analysis with points and polygons
library(dplyr) # data manipulation tools
library(purrr) # data manipulation tools

#/scripts/csmit -m 30G -b "/applications/R/R-3.6.0/bin/Rscript /home/sgw35/extract_dist_in_admin_scr.R"


print("load data")
admin_zone_nolakes <- st_read("/home/sgw35/admin_zone_lakes-removed_valid.gpkg") %>% 
  mutate(area_ha = as.numeric(st_area(.))*0.0001)

all_dist_poly <- st_read("/home/sgw35/all_dist_2001-2019.gpkg")

print("make list")
admin_list <- admin_zone_nolakes %>% 
  group_by(ID) %>% 
  group_split()
  


extract_dist <- safely(function(CATCH){
  #pre_area <- as.numeric(sf::st_area(CATCH)) # calculate area before cleaning to set max size of hole to fill

  out_dist <- sf::st_intersection(CATCH, all_dist_poly) # select outbreak events that intersect land
  
  out_df <- out_dist %>% 
    dplyr::mutate(dist_area = as.numeric(sf::st_area(.)*0.0001)) %>% # calculate total catchment area
    sf::st_set_geometry(NULL) #remove geometry to force output as df
  out_df # output df
})

print("run")
output <- admin_list %>% # map extract dist function across catchments
  purrr::map(extract_dist) #%>% 
# map("result") %>% 
# compact() %>% 
# dplyr::bind_rows(., .id = "column_label")

print("save")
saveRDS(output, "/home/sgw35/admin_zone_dist_1.rds")

# write.csv(output, "/home/sgw35/ForestDist/Abiotic_Dist_History.csv")
