library(tidyverse)
library(terra)
library(sf)

fri_files <- list.files(path = "/Volumes/Backup/FRI_GPKG",
                        pattern = ".gpkg",
                        full.names = T)

fri_names <- list.files(path = "/Volumes/Backup/FRI_GPKG/",
                        pattern = ".gpkg")


fri_forest <- fri_files[11:29] %>% 
  map(st_read) %>% 
  map( ~ filter(.x, POLYTYPE == "FOR")) %>% 
  map(~ select(.x, FMFOBJID, POLYID, YRSOURCE, SOURCE, FORMOD, DEVSTAGE, YRDEP, DEPTYPE,
               OYRORG, OSPCOMP, OLEADSPC, OAGE, OHT, OCCLO, OSI, OSC, UYRORG, USPCOMP, 
               ULEADSPC, UAGE, UHT, UCCLO, USI, USC, VERT, HORIZ, ACCESS1, ACCESS2, 
               MGMTCON1, MGMTCON2, MGMTCON3)) %>% 
  map(~ st_zm(.x, drop = T, what = "ZM"))

fri_forest_1_10[4]

fri_forest_2 <- do.call(rbind, fri_forest)

st_write(fri_forest_2, "/Volumes/Backup/FRI_Forest/fri_2.gpkg")

writeVector(, paste0("/Volumes/Backup/FRI_Forest", ))

map2(fri_forest_1_10, fri_names[1:10], ~ writeVector(.x, 
                                                     paste0("/Volumes/Backup/FRI_Forest/", .y),
                                                     filetype = "GPKG"))
