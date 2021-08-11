library(tidyverse)
library(sf)
library(future)
library(furrr)

#/scripts/csmit -m 30G -c 10 -b "/applications/R/R-3.6.0/bin/Rscript /home/sgw35/clean_fri_scr.R"

print("load")
fri_files <- list.files(path = "/home/sgw35/FRI_Data",
                        pattern = ".gpkg",
                        full.names = T)

fri_names <- list.files(path =  "/home/sgw35/FRI_Data",
                        pattern = ".gpkg")

print("process")
plan(multicore, workers = 10)
fri_forest <- fri_files[1:5] %>% 
  future_map(st_read) %>% 
  future_map( ~ filter(.x, POLYTYPE == "FOR")) %>% 
  future_map(~ select(.x, FMFOBJID, POLYID, YRSOURCE, SOURCE, FORMOD, DEVSTAGE, YRDEP, DEPTYPE,
               OYRORG, OSPCOMP, OLEADSPC, OAGE, OHT, OCCLO, OSI, OSC, UYRORG, USPCOMP, 
               ULEADSPC, UAGE, UHT, UCCLO, USI, USC, VERT, HORIZ, ACCESS1, ACCESS2, 
               MGMTCON1, MGMTCON2, MGMTCON3)) %>% 
  future_map(~ st_zm(.x, drop = T, what = "ZM"))

print("merge")
fri_forest_out <- do.call(rbind, fri_forest)

print("save")
st_write(fri_forest_out, "/Volumes/Backup/FRI_Forest/fri_2.gpkg")

