library(terra)

#/scripts/csmit -m 100G -b "/applications/R/R-3.6.0/bin/Rscript /home/sgw35/merge_fri_scr.R"


fri1 <- terra::vect("/home/sgw35/fri_1.gpkg")
fri2 <- terra::vect("/home/sgw35/fri_2.gpkg")
fri3 <- terra::vect("/home/sgw35/fri_3_valid.gpkg")

fri <- c(fri1, fri2, fri3)

writeVector(fri, "/home/sgw35/fri_full_valid.gpkg", filetype = "GPKG")
