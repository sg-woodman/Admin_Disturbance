library(terra)

#/scripts/csmit -m 100G -b "/applications/R/R-3.6.0/bin/Rscript /home/sgw35/merge_fri_rast_scr.R"


fri1 <- rast("/home/sgw35/fri_1_age_rast.tif")
fri2 <- rast("/home/sgw35/fri_2_age_rast.tif")
fri3 <- rast("/home/sgw35/fri_3_age_rast.tif")

fri_rast <- merge(fri1, fri2, fri3)

writeRaster(fri_rast, "home/sgw35/fri_age_raster.tif")