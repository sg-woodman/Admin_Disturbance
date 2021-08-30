library(terra)
library(raster)
library(sf)

#/scripts/csmit -m 150G -b "/applications/R/R-3.6.0/bin/Rscript /home/sgw35/rasterize_fri_scr.R"

print("load")
fri <- vect("/home/sgw35/fri_full_valid.gpkg")
cflux_30 <- terra::rast("/home/sgw35/aou_cflux_per_ha_30.tif")

print("age")
fri_age_rast <- terra::rasterize(fri, cflux_30, "OYRORG",
                                 backgroud = NA)
print("save")
terra::writeRaster(fri_age_rast, "/home/sgw35/fri_oyrorg_30.tif")

print("formod")
fri_prot_rast <- terra::rasterize(fri, cflux_30, "FORMOD",
                                  backgroud = NA)
print("save")
terra::writeRaster(fri_prot_rast, "/home/sgw35/fri_formod_30.tif")