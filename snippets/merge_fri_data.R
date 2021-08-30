
library(terra)
library(here)


cflux <- rast(here("data/raw/aou_cflux_per_ha_30.tif"))
fri <- vect("/Volumes/Backup/FRI_Forest/fri_3_valid.gpkg")


fri3_age_rast <- terra::rasterize(fri, cflux, "OYRORG",
                                  backgroud = NA)
writeRaster(fri3_age_rast, here("tmp/fri_3_age_rast.tif"))

fri1 <- rast(here("tmp/fri_1_age_rast.tif"))
fri2 <- rast(here("tmp/fri_2_age_rast.tif"))
fri3 <- rast(here("tmp/fri_3_age_rast.tif"))

fri2[fri2 == 0] <- NA

fri_rast <- merge(fri1, fri2, fri3)
