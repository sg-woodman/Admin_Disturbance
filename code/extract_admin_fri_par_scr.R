library(tidyverse)
library(sf) # geospatial analysis with points and polygons
library(parallel)

#/scripts/csmit -m 120G -c 5 -b "/applications/R/R-3.6.0/bin/Rscript /home/sgw35/extract_admin_fri_par_scr.R"


# Paralise any simple features analysis.
st_parallel <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    parallel::mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2){
    output_class <- output_class[2]
  }
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix"){
    result <- do.call("rbind", split_results)
    names(result) <- NULL
  } else if (output_class == "sfc") {
    result <- do.call("c", split_results)
    result <- sf_func(result) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions. 
  } else if (output_class %in% c('list', 'sgbp') ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else if (output_class == "data.frame" ){
    result <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  # Return result
  return(result)
}

print("load")
admin_id <- read.csv("/home/sgw35/admin_cflux_dist_poly_ha_df.csv") %>% 
  pull(ID) %>% unique()

admin_zone_nolakes <- st_read("/home/sgw35/admin_zone_lakes-removed_valid.gpkg") %>% 
  filter(ID %in% admin_id) %>%   
  mutate(area_m2 = as.numeric(st_area(.)))

fri <- st_read("/home/sgw35/fri_full_valid.gpkg")

print("run")
output <- st_parallel(admin_zone_nolakes, st_intersection, 5, y = fri)

print("save")
saveRDS(output, "/home/sgw35/admin_zone_fri_par.rds")



