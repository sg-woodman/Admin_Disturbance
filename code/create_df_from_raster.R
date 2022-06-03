library(tidyverse)
library(here)
library(terra)


# Load data ---------------------------------------------------------------

admin_rast_250 <- rast(here("data/processed/admin_zone_rast_250m.tif"))
cflux_250_ha <- rast(here("data/processed/aou_cflux_250m_ha.tif"))

# project from lat lon to metres
admin_rast_250_3161 <- rast(here("data/processed/admin_zone_rast_250m_3161.tif"))
cflux_250_aou_3161 <- rast(here("data/processed/cflux_per_ha_250_aou_3161.tif"))
land_cover_aou_250_3161 <- rast(here("data/processed/land_cover_aou_250_3161.tif"))

# distinct dist group
ind_dist_names <- list.files(path = here("data/processed/Annual_Raster/"))

# SR vs NSR 
sr_dist_names <- unlist(as.list(c("SR", "NSR")))
sr_dist_names_3161 <- unlist(as.list(c("SR_3161", "NSR_3161")))

# All dist
all_dist_names <- unlist(as.list(c("All_Dist")))
all_dist_names_3161 <- unlist(as.list(c("All_Dist_3161")))


# Functions ---------------------------------------------------------------

# change column name
change_colname <- function(df, new_colname){
  df %>% rename(!!new_colname := count)
}

write_annual_cell_dist <- function(DIST) {
  f_list <- list.files(path = here("data/processed/Annual_Raster/", paste0(DIST), "/"),
                       pattern = "*.tif",
                       full.names = T)
  
  f_names <- list.files(path = here("data/processed/Annual_Raster/", paste0(DIST), "/"),
                        pattern = "*.tif",
                        full.names = F) %>% 
    map(., ~str_replace(., "_250m.tif", "")) %>% 
    map(., ~str_replace(., "_PA", "")) %>% 
    map(., ~str_replace(., "^[a-zA-Z_]*", "count_"))
  
  out <- f_list %>% 
    map(rast) %>% 
    map(., as.data.frame, xy = T, cells = T) %>% 
    map2(.y = f_names, ~ change_colname(.x, .y)) %>% 
    reduce(full_join, by = c("cell", "x", "y"))# %>% 
    # mutate(dist_count = rowSums(across(starts_with("count")))) %>% 
    # dplyr::select(cell, x, y, dist_count) %>% 
    # rename({{DIST}} := dist_count)
  
  #return(out)
  write_csv(out, 
            here(paste0("data/processed/Annual_dist_pixel_df/", DIST, ".csv")))
  
}

write_annual_cell_dist_3161 <- function(DIST) {
  f_list <- list.files(path = here("data/processed/Annual_Raster/", paste0(DIST), "/"),
                       pattern = "*.tif",
                       full.names = T)
  
  f_names <- list.files(path = here("data/processed/Annual_Raster/", paste0(DIST), "/"),
                        pattern = "*.tif",
                        full.names = F) %>% 
    map(., ~str_replace(., "_250_3161m.tif", "")) %>% 
    map(., ~str_replace(., "3161_PA", "")) %>% 
    map(., ~str_replace(., "^[a-zA-Z_]*", "count_"))
  
  out <- f_list %>% 
    map(rast) %>% 
    map(., as.data.frame, xy = T, cells = T) %>% 
    map2(.y = f_names, ~ change_colname(.x, .y)) %>% 
    reduce(full_join, by = c("cell", "x", "y"))# %>% 
  # mutate(dist_count = rowSums(across(starts_with("count")))) %>% 
  # dplyr::select(cell, x, y, dist_count) %>% 
  # rename({{DIST}} := dist_count)
  
  #return(out)
  write_csv(out, 
            here(paste0("data/processed/Annual_dist_pixel_df/", DIST, ".csv")))
  
}



# Convert raster to DF ----------------------------------------------------

admin_250_df <- as.data.frame(admin_rast_250, cells = T, xy = T)
cflux_250_df <- as.data.frame(cflux_250_ha, cells = T, xy = T)

admin_250_df_3161 <- as.data.frame(admin_rast_250_3161, cells = T, xy = T)
cflux_250_df_3161 <- as.data.frame(cflux_250_aou_3161, cells = T, xy = T)
lc_250_df_3161 <- as.data.frame(land_cover_aou_250_3161, cells = T, xy = T)


## Save dist raster as df
sr_dist_names %>% 
  walk(write_annual_cell_dist) 

sr_dist_names_3161 %>% 
  walk(write_annual_cell_dist_3161) 

all_dist_names_3161 %>% 
  walk(write_annual_cell_dist_3161) 

write_csv(admin_250_df_3161, here("data/processed/admin_250_3161_df.csv"))
write_csv(cflux_250_df_3161, here("data/processed/cflux_250_3161_df.csv"))
write_csv(lc_250_df_3161, here("data/processed/landcover_250_3161_df.csv"))


## Read dist raster df 
sr_dist_raster_df <- list.files(path = here("data/processed/Annual_dist_pixel_df"),
                                pattern = "*SR_3161.csv", 
                                full.names = T) %>% 
  map(read_csv) %>% 
  set_names(list("NSR", "SR"))

all_dist_raster_df <- list.files(path = here("data/processed/Annual_dist_pixel_df"),
                                pattern = "*All_Dist_3161.csv", 
                                full.names = T) %>% 
  map_df(read_csv)
  


# Disturbance summaries ---------------------------------------------------

## Classify pixels according to the type of disturbance (NSR &SR) that occurs 
## within. 
## 
## For a single year, a single dist of each type can exist. For "Both", 
## a NSR and SR dist occurred in the same pixel in the same year. For NSR and SR
## only one dist occurred. The fact that Both counts as two disturbance may mean 
## it needs to be removed from the analysis. 
## 
## For spans of time, the dist record is collated into periods to simplify the 
## analysis. In this case special attention is needed for the how disturbances 
## are counted. Two options exist. 1) Only count when a single disturbance has 
## occurred in the pixel during the entire period. This is achieved when the 
## case_when argument is set to == 1. Doing so would again count two disturbance 
## in the Both class but only one in the NSR and SR. 2) Only count when 2 
## disturbances occur in each pixel over the period (case_when == 2 for NSR and 
## SR but == 1 for Both), This holds the number of disturbances constant but 
## varys the dist type. 

## All years (2001-2019)
## 
### For each dist type, how many of that disturbance occur in a pixel over the
### time period. Dist types (NSR vs SR) are combine after the fact to be able 
### to assess the order and spacing between
NSR_01_19_df <- sr_dist_raster_df[[1]] %>% 
  mutate(dist_count = count_2001 + count_2002 +  count_2003 + count_2004 + 
           count_2005 + count_2006 + count_2007 + count_2008 + count_2009 + 
           count_2010 + count_2011 +  count_2012 + count_2013 + count_2014 + 
           count_2015 + count_2016 + count_2017 + count_2018 + count_2019) %>% 
  select(cell, x, y, nsr_01_19 = dist_count)

SR_01_19_df <- sr_dist_raster_df[[2]] %>% 
  mutate(dist_count = count_2001 + count_2002 +  count_2003 + count_2004 + 
           count_2005 + count_2006 + count_2007 + count_2008 + count_2009 + 
           count_2010 + count_2011 +  count_2012 + count_2013 + count_2014 + 
           count_2015 + count_2016 + count_2017 + count_2018 + count_2019) %>% 
  select(cell, x, y, sr_01_19 = dist_count)

### Classify according to number of dist that occurred of each type. 
### NSR2 and SR2 mean 2 dist of those types occurred during the whole time series.
### These can show how more disturbance impact C flux and how they compare to 
### one SR and one NSR occurring (i.e. Both). Any pixel with more than 2 
### disturbances is removed
NSR_SR_01_19_df <- full_join(NSR_01_19_df, SR_01_19_df) %>% 
  mutate(dist_01_19 = case_when(nsr_01_19 == 0 & sr_01_19 == 1 ~ "SR",
                                nsr_01_19 == 1 & sr_01_19 == 0 ~ "NSR",
                                nsr_01_19 == 1 & sr_01_19 == 1 ~ "Both",
                                nsr_01_19 == 0 & sr_01_19 == 0 ~ "ND",
                                nsr_01_19 == 0 & sr_01_19 == 2 ~ "SR2",
                                nsr_01_19 == 2 & sr_01_19 == 0 ~ "NSR2",
                                TRUE ~ "remove")) %>% 
  filter(dist_01_19 != "remove")

NSR_SR_01_19_df %>% filter(is.na(dist_01_19))

NSR_SR_01_19_df %>% group_by(dist_01_19) %>% tally() %>% 
  mutate(freq = n/sum(n))

write_csv(NSR_SR_01_19_df, here("data/processed/nsr_sr_01_19_df.csv"))

## Years split into periods
### To assess the effects of recurring disturbances the time period (01-19) was
### split into 3 roughly equal (~ six year) periods where the number of dist
### of each type (NSR vs SR) was counted. For each period, whether no dist, one
### NSR, one SR, or both one NSR and one SR was encoded. Any pixel with more than 2 
### disturbances is removed

## 2001-2006
NSR_01_06_df <- sr_dist_raster_df[[1]] %>% 
  mutate(dist_count = count_2001 + count_2002 +  count_2003 + 
           count_2004 + count_2005 + count_2006) %>% 
  select(cell, x, y, nsr_01_06 = dist_count)

SR_01_06_df <- sr_dist_raster_df[[2]] %>% 
  mutate(dist_count = count_2001 + count_2002 +  count_2003 + 
           count_2004 + count_2005 + count_2006) %>% 
  select(cell, x, y, sr_01_06 = dist_count)

NSR_SR_01_06_df <- full_join(NSR_01_06_df, SR_01_06_df) %>% 
  mutate(dist_01_06 = case_when(nsr_01_06 == 0 & sr_01_06 == 1 ~ "SR",
                                nsr_01_06 == 1 & sr_01_06 == 0 ~ "NSR",
                                nsr_01_06 == 1 & sr_01_06 == 1 ~ "Both",
                                nsr_01_06 == 0 & sr_01_06 == 0 ~ "ND",
                                TRUE ~ "remove")) %>% 
  filter(dist_01_06 != "remove")

NSR_SR_01_06_df %>% filter(is.na(dist_01_06))

NSR_SR_01_06_df %>% group_by(dist_01_06) %>% tally()

## 2007-2012
NSR_07_12_df <- sr_dist_raster_df[[1]] %>% 
  mutate(dist_count = count_2007 + count_2008 +  count_2009 + 
           count_2010 + count_2011 + count_2012) %>% 
  select(cell, x, y, nsr_07_12 = dist_count)

SR_07_12_df <- sr_dist_raster_df[[2]] %>% 
  mutate(dist_count = count_2007 + count_2008 +  count_2009 + 
           count_2010 + count_2011 + count_2012) %>% 
  select(cell, x, y, sr_07_12 = dist_count)

NSR_SR_07_12_df <- full_join(NSR_07_12_df, SR_07_12_df) %>% 
  mutate(dist_07_12 = case_when(nsr_07_12 == 0 & sr_07_12 == 1 ~ "SR",
                                nsr_07_12 == 1 & sr_07_12 == 0 ~ "NSR",
                                nsr_07_12 == 1 & sr_07_12 == 1 ~ "Both",
                                nsr_07_12 == 0 & sr_07_12 == 0 ~ "ND",
                                TRUE ~ "remove")) %>% 
  filter(dist_07_12 != "remove")

NSR_SR_07_12_df %>% filter(is.na(dist_07_12))

NSR_SR_07_12_df %>% group_by(dist_07_12) %>% tally()

## 2013-2019
NSR_13_19_df <- sr_dist_raster_df[[1]] %>% 
  mutate(dist_count = count_2013 + count_2014 +  count_2015 + 
           count_2016 + count_2017 + count_2018 + count_2019) %>% 
  select(cell, x, y, nsr_13_19 = dist_count)

SR_13_19_df <- sr_dist_raster_df[[2]] %>% 
  mutate(dist_count = count_2013 + count_2014 +  count_2015 + 
           count_2016 + count_2017 + count_2018 + count_2019) %>% 
  select(cell, x, y, sr_13_19 = dist_count)

NSR_SR_13_19_df <- full_join(NSR_13_19_df, SR_13_19_df) %>% 
  mutate(dist_13_19 = case_when(nsr_13_19 == 0 & sr_13_19 == 1 ~ "SR",
                                nsr_13_19 == 1 & sr_13_19 == 0 ~ "NSR",
                                nsr_13_19 == 1 & sr_13_19 == 1 ~ "Both",
                                nsr_13_19 == 0 & sr_13_19 == 0 ~ "ND",
                                TRUE ~ "remove")) %>% 
  filter(dist_13_19 != "remove")

NSR_SR_13_19_df %>% filter(is.na(dist_13_19))

NSR_SR_13_19_df %>% group_by(dist_13_19) %>% tally()

## Combine periods
NSR_SR_df_periods <- full_join(NSR_SR_01_06_df, NSR_SR_07_12_df) %>% 
  full_join(., NSR_SR_13_19_df) %>% 
  unite(dist_pattern, c("dist_01_06", "dist_07_12", "dist_13_19"), 
        sep = "_", remove = F)

write_csv(NSR_SR_df_periods, here("data/processed/nsr_sr_periods_df.csv"))


## Calculate the order and years between two disturbances
## Select cells where one SR and one NSR occurred
both_cells <- NSR_SR_01_19_df %>% 
  filter(dist_01_19 == "Both") %>% 
  pull(cell)

## Create column with year of NSR dist
nsr_year <- sr_dist_raster_df[[1]] %>% 
  filter(cell %in% both_cells) %>% 
  rowwise() %>%
  mutate(mak=which.max(c_across(starts_with("count"))),
         mak = mak+2000) %>% 
  select(cell, x, y, nsr_dist_year = mak)
view(nsr_year)

## Create column with year of SR dist
sr_year <- sr_dist_raster_df[[2]] %>% 
  filter(cell %in% both_cells) %>% 
  rowwise() %>%
  mutate(mak=which.max(c_across(starts_with("count"))),
         mak = mak+2000) %>% 
  select(cell, x, y, sr_dist_year = mak)

## Select cells where two NSR dist occurred
nsr2_cells <- NSR_SR_01_19_df %>% 
  filter(dist_01_19 == "NSR2") %>% 
  pull(cell)

## Create column with year of first and second NSR dist 
nsr2_year <- sr_dist_raster_df[[1]] %>% 
  filter(cell %in% nsr2_cells) %>% 
  rowwise() %>%
  mutate(max_year = which.max(c_across(starts_with("count"))),
         max_year = max_year+2000,
         min_year = which.min(c_across(starts_with("count"))),
         min_year = min_year+2000) %>% 
  select(cell, x, y, 
         first_nsr_dist_year = min_year, 
         sec_nsr_dist_year = max_year)

## Select cells where two SR dist occurred
sr2_cells <- NSR_SR_01_19_df %>% 
  filter(dist_01_19 == "SR2") %>% 
  pull(cell)

## Create column with year of first and second SR dist 
sr2_year <- sr_dist_raster_df[[2]] %>% 
  filter(cell %in% sr2_cells) %>% 
  rowwise() %>%
  mutate(max_year = which.max(c_across(starts_with("count"))),
         max_year = max_year+2000,
         min_year = which.min(c_across(starts_with("count"))),
         min_year = min_year+2000) %>% 
  select(cell, x, y, 
         first_sr_dist_year = min_year, 
         sec_sr_dist_year = max_year)

## Calculate difference between first and second dist year and 
## combine 2 SR and 2 NSR dfs 
two_dist_year <- bind_rows(nsr2_year %>% 
                             ungroup() %>% 
                             mutate(diff = sec_nsr_dist_year-first_nsr_dist_year,
                                    order = "NSR_NSR")  %>% 
                             select(cell, x, y, diff, order),
                           sr2_year %>% 
                             ungroup() %>% 
                             mutate(diff = sec_sr_dist_year-first_sr_dist_year,
                                    order = "SR_SR") %>% 
                             select(cell, x, y, diff, order))

## combine dfs for Both dist in time series
dist_year <- left_join(nsr_year, sr_year) %>% 
  ## calc differenc in nsr and sr years
  ## negative means NSR was first
  mutate(diff = nsr_dist_year-sr_dist_year, 
         order = if_else(diff < 0, "NSR_SR", "SR_NSR")) %>% 
  ## select cells to match 2 same type dist data
  select(cell, x, y, diff, order) %>%
  ##bind 2 same dist data
  bind_rows(two_dist_year) %>% 
  rename(year_between_dist = diff) %>% 
  mutate(year_between_dist = abs(year_between_dist))

write_csv(dist_year, here("data/processed/dist_order_year_df_3161.csv"))

dist_year %>% group_by(order) %>% tally

dist_year %>% group_by(abs(diff)) %>% tally

## Create dist df

dist_df <- all_dist_raster_df %>% 
  select(cell, x, y) %>% 
  left_join(., NSR_SR_01_19_df) %>% 
  left_join(., NSR_SR_df_periods) %>% 
  left_join(., dist_year_3161)

write_csv(dist_df, here("data/processed/dist_250_3161_df.csv"))


### Assess data coverage of period groups
bind_cols(NSR_SR_01_06_df %>% 
            group_by(dist_01_06) %>% 
            tally() %>% 
            rename(dist = dist_01_06, n_01_06 = n),
          NSR_SR_07_12_df %>% 
            group_by(dist_07_12) %>% 
            tally() %>% 
            rename(dist = dist_07_12, n_07_12 = n),
          NSR_SR_13_19_df %>% 
            group_by(dist_13_19) %>% 
            tally() %>% 
            rename(dist = dist_13_19, n_13_19 = n)) %>% 
  select(dist = dist...1 , starts_with("n"))



