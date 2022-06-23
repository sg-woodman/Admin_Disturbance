


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sf)
library(exactextractr)
library(raster)
library(terra)

# Load data ---------------------------------------------------------------

crs_3161 <- rgdal::make_EPSG() %>% filter(code == 3161) %>% pull(prj4)

catch_nolakes <- 
  st_read("/Users/sam/Documents/Pheno_Disturbacne/data/processed/AoU_Catchments_Lakes_Removed_valid_clean.gpkg") %>% 
  # calculate area of each polygon in ha (1 m2 = 0.0001 ha)
  mutate(area_poly_m2 = as.numeric(st_area(.)))

nsr_dist <- list.files(path = here("data/processed/Annual_Raster/NSR_3161/"),
                       pattern = "*.tif",
                       full.names = T)  %>% 
  map(raster) %>% 
  reduce(stack)
names(nsr_dist) <- sprintf("NSR_%03d", 2001:2019)


sr_dist <- list.files(path = here("data/processed/Annual_Raster/SR_3161/"),
                      pattern = "*.tif",
                      full.names = T)  %>% 
  map(raster) %>% 
  reduce(stack)
names(sr_dist) <- sprintf("SR_%03d", 2001:2019)

dist_extract <- vroom::vroom("/Users/sam/Documents/Pheno_Disturbacne/data/processed/catch_dist_lag_df.csv")

harvest_30_3161 <- rast(here("data/processed/harvest_mask_30_3161.tif"))
cflux_30_ha_3161 <- rast(here("data/processed/aou_cflux_30m_ha_3161.tif")) %>% 
  mask(., harvest_30_3161) %>% 
  raster()

ecoprov_rast <- raster(here("data/processed/ecoprovince_rast_500m_3161.tif"))
ecoprov_key <- tibble(
  ecoprov = c(0, 1, 2, 3, 4, 5),
  ecoprov_name = c(15.2, 6.1, 6.2, 6.5, 6.6, 8.1)
)

admin_rast_250_3161 <- rast(here("data/processed/admin_zone_rast_250m_3161.tif"))
admin_key <- read_csv(here("data/processed/admin_zone_key.csv"))

catch_overstory <- vroom::vroom("/Users/sam/Documents/Pheno_Disturbacne/data/processed/catch_overstory_extract.csv") %>% 
  mutate(catchment = as.numeric(str_replace(catchment, "ID_", "")))

catch_land_cover <- vroom::vroom("/Users/sam/Documents/Pheno_Disturbacne/data/processed/catch_land_cover.csv") %>% 
  mutate(catchment = as.numeric(str_replace(catchment, "ID_", "")))

human_footprint_3161 <- raster(here("data/raw/ontario_harvest_3161.tif"))


# Extract -----------------------------------------------------------------

dist_df <- dist_extract %>% 
  filter(year > 2000) %>% pull(year) %>% 
  dplyr::select(-ends_with("lag1")) %>% 
  mutate(catchment = as.numeric(str_replace(catchment, "ID_", "")),
         nsr_dist_prop = dist_defoliators + dist_drought + dist_winter_drying + 
           dist_scorch + dist_disease + dist_winter_precip + dist_cold_temp,
         sr_dist_prop = dist_bark_beetles + dist_blowdown + dist_fire,
         nsr_dist_binary = if_else(nsr_dist_prop > 0, 1, 0),
         nsr_dist_prop = if_else(nsr_dist_prop > 1, 1, nsr_dist_prop),
         nsr_dist_prop = if_else(nsr_dist_prop == 0, NA_real_, nsr_dist_prop),
         sr_dist_binary = if_else(sr_dist_prop > 0, 1, 0), 
         sr_dist_prop = if_else(sr_dist_prop > 1, 1, sr_dist_prop),
         sr_dist_prop = if_else(sr_dist_prop == 0, NA_real_, sr_dist_prop)) %>% 
  dplyr::select(catchment, starts_with(c("nsr", "sr"))) %>% 
  group_by(catchment) %>% 
  summarise(nsr_n_dist = sum(nsr_dist_binary, na.rm = T),
            nsr_mean_prop = mean(nsr_dist_prop, na.rm = T),
            sr_n_dist = sum(sr_dist_binary, na.rm = T),
            sr_mean_prop = mean(sr_dist_prop, na.rm = T)) %>% 
  mutate(nsr_mean_prop = if_else(is.nan(nsr_mean_prop), 0, nsr_mean_prop),
         sr_mean_prop = if_else(is.nan(sr_mean_prop), 0, sr_mean_prop))


nsr_extract <- exact_extract(nsr_dist, catch_nolakes, 
                             "sum", stack_apply = T)

sr_extract <- exact_extract(sr_dist, catch_nolakes, 
                            "sum", stack_apply = T)

ncells <- exact_extract(sr_dist[[1]], catch_nolakes, 
                        "count")

cflux_extract <- exact_extract(cflux_30_ha_3161, catch_nolakes, 
                               "mean")

ecoprov_extract <- exact_extract(ecoprov_rast, catch_nolakes, 
                                 "mode")

admin_extract <- exact_extract(admin_rast_250_3161, catch_nolakes, 
                                 "mode")

impact_extract <- exact_extract(human_footprint_3161, catch_nolakes, 
                               "mean")

# Create df ---------------------------------------------------------------

## Catchment coordiantes ----
catch_nolakes_centroid <- st_centroid(catch_nolakes, of_largest_polygon = T) %>% 
  st_coordinates()

## Dist count ----
NSR_01_19_df <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(nsr_extract) %>% 
  mutate(catch_area_m2 = ncells*(250*250),
         across(contains("NSR"), ~ (.x*(250*250))/catch_area_m2)) %>% 
  rename_with(~str_replace(., "sum.", "prop_"), contains("NSR")) %>% 
  mutate(across(starts_with("prop"), ~replace(., . > 0, 1))) %>% 
  mutate(dist_count = prop_NSR_2001 + prop_NSR_2002 +  prop_NSR_2003 + 
           prop_NSR_2004 + prop_NSR_2005 + prop_NSR_2006 + prop_NSR_2007 + 
           prop_NSR_2008 + prop_NSR_2009 + prop_NSR_2010 + prop_NSR_2011 +  
           prop_NSR_2012 + prop_NSR_2013 + prop_NSR_2014 + prop_NSR_2015 + 
           prop_NSR_2016 + prop_NSR_2017 + prop_NSR_2018 + prop_NSR_2019) %>% 
  dplyr::select(catchment, nsr_01_19 = dist_count)

SR_01_19_df <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(sr_extract) %>% 
  mutate(catch_area_m2 = ncells*(250*250),
         across(contains("SR"), ~ (.x*(250*250))/catch_area_m2)) %>% 
  rename_with(~str_replace(., "sum.", "prop_"), contains("SR")) %>% 
  mutate(across(starts_with("prop"), ~replace(., . > 0, 1))) %>% 
  mutate(dist_count = prop_SR_2001 + prop_SR_2002 +  prop_SR_2003 + 
           prop_SR_2004 + prop_SR_2005 + prop_SR_2006 + prop_SR_2007 + 
           prop_SR_2008 + prop_SR_2009 + prop_SR_2010 + prop_SR_2011 +  
           prop_SR_2012 + prop_SR_2013 + prop_SR_2014 + prop_SR_2015 + 
           prop_SR_2016 + prop_SR_2017 + prop_SR_2018 + prop_SR_2019) %>% 
  dplyr::select(catchment, sr_01_19 = dist_count)

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

## Full dist count and prop ----
nsr_df <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(nsr_extract) %>% 
  mutate(catch_area_m2 = ncells*(250*250),
         across(contains("NSR"), ~ (.x*(250*250))/catch_area_m2)) %>% 
  rename_with(~str_replace(., "sum.", "prop_"), contains("NSR")) %>% 
  pivot_longer(cols = starts_with("prop"), 
               names_to = "vars", 
               values_to = "vals") %>% 
  mutate(dist_binary = if_else(vals > 0, 1, 0)) %>% 
  mutate(dist_prop = if_else(vals == 0, NA_real_, vals)) %>%
  group_by(catchment) %>% 
  summarise(nsr_n_dist = sum(dist_binary, na.rm = T),
            nsr_mean_prop = mean(dist_prop, na.rm = T)) %>% 
  mutate(nsr_mean_prop = if_else(is.nan(nsr_mean_prop), 0, nsr_mean_prop))


nsr_df %>% group_by(nsr_n_dist) %>% tally()

sr_df <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(sr_extract) %>% 
  mutate(catch_area_m2 = ncells*(250*250),
         across(contains("SR"), ~ (.x*(250*250))/catch_area_m2)) %>% 
  rename_with(~str_replace(., "sum.", "prop_"), contains("SR")) %>% 
  pivot_longer(cols = starts_with("prop"), 
               names_to = "vars", 
               values_to = "vals") %>% 
  mutate(dist_binary = if_else(vals > 0, 1, 0)) %>% 
  mutate(dist_prop = if_else(vals == 0, NA_real_, vals)) %>% 
  group_by(catchment) %>% 
  summarise(sr_n_dist = sum(dist_binary, na.rm = T),
            sr_mean_prop = mean(dist_prop, na.rm = T)) %>% 
  mutate(sr_mean_prop = if_else(is.nan(sr_mean_prop), 0, sr_mean_prop))

sr_df %>% group_by(sr_n_dist) %>% tally()

## Dist period ----
nsr_period <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(nsr_extract) %>% 
  mutate(catch_area_m2 = ncells*(250*250),
         across(contains("NSR"), ~ (.x*(250*250))/catch_area_m2)) %>% 
  rename_with(~str_replace(., "sum.", "prop_"), contains("NSR")) %>% 
  pivot_longer(cols = starts_with("prop"), 
               names_to = "nsr_year", 
               values_to = "prop") %>% 
  mutate(nsr_year = str_replace(nsr_year, "prop_NSR_", ""),
         dist_period = case_when(nsr_year %in% seq(2001, 2006, 1) ~ "nsr_01_06",
                                 nsr_year %in% seq(2007, 2012, 1) ~ "nsr_07_12",
                                 nsr_year %in% seq(2013, 2019, 1) ~ "nsr_13_19"),
         dist_binary = if_else(prop > 0, 1, 0)) %>% 
  group_by(catchment, dist_period) %>% 
  summarise(n_nsr_dist = sum(dist_binary)) %>% 
  pivot_wider(names_from = dist_period, values_from = n_nsr_dist)

sr_period <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(sr_extract) %>% 
  mutate(catch_area_m2 = ncells*(250*250),
         across(contains("SR"), ~ (.x*(250*250))/catch_area_m2)) %>% 
  rename_with(~str_replace(., "sum.", "prop_"), contains("SR")) %>% 
  pivot_longer(cols = starts_with("prop"), 
               names_to = "sr_year", 
               values_to = "prop") %>% 
  mutate(sr_year = str_replace(sr_year, "prop_SR_", ""),
         dist_period = case_when(sr_year %in% seq(2001, 2006, 1) ~ "sr_01_06",
                                 sr_year %in% seq(2007, 2012, 1) ~ "sr_07_12",
                                 sr_year %in% seq(2013, 2019, 1) ~ "sr_13_19"),
         dist_binary = if_else(prop > 0, 1, 0)) %>% 
  group_by(catchment, dist_period) %>% 
  summarise(n_sr_dist = sum(dist_binary)) %>% 
  pivot_wider(names_from = dist_period, values_from = n_sr_dist)

nsr_sr_period <- full_join(nsr_period, sr_period) %>% 
  mutate(dist_01_06 = case_when(nsr_01_06 == 0 & sr_01_06 == 1 ~ "SR",
                                nsr_01_06 == 1 & sr_01_06 == 0 ~ "NSR",
                                nsr_01_06 == 1 & sr_01_06 == 1 ~ "Both",
                                nsr_01_06 == 0 & sr_01_06 == 0 ~ "ND",
                                TRUE ~ "remove"),
         dist_07_12 = case_when(nsr_07_12 == 0 & sr_07_12 == 1 ~ "SR",
                                nsr_07_12 == 1 & sr_07_12 == 0 ~ "NSR",
                                nsr_07_12 == 1 & sr_07_12 == 1 ~ "Both",
                                nsr_07_12 == 0 & sr_07_12 == 0 ~ "ND",
                                TRUE ~ "remove"),
         dist_13_19 = case_when(nsr_13_19 == 0 & sr_13_19 == 1 ~ "SR",
                                nsr_13_19 == 1 & sr_13_19 == 0 ~ "NSR",
                                nsr_13_19 == 1 & sr_13_19 == 1 ~ "Both",
                                nsr_13_19 == 0 & sr_13_19 == 0 ~ "ND",
                                TRUE ~ "remove")) %>% 
  filter(dist_01_06 != "remove" & 
           dist_07_12 != "remove" &
           dist_13_19 != "remove") %>% 
  unite(dist_pattern, c("dist_01_06", "dist_07_12", "dist_13_19"), 
        sep = "_", remove = F)



nsr_sr_period %>% 
  group_by(dist_pattern) %>% 
  tally() %>% view


## Dist order ----


both_catch <- NSR_SR_01_19_catch_df %>% 
  filter(dist_01_19 == "Both") %>% 
  pull(catchment)

## Create column with year of NSR dist
nsr_year <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(nsr_extract) %>% 
  filter(catchment %in% both_catch) %>% 
  rowwise() %>%
  mutate(mak=which.max(c_across(starts_with("sum"))),
         mak = mak+2000) %>% 
  dplyr::select(catchment, nsr_dist_year = mak)
view(nsr_year)

## Create column with year of SR dist
sr_year <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(sr_extract) %>% 
  filter(catchment %in% both_catch) %>% 
  rowwise() %>%
  mutate(mak=which.max(c_across(starts_with("sum"))),
         mak = mak+2000) %>% 
  dplyr::select(catchment, sr_dist_year = mak)

## Select cells where two NSR dist occurred
nsr2_cells <- NSR_SR_01_19_df %>% 
  filter(dist_01_19 == "NSR2") %>% 
  pull(catchment)

## Create column with year of first and second NSR dist 
nsr2_year <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(nsr_extract) %>% 
  filter(catchment %in% nsr2_cells) %>% 
  pivot_longer(cols = starts_with("sum"), names_to = "nsr_year", values_to = "prop") %>% 
  mutate(nsr_year = as.numeric(str_replace(nsr_year, "sum.NSR_", ""))) %>% 
  filter(prop > 0) %>% 
  group_by(catchment) %>% 
  summarise(sec_nsr_dist_year = max(nsr_year),
            first_nsr_dist_year = min(nsr_year))

## Select cells where two SR dist occurred
sr2_cells <- NSR_SR_01_19_df %>% 
  filter(dist_01_19 == "SR2") %>% 
  pull(catchment)

## Create column with year of first and second SR dist 
sr2_year <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(sr_extract) %>% 
  filter(catchment %in% sr2_cells) %>% 
  pivot_longer(cols = starts_with("sum"), names_to = "sr_year", values_to = "prop") %>% 
  mutate(sr_year = as.numeric(str_replace(sr_year, "sum.SR_", ""))) %>% 
  filter(prop > 0) %>% 
  group_by(catchment) %>% 
  summarise(sec_sr_dist_year = max(sr_year),
            first_sr_dist_year = min(sr_year))

## Calculate difference between first and second dist year and 
## combine 2 SR and 2 NSR dfs 
two_dist_year <- bind_rows(nsr2_year %>% 
                             ungroup() %>% 
                             mutate(diff = sec_nsr_dist_year-first_nsr_dist_year,
                                    order = "NSR_NSR")  %>% 
                             dplyr::select(catchment, diff, order),
                           sr2_year %>% 
                             ungroup() %>% 
                             mutate(diff = sec_sr_dist_year-first_sr_dist_year,
                                    order = "SR_SR") %>% 
                             dplyr::select(catchment, diff, order))

## combine dfs for Both dist in time series
dist_year <- left_join(nsr_year, sr_year) %>% 
  ## calc differenc in nsr and sr years
  ## negative means NSR was first
  mutate(diff = nsr_dist_year-sr_dist_year, 
         order = if_else(diff < 0, "NSR_SR", "SR_NSR")) %>% 
  ## select cells to match 2 same type dist data
  dplyr::select(catchment, diff, order) %>%
  ##bind 2 same dist data
  bind_rows(two_dist_year) %>% 
  rename(year_between_dist = diff) %>% 
  mutate(year_between_dist = abs(year_between_dist))

dist_year %>% group_by(order) %>% tally

dist_year %>% group_by(abs(year_between_dist)) %>% tally

## Most recent dist ----

### To finish

nsr_recent <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(nsr_extract) %>% 
  pivot_longer(cols = starts_with("sum"), 
               names_to = "nsr_year", values_to = "prop") %>% 
  mutate(nsr_year = as.numeric(str_replace(nsr_year, "sum.NSR_", ""))) %>% 
  filter(prop > 0) %>% 
  group_by(catchment) %>% 
  summarise(recent_dist = max(nsr_year))

sr_recent <- catch_nolakes %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(catchment = OBJECTID) %>% 
  bind_cols(ncells = ncells) %>% 
  bind_cols(sr_extract) %>% 
  pivot_longer(cols = starts_with("sum"), 
               names_to = "sr_year", values_to = "prop") %>% 
  mutate(sr_year = as.numeric(str_replace(sr_year, "sum.SR_", ""))) %>% 
  filter(prop > 0) %>% 
  group_by(catchment) %>% 
  summarise(recent_dist = max(sr_year))
  
### To finish

## Make df ----

NSR_SR_01_19_catch_df <- full_join(nsr_df, sr_df) %>% 
  left_join(., NSR_SR_01_19_df) %>% 
  left_join(., nsr_sr_period) %>% 
  left_join(., dist_year) %>% 
  bind_cols(catch_nolakes_centroid) %>% 
  bind_cols(cflux_ha = cflux_extract) %>% 
  bind_cols(ecoprov = ecoprov_extract) %>% 
  bind_cols(human_impact = impact_extract) %>% 
  left_join(., ecoprov_key) %>% 
  bind_cols(zone_num = admin_extract) %>% 
  left_join(., admin_key) %>% 
  left_join(., catch_overstory) %>% 
  left_join(., catch_land_cover) %>% 
  mutate(cflux_ha = if_else(is.nan(cflux_ha), 0, cflux_ha))

write_csv(NSR_SR_01_19_catch_df, here("data/processed/catch_cad_df.csv"))

NSR_SR_01_19_catch_df %>% group_by(dist_01_19) %>% tally()

NSR_SR_01_19_catch_df %>% group_by(zone) %>% tally()


NSR_SR_01_19_catch_df %>% 
  group_by(dist_01_19) %>% 
  summarise(mean_c = mean(cflux_ha, na.rrm = T),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  ggplot(aes(x = dist_01_19, y = mean_c)) + 
  geom_col()


library(sdmTMB)

hist(NSR_SR_01_19_catch_df$cflux_ha)

#### Prepare dataframe ----
single_dist_type_df <- NSR_SR_01_19_catch_df %>% 
  # filter(cdm_treed > 0.5 & # catchments with > 50% of area as decid, conifr or mixed
  #          prop_dist_2000 < 0.05) %>% 
  mutate(sum = rowSums(across(contains("prop")))) %>% 
  mutate(sum = case_when(dist_01_19 == "ND" ~ 1, 
                         TRUE ~ sum)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov_name = as.factor(ecoprov_name)) %>%
  # select column used in model
  dplyr::select(catchment, cflux_ha, X, Y, ecoprov_name, 
                dist_01_19, sum, human_impact) %>%
  # select single dist classes 
  filter(dist_01_19 %in% c("ND", "NSR", "SR")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_01_19 = factor(dist_01_19, levels = c("ND", "NSR", "SR")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = X/1000000,
         y = Y/1000000)

single_dist_type_df %>% 
  group_by(dist_01_19) %>% 
  tally()

summary(lm(cflux_ha ~ dist_01_19 + ecoprov_name, 
           data = single_dist_type_df))

#### Create mesh ----
sdm_df_mesh <- make_mesh(single_dist_type_df, c("x", "y"), 
                         cutoff = 0.05) 

m_to_Mm <- function(x){x*1e6}

m_to_Mm(0.001)/1000


#### Fit model ----
#### Fit model
dist_type_mod <- sdmTMB(
  data = single_dist_type_df, 
  cflux_ha ~
    dist_01_19 + 
    (1|ecoprov_name) + 
    scale(human_impact), 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(dist_type_mod)


#### Summarise ----
summary(dist_type_mod) 
tidy(dist_type_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))
res <- residuals(dist_type_mod)


single_dist_type_df$res <- residuals(dist_type_mod)
dist_type_mod$data$res <- residuals(dist_type_mod)
hist(dist_type_mod$data$res)


