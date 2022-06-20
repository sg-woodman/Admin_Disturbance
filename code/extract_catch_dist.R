


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


ncells <- exact_extract(sr_dist[[1]], catch_nolakes, 
                        "count")

cflux_extract <- exact_extract(cflux_30_ha_3161, catch_nolakes, 
                               "mean")

ecoprov_extract <- exact_extract(ecoprov_rast, catch_nolakes, 
                                 "mode")

admin_extract <- exact_extract(admin_rast_250_3161, catch_nolakes, 
                                 "mode")

# Create df ---------------------------------------------------------------

catch_nolakes_centroid <- st_centroid(catch_nolakes, of_largest_polygon = T) %>% 
  st_coordinates()

NSR_SR_01_19_catch_df <- dist_df %>% 
  bind_cols(catch_nolakes_centroid) %>% 
  bind_cols(cflux_ha = cflux_extract) %>% 
  bind_cols(ecoprov = ecoprov_extract) %>% 
  left_join(., ecoprov_key) %>% 
  bind_cols(zone_num = admin_extract) %>% 
  left_join(., admin_key) %>% 
  left_join(., catch_overstory) %>% 
  left_join(., catch_land_cover) %>% 
  mutate(dist_01_19 = case_when(nsr_n_dist == 0 & sr_n_dist == 1 ~ "SR",
                                nsr_n_dist == 1 & sr_n_dist == 0 ~ "NSR",
                                nsr_n_dist == 1 & sr_n_dist == 1 ~ "Both",
                                nsr_n_dist == 0 & sr_n_dist == 0 ~ "ND",
                                nsr_n_dist == 0 & sr_n_dist == 2 ~ "SR2",
                                nsr_n_dist == 2 & sr_n_dist == 0 ~ "NSR2",
                                TRUE ~ "remove")) %>% 
  filter(dist_01_19 != "remove")

write_csv(NSR_SR_01_19_catch_df, here("data/processed/catch_cad_df.csv"))

NSR_SR_01_19_catch_df %>% group_by(dist_01_19) %>% tally()

NSR_SR_01_19_catch_df %>% group_by(zone) %>% tally()


NSR_SR_01_19_catch_df %>% 
  group_by(dist_01_19) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  ggplot(aes(x = dist_01_19, y = mean_c)) + 
  geom_col()


library(sdmTMB)

hist(NSR_SR_01_19_catch_df$cflux_ha)

#### Prepare dataframe ----
single_dist_type_df <- NSR_SR_01_19_catch_df %>% 
  filter(cdm_treed > 0.5 & # catchments with > 50% of area as decid, conifr or mixed
           prop_dist_2000 < 0.05) %>% 
  mutate(sum = rowSums(across(contains("prop")))) %>% 
  mutate(sum = case_when(dist_01_19 == "ND" ~ 1, 
                         TRUE ~ sum)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov_name = as.factor(ecoprov_name)) %>%
  # select column used in model
  dplyr::select(catchment, cflux_ha, X, Y, ecoprov_name, 
                dist_01_19, sum) %>%
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
                         cutoff = 0.01) 

m_to_Mm <- function(x){x*1e6}

m_to_Mm(0.05)/1000


#### Fit model ----
#### Fit model
dist_type_mod <- sdmTMB(
  data = single_dist_type_df, 
  cflux_ha ~
    dist_01_19 + 
    ecoprov_name, 
  weights = single_dist_type_df$sum,
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


