# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sdmTMB)
library(mgcv)
library(spgwr)
library(GWmodel)
library(easystats)
library(emmeans)
library(terra)

catch_cad_df <- read_csv(here("data/processed/catch_cad_df.csv"))


# Functions ---------------------------------------------------------------

## Convert Megametres to metres for interpretting mesh size
m_to_Mm <- function(x){x*1e6}


# Fig 1 -------------------------------------------------------------------

## Panel a ----
### sdmTMB ----
#### Prepare dataframe 
single_dist_type_df <- catch_cad_df %>% 
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

#### Create mesh 
sdm_df_mesh <- make_mesh(single_dist_type_df, c("x", "y"), 
                         cutoff = 0.05) 

m_to_Mm(0.05)/1000

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

#### Summarise 
summary(dist_type_mod) 
tidy(dist_type_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

single_dist_type_df$res <- residuals(dist_type_mod)
dist_type_mod$data$res <- residuals(dist_type_mod)
hist(dist_type_mod$data$res)

## Panel b ----
### sdmTMB ----
#### Prepare dataframes

##### NSR
nsr_period <- catch_cad_df %>% 
  mutate(period_group = case_when(dist_pattern == "ND_ND_ND" ~ "p0",
                                  dist_pattern == "NSR_ND_ND" ~ "p1",
                                  dist_pattern == "NSR_NSR_ND" ~ "p2",
                                  dist_pattern == "NSR_NSR_NSR" ~ "p3",
                                  TRUE ~ "remove")) %>% 
  filter(period_group != "remove") %>% 
  filter(!is.na(period_group)) %>% 
  mutate(ecoprov_name = as.factor(ecoprov_name)) %>%
  # select column used in model
  dplyr::select(catchment, cflux_ha, X, Y, ecoprov_name, 
                period_group, human_impact) %>%
  na.omit() %>% 
  droplevels() %>% 
  mutate(period_group = factor(period_group, levels = c("p0", "p1", 
                                          "p2", "p3")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = X/1000000,
         y = Y/1000000)

nsr_period %>% group_by(period_group) %>% tally()

#### Create mesh 
sdm_df_mesh <- make_mesh(nsr_period, c("x", "y"), 
                         cutoff = 0.05) 

m_to_Mm(0.05)/1000

#### Fit model 
nsr_repeat_mod <- sdmTMB(
  data = nsr_period, 
  cflux_ha ~
    period_group + 
    (1|ecoprov_name) + 
    scale(human_impact), 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(nsr_repeat_mod)

#### Summarise 
summary(nsr_repeat_mod) 
tidy(nsr_repeat_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

single_dist_type_df$res <- residuals(dist_type_mod)
dist_type_mod$data$res <- residuals(dist_type_mod)
hist(dist_type_mod$data$res)

##### SR
sr_period <- catch_cad_df %>% 
  mutate(period_group = case_when(dist_pattern == "ND_ND_ND" ~ "p0",
                                  dist_pattern == "SR_ND_ND" ~ "p1",
                                  dist_pattern == "SR_SR_ND" ~ "p2",
                                  dist_pattern == "SR_SR_SR" ~ "p3",
                                  TRUE ~ "remove")) %>% 
  filter(period_group != "remove") %>% 
  filter(!is.na(period_group)) %>% 
  mutate(ecoprov_name = as.factor(ecoprov_name)) %>%
  # select column used in model
  dplyr::select(catchment, cflux_ha, X, Y, ecoprov_name, 
                period_group, human_impact) %>%
  na.omit() %>% 
  droplevels() %>% 
  mutate(period_group = factor(period_group, levels = c("p0", "p1", 
                                                        "p2", "p3")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = X/1000000,
         y = Y/1000000) %>% 
  filter(period_group != "p3")

sr_period %>% group_by(period_group) %>% tally()

#### Create mesh 
sdm_df_mesh <- make_mesh(sr_period, c("x", "y"), 
                         cutoff = 0.05) 

m_to_Mm(0.05)/1000

#### Fit model 
sr_repeat_mod <- sdmTMB(
  data = sr_period, 
  cflux_ha ~
    period_group + 
    (1|ecoprov_name) + 
    scale(human_impact), 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(sr_repeat_mod)

#### Summarise 
summary(nsr_repeat_mod) 
tidy(nsr_repeat_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

single_dist_type_df$res <- residuals(dist_type_mod)
dist_type_mod$data$res <- residuals(dist_type_mod)
hist(dist_type_mod$data$res)

# Fig 2 -------------------------------------------------------------------

unique(catch_cad_df$order)
## Panel a ----
### sdmTMB
#### Prepare dataframe
two_dist_type_df <- catch_cad_df %>% 
  filter(!is.na(order)) %>% 
  # mutate(sum = rowSums(across(contains("prop")))) %>% 
  # mutate(sum = case_when(dist_01_19 == "ND" ~ 1, 
  #                        TRUE ~ sum)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov_name = as.factor(ecoprov_name)) %>%
  # select column used in model
  dplyr::select(catchment, cflux_ha, X, Y, ecoprov_name, 
                order, human_impact) %>%
  na.omit() %>% 
  droplevels() %>% 
  mutate(order = factor(order, levels = c("NSR_NSR", "NSR_SR", 
                                          "SR_NSR", "SR_SR")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = X/1000000,
         y = Y/1000000)

two_dist_type_df %>% group_by(order) %>% tally()

#### Create mesh ----
sdm_df_mesh <- make_mesh(two_dist_type_df, c("x", "y"), 
                         cutoff = 0.05) 

m_to_Mm(0.05)/1000

#### Fit model ----
dist_order_mod <- sdmTMB(
  data = two_dist_type_df, 
  cflux_ha ~
    order + 
    (1|ecoprov_name) + 
    scale(human_impact), 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(dist_order_mod)

#### Summarise ----
summary(dist_order_mod) 
tidy(dist_order_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

single_dist_type_df$res <- residuals(dist_type_mod)
dist_type_mod$data$res <- residuals(dist_type_mod)
hist(dist_type_mod$data$res)

## Panel b ----
### sdmTMB
#### Prepare dataframe
dist_year_df <- catch_cad_df %>% 
  filter(!is.na(order)) %>% 
  # mutate(sum = rowSums(across(contains("prop")))) %>% 
  # mutate(sum = case_when(dist_01_19 == "ND" ~ 1, 
  #                        TRUE ~ sum)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov_name = as.factor(ecoprov_name)) %>%
  # select column used in model
  dplyr::select(catchment, cflux_ha, X, Y, ecoprov_name, 
                order, human_impact, year_between_dist) %>%
  na.omit() %>% 
  droplevels() %>% 
  mutate(order = factor(order, levels = c("NSR_NSR", "NSR_SR", 
                                          "SR_NSR", "SR_SR")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = X/1000000,
         y = Y/1000000)

dist_year_df %>% group_by(order) %>% tally()
dist_year_df %>% group_by(year_between_dist) %>% tally()

range(dist_year_df$year_between_dist)

#### Create mesh ----
sdm_df_mesh <- make_mesh(dist_year_df, c("x", "y"), 
                         cutoff = 0.05) 

m_to_Mm(0.05)/1000

#### Fit model ----
dist_year_mod <- sdmTMB(
  data = dist_year_df, 
  cflux_ha ~
    order*year_between_dist + 
    (1|ecoprov_name) + 
    scale(human_impact), 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(dist_year_mod)

#### Summarise ----
summary(dist_year_mod) 
tidy(dist_year_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

single_dist_type_df$res <- residuals(dist_type_mod)
dist_type_mod$data$res <- residuals(dist_type_mod)
hist(dist_type_mod$data$res)


# Fig 3 -------------------------------------------------------------------

## sdmTMB
### Prepare dataframe
dist_admin_df <- catch_cad_df %>% 
  mutate(order = if_else(is.na(order), dist_01_19, order)) %>%
  filter(!is.na(order)) %>% 
  mutate(dist_type = case_when(str_detect(order, "_") ~ "two",
                               order == "ND" ~ "none",
                               order == "NSR" | order == "SR" ~ "one"),
         zone_type = case_when(zone == "General Use Area" ~ "Unprotected",
                               zone == "Enhanced Management Area" | 
                                 zone == "Indian Reserve" | 
                                 zone == "Forest Reserve" | 
                                 zone == "Wilderness Area" ~ "Semi-protected",
                               TRUE ~ "Protected")) %>% 
  dplyr::select(catchment, X, Y, dist_type, zone_type) %>% 
  mutate(val = 1) %>% 
  pivot_wider(names_from = dist_type, values_from = val, values_fill = 0) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(zone_type = factor(zone_type, levels = c("Unprotected", 
                                                  "Semi-protected",
                                                  "Protected")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = X/1000000,
         y = Y/1000000)

dist_admin_df %>% group_by(zone_type) %>% tally()


#### Create mesh ----
sdm_df_mesh <- make_mesh(dist_admin_df, c("x", "y"), 
                         cutoff = 0.05) 

m_to_Mm(0.05)/1000

#### Fit model ----
##### No dist
no_dist_admin_mod <- sdmTMB(
  data = dist_admin_df, 
  none ~
    zone_type, 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = binomial(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(no_dist_admin_mod)

#### Summarise ----
summary(no_dist_admin_mod) 
tidy(no_dist_admin_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

##### One dist
one_dist_admin_mod <- sdmTMB(
  data = dist_admin_df, 
  one ~
    zone_type, 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = binomial(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(one_dist_admin_mod)

#### Summarise ----
summary(one_dist_admin_mod) 
tidy(one_dist_admin_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

##### Two dist
two_dist_admin_mod <- sdmTMB(
  data = dist_admin_df, 
  two ~
    zone_type, 
  #weights = single_dist_type_df$sum,
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = binomial(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(two_dist_admin_mod)

#### Summarise ----
summary(two_dist_admin_mod) 
tidy(two_dist_admin_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))
