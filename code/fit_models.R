
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

cad_df_3161 <- read_csv(here("data/processed/cflux_admin_dist_pixel_250m_df_3161.csv"))
cad_df_3161 <- read_csv(here("data/processed/cflux_admin_dist_pixel_500m_df_3161.csv"))


# Abbreviations -----------------------------------------------------------

## SAC = Spatial Autocorrelation

# Fig 1 -------------------------------------------------------------------

## Panel a ----

### sdmTMB ----

###  Sophisticated modelling proceedure that utilizes a mesh to group 
###  neighbouring data points to reduce the number of distance weighted
###  comparisons needed. Mesh size is set according to the units of the 
###  datas x and y coordinates. Here, data are projected to 3161 with
###  metres as units. However, when attempting fit with metres an error 
###  message was displayed saying the magnitude of the x and y values was
###  too large. Converting to megametres resulted in the error no longer
###  appearing. 

#### Prepare dataframe ----
single_dist_type_df <- cad_df_3161 %>% 
  filter(harvest_3161 == 0) %>% 
  filter(!is.na(catchment)) %>% 
  # selcet only pixels classified as treed in OLCC
  #filter(Forest == "Treed") %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov = as.factor(ECOPROVINC)) %>%
  # convert numerical harvest values to factors
  mutate(harvest = as.factor(harvest_3161)) %>%
  mutate(catch = as.factor(catchment)) %>% 
  # select column used in model
  dplyr::select(cell, cflux_ha, x, y, ecoprov, catch, 
                dist_01_19, mean_evi_3161) %>%
  # select single dist classes 
  filter(dist_01_19 %in% c("ND", "NSR", "SR")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_01_19 = factor(dist_01_19, levels = c("ND", "NSR", "SR")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = x/1000000,
         y = y/1000000)

single_dist_type_df %>% 
  group_by(dist_01_19) %>% 
  tally()

#### Create mesh ----
sdm_df_mesh <- make_mesh(single_dist_type_df, c("x", "y"), 
                         cutoff = 0.01) 

m_to_Mm <- function(x){x*1e6}

m_to_Mm(0.01)/1000


#### Fit model ----
tictoc::tic()
#### Fit model
dist_type_mod <- sdmTMB(
  data = single_dist_type_df, 
  cflux_ha ~
    dist_01_19 + 
    scale(mean_evi_3161) + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))
tictoc::toc()

sanity(dist_type_mod)

#### Summarise ----
summary(dist_type_mod) 
tidy(dist_type_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))
res <- residuals(dist_type_mod)

saveRDS(dist_type_mod, here("output/models/fig1a_sdmTMB.rds"))

dist_type_mod <-  readRDS(here("output/models/sdmTMB_mesh1km_treed_only.rds"))
single_dist_type_df$res <- residuals(dist_type_mod)
dist_type_mod$data$res <- residuals(dist_type_mod)
hist(dist_type_mod$data$res)
res_rast <- dist_type_mod$data %>% 
  filter(!is.infinite(res)) %>% 
  mutate(x = x*1000000, 
         y = y*1000000) %>% 
  ungroup() %>% 
  dplyr::select(x, y, res) %>% 
  rename(z = res) %>% 
  terra::rast(type = "xyz")

terra::plot(res_rast)
terra::autocor(res_rast)
terra::writeRaster(res_rast, here::here("tmp/residual_raster.tif"))
# 20% frac, 0.005 cutoff = NaN
# 20% frac, 0.01 cutoff = 54228.03
# 20% frac, 0.1 cutoff = 58689.51 
# 20% frac, 0.5 cutoff = 60392.62

#### Considerations ----

#### A 1km mesh seems to have a good effect on SAC
#### Errors are common when fitting on subsets of data. 
####  NaN, nlminb most common; seem to relate to convergence
####  changing optimization parameters had little effect
####  term[i] error common when smoothing term was included
#### Ecoprovince seemed to improve SAC

### GAM ----

### General additive models allow for the inclusion of a smoothing term 
### that can capture non-linear effects of predictors. In our case, the 
### latitude and longitude of our data may have a non-linear effect. When 
### the aim of an analysis is to infer beyond the scope of the input data
### spatial autocorrelation must be accounted for in the model. The idea 
### behind GAMs is that the smoothing parameter can better capture the 
### spatial effects and ideally remove those effects from the residuals. 

#### Prepare dataframe ----
single_dist_type_df <- cad_df_3161 %>% 
  #filter(harvest_3161 == 0) %>% 
  #filter(Forest == "Treed") %>% 
  mutate(ecoprov = as.factor(ECOPROVINC)) %>% 
  #mutate(harvest = as.factor(harvest_3161)) %>% 
  dplyr::select(cell, cflux_ha, x, y, dist_01_19, mean_t_01_19,
                ecoprov, harvest_3161, ontario_human_footprint, 
                yrs_since_recent_dist, LC, 
                mean_evi_3161, evi_sen_slope, 
                OCCLO, UCCLO) %>% 
  filter(dist_01_19 %in% c("ND", "NSR", "SR")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_01_19 = factor(dist_01_19, levels = c("ND", "NSR", "SR")),
         harvest = factor(harvest_3161, levels = c(0, 2, 4)))

quantile(single_dist_type_df$cflux_ha, probs = seq(0,1, 0.001))

#### Fit model ----
tictoc::tic()
gam_dist_type <- gam(cflux_ha ~ 
                       dist_01_19 + 
                       ecoprov + 
                       s(x, y, k = 60),
                family = gaussian, data=single_dist_type_df,
                method = "REML", na.action = "na.fail")
res_gam <- residuals.gam(gam_dist_type)
tictoc::toc() # full data fits in ca. 70 seconds

gam.check(gam_dist_type)

#### Summarize ----
summary(gam_dist_type)

plot(gam_dist_type)
AIC(gam_dist_type)

single_dist_type_df %>% 
  group_by(dist_01_19) %>%
  summarise(mean_lat = mean(y),
            med_lat = median(y),
            sd_lat = sd(y))

estimate_means(gam_dist_type, at = "dist_01_19")
estimate_contrasts(gam_dist_type)
(emmeans(gam_dist_type, pairwise ~ dist_01_19))

single_dist_type_df$res <- residuals(gam_dist_type)
hist(single_dist_type_df$res)
res_rast <- single_dist_type_df %>% 
  ungroup() %>% 
  dplyr::select(x, y, res) %>% 
  rename(z = res) %>% 
  rast(type = "xyz")
plot(res_rast)
autocor(res_rast)
writeRaster(res_rast, here("tmp/gam_res_rast.tif"), overwrite = T)

library(DHARMa)
res = simulateResiduals(gam_dist_type) 
testSpatialAutocorrelation(res, x =  single_dist_type_df$x, 
                           y = single_dist_type_df$y, plot = F)
## 20% of data, bs = "gp, k = 100, m = 2; exhaust vector memory
## 5% of data, bs = "gp, k = 100, m = 2; exhaust vector memory
## 1% of data, bs = "gp, k = 100, m = 2; exhaust vector memory
## 5000 per dist type, bs = "gp, k = 100, m = 2; observed = 3.9022e-02, expected = -6.6671e-05, p-value < 2.2e-16
## 5% of data, te function; exhaust vector memory


### spgwr ----

### Typical statistical modelling attempt to fit global models for all
### input data. For data with a inherit spatial component it can be 
### more approriate to fit local models for data nearest to one another. 
### Geographically weighted regressions use this very approach. A kernel 
### is selected to group nearby data points into local subsets. A model is
### then fot to each of these subsets and a global weighted effect is 
### calculated. Here, an adaptive kernel seemed to perform best. Unlike a 
### fixed kernel, which assues the a consistent distance for creating 
### subgroups, an adpative kernel allows the distance to vary according to 
### the spatial distribution of the data. 

#### Prepare dataframe ----
single_dist_type_df <- cad_df_3161 %>% 
  filter(Forest == "Treed") %>% 
  mutate(ecoprov = as.factor(ECOPROVINC)) %>% 
  dplyr::select(cell, cflux_ha, x, y, ecoprov, mean_t_01_19, dist_01_19) %>% 
  filter(dist_01_19 %in% c("ND", "NSR", "SR")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_01_19 = factor(dist_01_19, levels = c("ND", "NSR", "SR"))) %>% 
  group_by(dist_01_19) %>%
  sample_n(1500, replace = T)

single_dist_type_df %>% 
  group_by(dist_01_19) %>% 
  tally()

#### Fit model ----
g.adapt.gauss <- gwr.sel(cflux_ha ~ dist_01_19 + ecoprov + mean_t_01_19, 
                         data=single_dist_type_df,
                         coords=cbind(single_dist_type_df$x, 
                                      single_dist_type_df$y),
                         adapt=TRUE)
res.adpt <- gwr(cflux_ha ~ dist_01_19 + ecoprov + mean_t_01_19, 
                data=single_dist_type_df,
                coords=cbind(single_dist_type_df$x, 
                             single_dist_type_df$y),
                adapt=g.adapt.gauss, se.fit = T)
res.adpt 
res.adpt$SDF$gwr.e # model residuals

#### Save models ----
saveRDS(res.adpt, here("output/models/dist_type_gwr_4500n.rds"))

#### Considerations ----

#### Ecoprovinces need to be included in the gwr models to remove SAC
#### Subsetting data appears to consistently remove SAC  

### GWmodel ----

#### Prepare dataframe ----
single_dist_type_df <- cad_df_3161 %>% 
  # selcet only pixels classified as treed in OLCC
  #filter(Forest == "Treed") %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov = as.factor(ECOPROVINC)) %>%
  mutate(harvest = as.factor(harvest_3161)) %>% 
  filter(harvest == 0) %>% 
  # select column used in model
  dplyr::select(cell, cflux_ha, x, y, mean_evi_3161, 
                dist_01_19, harvest) %>%
  # select single dist classes 
  filter(dist_01_19 %in% c("ND", "NSR", "SR")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_01_19 = factor(dist_01_19, levels = c("ND", "NSR", "SR")))

coordinates(single_dist_type_df) <- ~ x + y

#### Fit model ----
tictoc::tic()
res2 <- gwr.scalable(formula = cflux_ha ~ 
                      dist_01_19 + 
                       scale(mean_evi_3161), 
                    data = single_dist_type_df, 
                    longlat = T,
                    bw.adapt = 314) #5km = 314, 7.5km = 591
tictoc::toc()
res2
saveRDS(res, here("output/models/dist_type_gwmodel_bw100_treedonly.rds"))

res <- readRDS(here("output/models/dist_type_gwmodel_bw100_treedonly.rds"))

hist(res2$SDF$residual)
res_rast <- res2$SDF %>%
  as.data.frame() %>%
  dplyr::select(x, y = y.1, res = residual) %>% 
  rename(z = res) %>% 
  terra::rast(type = "xyz")

terra::plot(res_rast)
terra::autocor(res_rast)

hist(gw$SDF$residual)
res_rast <- gw2$SDF %>%
  as.data.frame() %>%
  dplyr::select(x, y = y.1, res = residual) %>% 
  rename(z = res) %>% 
  terra::rast(type = "xyz")

terra::plot(res_rast)
fr <- matrix(c(0,1,0,1,0,1,0,1,0), nrow=3)
f <- matrix(c(1,1,1,1,0,1,1,1,1), nrow=3)
moran <- terra::autocor(res_rast, w = fr, global = F, method = "moran")
plot(moran)
hist(moran)
writeRaster(moran, here("tmp/local_moran.tif"))

r <- raster(res_rast)

Moran(r)
test <- as.matrix(res_rast)
# bw.adapt = 314; Moran = 0.6997144
#bw.adapt = 591; Ehaust vector memory


#### Summarise ----
summ_df <- single_dist_type_df %>% as.data.frame() 
newdat <- expand_grid(cflux_ha = mean(summ_df$cflux_ha),
                      dist_01_19 = as.factor(unique(summ_df$dist_01_19)),
                      ecoprov = as.factor(unique(summ_df$ecoprov)),
                      x = mean(summ_df$x),
                      y = mean(summ_df$y),
                      ontario_human_footprint = mean(summ_df$ontario_human_footprint))

coordinates(newdat) <- ~ x + y

res_pred <- gwr.predict(cflux_ha ~ 
                      dist_01_19 + 
                      ecoprov +
                      ontario_human_footprint, 
                    data = newdat,
                    kernel = "gaussian",
                    adaptive = T)

### spaMM ----

### These models use a standard distance weighting approach to account
### for SAC. The lat/long coordinates are fed into the Matern parameter
### and a distance matrix is produced. The downside is the modelling 
### approach quickly exhausts vector memory and tehrefore requires 
### subsampling. Even with smaller subsets the model can be slow to fit. 

#### Prepare dataframe ----

single_dist_type_df <- cad_df_3161 %>% 
  filter(Forest == "Treed") %>% 
  mutate(ecoprov = as.factor(ECOPROVINC)) %>% 
  dplyr::select(cell, cflux_ha, x, y, ecoprov, mean_t_01_19, dist_01_19) %>% 
  filter(dist_01_19 %in% c("ND", "NSR", "SR")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_01_19 = factor(dist_01_19, levels = c("ND", "NSR", "SR"))) %>% 
  group_by(dist_01_19) %>%
  sample_n(1500, replace = T)

#### Fit model ----
mod_spamm <-
  fitme(cflux_ha ~ 
          dist_01_19 + ecoprov + mean_t_01_19, 
          Matern(1 |x + y),
        data = single_dist_type_df,
        family = "gaussian", method = "REML"
  ) 
#### Summarize ----
summary(mod_spamm)

### RF ----

### Random forests are non-parametric models that use bootstrapping and
### decison trees to construct model predictions. Since they are non-
### parametric it is argued that they handle SAC well. However, the 
### existence of spatail random forest modelling packages suggests there 
### may be more sophisticaed methids needed to truly deal with SAC. 
### Furthermore, RF do not allow for extrapolate outside of the range of 
### input data. This fact may be why they are better suited for SAC
### data since SAC is only an issue if you wish to extrapolate. 

## Panel b ----

nsr_pattern_df <- cad_df_3161 %>% 
  mutate(dist_group = case_when(dist_pattern == "ND_ND_ND" ~ "p0",
                                dist_pattern == "NSR_ND_ND" ~ "p1",
                                dist_pattern == "NSR_NSR_ND" ~ "p2",
                                dist_pattern == "NSR_NSR_NSR" ~ "p3",
                                TRUE ~ "remove")) %>% 
  filter(dist_group != "remove") %>% 
  filter(!is.na(dist_group)) %>% 
  filter(harvest_3161 == 0) %>% 
  filter(!is.na(catchment)) %>% 
  mutate(catch = as.factor(catchment)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov = as.factor(ECOPROVINC)) %>%
  dplyr::select(cell, cflux_ha, x, y, mean_evi_3161,
                ecoprov, dist_group, catch) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(# convert to Megametres for better processing
    # recommended by sdmTMB message when creating mesh using m
    x = x/1000000,
    y = y/1000000)

#### Create mesh ----
sdm_df_mesh <- make_mesh(nsr_pattern_df, c("x", "y"), 
                         cutoff = 0.01) 

#### Fit model ----
tictoc::tic()
#### Fit model
nsr_period_mod <- sdmTMB(
  data = nsr_pattern_df, 
  cflux_ha ~
    dist_group + 
    scale(mean_evi_3161) + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))
tictoc::toc()

sanity(nsr_period_mod)

#### Summarise ----
summary(nsr_period_mod) 
tidy(nsr_period_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

saveRDS(nsr_period_mod, here("output/models/fig1b_nsr_sdmTMB.rds"))


sr_pattern_df <- cad_df_3161 %>% 
  mutate(dist_group = case_when(dist_pattern == "ND_ND_ND" ~ "p0",
                                dist_pattern == "SR_ND_ND" ~ "p1",
                                dist_pattern == "SR_SR_ND" ~ "p2",
                                dist_pattern == "SR_SR_SR" ~ "p3",
                                TRUE ~ "remove")) %>% 
  filter(dist_group != "remove") %>% 
  filter(!is.na(dist_group)) %>% 
  filter(harvest_3161 == 0) %>% 
  filter(!is.na(catchment)) %>% 
  mutate(catch = as.factor(catchment)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov = as.factor(ECOPROVINC)) %>%
  dplyr::select(cell, cflux_ha, x, y, mean_evi_3161,
                ecoprov, dist_group, catch) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(# convert to Megametres for better processing
    # recommended by sdmTMB message when creating mesh using m
    x = x/1000000,
    y = y/1000000)

#### Create mesh ----
sdm_df_mesh <- make_mesh(sr_pattern_df, c("x", "y"), 
                         cutoff = 0.01) 

#### Fit model ----
tictoc::tic()
#### Fit model
sr_period_mod <- sdmTMB(
  data = sr_pattern_df, 
  cflux_ha ~
    dist_group + 
    scale(mean_evi_3161) + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))
tictoc::toc()

sanity(sr_period_mod)

#### Summarise ----
summary(sr_period_mod) 
tidy(sr_period_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

saveRDS(sr_period_mod, here("output/models/fig1b_sr_sdmTMB.rds"))

# Fig 2 -------------------------------------------------------------------

## Panel a ----
#### Prepare dataframe
two_dist_type_df <- cad_df_3161 %>% 
  filter(harvest_3161 == 0) %>% 
  filter(!is.na(catchment)) %>% 
  filter(!is.na(order)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov = as.factor(ECOPROVINC)) %>%
  mutate(catch = as.factor(catchment)) %>% 
  # select column used in model
  dplyr::select(cell, cflux_ha, x, y, ecoprov, catch,
                order, mean_evi_3161) %>%
  na.omit() %>% 
  droplevels() %>% 
  mutate(order = factor(order, levels = c("NSR_NSR", "NSR_SR", 
                                          "SR_NSR", "SR_SR")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = x/1000000,
         y = y/1000000)

two_dist_type_df %>% group_by(order) %>% tally()

#### Make mesh
sdm_df_mesh <- make_mesh(two_dist_type_df, c("x", "y"), 
                         cutoff = 0.01) 

#### Fit model
dist_order_mod <- sdmTMB(
  data = two_dist_type_df, 
  cflux_ha ~
    order + 
    scale(mean_evi_3161) + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(dist_order_mod)

#### Summarise ----
summary(dist_order_mod) 
tidy(dist_order_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

saveRDS(dist_order_mod, here("output/models/fig2a_sdmTMB.rds"))


## Panel b ----
dist_year_df <- cad_df_3161 %>% 
  filter(harvest_3161 == 0) %>% 
  filter(!is.na(catchment)) %>% 
  filter(!is.na(order)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov = as.factor(ECOPROVINC)) %>%
  mutate(catch = as.factor(catchment)) %>% 
  mutate(yrs_between_dist = abs(diff)) %>% 
  # select column used in model
  dplyr::select(cell, cflux_ha, x, y, ecoprov, catch,
                order, mean_evi_3161, yrs_between_dist) %>%
  na.omit() %>% 
  droplevels() %>% 
  mutate(order = factor(order, levels = c("NSR_NSR", "NSR_SR", 
                                          "SR_NSR", "SR_SR")),
         # convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = x/1000000,
         y = y/1000000)

dist_year_df %>% group_by(order) %>% tally()

#### Make mesh
sdm_df_mesh <- make_mesh(dist_year_df, c("x", "y"), 
                         cutoff = 0.01) 

#### Fit model
dist_year_mod <- sdmTMB(
  data = dist_year_df, 
  cflux_ha ~
    order*yrs_between_dist + 
    #scale(mean_evi_3161) + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(dist_year_mod)

#### Summarise ----
summary(dist_order_mod) 
tidy(dist_order_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

# Fig 3 -------------------------------------------------------------------

## Compare proportion of pixels  of each dist type (no, one, or two) with the 
## protection status. Each dist type is it's own column where 1 represents 
## occurrence (success) and 0 is no occurrence (failure). A binomial model 
## is threfore fit to compare the probabilty of each dist type across protection
## status. 
## Protection status is determined according to the allowed activities of the 
## status. Semi protected included areas that do not have strict regualtions of 
## no development or are not explicitly designated as areas for protecting 
## flora and fauna. 
## Due to large sample sizes bootstrapping may be necessary. A spatial structure 
## should also be included in the model. 
 
#### Prepare df
admin_dist_type_df <- cad_df_3161 %>% 
  filter(harvest_3161 == 0) %>% 
  filter(!is.na(catchment)) %>% 
  mutate(order = if_else(is.na(order), dist_01_19, order)) %>%
  filter(!is.na(order)) %>% 
  # convert numerical Ecoprovince values to factors
  mutate(ecoprov = as.factor(ECOPROVINC)) %>%
  mutate(catch = as.factor(catchment)) %>% 
  mutate(dist_type = case_when(str_detect(order, "_") ~ "two",
                               order == "ND" ~ "none",
                               order == "NSR" | order == "SR" ~ "one"),
         zone_type = case_when(zone == "General Use Area" ~ "Unprotected",
                               zone == "Enhanced Management Area" | 
                                 zone == "Indian Reserve" | 
                                 zone == "Forest Reserve" | 
                                 zone == "Wilderness Area" ~ "Semi-protected",
                               TRUE ~ "Protected")) %>% 
  select(cell, x, y, dist_type, zone_type, catch, ecoprov) %>% 
  mutate(val = 1) %>% 
  pivot_wider(names_from = dist_type, values_from = val, values_fill = 0) %>% 
  mutate(# convert to Megametres for better processing
         # recommended by sdmTMB message when creating mesh using m
         x = x/1000000,
         y = y/1000000)

admin_dist_type_df %>% group_by(two) %>% tally()


#### Make mesh
sdm_df_mesh <- make_mesh(admin_dist_type_df, c("x", "y"), 
                         cutoff = 0.01) 

#### Fit model
admin_none_mod <- sdmTMB(
  data = admin_dist_type_df, 
  none ~
    zone_type + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = binomial(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(admin_none_mod)

#### Summarise ----
summary(admin_none_mod) 
tidy(admin_none_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

saveRDS(admin_none_mod, here("output/models/fig3_none_sdmTMB.rds"))

#### Fit model
admin_one_mod <- sdmTMB(
  data = admin_dist_type_df, 
  one ~
    zone_type + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = binomial(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(admin_one_mod)

#### Summarise ----
summary(admin_one_mod) 
tidy(admin_one_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

saveRDS(admin_one_mod, here("output/models/fig3_one_sdmTMB.rds"))

#### Fit model
admin_two_mod <- sdmTMB(
  data = admin_dist_type_df, 
  two ~
    zone_type + 
    (1|catch), 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = binomial(),
  control = sdmTMBcontrol(newton_loops = 1))

sanity(admin_two_mod)

#### Summarise ----
summary(admin_two_mod) 
tidy(admin_two_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

saveRDS(admin_two_mod, here("output/models/fig3_two_sdmTMB.rds"))


mod_none <- glm(none ~ zone_type, data = test_df, family = binomial)
summary(mod_none)
estimate_means(mod_none, transform = "response")
estimate_contrasts(mod_none, transform = "response")
multcomp::cld((emmeans(mod_none, pairwise ~ zone_type, trans = "response")))

gam_none <- gam(none ~ zone_type +  
              s(x, y, bs='gp', k=100, m=2),
            family = binomial, data=test_df,
            method = "REML", na.action = "na.fail")
summary(gam_none)
estimate_means(gam_none, transform = "response")
estimate_contrasts(gam_none, transform = "response")
(emmeans(gam_none, pairwise ~ zone_type, trans = "response"))

res = simulateResiduals(gam_none) 

# Standard use
testSpatialAutocorrelation(res, x =  test_df$x, y = test_df$y, plot = F)

mod_one <- glm(one ~ zone_type, data = test_df, family = binomial)
summary(mod_one)
estimate_means(mod_one, transform = "response")
estimate_contrasts(mod_one, transform = "response")
multcomp::cld((emmeans(mod_one, pairwise ~ zone_type, trans = "response")))


mod_two <- glm(two ~ zone_type, data = test_df, family = binomial)
summary(mod_two)
estimate_means(mod_two, transform = "response")
estimate_contrasts(mod_two, transform = "response")
multcomp::cld((emmeans(mod_two, pairwise ~ zone_type, trans = "response")))

