library(tidyverse)
library(here)
library(sdmTMB)
library(tictoc)

c_dist_period_df <-  read_csv(here("data/processed/c_dist_period_df.csv"))

unique(c_dist_period_df$period_group)

c_dist_period_df %>% group_by(dist_type) %>% tally()

#### Create df
sdm_mod_df <- c_dist_period_df %>% 
  dplyr::select(cell, x, y, cflux_ha, dist_type, zone, period_group) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_type = factor(dist_type, levels = c("ND", "NSR", "SR")),
         period_group = factor(period_group, levels = c("p0", "p1", "p2", "p3")),
         x = x/1000000,
         y = y/1000000) %>% 
  group_by(dist_type) %>% 
  sample_frac(0.25)
  

(range(sdm_mod_df$y)[2] - range(sdm_mod_df$y)[1])/5000
(range(sdm_mod_df$x)[2] - range(sdm_mod_df$x)[1])/5000
#### Create mesh
sdm_df_mesh <- make_mesh(sdm_mod_df, c("x", "y"), 
                           cutoff = 0.01)
tictoc::tic()
#### Fit model
dist_type_mod <- sdmTMB(
  data = sdm_mod_df, 
  formula = cflux_ha ~ 
    dist_type, 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian())
tictoc::toc()
#### Summarise
summary(dist_type_mod)
tidy(dist_type_mod, conf.int = T, 
                     effects = c("fixed", "ran_pars"))


res_sdm <- residuals(dist_type_mod)
hist(res_sdm)


# (Intercept) -51.70487 10.038136 -71.37925 -32.030483
# 2 dist_typeNSR -14.11199  4.466491 -22.86615  -5.357827
# 3  dist_typeSR  34.51898  4.380590  25.93318  43.104776
# 
# 
# term  estimate std.error  conf.low  conf.high
# 1  (Intercept) -33.58354 7.0398917 -47.38148 -19.785607
# 2 dist_typeNSR -10.38584 0.3095192 -10.99249  -9.779192
# 3  dist_typeSR  22.18611 1.1372968  19.95705  24.415168

## cflux vs dist order, single and double dist both included
unique(two_dist_order_df$order)
sdm_mod_df <- two_dist_order_df %>% 
  dplyr::select(cell, x, y, cflux_ha, zone, order, LC) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(order = factor(order, levels = c("ND", "NSR", "SR", "NSR_NSR", 
                                              "SR_SR", "NSR_SR", "SR_NSR")),
         x = x/1000000,
         y = y/1000000) %>% 
  group_by(order) %>% 
  sample_frac(0.15)

sdm_df_mesh <- make_mesh(sdm_mod_df, c("x", "y"), 
                         cutoff = 0.01)
tictoc::tic()
#### Fit model
dist_order_mod <- sdmTMB(
  data = sdm_mod_df, 
  formula = cflux_ha ~ 
    order, 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian())
tictoc::toc()
#### Summarise
summary(dist_order_mod)
tidy(dist_order_mod, conf.int = T, 
     effects = c("fixed", "ran_pars")) %>% 
  as.data.frame() %>% 
  mutate(term = case_when(term == "(Intercept)" ~ "ND",
                          TRUE ~ term),
         cflux = .[1,2]  + estimate,
         cflux = case_when(term == "ND" ~ .[1,2],
                          TRUE ~ cflux),
         cflux_low = .[1,2]  + conf.low,
         cflux_low = case_when(term == "ND" ~ .[1,2],
                           TRUE ~ cflux_low),
         cflux_high = .[1,2]  + conf.high,
         cflux_high = case_when(term == "ND" ~ .[1,2],
                               TRUE ~ cflux_high),
         term = str_replace(term, "order", ""),
         term = factor(term, levels = c("ND", "NSR", "SR", "NSR_NSR", 
                                          "SR_SR", "NSR_SR", "SR_NSR"))) %>% 
  ggplot(aes(x = term, y = cflux)) + 
  geom_col(colour = "black", width = 0.75) + 
  geom_errorbar(aes(ymin = cflux_low, ymax = cflux_high), 
                width = 0.25) +
  geom_hline(yintercept = 0) + 
  theme_classic()

## Only pixels with two dist
sdm_mod_df <- two_dist_order_df %>% 
  dplyr::select(cell, x, y, cflux_ha, zone, order, LC) %>% 
  filter(order %in% c("NSR_NSR", "SR_SR", "NSR_SR", "SR_NSR")) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(order = factor(order, levels = c("NSR_NSR", "SR_SR", "NSR_SR", "SR_NSR")),
         x = x/1000000,
         y = y/1000000) %>% 
  group_by(order) %>% 
  sample_frac(0.5)

sdm_df_mesh <- make_mesh(sdm_mod_df, c("x", "y"), 
                         cutoff = 0.01)
tictoc::tic()
#### Fit model
dist_order_mod2 <- sdmTMB(
  data = sdm_mod_df, 
  formula = cflux_ha ~ 
    order, 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian())

summary(dist_order_mod2)
tidy(dist_order_mod2, conf.int = T, 
     effects = c("fixed", "ran_pars")) %>% 
  as.data.frame() %>% 
  mutate(term = case_when(term == "(Intercept)" ~ "NSR_NSR",
                          TRUE ~ term),
         cflux = .[1,2]  + estimate,
         cflux = case_when(term == "NSR_NSR" ~ .[1,2],
                           TRUE ~ cflux),
         cflux_low = .[1,2]  + conf.low,
         cflux_low = case_when(term == "NSR_NSR" ~ .[1,2],
                               TRUE ~ cflux_low),
         cflux_high = .[1,2]  + conf.high,
         cflux_high = case_when(term == "NSR_NSR" ~ .[1,2],
                                TRUE ~ cflux_high),
         term = str_replace(term, "order", ""),
         term = factor(term, levels = c("NSR_NSR", "SR_SR", "NSR_SR", "SR_NSR"))) %>% 
  ggplot(aes(x = term, y = cflux)) + 
  geom_col(colour = "black", width = 0.75) + 
  geom_errorbar(aes(ymin = cflux_low, ymax = cflux_high), 
                width = 0.25) +
  geom_hline(yintercept = 0) + 
  theme_classic()


## Dist pattern
unique(c_dist_period_df$period_group)
c_dist_period_df %>% group_by(dist_type, period_group) %>% tally()

sdm_mod_df <- c_dist_period_df %>% 
  dplyr::select(cell, x, y, cflux_ha, dist_type, zone, period_group) %>% 
  filter(dist_type %in% c("SR", "ND")) %>%
  mutate(dist_type = case_when(dist_type == "ND" ~ "SR",
                               TRUE ~ dist_type)) %>% 
  na.omit() %>% 
  droplevels() %>% 
  mutate(dist_type = factor(dist_type, levels = c("ND", "NSR", "SR")),
         period_group = factor(period_group, levels = c("p0", "p1", "p2", "p3")),
         x = x/1000000,
         y = y/1000000) %>% 
  group_by(dist_type) %>% 
  sample_frac(0.25)

sdm_df_mesh <- make_mesh(sdm_mod_df, c("x", "y"), 
                         cutoff = 0.005)
tictoc::tic()
#### Fit model
dist_pattern_mod <- sdmTMB(
  data = sdm_mod_df, 
  formula = cflux_ha ~ 
    period_group, 
  reml = T, silent = F, spatial = "on", spatiotemporal = "off",
  mesh = sdm_df_mesh, family = gaussian())
tictoc::toc()
#### Summarise
summary(dist_pattern_mod)
tidy(dist_pattern_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))
