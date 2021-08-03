# Title: Model C-flux 
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Model C-flux by zone and disturbance history

## The cumulative per ha flux of C from forested areas of land management 
## zones within the AoU is modeled. The aim is to test whether C-flux differs
## between land management type and if the history of disturbance (no. 
## disturbances or mean proportional area disturbed) interact to effect C-fluxes


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(spaMM)

# Load data ---------------------------------------------------------------

cad_poly_df <- read_csv(here("data/processed/admin_cflux_dist_poly_ha_df.csv"))

cad_poly_df$zone <- relevel(as.factor(cad_poly_df$zone), "General Use Area")


# Fit model ---------------------------------------------------------------

mod_spa_admin <-
  fitme(
    cflux_mean ~ zone*sqrt(mean_prop_dist) +
      Matern(1 |lon + lat),
    data = cad_poly_df,
    family = "gaussian", method = "REML"
  ) 

# Examine output ----------------------------------------------------------

# slow, need to optimize
confint.HLfit(mod_spa_admin, "zoneConservation Reserve:sqrt(mean_prop_dist)")

summary(mod_spa_admin)
anova(aov(mod_spa_admin))

hist(resid(mod_spa_admin))


# Plot predicted ----------------------------------------------------------

newdat <- expand_grid(zone = admin_cflux_dist_ha_df %>% pull(zone) %>% unique(), 
                      mean_prop_dist = sqrt(seq(0,0.5,0.001)), 
                      lon = admin_cflux_dist_ha_df %>% pull(lon) %>% mean,
                      lat = admin_cflux_dist_ha_df %>% pull(lat) %>% mean)

newdat$pred <- as.vector(predict(mod_spa_admin_ha, newdat))
newdat$mean_prop_dist <- (newdat$mean_prop_dist)^2

newdat %>% 
  ggplot(aes(x = (mean_prop_dist)^2, y = pred, colour = zone)) + 
  geom_line(size = 1.5) + 
  ggsci::scale_color_jco() +
  xlab("Mean proportion disturbed from 2001 to 2019") + 
  ylab("Megagrams CO2 emissions/ha") + 
  labs(colour = "Land\nManagement") + 
  theme_bw() + 
  theme(text = element_text(size=14))

cad_poly_df %>% 
  ggplot(aes(x = mean_prop_dist, y = cflux_per_ha_poly)) + 
  geom_point() + 
  facet_wrap(~zone)
