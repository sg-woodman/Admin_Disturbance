# Title: Fit Spatial Regression
# Author details: Author: Sam Woodman 
# Contact details: samuel.woodman@gmail.com
# Description: Fit spatial autoregressive model to data

## Since the data is inheritly spatial, any modelling needs to account for 
## spatial autocorreltion. Here, a spatial regression model is fit that includes
## the lat/lon coordinates of each pixel. This should remove the effects of 
## spatial aoutcorrelation and imporve the interretation of the the outputs. 


# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(spaMM)
library(glmmTMB)
library(emmeans)

# Load data ---------------------------------------------------------------

cad_df <- read_csv(here("data/processed/cflux_admin_dist_df.csv"))


# Constants ---------------------------------------------------------------

zone_to_remove <- c("National Park", "Wilderness Area", 
                    "National Marine Conservation Area", 
                    "National Wildlife Area", "Forest Reserve")

# Prep distance matrix ----------------------------------------------------

cad_df %>% 
  filter(!zone %in% zone_to_remove) %>% 
  mutate(zone = str_replace(zone, "Recommended ", "")) %>% 
  group_by(zone) %>% 
  tally()

##########
df_cells <- cad_df %>% 
  filter(!zone %in% zone_to_remove) %>% 
  mutate(zone = str_replace(zone, "Recommended ", ""))

df_cells %>% 
  ggplot(aes(x = count, y = cflux)) + 
  geom_point() + 
  facet_wrap(~zone)

mod <- lm(cflux ~ zone*scale(count) + scale(lat) + scale(lon), data = df_cells)
summary(mod)
anova(mod)
hist(resid(mod))
pairs(emmeans::emtrends(mod, "zone", "count"))
emmeans::emmip(mod, zone ~ count, at = list(count = c(0, 10, 20)))

pairs(emmeans::emmeans(mod, "zone"))
emmeans::emmeans(mod, "zone") %>% 
  as.data.frame() %>% 
  ggplot(aes(x = emmean, y = zone)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL)) + 
  theme_bw()


df_cells$resid <- resid(mod)

xy_resid <- df_cells %>% dplyr::select(lon, lat, resid)

resid_raster <- terra::rast(rasterFromXYZ(xy_resid))
plot(resid_raster)
terra::autocor(resid_raster)


#########
mod_df <- cad_df %>% 
  filter(!zone %in% zone_to_remove) %>% 
  mutate(zone = str_replace(zone, "Recommended ", ""), 
         zone = as.factor(zone)) %>% 
  group_by(zone) %>% 
  sample_n(50, replace = T)


# SpaMM
mod_spamm <-
  fitme(
    cflux ~  zone*count + 
      Matern(1 |lon + lat),
    data = mod_df,
    family = "gaussian", method = "REML"
  ) 
summary(mod_spamm)
output <- stripHLfit(mod_spamm)
print("save")
saveRDS(output, "/Users/sam/Documents/Admin_Disturbance/tmp/spamm_mod_250m_3000pnts.rds")
bigmod <- biglm(log(conc) ~ source + factor(percent), data = pigs)

rg2 <- qdrg(object = mod_spamm_admin, data = mod_df)
pairs(emmeans::emtrends(rg2, "zone", "count"))
summary(emmeans(rg2, "source"), type = "response")




