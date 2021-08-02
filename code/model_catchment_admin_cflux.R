library(tidyverse)
library(here)
library(spaMM)
library(emmeans)

dist_df <- vroom::vroom("/Volumes/SSD/Backup/ForestDist/data/Dist_df.csv") %>% 
  filter(year > 2000) %>% 
  group_by(catchment, year, catch_land_area, catch_lat, catch_lon) %>%
  summarise(total_dist_area = sum(dist_area)) %>% 
  mutate(total_dist_prop = total_dist_area/catch_land_area, 
         total_dist_prop = if_else(total_dist_prop > 1, 1, total_dist_prop)) %>% 
  group_by(catchment, catch_land_area, catch_lat, catch_lon) %>% 
  summarise(mean_total_dist_prop = mean(total_dist_prop))

hist(sqrt(dist_df$mean_total_dist_prop))

cflux_catch <- read_csv("/Volumes/SSD/Backup/ForestDist/data/c_flux_df.csv") %>% 
  mutate(cflux_per_ha = sum_c_flux/(n_pixels*0.09))

cflux_catch_df <- cflux_catch %>% 
  filter(n_eco == 1) %>%
  dplyr::select(catchment, catch_lat, catch_lon, cflux_per_ha,
                starts_with("prop"), 49:59) %>% 
  pivot_longer(cols = c(15:25), names_to = "zone", values_to = "prop_zone") %>% 
  dplyr::select(-prop_wetland, -prop_community_infrastructure) %>% 
  filter(prop_zone == 1) %>% 
  filter(zone != "forest_reserve") %>% 
  mutate(zone = str_replace(zone, "recommended_", "")) %>% 
  pivot_longer(cols = starts_with("prop"), 
               names_to = "dist", values_to = "prop_dist") %>% 
  group_by(catchment, catch_lat, catch_lon, zone) %>% 
  summarise(total_prop_dist = sum(prop_dist),
            cflux_per_ha = mean(cflux_per_ha)) %>% 
  left_join(., dist_df) %>% 
  filter(catchment != "ID_32254.61515") %>% 
  na.omit()

cflux_catch_df %>% 
  group_by(zone) %>% 
  tally()

cflux_catch_df %>% 
  ggplot(aes(x = mean_total_dist_prop , y = cflux_per_ha)) + 
  geom_point() + 
  facet_wrap(~zone)

catch_flux_mod <- lm(cflux_per_ha ~ as.factor(zone)*scale(sqrt(mean_total_dist_prop)) + 
                       scale(catch_lat) + scale(catch_lon), data = cflux_catch_df)
summary(catch_flux_mod)
anova(catch_flux_mod)

pairs(emmeans::emtrends(catch_flux_mod, "zone", "mean_total_dist_prop", type = "respones"))
emmeans::emmip(catch_flux_mod, zone ~ mean_total_dist_prop, at = list(mean_total_dist_prop = c(0, 0.5, 1)))

pairs(emmeans::emmeans(catch_flux_mod, "zone"))
emmeans::emmeans(catch_flux_mod, "zone") %>% 
  as.data.frame() %>% 
  ggplot(aes(x = emmean, y = zone)) + 
  geom_point() + 
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL)) + 
  theme_bw()

sub_catch <- cflux_catch_df %>% pull(catchment) %>% unique(.)

catchments <- sf::st_read("/Volumes/SSD/Backup/ForestDist/data/Spatial/AoU_Catchments_Full_Area_valid.gpkg") %>% 
  mutate(catchment = paste0("ID_", OBJECTID)) %>% 
  filter(catchment %in% sub_catch)

c1 <- cflux_catch_df %>% pull(catchment) %>% unique()
c2 <- catchments %>% pull(catchment) %>% unique()

setdiff(c1, c2)


catch_nb <- spdep::poly2nb(catchments, row.names("catchment"))
catch_listw <- spdep::nb2listw(catch_nb, style="W", zero.policy = T)
out <- spdep::moran.test(resid(catch_flux_mod), catch_listw, zero.policy = T)
out


set.seed(123)
sam_cflux_catch_df <- cflux_catch_df %>% 
  na.omit() %>% 
  mutate(zone = as.factor(zone)) %>% 
  group_by(zone) %>% 
  sample_frac(0.1, replace = T) 

### spamm
mod_spamm_catch <-
  fitme(
    cflux_per_ha ~ zone*sqrt(mean_total_dist_prop) + 
      Matern(1 |catch_lon + catch_lat),
    data = sam_cflux_catch_df,
    family = "gaussian", method = "REML"
  )
summary(mod_spamm_catch)
anova(aov(mod_spamm_catch))
summary(glht(mod_spamm_catch,mcp("zone" = "Tukey"), coef.=fixef.HLfit))
emmeans::emtrends(mod_spamm, "zone", var = "count")

resid(mod_spamm_catch)


sub_spamm_catch <- mod_spamm_catch$data$catchment

catchments <- sf::st_read("/Volumes/SSD/Backup/ForestDist/data/Spatial/AoU_Catchments_Full_Area_valid.gpkg") %>% 
  mutate(catchment = paste0("ID_", OBJECTID)) %>% 
  filter(catchment %in% sub_spamm_catch)

catch_nb <- spdep::poly2nb(catchments, row.names("catchment"))
catch_listw <- spdep::nb2listw(catch_nb, style="W", zero.policy = T)
out_spamm <- spdep::moran.test(resid(mod_spamm_catch), catch_listw, zero.policy = T)
out

newdat <- expand_grid(zone = cflux_catch_df %>% pull(zone) %>% unique(), 
            total_prop_dist = c(0,0.5,1), 
            catch_lon = cflux_catch_df %>% pull(catch_lon) %>% mean,
            catch_lat = cflux_catch_df %>% pull(catch_lat) %>% mean)

newdat$pred <- as.vector(predict(mod_spamm, newdat))


newdat %>% 
  ggplot(aes(x = total_prop_dist, y = pred, colour = zone)) + 
  geom_point() + 
  geom_line()

