

# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(ggtext)
library(easystats)
library(mgcv)
library(spaMM)
library(RColorBrewer)

# Load data ---------------------------------------------------------------

cad_df_3161 <- read_csv(here("data/processed/cflux_admin_dist_pixel_250m_df_3161.csv"))
cad_df_3161 <- read_csv(here("data/processed/cflux_admin_dist_pixel_500m_df_3161.csv"))


# Explore -----------------------------------------------------------------

brewer.pal(7, "Dark2")

## How does C flux differ based on disturbance type
c_dist_type_df <- cad_df_3161 %>% 
  filter(dist_01_19 %in% c("ND", "NSR", "SR", "Both", "NSR2", "SR2"))

write_csv(c_dist_type_df, here("data/processed/c_dist_type_df.csv"))

c_dist_type_df <- read_csv(here("data/processed/c_dist_type_df.csv"))

## Only one disturbance in each time period
dist_type_plot_single <- c_dist_type_df %>% 
  filter(dist_01_19 %in% c("ND", "NSR", "SR")) %>% 
  group_by(dist_01_19) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(sum(n_c)) %>% 
  ggplot(aes(x = dist_01_19, y= mean_c, fill = dist_01_19)) + 
  geom_col(width = 0.5, colour = "black") + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15) + 
  geom_hline(yintercept = 0) + 
  scale_x_discrete(name = "",
                   labels = c("No<br>disturbance",
                              "Non-stand-replacing<br>disturbance",
                              "Stand-replacing<br>disturbance")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-75, 0),
                     breaks = seq(-75, 0, 25)) + 
  # colour vision deficiency palette
  scale_fill_manual(values = c("#66A61E", "#E6AB02", "#A6761D"), 
                    labels = c("No disturbance",
                               "Non-stand-replacing disturbance",
                               "Stand-replacing disturbance"),
                    guide = "none") + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.25, 0.8),
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())
dist_type_plot_single

ggsave(here("output/figures/single_dist_type_plot.png"), 
       plot = dist_type_plot_single, bg = "white",
       dpi = 300, dev = "png", height = 10.63041, width = 17.2, unit = "cm")


## Does order matter when both one SR and one NSR occur during the years of 
## record?

two_dist_plot <- cad_df_3161 %>% 
  filter(!is.na(order)) %>% 
  group_by(order) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(order = factor(order, levels = c("NSR_NSR", "SR_SR", 
                                          "NSR_SR", "SR_NSR"))) %>% 
  ggplot(aes(x = order, y = mean_c)) + 
  geom_col(width = 0.5, colour = "black") + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15) + 
  geom_hline(yintercept = 0) + 
  scale_x_discrete(name = "",
                   labels = c("NSR\u2192NSR", "SR\u2192SR",
                              "NSR\u2192SR", "SR\u2192NSR")) +
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-75, 35),
                     breaks = seq(-75, 25, 25),
                     expand = c(0,0)) +
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())
two_dist_plot

ggsave(here("output/figures/two_dist_plot.png"), 
       plot = two_dist_plot, bg = "white",
       dpi = 300, dev = "png", height = 10.63041, width = 17.2, unit = "cm")

## Does the time between two disturbance events impact their C sink ability?

mod_df <- cad_df_3161 %>% 
  filter(!is.na(year_between_dist))

mod_df %>% 
  group_by(order) %>% tally()

mod_df %>% 
  group_by(year_between_dist) %>% tally()

mod_df %>% 
  group_by(year_between_dist, order) %>% tally() %>% view

mod_df %>% 
  group_by(order) %>% 
  summarise(max = max(year_between_dist), min = min(year_between_dist))

mod_df %>% 
  filter(!is.na(year_between_dist)) %>% 
  group_by(year_between_dist, order) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  ggplot(aes(x = year_between_dist, y = mean_c, fill = order)) + 
  geom_col(width = 0.5, colour = "black",
           position = position_dodge(0.75)) + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15, position = position_dodge(0.75)) + 
  geom_hline(yintercept = 0) + 
  facet_wrap(~order) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())


mod_df %>% 
  ggplot(aes(x = year_between_dist, ..scaled.., fill = order)) + 
  geom_density(alpha = 0.4, colour = "black", bw = 0.7) + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_continuous(expand = c(0,0)) + 
  scale_fill_manual(name = "Disturbance order", 
                      values = brewer.pal(4, "Set1"),
                      labels = c("NSR\u2192NSR",
                                 "NSR\u2192SR",
                                 "SR\u2192NSR",
                                 "SR\u2192SR")) + 
  facet_wrap(~order) + 
  theme_classic()

library(mgcv)
library(easystats)
gam1 <- gam(cflux_ha ~ diff*order +  
              s(x, y, bs='gp', k=100, m=2),
            family = gaussian, data=mod_df,
            method = "REML", na.action = "na.fail")


saveRDS(gam1, here("output/models/dist_type_order_diff_gam_3161.rds"))

gam1 <- readRDS(here("output/models/dist_type_order_diff_gam_3161.rds"))
summary(gam1)
anova(gam1)
estimate_means(gam1, "order")
estimate_means(gam1, "diff= c(0, 9, 18)")
estimate_means(gam1, at = c("order", "diff= c(0, 9, 18)"))
estimate_contrasts(gam1, "diff= c(0, 9, 18)")
estimate_contrasts(gam1, "order")
estimate_slopes(gam1, at = "order")
emmeans::emtrends(gam1, pairwise ~ order, var = "diff")
estimate_means(gam1, at = c("order", "diff= c(0, 9, 18)")) %>% 
  as.data.frame() %>% 
  mutate(diff = as.numeric(diff)) %>% 
  ggplot(aes(x = diff, y = Mean, colour = order, group = order)) + 
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), 
                width = 0.5, position = position_dodge(0.5)) +
  geom_line(position = position_dodge(0.5)) + 
  scale_x_continuous(name = "Years between distubances",
                     limits = c(-1, 19), 
                     breaks = seq(0, 18, 2),
                     expand = c(0,0)) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-50, 110),
                     breaks = seq(-50, 110, 20)) + 
  scale_colour_manual(name = "Disturbance order", 
                      values = brewer.pal(4, "Set1"),
                      labels = c("NSR\u2192NSR",
                                 "NSR\u2192SR",
                                 "SR\u2192NSR",
                                 "SR\u2192SR")) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.2, 0.8),
        legend.text = element_markdown(),
        #legend.title.align=0.5,
        legend.background = element_blank(),
        axis.title.y = element_markdown())


## How does the history of disturbance impact C flux

cad_df_3161 %>% 
  filter(!grepl('Both|NA', dist_pattern)) %>% 
  group_by(dist_pattern) %>% tally() %>% view

### Recurring disturbances
#### What effect does a disturbance of the same type occuring once in each 
#### time period have on C flux? (i.e. NSR→ND→ND vs NSR→NSR→ND vs NSR→NSR→NSR)
c_dist_period_df <- cad_df_3161 %>% 
  mutate(dist_period = case_when(dist_pattern == "ND_ND_ND" ~ "ND_p0",
                                 dist_pattern == "NSR_ND_ND" ~ "NSR_p1",
                                 dist_pattern == "NSR_NSR_ND" ~ "NSR_p2",
                                 dist_pattern == "NSR_NSR_NSR" ~ "NSR_p3",
                                 dist_pattern == "SR_ND_ND" ~ "SR_p1",
                                 dist_pattern == "SR_SR_ND" ~ "SR_p2",
                                 dist_pattern == "SR_SR_SR" ~ "SR_p3",
                                 TRUE ~ "remove"),
         dist_type = case_when(dist_pattern == "ND_ND_ND" ~ "ND",
                                 dist_pattern == "NSR_ND_ND" ~ "NSR",
                                 dist_pattern == "NSR_NSR_ND" ~ "NSR",
                                 dist_pattern == "NSR_NSR_NSR" ~ "NSR",
                                 dist_pattern == "SR_ND_ND" ~ "SR",
                                 dist_pattern == "SR_SR_ND" ~ "SR",
                                 dist_pattern == "SR_SR_SR" ~ "SR",
                                 TRUE ~ "remove"),
         period_group = case_when(dist_pattern == "ND_ND_ND" ~ "p0",
                                 dist_pattern == "NSR_ND_ND" ~ "p1",
                                 dist_pattern == "NSR_NSR_ND" ~ "p2",
                                 dist_pattern == "NSR_NSR_NSR" ~ "p3",
                                 dist_pattern == "SR_ND_ND" ~ "p1",
                                 dist_pattern == "SR_SR_ND" ~ "p2",
                                 dist_pattern == "SR_SR_SR" ~ "p3",
                                 TRUE ~ "remove")) %>% 
  filter(dist_period != "remove") %>% 
  filter(!is.na(dist_type))

c_dist_period_df %>% 
  group_by(dist_type, period_group) %>% 
  tally()

c_dist_period_df %>% 
  group_by(dist_type, period_group) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(total_cells = sum(n_c),
         dist_type = factor(dist_type, 
                            levels = c("ND", "NSR", "SR")),
         period_group = factor(period_group, 
                               levels = c("p0", "p1", "p2", "p3")))

dist_pattern_plot <- c_dist_period_df %>% 
  group_by(dist_type, period_group) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  bind_rows(tibble(dist_type = c("NSR", "SR"),
                   period_group = c("p0", "p0"),
                   mean_c = c(-71.0, -71.0),
                   sd_c = c(68.1, 68.1),
                   n_c = c(606200, 606200),
                   se_c = c(0.0875, 0.0875))) %>% 
  mutate(total_cells = sum(n_c),
         dist_type = factor(dist_type, 
                            levels = c("NSR", "SR")),
         period_group = factor(period_group, 
                               levels = c("p0", "p1", "p2", "p3"))) %>% 
  filter(dist_type != "ND") %>% 
  ggplot(aes(x = period_group, y = mean_c, colour = dist_type, group = dist_type)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15) + 
  geom_hline(yintercept = 0) + 
  geom_line(size = 0.75) + 
  geom_point(data = c_dist_period_df %>% 
               group_by(dist_type, period_group) %>% 
               summarise(mean_c = mean(cflux_ha),
                         sd_c = sd(cflux_ha),
                         n_c = n(),
                         se_c = sd_c/sqrt(n_c)) %>% 
               filter(dist_type == "ND"), colour = "#66A61E") + 
  geom_errorbar(data = c_dist_period_df %>% 
                  group_by(dist_type, period_group) %>% 
                  summarise(mean_c = mean(cflux_ha),
                            sd_c = sd(cflux_ha),
                            n_c = n(),
                            se_c = sd_c/sqrt(n_c)) %>% 
                  filter(dist_type == "ND"),
                aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15, colour = "#66A61E") + 
  scale_x_discrete(name = "",
                   labels = c("No<br>Disturbance",
                              "2001—2006",
                              "2007—2012",
                              "2013—2019")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-100, 0),
                     breaks = seq(-100, 0, 25)) + 
  # colour vision deficiency palette
  scale_colour_manual(name = "Disturbance type", 
                      values = c("#E6AB02", "#A6761D", "#66A61E"), 
                      labels = c("Non-stand-replacing disturbance",
                                 "Stand-replacing disturbance")) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.25, 0.8),
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())
dist_pattern_plot

ggsave(here("output/figures/dist_pattern_plot.png"), 
       plot = dist_pattern_plot, bg = "white",
       dpi = 300, dev = "png", height = 10.63041, width = 17.2, unit = "cm")

## Calc dist type means 
## Recurring disturbances in each time period
dist_type_plot_recurr <- c_dist_period_df %>% 
  group_by(dist_type) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(sum(n_c)) %>% 
  ggplot(aes(x = dist_type, y= mean_c, fill = dist_type)) + 
  geom_col(width = 0.5, colour = "black") + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15) + 
  geom_hline(yintercept = 0) + 
  scale_x_discrete(name = "",
                   labels = c("No<br>disturbance",
                              "Non-stand-replacing<br>disturbance",
                              "Stand-replacing<br>disturbance")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-75, 0),
                     breaks = seq(-75, 0, 25)) + 
  # colour vision deficiency palette
  scale_fill_manual(values = c("#66A61E", "#E6AB02", "#A6761D"), 
                    labels = c("No disturbance",
                               "Non-stand-replacing disturbance",
                               "Stand-replacing disturbance"),
                    guide = "none") + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.25, 0.8),
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())
dist_type_plot_recurr

ggsave(here("output/figures/dist_type_plot_recurr.png"), 
       plot = dist_type_plot_recurr, bg = "white",
       dpi = 300, dev = "png", height = 10.63041, width = 17.2, unit = "cm")

### One off disturbances
#### What effect does a disturbance of the same type occurring at different
#### time periods have on C flux? (i.e. NSR→ND→ND vs ND→NSR→ND vs ND→ND→NSR)

c_dist_period_df_single <- cad_df_3161 %>% 
  mutate(dist_period = case_when(dist_pattern == "ND_ND_ND" ~ "ND_p0",
                                 dist_pattern == "NSR_ND_ND" ~ "NSR_p1",
                                 dist_pattern == "ND_NSR_ND" ~ "NSR_p2",
                                 dist_pattern == "ND_ND_NSR" ~ "NSR_p3",
                                 dist_pattern == "SR_ND_ND" ~ "SR_p1",
                                 dist_pattern == "ND_SR_ND" ~ "SR_p2",
                                 dist_pattern == "ND_ND_SR" ~ "SR_p3",
                                 TRUE ~ "remove"),
         dist_type = case_when(dist_pattern == "ND_ND_ND" ~ "ND",
                                 dist_pattern == "NSR_ND_ND" ~ "NSR",
                                 dist_pattern == "ND_NSR_ND" ~ "NSR",
                                 dist_pattern == "ND_ND_NSR" ~ "NSR",
                                 dist_pattern == "SR_ND_ND" ~ "SR",
                                 dist_pattern == "ND_SR_ND" ~ "SR",
                                 dist_pattern == "ND_ND_SR" ~ "SR",
                                 TRUE ~ "remove"),
         period_group = case_when(dist_pattern == "ND_ND_ND" ~ "p0",
                                 dist_pattern == "NSR_ND_ND" ~ "p1",
                                 dist_pattern == "ND_NSR_ND" ~ "p2",
                                 dist_pattern == "ND_ND_NSR" ~ "p3",
                                 dist_pattern == "SR_ND_ND" ~ "p1",
                                 dist_pattern == "ND_SR_ND" ~ "p2",
                                 dist_pattern == "ND_ND_SR" ~ "p3",
                                 TRUE ~ "remove")) %>% 
  filter(dist_period != "remove") %>% 
  filter(!is.na(dist_type))

c_dist_period_df_single %>% 
  group_by(dist_type, period_group) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(total_cells = sum(n_c),
         dist_type = factor(dist_type, 
                            levels = c("ND", "NSR", "SR")),
         period_group = factor(period_group, 
                               levels = c("p0", "p1", "p2", "p3")))

single_dist_timing_plot <- c_dist_period_df_single %>% 
  group_by(dist_type, period_group) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  bind_rows(tibble(dist_type = c("NSR", "SR"),
                   period_group = c("p0", "p0"),
                   mean_c = c(-71.0, -71.0),
                   sd_c = c(68.1, 68.1),
                   n_c = c(606200, 606200),
                   se_c = c(0.0875, 0.0875))) %>% 
  mutate(total_cells = sum(n_c),
         dist_type = factor(dist_type, 
                            levels = c("NSR", "SR")),
         period_group = factor(period_group, 
                               levels = c("p0", "p1", "p2", "p3"))) %>% 
  filter(dist_type != "ND") %>% 
  ggplot(aes(x = period_group, y = mean_c, colour = dist_type, group = dist_type)) + 
  geom_point() +
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15) + 
  geom_hline(yintercept = 0) + 
  geom_line(size = 0.75) + 
  geom_point(data = c_dist_period_df_single %>% 
               group_by(dist_type, period_group) %>% 
               summarise(mean_c = mean(cflux_ha),
                         sd_c = sd(cflux_ha),
                         n_c = n(),
                         se_c = sd_c/sqrt(n_c)) %>% 
               filter(dist_type == "ND"), colour = "#66A61E") + 
  geom_errorbar(data = c_dist_period_df_single %>% 
                  group_by(dist_type, period_group) %>% 
                  summarise(mean_c = mean(cflux_ha),
                            sd_c = sd(cflux_ha),
                            n_c = n(),
                            se_c = sd_c/sqrt(n_c)) %>% 
                  filter(dist_type == "ND"),
                aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.15, colour = "#66A61E") + 
  scale_x_discrete(name = "",
                   labels = c("No<br>Disturbance",
                              "2001—2006",
                              "2007—2012",
                              "2013—2019")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-100, 50),
                     breaks = seq(-100, 50, 50)) + 
  # colour vision deficiency palette
  scale_colour_manual(name = "Disturbance type", 
                      values = c("#E6AB02", "#A6761D", "#66A61E"), 
                      labels = c("Non-stand-replacing disturbance",
                                 "Stand-replacing disturbance")) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.25, 0.8),
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())
single_dist_timing_plot

ggsave(here("output/figures/single_dist_timing_plot.png"), 
       plot = single_dist_timing_plot, bg = "white",
       dpi = 300, dev = "png", height = 10.63041, width = 17.2, unit = "cm")


## Does land administration effect the pattern/occurrence of disturbances
unique(two_dist_order_df$order)
unique(two_dist_order_df$zone)

cad_df_3161 %>% 
  group_by(zone) %>% tally()

## Do the number of disturbance that occur in a pixel depend on management?
cad_df_3161 %>% 
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
  group_by(zone_type, dist_type) %>% 
  tally() %>% 
  mutate(freq = n/sum(n),
         zone_type = factor(zone_type, levels = c("Unprotected", 
                                                  "Semi-protected",
                                                  "Protected"))) %>% 
  ggplot(aes(x = zone_type, y = freq, fill = dist_type)) + 
  geom_col(colour = "black", 
           position = position_dodge(0.9)) + 
  scale_y_continuous(name = "Frequency",
                     limits = c(0, 0.6),
                     breaks = seq(0, 0.6, 0.1),
                     expand = c(0,0)) +
  scale_fill_manual(name = "No. of disturbances",
                    values = rev(brewer.pal(3, "Set1")),
                    labels = c("None", "One", "Two")) + 
  theme_classic()

cad_df_3161 %>% 
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
  group_by(zone_type, dist_type) %>% 
  tally() %>% 
  mutate(freq = n/sum(n))


prop_df <- cad_df_3161 %>% 
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
                               TRUE ~ "Protected"))




boot_prop <- function(df) {
  sub <- df %>% 
    group_by(zone_type) %>% 
    sample_n(1000)
  
  out <- sub %>% 
    group_by(zone_type, dist_type) %>% 
    tally() %>% 
    mutate(freq = n/sum(n))
  
  return(out)
}

boot_freq <- 1000 %>% 
  rerun(boot_prop(prop_df)) %>% 
  bind_rows(.id = "iter")

boot_freq %>% 
  group_by(zone_type, dist_type) %>% 
  summarise(freq = quantile(freq, c(0.025, 0.5, 0.975)), 
            q = c(0.025, 0.5, 0.975)) %>% 
  pivot_wider(names_from = q, values_from = freq, names_prefix = "q_") %>% 
  ggplot(aes(x = zone_type, y = q_0.5, fill = dist_type)) + 
  geom_col(position = position_dodge(0.9), colour = "black") + 
  geom_errorbar(aes(ymin = q_0.025, ymax = q_0.975), 
                position = position_dodge(0.9), width =0.25) + 
  scale_y_continuous(name = "Median proportion",
                     limits = c(0, 0.6),
                     breaks = seq(0,0.6, 0.2),
                     expand = c(0,0)) + 
  xlab("") + 
  theme_classic() + 
  theme(axis.text.x = element_markdown(),
        legend.position = "top")


## Does land cover type control the type and pattern of distubance?

unique(two_dist_order_df$LC)

two_dist_order_df %>% 
  filter(!is.na(order)) %>% 
  mutate(dist_type = case_when(order == "NSR_SR" | 
                                 order == "SR_NSR" ~ "both",
                               order == "NSR_NSR" |
                                 order == "SR_SR" ~ "same",
                               order == "ND" ~ "none",
                               order == "NSR" | order == "SR" ~ "one")) %>% 
  group_by(LC, ON_Land_Cover, order) %>% 
  tally() %>% 
  mutate(freq = n/sum(n)) %>% 
  filter(LC %in% c("Deciduous treed", "Mixed treed","Coniferous treed")) %>% 
  ggplot(aes(x = LC, y = freq, fill = LC)) + 
  geom_col(colour = "black",
           position = position_dodge()) + 
  facet_wrap(~order, scales = "free_y")

















## Compound dist pattern
test <- cad_df %>% 
  left_join(., NSR_SR_df)

library(RColorBrewer)
brewer.pal(5, "Set1")

test %>% 
  filter(!str_detect(dist_pattern, "Both|NA")) %>% 
  group_by(dist_pattern) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(sum(n_c))


first_second_dist_plot <- test %>% 
  filter(!str_detect(dist_pattern, "Both|NA")) %>% 
  group_by(dist_pattern) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  separate(dist_pattern, into = c("first_dist", "second_dist"), "_") %>% 
  # mutate(dist_pattern = factor(dist_pattern, levels = c("ND_ND", "ND_NSR", "ND_SR",
  #                                                       "NSR_ND", "NSR_NSR", "NSR_SR",
  #                                                       "SR_ND", "SR_NSR", "SR_SR"))) %>% 
  ggplot(aes(x = first_dist, y = mean_c, colour = second_dist)) + 
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                width = 0.25, position = position_dodge(0.5)) + 
  geom_hline(yintercept = 0) + 
  scale_x_discrete(name = "2001 disturbance", 
                   labels = c("No<br>disturbance",
                              "Non-stand-replacing<br>disturbance",
                              "Stand-replacing<br>disturbance")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-100, 100),
                     breaks = seq(-100, 100, 50)) + 
  scale_colour_manual(name = "Subsequent disturbance (2002-2019)", 
                      values = c("#4DAF4A", "#FF7F00", "#E41A1C"), 
                      labels = c("No disturbance",
                                 "Non-stand-replacing disturbance",
                                 "Stand-replacing disturbance")) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.25, 0.8),
        #legend.title.align=0.5,
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())



ggsave(here("output/figures/first_second_dist_plot.png"), 
       plot = first_second_dist_plot, bg = "white",
       dpi = 300, dev = "png", height = 10.63041, width = 17.2, unit = "cm")

library(easystats)

mod_df <- test %>% 
  filter(!str_detect(dist_pattern, "Both|NA")) %>% 
  separate(dist_pattern, into = c("first_dist", "second_dist"), "_")

lm1 <- lm(cflux_ha ~ first_dist*second_dist + y, data = mod_df)
summary(lm1)
estimate_means(lm1) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = first_dist, y = Mean, colour = second_dist)) + 
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                width = 0.25, position = position_dodge(0.5)) + 
  geom_hline(yintercept = 0) + 
  scale_x_discrete(name = "2001 disturbance", 
                   labels = c("No<br>disturbance",
                              "Non-stand-replacing<br>disturbance",
                              "Stand-replacing<br>disturbance")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-150, 150),
                     breaks = seq(-150, 150, 50)) + 
  scale_colour_manual(name = "Subsequent disturbance (2002-2019)", 
                      values = c("#4DAF4A", "#FF7F00", "#E41A1C"), 
                      labels = c("No disturbance",
                                 "Non-stand-replacing disturbance",
                                 "Stand-replacing disturbance")) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.25, 0.8),
        #legend.title.align=0.5,
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())

library(mgcv)
mod_df %>% 
  group_by(first_dist, second_dist) %>% tally() %>% ungroup() %>% 
  gt()


set.seed(NULL)
sub_df <- mod_df %>% 
  group_by(first_dist, second_dist) %>% 
  sample_n(200, replace = T)


## First a GAM model is fit with a smoothig term for longitude (x) and latitude (y)
## The arguments in the s function have not been tuned. 
gam1 <- gam(cflux_ha ~ first_dist*second_dist +  
              s(x, y, bs='gp', k=100, m=2),
            family = gaussian, data=sub_df,
            method = "REML", na.action = "na.fail")

summary(gam1)
anova(gam1)
estimate_contrasts(gam1, contrast = c("first_dist", "second_dist"))
estimate_means(gam1) %>% 
  as.data.frame() %>% 
  ggplot(aes(x = first_dist, y = Mean, colour = second_dist)) + 
  geom_point(position = position_dodge(0.5)) + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                width = 0.25, position = position_dodge(0.5)) + 
  geom_hline(yintercept = 0) + 
  scale_x_discrete(name = "2001 disturbance", 
                   labels = c("No<br>disturbance",
                              "Non-stand-replacing<br>disturbance",
                              "Stand-replacing<br>disturbance")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-125, 125),
                     breaks = seq(-100, 100, 50)) + 
  scale_colour_manual(name = "Subsequent disturbance<br>(2002-2019)", 
                      values = c("#4DAF4A", "#FF7F00", "#E41A1C"), 
                      labels = c("No disturbance",
                                 "Non-stand-replacing<br>disturbance",
                                 "Stand-replacing<br>disturbance")) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = "top",
        legend.text = element_markdown(),
        #legend.title.align=0.5,
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())

ggsave(here("output/figures/first_second_dist_plot_gam.png"), 
       plot = first_second_dist_plot_gam, bg = "white",
       dpi = 300, dev = "png", height = 10.63041, width = 17.2, unit = "cm")



## Do more recent disturbance have greater effects on C flux than older 
## disturbances. !!Only pixels with zero (2000) or one dist area included. 
cad_df %>% 
  filter(total_dist <= 1) %>% 
  group_by(recent_dist_year, zone) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indian Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park",
                          zone == "Protected Area - Far North" ~ 
                            "Protected Area<br>Far North"
  ),
  zone = factor(zone, levels = c("General<br>Use Area", 
                                 "Enhanced<br>Management Area",
                                 "Indigenous<br>Reserve",
                                 "Protected Area<br>Far North",
                                 "Provincial<br>Park",
                                 "Conservation<br>Reserve"
  ))) %>% 
  ggplot(aes(x = factor(recent_dist_year), y = mean_c, fill = zone)) + 
  geom_col(position = position_dodge(0.95), 
           colour = "black") + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                position = position_dodge(0.95),
                width = 0.5) + 
  geom_hline(yintercept = 0) + 
  scale_x_discrete(breaks = c("2000", "2005", "2010", "2015", "2019")) + 
  facet_wrap(~ zone) + 
  theme_classic() + 
  theme(legend.position = "none", 
        legend.text =  element_markdown(),
        axis.text.x = element_markdown())
## There appears to be no clear pattern of time of disturbance, admin zone, 
## and cflux

## Does the period in which the most recent disturbance occur control the C flux 
## within land admin zones
cad_df %>% 
  group_by(zone, period) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indian Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park",
                          zone == "Protected Area - Far North" ~ 
                            "Protected Area<br>Far North"
  ),
  zone = factor(zone, levels = c("General<br>Use Area", 
                                 "Enhanced<br>Management Area",
                                 "Indigenous<br>Reserve",
                                 "Protected Area<br>Far North",
                                 "Provincial<br>Park",
                                 "Conservation<br>Reserve"
  ))) %>% 
  ggplot(aes(x = zone, y = mean_c, fill = period)) + 
  geom_col(position = position_dodge(0.95), 
           colour = "black") + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                position = position_dodge(0.95),
                width = 0.5) + 
  theme_classic() + 
  theme(legend.position = "top", 
        legend.text =  element_markdown(),
        axis.text.x = element_markdown())
## There are obvious differences between bars but no clear pattern. This is likely
## due to spatial effects no being considered and disturbance type playing a bit 
## role. I.e. it matters if an NSR or SR dist happened most recently. Overall, this 
## analysis is not very intuitive as is.

## Does the occurrence of different disturbance type (nsr vs sr) effect the C 
## flux potential of different administration zones?
cad_df %>% 
  group_by(zone, dist) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indian Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park",
                          zone == "Protected Area - Far North" ~ 
                            "Protected Area<br>Far North"
  ),
  zone = factor(zone, levels = c("General<br>Use Area", 
                                 "Enhanced<br>Management Area",
                                 "Indigenous<br>Reserve",
                                 "Protected Area<br>Far North",
                                 "Provincial<br>Park",
                                 "Conservation<br>Reserve"
  ))) %>% 
  ggplot(aes(x = zone, y = mean_c, fill = dist)) + 
  geom_col(position = position_dodge(0.95), 
           colour = "black") + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c),
                position = position_dodge(0.95),
                width = 0.5) + 
  theme_classic() + 
  theme(legend.position = "top", 
        legend.text =  element_markdown(),
        axis.text.x = element_markdown())
## SR and Both NSR & SR seem to have promote the release of carbon. No 
## disturbance typically has the highest sink potential. However, there is a 
## strong spatial component to C flux that is not accounted for by simply plotting
## the data. 


## Do land admin zone exhibit different C fluxes?
cad_df %>% 
  group_by(zone) %>% 
  summarise(mean_c = mean(cflux_ha),
            sd_c = sd(cflux_ha),
            n_c = n(),
            se_c = sd_c/sqrt(n_c)) %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indian Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park",
                          zone == "Protected Area - Far North" ~ 
                            "Protected Area<br>Far North"),
         zone = factor(zone, levels = c("General<br>Use Area", 
                                        "Enhanced<br>Management Area",
                                        "Indigenous<br>Reserve",
                                        "Protected Area<br>Far North",
                                        "Provincial<br>Park",
                                        "Conservation<br>Reserve"))) %>% 
  ggplot(aes(x = zone, y = mean_c, fill = zone)) + 
  geom_col(colour = "black") + 
  geom_errorbar(aes(ymin = mean_c - se_c, ymax = mean_c + se_c)) + 
  geom_hline(yintercept = 0) + 
  theme_classic() + 
  theme(legend.position = "none", 
        axis.text.x = ggtext::element_markdown())
## Yes, General land use and Conservation reserves are the weakest carbon sinks. 
## Aside from Conservation Reserves the, there is an expected pattern of increasing 
## C sink with management. 

## Modelling the interaction between zone and disturbance type. Indigenous reserves
## are removed becasue they have too few pixels with just SR disturbances (only 2).
lm1 <- lm(cflux_ha ~ 
            zone*dist + 
            y, 
          data = cad_df %>% 
            filter(zone != "Indian Reserve"))
summary(lm1)

estimate_means(lm1) %>% 
  as.data.frame() %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indian Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park",
                          zone == "Protected Area - Far North" ~ 
                            "Protected Area<br>Far North"),
         zone = factor(zone, levels = c("General<br>Use Area", 
                                        "Enhanced<br>Management Area",
                                        "Indigenous<br>Reserve",
                                        "Protected Area<br>Far North",
                                        "Provincial<br>Park",
                                        "Conservation<br>Reserve")),
         dist = factor(dist, levels = c("ND", "NSR", "SR", "Both"))) %>% 
  ggplot(aes(x = zone, y = Mean, fill = dist)) + 
  geom_col(position = position_dodge(0.9), 
           colour = "black") + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                position = position_dodge(0.9), width = 0.5) + 
  geom_hline(yintercept = 0) + 
  theme_classic() + 
  theme(legend.position = "top", 
        axis.text.x = ggtext::element_markdown())


estimate_means(lm1, at = "zone")
estimate_means(lm1, at = "dist")
estimate_contrasts(lm1, contrast = "zone")
estimate_contrasts(lm1, contrast = "dist")
## Independent of dist type, conservation reserves and general use area are the 
## weakest carbon sinks. Provincial parks are the strongest. For dist type, there 
## is a clear trend of decreasing C sink with more severe disturbances. Critically, 
## NSR dist occurring with SR seems to increase carbon release. Overall, only 
## Provincial Parks are still carbon sinks regardless of disturbances. Still, 
## this model does not accoutn for the spatial relationship of pixels. 


## Fitting spatial models
## Since these models are more complex they cannot be quickly fit on the full 
## dataset. So, a subset of each group is taken to explore possible fits. Once 
## again, Indigenous reserves are removed since they have insufficient samples. 
cad_df %>% 
  filter(zone != "Indian Reserve") %>% 
  group_by(dist, zone) %>% tally()


set.seed(912)
sub_df <- cad_df %>% 
  filter(zone != "Indian Reserve") %>% 
  group_by(dist, zone) %>% 
  sample_n(200, replace = F)


## First a GAM model is fit with a smoothig term for longitude (x) and latitude (y)
## The arguments in the s function have not been tuned. 
gam1 <- gam(cflux_ha ~ 
              s(x, y, bs='gp', k=100, m=2) +
              zone*dist,
            family = gaussian, data=sub_df,
            method = "REML", na.action = "na.fail")

summary(gam1)
anova(gam1)

estimate_means(gam1) %>% 
  as.data.frame() %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indian Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park",
                          zone == "Protected Area - Far North" ~ 
                            "Protected Area<br>Far North"),
         zone = factor(zone, levels = c("General<br>Use Area", 
                                        "Enhanced<br>Management Area",
                                        "Indigenous<br>Reserve",
                                        "Protected Area<br>Far North",
                                        "Provincial<br>Park",
                                        "Conservation<br>Reserve")),
         dist = factor(dist, levels = c("ND", "NSR", "SR", "Both"))) %>% 
  ggplot(aes(x = zone, y = Mean, fill = dist)) + 
  geom_col(position = position_dodge(0.9), 
           colour = "black") + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                position = position_dodge(0.9), 
                width = 0.5) + 
  geom_hline(yintercept = 0) + 
  theme_classic() + 
  theme(legend.position = "top", 
        axis.text.x = element_markdown())

estimate_means(gam1, at = "zone")
estimate_means(gam1, at = "dist")
estimate_contrasts(gam1, contrast = c("zone", "dist"))
estimate_contrasts(gam1, contrast = "zone")
estimate_contrasts(gam1, contrast = "dist")
## !!Any model interpretation is based on a small sub sample, bootstrapping 
## would be needed to make reliable inferences!! 
## Adding a spatial term to the model changes the results drastically. SR and 
## Both now are consistently carbon sources, as expected from destructive 
## disturbances. General use areas under no dist or NSR are porr carbon sinks. 
## Interestingly, having only NSR disturbances appears to increase C sink ability, 
## potentially due to regrowth stimulation. 


## GAM models are not specifically designed to deal with spatial data. Therefore, 
## spaMM models are used to see if the GAM results hold. spaMM is more complex so 
## an even smaller subsample is needed. 
set.seed(912)
sub_df_spamm <- cad_df %>% 
  filter(zone != "Indian Reserve") %>% 
  group_by(dist, zone) %>% 
  sample_n(100, replace = F)


spatial_mod <-
  fitme(
    cflux_ha ~ zone*dist +
      Matern(1 |x + y),
    data = sub_df_spamm,
    family = "gaussian", method = "REML"
  ) 

summary(spatial_mod)


dd <- dist(sub_df[,c("x","y")])
mm <- MaternCorr(dd, nu = 0.08954143, rho = 0.41285581)
plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]", ylab = "Estimated correlation")

newdat <- expand_grid(zone = cad_df %>% pull(zone) %>% unique(), 
                      dist =  cad_df %>% pull(dist) %>% unique(), 
                      x = cad_df %>% pull(x) %>% mean,
                      y = cad_df %>% pull(y) %>% mean) %>% 
  filter(zone != "Indian Reserve")

newdat$pred <- as.vector(predict(spatial_mod, newdat))

newdat %>% 
  mutate(zone = case_when(zone == "General Use Area" ~ 
                            "General<br>Use Area",
                          zone == "Conservation Reserve" ~ 
                            "Conservation<br>Reserve",
                          zone == "Enhanced Management Area" ~ 
                            "Enhanced<br>Management Area",
                          zone == "Indian Reserve" ~ 
                            "Indigenous<br>Reserve",
                          zone == "Provincial Park" ~ 
                            "Provincial<br>Park",
                          zone == "Protected Area - Far North" ~ 
                            "Protected Area<br>Far North"),
         zone = factor(zone, levels = c("General<br>Use Area", 
                                        "Enhanced<br>Management Area",
                                        "Indigenous<br>Reserve",
                                        "Protected Area<br>Far North",
                                        "Provincial<br>Park",
                                        "Conservation<br>Reserve")),
         dist = factor(dist, levels = c("ND", "NSR", "SR", "Both"))) %>% 
  ggplot(aes(x = zone, y = pred, fill = dist)) + 
  geom_col(position = position_dodge(0.9), 
           colour = "black") + 
  # geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
  #               position = position_dodge(0.9), 
  #               width = 0.5) + 
  geom_hline(yintercept = 0) + 
  theme_classic() + 
  theme(legend.position = "top", 
        axis.text.x = ggtext::element_markdown())

## The spaMM model has a similar result to the GAM model but should be taken 
## with a grain of salt due to the small subset. 

cad_df %>% 
  filter( zone != "Indian Reserve") %>%
  group_by(zone) %>%
  tally() %>%
  mutate(freq = n/sum(n))

cad_df %>%
  group_by(total_dist) %>%
  tally()
  
