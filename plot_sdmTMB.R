
# Packages ----------------------------------------------------------------

library(tidyverse)
library(here)
library(sdmTMB)
library(easystats)
library(emmeans)
library(terra)
library(ggtext)
library(patchwork)


# Figures -----------------------------------------------------------------


## Fig 1a ----
fig1a_mod <- readRDS(here("output/models/fig1a_sdmTMB.rds"))


scale(fig1a_mod$data$mean_evi_3161)
mean(fig1a_mod$data$mean_evi_3161)
mean(fig1a_mod$data$x)
mean(fig1a_mod$data$y)

fig1a_mod$data %>% 
  mutate(diff_x = x - 1.027767,
         diff_y = y - 12.42279,
         diff = diff_x + diff_y) %>% 
  group_by(catch) %>% 
  summarise(diff = abs(mean(diff))) %>% 
  arrange(diff) %>% head(10) %>% view
  filter(between(x, 1.025, 1.029)) %>% 
  filter(between(y, 12.40, 12.44))

fig1a_newdat <- expand_grid(catch = as.factor(79790),
                            dist_01_19 = unique(fig1a_mod$data$dist_01_19), 
                            mean_evi_3161 = mean(scale(fig1a_mod$data$mean_evi_3161)),
                            x = mean(fig1a_mod$data$x),
                            y = mean(fig1a_mod$data$y))


test3 <- predict(fig1a_mod, newdata = fig1a_newdat, 
                type = "response", se_fit = T, re_form = ~0)

test3 %>% view

fig_1a_mod_df <- tidy(fig1a_mod, conf.int = T, 
     effects = c("fixed", "ran_pars"))

ev <- scale(0.3580076, center = 0.3580076, scale = 0.0548541)[1]


sdm_pred_fig1a <- function(mod_df, nd, nsr, sr, name) {
  avg <- mod_df[1,2] + #intercept
    (mod_df[2,2]*nsr) + #nsr
    (mod_df[3,2]*sr) #sr
  
  # cal difference between mean and CI to add to 
  nd_ci_diff <- mod_df[1,2] - mod_df[1,4]
  
  lower <- mod_df[1,2] - #intercept
    (nd_ci_diff*nd) + # no dist
    (mod_df[2,4]*nsr) + #nsr
    (mod_df[3,4]*sr)  #sr
  
  
  upper <- mod_df[1,2] + #intercept
    (nd_ci_diff*nd) + # no dist
    (mod_df[2,5]*nsr) + #nsr
    (mod_df[3,5]*sr) #sr
  
  var <- c("mean", "lower", "upper")
  val <- c(avg, lower, upper)
  nam <- c()
  out <- data.frame(var, val) %>% 
    mutate(dist_01_19 = name) %>% 
    pivot_wider(names_from = var, values_from = val)
  return(out)
}

fig1a_nd_df <- sdm_pred_fig1a(fig_1a_mod_df, 1, 0, 0, "nd")
fig1a_nsr_df <- sdm_pred_fig1a(fig_1a_mod_df, 0, 1, 0, "nsr")
fig1a_sr_df <- sdm_pred_fig1a(fig_1a_mod_df, 0, 0, 1, "sr")


fig1a_plot <- bind_rows(fig1a_nd_df, fig1a_nsr_df, fig1a_sr_df) %>% 
  ggplot(aes(x = dist_01_19, y= mean, fill = dist_01_19)) + 
  geom_col(width = 0.5, colour = "black") + 
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15) + 
  geom_hline(yintercept = 0) + 
  annotate("text", x = 3, y = as.numeric(fig1a_sr_df[1,3]*1.3), size = 8, label = "*") +
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
fig1a_plot

## Fig 1b ----
fig1b_nsr_mod <- readRDS(here("output/models/fig1b_nsr_sdmTMB.rds"))

fig_1b_nsr_mod_df <- tidy(fig1b_nsr_mod, conf.int = T, 
                      effects = c("fixed", "ran_pars"))

sdm_pred_fig1b_nsr <- function(mod_df, p0, p1, p2, p3, name) {
  avg <- mod_df[1,2] + #intercept
    (mod_df[2,2]*p1) + #nsr
    (mod_df[3,2]*p2) +  #sr
    (mod_df[4,2]*p3)
  
  # cal difference between mean and CI to add to 
  nd_ci_diff <- mod_df[1,2] - mod_df[1,4]
  
  lower <- mod_df[1,2] - #intercept
    (nd_ci_diff*p0) + # no dist
    (mod_df[2,4]*p1) + #nsr
    (mod_df[3,4]*p2) +  #sr
    (mod_df[4,4]*p3)
  
  upper <- mod_df[1,2] + #intercept
    (nd_ci_diff*p0) + # no dist
    (mod_df[2,5]*p1) + #nsr
    (mod_df[3,5]*p2) +  #sr
    (mod_df[4,5]*p3)
  
  
  var <- c("mean", "lower", "upper")
  val <- c(avg, lower, upper)
  nam <- c()
  out <- data.frame(var, val) %>% 
    mutate(dist_group = name) %>% 
    pivot_wider(names_from = var, values_from = val)
  return(out)
}

fig1b_nsr_p0_df <- sdm_pred_fig1b_nsr(fig_1b_nsr_mod_df, 1, 0, 0, 0, "p0")
fig1b_nsr_p1_df <- sdm_pred_fig1b_nsr(fig_1b_nsr_mod_df, 0, 1, 0, 0, "p1")
fig1b_nsr_p2_df <- sdm_pred_fig1b_nsr(fig_1b_nsr_mod_df, 0, 0, 1, 0, "p2")
fig1b_nsr_p3_df <- sdm_pred_fig1b_nsr(fig_1b_nsr_mod_df, 0, 0, 0, 1, "p3")



fig1b_sr_mod <- readRDS(here("output/models/fig1b_sr_sdmTMB.rds"))

fig_1b_sr_mod_df <- tidy(fig1b_sr_mod, conf.int = T, 
                          effects = c("fixed", "ran_pars"))

sdm_pred_fig1b_sr <- function(mod_df, p0, p1, p2, name) {
  avg <- mod_df[1,2] + #intercept
    (mod_df[2,2]*p1) + 
    (mod_df[3,2]*p2)
  
  # cal difference between mean and CI to add to 
  nd_ci_diff <- mod_df[1,2] - mod_df[1,4]
  
  lower <- mod_df[1,2] - #intercept
    (nd_ci_diff*p0) + # no dist
    (mod_df[2,4]*p1) + 
    (mod_df[3,4]*p2)
  
  
  upper <- mod_df[1,2] + #intercept
    (nd_ci_diff*p0) + # no dist
    (mod_df[2,5]*p1) + 
    (mod_df[3,5]*p2)
  
  
  var <- c("mean", "lower", "upper")
  val <- c(avg, lower, upper)
  nam <- c()
  out <- data.frame(var, val) %>% 
    mutate(dist_group = name) %>% 
    pivot_wider(names_from = var, values_from = val)
  return(out)
}


fig1b_sr_p0_df <- sdm_pred_fig1b_sr(fig_1b_sr_mod_df, 1, 0, 0, "p0")
fig1b_sr_p1_df <- sdm_pred_fig1b_sr(fig_1b_sr_mod_df, 0, 1, 0, "p1")
fig1b_sr_p2_df <- sdm_pred_fig1b_sr(fig_1b_sr_mod_df, 0, 0, 1, "p2")


fig1b_plot <- bind_rows(fig1b_nsr_p0_df, fig1b_nsr_p1_df, 
                        fig1b_nsr_p2_df, fig1b_nsr_p3_df, 
                        fig1b_sr_p0_df, fig1b_sr_p1_df, fig1b_sr_p2_df) %>% 
  bind_cols(dist_type = c("nsr", "nsr", "nsr", "nsr", "sr", "sr", "sr")) %>% 
  ggplot(aes(x = dist_group, y = mean, colour = dist_type, group = dist_type)) + 
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15, position = position_dodge(0.2)) + 
  #geom_hline(yintercept = 0) + 
  geom_line(size = 0.75, position = position_dodge(0.2)) + 
  scale_x_discrete(name = "",
                   labels = c("No<br>Disturbance",
                              "2001—2006",
                              "2007—2012",
                              "2013—2019")) + 
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-90, -25),
                     breaks = seq(-90, -30, 30)) + 
  # colour vision deficiency palette
  scale_colour_manual(name = "Disturbance type", 
                      values = c("#E6AB02", "#A6761D", "#66A61E"), 
                      labels = c("Non-stand-replacing disturbance",
                                 "Stand-replacing disturbance")) + 
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = c(0.3, 0.87),
        legend.background = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_markdown())
fig1b_plot


fig_1 <- fig1a_plot + fig1b_plot + 
  plot_annotation(tag_levels = 'a')

ggsave("/Users/sam/Documents/Admin_Disturbance/output/fig_1.png", 
       plot = fig_1,
       dpi = 300, dev = "png", height = 10, width = 24, unit = "cm")

## Fig 2a

dist_order_mod <-  readRDS(here("output/models/fig2a_sdmTMB.rds"))

fig_2a_mod_df <- tidy(dist_order_mod, conf.int = T, 
                      effects = c("fixed", "ran_pars"))


sdm_pred_fig2a_nsr <- function(mod_df, nsr_nsr, nsr_sr, 
                               sr_nsr, sr_sr, name) {
  avg <- mod_df[1,2] + #intercept
    (mod_df[2,2]*nsr_sr) + #nsr
    (mod_df[3,2]*sr_nsr) +  #sr
    (mod_df[4,2]*sr_sr)
  
  # cal difference between mean and CI to add to 
  nd_ci_diff <- mod_df[1,2] - mod_df[1,4]
  
  lower <- mod_df[1,2] - #intercept
    (nd_ci_diff*nsr_nsr) + # no dist
    (mod_df[2,4]*nsr_sr) + #nsr
    (mod_df[3,4]*sr_nsr) +  #sr
    (mod_df[4,4]*sr_sr)
  
  upper <- mod_df[1,2] + #intercept
    (nd_ci_diff*nsr_nsr) + # no dist
    (mod_df[2,5]*nsr_sr) + #nsr
    (mod_df[3,5]*sr_nsr) +  #sr
    (mod_df[4,5]*sr_sr)
  
  
  var <- c("mean", "lower", "upper")
  val <- c(avg, lower, upper)
  nam <- c()
  out <- data.frame(var, val) %>% 
    mutate(order = name) %>% 
    pivot_wider(names_from = var, values_from = val)
  return(out)
}

fig2a_nsr_nsr_df <- sdm_pred_fig2a_nsr(fig_2a_mod_df, 1, 0, 0, 0, "nsr_nsr")
fig2a_nsr_sr_df <- sdm_pred_fig2a_nsr(fig_2a_mod_df, 0, 1, 0, 0, "nsr_sr")
fig2a_sr_nsr_df <- sdm_pred_fig2a_nsr(fig_2a_mod_df, 0, 0, 1, 0, "sr_nsr")
fig2a_sr_sr_df <- sdm_pred_fig2a_nsr(fig_2a_mod_df, 0, 0, 0, 1, "sr_sr")


fig2a <- bind_rows(fig2a_nsr_nsr_df, fig2a_nsr_sr_df, 
                   fig2a_sr_nsr_df, fig2a_sr_sr_df) %>% 
  ggplot(aes(x = order, y = mean)) + 
  geom_col(width = 0.5, colour = "black") + 
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = 0.15) + 
  geom_hline(yintercept = 0) + 
  annotate("text", x = 3, y = as.numeric(fig2a_sr_nsr_df[1,3]*1.15), size = 8, label = "*") +
  scale_x_discrete(name = "Disturbance order",
                   labels = c("NSR\u2192NSR", "NSR\u2192SR",
                              "SR\u2192NSR", "SR\u2192SR")) +
  scale_y_continuous(name = "Mean ±SE carbon flux (Mg C ha<sup>-1</sup>)",
                     limits = c(-100, 5),
                     breaks = seq(-100, 0, 25),
                     expand = c(0,0)) +
  theme_classic() + 
  theme(legend.title = element_markdown(),
        legend.position = "none",
        legend.background = element_blank(),
        axis.text.x = element_markdown(),
        axis.title.y = element_markdown())
fig2a
