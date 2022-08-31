# The code below is to plot the main figures of the paper

library(tidyverse)
library(nimble)
library(patchwork)
source("code/functions.R")

# ~ 1. Figure 1: Map -----------------------------------------------------------

# Made using QGIS

# ~ 2. Figure 2: Caterpillar plot all (significant & non significant) ----------

# ~~~ a. Full model ------------------------------------------------------------
model_full_df <- data.frame(name = c("beta[1]", "beta[2]", "beta[3]", "beta[4]", 
                                     "beta[5]", "beta[6]", "beta[7]", "beta[8]", 
                                     "beta[9]", "beta[10]", "beta[11]", "beta[12]", 
                                     "beta[13]", "beta[14]", "beta[15]", "beta[16]", 
                                     "beta[17]", "beta[18]", "beta[19]", "beta[20]", 
                                     "beta[21]", "beta[22]", "beta[23]", "beta[24]",
                                     "beta[25]", "beta[26]", "beta[27]", "beta[28]", 
                                     "beta[29]", "beta[30]", "beta[31]", "beta[32]",
                                     
                                     "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", 
                                     "gamma[5]", "gamma[6]", "gamma[7]", "gamma[8]", 
                                     "gamma[9]", "gamma[10]", "gamma[11]", "gamma[12]", 
                                     "gamma[13]", "gamma[14]", "gamma[15]", "gamma[16]", 
                                     "gamma[17]", "gamma[18]", "gamma[19]", "gamma[20]", 
                                     "gamma[21]", "gamma[22]", "gamma[23]", "gamma[24]"),
                            event = c(rep("litter production", times = 8*4), rep("litter size", times = 8*3)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr",
                                        "5-9 yr", "10-15 yr", "\u2265 16 yr"), each = 8),
                            type = rep(c("individual", "individual", "individual", "individual",
                                         "sea-ice", "AO", "AO", "AO"),  times = 7),
                            covariate = rep(c("intercept", "DateCapture", "Size", "Size2",
                                          "DateBreakUp", "WinterAO", "PriorWinterAO", "PriorSpringAO"), times = 7)) %>%
  mutate(covariate_age = ifelse(age == "5-9 yr", paste0(covariate, "_y"), 
                                ifelse(age == "10-15 yr", paste0(covariate, "_p"),
                                       ifelse(age == "16-20 yr", paste0(covariate, "_oa"), 
                                              ifelse(age == "\u2265 21 yr", paste0(covariate, "_ob"), 
                                                     ifelse(age == "\u2265 16 yr", paste0(covariate, "_o"), NA))))))

load("data/fit_break_up.RData")
numVars_1 <- 32 ; numVars_2 <- 24
N <- dim(fit_break_up$samples$chain1)[1]
C <- dim(fit_break_up$samples$chain1)[2]
res_temp <- rbind(fit_break_up$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up$samples$chain2[seq(1, N, by = 3), ])
res <- res_temp[, c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                    grep("sigma", colnames(res_temp)))]
cols <- colnames(res)[c(grep("beta", colnames(res)), grep("gamma", colnames(res)))]

caterpillar_full <- as.data.frame(res) %>%
  dplyr::select(-sigma1, -sigma2) %>%
  pivot_longer(cols = all_of(cols)) %>%
  left_join(x = .,
            y = model_full_df,
            by = "name") %>%
  mutate(age = factor(age,
                      levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr", "\u2265 16 yr")),
         model = "full")

caterpillar_full_1 <- caterpillar_full %>% 
  filter(event == "litter production") 

caterpillar_full_2 <- caterpillar_full %>% 
  filter(event == "litter size") 


# ~~~ b. Best model ------------------------------------------------------------
model_best_df <- data.frame(name = c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]",
                                "beta[6]", "beta[7]", "beta[8]", "beta[9]", "beta[10]", 
                                "beta[11]", "beta[12]", "beta[13]",
                                
                                "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", "gamma[5]", 
                                "gamma[6]"),
                       event = c(rep("litter production", times = 13), rep("litter size", times = 6)),
                       age = c("5-9 yr", "5-9 yr", 
                               "10-15 yr", "10-15 yr", "10-15 yr", "10-15 yr", "10-15 yr",
                               "16-20 yr", "16-20 yr", "16-20 yr", "16-20 yr",
                               "\u2265 21 yr", "\u2265 21 yr",
                               
                               "5-9 yr", "5-9 yr", "5-9 yr", 
                               "10-15 yr", "10-15 yr",
                               "\u2265 16 yr"),
                       type = c("individual", "individual",
                                "individual", "individual", "individual", "AO", "AO",
                                "individual", "individual", "AO", "AO",
                                "individual", "individual",
                                
                                "individual", "individual", "AO",
                                "individual", "sea-ice",
                                "individual"),
                       covariate = c("intercept", "DateCapture",
                                     "intercept", "Size", "Size2", "WinterAO", "PriorSpringAO",
                                     "intercept", "DateCapture", "WinterAO", "PriorSpringAO",
                                     "intercept", "DateCapture",
                                     
                                     "intercept", "DateCapture", "PriorWinterAO",
                                     "intercept", "DateBreakUp",
                                     "intercept")) %>%
  mutate(covariate_age = ifelse(age == "5-9 yr", paste0(covariate, "_y"), 
                                ifelse(age == "10-15 yr", paste0(covariate, "_p"),
                                       ifelse(age == "16-20 yr", paste0(covariate, "_oa"), 
                                              ifelse(age == "\u2265 21 yr", paste0(covariate, "_ob"), 
                                                     ifelse(age == "\u2265 16 yr", paste0(covariate, "_o"), NA))))))

load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
res <- res_temp[, c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                    grep("sigma", colnames(res_temp)))]
cols <- colnames(res)[c(grep("beta", colnames(res)), grep("gamma", colnames(res)))]

caterpillar_best <- as.data.frame(res) %>%
  dplyr::select(-sigma1, -sigma2) %>%
  pivot_longer(cols = all_of(cols)) %>%
  left_join(x = .,
            y = model_best_df,
            by = "name") %>%
  mutate(age = factor(age,
                      levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr", "\u2265 16 yr")),
         model = "best")

caterpillar_best_1 <- caterpillar_best %>% 
  filter(event == "litter production") 
  
caterpillar_best_2 <- caterpillar_best %>% 
  filter(event == "litter size")


# ~~~ c. Plot ------------------------------------------------------------------
caterpillar_plot_1 <- ggplot() + #, size = model)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(data = caterpillar_full_1, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.77, alpha = 0.2) +
  stat_summary(data = caterpillar_full_1, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.7, alpha = 0.2) +
  stat_summary(data = caterpillar_best_1, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(data = caterpillar_best_1, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(legend.title = element_blank()) + 
  scale_color_brewer(palette = "Dark2",
                     labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  scale_x_discrete(limits = rev(caterpillar_full_1 %>% distinct(covariate_age) %>% pull(covariate_age)),
                   labels = rev(caterpillar_full_1  %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "credible interval") +
  facet_wrap(.~event) +
  coord_flip() 


colfunc <- colorRampPalette(RColorBrewer::brewer.pal(n = 4, "Dark2")[3:4])
palette_litter_size <- c(RColorBrewer::brewer.pal(n = 3, "Dark2")[1:2], colfunc(3)[2])

caterpillar_plot_2 <- ggplot() + #, size = model)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(data = caterpillar_full_2, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.77, alpha = 0.2) +
  stat_summary(data = caterpillar_full_2, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.7, alpha = 0.2) +
  stat_summary(data = caterpillar_best_2, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(data = caterpillar_best_2, 
               aes(x = covariate_age , y = value, color = age),
               fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = palette_litter_size,
                     labels = c("5-9 yr", "10-15 yr", "\u2265 16 yr")) +
  scale_x_discrete(limits = rev(caterpillar_full_2 %>% distinct(covariate_age) %>% pull(covariate_age)),
                   labels = rev(caterpillar_full_2  %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "credible interval") +
  facet_wrap(.~event) +
  coord_flip()

ggsave("10_Meetings/figures meeting/caterpilar_litter_size.png", height = 12, width = 12, units = "cm")


# Combine +++++++++++++++++ 
layout <- '
AB
AB
AB
AB
AB
AB
AB
AB
A#
'
caterpillar_plot_1 + caterpillar_plot_2 +
  plot_annotation(tag_levels = 'A') +
  plot_layout(# guides = 'collect', 
              design = layout) &
  theme(legend.position = 'bottom')




# ~ 3. Figure 3: Boxplot absolute prob -----------------------------------------
load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest_1 <- grep("beta", colnames(res_temp))
cols_of_interest_2 <- grep("gamma", colnames(res_temp))

# First binomial
res_1 <- res_temp[, cols_of_interest_1]
# Back transform
p_litter_y <- p_litter_p <- p_litter_o1 <- p_litter_o2 <- c()
for (j in 1:dim(res_1)[1]) {
  p_litter_y[j] <- plogis(res_1[j, 1])
  p_litter_p[j] <- plogis(res_1[j, 3])
  p_litter_o1[j] <- plogis(res_1[j, 8])
  p_litter_o2[j] <- plogis(res_1[j, 12])
}

# Second binomial
res_2 <- res_temp[, cols_of_interest_2]
# Back transform
p_23_cub_y <- p_23_cub_p <- p_23_cub_o <- c()
for (j in 1:dim(res_2)[1]) {
  p_23_cub_y[j] <- plogis(res_2[j, 1])
  p_23_cub_p[j] <- plogis(res_2[j, 4])
  p_23_cub_o[j] <- plogis(res_2[j, 6])
}

# Multiply the probabilities
df_plot <- rbind(data.frame(event = "no litter",
                            probability = c(1 - p_litter_y, 1 - p_litter_p, 1 - p_litter_o1, 1 - p_litter_o2),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(p_litter_y))),
                 data.frame(event = "1 cub",
                            probability = c(p_litter_y * (1 - p_23_cub_y),
                                            p_litter_p * (1 - p_23_cub_p),
                                            p_litter_o1 * (1 - p_23_cub_o),
                                            p_litter_o2 * (1 - p_23_cub_o)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(p_23_cub_o))),
                 data.frame(event = "2-3 cubs",
                            probability = c(p_litter_y * p_23_cub_y,
                                            p_litter_p * p_23_cub_p,
                                            p_litter_o1 * p_23_cub_o,
                                            p_litter_o2 * p_23_cub_o),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(p_23_cub_y)))) %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")))


ggplot(data = df_plot) +
  geom_boxplot(aes(x = event, y = probability, color = age), size = 0.75) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2",
                     labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) + 
  scale_x_discrete(labels = c("no litter", "singleton\nlitter", "large\nlitter")) +
  ylim(c(0, 1)) +
  labs(x = "", y = "probability",
       color = "female's age") 



# ~ 4. Figure 4: Effect of date of capture -------------------------------------

df_plot <- read_csv("data/effect_DateCapture.csv", show_col_types = FALSE)  %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")),
         event_2 = factor(ifelse(event == "no litter", "no litter", 
                                 ifelse(event == "1 cub", "singleton litter", "large litter")),
                          levels = c("no litter", "singleton litter", "large litter")))

ggplot(data = df_plot) +
  geom_ribbon(aes(x = var, ymin = ci_2.5, ymax = ci_97.5, fill = age), alpha = 0.25) +
  geom_line(aes(x = var, y = mean, group = age, color = age), linetype = "solid", size = 1) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_x_continuous(breaks = c(85, 95, 105, 115, 125)) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "date of capture",
       y = "probability") +
  facet_wrap(event_2~.)



# ~ 5. Figure 5: Average effect size -------------------------------------------

# The graph below is obtained using the following method: I first calculate 
# the absolute value of all values in all environmental posteriors. Then I calculate 
# the mean across environmental covariates within iterations and within age classes. 
# Then I plot the mean, 89% CRI and 95% CRI.

model_full_df <- data.frame(name = c("beta[1]", "beta[2]", "beta[3]", "beta[4]", 
                                     "beta[5]", "beta[6]", "beta[7]", "beta[8]", 
                                     "beta[9]", "beta[10]", "beta[11]", "beta[12]", 
                                     "beta[13]", "beta[14]", "beta[15]", "beta[16]", 
                                     "beta[17]", "beta[18]", "beta[19]", "beta[20]", 
                                     "beta[21]", "beta[22]", "beta[23]", "beta[24]",
                                     "beta[25]", "beta[26]", "beta[27]", "beta[28]", 
                                     "beta[29]", "beta[30]", "beta[31]", "beta[32]",
                                     
                                     "gamma[1]", "gamma[2]", "gamma[3]", "gamma[4]", 
                                     "gamma[5]", "gamma[6]", "gamma[7]", "gamma[8]", 
                                     "gamma[9]", "gamma[10]", "gamma[11]", "gamma[12]", 
                                     "gamma[13]", "gamma[14]", "gamma[15]", "gamma[16]", 
                                     "gamma[17]", "gamma[18]", "gamma[19]", "gamma[20]", 
                                     "gamma[21]", "gamma[22]", "gamma[23]", "gamma[24]"),
                            event = c(rep("litter production", times = 8*4), rep("litter size", times = 8*3)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr",
                                        "5-9 yr", "10-15 yr", "\u2265 16 yr"), each = 8),
                            type = rep(c("individual", "individual", "individual", "individual",
                                         "sea-ice", "AO", "AO", "AO"),  times = 7),
                            covariate = rep(c("intercept", "DateCapture", "Size", "Size2",
                                              "DateBreakUp", "WinterAO", "PriorWinterAO", "PriorSpringAO"), times = 7)) %>%
  mutate(covariate_age = ifelse(age == "5-9 yr", paste0(covariate, "_y"), 
                                ifelse(age == "10-15 yr", paste0(covariate, "_p"),
                                       ifelse(age == "16-20 yr", paste0(covariate, "_oa"), 
                                              ifelse(age == "\u2265 21 yr", paste0(covariate, "_ob"), 
                                                     ifelse(age == "\u2265 16 yr", paste0(covariate, "_o"), NA))))))

load("data/fit_break_up.RData")
numVars_1 <- 32 ; numVars_2 <- 24
N <- dim(fit_break_up$samples$chain1)[1]
C <- dim(fit_break_up$samples$chain1)[2]
res_temp <- rbind(fit_break_up$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up$samples$chain2[seq(1, N, by = 3), ])
res <- res_temp[, c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                    grep("sigma", colnames(res_temp)))]
cols <- colnames(res)[c(grep("beta", colnames(res)), grep("gamma", colnames(res)))]

caterpillar_full <- as.data.frame(res) %>%
  dplyr::select(-sigma1, -sigma2) %>%
  mutate(iteration = c(1:dim(res)[1])) %>%
  pivot_longer(cols = all_of(cols)) %>%
  left_join(x = .,
            y = model_full_df,
            by = "name") %>%
  mutate(age = factor(age,
                      levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr", "\u2265 16 yr")),
         model = "full")

mean_effect <- caterpillar_full %>%
  filter(type %in% c("sea-ice", "AO")) %>%
  group_by(iteration, age, event) %>%
  summarize(mean = mean(value),
            mean_abs = mean(abs(value)),
            abs_mean = abs(mean(value)))

effect_size_breeding <- ggplot(data = mean_effect[mean_effect$event == "litter production", ], 
                               aes(x = age , y = mean_abs, color = age)) + 
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(limits = rev(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
                   labels = rev(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"))) +
  labs(x = "", y = "mean effect size") +
  facet_wrap(.~event) +
  coord_flip()

colfunc <- colorRampPalette(RColorBrewer::brewer.pal(n = 4, "Dark2")[3:4])
palette_litter_size <- c(RColorBrewer::brewer.pal(n = 3, "Dark2")[1:2], colfunc(3)[2])

effect_size_litter_size <- ggplot(data = mean_effect[mean_effect$event == "litter size", ], 
                                  aes(x = age , y = mean_abs, color = age)) + 
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  # geom_boxplot(size = 0.65) +   # With a boxplot instead of caterpilar
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none") +
  scale_color_manual(values = palette_litter_size) +
  scale_x_discrete(limits = rev(c("5-9 yr", "10-15 yr", "\u2265 16 yr")),
                   labels = rev(c("5-9 yr", "10-15 yr", "\u2265 16 yr"))) +
  labs(x = "", y = "mean effect size") +
  facet_wrap(.~event) +
  coord_flip()

# Combine +++++++++++++++++++++++
layout <- '
AB
AB
AB
AB
AB
AB
AB
AB
AB
AB
A#
'
effect_size_breeding + effect_size_litter_size + 
  plot_annotation(tag_levels = 'A') +
  plot_layout(design = layout)



# ~ 6. Figure 6: Effect of sea-ice --------------------------------------------

df_plot_day_break_up <- read_csv("data/effect_DateBreakUp.csv")  %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")),
         event_2 = factor(ifelse(event == "no litter", "no litter", 
                                 ifelse(event == "1 cub", "singleton litter", "large litter")),
                          levels = c("no litter", "singleton litter", "large litter")))

ggplot(data = df_plot_day_break_up) +
  geom_ribbon(aes(x = var, ymin = ci_2.5, ymax = ci_97.5, fill = age), alpha = 0.25) +
  geom_line(aes(x = var, y = mean, group = age, color = age), linetype = "solid", size = 1) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  scale_fill_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  labs(x = "date of sea ice break-up",
       y = "probability") +
  facet_wrap(event_2~.)



# ~ 7. Figure 7: Effect of AO -------------------------------------------------

df_plot_w_AO <- read_csv("data/effect_WinterAO.csv")  %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")),
         event_2 = factor(ifelse(event == "no litter", "no litter", 
                                 ifelse(event == "1 cub", "singleton litter", "large litter")),
                          levels = c("no litter", "singleton litter", "large litter")))

plot_w_AO_shading <- ggplot(data = df_plot_w_AO) +
  geom_ribbon(aes(x = var, ymin = ci_2.5, ymax = ci_97.5, fill = age), alpha = 0.25) +
  geom_line(aes(x = var, y = mean, group = age, color = age), linetype = "solid", size = 1) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  scale_fill_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +  
  labs(x = "winter AO index",
       y = "probability") +
  facet_wrap(event_2~.)

df_plot_w_AO_t_1 <- read_csv("data/effect_PriorWinterAO.csv")  %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")),
         event_2 = factor(ifelse(event == "no litter", "no litter", 
                                 ifelse(event == "1 cub", "singleton litter", "large litter")),
                          levels = c("no litter", "singleton litter", "large litter")))

plot_w_AO_t_1_shading <- ggplot(data = df_plot_w_AO_t_1) +
  geom_ribbon(aes(x = var, ymin = ci_2.5, ymax = ci_97.5, fill = age), alpha = 0.25) +
  geom_line(aes(x = var, y = mean, group = age, color = age), linetype = "solid", size = 1) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  scale_fill_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  labs(x = "prior winter AO index",
       y = "probability") +
  facet_wrap(event_2~.)

ggsave("10_Meetings/figures meeting/effect_winter_AO_t-1.png", height = 8, width = 18, units = "cm")


df_plot_s_AO_t_1 <- read_csv("data/effect_PriorSpringAO.csv")  %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")),
         event_2 = factor(ifelse(event == "no litter", "no litter", 
                                 ifelse(event == "1 cub", "singleton litter", "large litter")),
                          levels = c("no litter", "singleton litter", "large litter")))

plot_s_AO_t_1_shading <- ggplot(data = df_plot_s_AO_t_1) +
  geom_ribbon(aes(x = var, ymin = ci_2.5, ymax = ci_97.5, fill = age), alpha = 0.25) +
  geom_line(aes(x = var, y = mean, group = age, color = age), linetype = "solid", size = 1) +
  theme_bw() +
  theme(legend.title = element_blank(), legend.position = "top") +
  scale_color_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  scale_fill_brewer(palette = "Dark2", labels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")) +
  labs(x = "prior spring AO index",
       y = "probability") +
  facet_wrap(event_2~.)

plot_w_AO_shading / plot_w_AO_t_1_shading / plot_s_AO_t_1_shading + 
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = 'top')


# ~ 8. Figure 8: Cub survival (CR model) ---------------------------------------

# source("05_script/functions.R")
load("07_results/01_interim_results/model_outputs/CR_model/JAGS_fit_resident_bear_age_except_beta_gamma.RData")

res <- rbind(out_age_2$samples[[1]], out_age_2$samples[[2]]) 
temp_df <- as.data.frame(res[, -dim(res)[2]])

column_names <- colnames(temp_df)[-dim(res)[2]] 

caterpillar <- temp_df %>%
  pivot_longer(cols = all_of(column_names)) %>%
  mutate(family = ifelse(str_detect(name, "phi"), "phi",
                         ifelse(str_detect(name, "s"), "s",
                                ifelse(str_detect(name, "l"), "l",
                                       ifelse(str_detect(name, "beta"), "beta",
                                              ifelse(str_detect(name, "gamma"), "gamma",
                                                     ifelse(str_detect(name, "prop"), "prop", 
                                                            ifelse(str_detect(name, "kappa"), "kappa", "p"))))))))
caterpillar_2 <- caterpillar[caterpillar$family %in% c("s", "l"), ] %>% 
  mutate(mother_age = factor(ifelse(substring(name, first = 5, last = 5) == "1", "young",
                                    ifelse(substring(name, first = 5, last = 5) == "2", "prime-aged", "old")), 
                             levels = c("young", "prime-aged", "old"))) %>%
  filter(name %in% c("s[1,1]", "s[1,2]", "s[1,3]",     
                     "s[2,1]", "s[2,2]", "s[2,3]",
                     "l02[1]", "l02[2]", "l02[3]")) %>%
  mutate(probability = ifelse(name %in% c("s[1,1]", "s[1,2]", "s[1,3]"), 
                              "cub survival in \nsingleton litters",
                              ifelse(name %in% c("s[2,1]", "s[2,2]", "s[2,3]"), 
                                     "cub survival in \nlarge litters", # "cub survival in \ntwin litters",
                                     "cub litter survival \n(large litters)"))) #"litter survival \n(twin litters)")))

colfunc <- colorRampPalette(RColorBrewer::brewer.pal(n = 4, "Dark2")[3:4])
palette_cub_survival <- c(RColorBrewer::brewer.pal(n = 3, "Dark2")[1:2], colfunc(3)[2])

ggplot(caterpillar_2, aes(x = probability, y = value, color = mother_age)) +
  geom_boxplot(size = 0.65) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = palette_cub_survival,
                     labels = c("5-9 yr", "10-15 yr", "\u2265 16 yr")) + 
  scale_x_discrete(limits = c("cub survival in \nsingleton litters", "cub survival in \nlarge litters",
                              "cub litter survival \n(large litters)")) +
  labs(x = "", y = "probability", color = "mother's age")










