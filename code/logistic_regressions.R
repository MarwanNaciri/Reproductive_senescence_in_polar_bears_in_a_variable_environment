
#' The code below is to run the regressions of the probability of producing a litter and the probability of having a large litter, given a litter.
#' Both regressions are performed in a Bayesian framework using the {nimble} package. We built a single nimble model for both regressions, so that the absolute probability of producing a large litter can be calculated (as the product of the probability of producing a litter and the conditional probability of having a large litter).

knitr::opts_chunk$set(echo = FALSE)
library(tidyverse)
library(lubridate)
library(nimble)
library(patchwork)
library(cowplot)

#' Load the data 
source("code/functions.R")

# Environmental data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sea_ice_data <- read_csv("data/sea_ice_data.csv")

sea_ice_data <- rbind(sea_ice_data, c(2022, rep(NA, times = ncol(sea_ice_data)-1))) # Add row for year 2022
sea_ice_data <- data.frame(sea_ice_data,    
                           DateBreakUp_t_1 = c(NA, sea_ice_data$DateBreakUp[-nrow(sea_ice_data)]),    # Re-index, to get the date of break-up in year t-1
                           DateFreezeUp_t_1 = c(NA, sea_ice_data$DateFreezeUp[-nrow(sea_ice_data)]))    # Same for the date of freeze-up

AO_data <- read_csv("data/AO_data.csv")


# Data for the regression of the probability of litter production ~~~~~~~~~~~~~~
CR_data_1 <- CR_f_lone_or_with_coys <- read_csv("data/CR_f_lone_or_with_coys.csv") %>% # Relevant capture data for all the females that were alone at capture or with cubs-of-the-year
  filter(year >= 2000) # Keep only captures from 2000 onward due to changes in the relative capture probability of lone females vs females with cubs-of-the-year between 1992-1999 and 2000-2022

CR_data_1 %>%
  left_join(x = CR_data_1,      # Combine the capture data with environmental data 
            y = sea_ice_data,
            by = "year") %>% 
  left_join(., y = AO_data,
            by = "year") -> data_model_1 


# Data for the regression of the probability of a large litter (given a litter) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CR_data_2 <- read_csv("data/CR_f_with_coys.csv") %>% # Relevant capture data for all the females that were with cubs-of-the-year at capture
  filter(lon < 40) # Remove one female that was captured outside of the study area

CR_data_2 %>%
  left_join(x = CR_data_2,
            y = sea_ice_data,
            by = "year") %>% 
  left_join(., y = AO_data,
            by = "year") -> data_model_2

#' 
#' 
#' Now let's combine both datasets (i.e. females with cubs of the year captured between 1992 and 2022, and lone females captured between 200), so that when we scale the covariates, they are scaled over the same values.
#' 
CR_data_f_w_cubs <- read_csv("data/CR_f_with_coys.csv") %>%
  filter(lon < 40)
CR_data_lone_fem <- read_csv("data/CR_f_lone_or_with_coys.csv") %>%
  filter(year >= 2000,
         cub_status == "n")
CR_data <- rbind(CR_data_f_w_cubs, CR_data_lone_fem)

CR_data %>%
  left_join(x = CR_data,
            y = sea_ice_data,
            by = "year") %>% 
  left_join(., y = AO_data,
            by = "year") -> data_model

# Predictors
{DateCapture <- data_model$day_number ; DateCapture_s <- as.vector(scale(DateCapture))

  Size <- data_model$s_length ; Size_s <- as.vector(scale(Size))
  Size_s2 <- Size_s^2
  
  DateBreakUp_t_1 <- data_model$DateBreakUp_t_1 ; DateBreakUp_t_1_s <- as.vector(scale(DateBreakUp_t_1))
  DateFreezeUp_t_1 <- data_model$DateFreezeUp_t_1 ; DateFreezeUp_t_1_s <- as.vector(scale(DateFreezeUp_t_1))
  
  WinterAO <- data_model$winter_AO ; WinterAO_s <- as.vector(scale(WinterAO))
  PriorWinterAO <- data_model$prior_winter_AO ; PriorWinterAO_s <- as.vector(scale(PriorWinterAO))
  PriorSpringAO <- data_model$prior_spring_AO ; PriorSpringAO_s <- as.vector(scale(PriorSpringAO))
}

#' 
#' # A. With *DayBreakUp*
#' 
#' ## 1. Full model
#' 
#' Let's build and run the full model. This model includes *DayBreakUp* as the sea-ice covariate. The model in part B. includes *DayFreezeUp* as the sea-ice covariate.
#' 
model_break_up <- nimbleCode({
  # Logistic regression for litter production
  for (i in 1:numVars_1) {
    beta[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_1) {
    y_1[i] ~ dbern(p_1[i])
    logit(p_1[i]) <-
      beta[1] * age_5_9_1[i] +
      beta[2] * age_5_9_1[i] * DateCapture_s_1[i] +
      beta[3] * age_5_9_1[i] * Size_s_1[i] +
      beta[4] * age_5_9_1[i] * Size_s2_1[i] +
      beta[5] * age_5_9_1[i] * DateBreakUp_t_1_s_1[i] +
      beta[6] * age_5_9_1[i] * WinterAO_s_1[i] +
      beta[7] * age_5_9_1[i] * PriorWinterAO_s_1[i] +
      beta[8] * age_5_9_1[i] * PriorSpringAO_s_1[i] +
      
      beta[9] * age_10_15_1[i] +
      beta[10] * age_10_15_1[i] * DateCapture_s_1[i] +
      beta[11] * age_10_15_1[i] * Size_s_1[i] +
      beta[12] * age_10_15_1[i] * Size_s2_1[i] +
      beta[13] * age_10_15_1[i] * DateBreakUp_t_1_s_1[i] +
      beta[14] * age_10_15_1[i] * WinterAO_s_1[i] +
      beta[15] * age_10_15_1[i] * PriorWinterAO_s_1[i] +
      beta[16] * age_10_15_1[i] * PriorSpringAO_s_1[i] +
      
      beta[17] * age_16_20[i] +
      beta[18] * age_16_20[i] * DateCapture_s_1[i] +
      beta[19] * age_16_20[i] * Size_s_1[i] +
      beta[20] * age_16_20[i] * Size_s2_1[i] +
      beta[21] * age_16_20[i] * DateBreakUp_t_1_s_1[i] +
      beta[22] * age_16_20[i] * WinterAO_s_1[i] +
      beta[23] * age_16_20[i] * PriorWinterAO_s_1[i] +
      beta[24] * age_16_20[i] * PriorSpringAO_s_1[i] +
      
      beta[25] * age_21_more[i] +
      beta[26] * age_21_more[i] * DateCapture_s_1[i] +
      beta[27] * age_21_more[i] * Size_s_1[i] +
      beta[28] * age_21_more[i] * Size_s2_1[i] +
      beta[29] * age_21_more[i] * DateBreakUp_t_1_s_1[i] +
      beta[30] * age_21_more[i] * WinterAO_s_1[i] +
      beta[31] * age_21_more[i] * PriorWinterAO_s_1[i] +
      beta[32] * age_21_more[i] * PriorSpringAO_s_1[i] +
      
      eps1[year_1[i]]
  }
  for (i in 1:nbyear_1) {
    eps1[i] ~ dnorm(0, sd = sigma1)
  }
  sigma1 ~ dunif(0, 10)
  # Logistic regression for litter size
  for (i in 1:numVars_2) {
    gamma[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_2) {
    y_2[i] ~ dbern(p_2[i])
    logit(p_2[i]) <-
      gamma[1] * age_5_9_2[i] +
      gamma[2] * age_5_9_2[i] * DateCapture_s_2[i] +
      gamma[3] * age_5_9_2[i] * Size_s_2[i] +
      gamma[4] * age_5_9_2[i] * Size_s2_2[i] +
      gamma[5] * age_5_9_2[i] * DateBreakUp_t_1_s_2[i] +
      gamma[6] * age_5_9_2[i] * WinterAO_s_2[i] +
      gamma[7] * age_5_9_2[i] * PriorWinterAO_s_2[i] +
      gamma[8] * age_5_9_2[i] * PriorSpringAO_s_2[i] +
      
      gamma[9] * age_10_15_2[i] +
      gamma[10] * age_10_15_2[i] * DateCapture_s_2[i] +
      gamma[11] * age_10_15_2[i] * Size_s_2[i] +
      gamma[12] * age_10_15_2[i] * Size_s2_2[i] +
      gamma[13] * age_10_15_2[i] * DateBreakUp_t_1_s_2[i] +
      gamma[14] * age_10_15_2[i] * WinterAO_s_2[i] +
      gamma[15] * age_10_15_2[i] * PriorWinterAO_s_2[i] +
      gamma[16] * age_10_15_2[i] * PriorSpringAO_s_2[i] +
      
      gamma[17] * age_16_more[i] +
      gamma[18] * age_16_more[i] * DateCapture_s_2[i] +
      gamma[19] * age_16_more[i] * Size_s_2[i] +
      gamma[20] * age_16_more[i] * Size_s2_2[i] +
      gamma[21] * age_16_more[i] * DateBreakUp_t_1_s_2[i] +
      gamma[22] * age_16_more[i] * WinterAO_s_2[i] +
      gamma[23] * age_16_more[i] * PriorWinterAO_s_2[i] +
      gamma[24] * age_16_more[i] * PriorSpringAO_s_2[i] +
      
      eps2[year_2[i]]
  }
  for (i in 1:nbyear_2) {
    eps2[i] ~ dnorm(0, sd = sigma2)
  }
  sigma2 ~ dunif(0, 10)
})

#' 
# Logistic regression for litter production ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable 
y_1 <- data_model_1$cub_number                         # Extract the response variable
y_1[y_1 == 0] <- 0 ; y_1[y_1 %in% c(1, 2, 3)] <- 1     # Success = 1, 2 or 3 cubs of the year ; failure = 0 cubs

# Covariates
numVars_1 <- 32 
age_1 <- as.numeric(data_model_1$age)
age_5_9_1 <- ifelse (age_1 < 10, 1, 0)
age_10_15_1 <- ifelse(age_1 >= 10 & age_1 <= 15, 1, 0)
age_16_20 <- ifelse(age_1 > 15 & age_1 <= 20, 1, 0)
age_21_more <- ifelse (age_1 > 20, 1, 0)

DateCapture_s_1 <- scale_ext(data_model_1$day_number, DateCapture)
Size_s_1 <- scale_ext(data_model_1$s_length, Size)
Size_s2_1 <- Size_s_1^2
DateBreakUp_t_1_s_1 <- scale_ext(data_model_1$DateBreakUp_t_1, DateBreakUp_t_1)
WinterAO_s_1 <- scale_ext(data_model_1$winter_AO, WinterAO)
PriorWinterAO_s_1 <- scale_ext(data_model_1$prior_winter_AO, PriorWinterAO)
PriorSpringAO_s_1 <- scale_ext(data_model_1$prior_spring_AO, PriorSpringAO)

# Yearly random effect
df_years_1 <- data.frame(year = unique(data_model_1$year),
                       index_year = seq(1:length(unique(data_model_1$year))))
year_1 <- data_model_1$year
for (k in 1:nrow(df_years_1)) {
  year_1[which(year_1 == df_years_1$year[k])] <- df_years_1$index_year[k]
}
nbyear_1 <- length(unique(year_1))

}

# Logistic regression for litter size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable
  y_2 <- data_model_2$cub_number                  # Extract the response variable
y_2[y_2 == 1] <- 0 ; y_2[y_2 %in% c(2, 3)] <- 1 # Success = 2 or 3 cubs-of-the-year ; failure = 1 cubs-of-the-year 

# Covariates
  numVars_2 <- 24
  age_2 <- as.numeric(data_model_2$age)
  age_5_9_2 <- ifelse (age_2 < 10, 1, 0)
  age_10_15_2 <- ifelse(age_2 >= 10 & age_2 <= 15, 1, 0)
  age_16_more <- ifelse (age_2 > 15, 1, 0)
DateCapture_s_2 <- scale_ext(data_model_2$day_number, DateCapture)
Size_s_2 <- scale_ext(data_model_2$s_length, Size)
Size_s2_2 <- Size_s_2^2
DateBreakUp_t_1_s_2 <- scale_ext(data_model_2$DateBreakUp_t_1, DateBreakUp_t_1)
WinterAO_s_2 <- scale_ext(data_model_2$winter_AO, WinterAO)
PriorWinterAO_s_2 <- scale_ext(data_model_2$prior_winter_AO, PriorWinterAO)
PriorSpringAO_s_2 <- scale_ext(data_model_2$prior_spring_AO, PriorSpringAO)

# Yearly random effect
df_years_2 <- data.frame(year = unique(data_model_2$year),
                       index_year = seq(1:length(unique(data_model_2$year))))
year_2 <- data_model_2$year
for (k in 1:nrow(df_years_2)) {
  year_2[which(year_2 == df_years_2$year[k])] <- df_years_2$index_year[k]
}
nbyear_2 <- length(unique(year_2))

}


# Create a list with all the constants required to run the nimble model
my.constants <- list(
  # For the litter production part
  N_1 = length(y_1),            # nb of females captured with or without cubs
                     year_1 = year_1,
                     nbyear_1 = nbyear_1,
                     age_5_9_1 = age_5_9_1,
                     age_10_15_1 = age_10_15_1, 
                     age_16_20 = age_16_20, 
                     age_21_more = age_21_more,
                     DateCapture_s_1 = DateCapture_s_1,
                     Size_s_1 = Size_s_1,
                     Size_s2_1 = Size_s2_1,
                     DateBreakUp_t_1_s_1 = DateBreakUp_t_1_s_1,
                     WinterAO_s_1 = WinterAO_s_1,
                     PriorWinterAO_s_1 = PriorWinterAO_s_1,
                     PriorSpringAO_s_1 = PriorSpringAO_s_1,
                     numVars_1 = numVars_1,
   # For the litter Size part
                     N_2 = length(y_2),            
                     year_2 = year_2, 
                     nbyear_2 = nbyear_2,
                     age_5_9_2 = age_5_9_2,
                     age_10_15_2 = age_10_15_2, 
                     age_16_more = age_16_more,
                     DateCapture_s_2 = DateCapture_s_2,
                     Size_s_2 = Size_s_2,
                     Size_s2_2 = Size_s2_2,
                     DateBreakUp_t_1_s_2 = DateBreakUp_t_1_s_2,
                     WinterAO_s_2 = WinterAO_s_2,
                     PriorWinterAO_s_2 = PriorWinterAO_s_2,
                     PriorSpringAO_s_2 = PriorSpringAO_s_2,
                     numVars_2 = numVars_2) 

# Create a list with all the resposne variables
dat <- list(y_1 = y_1,
            y_2 = y_2)


# Specify the parameters to monitor
params <- c("beta",    # The coefficients of the regression of litter production
            "sigma1",  # The standard deviation of the yearly random effect for the regression of litter production
            "eps1",    
            "gamma",   # The coefficients of the regression of litter size 
            "sigma2",  # The standard deviation of the yearly random effect for the regression of litter size
            "eps2")    

# generate a list with the initial values drawn in uniform distributions
inits <- function() list(beta = runif(n = numVars_1, min = -1, max = 1), 
                         sigma1 = runif(1),
                         gamma = runif(n = numVars_2, min = -1, max = 1),
                         sigma2 = runif(1))

#' 
#' 
#' Let's run the nimble model
#' 
set.seed(2022)
fit_break_up <-  nimbleMCMC(code = model_break_up,
                                 data = dat,
                                 constants = my.constants,
                                 inits = inits,
                                 monitors = params,
                                 thin = 10,
                                 niter = 135000,
                                 nburnin = 10000,
                                 nchains = 2,
                                 summary = TRUE,
                                 WAIC = TRUE)

save(fit_break_up,
     file = "data/fit_break_up.RData")

#' 
#' Let's visually inspect the MCMC

load("data/fit_break_up.RData")

numVars_1 <- 32 ; numVars_2 <- 24
N <- dim(fit_break_up$samples$chain1)[1]
C <- dim(fit_break_up$samples$chain1)[2]
res_temp <- rbind(fit_break_up$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest <- c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                      grep("sigma", colnames(res_temp)))
res <- res_temp[, cols_of_interest]

# Plots
params.plot <- c()
for (k in 1:numVars_1) {
  params.plot <- c(params.plot, paste0("beta.", k, "."))
}
params.plot <- c(params.plot, "sigma1")
for (k in 1:numVars_2) {
  params.plot <- c(params.plot, paste0("gamma.", k, "."))
}
params.plot <- c(params.plot, "sigma2")

# Let's make two separate figures as there are a lot of parameters
diagnostic_plot_1 <- check_convergence_several_predictors(params.plot = params.plot[1:33],
                                                          nimble_output = fit_break_up)
nrows = length(params.plot[1:33])
save_plot(filename = "dianostic_fit_break_up_1st_part.png",
          plot = diagnostic_plot_1,
          ncol = 3, nrow = 1,
          base_width = 3, base_height = nrows * 1.1, limitsize = FALSE)

diagnostic_plot_2 <- check_convergence_several_predictors(params.plot = params.plot[34:58],
                                                          nimble_output = fit_break_up)
nrows = length(params.plot[34:58])
save_plot(filename = "dianostic_fit_break_up_2nd_part.png",
          plot = diagnostic_plot_2,
          ncol = 3, nrow = 1,
          base_width = 3, base_height = nrows * 1.1, limitsize = FALSE)

#' 
#' Let's check the Rhat for each parameter.
summary <- MCMCvis::MCMCsummary(fit_break_up$samples)
Rhats <- data.frame(rownames(summary), summary$Rhat)

sum(Rhats$summary.Rhat <1.1) - nrow(Rhats) # If this returns 0, then convergence is satisfactory for all parameters

#' 
#' Let's extract the output of the model, plot the caterpilar plots to visualize the results and see which covariates are significant
#' 

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
                    grep("sigma", colnames(res_temp)))] ; rm(res_temp)
cols <- colnames(res)[c(grep("beta", colnames(res)), grep("gamma", colnames(res)))]

caterpillar <- as.data.frame(res) %>%
  dplyr::select(-sigma1, -sigma2) %>%
  pivot_longer(cols = all_of(cols)) %>%
  left_join(x = .,
            y = model_full_df,
            by = "name") %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr", "\u2265 16 yr")))

caterpillar_1 <- caterpillar %>% 
  filter(event == "litter production")
caterpillar_2 <- caterpillar %>% 
  filter(event == "litter size")

#' 
#' ### Individual metrics
plot_1 <- ggplot(data = caterpillar_1[caterpillar_1$type == "individual", ], 
       aes(x = name , y = value, color = age)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(limits = rev(caterpillar_1 %>% filter(type == "individual") %>% distinct(name) %>% pull(name)),
                   labels = rev(caterpillar_1 %>% filter(type == "individual") %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "",
       subtitle = paste0("wAIC = ", round(fit_break_up$WAIC$WAIC, 2))) +
  facet_wrap(.~event) +
  coord_flip()

plot_2 <- ggplot(data = caterpillar_2[caterpillar_2$type == "individual", ], 
       aes(x = name , y = value, color = age)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(limits = rev(caterpillar_2 %>% filter(type == "individual") %>% distinct(name) %>% pull(name)),
                   labels = rev(caterpillar_2 %>% filter(type == "individual") %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "",
       subtitle = paste0("wAIC = ", round(fit_break_up$WAIC$WAIC, 2))) +
  facet_wrap(.~event) +
  coord_flip()

plot_1 + plot_2 +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

#' Note the wAIC of the full model reported on the top left of the graph.
#' 
#' ### Sea-ice covariates
plot_1 <- ggplot(data = caterpillar_1[caterpillar_1$type == "sea-ice", ], 
       aes(x = name , y = value, color = age)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(limits = rev(caterpillar_1 %>% filter(type == "sea-ice") %>% distinct(name) %>% pull(name)),
                   labels = rev(caterpillar_1 %>% filter(type == "sea-ice") %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "") +
  facet_wrap(.~event) +
  coord_flip()

plot_2 <- ggplot(data = caterpillar_2[caterpillar_2$type == "sea-ice", ], 
       aes(x = name , y = value, color = age)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(limits = rev(caterpillar_2 %>% filter(type == "sea-ice") %>% distinct(name) %>% pull(name)),
                   labels = rev(caterpillar_2 %>% filter(type == "sea-ice") %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "") +
  facet_wrap(.~event) +
  coord_flip()

plot_1 + plot_2 +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')

#' 
#' ### AO covariates
plot_1 <- ggplot(data = caterpillar_1[caterpillar_1$type == "AO", ], 
       aes(x = name , y = value, color = age)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(limits = rev(caterpillar_1 %>% filter(type == "AO") %>% distinct(name) %>% pull(name)),
                   labels = rev(caterpillar_1 %>% filter(type == "AO") %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "") +
  facet_wrap(.~event) +
  coord_flip()

plot_2 <- ggplot(data = caterpillar_2[caterpillar_2$type == "AO", ], 
       aes(x = name , y = value, color = age)) + 
  geom_hline(yintercept = 0, color = "black") +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.025, upper = 0.975),
               geom = "pointrange", size = 0.5) +
  stat_summary(fun.data = get_mean_and_CI,
               fun.args = list(lower = 0.055, upper = 0.945),
               geom = "pointrange", size = 1.1) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_discrete(limits = rev(caterpillar_2 %>% filter(type == "AO") %>% distinct(name) %>% pull(name)),
                   labels = rev(caterpillar_2 %>% filter(type == "AO") %>% distinct(age, covariate) %>% pull(covariate))) +
  labs(x = "", y = "") +
  facet_wrap(.~event) +
  coord_flip()

plot_1 + plot_2 +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom')




#' Covariates are retained only if their 89% credible interval does not overlap with 0. Age-specific intercepts are retained anyway. 
#' 
#' The tables below summarizes the significance of each age-specific effect (* : the 89% CI does not overlap with 0, and ** : the 95% CI does not overlap with 0).
#' 
#' For the regression of breeding probability: 
significance_1 <- rep("", times = numVars_1)
for (k in 1:numVars_1) {
    if (!between(0, left = quantile(res[, k], probs = 0.055), right = quantile(res[, k], probs = 0.945))) {
      significance_1[k] <- "*"
      if (!between(0, left = quantile(res[, k], probs = 0.025), right = quantile(res[, k], probs = 0.975))) {
        significance_1[k] <- "**"
      }
    }
  }

(df_significance_1 <- data.frame(parameter = rep(c("intercept", "DateCapture", "Size", "Size2",
                                                  "DateBreakUp",
                                                  "WinterAO", "PriorWinterAO", "PriorSpringAO"), times = 4),
                                age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = 8),
                                significance = significance_1) %>%
  pivot_wider(names_from = age, values_from = significance) %>%
  filter(parameter != "intercept"))

#' 
#' For the regression of litter size: 
res_2 <- res[, 33:56]
significance_2 <- rep("", times = numVars_2)
for (k in 1:numVars_2) {
    if (!between(0, left = quantile(res_2[, k], probs = 0.055), right = quantile(res_2[, k], probs = 0.945))) {
      significance_2[k] <- "*"
      if (!between(0, left = quantile(res_2[, k], probs = 0.025), right = quantile(res_2[, k], probs = 0.975))) {
        significance_2[k] <- "**"
      }
    }
  }

(df_significance_2 <- data.frame(parameter = rep(c("intercept", "DateCapture", "Size", "Size2",
                                                  "DateBreakUp",
                                                  "WinterAO", "PriorWinterAO", "PriorSpringAO"), times = 3),
                                age = rep(c("5-9 yr", "10-15 yr", "\u2265 16 yr"), each = 8),
                                significance = significance_2) %>%
  pivot_wider(names_from = age, values_from = significance) %>%
  filter(parameter != "intercept"))

#' 
#' 
#' ## 2. Final model 
#' 
#' Let's build and run the final model, in which only the intercept and the covariates that were signficiant in the full model are included. 
model_break_up_final <- nimbleCode({
  # Logistic regression for litter production
  for (i in 1:numVars_1) {
    beta[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_1) {
    y_1[i] ~ dbern(p_1[i])
    logit(p_1[i]) <-
      beta[1] * age_5_9_1[i] +
      beta[2] * age_5_9_1[i] * DateCapture_s_1[i] +
      
      beta[3] * age_10_15_1[i] +
      beta[4] * age_10_15_1[i] * Size_s_1[i] +
      beta[5] * age_10_15_1[i] * Size_s2_1[i] +
      beta[6] * age_10_15_1[i] * WinterAO_s_1[i] +
      beta[7] * age_10_15_1[i] * PriorSpringAO_s_1[i] +
      
      beta[8] * age_16_20[i] +
      beta[9] * age_16_20[i] * DateCapture_s_1[i] +
      beta[10] * age_16_20[i] * WinterAO_s_1[i] +
      beta[11] * age_16_20[i] * PriorSpringAO_s_1[i] +
      
      beta[12] * age_21_more[i] +
      beta[13] * age_21_more[i] * DateCapture_s_1[i] +
      
      eps1[year_1[i]]
  }
  for (i in 1:nbyear_1) {
    eps1[i] ~ dnorm(0, sd = sigma1)
  }
  sigma1 ~ dunif(0, 10)
  # Logistic regression for litter size
  for (i in 1:numVars_2) {
    gamma[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_2) {
    y_2[i] ~ dbern(p_2[i])
    logit(p_2[i]) <-
      gamma[1] * age_5_9_2[i] +
      gamma[2] * age_5_9_2[i] * DateCapture_s_2[i] +
      gamma[3] * age_5_9_2[i] * PriorWinterAO_s_2[i] +

      gamma[4] * age_10_15_2[i] +
      gamma[5] * age_10_15_2[i] * DateBreakUp_t_1_s_2[i] +

      gamma[6] * age_16_more[i] +
    
      eps2[year_2[i]]
  }
  for (i in 1:nbyear_2) {
    eps2[i] ~ dnorm(0, sd = sigma2)
  }
  sigma2 ~ dunif(0, 10)
})

#' 
# Logistic regression for litter production ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable 
y_1 <- data_model_1$cub_number                         # Extract the response variable
y_1[y_1 == 0] <- 0 ; y_1[y_1 %in% c(1, 2, 3)] <- 1     # Success = 1, 2 or 3 cubs of the year ; failure = 0 cubs

# Covariates
numVars_1 <- 13
age_1 <- as.numeric(data_model_1$age)
age_5_9_1 <- ifelse (age_1 < 10, 1, 0)
age_10_15_1 <- ifelse(age_1 >= 10 & age_1 <= 15, 1, 0)
age_16_20 <- ifelse(age_1 > 15 & age_1 <= 20, 1, 0)
age_21_more <- ifelse (age_1 > 20, 1, 0)

DateCapture_s_1 <- scale_ext(data_model_1$day_number, DateCapture)
Size_s_1 <- scale_ext(data_model_1$s_length, Size)
Size_s2_1 <- Size_s_1^2
WinterAO_s_1 <- scale_ext(data_model_1$winter_AO, WinterAO)
PriorSpringAO_s_1 <- scale_ext(data_model_1$prior_spring_AO, PriorSpringAO)

# Yearly random effect
df_years_1 <- data.frame(year = unique(data_model_1$year),
                       index_year = seq(1:length(unique(data_model_1$year))))
year_1 <- data_model_1$year
for (k in 1:nrow(df_years_1)) {
  year_1[which(year_1 == df_years_1$year[k])] <- df_years_1$index_year[k]
}
nbyear_1 <- length(unique(year_1))

}

# Logistic regression for litter size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable
  y_2 <- data_model_2$cub_number                  # Extract the response variable
y_2[y_2 == 1] <- 0 ; y_2[y_2 %in% c(2, 3)] <- 1 # Success = 2 or 3 cubs-of-the-year ; failure = 1 cubs-of-the-year 

# Covariates
  numVars_2 <- 6
  age_2 <- as.numeric(data_model_2$age)
  age_5_9_2 <- ifelse (age_2 < 10, 1, 0)
  age_10_15_2 <- ifelse(age_2 >= 10 & age_2 <= 15, 1, 0)
  age_16_more <- ifelse (age_2 > 15, 1, 0)
DateCapture_s_2 <- scale_ext(data_model_2$day_number, DateCapture)
DateBreakUp_t_1_s_2 <- scale_ext(data_model_2$DateBreakUp_t_1, DateBreakUp_t_1)
PriorWinterAO_s_2 <- scale_ext(data_model_2$prior_winter_AO, PriorWinterAO)

# Yearly random effect
df_years_2 <- data.frame(year = unique(data_model_2$year),
                       index_year = seq(1:length(unique(data_model_2$year))))
year_2 <- data_model_2$year
for (k in 1:nrow(df_years_2)) {
  year_2[which(year_2 == df_years_2$year[k])] <- df_years_2$index_year[k]
}
nbyear_2 <- length(unique(year_2))

}

# Create a list with all the constants required to run the nimble model
my.constants <- list(
  # For the litter production part
                     N_1 = length(y_1),            # nb of females in the dataset
                     year_1 = year_1,
                     nbyear_1 = nbyear_1,
                     age_5_9_1 = age_5_9_1,
                     age_10_15_1 = age_10_15_1, 
                     age_16_20 = age_16_20, 
                     age_21_more = age_21_more,
                     DateCapture_s_1 = DateCapture_s_1,
                     Size_s_1 = Size_s_1,
                     Size_s2_1 = Size_s2_1,
                     WinterAO_s_1 = WinterAO_s_1,
                     PriorSpringAO_s_1 = PriorSpringAO_s_1,
                     numVars_1 = numVars_1,
   # For the litter size part
                     N_2 = length(y_2),            
                     year_2 = year_2, 
                     nbyear_2 = nbyear_2,
                     age_10_15_2 = age_10_15_2, 
                     age_5_9_2 = age_5_9_2,
                     age_16_more = age_16_more,
                     DateCapture_s_2 = DateCapture_s_2,
                     DateBreakUp_t_1_s_2 = DateBreakUp_t_1_s_2,
                     PriorWinterAO_s_2 = PriorWinterAO_s_2,
                     numVars_2 = numVars_2) 

# Create a list with all the response variables
dat <- list(y_1 = y_1,
            y_2 = y_2)

# Specify the parameters to monitor
params <- c("beta", "sigma1", "eps1", "gamma", "sigma2", "eps2")

# generate a list with the initial values drawn in uniform distributions
inits <- function() list(beta = runif(n = numVars_1, min = -1, max = 1), 
                         sigma1 = runif(1),
                         gamma = runif(n = numVars_2, min = -1, max = 1),
                         sigma2 = runif(1))

#' 
#' Run the model

set.seed(2022)
fit_break_up_final <-  nimbleMCMC(code = model_break_up_final,
                                                             data = dat,
                                                             constants = my.constants,
                                                             inits = inits,
                                                             monitors = params,
                                                             thin = 10,
                                                             niter = 135000,
                                                             nburnin = 10000,
                                                             nchains = 2,
                                                             summary = TRUE,
                                                             WAIC = TRUE)

save(fit_break_up_final,
     file = "data/fit_break_up_final.RData")

#' 
#' Let's visually inspect the MCMC 
#' 

load("data/fit_break_up_final.RData")

numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest <- c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                      grep("sigma", colnames(res_temp)))
res <- res_temp[, cols_of_interest]

# Parameters to plot
params.plot <- c()
for (k in 1:numVars_1) {
  params.plot <- c(params.plot, paste0("beta.", k, "."))
}
params.plot <- c(params.plot, "sigma1")
for (k in 1:numVars_2) {
  params.plot <- c(params.plot, paste0("gamma.", k, "."))
}
params.plot <- c(params.plot, "sigma2")


# Plot
diagnostic_plot <- check_convergence_several_predictors(params.plot = params.plot,
                                                        nimble_output = fit_break_up_final)

#' 
#' Let's check the Rhat for each parameter.
#' 
summary <- MCMCvis::MCMCsummary(fit_break_up_final$samples)
Rhats <- data.frame(rownames(summary), summary$Rhat)

sum(Rhats$summary.Rhat <1.1) - nrow(Rhats) # If this returns 0, then convergence is satisfactory for all parameters


#' ## 3. Generate predictions
#' 
#' ### Effect of *DateCapture*
#' 
load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest_1 <- grep("beta", colnames(res_temp))
cols_of_interest_2 <- grep("gamma", colnames(res_temp))


range <- range(DateCapture_s)
lengthgrid <- 100
grid_scaled <- seq(from = range[1], to = range[2], length = lengthgrid)
grid <- unscale(grid_scaled, DateCapture)

# Regression of litter production
res_1 <- res_temp[, cols_of_interest_1]
p_litter_y <- p_litter_p <- p_litter_o1 <- p_litter_o2 <- matrix(data = NA, nrow = dim(res_1)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_1)[1]) {
    p_litter_y[j, i] <- plogis(res_1[j, 1] +
                              res_1[j, 2] * grid_scaled[i])
    p_litter_p[j, i] <- plogis(res_1[j, 3])
    p_litter_o1[j, i] <- plogis(res_1[j, 8] +
                             res_1[j, 9] * grid_scaled[i])
    p_litter_o2[j, i] <- plogis(res_1[j, 12] +
                             res_1[j, 13] * grid_scaled[i])
  }
} ; end <- Sys.time() ; end - start

# Regression of litter size
res_2 <- res_temp[, cols_of_interest_2]
p_23_cub_5_9 <- p_23_cub_10_15 <- p_23_cub_16_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_23_cub_5_9[j, i] <- plogis(res_2[j, 1] +
                              res_2[j, 2] * grid_scaled[i])
    p_23_cub_10_15[j, i] <- plogis(res_2[j, 4])

    p_23_cub_16_more[j, i] <- plogis(res_2[j, 6])
  }
} ; end <- Sys.time() ; end - start


# Multiply the probabilities ---------------------------------------------------

p_litter_23_cub_5_9 <- p_litter_23_cub_10_15 <- p_litter_23_cub_16_20 <- p_litter_23_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
p_litter_1_cub_5_9 <- p_litter_1_cub_10_15 <- p_litter_1_cub_16_20 <- p_litter_1_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_litter_23_cub_5_9[j, i] <- p_litter_y[j, i] * p_23_cub_5_9[j, i]
    p_litter_23_cub_10_15[j, i] <- p_litter_p[j, i] * p_23_cub_10_15[j, i]
    p_litter_23_cub_16_20[j, i] <- p_litter_o1[j, i] * p_23_cub_16_more[j, i]
    p_litter_23_cub_21_more[j, i] <- p_litter_o2[j, i] * p_23_cub_16_more[j, i]

    p_litter_1_cub_5_9[j, i] <- p_litter_y[j, i] * (1 - p_23_cub_5_9[j, i])
    p_litter_1_cub_10_15[j, i] <- p_litter_p[j, i] * (1 - p_23_cub_10_15[j, i])
    p_litter_1_cub_16_20[j, i] <- p_litter_o1[j, i] * (1 - p_23_cub_16_more[j, i])
    p_litter_1_cub_21_more[j, i] <- p_litter_o2[j, i] * (1 - p_23_cub_16_more[j, i])
  }
} ; end <- Sys.time() ; end - start

df_plot <- rbind(data.frame(var =  rep(grid, times = 4),
                            event = "no litter",
                            mean = c(apply((1 - p_litter_y), 2, mean),
                                     apply((1 - p_litter_p), 2, mean),
                                     apply((1 - p_litter_o1), 2, mean),
                                     apply((1 - p_litter_o2), 2, mean)),
                            ci_2.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_p), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o1), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o2), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_p), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o1), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o2), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "1 cub",
                            mean = c(apply((p_litter_1_cub_5_9), 2, mean),
                                     apply((p_litter_1_cub_10_15), 2, mean),
                                     apply((p_litter_1_cub_16_20), 2, mean),
                                     apply((p_litter_1_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "2-3 cubs",
                            mean = c(apply((p_litter_23_cub_5_9), 2, mean),
                                     apply((p_litter_23_cub_10_15), 2, mean),
                                     apply((p_litter_23_cub_16_20), 2, mean),
                                     apply((p_litter_23_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid)))) %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")))

write_csv(df_plot, "data/effect_DateCapture.csv")

#' 
#' ### Effect of *Size*
#' 

load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest_1 <- grep("beta", colnames(res_temp))
cols_of_interest_2 <- grep("gamma", colnames(res_temp))


range <- range(Size_s)
lengthgrid <- 100
grid_scaled <- seq(from = range[1], to = range[2], length = lengthgrid)
grid <- unscale(grid_scaled, Size)

# Regression of litter production
res_1 <- res_temp[, cols_of_interest_1]
p_litter_y <- p_litter_p <- p_litter_o1 <- p_litter_o2 <- matrix(data = NA, nrow = dim(res_1)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_1)[1]) {
    p_litter_y[j, i] <- plogis(res_1[j, 1])
    p_litter_p[j, i] <- plogis(res_1[j, 3] +
                                res_1[j, 4] * grid_scaled[i] +
                                res_1[j, 5] * grid_scaled[i]^2)
    p_litter_o1[j, i] <- plogis(res_1[j, 8])
    p_litter_o2[j, i] <- plogis(res_1[j, 12])
  }
} ; end <- Sys.time() ; end - start


# Regression of litter size
res_2 <- res_temp[, cols_of_interest_2]
p_23_cub_5_9 <- p_23_cub_10_15 <- p_23_cub_16_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_23_cub_5_9[j, i] <- plogis(res_2[j, 1])
    p_23_cub_10_15[j, i] <- plogis(res_2[j, 4])

    p_23_cub_16_more[j, i] <- plogis(res_2[j, 6])
  }
} ; end <- Sys.time() ; end - start


# Multiply the probabilities ---------------------------------------------------
p_litter_23_cub_5_9 <- p_litter_23_cub_10_15 <- p_litter_23_cub_16_20 <- p_litter_23_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
p_litter_1_cub_5_9 <- p_litter_1_cub_10_15 <- p_litter_1_cub_16_20 <- p_litter_1_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_litter_23_cub_5_9[j, i] <- p_litter_y[j, i] * p_23_cub_5_9[j, i]
    p_litter_23_cub_10_15[j, i] <- p_litter_p[j, i] * p_23_cub_10_15[j, i]
    p_litter_23_cub_16_20[j, i] <- p_litter_o1[j, i] * p_23_cub_16_more[j, i]
    p_litter_23_cub_21_more[j, i] <- p_litter_o2[j, i] * p_23_cub_16_more[j, i]

    p_litter_1_cub_5_9[j, i] <- p_litter_y[j, i] * (1 - p_23_cub_5_9[j, i])
    p_litter_1_cub_10_15[j, i] <- p_litter_p[j, i] * (1 - p_23_cub_10_15[j, i])
    p_litter_1_cub_16_20[j, i] <- p_litter_o1[j, i] * (1 - p_23_cub_16_more[j, i])
    p_litter_1_cub_21_more[j, i] <- p_litter_o2[j, i] * (1 - p_23_cub_16_more[j, i])
  }
} ; end <- Sys.time() ; end - start

df_plot <- rbind(data.frame(var =  rep(grid, times = 4),
                            event = "no litter",
                            mean = c(apply((1 - p_litter_y), 2, mean),
                                     apply((1 - p_litter_p), 2, mean),
                                     apply((1 - p_litter_o1), 2, mean),
                                     apply((1 - p_litter_o2), 2, mean)),
                            ci_2.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_p), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o1), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o2), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_p), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o1), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o2), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "1 cub",
                            mean = c(apply((p_litter_1_cub_5_9), 2, mean),
                                     apply((p_litter_1_cub_10_15), 2, mean),
                                     apply((p_litter_1_cub_16_20), 2, mean),
                                     apply((p_litter_1_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "2-3 cubs",
                            mean = c(apply((p_litter_23_cub_5_9), 2, mean),
                                     apply((p_litter_23_cub_10_15), 2, mean),
                                     apply((p_litter_23_cub_16_20), 2, mean),
                                     apply((p_litter_23_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid)))) %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")))

write_csv(df_plot, "data/effect_Size.csv")

#' 
#' ### Effect of *DateBreakUp*
#' 

load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest_1 <- grep("beta", colnames(res_temp))
cols_of_interest_2 <- grep("gamma", colnames(res_temp))


range <- range(DateBreakUp_t_1_s)
lengthgrid <- 100
grid_scaled <- seq(from = range[1], to = range[2], length = lengthgrid)
grid <- unscale(grid_scaled, DateBreakUp_t_1)

# Regression of litter production
res_1 <- res_temp[, cols_of_interest_1]
p_litter_y <- p_litter_p <- p_litter_o1 <- p_litter_o2 <- matrix(data = NA, nrow = dim(res_1)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_1)[1]) {
    p_litter_y[j, i] <- plogis(res_1[j, 1])
    p_litter_p[j, i] <- plogis(res_1[j, 3])
    p_litter_o1[j, i] <- plogis(res_1[j, 8])
    p_litter_o2[j, i] <- plogis(res_1[j, 12])
  }
} ; end <- Sys.time() ; end - start

# Regression of litter size
res_2 <- res_temp[, cols_of_interest_2]
p_23_cub_5_9 <- p_23_cub_10_15 <- p_23_cub_16_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_23_cub_5_9[j, i] <- plogis(res_2[j, 1])
    p_23_cub_10_15[j, i] <- plogis(res_2[j, 4] +
                                 res_2[j, 5] * grid_scaled[i])
    p_23_cub_16_more[j, i] <- plogis(res_2[j, 6])
  }
} ; end <- Sys.time() ; end - start


# Multiply the probabilities ---------------------------------------------------
p_litter_23_cub_5_9 <- p_litter_23_cub_10_15 <- p_litter_23_cub_16_20 <- p_litter_23_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
p_litter_1_cub_5_9 <- p_litter_1_cub_10_15 <- p_litter_1_cub_16_20 <- p_litter_1_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_litter_23_cub_5_9[j, i] <- p_litter_y[j, i] * p_23_cub_5_9[j, i]
    p_litter_23_cub_10_15[j, i] <- p_litter_p[j, i] * p_23_cub_10_15[j, i]
    p_litter_23_cub_16_20[j, i] <- p_litter_o1[j, i] * p_23_cub_16_more[j, i]
    p_litter_23_cub_21_more[j, i] <- p_litter_o2[j, i] * p_23_cub_16_more[j, i]

    p_litter_1_cub_5_9[j, i] <- p_litter_y[j, i] * (1 - p_23_cub_5_9[j, i])
    p_litter_1_cub_10_15[j, i] <- p_litter_p[j, i] * (1 - p_23_cub_10_15[j, i])
    p_litter_1_cub_16_20[j, i] <- p_litter_o1[j, i] * (1 - p_23_cub_16_more[j, i])
    p_litter_1_cub_21_more[j, i] <- p_litter_o2[j, i] * (1 - p_23_cub_16_more[j, i])
  }
} ; end <- Sys.time() ; end - start

df_plot <- rbind(data.frame(var =  rep(grid, times = 4),
                            event = "no litter",
                            mean = c(apply((1 - p_litter_y), 2, mean),
                                     apply((1 - p_litter_p), 2, mean),
                                     apply((1 - p_litter_o1), 2, mean),
                                     apply((1 - p_litter_o2), 2, mean)),
                            ci_2.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_p), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o1), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o2), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_p), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o1), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o2), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "1 cub",
                            mean = c(apply((p_litter_1_cub_5_9), 2, mean),
                                     apply((p_litter_1_cub_10_15), 2, mean),
                                     apply((p_litter_1_cub_16_20), 2, mean),
                                     apply((p_litter_1_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "2-3 cubs",
                            mean = c(apply((p_litter_23_cub_5_9), 2, mean),
                                     apply((p_litter_23_cub_10_15), 2, mean),
                                     apply((p_litter_23_cub_16_20), 2, mean),
                                     apply((p_litter_23_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid)))) %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")))

write_csv(df_plot, "data/effect_DateBreakUp.csv")

#' 
#' ### Effect of *WinterAO*
#' 

load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest_1 <- grep("beta", colnames(res_temp))
cols_of_interest_2 <- grep("gamma", colnames(res_temp))

range <- range(WinterAO_s)
lengthgrid <- 100
grid_scaled <- seq(from = range[1], to = range[2], length = lengthgrid)
grid <- unscale(grid_scaled, WinterAO)

# Regression of litter production
res_1 <- res_temp[, cols_of_interest_1]
p_litter_y <- p_litter_p <- p_litter_o1 <- p_litter_o2 <- matrix(data = NA, nrow = dim(res_1)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_1)[1]) {
    p_litter_y[j, i] <- plogis(res_1[j, 1])
    p_litter_p[j, i] <- plogis(res_1[j, 3] +
                                 res_1[j, 6] * grid_scaled[i])
    p_litter_o1[j, i] <- plogis(res_1[j, 8] +
                                  res_1[j, 10] * grid_scaled[i])
    p_litter_o2[j, i] <- plogis(res_1[j, 12])
  }
} ; end <- Sys.time() ; end - start

# Regression of litter size
res_2 <- res_temp[, cols_of_interest_2]
p_23_cub_5_9 <- p_23_cub_10_15 <- p_23_cub_16_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_23_cub_5_9[j, i] <- plogis(res_2[j, 1])
    p_23_cub_10_15[j, i] <- plogis(res_2[j, 4])
    p_23_cub_16_more[j, i] <- plogis(res_2[j, 6])
  }
} ; end <- Sys.time() ; end - start


# Multiply the probabilities ---------------------------------------------------
p_litter_23_cub_5_9 <- p_litter_23_cub_10_15 <- p_litter_23_cub_16_20 <- p_litter_23_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
p_litter_1_cub_5_9 <- p_litter_1_cub_10_15 <- p_litter_1_cub_16_20 <- p_litter_1_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_litter_23_cub_5_9[j, i] <- p_litter_y[j, i] * p_23_cub_5_9[j, i]
    p_litter_23_cub_10_15[j, i] <- p_litter_p[j, i] * p_23_cub_10_15[j, i]
    p_litter_23_cub_16_20[j, i] <- p_litter_o1[j, i] * p_23_cub_16_more[j, i]
    p_litter_23_cub_21_more[j, i] <- p_litter_o2[j, i] * p_23_cub_16_more[j, i]

    p_litter_1_cub_5_9[j, i] <- p_litter_y[j, i] * (1 - p_23_cub_5_9[j, i])
    p_litter_1_cub_10_15[j, i] <- p_litter_p[j, i] * (1 - p_23_cub_10_15[j, i])
    p_litter_1_cub_16_20[j, i] <- p_litter_o1[j, i] * (1 - p_23_cub_16_more[j, i])
    p_litter_1_cub_21_more[j, i] <- p_litter_o2[j, i] * (1 - p_23_cub_16_more[j, i])
  }
} ; end <- Sys.time() ; end - start

df_plot <- rbind(data.frame(var =  rep(grid, times = 4),
                            event = "no litter",
                            mean = c(apply((1 - p_litter_y), 2, mean),
                                     apply((1 - p_litter_p), 2, mean),
                                     apply((1 - p_litter_o1), 2, mean),
                                     apply((1 - p_litter_o2), 2, mean)),
                            ci_2.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_p), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o1), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o2), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_p), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o1), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o2), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "1 cub",
                            mean = c(apply((p_litter_1_cub_5_9), 2, mean),
                                     apply((p_litter_1_cub_10_15), 2, mean),
                                     apply((p_litter_1_cub_16_20), 2, mean),
                                     apply((p_litter_1_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "2-3 cubs",
                            mean = c(apply((p_litter_23_cub_5_9), 2, mean),
                                     apply((p_litter_23_cub_10_15), 2, mean),
                                     apply((p_litter_23_cub_16_20), 2, mean),
                                     apply((p_litter_23_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid)))) %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")))

write_csv(df_plot, "data/effect_WinterAO.csv")


#' 
#' 
#' ### Effect of *PriorWinterAO*
#' 

load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest_1 <- grep("beta", colnames(res_temp))
cols_of_interest_2 <- grep("gamma", colnames(res_temp))


range <- range(PriorWinterAO_s)
lengthgrid <- 100
grid_scaled <- seq(from = range[1], to = range[2], length = lengthgrid)
grid <- unscale(grid_scaled, PriorWinterAO)

# Regression of litter production
res_1 <- res_temp[, cols_of_interest_1]
p_litter_y <- p_litter_p <- p_litter_o1 <- p_litter_o2 <- matrix(data = NA, nrow = dim(res_1)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_1)[1]) {
    p_litter_y[j, i] <- plogis(res_1[j, 1])
    p_litter_p[j, i] <- plogis(res_1[j, 3])
    p_litter_o1[j, i] <- plogis(res_1[j, 8])
    p_litter_o2[j, i] <- plogis(res_1[j, 12])
  }
} ; end <- Sys.time() ; end - start

# Regression of litter size
res_2 <- res_temp[, cols_of_interest_2]
p_23_cub_5_9 <- p_23_cub_10_15 <- p_23_cub_16_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_23_cub_5_9[j, i] <- plogis(res_2[j, 1] +
                                 res_2[j, 3] * grid_scaled[i])
    p_23_cub_10_15[j, i] <- plogis(res_2[j, 4])
    p_23_cub_16_more[j, i] <- plogis(res_2[j, 6])
  }
} ; end <- Sys.time() ; end - start


# Multiply the probabilities ---------------------------------------------------
p_litter_23_cub_5_9 <- p_litter_23_cub_10_15 <- p_litter_23_cub_16_20 <- p_litter_23_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
p_litter_1_cub_5_9 <- p_litter_1_cub_10_15 <- p_litter_1_cub_16_20 <- p_litter_1_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_litter_23_cub_5_9[j, i] <- p_litter_y[j, i] * p_23_cub_5_9[j, i]
    p_litter_23_cub_10_15[j, i] <- p_litter_p[j, i] * p_23_cub_10_15[j, i]
    p_litter_23_cub_16_20[j, i] <- p_litter_o1[j, i] * p_23_cub_16_more[j, i]
    p_litter_23_cub_21_more[j, i] <- p_litter_o2[j, i] * p_23_cub_16_more[j, i]

    p_litter_1_cub_5_9[j, i] <- p_litter_y[j, i] * (1 - p_23_cub_5_9[j, i])
    p_litter_1_cub_10_15[j, i] <- p_litter_p[j, i] * (1 - p_23_cub_10_15[j, i])
    p_litter_1_cub_16_20[j, i] <- p_litter_o1[j, i] * (1 - p_23_cub_16_more[j, i])
    p_litter_1_cub_21_more[j, i] <- p_litter_o2[j, i] * (1 - p_23_cub_16_more[j, i])
  }
} ; end <- Sys.time() ; end - start

df_plot <- rbind(data.frame(var =  rep(grid, times = 4),
                            event = "no litter",
                            mean = c(apply((1 - p_litter_y), 2, mean),
                                     apply((1 - p_litter_p), 2, mean),
                                     apply((1 - p_litter_o1), 2, mean),
                                     apply((1 - p_litter_o2), 2, mean)),
                            ci_2.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_p), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o1), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o2), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_p), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o1), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o2), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "1 cub",
                            mean = c(apply((p_litter_1_cub_5_9), 2, mean),
                                     apply((p_litter_1_cub_10_15), 2, mean),
                                     apply((p_litter_1_cub_16_20), 2, mean),
                                     apply((p_litter_1_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "2-3 cubs",
                            mean = c(apply((p_litter_23_cub_5_9), 2, mean),
                                     apply((p_litter_23_cub_10_15), 2, mean),
                                     apply((p_litter_23_cub_16_20), 2, mean),
                                     apply((p_litter_23_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid)))) %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")))

write_csv(df_plot, "data/effect_PriorWinterAO.csv")

#' 
#' ### Effect of *PriorSpringAO*
#' 

load("data/fit_break_up_final.RData")
numVars_1 <- 13 ; numVars_2 <- 6
N <- dim(fit_break_up_final$samples$chain1)[1]
C <- dim(fit_break_up_final$samples$chain1)[2]
res_temp <- rbind(fit_break_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_break_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest_1 <- grep("beta", colnames(res_temp))
cols_of_interest_2 <- grep("gamma", colnames(res_temp))

range <- range(PriorSpringAO_s)
lengthgrid <- 100
grid_scaled <- seq(from = range[1], to = range[2], length = lengthgrid)
grid <- unscale(grid_scaled, PriorSpringAO)

# Regression of litter production
res_1 <- res_temp[, cols_of_interest_1]
p_litter_y <- p_litter_p <- p_litter_o1 <- p_litter_o2 <- matrix(data = NA, nrow = dim(res_1)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_1)[1]) {
    p_litter_y[j, i] <- plogis(res_1[j, 1])
    p_litter_p[j, i] <- plogis(res_1[j, 3] +
                                 res_1[j, 7] * grid_scaled[i])
    p_litter_o1[j, i] <- plogis(res_1[j, 8] +
                                  res_1[j, 11] * grid_scaled[i])
    p_litter_o2[j, i] <- plogis(res_1[j, 12])
  }
} ; end <- Sys.time() ; end - start


# Regression of litter size
res_2 <- res_temp[, cols_of_interest_2]
p_23_cub_5_9 <- p_23_cub_10_15 <- p_23_cub_16_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
# Back transform
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_23_cub_5_9[j, i] <- plogis(res_2[j, 1])
    p_23_cub_10_15[j, i] <- plogis(res_2[j, 4])
    p_23_cub_16_more[j, i] <- plogis(res_2[j, 6])
  }
} ; end <- Sys.time() ; end - start


# Multiply the probabilities ---------------------------------------------------
p_litter_23_cub_5_9 <- p_litter_23_cub_10_15 <- p_litter_23_cub_16_20 <- p_litter_23_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
p_litter_1_cub_5_9 <- p_litter_1_cub_10_15 <- p_litter_1_cub_16_20 <- p_litter_1_cub_21_more <- matrix(data = NA, nrow = dim(res_2)[1], ncol = lengthgrid)
start <- Sys.time()
for (i in 1:lengthgrid) {
  for (j in 1:dim(res_2)[1]) {
    p_litter_23_cub_5_9[j, i] <- p_litter_y[j, i] * p_23_cub_5_9[j, i]
    p_litter_23_cub_10_15[j, i] <- p_litter_p[j, i] * p_23_cub_10_15[j, i]
    p_litter_23_cub_16_20[j, i] <- p_litter_o1[j, i] * p_23_cub_16_more[j, i]
    p_litter_23_cub_21_more[j, i] <- p_litter_o2[j, i] * p_23_cub_16_more[j, i]

    p_litter_1_cub_5_9[j, i] <- p_litter_y[j, i] * (1 - p_23_cub_5_9[j, i])
    p_litter_1_cub_10_15[j, i] <- p_litter_p[j, i] * (1 - p_23_cub_10_15[j, i])
    p_litter_1_cub_16_20[j, i] <- p_litter_o1[j, i] * (1 - p_23_cub_16_more[j, i])
    p_litter_1_cub_21_more[j, i] <- p_litter_o2[j, i] * (1 - p_23_cub_16_more[j, i])
  }
} ; end <- Sys.time() ; end - start

df_plot <- rbind(data.frame(var =  rep(grid, times = 4),
                            event = "no litter",
                            mean = c(apply((1 - p_litter_y), 2, mean),
                                     apply((1 - p_litter_p), 2, mean),
                                     apply((1 - p_litter_o1), 2, mean),
                                     apply((1 - p_litter_o2), 2, mean)),
                            ci_2.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_p), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o1), 2, quantile, probs = 0.025),
                                       apply((1 - p_litter_o2), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((1 - p_litter_y), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_p), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o1), 2, quantile, probs = 0.975),
                                        apply((1 - p_litter_o2), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "1 cub",
                            mean = c(apply((p_litter_1_cub_5_9), 2, mean),
                                     apply((p_litter_1_cub_10_15), 2, mean),
                                     apply((p_litter_1_cub_16_20), 2, mean),
                                     apply((p_litter_1_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_1_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_1_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid))),

                 data.frame(var =  rep(grid, times = 4),
                            event = "2-3 cubs",
                            mean = c(apply((p_litter_23_cub_5_9), 2, mean),
                                     apply((p_litter_23_cub_10_15), 2, mean),
                                     apply((p_litter_23_cub_16_20), 2, mean),
                                     apply((p_litter_23_cub_21_more), 2, mean)),
                            ci_2.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.025),
                                       apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.025)),
                            ci_97.5 = c(apply((p_litter_23_cub_5_9), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_10_15), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_16_20), 2, quantile, probs = 0.975),
                                        apply((p_litter_23_cub_21_more), 2, quantile, probs = 0.975)),
                            age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = length(grid)))) %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr")),
         event = factor(event, levels = c("no litter", "1 cub", "2-3 cubs")))

write_csv(df_plot, "data/effect_PriorSpringAO.csv")

#' 
#' 
#' # B. With *DayFreezeUp*
#' 
#' ## 1. Full model
#' 
#' Let's build and run the full model. This model includes *DayFreezeUp* as the sea-ice covariate. The model in part A. includes *DayBreakUp* as the sea-ice covariate.
#' 
model_freeze_up <- nimbleCode({
  # Logistic regression for litter production
  for (i in 1:numVars_1) {
    beta[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_1) {
    y_1[i] ~ dbern(p_1[i])
    logit(p_1[i]) <-
      beta[1] * age_5_9_1[i] +
      beta[2] * age_5_9_1[i] * DateCapture_s_1[i] +
      beta[3] * age_5_9_1[i] * Size_s_1[i] +
      beta[4] * age_5_9_1[i] * Size_s2_1[i] +
      beta[5] * age_5_9_1[i] * DateFreezeUp_t_1_s_1[i] +
      beta[6] * age_5_9_1[i] * WinterAO_s_1[i] +
      beta[7] * age_5_9_1[i] * PriorWinterAO_s_1[i] +
      beta[8] * age_5_9_1[i] * PriorSpringAO_s_1[i] +
      
      beta[9] * age_10_15_1[i] +
      beta[10] * age_10_15_1[i] * DateCapture_s_1[i] +
      beta[11] * age_10_15_1[i] * Size_s_1[i] +
      beta[12] * age_10_15_1[i] * Size_s2_1[i] +
      beta[13] * age_10_15_1[i] * DateFreezeUp_t_1_s_1[i] +
      beta[14] * age_10_15_1[i] * WinterAO_s_1[i] +
      beta[15] * age_10_15_1[i] * PriorWinterAO_s_1[i] +
      beta[16] * age_10_15_1[i] * PriorSpringAO_s_1[i] +
      
      beta[17] * age_16_20[i] +
      beta[18] * age_16_20[i] * DateCapture_s_1[i] +
      beta[19] * age_16_20[i] * Size_s_1[i] +
      beta[20] * age_16_20[i] * Size_s2_1[i] +
      beta[21] * age_16_20[i] * DateFreezeUp_t_1_s_1[i] +
      beta[22] * age_16_20[i] * WinterAO_s_1[i] +
      beta[23] * age_16_20[i] * PriorWinterAO_s_1[i] +
      beta[24] * age_16_20[i] * PriorSpringAO_s_1[i] +
      
      beta[25] * age_21_more[i] +
      beta[26] * age_21_more[i] * DateCapture_s_1[i] +
      beta[27] * age_21_more[i] * Size_s_1[i] +
      beta[28] * age_21_more[i] * Size_s2_1[i] +
      beta[29] * age_21_more[i] * DateFreezeUp_t_1_s_1[i] +
      beta[30] * age_21_more[i] * WinterAO_s_1[i] +
      beta[31] * age_21_more[i] * PriorWinterAO_s_1[i] +
      beta[32] * age_21_more[i] * PriorSpringAO_s_1[i] +
      
      eps1[year_1[i]]
  }
  for (i in 1:nbyear_1) {
    eps1[i] ~ dnorm(0, sd = sigma1)
  }
  sigma1 ~ dunif(0, 10)
  # Logistic regression for litter size
  for (i in 1:numVars_2) {
    gamma[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_2) {
    y_2[i] ~ dbern(p_2[i])
    logit(p_2[i]) <-
      gamma[1] * age_5_9_2[i] +
      gamma[2] * age_5_9_2[i] * DateCapture_s_2[i] +
      gamma[3] * age_5_9_2[i] * Size_s_2[i] +
      gamma[4] * age_5_9_2[i] * Size_s2_2[i] +
      gamma[5] * age_5_9_2[i] * DateFreezeUp_t_1_s_2[i] +
      gamma[6] * age_5_9_2[i] * WinterAO_s_2[i] +
      gamma[7] * age_5_9_2[i] * PriorWinterAO_s_2[i] +
      gamma[8] * age_5_9_2[i] * PriorSpringAO_s_2[i] +
      
      gamma[9] * age_10_15_2[i] +
      gamma[10] * age_10_15_2[i] * DateCapture_s_2[i] +
      gamma[11] * age_10_15_2[i] * Size_s_2[i] +
      gamma[12] * age_10_15_2[i] * Size_s2_2[i] +
      gamma[13] * age_10_15_2[i] * DateFreezeUp_t_1_s_2[i] +
      gamma[14] * age_10_15_2[i] * WinterAO_s_2[i] +
      gamma[15] * age_10_15_2[i] * PriorWinterAO_s_2[i] +
      gamma[16] * age_10_15_2[i] * PriorSpringAO_s_2[i] +
      
      gamma[17] * age_16_more[i] +
      gamma[18] * age_16_more[i] * DateCapture_s_2[i] +
      gamma[19] * age_16_more[i] * Size_s_2[i] +
      gamma[20] * age_16_more[i] * Size_s2_2[i] +
      gamma[21] * age_16_more[i] * DateFreezeUp_t_1_s_2[i] +
      gamma[22] * age_16_more[i] * WinterAO_s_2[i] +
      gamma[23] * age_16_more[i] * PriorWinterAO_s_2[i] +
      gamma[24] * age_16_more[i] * PriorSpringAO_s_2[i] +
      
      eps2[year_2[i]]
  }
  for (i in 1:nbyear_2) {
    eps2[i] ~ dnorm(0, sd = sigma2)
  }
  sigma2 ~ dunif(0, 10)
})

#' 
# Logistic regression for litter production ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable 
y_1 <- data_model_1$cub_number                         # Extract the response variable
y_1[y_1 == 0] <- 0 ; y_1[y_1 %in% c(1, 2, 3)] <- 1     # Success = 1, 2 or 3 cubs of the year ; failure = 0 cubs

# Covariates
numVars_1 <- 32 
age_1 <- as.numeric(data_model_1$age)
age_5_9_1 <- ifelse (age_1 < 10, 1, 0)
age_10_15_1 <- ifelse(age_1 >= 10 & age_1 <= 15, 1, 0)
age_16_20 <- ifelse(age_1 > 15 & age_1 <= 20, 1, 0)
age_21_more <- ifelse (age_1 > 20, 1, 0)

DateCapture_s_1 <- scale_ext(data_model_1$day_number, DateCapture)
Size_s_1 <- scale_ext(data_model_1$s_length, Size)
Size_s2_1 <- Size_s_1^2
DateFreezeUp_t_1_s_1 <- scale_ext(data_model_1$DateFreezeUp_t_1, DateFreezeUp_t_1)
WinterAO_s_1 <- scale_ext(data_model_1$winter_AO, WinterAO)
PriorWinterAO_s_1 <- scale_ext(data_model_1$prior_winter_AO, PriorWinterAO)
PriorSpringAO_s_1 <- scale_ext(data_model_1$prior_spring_AO, PriorSpringAO)

# Yearly random effect
df_years_1 <- data.frame(year = unique(data_model_1$year),
                       index_year = seq(1:length(unique(data_model_1$year))))
year_1 <- data_model_1$year
for (k in 1:nrow(df_years_1)) {
  year_1[which(year_1 == df_years_1$year[k])] <- df_years_1$index_year[k]
}
nbyear_1 <- length(unique(year_1))

}

# Logistic regression for litter size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable
  y_2 <- data_model_2$cub_number                  # Extract the response variable
y_2[y_2 == 1] <- 0 ; y_2[y_2 %in% c(2, 3)] <- 1 # Success = 2 or 3 cubs-of-the-year ; failure = 1 cubs-of-the-year 

# Covariates
  numVars_2 <- 24
  age_2 <- as.numeric(data_model_2$age)
  age_5_9_2 <- ifelse (age_2 < 10, 1, 0)
  age_10_15_2 <- ifelse(age_2 >= 10 & age_2 <= 15, 1, 0)
  age_16_more <- ifelse (age_2 > 15, 1, 0)
DateCapture_s_2 <- scale_ext(data_model_2$day_number, DateCapture)
Size_s_2 <- scale_ext(data_model_2$s_length, Size)
Size_s2_2 <- Size_s_2^2
DateFreezeUp_t_1_s_2 <- scale_ext(data_model_2$DateFreezeUp_t_1, DateFreezeUp_t_1)
WinterAO_s_2 <- scale_ext(data_model_2$winter_AO, WinterAO)
PriorWinterAO_s_2 <- scale_ext(data_model_2$prior_winter_AO, PriorWinterAO)
PriorSpringAO_s_2 <- scale_ext(data_model_2$prior_spring_AO, PriorSpringAO)

# Yearly random effect
df_years_2 <- data.frame(year = unique(data_model_2$year),
                       index_year = seq(1:length(unique(data_model_2$year))))
year_2 <- data_model_2$year
for (k in 1:nrow(df_years_2)) {
  year_2[which(year_2 == df_years_2$year[k])] <- df_years_2$index_year[k]
}
nbyear_2 <- length(unique(year_2))

}


# Create a list with all the constants required to run the nimble model
my.constants <- list(
  # For the litter production part
  N_1 = length(y_1),            # nb of females captured with or without cubs
                     year_1 = year_1,
                     nbyear_1 = nbyear_1,
                     age_5_9_1 = age_5_9_1,
                     age_10_15_1 = age_10_15_1, 
                     age_16_20 = age_16_20, 
                     age_21_more = age_21_more,
                     DateCapture_s_1 = DateCapture_s_1,
                     Size_s_1 = Size_s_1,
                     Size_s2_1 = Size_s2_1,
                     DateFreezeUp_t_1_s_1 = DateFreezeUp_t_1_s_1,
                     WinterAO_s_1 = WinterAO_s_1,
                     PriorWinterAO_s_1 = PriorWinterAO_s_1,
                     PriorSpringAO_s_1 = PriorSpringAO_s_1,
                     numVars_1 = numVars_1,
   # For the litter size part
                     N_2 = length(y_2),            
                     year_2 = year_2, 
                     nbyear_2 = nbyear_2,
                     age_5_9_2 = age_5_9_2,
                     age_10_15_2 = age_10_15_2, 
                     age_16_more = age_16_more,
                     DateCapture_s_2 = DateCapture_s_2,
                     Size_s_2 = Size_s_2,
                     Size_s2_2 = Size_s2_2,
                     DateFreezeUp_t_1_s_2 = DateFreezeUp_t_1_s_2,
                     WinterAO_s_2 = WinterAO_s_2,
                     PriorWinterAO_s_2 = PriorWinterAO_s_2,
                     PriorSpringAO_s_2 = PriorSpringAO_s_2,
                     numVars_2 = numVars_2) 

# Create a list with all the resposne variables
dat <- list(y_1 = y_1,
            y_2 = y_2)


# Specify the parameters to monitor
params <- c("beta",    # The coefficients of the regression of litter production
            "sigma1",  # The standard deviation of the yearly random effect for the regression of litter production
            "eps1",    # 
            "gamma",   # The coefficients of the regression of litter size 
            "sigma2",  # The standard deviation of the yearly random effect for the regression of litter size
            "eps2")    #

# generate a list with the initial values drawn in uniform distributions
inits <- function() list(beta = runif(n = numVars_1, min = -1, max = 1), 
                         sigma1 = runif(1),
                         gamma = runif(n = numVars_2, min = -1, max = 1),
                         sigma2 = runif(1))

#' 
#' 
#' Let's run the nimble model
#' 

set.seed(2022)
fit_freeze_up <-  nimbleMCMC(code = model_freeze_up,
                                 data = dat,
                                 constants = my.constants,
                                 inits = inits,
                                 monitors = params,
                                 thin = 10,
                                 niter = 135000,
                                 nburnin = 10000,
                                 nchains = 2,
                                 summary = TRUE,
                                 WAIC = TRUE)

save(fit_freeze_up,
     file = "data/fit_freeze_up_final")

#' 
#' Let's visually inspect the MCMC
#' 

load("07_results/01_interim_results/fit_outputs/litter_production_&_size/fit_freeze_up.RData")

numVars_1 <- 32 ; numVars_2 <- 24
N <- dim(fit_freeze_up$samples$chain1)[1]
C <- dim(fit_freeze_up$samples$chain1)[2]
res_temp <- rbind(fit_freeze_up$samples$chain1[seq(1, N, by = 3), ],
                  fit_freeze_up$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest <- c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                      grep("sigma", colnames(res_temp)))
res <- res_temp[, cols_of_interest]

# Plots
params.plot <- c()
for (k in 1:numVars_1) {
  params.plot <- c(params.plot, paste0("beta.", k, "."))
}
params.plot <- c(params.plot, "sigma1")
for (k in 1:numVars_2) {
  params.plot <- c(params.plot, paste0("gamma.", k, "."))
}
params.plot <- c(params.plot, "sigma2")

# Let's make two separate figures as there are a lot of parameters
diagnostic_plot_1 <- check_convergence_several_predictors(params.plot = params.plot[1:33],
                                                          nimble_output = fit_freeze_up)
nrows = length(params.plot[1:33])
save_plot(filename = "diagnostic_fit_freeze_up_1st_part.png",
          plot = diagnostic_plot_1,
          ncol = 3, nrow = 1,
          base_width = 3, base_height = nrows * 1.1, limitsize = FALSE)

diagnostic_plot_2 <- check_convergence_several_predictors(params.plot = params.plot[34:58],
                                                          nimble_output = fit_freeze_up)
nrows = length(params.plot[34:58])
save_plot(filename = "diagnostic_fit_freeze_up_2nd_part.png",
          plot = diagnostic_plot_2,
          ncol = 3, nrow = 1,
          base_width = 3, base_height = nrows * 1.1, limitsize = FALSE)

#' 
#' Let's check the Rhat for each parameter.
#' 
summary <- MCMCvis::MCMCsummary(fit_freeze_up$samples)
Rhats <- data.frame(rownames(summary), summary$Rhat)

sum(Rhats$summary.Rhat <1.1) - nrow(Rhats) # If this returns 0, then convergence is satisfactory for all parameters

#' 
#' Let's extract the output of the model and see which covariates are significant
#' 

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
                                          "DateFreezeUp", "WinterAO", "PriorWinterAO", "PriorSpringAO"), times = 7)) %>%
  mutate(covariate_age = ifelse(age == "5-9 yr", paste0(covariate, "_y"), 
                                ifelse(age == "10-15 yr", paste0(covariate, "_p"),
                                       ifelse(age == "16-20 yr", paste0(covariate, "_oa"), 
                                              ifelse(age == "\u2265 21 yr", paste0(covariate, "_ob"), 
                                                     ifelse(age == "\u2265 16 yr", paste0(covariate, "_o"), NA))))))

load("data/fit_freeze_up.RData")
numVars_1 <- 32 ; numVars_2 <- 24
N <- dim(fit_freeze_up$samples$chain1)[1]
C <- dim(fit_freeze_up$samples$chain1)[2]
res_temp <- rbind(fit_freeze_up$samples$chain1[seq(1, N, by = 3), ],
                  fit_freeze_up$samples$chain2[seq(1, N, by = 3), ])
res <- res_temp[, c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                    grep("sigma", colnames(res_temp)))] ; rm(res_temp)
cols <- colnames(res)[c(grep("beta", colnames(res)), grep("gamma", colnames(res)))]

caterpillar <- as.data.frame(res) %>%
  dplyr::select(-sigma1, -sigma2) %>%
  pivot_longer(cols = all_of(cols)) %>%
  left_join(x = .,
            y = model_full_df,
            by = "name") %>%
  mutate(age = factor(age, levels = c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr", "\u2265 16 yr")))

caterpillar_1 <- caterpillar %>% 
  filter(event == "litter production")
caterpillar_2 <- caterpillar %>% 
  filter(event == "litter size")

#' 
#' Covariates are retained only if their 89% credible interval does not overlap with 0. Age-specific intercepts are retained anyway. 
#' 
#' The tables below summarizes the significance of each age-specific effect (* : the 89% CI does not overlap with 0, and ** : the 95% CI does not overlap with 0).
#' 
#' For the regression of breeding probability: 

significance_1 <- rep("", times = numVars_1)
for (k in 1:numVars_1) {
    if (!between(0, left = quantile(res[, k], probs = 0.055), right = quantile(res[, k], probs = 0.945))) {
      significance_1[k] <- "*"
      if (!between(0, left = quantile(res[, k], probs = 0.025), right = quantile(res[, k], probs = 0.975))) {
        significance_1[k] <- "**"
      }
    }
  }

(df_significance_1 <- data.frame(parameter = rep(c("intercept", "DateCapture", "Size", "Size2",
                                                  "DateFreezeUp",
                                                  "WinterAO", "PriorWinterAO", "PriorSpringAO"), times = 4),
                                age = rep(c("5-9 yr", "10-15 yr", "16-20 yr", "\u2265 21 yr"), each = 8),
                                significance = significance_1) %>%
  pivot_wider(names_from = age, values_from = significance) %>%
  filter(parameter != "intercept"))

#' 
#' For the regression of litter size: 

res_2 <- res[, 33:56]
significance_2 <- rep("", times = numVars_2)
for (k in 1:numVars_2) {
    if (!between(0, left = quantile(res_2[, k], probs = 0.055), right = quantile(res_2[, k], probs = 0.945))) {
      significance_2[k] <- "*"
      if (!between(0, left = quantile(res_2[, k], probs = 0.025), right = quantile(res_2[, k], probs = 0.975))) {
        significance_2[k] <- "**"
      }
    }
  }

(df_significance_2 <- data.frame(parameter = rep(c("intercept", "DateCapture", "Size", "Size2",
                                                  "DateFreezeUp",
                                                  "WinterAO", "PriorWinterAO", "PriorSpringAO"), times = 3),
                                age = rep(c("5-9 yr", "10-15 yr", "\u2265 16 yr"), each = 8),
                                significance = significance_2) %>%
  pivot_wider(names_from = age, values_from = significance) %>%
  filter(parameter != "intercept"))

#' 
#' 
#' ## 2. Final model 
#' 
#' Let's build and run the final model, in which only the intercept and the covariates that were signficiant in the full model are included. 
model_freeze_up_final <- nimbleCode({
  # Logistic regression for litter production
  for (i in 1:numVars_1) {
    beta[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_1) {
    y_1[i] ~ dbern(p_1[i])
    logit(p_1[i]) <-
      beta[1] * age_5_9_1[i] +
      beta[2] * age_5_9_1[i] * DateCapture_s_1[i] +

      beta[3] * age_10_15_1[i] +
      beta[4] * age_10_15_1[i] * Size_s_1[i] +
      beta[5] * age_10_15_1[i] * Size_s2_1[i] +
      beta[6] * age_10_15_1[i] * WinterAO_s_1[i] +
      beta[7] * age_10_15_1[i] * PriorSpringAO_s_1[i] +
      
      beta[8] * age_16_20[i] +
      beta[9] * age_16_20[i] * DateCapture_s_1[i] +
      beta[10] * age_16_20[i] * WinterAO_s_1[i] +
      beta[11] * age_16_20[i] * PriorWinterAO_s_1[i] +
      beta[12] * age_16_20[i] * PriorSpringAO_s_1[i] +
      
      beta[13] * age_21_more[i] +
      beta[14] * age_21_more[i] * DateCapture_s_1[i] +
      
      eps1[year_1[i]]
  }
  for (i in 1:nbyear_1) {
    eps1[i] ~ dnorm(0, sd = sigma1)
  }
  sigma1 ~ dunif(0, 10)
  # Logistic regression for litter size
  for (i in 1:numVars_2) {
    gamma[i] ~ dnorm(0, sd = 1.5)
  }
  for (i in 1:N_2) {
    y_2[i] ~ dbern(p_2[i])
    logit(p_2[i]) <-
      gamma[1] * age_5_9_2[i] +
      gamma[2] * age_5_9_2[i] * DateCapture_s_2[i] +
      gamma[3] * age_5_9_2[i] * PriorWinterAO_s_2[i] +

      gamma[4] * age_10_15_2[i] +
      
      gamma[5] * age_16_more[i] +
    
      eps2[year_2[i]]
  }
  for (i in 1:nbyear_2) {
    eps2[i] ~ dnorm(0, sd = sigma2)
  }
  sigma2 ~ dunif(0, 10)
})

#' 

# Logistic regression for litter production ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable 
y_1 <- data_model_1$cub_number                         # Extract the response variable
y_1[y_1 == 0] <- 0 ; y_1[y_1 %in% c(1, 2, 3)] <- 1     # Success = 1, 2 or 3 cubs of the year ; failure = 0 cubs

# Covariates
numVars_1 <- 14 
age_1 <- as.numeric(data_model_1$age)
age_5_9_1 <- ifelse (age_1 < 10, 1, 0)
age_10_15_1 <- ifelse(age_1 >= 10 & age_1 <= 15, 1, 0)
age_16_20 <- ifelse(age_1 > 15 & age_1 <= 20, 1, 0)
age_21_more <- ifelse (age_1 > 20, 1, 0)

DateCapture_s_1 <- scale_ext(data_model_1$day_number, DateCapture)
Size_s_1 <- scale_ext(data_model_1$s_length, Size)
Size_s2_1 <- Size_s_1^2
WinterAO_s_1 <- scale_ext(data_model_1$winter_AO, WinterAO)
PriorWinterAO_s_1 <- scale_ext(data_model_1$prior_winter_AO, PriorWinterAO)
PriorSpringAO_s_1 <- scale_ext(data_model_1$prior_spring_AO, PriorSpringAO)

# Yearly random effect
df_years_1 <- data.frame(year = unique(data_model_1$year),
                       index_year = seq(1:length(unique(data_model_1$year))))
year_1 <- data_model_1$year
for (k in 1:nrow(df_years_1)) {
  year_1[which(year_1 == df_years_1$year[k])] <- df_years_1$index_year[k]
}
nbyear_1 <- length(unique(year_1))

}

# Logistic regression for litter size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
# Response variable
  y_2 <- data_model_2$cub_number                  # Extract the response variable
y_2[y_2 == 1] <- 0 ; y_2[y_2 %in% c(2, 3)] <- 1 # Success = 2 or 3 cubs-of-the-year ; failure = 1 cubs-of-the-year 

# Covariates
  numVars_2 <- 5
  age_2 <- as.numeric(data_model_2$age)
  age_5_9_2 <- ifelse (age_2 < 10, 1, 0)
  age_10_15_2 <- ifelse(age_2 >= 10 & age_2 <= 15, 1, 0)
  age_16_more <- ifelse (age_2 > 15, 1, 0)
DateCapture_s_2 <- scale_ext(data_model_2$day_number, DateCapture)
PriorWinterAO_s_2 <- scale_ext(data_model_2$prior_winter_AO, PriorWinterAO)

# Yearly random effect
df_years_2 <- data.frame(year = unique(data_model_2$year),
                       index_year = seq(1:length(unique(data_model_2$year))))
year_2 <- data_model_2$year
for (k in 1:nrow(df_years_2)) {
  year_2[which(year_2 == df_years_2$year[k])] <- df_years_2$index_year[k]
}
nbyear_2 <- length(unique(year_2))

}

# Create a list with all the constants required to run the nimble model
my.constants <- list(
  # For the litter production part
  N_1 = length(y_1),            # nb of females captured with or without cubs
                     year_1 = year_1,
                     nbyear_1 = nbyear_1,
                     age_5_9_1 = age_5_9_1,
                     age_10_15_1 = age_10_15_1, 
                     age_16_20 = age_16_20, 
                     age_21_more = age_21_more,
                     DateCapture_s_1 = DateCapture_s_1,
                     Size_s_1 = Size_s_1,
                     Size_s2_1 = Size_s2_1,
                     WinterAO_s_1 = WinterAO_s_1,
                     PriorWinterAO_s_1 = PriorWinterAO_s_1,
                     PriorSpringAO_s_1 = PriorSpringAO_s_1,
                     numVars_1 = numVars_1,
   # For the litter size part
                     N_2 = length(y_2),            
                     year_2 = year_2, 
                     nbyear_2 = nbyear_2,
                     age_5_9_2 = age_5_9_2,
                     age_10_15_2 = age_10_15_2, 
                     age_16_more = age_16_more,
                     DateCapture_s_2 = DateCapture_s_2,
                     PriorWinterAO_s_2 = PriorWinterAO_s_2,
                     numVars_2 = numVars_2) 

# Create a list with all the resposne variables
dat <- list(y_1 = y_1,
            y_2 = y_2)

# Specify the parameters to monitor
params <- c("beta", "sigma1", "eps1", "gamma", "sigma2", "eps2")

# generate a list with the initial values drawn in uniform distributions
inits <- function() list(beta = runif(n = numVars_1, min = -1, max = 1), 
                         sigma1 = runif(1),
                         gamma = runif(n = numVars_2, min = -1, max = 1),
                         sigma2 = runif(1))

#' 
#' Let's run the nimble model

set.seed(2022)
fit_freeze_up_final <-  nimbleMCMC(code = model_freeze_up_final,
                                                             data = dat,
                                                             constants = my.constants,
                                                             inits = inits,
                                                             monitors = params,
                                                             thin = 10,
                                                             niter = 135000,
                                                             nburnin = 10000,
                                                             nchains = 2,
                                                             summary = TRUE,
                                                             WAIC = TRUE)

save(fit_freeze_up_final,
     file = "data/fit_freeze_up_final.RData")

#' 
#' Let's visually inspect the MCMC
#' 

load("data/fit_freeze_up_final.RData")

# Check convergence with R-hat
summary <- MCMCvis::MCMCsummary(fit_freeze_up_final$samples)
data.frame(rownames(summary), summary$Rhat)


numVars_1 <- 14 ; numVars_2 <- 5
N <- dim(fit_freeze_up_final$samples$chain1)[1]
C <- dim(fit_freeze_up_final$samples$chain1)[2]
res_temp <- rbind(fit_freeze_up_final$samples$chain1[seq(1, N, by = 3), ],
                  fit_freeze_up_final$samples$chain2[seq(1, N, by = 3), ])
cols_of_interest <- c(grep("beta", colnames(res_temp)), grep("gamma", colnames(res_temp)),
                      grep("sigma", colnames(res_temp)))
res <- res_temp[, cols_of_interest]

# Parameters to plot
params.plot <- c()
for (k in 1:numVars_1) {
  params.plot <- c(params.plot, paste0("beta.", k, "."))
}
params.plot <- c(params.plot, "sigma1")
for (k in 1:numVars_2) {
  params.plot <- c(params.plot, paste0("gamma.", k, "."))
}
params.plot <- c(params.plot, "sigma2")


# Plot
diagnostic_plot <- check_convergence_several_predictors(params.plot = params.plot,
                                                          nimble_output = fit_freeze_up_final)

#' 
#' Let's check the Rhat for each parameter.

summary <- MCMCvis::MCMCsummary(fit_freeze_up_final$samples)
Rhats <- data.frame(rownames(summary), summary$Rhat)

sum(Rhats$summary.Rhat <1.1) - nrow(Rhats) # If this returns 0, then convergence is satisfactory for all parameters

