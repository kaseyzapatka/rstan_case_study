# ==========================================================================
# Analysis

# Summary: This script ...
# ==========================================================================


# ==========================================================================
# PACKAGES & OPTIONS
# ==========================================================================
# clear all
remove(list = ls())


# set terminal width options and increase Java memory in R 
options(
  java.parameters = paste0("-Xmx200g"), # Increase Java memory
  scipen = 999, # avoid scientific notation
  width=Sys.getenv("COLUMNS") # set width to terminal window
)

#
# Load libraries
# --------------------------------------------------------------------------
# Loading required packages and setting defaults
librarian::shelf(
  cmdstanr, tidyverse, tidybayes, posterior, bayesplot, timathomas/colorout,
  ggdark, patchwork
)

 
#
# Set ggplot themes 
# --------------------------------------------------------------------------
plot_theme <- 
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14),  # Increase x-axis label size
    axis.title.y = element_text(size = 14),   # Increase y-axis label size
    axis.text.x = element_text(size = 14),   # Increase x-axis tick labels
    axis.text.y = element_text(size = 14),    # Increase y-axis tick labels
    plot.title.position = "plot",
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "bold.italic", hjust = 0.05),
    plot.caption = element_text(size = 12, hjust = 0),
    plot.caption.position = "plot",
    
    # Custom panel border (only left and bottom)
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black"),  # Bottom border
    axis.line.y = element_line(color = "black")   # Left border
  )


# ==========================================================================
# LOAD DATA
# ==========================================================================

# data path
data_path <- "/Users/Dora/git/projects/rstan_case_study/data/" 
data_path

# STEP 1 -------------------------------------------
# function to load data
load_in_channel_data <- function(data_path){
  data <- read_rds(paste0(data_path, "channel-spend.rds"))
  return(data)
}

# Call your function below
channels <- load_in_channel_data(data_path)

# ==========================================================================
# DATA VISUALIZATIONS
# ==========================================================================

# 1. Density plot of observed daily revenue
p1 <- 
  channels %>% 
  ggplot(aes(x = depvar)) + 
  geom_density(fill = "steelblue", alpha = 0.5) +
  plot_theme +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
  labs(
    title = "Observed Distribution of Daily Revenue",
    x = "Daily Revenue \n($ thousands of dollars)",
    y = "Density"
  )

# 2. Time series plot of observed revenue
p2 <- 
  channels %>% 
  mutate(day = row_number()) %>% 
  ggplot(aes(x = day, y = depvar)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_jitter(width = 5, alpha = 0.3, size = 1) +
  geom_smooth(se = TRUE, method = "loess", color = "black", linewidth = 1) +
  plot_theme +
  labs(
    title = "Predicted Values Over Time",
    x = "Day",
    y = "Predicted Value"
  )

# Combine using patchwork
p1 / p2 + plot_layout(heights = c(1, 1.2))


# ==========================================================================
# DATA + PRIORS
# ==========================================================================

dat = list(
 # Data --------------------------------------------------------------------
   channels = channels[, colnames(channels)!='depvar'],
   
   n_channels = dim(channels)[2]-1,
   n_timesteps = dim(channels)[1],
   depvar = channels$depvar,
   
 # Priors ------------------------------------------------------------------
   intercept_lb = 10,
   intercept_ub = 20,
   intercept_eta_mean = 0,
   intercept_eta_scale = 100,
   beta_lb = 0,
   beta_ub = 5,
   beta_eta_mean = -1,
   beta_eta_scale = 1,
   
   kappa_lb = 3,
   kappa_ub = 10,
   kappa_eta_mean = -1,
   kappa_eta_scale = 1,
   
   conc_lb = 0.3,
   conc_ub = 1.5,
   conc_eta_mean = 0,
   conc_eta_scale = 1,
   
   shift_lb = 0,
   shift_ub = 10,
   shift_eta_mean = 0,
   shift_eta_scale = 1,
 
 # Sample From Prior Predictive? -------------------------------------------
   ## 1 - Yes
   ## 0 - No (sample from posterior)
   prior_only = 1
 )
 

# ==========================================================================
# MODEL
# ==========================================================================

mod = cmdstan_model("code/simple-model.stan")

# Sample the model
fit = mod$sample(data = dat, parallel_chains = parallel::detectCores(), seed = 0)


# ==========================================================================
# STEP 2: SAMPLE FROM THE PREDICTIVE PRIOR
# ==========================================================================

# STEP 2 -------------------------------------------
# sample from the predictive prior
dat_prior <- dat
dat_prior$prior_only <- 1


prior_fit <- mod$sample(
  data = dat_prior,
  parallel_chains = parallel::detectCores(),
  seed = 0
)

# Use tidybayes to extract and tidy the array
prior_draws <- 
  prior_fit  %>% 
  spread_draws(predicted[t])  %>%  # spread by t (time index)
  glimpse()

# All prior predictive values collapsed
prior_draws %>% 
  ggplot(aes(x = predicted)) +
    geom_density(fill = "steelblue", alpha = 0.3) +
    labs(title = "Prior Predictive Distribution", x = "Simulated depvar", y = "Density") +
    plot_theme +
    scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
    labs(
      title = "Observed Distribution of Daily Revenue",
      x = "Daily Revenue \n($ thousands of dollars)",
      y = "Density"
    )


prior_draws %>% 
  summary()
  
channels %>% 
  as_tibble() %>% 
  summary()


# 10 randomly sampled days
set.seed(193)  # optional, for reproducibility

prior_draws %>%
  ungroup()  %>% 
  distinct(t) %>%              # get unique time periods
  slice_sample(n = 12) %>%     # sample 10 time periods
  pull(t) -> sampled_times     # extract sampled time periods


prior_draws %>%
  filter(t %in% sampled_times) %>% 
  ggplot(aes(x = predicted)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ t, scales = "free") +
  labs(title = "Prior Predictive by Timestep")


# ==========================================================================
# STEP 3: SAMPLE FROM THE POSTERIOR
# ==========================================================================

# Diagnose -------------------------------------------
# sample from the predictive prior

# Diagnose -------------------------------------------
# sample from the predictive posterior
dat_posterior <- dat
dat_posterior$prior_only <- 0


posterior_fit <- mod$sample(
  data = dat_posterior,
  parallel_chains = parallel::detectCores(),
  seed = 0
)

# Use tidybayes to extract and tidy the array
posterior_draws <- 
  posterior_fit  %>% 
  spread_draws(predicted[t])  %>%  # spread by t (time index)
  glimpse()

# All posterior predictive values collapsed
posterior_draws %>% 
  ggplot(aes(x = predicted)) +
    geom_density(fill = "steelblue", alpha = 0.3) +
    labs(title = "Posterior Predictive Distribution", x = "Simulated depvar", y = "Density") +
    plot_theme +
    scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
    labs(
      title = "Observed Distribution of Daily Revenue",
      x = "Daily Revenue \n($ thousands of dollars)",
      y = "Density"
    )




# Diagnose -------------------------------------------
fit$cmdstan_diagnose()

# STEP 4 -------------------------------------------
# Updated priors
dat = list(
# Data --------------------------------------------------------------------
  channels = channels[, colnames(channels)!='depvar'],
  
  n_channels = dim(channels)[2]-1,
  n_timesteps = dim(channels)[1],
  depvar = channels$depvar,
  
# Priors ------------------------------------------------------------------
  intercept_lb = 4,
  intercept_ub = 10,
  intercept_eta_mean = 7,
  intercept_eta_scale = 2,
  beta_lb = 0,
  beta_ub = 5,
  beta_eta_mean = 1,
  beta_eta_scale = 0.5,
  
  kappa_lb = 3,
  kappa_ub = 10,
  kappa_eta_mean = 6.5,
  kappa_eta_scale = 1.5,
  
  conc_lb = 0.3,
  conc_ub = 1.5,
  conc_eta_mean = 0,
  conc_eta_scale = 1,
  
  shift_lb = 0,
  shift_ub = 10,
  shift_eta_mean = 0,
  shift_eta_scale = 0.5,

# Sample From Prior Predictive? -------------------------------------------
  ## 1 - Yes
  ## 0 - No (sample from posterior)
  prior_only = 0
)



# UPDATED POSTERIOR -------------------------------------------
# sample from the predictive posterior
dat_posterior_updated <- dat
dat_posterior_updated$prior_only <- 0


posterior_fit_updated <- mod$sample(
  data = dat_posterior_updated,
  parallel_chains = parallel::detectCores(),
  seed = 0
)

# Use tidybayes to extract and tidy the array
posterior_draws_updated <- 
  posterior_fit_updated  %>% 
  spread_draws(predicted[t])  %>%  # spread by t (time index)
  glimpse()

# All posterior predictive values collapsed
posterior_draws_updated %>% 
  ggplot(aes(x = predicted)) +
    geom_density(fill = "steelblue", alpha = 0.3) +
    labs(title = "Posterior Predictive Distribution", x = "Simulated depvar", y = "Density") +
    plot_theme +
    scale_x_continuous(
      breaks = seq(0, max(posterior_draws_updated$predicted), by = 2.5),
      labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")
    ) +
    labs(
      title = "Observed Distribution of Daily Revenue",
      x = "Daily Revenue \n($ thousands of dollars)",
      y = "Density"
    )


draws_array <- as_draws_array(fit$draws(variables = "kappa[7]"))

mcmc_dens(draws_array, pars = "kappa[7]")
mcmc_areas(draws_array, pars = "kappa[7]")

variables(fit$draws())

color_scheme_set("red")
mcmc_intervals(draws_array, pars = c("mailers", "podcast", "influencers", "sigma"))

draws_array