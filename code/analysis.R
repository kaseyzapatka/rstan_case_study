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
)

# ==========================================================================
# DATA
# ==========================================================================

# data path
data_path <- "/Users/Dora/git/projects/rstan_case_study/data/" 
data_path

# function to load data
load_in_channel_data <- function(data_path){
  data <- read_rds(paste0(data_path, "channel-spend.rds"))
  return(data)
}

# Call your function below
channels <- load_in_channel_data(data_path)

# view data
channels %>% glimpse()

# Data passed to stan.
# We recommend you only edit the priors and 
# The prior_only argument
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

mod = cmdstan_model("simple-model.stan")

# Sample the model
fit = mod$sample(data = dat, parallel_chains = parallel::detectCores(), seed = 0)



# STEP 1 -------------------------------------------
dat_prior <- dat
dat_prior$prior_only <- 0


prior_fit <- mod$sample(
  data = dat_prior,
  parallel_chains = parallel::detectCores(),
  seed = 0
)


# Replace "y_rep" with the correct generated quantity or parameter
prior_draws <- posterior::as_draws_df(prior_fit$draws("predicted"))
prior_draws



# Convert to long format
prior_long <-
  prior_draws  %>% 
  pivot_longer(starts_with("predicted"), names_to = "draw", values_to = "predicted") %>% 
  glimpse()


# Plot histogram
prior_long %>% 
ggplot(aes(x = predicted)) +
  geom_histogram(fill = "maroon", alpha = 0.6) +
  labs(
    title = "Prior Predictive Distribution of y_rep",
    x = "Predicted y (from priors only)",
    y = "Density"
  )



# STEP 2 -------------------------------------------
# Extract draws as a data frame
draws_df <- as_draws_df(prior_fit$draws("predicted"))
draws_df

# Use tidybayes to extract and tidy the array
draws_long <- 
  prior_fit  %>% 
  spread_draws(predicted[t])  # assuming depvar_rep is indexed by t
  draws_long


# All prior predictive values collapsed
ggplot(draws_long, aes(x = predicted)) +
  geom_density(fill = "maroon", alpha = 0.6) +
  labs(title = "Prior Predictive Distribution", x = "Simulated depvar", y = "Density")


# 10 randomly sampled days
set.seed(345)  # optional, for reproducibility

draws_long %>%
  ungroup()  %>% 
  distinct(t) %>%              # get unique time periods
  slice_sample(n = 12) %>%     # sample 10 time periods
  pull(t) -> sampled_times     # extract sampled time periods


draws_long %>%
  filter(t %in% sampled_times) %>% 
  ggplot(aes(x = predicted)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ t, scales = "free") +
  labs(title = "Prior Predictive by Timestep")


draws_long %>% 
  glimpse()
  group_by(t) %>% 
  slice_sample(n = 10) %>% 
  ungroup() %>% 
ggplot(aes(x = predicted)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ t, scales = "free") +
  labs(title = "Prior Predictive by Timestep")


draws_long %>% 
  distinct(t) %>%
   glimpse()

# STEP 3 -------------------------------------------
fit$cmdstan_diagnose()


# basic dataviz -------------------------------------------

prior_long %>% 
  summary()

draws_long  %>% 
  summary()

  
channels %>% 
  as_tibble() %>% 
  summary()

  ggplot(aes(x=depvar)) + 
    geom_density(bins = 40)


draws_long  %>% 
  summary()
  glimpse()


# STEP 4 -------------------------------------------


