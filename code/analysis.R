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

mod = cmdstan_model("simple-model.stan")

# Sample the model
fit = mod$sample(data = dat, parallel_chains = parallel::detectCores(), seed = 0)



# 
get_variables(fit)

# Visualize
bayesplot::ppc_dens_overlay(y = prior_draws, yrep = fit)

prior_draws <- extract(fit, pars = "depvar")
prior_draws


dat_prior <- dat
dat_prior$prior_only <- 1


prior_fit <- mod$sample(
  data = dat_prior,
  parallel_chains = parallel::detectCores(),
  seed = 0
)


library(posterior)

# Replace "y_rep" with the correct generated quantity or parameter
prior_draws <- posterior::as_draws_df(prior_fit$draws("depvar"))

variables <- prior_fit$metadata()$model_vars
print(variables)
