library(cmdstanr)


load_in_channel_data <- function(){
  # Insert your code here
  return(NA)  
}

# Call your function below
channels <- NA

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
