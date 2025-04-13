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


#
# Load libraries
# --------------------------------------------------------------------------
# Loading required packages and setting defaults
librarian::shelf(
  cmdstanr, tidyverse, tidybayes, posterior, bayesplot, timathomas/colorout,
  ggdark, patchwork
)

# ==========================================================================
# DATA
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
  prior_only = 1
)

mod = cmdstan_model("simple-model.stan")

# Sample the model
fit = mod$sample(data = dat, parallel_chains = parallel::detectCores(), seed = 0)


# ==========================================================================
# DATA VISUALIZATIONS
# ==========================================================================

# density plot
channels %>% 
  ggplot(aes(x = depvar)) + 
  geom_density(fill = "steelblue", alpha = 0.5, color = NA) +
  plot_theme +
  #dark_theme_bw() +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
  labs(
    title = "Observed Distribution of Daily Revenue",
    x = "Daily Revenue \n($ thousands of dollars)",
    y = "Density"
  ) 
  

# plot over time
channels %>% 
  mutate(day = row_number()) %>% 
  ggplot(aes(x = day, y = depvar)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_jitter() +
  geom_smooth(se = TRUE, method = "loess", color = "maroon", linewidth = 1) +
  labs(
    title = "Predicted Values Over Time",
    x = "Day",
    y = "Predicted Value"
  ) +
  plot_theme
  


# Main plot with line + smoother + jittered points
channels %>%
  mutate(t = row_number()) %>%
  ggplot(aes(x = t, y = depvar)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_smooth(se = TRUE, method = "loess", color = "maroon", linewidth = 1) +
  geom_point(
    aes(x = t, y = depvar),
    width = 5, height = 0,
    alpha = 0.4, color = "black", size = 2
  ) +
  labs(
    title = "Predicted Values Over Time with Random Sampled Observations",
    x = "Day",
    y = "Predicted Value"
  ) +
  plot_theme


# 1. Density plot
p1 <- channels %>% 
  ggplot(aes(x = depvar)) + 
  geom_density(fill = "steelblue", alpha = 0.5, color = NA) +
  plot_theme +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
  labs(
    title = "Observed Distribution of Daily Revenue",
    x = "Daily Revenue \n($ thousands of dollars)",
    y = "Density"
  )

# 2. Time series plot
p2 <- channels %>% 
  mutate(day = row_number()) %>% 
  ggplot(aes(x = day, y = depvar)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_jitter(width = 5, alpha = 0.3, size = 1) +
  geom_smooth(se = TRUE, method = "loess", color = "maroon", linewidth = 1) +
  plot_theme +
  labs(
    title = "Predicted Values Over Time",
    x = "Day",
    y = "Predicted Value"
  )

# Combine using patchwork
p1 / p2 + plot_layout(heights = c(1, 1.2))


# STEP 2 -------------------------------------------
# sample from the predictive prior
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


