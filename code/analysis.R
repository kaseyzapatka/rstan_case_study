# ==========================================================================
# Analysis

# Summary: Evaluate model fit based on prior and posterior fits. Update as 
# needed.
# ==========================================================================


# ==========================================================================
# PACKAGES & OPTIONS
# ==========================================================================
# clear all
remove(list = ls())


# set terminal width options and increase Java memory in R 
#options(
#  java.parameters = paste0("-Xmx200g"), # Increase Java memory
#  scipen = 999, # avoid scientific notation
#  width=Sys.getenv("COLUMNS") # set width to terminal window
#)

#
# Load libraries
# --------------------------------------------------------------------------
# Loading required packages and setting defaults
librarian::shelf(
  cmdstanr, tidyverse, tidybayes, posterior, bayesplot, timathomas/colorout,
  ggdark, patchwork, here, qs
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
# STEP 1: LOAD DATA
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
  
  glimpse(channels)

# ==========================================================================
# DATA VISUALIZATIONS
# ==========================================================================

# 1. Density plot of observed daily revenue
p1 <- 
  channels %>% 
  ggplot(aes(x = depvar)) + 
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 4.9, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 7.45, color = "red", linetype = "dashed", linewidth = 1) +
  plot_theme +
  scale_x_continuous(labels = scales::label_dollar(scale = 1, accuracy = 0.1, suffix = "k")) +
  labs(
    title = "Observed Distribution of Daily Revenue",
    x = "Daily Revenue \n($ hundreds of thousands of dollars)",
    y = "Density"
  )

# 2. Time series plot of observed revenue
p2 <- 
  channels %>% 
  mutate(day = row_number()) %>% 
  ggplot(aes(x = day, y = depvar)) +
  geom_line(color = "steelblue", linewidth = 1, alpha = 0.7,) +
  geom_jitter(width = 5, alpha = 0.3, size = 1) +
  geom_smooth(se = TRUE, method = "loess", color = "black", linewidth = 1) +
  plot_theme +
    scale_y_continuous(labels = scales::label_dollar(scale = 1, accuracy = 0.1, suffix = "k")) +
    scale_x_continuous(labels = scales::label_number()) +
  labs(
    title = "Observed Daily Revenue Over Time",
    x = "Day",
    y = "Daily Revenue \n($ hundreds of thousands of dollars)",
  )

# Combine using patchwork
plot_observed <- p1 / p2 + plot_layout(heights = c(1, 1.2))
plot_observed

  # save for submission
  ggsave(here("output/figures/observed_combinded.png"), plot_observed)


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

mod = cmdstan_model(here("code/simple-model.stan"))

# Sample the model
fit = mod$sample(data = dat, parallel_chains = parallel::detectCores(), seed = 0)


# ==========================================================================
# STEP 2: SAMPLE FROM THE PREDICTIVE PRIOR
# ==========================================================================


#
# Sample from the predictive posterior
# --------------------------------------------------------------------------
# make sure switch samples from prior only
dat_prior <- dat
dat_prior$prior_only <- 1

# sample from the predictive prior
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


#
# Visualize prior distribution
# --------------------------------------------------------------------------
plot_priors <-
  prior_draws %>% 
  ggplot(aes(x = predicted)) +
    geom_density(fill = "steelblue", alpha = 0.5) +
    labs(title = "Prior Predictive Distribution", x = "Simulated depvar", y = "Density") +
    plot_theme +
    scale_x_continuous(labels = scales::label_dollar(scale = 1, accuracy = 0.1, suffix = "k")) +
    labs(
      title = "Prior Predictive Distribution of Daily Revenue",
      x = "Daily Revenue \n($ hundreds of thousands of dollars)",
      y = "Density"
    )
  
plot_priors

    # save for submission
    ggsave(here("output/figures/plot_priors.png"), plot_priors)

#
# Distributional summaries
# --------------------------------------------------------------------------

# What is the distribution of predictions?
predicted_summary <- 
  prior_draws %>% 
  ungroup() %>% 
  select(predicted) %>% 
  summary()

# What is the distribution of observed?
observed_summary <- 
  channels %>% 
  as_tibble() %>% 
  select(depvar) %>% 
  summary()

  # save for submission
  qsave(predicted_summary, here("output/tables/predicted_summary.qs"))
  qsave(observed_summary, here("output/tables/observed_summary.qs"))



# ==========================================================================
# STEP 3: SAMPLE FROM THE POSTERIOR
# ==========================================================================

#
# Sample from the predictive posterior
# --------------------------------------------------------------------------
# switch to sample from posterior
dat_posterior <- dat
dat_posterior$prior_only <- 0

# sample 
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

#
# Visualize posterior distribution
# --------------------------------------------------------------------------
plot_posterior <-
  posterior_draws %>% 
  ggplot(aes(x = predicted)) +
    geom_density(fill = "steelblue", alpha = 0.5) +
    labs(title = "Posterior Predictive Distribution", x = "Simulated depvar", y = "Density") +
    plot_theme +
    scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
    labs(
      title = "Observed Distribution of Daily Revenue",
      x = "Daily Revenue \n($ thousands of dollars)",
      y = "Density"
    )

plot_posterior

    # save for submission
    ggsave(("output/figures/plot_posterior.png"), plot_posterior)


#
# Overlay prior and posterior
# --------------------------------------------------------------------------

# Combine the data with a new column to label prior/posterior
combined_draws <- bind_rows(
  prior_draws  %>% mutate(type = "Prior"),
  posterior_draws %>% mutate(type = "Posterior")
)

# Plot both distributions on the same graph
plot_prior_on_posterior <- 
combined_draws  %>% 
ggplot(aes(x = predicted, fill = type)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "firebrick")) +
  labs(
    title = "Prior vs. Posterior Predictive Distributions of Daily Revenue",
    x = "Daily Revenue \n($ thousands of dollars)",
    y = "Density",
    fill = NULL
  ) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
  plot_theme

plot_prior_on_posterior

    # save for submission
    ggsave(("output/figures/plot_prior_on_posterior.png"), plot_prior_on_posterior)

#
# Diagnose
# --------------------------------------------------------------------------
fit_posterior = mod$sample(data = dat_posterior, parallel_chains = parallel::detectCores(), seed = 0)

fit_posterior$cmdstan_diagnose()

  # save for submission 
  writeLines(
    capture.output(fit_posterior$cmdstan_diagnose()),
    here::here("output/tables", "posterior_diagnostics.txt")
  )


# ==========================================================================
# STEP 4: UPDATE PRIORS
# ==========================================================================

#
# Update Priors
# --------------------------------------------------------------------------
dat_updated = list(
  channels = channels[, colnames(channels) != 'depvar'],
  n_channels = dim(channels)[2] - 1,
  n_timesteps = dim(channels)[1],
  depvar = channels$depvar,

  intercept_lb = 2,
  intercept_ub = 6,
  intercept_eta_mean = 0,
  intercept_eta_scale = 1,

  beta_lb = 0,
  beta_ub = 4,
  beta_eta_mean = 0,
  beta_eta_scale = 2,  # allow both weak and strong steepness

  kappa_lb = 1,
  kappa_ub = 6,
  kappa_eta_mean = 0,
  kappa_eta_scale = 2,  # allow flexibility for tall + short hills

  conc_lb = 0.3,
  conc_ub = 2.5,
  conc_eta_mean = 0,
  conc_eta_scale = 1.2,

  shift_lb = 0,
  shift_ub = 10,
  shift_eta_mean = 0,
  shift_eta_scale = 4.5,  # very wide to cover both 4.9 and 7.45

  prior_only = 1
)



#
# Sample from the predictive posterior
# --------------------------------------------------------------------------
# make sure switch samples from prior only
dat_prior_updated <- dat_updated
dat_prior_updated$prior_only <- 1

# sample from the predictive prior
prior_fit_updated <- mod$sample(
  data = dat_prior_updated,
  parallel_chains = parallel::detectCores(),
  seed = 0
)

# Use tidybayes to extract and tidy the array
prior_draws_updated <- 
  prior_fit_updated  %>% 
  spread_draws(predicted[t])  %>%  # spread by t (time index)
  glimpse()


#
# Visualize prior distribution
# --------------------------------------------------------------------------
plot_priors_updated <-
  prior_draws_updated %>% 
  ggplot(aes(x = predicted)) +
    geom_density(fill = "steelblue", alpha = 0.5) +
    labs(title = "Prior Predictive Distribution", x = "Simulated depvar", y = "Density") +
    plot_theme +
    scale_x_continuous(labels = scales::label_dollar(scale = 1, accuracy = 0.1, suffix = "k")) +
    labs(
      title = "Prior Predictive Distribution of Daily Revenue",
      x = "Daily Revenue \n($ hundreds of thousands of dollars)",
      y = "Density"
    )
  
plot_priors_updated

    # save for submission
    ggsave(here("output/figures/plot_priors_updated.png"), plot_priors_updated)



#
# Refit model with updated priors
# --------------------------------------------------------------------------
fit_updated = mod$sample(data = dat_updated, parallel_chains = parallel::detectCores(), seed = 0)

#
# Diagnose
# --------------------------------------------------------------------------
fit_updated$cmdstan_diagnose()


  # save for submission 
  writeLines(
    capture.output(fit_updated$cmdstan_diagnose()),
    here::here("output/tables", "posterior_updated_diagnostics.txt")
  )


#
# Sample from the updated posterior
# --------------------------------------------------------------------------

# switch to sample from posterior
dat_posterior_updated <- dat_updated
dat_posterior_updated$prior_only <- 0

# sample 
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

#
# Visualize updated posterior distribution
# --------------------------------------------------------------------------
plot_posterior_updated <-
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

  plot_posterior_updated

    # save for submission
    ggsave(("output/figures/plot_posterior_updated.png"), plot_posterior_updated)


#
# Overlay prior and posterior
# --------------------------------------------------------------------------

# Combine the data with a new column to label prior/posterior
combined_draws <- bind_rows(
  prior_draws_updated  %>% mutate(type = "Prior"),
  posterior_draws_updated %>% mutate(type = "Posterior")
)

# Plot both distributions on the same graph
plot_prior_on_posterior <- 
combined_draws  %>% 
ggplot(aes(x = predicted, fill = type)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 4.9, color = "#A52A2A", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 7.45, color = "#A52A2A", linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("Prior" = "steelblue", "Posterior" = "firebrick")) +
  labs(
    title = "Prior vs. Posterior Predictive Distributions of Daily Revenue",
    x = "Daily Revenue \n($ thousands of dollars)",
    y = "Density",
    fill = NULL
  ) +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e3, accuracy = 1, suffix = "k")) +
  plot_theme

plot_prior_on_posterior

    # save for submission
    ggsave(("output/figures/plot_prior_on_posterior_updated.png"), plot_prior_on_posterior)


