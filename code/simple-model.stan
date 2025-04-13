
functions {
  real scaled_inv_logit(real lb, real ub, real m, real s, real eta) {
    return lb + (ub-lb) * inv_logit(m + eta*s);
  }
  
  vector scaled_inv_logit2(real lb, real ub, real m, real s, vector eta) {
    return lb + (ub-lb) * inv_logit(m + eta*s);
  }
  
  vector sub_col2(vector x, int start, int length) {
    vector[length] result;
    for (i in 1:length) {
      result[i] = x[start + i - 1];
    }
    return result;
  }
  
  vector conv1d(vector x, vector k) {
    int L = num_elements(x);
    int K = num_elements(k);
    vector[L] y;
    
    // Pad the input vector with zeros on both ends
    vector[L + K - 1] padded_x;
    for (i in 1:(K-1)/2) {
      padded_x[i] = 0;
    }
    for (i in 1:L) {
      padded_x[i + (K-1)/2] = x[i];
    }
    for (i in (L + (K-1)/2 + 1):(L + K - 1)) {
      padded_x[i] = 0;
    }
    
    // Perform the convolution
    for (i in 1:L) {
      y[i] = dot_product(sub_col2(padded_x, i, K), k);
    }
    
    return y;
  }
  
  
  vector time_shift(vector x, real s, real c) {
    vector[30] shift_weights;
    for(i in 0:29) {
      shift_weights[i+1] = exp(neg_binomial_2_lpmf(i | s, c));
    }
    return conv1d(x, shift_weights);
  }
  
  vector hill(vector x, real k) {
    return (x * k) ./ (k + x);
  }
}

data {
  int<lower=0> n_timesteps;
  vector[n_timesteps] depvar;
  int<lower=0> n_channels;
  matrix[n_timesteps, n_channels] channels;
  
  // intercept
  real intercept_lb;
  real intercept_ub;
  real intercept_eta_mean;
  real intercept_eta_scale;
  
  // betas
  real beta_lb;
  real beta_ub;
  real beta_eta_mean;
  real beta_eta_scale;
  
  // kappas
  real kappa_lb;
  real kappa_ub;
  real kappa_eta_mean;
  real kappa_eta_scale;
  
  
  // concentrations
  real conc_lb;
  real conc_ub;
  real conc_eta_mean;
  real conc_eta_scale;
  
  // shifts
  real shift_lb;
  real shift_ub;
  real shift_eta_mean;
  real shift_eta_scale;
  
  // other
  int prior_only;
  
}

parameters {
  real intercept_eta;
  vector[n_channels] beta_eta;
  vector[n_channels] kappa_eta;
  vector[n_channels] conc_eta;
  vector[n_channels] shift_eta;
  real<lower=0> sigma;
}


transformed parameters {
  real intercept = scaled_inv_logit(intercept_lb, intercept_ub, intercept_eta_mean, intercept_eta_scale, intercept_eta);
  vector[n_channels] beta = scaled_inv_logit2(beta_lb, beta_ub, beta_eta_mean, beta_eta_scale, beta_eta);
  vector[n_channels] kappa = scaled_inv_logit2(kappa_lb, kappa_ub, kappa_eta_mean, kappa_eta_scale, kappa_eta);
  vector[n_channels] conc = scaled_inv_logit2(conc_lb, conc_ub, conc_eta_mean, conc_eta_scale, conc_eta);
  vector[n_channels] shift = scaled_inv_logit2(shift_lb, shift_ub, shift_eta_mean, shift_eta_scale, shift_eta);
  matrix[n_timesteps, n_channels] impacts;
  vector[n_timesteps] prediction;
  
  for(i in 1:n_channels) {
    impacts[, i] = beta[i] * channels[, i];
    impacts[, i] = hill(impacts[, i], kappa[i]);
    impacts[, i] = time_shift(impacts[, i], shift[i], conc[i]);
  }
  
  prediction = intercept + impacts * rep_vector(1, n_channels);
  
}

model {
  // priors;
  sigma ~ exponential(5);
  intercept_eta ~ std_normal();
  beta_eta ~ std_normal();
  kappa_eta ~ std_normal();
  conc_eta ~ std_normal();
  shift_eta ~ std_normal();
  
  // likelihood
  if(prior_only != 1) {
    depvar ~ normal(prediction, sigma);
  }
}

generated quantities {
  vector[n_timesteps] predicted = to_vector(normal_rng(prediction, sigma));
}

