
library(dplyr)
library(ggplot2)
library(tidyr)


emissions <- read.csv("data/emissions.csv")

str(emissions)

colSums(is.na(emissions))

head(emissions)

emissions <- emissions %>%
  mutate(year_centered = year - mean(year, na.rm = TRUE))

emissions <- emissions %>%
  mutate(commodity = as.factor(commodity))

summary(emissions)


library(rstan)


stan_model_code <- "
data {
  int<lower=0> N;                  // Number of data points
  int<lower=1> K;                  // Number of commodities
  vector[N] year_centered;         // Centered year variable
  matrix[N, K] X_commodity;        // Commodity dummy matrix
  vector[N] emissions;             // Total emissions
}

parameters {
  real beta_0;                     // Intercept
  real beta_year;                  // Slope for year
  vector[K] beta_commodity;        // Coefficients for commodities
  real<lower=0> sigma;             // Standard deviation of the error term
}

model {
  // Priors
  beta_0 ~ normal(0, 100);         // Weak prior for intercept
  beta_year ~ normal(0, 10);       // Weak prior for year slope
  beta_commodity ~ normal(0, 10);  // Weak priors for commodity effects
  sigma ~ cauchy(0, 2);            // Weakly informative prior for sigma

  // Likelihood
  emissions ~ normal(beta_0 + beta_year * year_centered + X_commodity * beta_commodity, sigma);
  
  // Log-likelihood
  target += normal_lpdf(emissions | beta_0 + beta_year * year_centered + X_commodity * beta_commodity, sigma);
}



generated quantities {
  vector[N] y_rep;  // Posterior predictions for emissions

  for (n in 1:N) {
    y_rep[n] = normal_rng(beta_0 + beta_year * year_centered[n] + dot_product(X_commodity[n], beta_commodity), sigma);
  }
}
"

commodity_dummies <- model.matrix(~ commodity - 1, data = emissions)

stan_data <- list(
  N = nrow(emissions),
  K = ncol(commodity_dummies),
  year_centered = emissions$year_centered,
  X_commodity = commodity_dummies,
  emissions = emissions$total_emissions_MtCO2e
)


stan_model <- stan_model(model_code = stan_model_code)

fit <- sampling(stan_model, data = stan_data, iter = 2000, chains = 4, seed = 123)

print(fit, pars = c("beta_0", "beta_year", "beta_commodity", "sigma"))

library(bayesplot)

posterior_predictions <- extract(fit, pars = "y_rep")$y_rep  # Assuming `y_rep` is modeled in Stan

# 1. Density overlay: Compare observed and predicted emissions
ppc_dens_overlay(y = emissions$total_emissions_MtCO2e, yrep = posterior_predictions[1:100, ])

# 2. Predicted vs Observed: Scatter plot
ppc_scatter_avg(y = emissions$total_emissions_MtCO2e, yrep = posterior_predictions)

# 3. Summary statistics: Mean and standard deviation checks
ppc_stat(y = emissions$total_emissions_MtCO2e, yrep = posterior_predictions, stat = "mean")
ppc_stat(y = emissions$total_emissions_MtCO2e, yrep = posterior_predictions, stat = "sd")

mcmc_areas(fit, regex_pars = "beta_commodity\\[")

y_pred_mean <- apply(posterior_predictions, 2, mean)
plot(emissions$total_emissions_MtCO2e, y_pred_mean,
     xlab = "Observed Emissions", ylab = "Predicted Emissions",
     main = "Observed vs Predicted Emissions")
abline(a = 0, b = 1, col = "red")


library(loo)
log_lik <- extract_log_lik(fit)
loo_result <- loo(log_lik)
print(loo_result)
