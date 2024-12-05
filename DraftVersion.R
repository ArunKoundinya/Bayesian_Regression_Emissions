
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
