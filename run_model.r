library(cmdstanr)
library(bayesplot)

# Stan User guide SV model
# Compile stan model
file <- file.path("models", "sv_user_guide.stan")
mod <- cmdstan_model(file)

# Get data
data <- read.csv("data/preprocessed/GSPC/20100104_20230101.csv")
log_returns  <-  data[complete.cases(data['log_return']), 'log_return']

# Fit model
data_list <- list(T = 3271, y = log_returns)
fit <- mod$sample(
  data = data_list, 
  seed = 123, 
  chains = 2, 
  parallel_chains = 2,
  refresh = 500 # print update every 500 iters
)

fit$summary()
mcmc_hist(fit$draws("mu"))
