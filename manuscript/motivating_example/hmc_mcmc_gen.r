library(dplyr)
library(tidyr)
library(cmdstanr)
library(arrow)
library(posterior)
library(shinystan)

set.seed(1349)

# data_path <- here::here("manuscript/motivating_example/fin_data.parquet")
# raw <- arrow::read_parquet(data_path)
# first_diff <- diff(log(raw$USXUK))
# returns <- (first_diff - mean(first_diff)) * 100

# data_path <- here::here("data/preprocessed/GSPC/20190104_20230201.csv")
data_path <- here::here("data/preprocessed/GSPC/20230101_20230910.csv")
raw <- read.csv(data_path)
log_return <- raw$log_return[complete.cases(raw$log_return)]
returns <- (log_return - mean(log_return)) * 100

model_path <- "models/sv_user_guide_reparameterised_ksc_priors.stan"
mod <- cmdstan_model(model_path, include_paths = here::here("models", "functions"))

data_list <- list(T = length(returns), y = returns, sample_prior = 0)

model_fit <- mod$sample(
    data = data_list,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    adapt_delta = 0.999
  )

draws <- model_fit$draws(variables = c("mu", "phi", "sigma"), format = "df") %>% 
    select(-.chain, -.iteration, -.draw) %>% 
     pivot_longer(everything())

write.csv(draws, file = "manuscript/motivating_example/hmc_draws_2023.csv")