library(tidyverse)
library(cmdstanr)
library(bayesplot)
library(posterior)

source(here::here("configs/adapt_delta_sim.r"))
source(here::here("R", "plots.R"))
source(here::here("R", "model_eval.R"))

executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

# Get data
data <- read.csv(here::here("data", data_loc, data_type, paste(data_file_name, ".csv", sep = "")))
returns <- data[complete.cases(data[dependent_variable]), dependent_variable]

# Fit model
data_list <- list(T = length(returns), y = returns, sample_prior = sample_prior, gen_quantities = gen_quantities)
# adapt_delta_list <- seq(0.94, 0.96, 0.001)
adapt_delta_list <- seq(0.94, 0.96, 0.01)

model_fit <- mod$sample(
    data = data_list,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh,
    adapt_delta = adapt_delta,
    save_warmup = save_warmup
)

params = c('mu',
    'phi',
    'sigma',
    'h[1]',
    'h[100]',
    'h[200]',
    'h[300]',
    'h[400]',
    'h[500]',
    'h[600]',
    'h[700]',
    'h[800]',
    'h[900]',
    'h[1000]'
)

results = list()
results[["adapt_delta"]] = adapt_delta
results[["divergences"]] = sum(model_fit$diagnostic_summary()$num_divergent)
results[["model_summary"]] <- model_fit$summary(variables = params)
results[["chain1_summary"]] <- summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 1))
results[["chain2_summary"]] <- summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 2))
results[["chain3_summary"]] <- summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 3))
results[["chain4_summary"]] <- summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 4))

saveRDS(results, file = "results.RDS")
