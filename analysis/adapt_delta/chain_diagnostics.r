library(cmdstanr)
library(posterior)
library(parallel)
source(here::here("R", "data.R"))
source(here::here("configs", "adapt_delta_sim.r"))

## Stan sampling seeds
set.seed(123)
sample_vect <- sample.int(10000, 100)
seed <- sample_vect[14]

# Set executables path
executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide reparameterised SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

data <- read.csv(here::here("data", data_loc, data_type, paste(data_file_name, ".csv", sep = "")))

returns <- data[complete.cases(data[dependent_variable]), dependent_variable]

# Fit model
data_list <- list(T = length(returns), y = returns, sample_prior = sample_prior, gen_quantities = gen_quantities)

model_fit <- mod$sample(
    data = data_list,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh,
    adapt_delta = 0.905,
    save_warmup = save_warmup
)



