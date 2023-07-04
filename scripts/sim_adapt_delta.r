library(cmdstanr)
library(posterior)
library(parallel)
source(here::here("R", "data.R"))
source(here::here("configs", "adapt_delta_sim.r"))

# Slurm ID
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
ITE = as.numeric(slurm_arrayid)

## Stan sampling seeds
set.seed(123)
sample_vect <- sample.int(10000, 100)
seed <- sample_vect[ITE]

# Set executables path
executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide reparameterised SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

# Get data
if (simulate_data){
  if(manual_seed){
    set.seed(manual_seed)
  } else {
    set.seed(seed)
  }
data <- simulate_ksc(
    T = size,
    phi.true = phi,
    sig.true = sig,
    beta.true = beta
)
} else {
data <- read.csv(here::here("data", data_loc, data_type, paste(data_file_name, ".csv", sep = "")))
}

returns <- data[complete.cases(data[dependent_variable]), dependent_variable]

# Fit model
data_list <- list(T = length(returns), y = returns, sample_prior = sample_prior, gen_quantities = gen_quantities)

sample_model <- function(adapt_delta){

model_fit <- mod$sample(
    data = data_list,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh,
    adapt_delta = adapt_delta,
    save_warmup = save_warmup
)

results = list()
results[["adapt_delta"]] = adapt_delta
results[["divergences"]] = sum(model_fit$diagnostic_summary()$num_divergent)
results[["model_summary"]] <- model_fit$summary(variables = params)
results[["chain1_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 1)))
results[["chain2_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 2)))
results[["chain3_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 3)))
results[["chain4_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 4)))

path <- here::here("simulation_output")
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

saveRDS(results, 
        file = here::here("simulation_output",
                          simulation_name,
                          "output",
                         paste("slurm_id",
                               ITE,
                               "adapt_delta", 
                               adapt_delta, 
                               ".RDS", 
                               sep ="_")))
}


lapply(adapt_delta_list, sample_model)

