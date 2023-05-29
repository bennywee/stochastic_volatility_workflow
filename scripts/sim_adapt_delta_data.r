library(cmdstanr)
library(posterior)
library(parallel)
source(here::here("R", "data.R"))

# Slurm ID
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
ITE = as.numeric(slurm_arrayid)
############################ Parameters to set ############################

# Model training
## Model metadata
model_name <- "sv_user_guide_reparameterised_ksc_priors" 
unique_identifier <- ""

## Data location
data_loc <- "simulated"
data_type <- "ksc"
data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733"
dependent_variable <- "yobs"

## Stan sampling
set.seed(123)
sample_vect <- sample.int(10000, 100)
seed <- sample_vect[ITE]
set.seed(seed)

chains <- 4
parallel_chains <- 4
refresh <- 500
save_warmup <- FALSE
gen_quantities <- 0
sample_prior <- 0
adapt_delta_list <- seq(0.90, 0.99, 0.005)

############################ Parameters to set ############################

# Set executables path
executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide reparameterised SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

# Get data
size <- 1000
phi <- 0.97779
sig <- 0.15850
beta <- 0.64733

data <- simulate_ksc(
    T = size,
    phi.true = phi,
    sig.true = sig,
    beta.true = beta
)

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
results[["seed"]] = seed
results[["divergences"]] = sum(model_fit$diagnostic_summary()$num_divergent)
results[["model_summary"]] <- model_fit$summary(variables = params)
results[["chain1_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 1)))
results[["chain2_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 2)))
results[["chain3_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 3)))
results[["chain4_summary"]] <- try(summarise_draws(subset_draws(model_fit$draws(variables = params), chain = 4)))
results[['script']] <- readLines(here::here("scripts", "sim_adapt_delta_data.r"))

path <- here::here("simulation_output")
if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

saveRDS(results, 
        file = here::here("simulation_output", 
                         paste("slurm_id",
                               ITE,
                               "adapt_delta", 
                               adapt_delta, 
                               "seed_dataset_100",
                               ".RDS", 
                               sep ="_")))
}


lapply(adapt_delta_list, sample_model)
