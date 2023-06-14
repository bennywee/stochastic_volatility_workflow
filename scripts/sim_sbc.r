library(cmdstanr)
library(dplyr)

source(here::here("configs", "sbc_sim.r"))

# Slurm ID
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
ITE = as.numeric(slurm_arrayid)

## Stan sampling seeds
set.seed(123)
sample_vect <- sample.int(10000, iter_sampling)
seed <- sample_vect[ITE]

# Set executables path
executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide reparameterised SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

sv_fit <- mod$sample(
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh,
    adapt_delta = adapt_delta,
    save_warmup = save_warmup,
    iter_sampling = iter_sampling
)

ranks <- sv_fit$draws(variables = c('sim_ranks'), format = 'df') %>%
    select(-c(.chain, .iteration, .draw))

names(ranks) <- params
agg_ranks <- apply(ranks, 2, FUN = sum)

saveRDS(agg_ranks,
    file = here::here("simulation_output",
        simulation_name,
        "output",
        paste("slurm_id",
            ITE,
            ".RDS",
            sep = "_")))
