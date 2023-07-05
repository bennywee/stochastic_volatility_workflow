library(cmdstanr)
library(dplyr)

source(here::here("configs", "sbc_sim.r"))

# Slurm ID
slurm_arrayid <- Sys.getenv('SLURM_ARRAY_TASK_ID')
ITE = as.numeric(slurm_arrayid)

## Stan sampling seeds
set.seed(123)
sample_vect <- sample.int(10000)
seed <- sample_vect[ITE]

# Set executables path
executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide reparameterised SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

attempts <- 1

while(attempts < max_retries){
    sv_fit <- mod$sample(
        seed = seed,
        chains = chains,
        parallel_chains = parallel_chains,
        refresh = refresh,
        adapt_delta = adapt_delta,
        save_warmup = save_warmup,
        iter_sampling = iter_sampling
    )

    ranks_stats <- function(model_fit){
        sim_ranks <- model_fit$draws(variables = c('sim_ranks'), format = 'df') %>%
            select(-c(.chain, .iteration, .draw))

        return(sim_ranks)
    }

    ranks <- try(ranks_stats(model_fit = sv_fit), silent= FALSE)

    if(!is(ranks, 'try-error')){
        break
    } else {
        attempts <- attempts + 1
    }
}

if(attempts == max_retries) stop(paste("Model failed to sample after", max_retries, "attempts"))

names(ranks) <- params
agg_ranks <- apply(ranks, 2, FUN = sum)

results <- list()
results[["agg_ranks"]] <- agg_ranks
results[["array_id"]] <- ITE
results[["seed"]] <- seed
results[["attempts"]] <- attempts

saveRDS(results,
    file = here::here("simulation_output",
        simulation_name,
        "output",
        paste("slurm_id",
            ITE,
            ".RDS",
            sep = "_")))
