library(cmdstanr)
library(dplyr)
library(future.apply)
plan(multisession)

source(here::here("configs", "sbc_sim.r"))

## Stan sampling seeds
set.seed(123)
sample_vect <- sample.int(10000)[1:sim_iter] # Number of data files

# Set executables path
executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide reparameterised SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

simulation <- function(seed) {
    seed_index <- which(sample_vect == seed)

    # Load data
    prior_params <- readRDS(here::here("data/simulated/sbc", data_location, paste(seed_index, "RDS", sep = ".")))
    returns <- prior_params$y_sim
    data_list <- list(T = length(returns), y_sim = returns)

    sv_fit <- mod$sample(
        data = data_list,
        seed = seed,
        chains = chains,
        parallel_chains = parallel_chains,
        refresh = refresh,
        adapt_delta = adapt_delta,
        save_warmup = save_warmup,
        iter_sampling = iter_sampling
    )

    draws <- sv_fit$draws(format = "df")
    params <- draws[, !grepl("lp__|.chain|.iteration|.draw|y_sim|h_std", names(draws))]
    
    rank_stats <- function(parameter){
        sum(params[[parameter]] < prior_params[[parameter]])
    }

    agg_ranks = sapply(names(params), rank_stats,USE.NAMES = TRUE)    
    
    results <- list()
    results[["agg_ranks"]] <- agg_ranks
    results[["seed_index"]] <- seed_index
    results[["seed"]] <- seed
    results[["sv_fit"]] <- sv_fit

    saveRDS(results,
        file = here::here("simulation_output",
            simulation_name,
            "output",
            paste("seed_index",
                seed_index,
                ".RDS",
                sep = "_")))
}

future_lapply(sample_vect, simulation, future.seed = NULL)