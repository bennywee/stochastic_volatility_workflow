library(cmdstanr)
library(dplyr)
library(future.apply)
plan(multisession)

source(here::here("configs", "sbc_sim.r"))

## Stan sampling seeds
set.seed(123)
sample_vect <- sample.int(1e6)[1:sim_iter] # Number of data files

# Set executables path
executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

# Stan User guide reparameterised SV model
# Compile stan model
file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

simulation <- function(seed) {
    seed_index <- which(sample_vect == seed)
    results <- list()
    results[["all_chains"]] <- list()
    results[["one_chain"]] <- list()

    # Load data
    prior_params <- readRDS(here::here("data/simulated/sbc", data_location, paste(seed_index, "RDS", sep = ".")))
    parameters <- names(prior_params)
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

    draws <- as.data.frame(sv_fit$draws(format = "df"))
    nuisance_vars = "lp__|.chain|.iteration|.draw|y_sim|h_std"
    all_draws <- draws[, !grepl(nuisance_vars, names(draws))]
    draws_one_chain <- draws[draws[".chain"] == 1, !grepl(nuisance_vars, names(draws))]

    rank_stats <- function(parameter, draws, prior_parameters) {
        sum(draws[[parameter]] < prior_parameters[[parameter]])
    }

    results[["all_chains"]][["agg_ranks"]] <- sapply(parameters, rank_stats, draws = all_draws, prior_parameters = prior_params, USE.NAMES = TRUE)
    results[["all_chains"]][["rhat_basic"]] <- sapply(all_draws, posterior::rhat_basic, USE.NAMES = TRUE)
    results[["all_chains"]][["rhat"]] <- sapply(all_draws, posterior::rhat, USE.NAMES = TRUE)
    results[["all_chains"]][["ess_basic"]] <- sapply(all_draws, posterior::ess_basic, USE.NAMES = TRUE)
    results[["all_chains"]][["ess_bulk"]] <- sapply(all_draws, posterior::ess_bulk, USE.NAMES = TRUE)
    results[["all_chains"]][["ess_tail"]] <- sapply(all_draws, posterior::ess_tail, USE.NAMES = TRUE)

    results[["one_chain"]][["agg_ranks"]] <- sapply(parameters, rank_stats, draws = draws_one_chain, prior_parameters = prior_params, USE.NAMES = TRUE)
    results[["one_chain"]][["ess_basic"]] <- sapply(draws_one_chain, posterior::ess_basic, USE.NAMES = TRUE)
    results[["one_chain"]][["ess_bulk"]] <- sapply(draws_one_chain, posterior::ess_bulk, USE.NAMES = TRUE)
    results[["one_chain"]][["ess_tail"]] <- sapply(draws_one_chain, posterior::ess_tail, USE.NAMES = TRUE)

    results[["seed_index"]] <- seed_index
    results[["seed"]] <- seed
    results[["diagnostic_smmary"]] <- sv_fit$diagnostic_summary()
    results[["time"]] <- sv_fit$time()

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
