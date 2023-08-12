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

    nuisance_vars = "lp__|.chain|.iteration|.draw|y_sim|h_std|sigma$"
    parameters <- names(prior_params)[!grepl(nuisance_vars, names(prior_params))]
    diagnostics <- c("rhat", "rhat_basic", "ess_bulk", "ess_tail", "ess_basic")
    all_chains_diagnostics <- posterior::summarise_draws(sv_fit$draws(variables = parameters), diagnostics)

    draws_df <- as.data.frame(sv_fit$draws(variables = parameters, format = "df"))
    draws_one_chain <- draws_df[draws_df[".chain"] == 1, !grepl(nuisance_vars, names(draws_df))]
    all_draws <- draws_df[, !grepl(nuisance_vars, names(draws_df))]
    
    rank_stats <- function(parameter, draws, prior_parameters) {
        sum(draws[[parameter]] < prior_parameters[[parameter]])
    }

    ess_basic <- sapply(draws_one_chain, posterior::ess_basic, USE.NAMES = TRUE)
    ess_bulk <- sapply(draws_one_chain, posterior::ess_bulk, USE.NAMES = TRUE)
    ess_tail <- sapply(draws_one_chain, posterior::ess_tail, USE.NAMES = TRUE)

    one_chain_diagnostic <- as.data.frame(cbind(ess_bulk, ess_tail, ess_basic))
    one_chain_diagnostic["variable"] <- rownames(one_chain_diagnostic)
    rownames(one_chain_diagnostic) <- NULL

    results[["all_chains"]][["agg_ranks"]] <- sapply(parameters, rank_stats, draws = all_draws, prior_parameters = prior_params, USE.NAMES = TRUE)
    results[["one_chain"]][["agg_ranks"]] <- sapply(parameters, rank_stats, draws = draws_one_chain, prior_parameters = prior_params, USE.NAMES = TRUE)
    results[["all_chains"]][["diagnostics"]] <- all_chains_diagnostics
    results[["one_chain"]][["diagnostics"]] <- one_chain_diagnostic
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
