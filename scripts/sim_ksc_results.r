library(jsonlite)
library(dplyr)
library(tidyr)
library(arrow)
library(posterior)
library(future.apply)
plan(multisession)

config <- jsonlite::read_json("configs/ksc.json")
tmp_data_location <- paste("simulation_output/", config$simulation_name, "/tmp", sep ="") 

file_metadata <- as.data.frame(list.files(tmp_data_location))
names(file_metadata) <- c("files")

file_metadata <- separate_wider_delim(file_metadata, 
                     cols = files, 
                     delim = "_", 
                     names = c("dataset", "mcmc_seed"), 
                     cols_remove = FALSE) %>% 
    separate_wider_delim(., cols = mcmc_seed, delim = ".", names = c("mcmc_seed", "parquet")) %>% 
    select(-parquet)

simulation_results <- function(file_number){
    results <- list()
    results[["all_chains"]] <- list()
    results[["one_chain"]] <- list()

    iteration_list <- list()
    iter_files <- file_metadata[file_metadata["dataset"] == file_number, ][["files"]]
    iter_files_path <- paste(tmp_data_location, "/", iter_files, sep = "")
    iteration_list <- lapply(iter_files_path, arrow::read_parquet, as_data_frame=TRUE)
    parameter_names <- names(iteration_list[[1]])

    combine_chains <- function(parameter, list_obj){
        parameter_df = as.data.frame(cbind(
        list_obj[[1]][parameter],
        list_obj[[2]][parameter],
        list_obj[[3]][parameter],
        list_obj[[4]][parameter]
            )
        )

        names(parameter_df) <- c("chain_1", "chain_2", "chain_3", "chain_4")

        return(parameter_df)
    }


    all_chains = sapply(parameter_names, combine_chains, list_obj = iteration_list, simplify=FALSE)
    names(all_chains)[names(all_chains) == "sigma2"] <- "sigma_sqd"

    one_chain <- function(df){
        return(df$chain_1)
    }

    one_chain = sapply(all_chains, one_chain, simplify=FALSE)

    diagnostics <- function(dataframe){
        rhat <- posterior::rhat(as.matrix(dataframe))
        rhat_basic <- posterior::rhat_basic(as.matrix(dataframe))
        ess_bulk <- posterior::ess_bulk(as.matrix(dataframe))
        ess_tail <- posterior::ess_tail(as.matrix(dataframe))
        ess_basic <- posterior::ess_basic(as.matrix(dataframe))

        results = as.data.frame(list(
                           rhat=rhat, 
                           rhat_basic=rhat_basic, 
                           ess_bulk=ess_bulk,
                           ess_tail=ess_tail,
                           ess_basic=ess_basic))

        return(results)
    }

    all_chains_diagnostics <- sapply(all_chains, diagnostics, simplify=FALSE)

    # One chain diagostics
    ess_basic <- sapply(one_chain, posterior::ess_basic, USE.NAMES = TRUE)
    ess_bulk <- sapply(one_chain, posterior::ess_bulk, USE.NAMES = TRUE)
    ess_tail <- sapply(one_chain, posterior::ess_tail, USE.NAMES = TRUE)
    one_chain_diagnostic <- as.data.frame(cbind(ess_bulk, ess_tail, ess_basic))
    one_chain_diagnostic["variable"] <- rownames(one_chain_diagnostic)
    rownames(one_chain_diagnostic) <- NULL

    # Rank statistics
    rank_stats <- function(parameter, draws, prior_parameters) {
            sum(draws[[parameter]] < prior_parameters[[parameter]])
        }

    data_location <- here::here("data/simulated/sbc", config$data_location, paste(file_number, "json", sep = "."))
    prior_params <- jsonlite::read_json(data_location)

    results[["all_chains"]][["agg_ranks"]] <- sapply(names(all_chains), rank_stats, draws = all_chains, prior_parameters = prior_params, USE.NAMES = TRUE)
    results[["one_chain"]][["agg_ranks"]] <- sapply(names(one_chain), rank_stats, draws = one_chain, prior_parameters = prior_params, USE.NAMES = TRUE)
    results[["all_chains"]][["diagnostics"]] <- all_chains_diagnostics
    results[["one_chain"]][["diagnostics"]] <- one_chain_diagnostic
    results[["config"]] <- config
    results[["seed_files"]] <- file_metadata[file_metadata["dataset"] == file_number, ]

    saveRDS(results,
            file = here::here("simulation_output",
                config$simulation_name,
                "output",
                paste("seed_index",
                    file_number,
                    ".RDS",
                    sep = "_")))
}

datasets <- unique(file_metadata$dataset)
future_lapply(datasets, simulation_results, future.seed = NULL)
