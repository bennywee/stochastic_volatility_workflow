library(jsonlite)
library(dplyr)
library(tidyr)
library(arrow)
library(posterior)
library(future.apply)
plan(multisession)

source("R/ksc_results.r")

config <- jsonlite::read_json("configs/ksc.json")
tmp_data_location <- paste("simulation_output/", config$simulation_name, "/tmp", sep ="") 

file_metadata <- as.data.frame(list.files("analysis/gauss_mix/data"))
names(file_metadata) <- c("files")

file_metadata <- tidyr::separate(file_metadata, 
                     col = files, 
                     sep = "_", 
                     into = c("dataset", "mcmc_seed"),
                     remove = FALSE) %>% 
    tidyr::separate(., col = mcmc_seed, sep = "\\.", into = c("mcmc_seed", "parquet"), remove = TRUE) %>% 
    select(-parquet)

    results <- list()
    results[["all_chains"]] <- list()
    results[["one_chain"]] <- list()

    iteration_list <- list()
    iter_files <- file_metadata[["files"]]
    iter_files_path <- paste("analysis/gauss_mix/data", "/", iter_files, sep = "")
    iteration_list <- lapply(iter_files_path, arrow::read_parquet, as_data_frame=TRUE)
    parameter_names <- names(iteration_list[[1]])

    all_chains = sapply(parameter_names, combine_chains, list_obj = iteration_list, simplify=FALSE)
    names(all_chains)[names(all_chains) == "sigma2"] <- "sigma_sqd"

    post_weights <- all_chains[['weights']] %>% 
            tibble::rowid_to_column("index") %>% 
            pivot_longer(cols = -c(index),  names_to = "chain", values_to = "weights")

    all_chains <- within(all_chains, rm(weights))
    
    one_chain = sapply(all_chains, one_chain, simplify=FALSE)

    # All chain diagnostics
    all_chains_diagnostics <- sapply(all_chains, diagnostics, simplify=FALSE)

    # One chain diagnostics
    ess_basic <- sapply(one_chain, posterior::ess_basic, USE.NAMES = TRUE)
    ess_bulk <- sapply(one_chain, posterior::ess_bulk, USE.NAMES = TRUE)
    ess_tail <- sapply(one_chain, posterior::ess_tail, USE.NAMES = TRUE)
    one_chain_diagnostic <- as.data.frame(cbind(ess_bulk, ess_tail, ess_basic))
    one_chain_diagnostic["variable"] <- rownames(one_chain_diagnostic)
    rownames(one_chain_diagnostic) <- NULL
    
    # Append results
    results[["all_chains"]][["diagnostics"]] <- all_chains_diagnostics
    results[["one_chain"]][["diagnostics"]] <- one_chain_diagnostic
    

# saveRDS(results, "analysis/gauss_mix/results/results_real_data.rds")

library(tseries)
print(acf(iteration_list[[1]]$mu,lag=20,pl=FALSE))
print(acf(iteration_list[[1]]$sigma2,lag=20,pl=FALSE))
print(acf(iteration_list[[1]]$phi,lag=20,pl=FALSE))
print(acf(iteration_list[[2]]$mu,lag=20,pl=FALSE))
print(acf(iteration_list[[2]]$sigma2,lag=20,pl=FALSE))
print(acf(iteration_list[[2]]$phi,lag=20,pl=FALSE))

cov(cbind(iteration_list[[1]]$mu,iteration_list[[1]]$sigma2,iteration_list[[1]]$phi))

plot(iteration_list[[1]]$mu, type = 'l')
plot(iteration_list[[1]]$sigma2, type = 'l')
plot(iteration_list[[1]]$phi, type = 'l')
plot(iteration_list[[1]]$`h[54]`, type = 'l')

plot(iteration_list[[1]]$mu, type = 'l')
plot(iteration_list[[2]]$sigma2, type = 'l')
plot(iteration_list[[2]]$phi, type = 'l')
plot(iteration_list[[1]]$`h[54]`, type = 'l')

sigma2 = cbind(iteration_list[[1]]$sigma2,iteration_list[[2]]$sigma2,iteration_list[[3]]$sigma2,iteration_list[[4]]$sigma2)

sigma2 %>% 
as.data.frame() %>% 
mutate(index = row_number()) %>% 
pivot_longer(cols = starts_with("V")) %>% 
ggplot(.) +
geom_line(aes(x=index, y =value, colour = name))
