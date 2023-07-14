library(dplyr)
library(ggplot2)
source(here::here("R", "simulations.R"))

path <- "simulation_output/adapt_delta_0.9_0.99_0.005_sigsqd_prior_single_dataset_r2"

rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")

get_chains <- function(rds_path) {
    data = readRDS(here::here(paste(path, rds_path, sep = "/")))
    return(c(dim(data$chain1_summary), dim(data$chain2_summary), dim(data$chain3_summary), dim(data$chain4_summary)))
}

divergence_list <- lapply(paste("output", rds_list, sep = "/"), get_chains)

for (i in 1:length(divergence_list)) {
    if (length(divergence_list[[i]]) < 8) {
        print(c(i, length(divergence_list[[i]])))
    }
}

test = readRDS("simulation_output/adapt_delta_0.9_0.99_0.005_sigsqd_prior_single_dataset/metadata.RDS")
test
writeLines(test$config)
