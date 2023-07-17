get_divergence <- function(rds_path) {
    data = readRDS(here::here(paste(path, rds_path, sep = "/")))
    return(c(data$adapt_delta, data$divergences, data$seed))
}

write_sbc_data <- function(iter, sim_param, sim_data) {
    sim_results <- list()
    data <- as.data.frame(t(sim_data[iter, ]))
    row.names(data) <- 1:dim(data)[1]
    names(data) <- c("y_sim")
    sim_results[["data"]] <- data
    sim_results[["parameters"]] <- sim_param[iter, ]
    saveRDS(sim_results, file = paste(data_path, paste(iter, "RDS", sep = "."), sep = "/"))
}
