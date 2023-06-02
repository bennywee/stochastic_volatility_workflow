get_divergence <- function(rds_path) {
    data = readRDS(here::here(paste(path,rds_path, sep="/")))
    return(c(data$adapt_delta, data$divergences, data$seed))
}