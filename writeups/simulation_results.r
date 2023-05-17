library(ggplot2)
path <- "simulation_output/adapt_delta_0.90_0.99_0.001"
rds_list <- list.files(path = path, pattern = ".*RDS")

get_divergence <- function(rds_path) {
    data = readRDS(here::here(paste(path,rds_path, sep="/")))
    return(c(data$adapt_delta, data$divergences))
}

divergence_list <- lapply(rds_list, get_divergence)
df <- as.data.frame(do.call("rbind", divergence_list))
names(df) <- c("adapt_delta", "divergences")

ggplot(df, aes(x = adapt_delta, y = divergences)) +
    geom_point() + scale_y_log10() 

ggplot(df, aes(x = factor(adapt_delta), y = divergences)) +
    geom_boxplot() + scale_y_log10() 

