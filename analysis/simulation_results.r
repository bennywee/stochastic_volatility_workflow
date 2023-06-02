library(ggplot2)
path <- "simulation_output/adapt_delta_0.9_0.99_0.005_seed_dataset_100"
rds_list <- list.files(path = path, pattern = ".*RDS")

get_divergence <- function(rds_path) {
    data = readRDS(here::here(paste(path,rds_path, sep="/")))
    return(c(data$adapt_delta, data$divergences, data$seed))
}

divergence_list <- lapply(rds_list, get_divergence)
df <- as.data.frame(do.call("rbind", divergence_list))
names(df) <- c("adapt_delta", "divergences", "seed")

ggplot(df, aes(x = adapt_delta, y = divergences)) +
    geom_point() + scale_y_log10() 

ggplot(df, aes(x = factor(adapt_delta), y = divergences)) +
    geom_boxplot() + scale_y_log10() 

ggplot(df, aes(x = factor(adapt_delta), y = divergences)) +
    geom_boxplot()


# Plot proportion of 0s
df %>% filter(divergences > 900)
