library(dplyr)
library(ggplot2)

# Get all parameter data
path <- "simulation_output/sbc_ncp_ksc_priors_ig_sd_0.999_adapt_delta_r2"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")

get_ranks <- function(rds_path) {
    data = readRDS(here::here(paste(path, rds_path, sep = "/")))
    return(data$agg_ranks)
}

results <- lapply(paste("output", rds_list, sep = "/"), get_results)

df <- as.data.frame(do.call("rbind", results))

# df %>%
#     group_by(V1) %>%
#     count()

m <- 999
j <- 20

binning <- function(rank, posterior_draws = m, bins = j) {
    return(1 + floor(rank / ((posterior_draws + 1) / bins)))
}

rank_bins <- as.data.frame(lapply(df, binning))

rank_bins %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
    ggplot(.) +
    geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
    scale_x_continuous(labels = function(x) x * 50) +
    facet_wrap(~variable) +
    theme_minimal() +
    labs(title = "Distribution of Rank Statistics (IG prior on SD)")


# Chi sqd stat
e = 50

rank_chi_sqd <- function(column, expected = e) {
    return(sum((table(column) - expected)^2 / expected))
}

ranks_stats <- as.data.frame(lapply(rank_bins, rank_chi_sqd))


# Percentile of chi sqd with J-1 DoF on y axis. Parameters on x axis
ranks_stats %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "chisq_stat") %>%
    ggplot(.) +
    geom_point(aes(x = variable, y = chisq_stat), size = 3) +
    labs(title = "Chi squared estimates for rank statistics")

ranks_stats %>%
    tidyr::pivot_longer(everything(), names_to = "parameter", values_to = "chisq_stat") %>%
    arrange(chisq_stat) %>%
    mutate(parameter = factor(parameter, unique(parameter))) %>%
    ggplot(.) +
    geom_segment(aes(x = parameter, xend = parameter, y = 0, yend = chisq_stat), color = "grey") +
    geom_point(aes(x = parameter, y = chisq_stat), size = 3, colour = "light blue") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Chi squared estimates for rank statistics (IG prior on SD)",
        x = "Parameters",
        y = "Chi squared stats")
