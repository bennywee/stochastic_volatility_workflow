library(dplyr)
library(ggplot2)
library(future.apply)
library(posterior)
plan(multicore, workers = 9)

# Get all parameter data
path <- "simulation_output/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r3"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")

get_ranks <- function(rds_path) {
    data = readRDS(here::here(paste(path, rds_path, sep = "/")))
    return(data$agg_ranks)
}

results <- lapply(paste("output", rds_list, sep = "/"), get_ranks)

df <- as.data.frame(do.call("rbind", results)) %>% select(-p)

# df %>%
#     group_by(V1) %>%
#     count()

m <- 999
j <- 20

binning <- function(rank, posterior_draws = m, bins = j) {
    return(1 + floor(rank / ((posterior_draws + 1) / bins)))
}

rank_bins <- as.data.frame(lapply(df, binning))

# Facet plots (hist of params)
rank_bins %>%
    select(mu, phi, sigma_sqd, h.1., h.10., h.100., h.500., h.1000.) %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
    ggplot(.) +
    geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
    scale_x_continuous(labels = function(x) x * 50) +
    facet_wrap(~variable) +
    theme_minimal() +
    geom_hline(yintercept = 50, size = 0.5, alpha = 0.3) +
    labs(title = "Distribution of Rank Statistics")

# All in one bins
rank_bins %>%
    tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
    group_by(variable, rank) %>%
    count() %>%
    ggplot(.) +
    geom_line(aes(x = rank, y = n, group = variable), alpha = 0.09, colour = 'blue') +
    theme_minimal() +
    geom_hline(yintercept = 50, size = 1.2) +
    labs(title = "Distribution of Rank Statistics for every parameter")

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
    theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
    labs(title = "Chi squared estimates for rank statistics",
        x = "Parameters",
        y = "Chi squared stats")

# R hats, effective sample size, "time taken to run"
t = readRDS("simulation_output/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r3/output/seed_index_864_.RDS")

load_parameters <- function(rds_path) {
    results <- list()
    data <- readRDS(here::here(paste(path, rds_path, sep = "/")))
    draws <- as.data.frame(data$sv_fit$draws(format = "df"))
    std_h <- unlist(lapply(1:1000, FUN = function(x) {paste("h_std[", x, "]", sep = "")}))
    parameters <- draws[, -which(names(draws) %in% c(std_h, c(".chain", ".iteration", ".draw", "lp__")))]
    results[["parameters"]] <- parameters
    results[["seed_index"]] <- data$seed_index
    return(results)
}

get_rhat <- function(rds_path) {
    simulation_data <- load_parameters(rds_path)
    rhats <- apply(simulation_data$parameters, 2, posterior::rhat_basic)
    return(c(rhats, seed_index = simulation_data$seed_index))
}

get_ess <- function(rds_path) {
    simulation_data <- load_parameters(rds_path)
    ess <- apply(simulation_data$parameters, 2, posterior::ess_basic)
    return(c(ess, seed_index = simulation_data$seed_index))
}

get_time <- function(rds_path) {
    data <- readRDS(here::here(paste(path, rds_path, sep = "/")))
    time <- data$sv_fit$time()$total
    return(c(total_time = time, seed_index = data$seed_index))
}


rhats <- future_lapply(paste("output", rds_list, sep = "/"), get_rhat, future.seed = NULL)
rhat_df <- as.data.frame(do.call("rbind", rhats))

ess <- future_lapply(paste("output", rds_list, sep = "/"), get_ess, future.seed = NULL)
ess_df <- as.data.frame(do.call("rbind", rhats))

times <- future_lapply(paste("output", rds_list, sep = "/"), get_time, future.seed = NULL)
times_df <- as.data.frame(do.call("rbind", times))

results <- lapply(paste("output", rds_list, sep = "/"), get_rhat)
df <- as.data.frame(do.call("rbind", results)) %>% select(-p)

get_rhat("simulation_output/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r3/output/seed_index_1_.RDS")

fit = t$sv_fit
fit$summary()

sort(apply(draws, 2, ess_bulk))
sort(apply(draws, 2, ess_basic))
sort(apply(draws, 2, ess_tail))
sort(apply(draws, 2, rhat_basic))

# ECDF
