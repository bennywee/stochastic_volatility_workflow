get_ranks <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    return(data$one_chain$agg_ranks)
}

rank_chi_sqd <- function(column, expected) {
    return(sum((table(column) - expected)^2 / expected))
}

binning <- function(rank, posterior_draws, bins) {
    return(1 + floor(rank / ((posterior_draws + 1) / bins)))
}

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

facet_hist <- function(data, variables, nbins, expected_bin_count) {
    rank_bins %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
        ggplot(.) +
        geom_histogram(aes(rank), bins = nbins, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * expected_bin_count) +
        facet_wrap(~variable) +
        theme_minimal() +
        geom_hline(yintercept = expected_bin_count, size = 0.5, alpha = 0.3) +
        labs(title = "Distribution of Rank Statistics")
}

dot_plots <- function(data, variables, plot_title) {
    data %>%
        select(all_of(variables)) %>%
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
        labs(title = plot_title,
            x = "Parameters",
            y = "Chi squared stats")
}

chi_sq_hist <- function(data, variables, plot_title) {
    data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "parameter", values_to = "chisq_stat") %>%
        arrange(chisq_stat) %>%
        mutate(parameter = factor(parameter, unique(parameter))) %>%
        ggplot(.) +
        geom_histogram(aes(x = chisq_stat), fill = 'blue', alpha = 0.4) +
        theme_minimal() +
        theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
        labs(title = plot_title,
            y = "Chi squared stats")
}

pval_hist <- function(data, expected_bin_count, plot_title) {
    df <- as.data.frame(as.matrix(lapply(data, pchisq, df = expected_bin_count - 1)))
    names(df) <- c("pvals")
    df$pvals <- as.numeric(df$pvals)

    df %>%
        ggplot(.) +
        geom_histogram(aes(x = pvals)) +
        theme_minimal() +
        theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
        labs(title = plot_title)
}

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

get_rhat_basic <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    return(data$all_chains$rhat_basic)
}

get_rhat <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    return(data$all_chains$rhat)
}

get_ess <- function(rds_path, model_path, ess_type, chains) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    return(data[[chains]][[ess_type]])
}

get_time <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    time <- data$time
    return(c(total_time = time$total))
}

ess_boxplot <- function(data, variables, plot_title) {
    data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "ess") %>%
        ggplot(.) +
        geom_boxplot(aes(x = variable, y = ess)) +
        theme_minimal() +
        labs(title = plot_title)
}
