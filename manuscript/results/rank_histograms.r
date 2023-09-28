library(tidyverse)
source("R/sbc.r")

n_iterations <- 1000
posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat


# HMC results
# 1000 iterations
rank_type = "agg_ranks"
path <- "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_1000_iterations"

rank_bins_f <- function(path, rank_type, posterior_samples, n_bins){
    rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
    rds_path <- paste("output", rds_list, sep = "/")

    n_iterations <- length(rds_path)
    posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
    n_bins <- 20 # Bins
    expected_count <- n_iterations / n_bins # Chi sqd stat

    results <- lapply(rds_path, get_ranks, model_path = path, rank_type = rank_type)

    if (rank_type == "agg_ranks"){
        df <- as.data.frame(do.call("rbind", results))
        rank_bins <- as.data.frame(lapply(df, binning, posterior_draws = posterior_samples, bins = n_bins))
    } else if(rank_type == "weighted_ranks"){
        df <- as.data.frame(do.call("rbind", results)) * (posterior_samples+1)
        rank_bins <- as.data.frame(lapply(df, binning, posterior_draws = posterior_samples+1, bins = n_bins))
    } else {
        print("error")
    }

    names(df) <- clean_variable_names(df)
    names(rank_bins) <- clean_variable_names(rank_bins)

    rank_bins <- rank_bins[!(names(rank_bins) %in% c("y_sim", "sigma"))]

    return(rank_bins)
}

static_rank_bins <- function(data, variables, type){
    result <- data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>% 
        mutate(type = type)

    return(result)
}

# HMC 1k
hmc_cp_onek_bins <- rank_bins_f(path = "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_1000_iterations", 
                                rank_type = "agg_ranks", 
                                posterior_samples = 999, 
                                n_bins= 20)

hmc_ncp_onek_bins <- rank_bins_f(path = "simulation_output/stan/ncp/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r8_1000_iterations", 
                                rank_type = "agg_ranks", 
                                posterior_samples = 999, 
                                n_bins= 20)

hmc_cp_onek_bin_long <- static_rank_bins(data = hmc_cp_onek_bins, 
                                         variables = c("~mu", "~sigma^2", "~phi"), 
                                         type = "Centered")
hmc_ncp_onek_bin_long <- static_rank_bins(data = hmc_ncp_onek_bins, 
                                          variables = c("~mu", "~sigma^2", "~phi"), 
                                          type = "NonCentered")

hmc_1k = rbind(hmc_cp_onek_bin_long, hmc_ncp_onek_bin_long)

ggplot(hmc_1k) +
        geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        facet_grid(type~variable, labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_count, linewidth = 0.5, alpha = 0.3) +
        labs(y = "Count",
             x = "Ranks")

ggsave("manuscript/results/hmc_1k.png", bg = "white", width = 14, height = 9.42)

# HMC 5k
n_iterations <- 5000
posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

hmc_cp_fivek_bins <- rank_bins_f(path = "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_5000_iterations", 
                                rank_type = "agg_ranks", 
                                posterior_samples = 999, 
                                n_bins= 20)

hmc_ncp_fivek_bins <- rank_bins_f(path = "simulation_output/stan/ncp/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r9_5000_iterations", 
                                rank_type = "agg_ranks", 
                                posterior_samples = 999, 
                                n_bins= 20)

hmc_cp_fivek_bin_long <- static_rank_bins(data = hmc_cp_fivek_bins, 
                                         variables = c("~mu", "~sigma^2", "~phi"), 
                                         type = "Centered")
hmc_ncp_fivek_bin_long <- static_rank_bins(data = hmc_ncp_fivek_bins, 
                                          variables = c("~mu", "~sigma^2", "~phi"), 
                                          type = "NonCentered")

hmc_5k = rbind(hmc_cp_fivek_bin_long, hmc_ncp_fivek_bin_long)

ggplot(hmc_5k) +
        geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        facet_grid(type~variable, labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_count, linewidth = 0.5, alpha = 0.3) +
        labs(x = "Ranks",
             y = "Count")

ggsave("manuscript/results/hmc_5k.png", bg = "white", width = 14, height = 9.42)

# KSC 1k
n_iterations <- 1000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ksc_cp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_1000iter_r2", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

ksc_cp_onek_bins_long <- static_rank_bins(data = ksc_cp_onek_bins, 
                                         variables = c("~mu", "~sigma^2", "~phi"), 
                                         type = "Centered")


ggplot(ksc_cp_onek_bins_long) +
        geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        facet_grid(~variable, labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_count, linewidth = 0.5, alpha = 0.3) +
        labs(x = "Ranks", 
             y = "Count")

ggsave("manuscript/results/ksc_1k.png", bg = "white", width = 14, height = 5.42)

# SIR 1k
n_iterations <- 1000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

sir_ksc_cp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sir_sbc_cp_ksc_model_cp_dgf_10kmcmc_1000iter_r3", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

sir_ksc_cp_onek_bins_long <- static_rank_bins(data = sir_ksc_cp_onek_bins, 
                                         variables = c("~mu", "~sigma^2", "~phi"), 
                                         type = "Centered")

ggplot(sir_ksc_cp_onek_bins_long) +
        geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        facet_grid(~variable, labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_count, linewidth = 0.5, alpha = 0.3) +
        labs(x = "Ranks",
             y = "Count")

ggsave("manuscript/results/sir_1k.png", bg = "white", width = 14, height = 5.42)

# SIR 5k
n_iterations <- 5000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

sir_ksc_cp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sir_sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

sir_ksc_cp_fivek_bins_long <- static_rank_bins(data = sir_ksc_cp_fivek_bins, 
                                         variables = c("~mu", "~sigma^2", "~phi"), 
                                         type = "Centered")

ggplot(sir_ksc_cp_fivek_bins_long) +
        geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        facet_grid(~variable, labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_count, linewidth = 0.5, alpha = 0.3) +
        labs(x = "Ranks",
             y = "Count")

ggsave("manuscript/results/sir_5k.png", bg = "white", width = 14, height = 5.42)



function(data, variables, nbins, expected_bin_count, rank_scales) {
    data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
        ggplot(.) +
        geom_histogram(aes(rank), bins = nbins, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * rank_scales/nbins) +
        facet_wrap(~variable, labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_bin_count, linewidth = 0.5, alpha = 0.3) +
        labs(title = "Distribution of Rank Statistics",
                y = "Count",
                x = "Ranks")
}


facet_hist(data = rank_bins, 
           variables = static_parameters, 
           expected_bin_count = expected_count, 
           nbins = n_bins,
           rank_scales = (posterior_samples+1)
           )
    

