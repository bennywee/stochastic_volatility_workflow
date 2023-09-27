library(tidyverse)
source("R/sbc.r")

n_iterations <- 1000
posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat


# HMC CENTERED results
# 1000 iterations
rank_type = "agg_ranks"
path <- "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_1000_iterations"

hmc_cp_onek_bins <- rank_bins_f(path = "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_1000_iterations", 
                                rank_type = "agg_ranks", 
                                posterior_samples = 999, 
                                n_bins= 20)

hmc_cprank_stats <- as.data.frame(lapply(hmc_cp_onek_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())
hmc_cprank_stats$name <- names(hmc_cp_onek_bins)
hmc_cprank_stats$type <- "hmc_cp"

# KSC CENTERED results
n_iterations <- 1000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ksc_cp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_1000iter_r2", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)
ksc_cp_rank_stats <- as.data.frame(lapply(ksc_cp_onek_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())

ksc_cp_rank_stats$name <- names(ksc_cp_onek_bins)
ksc_cp_rank_stats$type <- "ksc_cp"



# SIR 1k
n_iterations <- 1000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

sir_ksc_cp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sir_sbc_cp_ksc_model_cp_dgf_10kmcmc_1000iter_r3", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

sir_rank_stats <- as.data.frame(lapply(sir_ksc_cp_onek_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())

sir_rank_stats$name <- names(sir_ksc_cp_onek_bins)
sir_rank_stats$type <- "sir"


df <- rbind(hmc_cprank_stats, ksc_cp_rank_stats, sir_rank_stats)

ggplot(df) +
    geom_histogram(aes(x = value, fill = type), alpha = 0.4, bins = 200) +
        theme_minimal(base_size = 22) +
        theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())  +
        labs(title = "Distribution of chi squared statistics")

ggsave("manuscript/results/dist_chisq.png", bg = "white", width = 14, height = 8.42)

        # geom_histogram(aes(value), bins = 20, fill = "light blue", alpha = 0.7) +
        # scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        # facet_grid(~variable, labeller = label_parsed) +
        # theme_minimal(base_size = 20) +
        # theme(strip.text.x = element_text(size = 22)) +
        # geom_hline(yintercept = expected_count, linewidth = 0.5, alpha = 0.3) +
        # labs(title = "Resampled weighted rank statistics",
        #         y = "Count",
        #         x = "Ranks")