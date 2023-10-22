library(tidyverse)
source("R/sbc.r")

n_iterations <- 1000
posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

parameters <- c("~mu", "~sigma^2", "~phi", "h[1]", "h[500]", "h[1000]")

# HMC results
# 1000 iterations
rank_type = "agg_ranks"
path <- "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_1000_iterations"


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
                                         variables = parameters, 
                                         type = "Centered")
hmc_ncp_onek_bin_long <- static_rank_bins(data = hmc_ncp_onek_bins, 
                                          variables = parameters, 
                                          type = "NonCentered")

hmc_1k = rbind(hmc_cp_onek_bin_long, hmc_ncp_onek_bin_long)

rank_hist(hmc_cp_onek_bin_long) # HMC CP 1k
ggsave("manuscript/results/hmc_cp_1k.png", bg = "white", width = 14, height = 9.42)

rank_hist(hmc_ncp_onek_bin_long) # HMC NCP 1k
ggsave("manuscript/results/hmc_ncp_1k.png", bg = "white", width = 14, height = 9.42)

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
                                         variables = parameters, 
                                         type = "Centered")
hmc_ncp_fivek_bin_long <- static_rank_bins(data = hmc_ncp_fivek_bins, 
                                          variables = parameters, 
                                          type = "NonCentered")

# hmc_5k = rbind(hmc_cp_fivek_bin_long, hmc_ncp_fivek_bin_long)
rank_hist(hmc_cp_fivek_bin_long)  # HMC NCP 1k
ggsave("manuscript/results/hmc_cp_5k.png", bg = "white", width = 14, height = 9.42)

rank_hist(hmc_ncp_fivek_bin_long) + 
    ylim(0, 600) # HMC NCP 5k
ggsave("manuscript/results/hmc_ncp_5k.png", bg = "white", width = 14, height = 9.42)

# KSC 1k
n_iterations <- 1000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ksc_cp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_1000iter_r2", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)


ksc_ncp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/ncp/sbc_ncp_ksc_model_ncp_dgf_10kmcmc_1000iter_r1", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

ksc_cp_onek_bins_long <- static_rank_bins(data = ksc_cp_onek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

ksc_ncp_onek_bins_long <- static_rank_bins(data = ksc_ncp_onek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

rank_hist(ksc_cp_onek_bins_long)  # KSC CP 1k
ggsave("manuscript/results/ksc_cp_1k.png", bg = "white", width = 14, height = 9.42)

rank_hist(ksc_ncp_onek_bins_long) # KSC NCP 1k
ggsave("manuscript/results/ksc_ncp_1k.png", bg = "white", width = 14, height = 9.42)

# KSC 5k
n_iterations <- 5000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ksc_cp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

ksc_ncp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/ncp/sbc_ncp_ksc_model_ncp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

ksc_cp_fivek_bins_long <- static_rank_bins(data = ksc_cp_fivek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

ksc_ncp_fivek_bins_long <- static_rank_bins(data = ksc_ncp_fivek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

rank_hist(ksc_cp_fivek_bins_long)  # KSC CP 5k
ggsave("manuscript/results/ksc_cp_5k.png", bg = "white", width = 14, height = 9.42)

rank_hist(ksc_ncp_fivek_bins_long)  # KSC NCP 5k
ggsave("manuscript/results/ksc_ncp_5k.png", bg = "white", width = 14, height = 9.42)

# REWEIGHTED KSC 1k
n_iterations <- 1000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ksc_cp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_1000iter_r2", 
                                    rank_type = "weighted_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)


ksc_ncp_onek_bins <- rank_bins_f(path = "simulation_output/ksc/ncp/sbc_ncp_ksc_model_ncp_dgf_10kmcmc_1000iter_r1", 
                                    rank_type = "weighted_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

ksc_cp_onek_bins_long <- static_rank_bins(data = ksc_cp_onek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

ksc_ncp_onek_bins_long <- static_rank_bins(data = ksc_ncp_onek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

rank_hist(ksc_cp_onek_bins_long) # Reweighted KSC CP 1k
ggsave("manuscript/results/weighted_ksc_cp_1k.png", bg = "white", width = 14, height = 9.42)

rank_hist(ksc_ncp_onek_bins_long) # Reweighted KSC NCP 1k
ggsave("manuscript/results/weighted_ksc_ncp_1k.png", bg = "white", width = 14, height = 9.42)

# REWEIGHTED KSC 5k
n_iterations <- 5000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ksc_cp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "weighted_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

ksc_ncp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/ncp/sbc_ncp_ksc_model_ncp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "weighted_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

ksc_cp_fivek_bins_long <- static_rank_bins(data = ksc_cp_fivek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

ksc_ncp_fivek_bins_long <- static_rank_bins(data = ksc_ncp_fivek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

rank_hist(ksc_cp_fivek_bins_long) # KSC CP 5k
ggsave("manuscript/results/weighted_ksc_cp_5k.png", bg = "white", width = 14, height = 9.42)

rank_hist(ksc_ncp_fivek_bins_long) # KSC NCP 5k
ggsave("manuscript/results/weighted_ksc_ncp_5k.png", bg = "white", width = 14, height = 9.42)


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
                                         variables = parameters, 
                                         type = "Centered")

rank_hist(sir_ksc_cp_onek_bins_long) # KSC CP 5k
ggsave("manuscript/results/sir_cp_1k.png", bg = "white", width = 14, height = 9.42)


# SIR CP 5k
n_iterations <- 5000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

sir_ksc_cp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sir_sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)

sir_ksc_cp_fivek_bins_long <- static_rank_bins(data = sir_ksc_cp_fivek_bins, 
                                         variables = parameters, 
                                         type = "Centered")

rank_hist(sir_ksc_cp_fivek_bins_long) # SIR CP 5k
ggsave("manuscript/results/sir_cp_5k.png", bg = "white", width = 14, height = 9.42)




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

