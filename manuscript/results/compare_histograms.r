library(tidyverse)
library(cowplot)
source("R/sbc.r")

n_iterations <- 5000
posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat


# HMC CENTERED results
# 5000 iterations
rank_type = "agg_ranks"
path <- "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_5000_iterations"

hmc_cp_fivek_bins <- rank_bins_f(path = path, 
                                rank_type = "agg_ranks", 
                                posterior_samples = 999, 
                                n_bins= 20)

hmc_cprank_stats <- as.data.frame(lapply(hmc_cp_fivek_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())
hmc_cprank_stats$name <- names(hmc_cp_fivek_bins)
hmc_cprank_stats$type <- "Centered HMC"
hmc_cprank_stats$group <- "HMC"

# HMC NONCENTERED results
# 5000 iterations
rank_type = "agg_ranks"
path <- "simulation_output/stan/ncp/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r9_5000_iterations"

hmc_ncp_fivek_bins <- rank_bins_f(path = path, 
                                rank_type = "agg_ranks", 
                                posterior_samples = 999, 
                                n_bins= 20)

hmc_ncprank_stats <- as.data.frame(lapply(hmc_ncp_fivek_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())
hmc_ncprank_stats$name <- names(hmc_ncp_fivek_bins)
hmc_ncprank_stats$type <- "Reparameterised HMC"
hmc_ncprank_stats$group <- "HMC"

# KSC CENTERED results
n_iterations <- 5000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ksc_cp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "agg_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)
ksc_cp_rank_stats <- as.data.frame(lapply(ksc_cp_fivek_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())

ksc_cp_rank_stats$name <- names(ksc_cp_fivek_bins)
ksc_cp_rank_stats$type <- "Centered KSC"
ksc_cp_rank_stats$group <- "KSC"

# KSC CENTERED ISW results
n_iterations <- 5000
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

ISW_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1", 
                                    rank_type = "weighted_ranks", 
                                    posterior_samples = posterior_samples, 
                                    n_bins= n_bins)
ISW_rank_stats <- as.data.frame(lapply(ISW_fivek_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())

ISW_rank_stats$name <- names(ISW_fivek_bins)
ISW_rank_stats$type <- "Importance Weighted Ranks"
ISW_rank_stats$group <- "KSC"

# # KSC NONCENTERED results
# n_iterations <- 5000
# posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
# n_bins <- 20 # Bins
# expected_count <- n_iterations / n_bins # Chi sqd stat

# ksc_ncp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/ncp/sbc_ncp_ksc_model_ncp_dgf_10kmcmc_5000iter_r1", 
#                                     rank_type = "agg_ranks", 
#                                     posterior_samples = posterior_samples, 
#                                     n_bins= n_bins)
# ksc_ncp_rank_stats <- as.data.frame(lapply(ksc_ncp_fivek_bins, rank_chi_sqd, expected = expected_count)) %>% 
#             pivot_longer(everything())

# ksc_ncp_rank_stats$name <- names(ksc_ncp_fivek_bins)
# ksc_ncp_rank_stats$type <- "Reparameterised KSC"
# ksc_ncp_rank_stats$group <- "KSC"


# # SIR 5k
# n_iterations <- 5000
# posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
# n_bins <- 20 # Bins
# expected_count <- n_iterations / n_bins # Chi sqd stat

# sir_ksc_cp_fivek_bins <- rank_bins_f(path = "simulation_output/ksc/cp/sir_sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1", 
#                                     rank_type = "agg_ranks", 
#                                     posterior_samples = posterior_samples, 
#                                     n_bins= n_bins)

# sir_rank_stats <- as.data.frame(lapply(sir_ksc_cp_fivek_bins, rank_chi_sqd, expected = expected_count)) %>% 
#             pivot_longer(everything())

# sir_rank_stats$name <- names(sir_ksc_cp_fivek_bins)
# sir_rank_stats$type <- "Importance Weighted Ranks"
# sir_rank_stats$group <- "KSC"

rbind(
    hmc_cprank_stats, 
    hmc_ncprank_stats, 
    ksc_cp_rank_stats, 
    ISW_rank_stats
    # sir_rank_stats
) %>% 
    filter(!(name %in% c("~sigma^2", "~mu", "~phi"))) %>% 
ggplot(.) +
    geom_histogram(aes(x = value, fill = type), alpha = 0.6, bins = 200, position = "identity") +
        theme_minimal(base_size = 22) +
        theme(legend.title=element_blank()) +
        labs(x = "Chi squared value",
             y = "Count") +
        scale_fill_brewer(palette="Set2",
                        breaks=c('Centered HMC', 'Reparameterised HMC', 'Centered KSC', 'Importance Weighted Ranks'))

ggsave("manuscript/results/dist_chisq_all.png", bg = "white", width = 14, height = 8.42)

head(hmc_cprank_stats)

set.seed(432)
sbc_iterations <- 5000
variables <- 1000
t <- runif((sbc_iterations*variables),0,1)

for(i in 1:1000){
    if(i==1){
        result = t[1:5000]
    } else {
        lwr = 1 + 5000 * (i-1)
        upr = 5000 * i
        new_result = t[lwr:upr]
        result = cbind(result, new_result)
    }
}
unif <- as.data.frame(result)
unif_bins <- floor(unif / 0.05) + 1

unif_chi_sq <- as.data.frame(lapply(unif_bins, rank_chi_sqd, expected = expected_count)) %>% 
            pivot_longer(everything())
unif_chi_sq$type <- "Uniform"
unif_chi_sq$group <- "Uniform"

uni_centered <- rbind(
    hmc_cprank_stats, 
    unif_chi_sq
) %>% 
    filter(!(name %in% c("~sigma^2", "~mu", "~phi"))) %>% 
ggplot(.) +
    geom_histogram(aes(x = value, fill = type), alpha = 0.6, bins = 50, position = "dodge") +
        theme_minimal(base_size = 22) + 
        ylim(0,75) +
        scale_fill_brewer(palette="Set2")

uni_uncentered <- rbind(
    hmc_ncprank_stats,
    unif_chi_sq
) %>% 
    filter(!(name %in% c("~sigma^2", "~mu", "~phi"))) %>% 
ggplot(.) +
    geom_histogram(aes(x = value, fill = type), alpha = 0.6, bins = 50, position = "dodge") +
    # facet_wrap(~type) +
        theme_minimal(base_size = 22) +
        # scale_fill_manual(values=c("blue")) +
        labs(x = "Chi squared value")+
        scale_fill_brewer(palette="Set2",
                        breaks=c('Reparameterised HMC', 'Uniform')) + 
        ylim(0,75) +
        scale_fill_brewer(palette="Set1")


plot_grid(uni_centered, uni_uncentered, ncol=1, labels = c("Centered", "Non Centered"))

# function(column, expected) {
#     return(sum((table(column) - expected)^2 / expected))
# }

# rbind(
#     hmc_cprank_stats, 
#     hmc_ncprank_stats, 
#     ksc_cp_rank_stats, 
#     sir_rank_stats
# ) %>% 
#     filter(!(name %in% c("~sigma^2", "~mu", "~phi"))) %>% 
# ggplot(.) +
#     geom_histogram(aes(x = value, fill = type), alpha = 0.4, bins = 50, position = "identity") +
#         theme_minimal(base_size = 22) +
#         theme(axis.title.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             legend.title=element_blank()) +
#         labs(x = "Chi squared value") +
#         facet_grid(~group, scales = "free")+
#         scale_fill_discrete(#palette="Set2", 
#                         breaks=c('Centered HMC', 'Reparameterised HMC', 'Centered KSC', 'Importance Weighted Ranks'))

# ggsave("manuscript/results/dist_chisq_facet.png", bg = "white", width = 14, height = 8.42)    


# hmc_plot <- rbind(
#     hmc_cprank_stats, 
#     hmc_ncprank_stats
# ) %>% 
#     filter(!(name %in% c("~sigma^2", "~mu", "~phi"))) %>% 
# ggplot(.) +
#     geom_histogram(aes(x = value, fill = type), alpha = 0.4, bins = 100, position = "identity") +
#         theme_minimal(base_size = 22) +
#         theme(axis.title.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             legend.title=element_blank()) +
#         labs(x = "Chi squared value")

# ksc_plot <- rbind(
#     ksc_cp_rank_stats, 
#     sir_rank_stats
# ) %>% 
#     filter(!(name %in% c("~sigma^2", "~mu", "~phi"))) %>% 
# ggplot(.) +
#     geom_histogram(aes(x = value, fill = type), alpha = 0.4, bins = 100, position = "identity") +
#         theme_minimal(base_size = 22) +
#         theme(axis.title.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks.y = element_blank(),
#             legend.title=element_blank()) +
#         labs(x = "Chi squared value")+ 
#     scale_fill_manual(values=c("#9999CC", "#66CC99"))

# plot_grid(hmc_plot, ksc_plot)

# ggsave("manuscript/results/dist_chisq_hmc.png", bg = "white", width = 14, height = 8.42)

        # geom_histogram(aes(value), bins = 20, fill = "light blue", alpha = 0.7) +
        # scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        # facet_grid(~variable, labeller = label_parsed) +
        # theme_minimal(base_size = 20) +
        # theme(strip.text.x = element_text(size = 22)) +
        # geom_hline(yintercept = expected_count, linewidth = 0.5, alpha = 0.3) +
        # labs(title = "Resampled weighted rank statistics",
        #         y = "Count",
        #         x = "Ranks")
