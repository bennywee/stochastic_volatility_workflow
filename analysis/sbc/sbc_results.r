library(dplyr)
library(ggplot2)
library(future.apply)
library(posterior)
source("R/sbc.r")
plan(multicore, workers = 9)

# Get all parameter data
path <- "simulation_output/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_test_run2"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
results <- lapply(paste("output", rds_list, sep = "/"), get_ranks, model_path = path)
df <- as.data.frame(do.call("rbind", results))

# Parameters
n_iterations <- length(rds_list)
posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

# Create bins and chi sq stats
rank_bins <- as.data.frame(lapply(df, binning, posterior_draws = posterior_samples, bins = n_bins))
rank_stats <- as.data.frame(lapply(rank_bins, rank_chi_sqd, expected = expected_count))

# Define static and state parameters
static_parameters <- c("mu", "sigma_sqd", "phi")
state_parameters <- names(rank_bins)[!(names(rank_bins) %in% static_parameters)]
additional_parameters <- c("h.1.", "h.10.", "h.100.", "h.500.", "h.1000.", "h.995.")

# Facet plots (hist of params)
facet_hist(data = rank_bins, variables = static_parameters, expected_bin_count = expected_count, nbins = n_bins)
facet_hist(data = rank_bins, variables = append(static_parameters, additional_parameters), expected_bin_count = expected_count, nbins = n_bins)

# Chi squared state parameters (dot plots)
dot_plots(data = rank_stats,
    variables = state_parameters,
    plot_title = "Chi squared estimates for state rank statistics")

# Chi squared state parameters (hist)
chi_sq_hist(data = rank_stats,
    variables = state_parameters,
    plot_title = "Chi squared estimates for state rank statistics")

# Chi squared state parameters (dot plots)
dot_plots(data = rank_stats,
    variables = static_parameters,
    plot_title = "Chi squared estimates for static rank statistics")

pval_hist(data = rank_stats,
    expected_bin_count = n_bins,
    plot_title = "Distribution of p-values")

# R hats
rhats <- future_lapply(paste("output", rds_list, sep = "/"), get_rhat, model_path = path, future.seed = NULL)
rhat_df <- as.data.frame(do.call("rbind", rhats))

# Basic R hats
basic_rhats <- future_lapply(paste("output", rds_list, sep = "/"), get_rhat_basic, model_path = path, future.seed = NULL)
basic_rhat_df <- as.data.frame(do.call("rbind", basic_rhats))

# Basic ESS
ess_basic <- future_lapply(paste("output", rds_list, sep = "/"), get_ess, model_path = path, ess_type = "ess_basic", chains = "one_chain", future.seed = NULL)
ess_basic_df <- as.data.frame(do.call("rbind", ess_basic))
ess_boxplot(data = ess_basic_df,
    variables = static_parameters,
    plot_title = "Basic ESS distribution")

# Bulk ESS
ess_bulk <- future_lapply(paste("output", rds_list, sep = "/"), get_ess, model_path = path, ess_type = "ess_bulk", chains = "one_chain", future.seed = NULL)
ess_bulk_df <- as.data.frame(do.call("rbind", ess_bulk))
ess_boxplot(data = ess_bulk_df,
    variables = static_parameters,
    plot_title = "Bulk ESS distribution")

# Tail ESS
ess_tail <- future_lapply(paste("output", rds_list, sep = "/"), get_ess, model_path = path, ess_type = "ess_tail", chains = "one_chain", future.seed = NULL)
ess_tail_df <- as.data.frame(do.call("rbind", ess_tail))
ess_boxplot(data = ess_tail_df,
    variables = static_parameters,
    plot_title = "Tail ESS distribution")

# Total time for each model
times <- future_lapply(paste("output", rds_list, sep = "/"), get_time, model_path = path, future.seed = NULL)
times_df <- as.data.frame(do.call("rbind", times))


# fit = t$sv_fit
# fit$summary()

# sort(apply(draws, 2, ess_bulk))
# sort(apply(draws, 2, ess_basic))
# sort(apply(draws, 2, ess_tail))
# sort(apply(draws, 2, rhat_basic))





# All in one bins
# rank_bins %>%
#     tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
#     group_by(variable, rank) %>%
#     count() %>%
#     ggplot(.) +
#     geom_line(aes(x = rank, y = n, group = variable), alpha = 0.09, colour = 'blue') +
#     theme_minimal() +
#     geom_hline(yintercept = 50, size = 1.2) +
#     labs(title = "Distribution of Rank Statistics for every parameter")

# Percentile of chi sqd with J-1 DoF on y axis. Parameters on x axis
# ranks_stats %>%
#     tidyr::pivot_longer(everything(), names_to = "variable", values_to = "chisq_stat") %>%
#     ggplot(.) +
#     geom_point(aes(x = variable, y = chisq_stat), size = 3) +
#     labs(title = "Chi squared estimates for rank statistics")
