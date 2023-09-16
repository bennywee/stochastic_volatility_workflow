library(dplyr)
library(ggplot2)
library(future.apply)
library(posterior)
library(car)
source("R/sbc.r")
plan(multicore, workers = 9)

# Get all parameter data
# path <- "simulation_output/sbc_cp_ksc_model_cp_dgf_10kmcmc_r2"
rds_list <- list.files(path = "analysis/gauss_mix/results", pattern = "*.rds")
rds_path <- "analysis/gauss_mix/results/results_real_data.rds"

# Rank type
# rank_type = "weighted_ranks"
rank_type = "agg_ranks"

# Parameters
# n_iterations <- length(rds_list)
posterior_samples <- 999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

# Load ranks
data <- readRDS(rds_path)
results <- data$one_chain[[rank_type]]

if (rank_type == "agg_ranks"){
    df <- as.data.frame(do.call("rbind", results))
} else if(rank_type == "weighted_ranks"){
    df <- as.data.frame(do.call("rbind", results)) * (posterior_samples+1)
} else {
    print("error")
}

# R hats
rhats <- future_lapply(rds_path, 
                       get_rhat, 
                       model_path = path, 
                       future.seed = NULL)
rhat_df <- as.data.frame(do.call("rbind", rhats))

rhat_df$rhat <- as.numeric(rhat_df$rhat)
rhat_box <- rhat_boxplot(data = rhat_df,
    variables = static_parameters,
    plot_title = "Rhat distribution",
    rhat_type = rhat)

ggsave(paste(path, "/rhat_box_static_box.png", sep =""), rhat_box, bg = "white")

# Basic R hats
basic_rhats <- future_lapply(rds_path, 
                             get_rhat_basic, 
                             model_path = path, 
                             future.seed = NULL)
basic_rhat_df <- as.data.frame(do.call("rbind", basic_rhats))
basic_rhat_df$rhat_basic <- as.numeric(basic_rhat_df$rhat_basic)
rhat_basic_box <- rhat_boxplot(data = basic_rhat_df,
    variables = static_parameters,
    plot_title = "Basic Rhat distribution",
    rhat_type = rhat_basic)

ggsave(paste(path, "/rhat_basic_box_static_box.png", sep =""), rhat_basic_box, bg = "white")

# Basic ESS
ess_basic <- future_lapply(rds_path,
                           get_ess, 
                           model_path = path, 
                           ess_type = "ess_basic", 
                           chains = "one_chain", 
                           future.seed = NULL)

ess_basic_df <- as.data.frame(do.call("rbind", ess_basic))
ess_basic_box <- ess_boxplot(data = ess_basic_df,
    variables = static_parameters,
    plot_title = "Basic ESS distribution",
    ess_type = ess_basic)

ggsave(paste(path, "/ess_basic_static_box.png", sep =""), ess_basic_box, bg = "white")

# Bulk ESS
ess_bulk <- future_lapply(rds_path, 
                          get_ess, 
                          model_path = path, 
                          ess_type = "ess_bulk", 
                          chains = "one_chain", 
                          future.seed = NULL)
ess_bulk_df <- as.data.frame(do.call("rbind", ess_bulk))
ess_bulk_box <- ess_boxplot(data = ess_bulk_df,
                           variables = static_parameters,
                           plot_title = "Bulk ESS distribution",
                           ess_type = ess_bulk)
ggsave(paste(path, "/ess_bulk_static_box.png", sep =""), ess_bulk_box, bg = "white")

# Tail ESS
ess_tail <- future_lapply(rds_path, 
                          get_ess, 
                          model_path = path, 
                          ess_type = "ess_tail", 
                          chains = "one_chain", 
                          future.seed = NULL)
ess_tail_df <- as.data.frame(do.call("rbind", ess_tail))
ess_tail_box <- ess_boxplot(data = ess_tail_df,
    variables = static_parameters,
    plot_title = "Tail ESS distribution",
    ess_type = ess_tail)
ggsave(paste(path, "/ess_tail_static_box.png", sep =""), ess_tail_box, bg = "white")

# Total time for each model
# times <- future_lapply(paste("output", rds_list, sep = "/"), get_time, model_path = path, future.seed = NULL)
# times_df <- as.data.frame(do.call("rbind", times))


# basic_rhat_df %>% 
#     select(c("mu", "sigma_sqd", "phi")) %>% 
#     tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rhat") %>%
#     ggplot(.) +
#         geom_boxplot(aes(x = variable, y = rhat)) +
#         theme_minimal() +
#         labs(title = "Basic Rhats")



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
