library(dplyr)
library(ggplot2)
library(future.apply)
library(posterior)
library(car)
source("R/sbc.r")
plan(multicore, workers = 9)

# Get all parameter data
path <- "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_r2"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
rds_path <- paste("output", rds_list, sep = "/")

# Rank type
rank_type = "weighted_ranks"
# rank_type = "agg_ranks"

# Parameters
n_iterations <- length(rds_list)
posterior_samples <- 9999 # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
n_bins <- 20 # Bins
expected_count <- n_iterations / n_bins # Chi sqd stat

# Load ranks
results <- lapply(rds_path, get_ranks, model_path = path, rank_type = rank_type)

if (rank_type == "agg_ranks"){
    df <- as.data.frame(do.call("rbind", results))
} else if(rank_type == "weighted_ranks"){
    df <- as.data.frame(do.call("rbind", results)) * (posterior_samples+1)
} else {
    print("error")
}

# Create bins and chi sq stats
rank_bins <- as.data.frame(lapply(df, binning, posterior_draws = posterior_samples+1, bins = n_bins))
rank_bins <- rank_bins[!(names(rank_bins) %in% c("y_sim", "sigma"))]

rank_bins %>% select(mu) %>% distinct() %>% count()

rank_stats <- as.data.frame(lapply(rank_bins, rank_chi_sqd, expected = expected_count))

# Define static and state parameters
static_parameters <- c("mu", "sigma_sqd", "phi")
state_parameters <- names(rank_bins)[!(names(rank_bins) %in% static_parameters)]
additional_parameters <- c("h.1.", "h.10.", "h.100.", "h.500.", "h.1000.", "h.995.")

t = rank_bins %>%
        select(mu) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") 

t %>% select(rank) %>% distinct() %>% count()

hist(t$rank, breaks = 20)
        
    rank_bins %>%
        select(mu) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>% select(rank) %>% distinct() %>% count()
        ggplot(.) +
        geom_histogram(aes(rank), bins = n_bins, fill = "light blue", alpha = 0.7) +
        facet_wrap(~variable) +
        scale_x_continuous(labels = function(x) x * rank_scales/nbins) +
        theme_minimal(base_size = 22) +
        geom_hline(yintercept = expected_bin_count, size = 0.5, alpha = 0.3) +
        labs(title = "Distribution of Rank Statistics",
                y = "Count",
                x = "Ranks")

# Facet plots (hist of params)
static_hist <- facet_hist(data = rank_bins, 
                          variables = static_parameters, 
                          expected_bin_count = expected_count, 
                          nbins = n_bins,
                          rank_scales = (posterior_samples+1)
                          )

ggsave(paste(path, "/static_hist_", rank_type, ".png", sep =""), static_hist, bg = "white", width = 11)

static_state_hist <- facet_hist(data = rank_bins, 
                                variables = append(static_parameters, additional_parameters), 
                                expected_bin_count = expected_count, 
                                nbins = n_bins,
                                rank_scales = (posterior_samples+1)
                                )
ggsave(paste(path, "/static_state_hist_", rank_type, ".png", sep =""), static_state_hist, bg = "white", width = 11)

# Chi squared state parameters (dot plots)
chi_sq_dot_plots <- dot_plots(data = rank_stats,
    variables = state_parameters,
    plot_title = "Chi squared estimates for state rank statistics")
ggsave(paste(path, "/chi_sq_dot_plots_", rank_type, ".png", sep =""), chi_sq_dot_plots, bg = "white", width = 11)

# Chi squared state parameters (hist)
chi_sq_histogram <- chi_sq_hist(data = rank_stats,
    variables = state_parameters,
    plot_title = "Chi squared estimates for state rank statistics") + 
    geom_vline(xintercept = qchisq(0.95, df =n_bins-1))
ggsave(paste(path, "/chi_sq_hist", rank_type, ".png", sep =""), chi_sq_histogram, bg = "white", width = 11)

# sum(rank_stats > qchisq(0.95, df =n_bins-1)) / 1003

# QQ plot
chisq_ranks = rank_stats %>%
        tidyr::pivot_longer(everything(), names_to = "parameter", values_to = "chisq_stat") %>%
        arrange(chisq_stat) %>%
        mutate(parameter = factor(parameter, unique(parameter))) 

jpeg(file=paste(path, "/chi_sq_qq_", rank_type, ".png", sep =""))
qqPlot(chisq_ranks$chisq_stat, distribution = "chisq", df =19)
dev.off()

# fsave(paste(path, "/chi_sq_qq.png", sep =""), chi_sq_qq, bg = "white")

# Chi squared state parameters (dot plots)
chi_sq_static_dots <- dot_plots(data = rank_stats,
    variables = static_parameters,
    plot_title = "Chi squared estimates for static rank statistics")
ggsave(paste(path, "/chi_sq_static_dots_", rank_type, ".png", sep =""), chi_sq_static_dots, bg = "white", width = 11)

# pval_hist(data = rank_stats,
#     expected_bin_count = n_bins,
#     plot_title = "Distribution of p-values")

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

ggsave(paste(path, "/rhat_box_static.png", sep =""), rhat_box, bg = "white")

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

ggsave(paste(path, "/rhat_basic_box_static.png", sep =""), rhat_basic_box, bg = "white")


## ESS weights
ess_weights <- future_lapply(rds_path, get_ess_weights, path)
ess_weights_df <- as.data.frame(do.call("rbind", ess_weights))
ess_weights_hist <- ggplot(ess_weights_df) + 
    geom_histogram(aes(x = ess_weights)) +
    labs(title = "ESS Weight distribution")
ggsave(paste(path, "/ess_weights_hist.png", sep =""), ess_weights_hist, bg = "white")

