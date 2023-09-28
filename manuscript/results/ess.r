library(tidyverse)
source("R/sbc.r")
static_parameters <- c("~mu", "~sigma^2", "~phi")

# HMC results
# 5000 iterations CP
path <- "simulation_output/stan/cp/sbc_cp_ksc_priors_0.999_adapt_delta_premade_datasets_r1_5000_iterations"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
rds_path <- paste("output", rds_list, sep = "/")

hmc_cp_ess_basic_5k <- future_lapply(rds_path,
                           get_ess, 
                           model_path = path, 
                           ess_type = "ess_basic", 
                           chains = "one_chain", 
                           future.seed = NULL)

hmc_cp_ess_basic_5k_df <- as.data.frame(do.call("rbind", hmc_cp_ess_basic_5k))
hmc_cp_ess_basic_5k_df <- latex_variables(hmc_cp_ess_basic_5k_df)
hmc_cp_ess_basic_5k_df$Type <- "Centered"


# 5000 iterations NCP
path <- "simulation_output/stan/ncp/sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r9_5000_iterations"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
rds_path <- paste("output", rds_list, sep = "/")

hmc_ncp_ess_basic_5k <- future_lapply(rds_path,
                           get_ess, 
                           model_path = path, 
                           ess_type = "ess_basic", 
                           chains = "one_chain", 
                           future.seed = NULL)

hmc_ncp_ess_basic_5k_df <- as.data.frame(do.call("rbind", hmc_ncp_ess_basic_5k))
hmc_ncp_ess_basic_5k_df <- latex_variables(hmc_ncp_ess_basic_5k_df)
hmc_ncp_ess_basic_5k_df$Type <- "Non Centered"

hmc_ess_df <- rbind(hmc_cp_ess_basic_5k_df, hmc_ncp_ess_basic_5k_df) %>% 
    filter(parameters %in% static_parameters)

hmc_ess_df <- latex_variables(hmc_ess_df)

plot <- ggplot(hmc_ess_df, aes(x = parameters, y = ess_basic, fill = Type)) +
    geom_boxplot() +
    theme_minimal(base_size = 22) +
        labs(x = "Parameters",
             y = "Count")

x_labs = ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()
plot <- plot + scale_x_discrete(labels = parse(text = x_labs))
ggsave("manuscript/results/hmc_ess.png", bg = "white", width = 14, height = 9.42)

# KSC results
# 5000 iterations CP
path <- "simulation_output/ksc/cp/sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
rds_path <- paste("output", rds_list, sep = "/")

ksc_cp_ess_basic_5k <- future_lapply(rds_path,
                           get_ess, 
                           model_path = path, 
                           ess_type = "ess_basic", 
                           chains = "one_chain", 
                           future.seed = NULL)

ksc_cp_ess_basic_5k_df <- as.data.frame(do.call("rbind", ksc_cp_ess_basic_5k))
ksc_cp_ess_basic_5k_df <- latex_variables(ksc_cp_ess_basic_5k_df)
ksc_cp_ess_basic_5k_df$Type <- "Gaussian Mixture"


# 5000 iterations NCP
path <- "simulation_output/ksc/cp/sir_sbc_cp_ksc_model_cp_dgf_10kmcmc_5000iter_r1"
rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
rds_path <- paste("output", rds_list, sep = "/")

sir_ncp_ess_basic_5k <- future_lapply(rds_path,
                           get_ess, 
                           model_path = path, 
                           ess_type = "ess_basic", 
                           chains = "one_chain", 
                           future.seed = NULL)

sir_ncp_ess_basic_5k_df <- as.data.frame(do.call("rbind", sir_ncp_ess_basic_5k))
sir_ncp_ess_basic_5k_df <- latex_variables(sir_ncp_ess_basic_5k_df)
sir_ncp_ess_basic_5k_df$Type <- "Importance Resampling"

ksc_sir_df <- rbind(ksc_cp_ess_basic_5k_df, sir_ncp_ess_basic_5k_df) %>% 
    filter(parameters %in% static_parameters)

ksc_sir_df <- latex_variables(ksc_sir_df)

plot <- ggplot(ksc_sir_df, aes(x = parameters, y = ess_basic, fill = Type)) +
    geom_boxplot() +
    theme_minimal(base_size = 22) +
        labs(x = "Parameters",
             y = "Count")

x_labs = ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()
plot <- plot + scale_x_discrete(labels = parse(text = x_labs))
ggsave("manuscript/results/ksc_sir_ess.png", bg = "white", width = 14, height = 9.42)





# hmc_1k_basic <- hmc_cp_ess_basic_df %>%
#         filter(parameters %in% static_parameters) %>% 
#         ggplot(.) +
#         geom_boxplot(aes(x = parameters, y = ess_basic)) +
#         theme_minimal(base_size = 22) +
#         labs(x = "Parameters",
#              y = "Count")

# x_labs = ggplot_build(hmc_1k_basic)$layout$panel_params[[1]]$x$get_labels()
# hmc_1k_basic <- hmc_1k_basic + scale_x_discrete(labels = parse(text = x_labs))

#     return(plot)