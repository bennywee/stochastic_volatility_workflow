library(tidyverse)
library(shinystan)

############################ Parameters to set ############################

model_name <- "sv_user_guide_reparameterised_ksc_phi_0.97779_sig_0.1585_beta_0.64733_default_priors"
pp_flag <- "posterior" # prior = no likelihood estimation, posterior = likelihood estimation

############################ Parameters to set ############################

path <- file.path("output", model_name)
rds_path  <- list.files(path = file.path("output", model_name), full.names = TRUE, pattern = "*.RDS")
csv_path  <- list.files(path = file.path("output", model_name), full.names = TRUE, pattern = "*.csv")

model_fit <- readRDS(rds_path[grep(paste(pp_flag,"fit",sep=""),rds_path)])
launch_shinystan(model_fit)

# Generated quantities (predictive checks)
output_csv <- list.files(path = file.path("output", model_name), pattern = "*.csv", full.names = TRUE)
output <- read_cmdstan_csv(csv_path[grep(paste(pp_flag, "-", sep =""), csv_path)], variables = "y_rep", format = "matrix")

# Evaluation
# Prepare generated data for predictive plots
y_rep_df <- output$post_warmup_draws %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(time = 1:nrow(.)) %>% 
  pivot_longer(!time, names_to = "mcmc_draw", values_to = "y_t")

# Predictive check
y_rep_df_sample  <- y_rep_df  %>% 
  filter(mcmc_draw %in% sample(1:4000, 10))

ggplot(y_rep_df_sample, aes(x = time, y = y_t)) +
    geom_line(aes(group = mcmc_draw), alpha = 0.3, colour = "blue") +
    labs(title = paste(prefix, " Predictive Checks"),
         subtitle = "10 blue MCMC draws") +
    theme_minimal()
