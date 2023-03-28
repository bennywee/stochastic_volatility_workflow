library(tidyverse)
library(shinystan)
source(here::here("R", "plots.R"))
source(here::here("R", "model_eval.R"))

############################ Parameters to set ############################

model_name <- "sv_user_guide_reparameterised_ksc_phi_0.97779_sig_0.1585_beta_0.64733_default_priors_log_y_squared"
pp_flag <- "posterior" # prior = no likelihood estimation, posterior = likelihood estimation


# Data location
data_loc <- "simulated"
data_type <- "ksc"
file_name <- "phi_0.97779_sig_0.1585_beta_0.64733"
############################ Parameters to set ############################

path <- here::here("output", model_name)
rds_path <- list.files(path = here::here("output", model_name), full.names = TRUE, pattern = "*.RDS")
csv_path <- list.files(path = here::here("output", model_name), full.names = TRUE, pattern = "*.csv")

model_fit <- readRDS(rds_path[grep(paste(pp_flag, "fit", sep = ""), rds_path)])
model_fit$summary()
launch_shinystan(model_fit)


data <- read.csv(here::here("data", data_loc, data_type, paste(file_name, ".csv", sep = "")))
dependent_variable <- "yobs" # log squared returns
returns <- data[complete.cases(data[dependent_variable]), dependent_variable]

y_rep_df_sample <- mcmc_output_df(model_obj = model_fit, variable = "y_rep", sample_n = 10)
log_y_squared_avg <- average_over_draws(model_obj = model_fit, variable = "log_y_squared")
true_returns_df <- returns_df(returns)
true_log_sq_returns <- log_squared_returns(returns)

plot_predictive_check(
  dataframe = y_rep_df_sample,
  x_axis = time,
  y_axis = y_rep,
  groups = mcmc_draw,
  prior_post = pp_flag,
  save = FALSE,
  path = NULL,
  true_data = true_returns_df
)

plot_log_y_sqd_hist(
  mcmc_data = log_y_squared_avg,
  true_data = true_log_sq_returns,
  mcmc_x_axis = average,
  true_x_axis = log_y_squared,
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)

plot_log_y_sqd_kde(
  mcmc_data = log_y_squared_avg,
  true_data = true_log_sq_returns,
  mcmc_x_axis = average,
  true_x_axis = log_y_squared,
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)

# # Generated quantities (predictive checks)
# output_csv <- list.files(path = here::here("output", model_name), pattern = "*.csv", full.names = TRUE)
# output <- read_cmdstan_csv(csv_path[grep(paste(pp_flag, "-", sep = ""), csv_path)], variables = "y_rep", format = "matrix")

# # Evaluation
# # Prepare generated data for predictive plots
# y_rep_df <- output$post_warmup_draws %>%
#   t() %>%
#   as.data.frame() %>%
#   mutate(time = 1:nrow(.)) %>%
#   pivot_longer(!time, names_to = "mcmc_draw", values_to = "y_t")

# # Predictive check
# y_rep_df_sample <- y_rep_df %>%
#   filter(mcmc_draw %in% sample(1:4000, 10))

# ggplot(y_rep_df_sample, aes(x = time, y = y_t)) +
#   geom_line(aes(color = mcmc_draw), alpha = 0.3, colour = "blue") +
#   labs(
#     title = paste(prefix, " Predictive Checks"),
#     subtitle = "10 blue MCMC draws"
#   ) +
#   theme_minimal()
