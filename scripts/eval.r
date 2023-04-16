library(tidyverse)
library(shinystan)
library(posterior)

source(here::here("config.r"))
source(here::here("R", "plots.R"))
source(here::here("R", "model_eval.R"))

############################ Parameters to set ############################

if (!(config)) {
  output_name <- "sv_user_guide_reparameterised_ksc_priors_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.8"
  pp_flag <- "posterior" # prior = no likelihood estimation, posterior = likelihood estimation


  # Data location
  data_loc <- "simulated"
  data_type <- "ksc"
  data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733"
}
############################ Parameters to set ############################

path <- here::here("output", output_name)
rds_path <- list.files(path = here::here("output", output_name), full.names = TRUE, pattern = "*.RDS")
csv_path <- list.files(path = here::here("output", output_name), full.names = TRUE, pattern = "*.csv")

model_fit <- readRDS(rds_path[grep(paste(pp_flag, "fit", sep = ""), rds_path)])

# Shiny stan
# launch_shinystan(model_fit)

# Contains ess and rhats (new)
model_fit$summary()

# Warm up samples
model_fit$draws(variable = "sigma",
  inc_warmup = TRUE,
  format = "draws_df") %>%
  pivot_wider(id_cols = ".iteration",
    names_from = ".chain",
    values_from = "sigma") %>% View # %>% filter(.iteration >= 1000)

# Time to sample
model_fit$time()

# Console messages
model_fit$output()

# Old r hats
# for (param in model_fit$metadata()$model_params) print(param)
# str(model_fit$metadata())
rhat_basic(extract_variable_matrix(model_fit$draws(), "mu"))

# Old ESS
ess_basic(extract_variable_matrix(model_fit$draws(), "mu"))

# Std out
model_fit$output()

# Predictive checks
data <- read.csv(here::here("data", data_loc, data_type, paste(data_file_name, ".csv", sep = "")))
dependent_variable <- "yobs" # log squared returns
returns <- data[complete.cases(data[dependent_variable]), dependent_variable]

y_rep_df_sample <- mcmc_output_df(model_obj = model_fit, variable = "y_rep", sample_n = 10)
log_y_squared_avg <- average_over_draws(model_obj = model_fit, variable = "log_y_squared")
auto_corr <- mcmc_output_df(model_obj = model_fit, variable = "log_y_squared_autocorr", sample_n = NULL)
kurtosis <- mcmc_output_df(model_obj = model_fit, variable = "y_rep_kurtosis", sample_n = NULL)
skewness <- mcmc_output_df(model_obj = model_fit, variable = "y_rep_skewness", sample_n = NULL)
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

plot_hist(
  data = auto_corr,
  x_axis = log_y_squared_autocorr,
  variable_name = "log(Y^2) autocorrelation",
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)

plot_hist(
  data = auto_corr,
  x_axis = log_y_squared_autocorr,
  variable_name = "log(Y^2) autocorrelation",
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)
plot_hist(
  data = kurtosis,
  x_axis = y_rep_kurtosis,
  variable_name = "y_t kurtosis",
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)

plot_hist(
  data = skewness,
  x_axis = y_rep_skewness,
  variable_name = "y_t skewness",
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)

plot_hist(
  data = mcmc_output_df(model_obj = model_fit, variable = "phi"),
  x_axis = phi,
  variable_name = "phi",
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)

plot_hist(
  data = mcmc_output_df(model_obj = model_fit, variable = "p"),
  x_axis = p,
  variable_name = "p",
  prior_post = pp_flag,
  save = FALSE,
  path = fit_location
)

mean(mcmc_output_df(model_obj = model_fit, variable = "p")$p)

# # Generated quantities (predictive checks)
# output_csv <- list.files(path = here::here("output", output_name), pattern = "*.csv", full.names = TRUE)
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

# model_fit$code()
# model_fit$diagnostic_summary()
# cat(model_fit$code(), sep = "\n")

# model_summ = model_fit$summary()

# View(model_summ)


# model_fit$draws()
# model_fit$summary()

# model_fit$draws()
# as_draws_df(model_fit$draws())
# cat(model_fit$code(), sep = '\n')



# parallel::mclapply()
# ?parallel
# library(help = "parallel")

# params = model_fit$metadata()$model_params
# rhats_basic_df = function(model_draws, parameter) {
#   return(rhat_basic(extract_variable_matrix(model_draws, parameter)))
# }

# lapply(params, rhats_basic_df(), model_draws= model_fit$draws())

# lapply(params, function(p) rhat_basic(extract_variable_matrix(model_fit$draws(), p)))

# parallel::mclapply(params, function(params) {
#                  rhat_basic(extract_variable_matrix(model_fit$draws(), params))
#          }, mc.cores = 6)

# rhat_basic(extract_variable_matrix(model_fit$draws(), "mu"))


# as_draws_rvars(example_draws("multi_normal"))
