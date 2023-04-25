library(tidyverse)
library(cmdstanr)
library(bayesplot)
source(here::here("config.r"))
source(here::here("R", "plots.R"))
source(here::here("R", "model_eval.R"))

set.seed(321321)

############################ Parameters to set ############################

if (!(config)) {
  # Model description
  model_name <- NULL
  dependent_variable <- NULL
  unique_identifier <- NULL

  # Data location
  data_loc <- NULL
  data_type <- NULL
  data_file_name <- NULL

  # Stan paramaters
  seed = 123
  chains = 4
  parallel_chains = 4
  refresh = 500
  adapt_delta = 0.8
  save_warmup <- TRUE
}

############################ Parameters to set ############################

executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)

for (prior in c(1, 0)) {
  sample_prior <- prior # Prior predictive check (0) or estimate full model (1)

  if (sample_prior == 1) {
    prefix <- "Prior"
  } else if (sample_prior == 0) {
    prefix <- "Posterior"
  } else {
    stop("Estimation mode not set")
  }

  # Stan User guide SV model
  # Compile stan model
  file <- here::here("models", paste(model_name, ".stan", sep = ""))
  mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

  # Get data
  data <- read.csv(here::here("data", data_loc, data_type, paste(data_file_name, ".csv", sep = "")))
  returns <- data[complete.cases(data[dependent_variable]), dependent_variable]

  # Fit model
  data_list <- list(T = length(returns), y = returns, sample_prior = sample_prior)
  model_fit <- mod$sample(
    data = data_list,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh,
    adapt_delta = adapt_delta,
    save_warmup = save_warmup
  )

  # Save model
  fit_location <- paste(model_name, "_", data_type, "_", data_file_name, "_", unique_identifier, sep = "") # "_", format(Sys.time(), "%Y%m%d%H%M%S"), sep = "")
  path <- here::here("output", fit_location)
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  model_fit$save_object(file = here::here("output", fit_location, paste(tolower(prefix), "fit.RDS", sep = "")))
  model_fit$save_output_files(dir = path, basename = tolower(prefix))
  model_fit$save_data_file(dir = path)

  # Evaluation
  # Predictive check
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
    prior_post = prefix,
    save = TRUE,
    path = fit_location,
    true_data = true_returns_df
  )

  plot_log_y_sqd_hist(
    mcmc_data = log_y_squared_avg,
    true_data = true_log_sq_returns,
    mcmc_x_axis = average,
    true_x_axis = log_y_squared,
    prior_post = prefix,
    save = TRUE,
    path = fit_location
  )

  plot_log_y_sqd_kde(
    mcmc_data = log_y_squared_avg,
    true_data = true_log_sq_returns,
    mcmc_x_axis = average,
    true_x_axis = log_y_squared,
    prior_post = prefix,
    save = TRUE,
    path = fit_location
  )

  plot_hist(
    data = auto_corr,
    x_axis = log_y_squared_autocorr,
    variable_name = "log(Y^2) autocorrelation",
    prior_post = prefix,
    save = TRUE,
    path = fit_location
  )

  plot_hist(
    data = auto_corr,
    x_axis = log_y_squared_autocorr,
    variable_name = "log(Y^2) autocorrelation",
    prior_post = prefix,
    save = TRUE,
    path = fit_location
  )

  plot_hist(
    data = auto_corr,
    x_axis = log_y_squared_autocorr,
    variable_name = "log(Y^2) autocorrelation",
    prior_post = prefix,
    save = TRUE,
    path = fit_location
  )

  plot_hist(
    data = kurtosis,
    x_axis = y_rep_kurtosis,
    variable_name = "y_t kurtosis",
    prior_post = prefix,
    save = TRUE,
    path = fit_location
  )

  plot_hist(
    data = skewness,
    x_axis = y_rep_skewness,
    variable_name = "y_t skewness",
    prior_post = prefix,
    save = TRUE,
    path = fit_location
  )

  plot_hist(
    data = mcmc_output_df(model_obj = model_fit, variable = "phi"),
    x_axis = phi,
    variable_name = "phi",
    prior_post = 'prior',
    save = TRUE,
    path = fit_location
  )

  if (prior == 0) {
    draws_array = model_fit$draws(format = "draws_array")
    np = nuts_params(model_fit)

    pairs_plot(array = draws_array,
      np = np,
      parameters = c("mu", "sigma"),
      save = TRUE,
      path = fit_location)

    pairs_plot(array = draws_array,
      np = np,
      parameters = c("mu", "phi"),
      save = TRUE,
      path = fit_location)

    pairs_plot(array = draws_array,
      np = np,
      parameters = c("sigma", "phi"),
      save = TRUE,
      path = fit_location)


    pairs_plot(array = draws_array,
      np = np,
      parameters = c("h[1]", "mu"),
      save = TRUE,
      path = fit_location)


    pairs_plot(array = draws_array,
      np = np,
      parameters = c("h[1]", "sigma"),
      save = TRUE,
      path = fit_location)


    pairs_plot(array = draws_array,
      np = np,
      parameters = c("h[1]", "phi"),
      save = TRUE,
      path = fit_location)


  }
}
