library(cmdstanr)
library(bayesplot)

# Model descirption
model_name  <- "sv_user_guide" 
unique_identifier  <- "default_priors"
dependent_variable  <- "yobs"

# Data location
data_loc <- "simulated"
data_type  <-  "ksc"
file_name  <- "phi_0.97779_sig_0.1585_beta_0.64733"

# Stan User guide SV model
# Compile stan model
file <- file.path("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file)

# Get data
data <- read.csv(file.path("data", data_loc, data_type, paste(file_name, ".csv", sep = "")))
returns  <-  data[complete.cases(data[dependent_variable]), dependent_variable]

# Fit model
data_list <- list(T = length(returns), y = returns)
fit <- mod$sample(
  data = data_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500 # print update every 500 iters
)

# Save model
fit_location  <- paste(model_name, "_", data_type, "_", file_name, "_", unique_identifier, "_", format(Sys.time(), "%Y%m%d%H%M%S"), sep = "")
dir.create(file.path("output", fit_location), recursive = TRUE)
fit$save_object(file = file.path("output", fit_location, "fit.RDS"))