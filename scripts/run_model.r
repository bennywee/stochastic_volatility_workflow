library(cmdstanr)
library(bayesplot)

# Stan User guide SV model
# Compile stan model
model_name  <- "sv_user_guide" 
data_type  <-  "GSPC"
file_name  <- "20100104_20230101"
unique_identifier  <- "default_priors"

file <- file.path("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file)

# Get data
data <- read.csv(file.path("data", "preprocessed", data_type, paste(file_name, ".csv", sep = "")))
log_returns  <-  data[complete.cases(data['log_return']), 'log_return']

# Fit model
data_list <- list(T = 3271, y = log_returns)
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