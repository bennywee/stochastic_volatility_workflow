config = TRUE

# Create Yahoo Data
stock_index <- "GSPC"
start_date <- "2019-01-04"
end_date <- "2023-02-01"

# Create Simulated Data
set.seed(323651)
size <- 1000
phi <- 0.97779
sig <- 0.15850
beta <- 0.64733

# Model training
## Model metadata
model_name <- "sv_user_guide_reparameterised_ksc_priors"
unique_identifier <- "adapt_delta_0.9"
dependent_variable <- "yobs"

## Data location
data_loc <- "simulated"
data_type <- "ksc"
data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733"

## Stan sampling
seed <- 123
chains <- 4
parallel_chains <- 4
refresh <- 500
adapt_delta <- 0.9
save_warmup <- TRUE

# Evaluation
output_name <- paste(model_name, data_type, data_file_name, unique_identifier, sep = "_")
pp_flag <- "posterior"
