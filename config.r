config = TRUE

# Create Yahoo Data

# Create Simulated Data

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
