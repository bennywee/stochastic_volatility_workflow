config = TRUE # TRUE if using config parameters. FALSE if setting parameters inside executable scripts

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
model_name <- "sv_user_guide_reparameterised_ksc_priors" # Name of the .stan file in the models directory
unique_identifier <- "adapt_delta_0.9" # Suffix given for specific run of that stan script, needed to identify exact model output

## Data location
data_loc <- "simulated" # Either simulated or preprocessed data (don't use raw)
data_type <- "ksc" # Type of data in the data location
data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733" # Name of the data file
dependent_variable <- "yobs" # Data column name for y variable

## Stan sampling
seed <- 123
chains <- 4
parallel_chains <- 4
refresh <- 500
adapt_delta <- 0.95
save_warmup <- TRUE

# Evaluation
pp_flag <- "posterior" # Evaluation script (scripts/eval.r). Posterior or prior samples
output_name <- paste(model_name, data_type, data_file_name, unique_identifier, sep = "_")
