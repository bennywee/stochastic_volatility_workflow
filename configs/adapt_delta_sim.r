config = TRUE # TRUE if using config parameters. FALSE if setting parameters inside executable scripts

simulate_data = TRUE

# Create Simulated Data (if simulate data is TRUE)
size <- 1000
phi <- 0.97779
sig <- 0.15850
beta <- 0.64733

## Data location (if simulate data is FALSE)
data_loc <- "simulated" # Either simulated or preprocessed data (don't use raw)
data_type <- "ksc" # Type of data in the data location
data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733" # Name of the data file
dependent_variable <- "yobs" # Data column name for y variable

# Model metadata
model_name <- "sv_user_guide_reparameterised_ksc_priors" # Name of the .stan file in the models directory
unique_identifier <- "" # Suffix given for specific run of that stan script, needed to identify exact model output

## Stan parameters
chains <- 4
parallel_chains <- 4
refresh <- 500
save_warmup <- FALSE
gen_quantities <- 0
sample_prior <- 0
adapt_delta_list <- seq(0.90, 0.99, 0.005)