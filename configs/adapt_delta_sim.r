config = TRUE # TRUE if using config parameters. FALSE if setting parameters inside executable scripts

# Create Yahoo Data
stock_index <- "GSPC"
start_date <- "2019-01-04"
end_date <- "2023-02-01"

# Model training
## Model metadata
model_name <- "sv_user_guide_reparameterised_ksc_priors" # Name of the .stan file in the models directory
unique_identifier <- "" # Suffix given for specific run of that stan script, needed to identify exact model output

## Data location
data_loc <- "simulated" # Either simulated or preprocessed data (don't use raw)
data_type <- "ksc" # Type of data in the data location
data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733" # Name of the data file
dependent_variable <- "yobs" # Data column name for y variable

## Stan sampling
seed <- 123
chains <- 1
parallel_chains <- 1
refresh <- 500
save_warmup <- FALSE
gen_quantities <- 0
sample_prior <- 0
