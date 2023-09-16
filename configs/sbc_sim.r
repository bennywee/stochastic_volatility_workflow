simulation_name <- "sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r9_5000_iterations"
simulation_description = "
SBC with noncentered model and ksc priors and noncentered dataset with pre-simulated datasets run: 9.0
"

# Simulation iterations
sim_iter <- 5000

# Model metadata
model_name <- "sv_ncp_ksc_sbc" # Name of the .stan file in the models directory

# Data location
data_location <- "sbc_data_gen_sv_ncp_ksc_priors_5000_datasets"
dependent_variable <- "y_sim"

## Stan parameters
chains <- 4
parallel_chains <- 4
refresh <- 500
adapt_delta <- 0.999
save_warmup <- FALSE
iter_sampling <- 999
