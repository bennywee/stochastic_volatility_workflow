simulation_name <- "sbc_ncp_ksc_priors_0.999_adapt_delta_premade_datasets_r2"
simulation_description = "
SBC with non centered model and ksc priors with pre-simulated datasets run: 2.0
"

# Simulation iterations
sim_iter <- 1000

# Model metadata
model_name <- "sv_ncp_ksc_sbc" # Name of the .stan file in the models directory

# Data location
data_location <- "sbc_data_gen_sv_ncp_ksc_priors"
dependent_variable <- "y_sim"

## Stan parameters
chains <- 1
parallel_chains <- 1
refresh <- 500
adapt_delta <- 0.999
save_warmup <- FALSE
iter_sampling <- 999