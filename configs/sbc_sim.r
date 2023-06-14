simulation_name <- "sbc_ncp_ksc_priors_0.99_adapt_delta"
simulation_description = "
SBC with non centereed model and ksc priors
"

# Model metadata
model_name <- "sv_ncp_ksc_sbc" # Name of the .stan file in the models directory

## Stan parameters
chains <- 1
parallel_chains <- 1
refresh <- 500
adapt_delta <- 0.99
save_warmup <- FALSE
iter_sampling <- 999

## Model parameters to track
params <- c("mu",
    "phi",
    "sigma",
    "h_1",
    "h_100",
    "h_400",
    "h_500",
    "h_600",
    "h_1000"
)
