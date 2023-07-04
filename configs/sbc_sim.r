simulation_name <- "sbc_ncp_ksc_priors_0.999_adapt_delta_test_retry_function_r3"
simulation_description = "
SBC with non centereed model and ksc priors (TEST: retry function and error handling) run: 3.0
"

# Retry attempts
max_retries <- 5

# Model metadata
model_name <- "sv_ncp_ksc_sbc" # Name of the .stan file in the models directory

## Stan parameters
chains <- 1
parallel_chains <- 1
refresh <- 500
adapt_delta <- 0.999
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
