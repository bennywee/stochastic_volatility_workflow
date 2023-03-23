# Load data functions
source(here::here('R', 'data.R'))

# Set parameters (mu is set within simulate_ksc function) and seed
set.seed(323651)
size <- 1000
phi <-  0.97779
sig <-  0.15850
beta <- 0.64733

# Set directory paths
path <- here::here("data", "simulated", "ksc")

data_name <- paste("phi_", as.character(phi), "_",
                   "sig_", as.character(sig), "_",
                   "beta_", as.character(beta),
                   ".csv", sep ="")

data_path <- here::here(path, data_name)

# Simulate data
df <- simulate_ksc(T=size,
                   phi.true = phi,
                   sig.true = sig,
                   beta.true = beta)

if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }

if (file.exists(data_path)) {
        stop("Data file already exists")
    } else {
        write.csv(df, data_path, row.names = FALSE)
        print(paste("File successfully downloaded: ", data_path, sep = ""))
}