model_name <- "sbc_data_gen_sv_ncp_ksc_priors"
n_time_points <- 1000
n_datasets <- 1000
data_path <- here::here("data", "simulated", "sbc", model_name)
seed <- 543

executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)
if (!dir.exists(data_path)) dir.create(data_path, recursive = TRUE)

gen_ncp_sv_dataset <- function(iter, seed_list, T){
    set.seed(seed_list[iter])
    sim_results <- list()

    # Priors
    mu <- rnorm(1, 0, sqrt(10))
    sigma_sqd <- 1 / rgamma(1, 5/2, (0.01*5)/2)
    sigma <- sqrt(sigma_sqd)
    p <- rbeta(1, 20, 1.5)
    phi <- 2*p - 1

    # Sample from standardised normal dist and multiply by sigma
    h_std <- rnorm(T, mean = 0, sd = sigma)

    # Generate log volatilities
    h <- rep(0, T)
    h[1] <- rnorm(1, mean=mu, sd=sigma/sqrt(1-phi^2))
    for(i in 2:T){
        h[i] <- h_std[i] + mu + phi*(h[i-1] - mu)
      }

    # Generate data from prior
    y_sim <- exp(h/2)*rnorm(T, 0, 1)

    sim_results[["y_sim"]] <- y_sim
    sim_results[["sigma_sqd"]] <- sigma_sqd
    sim_results[["sigma"]] <- sigma
    sim_results[["mu"]] <- mu
    sim_results[["phi"]] <- phi
    
    for(i in 1:length(h)){
        sim_results[[paste("h[", i, "]", sep = "")]] <- h[i]
    }

    saveRDS(sim_results, file = paste(data_path, paste(iter, "RDS", sep = "."), sep = "/"))
    
}

set.seed(seed)
seeds <- sample.int(10000)[1:n_datasets] 
lapply(1:n_datasets, gen_ncp_sv_dataset, seed_list = seeds, T = n_time_points)

config_file <- readLines(here::here("scripts", "sbc_data_gen.r"))
metadata <- list(config = config_file, seed_list = seeds)
saveRDS(metadata, file = here::here(here::here(data_path, "metadata.RDS")))
