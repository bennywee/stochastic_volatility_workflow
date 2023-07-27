library(cmdstanr)
source(here::here("R", "simulations.r"))

T <- 1000
n_iterations <- 1000
mcmc_iter <- 999
model_name <- "sbc_data_gen_sv_ncp_ksc_priors"
data_path <- here::here("data", "simulated", "sbc", model_name)
seed <- 543

executables_path <- here::here("models/executables")
if (!dir.exists(executables_path)) dir.create(executables_path)
if (!dir.exists(data_path)) dir.create(data_path, recursive = TRUE)

file <- here::here("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = executables_path)

data_list <- list(T = T)

model_fit <- mod$sample(
    data = data_list,
    seed = seed,
    chains = 1,
    parallel_chains = 1,
    adapt_delta = 0.999,
    save_warmup = FALSE,
    iter_sampling = mcmc_iter
)

sim_parameters <- model_fit$draws(format = "df")
params <- sim_parameters[, !grepl("lp__|.chain|.iteration|.draw|y_sim|h_std", names(sim_parameters))]
df <- sim_parameters[, grepl("y_sim", names(sim_parameters))]

lapply(1:n_iterations, write_sbc_data, sim_param = sim_parameters, sim_data = df)
config_file <- readLines(here::here("scripts", "sbc_data_gen.r"))
metadata <- list(stan = file, config = config_file)
saveRDS(metadata, file = here::here(here::here(data_path, "metadata.RDS")))