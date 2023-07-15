library(cmdstanr)

T <- 1000
n_iterations <- 999
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
    iter_sampling = n_iterations
)

df <- model_fit$draws(variable = "y_sim", format = "df")
df <- df[, !(names(df) %in% c(".chain", ".iteration", ".draw"))]

for (i in 1:n_iterations) {
    data <- as.data.frame(t(df[i, ]))
    row.names(data) <- 1:dim(data)[1]
    names(data) <- c("y_sim")
    write.csv(data, file = paste(data_path, paste(i, "csv", sep = "."), sep = "/"))
}

stan_code <- file
config_file <- readLines(here::here("scripts", "sbc_data_gen.r"))
metadata <- list(stan = stan_code, config = config_file)
saveRDS(metadata, file = here::here(here::here(data_path, "metadata.RDS")))
