args = commandArgs(trailingOnly = TRUE)
source(here::here("configs", args[1]))

path <- here::here("simulation_output", simulation_name, "output")

if (!dir.exists(path)) {
  dir.create(path, recursive = TRUE)
}

# Use writeLines to pretty print this text
stan_code <- readLines(here::here("models", paste(model_name, ".stan", sep = "")))
config_file <- readLines(here::here("configs", args[1]))
simulation_description <- simulation_description
metadata <- list(stan = stan_code, config = config_file, description = simulation_description)
saveRDS(metadata, file = here::here(here::here("simulation_output", simulation_name, "metadata.RDS")))
