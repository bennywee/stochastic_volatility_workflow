library(tidyverse)
library(cmdstanr)

set.seed(321321)

############################ Parameters to set ############################
# Model description
model_name  <- "sv_user_guide" 
unique_identifier  <- "default_priors"
dependent_variable  <- "yobs"

# Data location
data_loc <- "simulated"
data_type  <-  "ksc"
file_name  <- "phi_0.97779_sig_0.1585_beta_0.64733"
############################ Parameters to set ############################

for (estimate in c(0,1)){

run_estimation <- estimate # Prior predictive check (0) or estimate full model (1)

if (run_estimation == 0){
  prefix <- "Prior"
} else if(run_estimation == 1) {
  prefix <- "Posterior"
} else {
  stop("Estimation mode not set")
}

# Stan User guide SV model
# Compile stan model
file <- file.path("models", paste(model_name, ".stan", sep = ""))
mod <- cmdstan_model(file)

# Get data
data <- read.csv(file.path("data", data_loc, data_type, paste(file_name, ".csv", sep = "")))
returns  <-  data[complete.cases(data[dependent_variable]), dependent_variable]

# Fit model
data_list <- list(T = length(returns), y = returns, run_estimation = run_estimation)
model_fit <- mod$sample(
  data = data_list, 
  seed = 123, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 500
)

# Save model
fit_location  <- paste(model_name, "_", data_type, "_", file_name, "_", unique_identifier, sep = "")# "_", format(Sys.time(), "%Y%m%d%H%M%S"), sep = "")
path <- file.path("output", fit_location)
if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }

model_fit$save_object(file = file.path("output", fit_location, paste(tolower(prefix),"fit.RDS", sep ="")))
model_fit$save_output_files(dir = path, basename = tolower(prefix))
model_fit$save_data_file(dir = path)

# Evaluation
# Prepare generated data for predictive plots
y_rep_df <- model_fit$draws(variable = "y_rep", format = "matrix") %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(time = 1:nrow(.)) %>% 
  pivot_longer(!time, names_to = "mcmc_draw", values_to = "y_t")

# Predictive check
y_rep_df_sample  <- y_rep_df  %>% 
  filter(mcmc_draw %in% sample(1:4000, 10))

true_data <- returns %>% 
  as.data.frame() %>% 
  mutate(time = 1:1000)

colnames(true_data)  <- c("y_t", "time")

ggplot(y_rep_df_sample, aes(x = time, y = y_t)) +
    geom_line(aes(group = mcmc_draw), alpha = 0.3, colour = "blue") +
    geom_line(data = true_data, aes(x=time, y = y_t)) +
    labs(title = paste(prefix, " Predictive Checks"),
         subtitle = "10 blue MCMC draws, true value in black") +
    theme_minimal()

ggsave(file.path("output", fit_location, paste(tolower(prefix),"_predictive_check.png", sep ="")), 
       bg="white",
       width = 8,
       height = 5,
       units = "in")
}