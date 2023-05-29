library(cmdstanr)
library(tidyverse)
library(posterior)
library(tourr)
library(magick)

output_name <- "sv_user_guide_reparameterised_ksc_priors_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.95"
pp_flag <- "posterior" # prior = no likelihood estimation, posterior = likelihood estimation

# Data location
data_loc <- "simulated"
data_type <- "ksc"
data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733"

path <- here::here("output", output_name)
rds_path <- list.files(path = here::here("output", output_name), full.names = TRUE, pattern = "*.RDS")
csv_path <- list.files(path = here::here("output", output_name), full.names = TRUE, pattern = "*.csv")

# model_fit <- readRDS(rds_path[grep(paste('posterior', "fit", sep = ""), rds_path)])

data <- read.csv(here::here("data", data_loc, data_type, paste(data_file_name, ".csv", sep = "")))
dependent_variable = 'yobs'
returns <- data[complete.cases(data[dependent_variable]), dependent_variable]

data_list <- list(T = length(returns), y = returns, sample_prior = 0, gen_quantities = 1)
file <- here::here("models", paste('sv_user_guide_reparameterised_ksc_priors', ".stan", sep = ""))
mod <- cmdstan_model(file, include_paths = here::here("models", "functions"), dir = 'models/executables')

model_fit <- mod$sample(
    data = data_list,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 500,
    adapt_delta = 0.95,
    save_warmup = FALSE
  )

library(shinystan)


model_div = model_fit$sampler_diagnostics(format = "df")
div_chains = model_div %>% 
    select('divergent__', '.chain', '.iteration')

df = model_fit$draws(variables = c('mu', 'sigma', 'phi', 'h'), format = 'df') %>%
    left_join(div_chains, by = c('.chain', '.iteration'))

df$divergent__ <- factor(df$divergent__,levels = c(0,1))

df = df %>% 
  arrange(divergent__)

col <- rainbow(length(unique(df$divergent__)))[as.numeric(as.factor(df$divergent__))]
render(df[, c(1:10)], guided_tour(lda_pp(df$divergent__)), sphere = FALSE, display_xy(col = col, pch = 4),"png","figures/tour/tau_tour-%03d.png")

list.files(path='figures/tour/', pattern = '*.png', full.names = TRUE) %>% 
        image_read() %>% # reads each path file
        image_join() %>% # joins image
        image_animate(fps=4,loop =1) %>% # animates, can opt for number of loops
        image_write("figures/tour/test.gif") # write to current dir

