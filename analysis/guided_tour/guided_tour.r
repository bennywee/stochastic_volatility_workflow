library(cmdstanr)
library(tidyverse)
library(posterior)
library(tourr)
library(magick)

figures_path <- here::here("analysis", "guided_tour", "figures")

output_name <- "sv_user_guide_reparameterised_ksc_priors_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.95"
pp_flag <- "posterior" 

# Data location
data_loc <- "simulated"
data_type <- "ksc"
data_file_name <- "phi_0.97779_sig_0.1585_beta_0.64733"

path <- here::here("output", output_name)
rds_path <- list.files(path = here::here("output", output_name), full.names = TRUE, pattern = "*.RDS")

model_fit <- readRDS(rds_path[grep(paste(pp_flag, "fit", sep = ""), rds_path)])

params = c(
    'mu',
    'phi',
    'sigma',
    'h[1]',
    'h[100]',
    'h[200]',
    'h[300]',
    'h[400]',
    'h[500]',
    'h[600]',
    'h[700]',
    'h[800]',
    'h[900]',
    'h[1000]'
)

div_chains = model_fit$sampler_diagnostics(format = "df") %>% 
    select('divergent__', '.chain', '.iteration')

df = model_fit$draws(variables = params, format = 'df') %>%
    left_join(div_chains, by = c('.chain', '.iteration'))

df$divergent__ <- factor(df$divergent__,levels = c(0,1))

df = df %>% 
  arrange(divergent__)

col <- rainbow(length(unique(df$divergent__)))[as.numeric(as.factor(df$divergent__))]

render(
    df[, c(4:14, 1:3)], # Static parameters last
    guided_tour(lda_pp(df$divergent__)), 
    sphere = FALSE, 
    display_xy(col = col, pch = 4),
    "png", 
    paste(figures_path, "sv_tour-%03d.png", sep = "/")
)

list.files(path=figures_path, pattern = '*.png', full.names = TRUE) %>% 
        image_read() %>% # reads each path file
        image_join() %>% # joins image
        image_animate(fps=4,loop =1) %>% # animates, can opt for number of loops
        image_write(paste(figures_path, "tour.gif", sep = "/")) # write to current dir
