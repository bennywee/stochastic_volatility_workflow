library(tidyr)
library(dplyr)
library(cmdstanr)
library(ggplot2)
set.seed(630)
set.seed(634)

n_time_points <- 1000

gen_ncp_sv_dataset <- function(T){
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
    

    return(sim_results)
}

data <- gen_ncp_sv_dataset(T = 1000)
returns <- data$y_sim

model_path <- "models/sv_user_guide_reparameterised_ksc_priors.stan"
mod <- cmdstan_model(model_path, include_paths = here::here("models", "functions"))

data_list <- list(T = length(returns), y = returns, sample_prior = 0)

model_fit <- mod$sample(
    data = data_list,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    adapt_delta = 0.999
  )

draws <- model_fit$draws(variables = c("mu", "phi", "sigma"), format = "df") %>% 
    select(-.chain, -.iteration, -.draw) %>% 
     pivot_longer(everything())

means = data.frame(value = rbind(data[['sigma_sqd']], data[['phi']], data[['mu']]),
           name = rbind('sigma_sqd', 'phi', 'mu'))

quantiles = model_fit$summary(variables = c("mu", "phi", "sigma")) %>% 
              select(variable, q5, q95) %>% 
              pivot_longer(!variable) %>% 
              select(-name)

names(quantiles) <- c("name", "value")

latex_variables <- function(df){
  df$name <- replace(df$name, df$name=="sigma", "~sigma^2")
  df$name <- replace(df$name, df$name=="sigma_sqd", "~sigma^2")
  df$name <- replace(df$name, df$name=="mu", "~mu")
  df$name <- replace(df$name, df$name=="phi", "~phi")

  return(df)
}

draws <- latex_variables(draws)
quantiles <- latex_variables(quantiles)
means <- latex_variables(means)

quantiles$name <- replace(quantiles$name, quantiles$name=="sigma", "~sigma^2")
quantiles$name <- replace(quantiles$name, quantiles$name=="mu", "~mu")
quantiles$name <- replace(quantiles$name, quantiles$name=="phi", "~phi")
draws$name <- replace(draws$name, draws$name=="sigma_sqd", "~sigma^2")
draws$name <- replace(draws$name, draws$name=="mu", "~mu")
draws$name <- replace(draws$name, draws$name=="phi", "~phi")

ggplot(draws, aes(x = value)) +
  geom_histogram(aes(fill = name), alpha = 0.3) +
  theme_minimal(base_size = 20) +
  geom_vline(data = means, aes(xintercept=value, color=name), size = 1.5) +
  geom_vline(data = quantiles, aes(xintercept=value), linetype="dotted", size = 1) +
    facet_wrap(~name, scale = "free", labeller = label_parsed) +
    theme(legend.position="none") +
    labs(title = "Posterior distribution - simulated dataset",
         x = "Parameter value", 
         y= "Count") + 
       theme(strip.text.x = element_text(size = 22))

ggsave("manuscript/motivating_example/single_sim.png", bg = "white", width = 14, height = 9.42)
