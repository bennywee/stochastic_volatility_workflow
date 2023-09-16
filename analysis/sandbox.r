cp <- readRDS("output/sv_user_guide_ksc_priors_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.999/priorfit.RDS")
ncp <- readRDS("output/sv_user_guide_reparameterised_ksc_priors_ksc_phi_0.97779_sig_0.1585_beta_0.64733_adapt_delta_0.999/priorfit.RDS")

cpdf <- cp$draws(variable = "y_rep", format = "df")
ncpdf <- ncp$draws(variable = "y_rep", format = "df")

min(cpdf)
min(ncpdf)

library(dplyr)
library(tidyr)
library(ggplot2)

cpdf %>% 
    select(-`.chain`, -`.iteration`, -`.draw`) %>%
    mutate(t = row.names(.)) %>%
    pivot_longer(cols = starts_with("y_rep")) %>%
    summary()

ncpdf %>% 
    select(-`.chain`, -`.iteration`, -`.draw`) %>%
    mutate(t = row.names(.)) %>%
    pivot_longer(cols = starts_with("y_rep")) %>%
    summary()

    ggplot(., aes(x=t, y = value) +
        geom_line(aes(group = name), alpha = 0.2)

cpdf %>% 
    select(-`.chain`, `.iteration`, `.draw`) %>%
    pivot_longer(cols = everything()) %>%
    ggplot(.)

gen_sv_sbc_dataset <- function(seed){
    set.seed(seed)

    # Length of data
    T <- 1000

    # Priors
    mu <- rnorm(1, 0, 10)
    sigma_sqd <- 1 / rgamma(1, 5/2, (0.01*5)/2)
    sigma <- sqrt(sigma_sqd)
    p <- rbeta(1, 20, 1.5)
    phi <- 2*p - 1

    # Generate log volatilities
    h <- rep(0, T)
    h[1] <- rnorm(1, mean=mu, sd=sigma/sqrt(1-phi^2))
    for(i in 2:T){
        h[i] <- rnorm(1, mean=(mu+phi*(h[(i-1)]-mu)), sd=sigma)
      }

    # Generate data from prior
    y <- exp(h/2)*rnorm(T,0, 1)

    return(y)
}

set.seed(123)
seed_list <- sample.int(10000)[1:1000]
simulated_cp_datasets = lapply(seed_list, gen_sv_sbc_dataset)

min(test)

T=1000 # length of simulated data


gen_ncp_sv_dataset <- function(seed){
    set.seed(seed)

    # Length of data
    T <- 1000

    # Priors
    mu <- rnorm(1, 0, 10)
    sigma_sqd <- 1 / rgamma(1, 5/2, (0.01*5)/2)
    sigma <- sqrt(sigma_sqd)
    p <- rbeta(1, 20, 1.5)
    phi <- 2*p - 1

    # Sample from standardised normal dist and multiply by sigma
    h_std <- rnorm(T, mean = 0, sd = sigma)

    # Generate log volatilities
    h[1] <- rnorm(1, mean=mu, sd=sigma/sqrt(1-phi^2))
    for(i in 2:T){
        h[i] <- h_std[i] + mu + phi*(h[i-1] - mu)
      }

    # Generate data from prior
    y <- exp(h/2)*rnorm(T,0, 1)

    return(y)
}

set.seed(123)
seed_list <- sample.int(10000)[1:1000] 
simulated_ncp_datasets = lapply(seed_list, gen_ncp_sv_dataset)
