# 8 Schools Centered 
library(cmdstanr)
library(ggplot2)
library(dplyr)

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

cp_mod <- cmdstan_model(here::here("analysis", "eight_schools", "eight_schools_cp.stan"), 
                        dir = here::here("analysis", "eight_schools", "executables"))

cp_fit <- cp_mod$sample(
    data = schools_dat,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 500,
    adapt_delta = 0.95,
    save_warmup = FALSE
  )

cp_div = cp_fit$sampler_diagnostics(format = "df")

div_chains = cp_div %>% 
    select('divergent__', '.chain', '.iteration')

cp_df = cp_fit$draws(variables = c('theta', 'tau'), format = 'df') %>%
    left_join(div_chains, by = c('.chain', '.iteration'))

cp_df$divergent__ <- factor(cp_df$divergent__,levels = c(0,1))
cp_df$log_tau <- log(cp_df$tau)

ggplot(cp_df%>%filter(divergent__==0), aes(x=`theta[1]`, y=tau,colour=divergent__))+
         geom_point(alpha=.3) +
         geom_point(data = cp_df%>%filter(divergent__==1), alpha = 0.9 )

ggplot(cp_df%>%filter(divergent__==0), aes(x=`theta[1]`, y=log_tau,colour=divergent__))+
         geom_point(alpha=.3) +
         geom_point(data = cp_df%>%filter(divergent__==1), alpha = 0.9 )

# 8 Schools Non-Centered 
ncp_mod <- cmdstan_model(here::here("analysis", "eight_schools", "eight_schools_ncp.stan"))

ncp_fit <- ncp_mod$sample(
    data = schools_dat,
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 500,
    adapt_delta = 0.95,
    save_warmup = FALSE
  )

ncp_div = ncp_fit$sampler_diagnostics(format = "df")

ncp_div_chains = ncp_div %>% 
    select('divergent__', '.chain', '.iteration')

ncp_df = ncp_fit$draws(variables = c('theta', 'tau'), format = 'df') %>%
    left_join(ncp_div_chains, by = c('.chain', '.iteration'))

ncp_df$divergent__ <- factor(ncp_df$divergent__,levels = c(0,1))
ncp_df$log_tau <- log(ncp_df$tau)

ggplot(ncp_df%>%filter(divergent__==0), aes(x=`theta[1]`, y=tau,colour=divergent__))+
         geom_point(alpha=.3) +
         geom_point(data = ncp_df%>%filter(divergent__==1), alpha = 0.9 )

ggplot(ncp_df%>%filter(divergent__==0), aes(x=`theta[1]`, y=log_tau,colour=divergent__))+
         geom_point(alpha=.3) +
         geom_point(data = ncp_df%>%filter(divergent__==1), alpha = 0.9 )
