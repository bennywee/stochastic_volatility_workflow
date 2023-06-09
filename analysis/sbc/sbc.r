library(cmdstanr)

sv_mod <- cmdstan_model(here::here("models", "sv_ncp_ksc_svb.stan"),
    dir = here::here("models", "executables"))

sv_fit <- sv_mod$sample(
    seed = 123,
    chains = 4,
    parallel_chains = 4,
    refresh = 500,
    adapt_delta = 0.95,
    save_warmup = FALSE
)

sv_fit$draws(variables = c('sim_ranks'), format = 'df')
