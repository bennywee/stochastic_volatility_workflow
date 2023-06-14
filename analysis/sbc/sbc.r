library(cmdstanr)
library(dplyr)

sv_mod <- cmdstan_model(here::here("models", "sv_ncp_ksc_sbc.stan"),
    dir = here::here("models", "executables"))

sv_fit <- sv_mod$sample(
    seed = 123,
    chains = 1,
    parallel_chains = 1,
    refresh = 500,
    adapt_delta = 0.99,
    save_warmup = FALSE,
    iter_sampling = 999
)

ranks <- sv_fit$draws(variables = c('sim_ranks'), format = 'df') %>% select(-c(.chain, .iteration, .draw))
params <- c("mu",
    "phi",
    "sigma",
    "h_1",
    "h_100",
    "h_400",
    "h_500",
    "h_600",
    "h_1000"
)
names(ranks) <- params
agg_ranks <- apply(ranks, 2, FUN = sum)

# Getting lots of Chain 1 Exception: normal_lpdf: Scale parameter[1] is nan, but must be positive
# errors. Doesn't happen when posterior sampling in previous models.
# But it happened with prior sampling in the past.
# Maybe need to be more specific about prior choice. Particularly with the
# Reparameterised model.
# Probably should check when this error occurred previously.
# Maybe posterior sampling pushed the warmup into regions where it doesn't sample
# a value which gives -inf (exp(o)).
# And sampling from prior puts too much weight on values that shouldn't exist.
# Look at how reparameterised model is derived mathematically. see if it makes sense
