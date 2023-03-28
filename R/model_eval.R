library(tidyverse)

mcmc_output_df <- function(model_obj, variable, sample_n = NULL) {
    output_df <- model_obj$draws(variable = variable, format = "matrix") %>%
        t() %>%
        as.data.frame() %>%
        mutate(time = 1:nrow(.)) %>%
        pivot_longer(!time, names_to = "mcmc_draw", values_to = variable)

    if (!is.null(sample_n)) {
        output_df <- output_df %>%
            filter(mcmc_draw %in% sample(1:4000, sample_n))
    }

    return(output_df)
}

average_over_draws <- function(model_obj, variable) {
    mcmc_df <- mcmc_output_df(model_obj = model_obj, variable = variable, sample_n = NULL)
    avg_draws <- mcmc_df[is.finite(mcmc_df[[variable]]), ] %>%
        group_by(time) %>%
        summarise(average = mean(eval(parse(text = variable))))

    return(avg_draws)
}

returns_df <- function(returns_data) {
    retruns_df <- returns_data %>%
        as.data.frame() %>%
        mutate(time = 1:1000)

    colnames(retruns_df) <- c("y_t", "time")

    return(retruns_df)
}

log_squared_returns <- function(returns_data) {
    log_sq_returns <- log(returns_data^2) %>%
        as.data.frame() %>%
        mutate(time = 1:1000)

    colnames(log_sq_returns) <- c("log_y_squared", "time")

    return(log_sq_returns)
}
