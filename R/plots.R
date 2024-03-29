library(tidyverse)
library(bayesplot)

plot_predictive_check <- function(dataframe,
                                  x_axis,
                                  y_axis,
                                  groups,
                                  prior_post,
                                  save = FALSE,
                                  path,
                                  true_data = NULL) {
    checking_plot <- ggplot(dataframe, aes(x = {{ x_axis }}, y = {{ y_axis }})) +
        geom_line(aes(group = {{ groups }}), alpha = 0.3, colour = "blue") +
        labs(
            title = paste(prior_post, "Predictive Checks"),
            subtitle = "10 blue MCMC draws, true value in black"
        ) +
        theme_minimal()

    if (!is.null(true_data)) {
        checking_plot <- checking_plot + geom_line(data = true_data, aes(x = time, y = y_t))
    }

    if (isTRUE(save)) {
        ggsave(
            here::here(
                "output",
                path, paste(tolower(prior_post), "_predictive_check.png", sep = "")
            ),
            plot = checking_plot,
            bg = "white",
            width = 8,
            height = 5,
            units = "in"
        )
    } else {
        return(checking_plot)
    }
}

plot_log_y_sqd_hist <- function(mcmc_data, mcmc_x_axis, true_data, true_x_axis, prior_post, save, path, n_bins = 30) {
    hist <- ggplot(mcmc_data, aes(x = {{ mcmc_x_axis }})) +
        geom_histogram(alpha = 0.3, fill = "blue", bins = n_bins) +
        geom_histogram(data = true_data, aes(x = {{ true_x_axis }}), alpha = 0.5, fill = "red", bins = n_bins) +
        labs(
            title = paste(prior_post, "distribution of log(y*^2)"),
            subtitle = "Red is log(y^2) of data and blue is the average over MCMC samples"
        ) +
        theme_minimal()

    if (isTRUE(save)) {
        ggsave(
            here::here(
                "output",
                path, paste(tolower(prior_post), "_log_y_sqd_hist.png", sep = "")
            ),
            plot = hist,
            bg = "white",
            width = 8,
            height = 5,
            units = "in"
        )
    } else {
        return(hist)
    }
}

plot_log_y_sqd_kde <- function(mcmc_data, mcmc_x_axis, true_data, true_x_axis, prior_post, save, path) {
    kde <- ggplot(mcmc_data, aes(x = {{ mcmc_x_axis }})) +
        geom_density(alpha = 0.1, colour = "blue") +
        geom_density(data = true_data, aes(x = {{ true_x_axis }}), colour = "red") +
        labs(
            title = paste(prior_post, "KDE of log(y*^2)"),
            subtitle = "Red is log(y^2) of data and blue is the average over MCMC samples"
        ) +
        theme_minimal()

    if (isTRUE(save)) {
        ggsave(
            here::here(
                "output",
                path, paste(tolower(prior_post), "_log_y_sqd_kde.png", sep = "")
            ),
            plot = kde,
            bg = "white",
            width = 8,
            height = 5,
            units = "in"
        )
    } else {
        return(kde)
    }
}

plot_hist <- function(data, x_axis, variable_name, prior_post, save, path, n_bins = 30, true_data = NULL) {
    hist <- ggplot(data, aes(x = {{ x_axis }})) +
        geom_histogram(alpha = 0.3, fill = "blue", bins = n_bins) +
        labs(
            title = paste(prior_post, "distribution of", variable_name),
        ) +
        theme_minimal()

    if (!is.null(true_data)) {
        hist <- hist + geom_vline(xintercept = true_data)
    }

    if (isTRUE(save)) {
        ggsave(
            here::here(
                "output",
                path, paste(tolower(prior_post), "_", tolower(variable_name), "_hist.png", sep = "")
            ),
            plot = hist,
            bg = "white",
            width = 8,
            height = 5,
            units = "in"
        )
    } else {
        return(hist)
    }
}

pairs_plot <- function(array, np, parameters, save, path) {
    scatter_plot <- mcmc_scatter(array, pars = parameters, np = np)
    if (isTRUE(save)) {
        ggsave(here::here(
            "output",
            path, paste("posterior_", parameters[1], "_", parameters[2], "_pairs_plot.png", sep = "")),
        plot = scatter_plot
        )
    }
    else {
        return(scatter_plot)
    }
}
