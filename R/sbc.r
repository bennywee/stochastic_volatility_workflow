get_ranks <- function(rds_path, model_path, rank_type) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    return(data$one_chain[[rank_type]])
}

rank_chi_sqd <- function(column, expected) {
    return(sum((table(column) - expected)^2 / expected))
}

binning <- function(rank, posterior_draws, bins) {
    return(1 + floor(rank / ((posterior_draws + 1) / bins)))
}

load_parameters <- function(rds_path) {
    results <- list()
    data <- readRDS(here::here(paste(path, rds_path, sep = "/")))
    draws <- as.data.frame(data$sv_fit$draws(format = "df"))
    std_h <- unlist(lapply(1:1000, FUN = function(x) {paste("h_std[", x, "]", sep = "")}))
    parameters <- draws[, -which(names(draws) %in% c(std_h, c(".chain", ".iteration", ".draw", "lp__")))]
    results[["parameters"]] <- parameters
    results[["seed_index"]] <- data$seed_index
    return(results)
}

facet_hist <- function(data, variables, nbins, expected_bin_count, rank_scales) {
    data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>%
        ggplot(.) +
        geom_histogram(aes(rank), bins = nbins, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * rank_scales/nbins) +
        facet_wrap(~variable, labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_bin_count, linewidth = 0.5, alpha = 0.3) +
        labs(title = "Distribution of Rank Statistics",
                y = "Count",
                x = "Ranks")
}

dot_plots <- function(data, variables, plot_title) {
    data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "parameter", values_to = "chisq_stat") %>%
        arrange(chisq_stat) %>%
        mutate(parameter = factor(parameter, unique(parameter))) %>%
        ggplot(.) +
        geom_segment(aes(x = parameter, xend = parameter, y = 0, yend = chisq_stat), color = "grey") +
        geom_point(aes(x = parameter, y = chisq_stat), size = 3, colour = "light blue") +
        coord_flip() +
        theme_minimal(base_size = 22) +
        theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
        labs(title = plot_title,
            x = "Parameters",
            y = "Chi squared stats")
}

chi_sq_hist <- function(data, variables, plot_title) {
    data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "parameter", values_to = "chisq_stat") %>%
        arrange(chisq_stat) %>%
        mutate(parameter = factor(parameter, unique(parameter))) %>%
        ggplot(.) +
        geom_histogram(aes(x = chisq_stat), fill = 'blue', alpha = 0.4) +
        theme_minimal(base_size = 22) +
        theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
        labs(title = plot_title,
            y = "Chi squared stats")
}

pval_hist <- function(data, expected_bin_count, plot_title) {
    df <- as.data.frame(as.matrix(lapply(data, pchisq, df = expected_bin_count - 1)))
    names(df) <- c("pvals")
    df$pvals <- as.numeric(df$pvals)

    df %>%
        ggplot(.) +
        geom_histogram(aes(x = pvals)) +
        theme_minimal() +
        theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
        labs(title = plot_title)
}

load_parameters <- function(rds_path) {
    results <- list()
    data <- readRDS(here::here(paste(path, rds_path, sep = "/")))
    draws <- as.data.frame(data$sv_fit$draws(format = "df"))
    std_h <- unlist(lapply(1:1000, FUN = function(x) {paste("h_std[", x, "]", sep = "")}))
    parameters <- draws[, -which(names(draws) %in% c(std_h, c(".chain", ".iteration", ".draw", "lp__")))]
    results[["parameters"]] <- parameters
    results[["seed_index"]] <- data$seed_index
    return(results)
}

get_rhat_basic <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    if(is.null(dim(data$all_chains$diagnostics))){
        diagnostics = do.call("rbind", data$all_chains$diagnostics)
        results <- as.data.frame(cbind(rownames(diagnostics), diagnostics$rhat_basic))
    } else{
        diagnostics = data$all_chains$diagnostics
        results <- as.data.frame(cbind(diagnostics$variable, diagnostics$rhat_basic))
    }
    names(results) <- c("parameters", "rhat_basic")
    return(results)
}

get_rhat <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    if(is.null(dim(data$all_chains$diagnostics))){
        diagnostics = do.call("rbind", data$all_chains$diagnostics)
        results <- as.data.frame(cbind(rownames(diagnostics), diagnostics$rhat))
    } else{
        diagnostics = data$all_chains$diagnostics
        results <- as.data.frame(cbind(diagnostics$variable, diagnostics$rhat))
    }
    names(results) <- c("parameters", "rhat")
    return(results)
}

get_ess <- function(rds_path, model_path, ess_type, chains) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    diagnostics = data[[chains]][["diagnostics"]]
    results <- as.data.frame(cbind(diagnostics["variable"], diagnostics[ess_type]))
    names(results) <- c("parameters", ess_type)
    return(results)
}

get_time <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    time <- data$time
    return(c(total_time = time$total))
}

ess_boxplot <- function(data, variables, plot_title, ess_type) {
    plot <- data %>%
        filter(parameters %in% variables) %>% 
        ggplot(.) +
        geom_boxplot(aes(x = parameters, y = {{ess_type}})) +
        theme_minimal(base_size = 22) +
        labs(title = plot_title,
             x = "Parameters",
             y = "Count")

    x_labs = ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()
    plot <- plot + scale_x_discrete(labels = parse(text = x_labs))

    return(plot)
}

rhat_boxplot <- function(data, variables, plot_title, rhat_type) {
    plot <- data %>%
        filter(parameters %in% variables) %>% 
        ggplot(.) +
        geom_boxplot(aes(x = parameters, y = {{rhat_type}})) +
        theme_minimal(base_size = 22) +
        labs(title = plot_title,
             x = "Parameters",
             y = "Count")

     x_labs = ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels()
     plot <- plot + scale_x_discrete(labels = parse(text = x_labs))
     
     return(plot)

}

get_ess_weights <- function(rds_path, model_path) {
    data = readRDS(here::here(paste(model_path, rds_path, sep = "/")))
    results = data[["weights_ess"]]
    return(results)
}

latex_variables <- function(df){
  df$parameters <- replace(df$parameters, df$parameters=="sigma_sqd", "~sigma^2")
  df$parameters <- replace(df$parameters, df$parameters=="mu", "~mu")
  df$parameters <- replace(df$parameters, df$parameters=="phi", "~phi")

  return(df)
}

clean_variable_names <- function(dataframe){
    result <- gsub("mu", "~mu",
    gsub("phi", "~phi",
    gsub("sigma_sqd", "~sigma^2",
    gsub("\\.", "\\]",
    gsub("h[.]", "h\\[",
    names(dataframe))))))

    return(result)
}


rank_bins_f <- function(path, rank_type, posterior_samples, n_bins){
    rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
    rds_path <- paste("output", rds_list, sep = "/")

    n_iterations <- length(rds_path)
    posterior_samples <- posterior_samples # Posterior samples. m+1 possible ranks. So want (m+1)/J = expected samples for uniform dist.
    n_bins <- n_bins # Bins
    expected_count <- n_iterations / n_bins # Chi sqd stat

    results <- lapply(rds_path, get_ranks, model_path = path, rank_type = rank_type)

    if (rank_type == "agg_ranks"){
        df <- as.data.frame(do.call("rbind", results))
        rank_bins <- as.data.frame(lapply(df, binning, posterior_draws = posterior_samples, bins = n_bins))
    } else if(rank_type == "weighted_ranks"){
        df <- as.data.frame(do.call("rbind", results)) * (posterior_samples+1)
        rank_bins <- as.data.frame(lapply(df, binning, posterior_draws = posterior_samples+1, bins = n_bins))
    } else {
        print("error")
    }

    names(df) <- clean_variable_names(df)
    names(rank_bins) <- clean_variable_names(rank_bins)

    rank_bins <- rank_bins[!(names(rank_bins) %in% c("y_sim", "sigma"))]

    return(rank_bins)
}

static_rank_bins <- function(data, variables, type){
    result <- data %>%
        select(all_of(variables)) %>%
        tidyr::pivot_longer(everything(), names_to = "variable", values_to = "rank") %>% 
        mutate(type = type)

    return(result)
}

rank_hist <- function(dataframe){
        ggplot(dataframe) +
        geom_histogram(aes(rank), bins = 20, fill = "light blue", alpha = 0.7) +
        scale_x_continuous(labels = function(x) x * (posterior_samples+1)/n_bins) +
        facet_wrap(~factor(variable, levels = parameters), labeller = label_parsed) +
        theme_minimal(base_size = 20) +
        theme(strip.text.x = element_text(size = 22)) +
        geom_hline(yintercept = expected_count, linewidth = 1, alpha = 0.6) +
        labs(y = "Count",
             x = "Ranks")
}