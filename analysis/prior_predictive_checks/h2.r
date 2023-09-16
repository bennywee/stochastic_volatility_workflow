gen_ncp_sv_dataset <- function(iter, seed_list, T) {
    set.seed(seed_list[iter])

    # Priors
    mu <- rnorm(1, 0, sqrt(10))
    sigma_sqd <- 1 / rgamma(1, 5 / 2, (0.01 * 5) / 2)
    sigma <- sqrt(sigma_sqd)
    p <- rbeta(1, 20, 1.5)
    phi <- 2 * p - 1

    # Sample from standardised normal dist and multiply by sigma
    h_std <- rnorm(T, mean = 0, sd = sigma)

    # Generate log volatilities
    h <- rep(0, T)
    h[1] <- rnorm(1, mean = mu, sd = sigma / sqrt(1 - phi^2))
    for (i in 2:T) {
        h[i] <- h_std[i] + mu + phi * (h[i - 1] - mu)
    }

    return(h[2])
}

gen_cp_sv_dataset <- function(iter, seed_list, T) {
    set.seed(seed_list[iter])

    # Length of data
    T <- 1000

    # Priors
    mu <- rnorm(1, 0, sqrt(10))
    sigma_sqd <- 1 / rgamma(1, 5 / 2, (0.01 * 5) / 2)
    sigma <- sqrt(sigma_sqd)
    p <- rbeta(1, 20, 1.5)
    phi <- 2 * p - 1

    # Generate log volatilities
    h <- rep(0, T)
    h[1] <- rnorm(1, mean = mu, sd = sigma / sqrt(1 - phi^2))
    for (i in 2:T) {
        h[i] <- rnorm(1, mean = (mu + phi * (h[(i - 1)] - mu)), sd = sigma)
    }

    return(h[2])
}

set.seed(123)
seeds <- sample.int(10000)[1:1000]
ncp_list <- lapply(1:1000, gen_ncp_sv_dataset, seed_list = seeds, T = 1000)
cp_list <- lapply(1:1000, gen_cp_sv_dataset, seed_list = seeds, T = 1000)

ncp <- as.data.frame(do.call("rbind", ncp_list))
cp <- as.data.frame(do.call("rbind", cp_list))


ggplot(ncp) +
    geom_histogram(aes(V1), fill = 'blue', alpha = 0.2) +
    geom_histogram(data = cp, aes(V1), fill = 'red', alpha = 0.2) +
    theme_minimal() +
    labs(title = "Prior simulations of CP (red) and NCP (blue) h[2] parameters",
        x = "h[2]")
