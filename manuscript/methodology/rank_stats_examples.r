library(ggplot2)
library(cowplot)

df = as.data.frame(list(v1=rnorm(5e6)))

ggplot(df, aes(x=v1))+ geom_density(linewidth=1.2) + 
    theme_void() +
    geom_vline(xintercept = -4, linewidth = 1)



x <- seq(-5,5, 0.01)
dens <- dnorm(x)
df <- as.data.frame(cbind(x, dens))

# LHS
dens_lhs <- ggplot(df, aes(x = x, y = dens)) + 
    geom_line() +
    theme_void() +
    geom_vline(xintercept = -4, linewidth = 1, colour = "red")


hist_df = as.data.frame(list(v1 = c(rep(1,500), runif(3000, 2,20))))
hist_lhs <- ggplot(hist_df, aes(x=v1)) +
    geom_histogram(fill = "red", bins=20, alpha = 0.4) +
    theme_void()

plot_grid(hist_lhs, dens_lhs)
ggsave("manuscript/methodology/lhs.png", bg = "white", width = 14, height = 8.42)

# RHS
dens_rhs <- ggplot(df, aes(x = x, y = dens)) + 
    geom_line() +
    theme_void() +
    geom_vline(xintercept = 4, linewidth = 1, colour = "red")


hist_df = as.data.frame(list(v1 = c(rep(20,500), runif(3000, 1,19))))
hist_rhs <- ggplot(hist_df, aes(x=v1)) +
    geom_histogram(fill = "red", bins=20, alpha = 0.4) +
    theme_void()

plot_grid(hist_rhs, dens_rhs)
ggsave("manuscript/methodology/rhs.png", bg = "white", width = 14, height = 8.42)

# Overdispersion
x <- seq(-5,5, 0.01)
post_draws <- dnorm(x, 0, 0.5)
prior_draws <- dnorm(x,0, 2)

df <- as.data.frame(cbind(x, post_draws, prior_draws))
names(df) <- c("x", "density", "type")

hist_df = as.data.frame(list(v1 = c(rep(1,500), rep(20,500), runif(3000, 1,19))))
hist_lhs_rhs <- ggplot(hist_df, aes(x=v1)) +
    geom_histogram(fill = "red", bins=20, alpha = 0.4) +
    theme_void()


dens_both <- ggplot(df, aes(x = x)) +
    geom_line(aes(y = post_draws), colour = "blue") + 
    geom_line(aes(y = prior_draws), colour = 'red') +
    theme_void() +
    annotate("text", x = 4.5, y = 0.05, label = "Prior", colour = "red", size = 8) +
    annotate("text", x = 2.5, y = 0.3, label = "Posterior", colour = "blue", size = 8)

plot_grid(hist_lhs_rhs, dens_both)

ggsave("manuscript/methodology/underdispersed.png", bg = "white", width = 12, height = 8.42)
