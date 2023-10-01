library(ggplot2)
df = as.data.frame(list(v1=rnorm(5e6)))

ggplot(df, aes(x=v1))+ geom_density(linewidth=1.2) + 
    theme_void() +
    geom_vline(xintercept = -4, linewidth = 1)

hist_df = as.data.frame(list(v1 = c(rep(1,500), runif(3000, 2,20))))
ggplot(hist_df, aes(x=v1)) +
    geom_histogram(bins=20) +
    theme_void()


