library(ggplot2)
source(here::here("R", "simulations.R"))

path <- "simulation_output/adapt_delta_0.9_0.99_0.005_stan_user_guide_single_dataset"

rds_list <- list.files(path = paste(path, "output", sep = "/"), pattern = "*.RDS")
divergence_list <- lapply(paste("output", rds_list, sep="/"), get_divergence)

df <- as.data.frame(do.call("rbind", divergence_list))
names(df) <- c("adapt_delta", "divergences")#, "seed")
df$divergence_plusone <- df$divergence + 1

boxplot <- ggplot(df, aes(x = factor(adapt_delta), y = divergences)) +
    geom_boxplot()
    
boxplot_log10 <- ggplot(df, aes(x = factor(adapt_delta), y = divergence_plusone)) +
    geom_boxplot() + scale_y_log10() 

ggsave(here::here(path, "boxplot.png"), 
       plot = boxplot, 
       width = 8, 
       height = 5, 
       unit= "in")

ggsave(here::here(path, "boxplot_log10.png"), 
       plot = boxplot_log10, 
       width = 8, 
       height = 5, 
       unit= "in")
