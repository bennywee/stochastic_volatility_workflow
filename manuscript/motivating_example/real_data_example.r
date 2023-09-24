library(dplyr)
library(ggplot2)

# Maybe consider latent states
gma <- read.csv("manuscript/motivating_example/gma_draws_2023.csv")
gma$model <- "gma"

hmc <- read.csv("manuscript/motivating_example/hmc_draws_2023.csv")
hmc$model <- "hmc"

names(hmc) <- c("index", "name", "value", "model")
names(gma) <- c("index", "name", "value", "chain", "model")

hmc$name <- replace(hmc$name, hmc$name=="sigma", "sigma2")

df <- rbind(gma %>% select(-chain), hmc)
means <- df %>% group_by(model, name) %>% summarise(avg = mean(value))
medians <- df %>% group_by(model, name) %>% summarise(med = median(value))

ggplot(df, aes(x = value)) +
    geom_histogram(position="identity", aes(fill = model), alpha =0.3) +
    facet_wrap(~name, scale="free") +
    # geom_vline(data =means, aes(xintercept=avg, colour=model), size = 1.2) +
    theme_minimal(base_size = 18) +
    theme(legend.position="none") +
    labs(x = "Parameter value", y= "Count")

ggsave("manuscript/motivating_example/real_data_ex.png", bg = "white")
    
    # geom_vline(data = medians, aes(xintercept=med, colour=model))

df %>% group_by(model, name) %>% 
  summarise(as_tibble(rbind(summary(value)))) %>% 
  ungroup() %>% arrange(name)

ggplot(df, aes(x = value)) +
    geom_histogram(aes(fill = model), alpha =0.4) +
    facet_grid(model~name, scale="free")


dim(hmc)

gma %>% filter(name=="mu") %>% dim
hmc %>% filter(name=="mu") %>% dim

