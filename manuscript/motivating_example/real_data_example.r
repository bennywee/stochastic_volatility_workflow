library(dplyr)
library(ggplot2)

# Maybe consider latent states
gma <- read.csv("manuscript/motivating_example/gma_draws_2023.csv")
gma$model <- "gma"

hmc <- read.csv("manuscript/motivating_example/hmc_draws_2023.csv")
hmc$model <- "hmc"

names(hmc) <- c("index", "name", "value", "model")
names(gma) <- c("index", "name", "value", "chain", "model")

hmc$name <- replace(hmc$name, hmc$name=="sigma", "~sigma^2")
hmc$name <- replace(hmc$name, hmc$name=="mu", "~mu")
hmc$name <- replace(hmc$name, hmc$name=="phi", "~phi")

gma$name <- replace(gma$name, gma$name=="sigma2", "~sigma^2")
gma$name <- replace(gma$name, gma$name=="mu", "~mu")
gma$name <- replace(gma$name, gma$name=="phi", "~phi")

df <- rbind(gma %>% select(-chain), hmc)
means <- df %>% group_by(model, name) %>% summarise(avg = mean(value))
medians <- df %>% group_by(model, name) %>% summarise(med = median(value))

ggplot(df, aes(x = value)) +
    geom_histogram(position="identity", aes(y= after_stat(density), fill = model), alpha =0.3) +
    facet_wrap(~name, scale="free", labeller = label_parsed) +
    # geom_vline(data =means, aes(xintercept=avg, colour=model), size = 1.2) +
    theme_bw(base_size = 20) +
    scale_fill_discrete(labels = c("KSC", "HMC")) +
    labs(title = "",
         fill = "MCMC\nSamplers",
         x = "Parameter value", 
         y= "Density") + 
       theme(strip.text.x = element_text(size = 22))

ggsave("manuscript/motivating_example/real_data_ex.png", bg = "white", width = 14, height = 9.42)
    
    # geom_vline(data = medians, aes(xintercept=med, colour=model))

df %>% group_by(model, name) %>% 
  summarise(as_tibble(rbind(summary(value)))) %>% 
  ungroup() %>% arrange(name)

ggplot(df, aes(x = value)) +
    geom_histogram(aes(fill = model), alpha =0.4) +
    facet_grid(model~name, scale="free")