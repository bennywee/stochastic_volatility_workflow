library(ggplot2)
df <- read.csv("data/preprocessed/GSPC/19900101_20230201.csv")

ts <- ggplot(df[!is.na(df['log_return']), ], aes(x=as.Date(Date), y = log_return)) + 
    geom_line(colour = 'blue') +
    theme_minimal() +
    labs(x = "Date",
         y = "Log Return") + 
    theme(axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20), #angle = 0, vjust = 0.5),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=16))

ggsave(paste(here::here(), "/writeups/proposal_presentation/returns.png", sep =""), ts, bg = "white")

# plot(rnorm(2e4),type="l")
