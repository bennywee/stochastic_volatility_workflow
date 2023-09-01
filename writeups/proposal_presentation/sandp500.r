library(ggplot2)
df <- read.csv("data/preprocessed/GSPC/19900101_20230201.csv")
ggplot(df, aes(x=as.Date(Date), y = log_return)) + geom_line()