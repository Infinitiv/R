library(tidyverse)
df <- read.csv("/home/markovnin/R/data/entrants/stat.csv", check.names = F)
df$free <- df$budget - df$special - df$target
df$rank <- as.factor(df$rank)
df <- filter_all(df, all_vars(!is.na(.)))
ggplot(df, aes(x = free, y = free_c, col = rank)) + geom_point()

fit <- lm(free_c ~ free * rank, data = df)
summary(fit)
summary(lm(target_c ~ rank, data = df))
