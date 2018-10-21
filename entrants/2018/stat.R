library(tidyverse)

df <- read.csv('/home/markovnin/R/data/entrants/stat.csv')
df <- filter_all(df, all_vars(!is.na(.)))
df$free <- df$budget - df$target - df$special
df$rank <- as.factor(df$rank)
df$free_ratio <- df$free_c/df$free
df$target_ratio <- df$target_c/df$target
df$special_ratio <- df$special_c/df$special
df$paid_ratio <- df$paid_c/df$paid

ggplot(filter(df, speciality == '31.05.01'), aes(x = free, y = free_c, col = rank)) + geom_point()

ggplot(filter(df, speciality == '31.05.02'), aes(x = free, y = free_c, col = rank)) + geom_point()

ggplot(df, aes(x = speciality, y = target_ratio, col = rank)) + geom_point()

wilcox.test(free_ratio ~ rank, data = df)
wilcox.test(target_ratio ~ rank, data = filter(df, speciality == '31.05.03'))
wilcox.test(special_ratio ~ rank, data = df)
wilcox.test(paid_ratio ~ rank, data = df)

fit <- glm(rank ~ target_ratio, df, family = 'binomial')
summary(fit)

ggplot(df, aes(x = speciality, y = free_ratio, col = rank)) + stat_summary(fun.data = 'mean_se')

d <- df[, c("target", "target_ratio")]
fit <- kmeans(d, centers = 3)
d$clusters <- factor(fit$cluster)
ggplot(d, aes(x = target, y = target_ratio, col = clusters)) + geom_point(size = 2)
