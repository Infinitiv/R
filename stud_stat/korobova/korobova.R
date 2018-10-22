library(tidyverse)
# устанавливаем уровень достоверности
a <- 0.05

# читаем данные
df <- read.csv("~/R/data/korobova/korobova.csv", check.names = F)
df$vsd_f[df$vsd > 14] <- 'yes'
df$vsd_f[df$vsd < 15] <- 'no'
fisher.test(df$sex, df$vsd_f)
tapply(df$vsd_f, df$sex, table)
table(df$age)/length(df$age)
tapply(df$vsd_f, df$age, table)
mean(df$age)
sd(df$age)/sqrt(length(df$age))
