library(tidyverse)

a <- 0.95

df <- read.csv('/home/markovnin/R/data/guschina/guschina.csv')
df$hospitalization <- as.factor(df$hospitalization)
df$mother <- as.factor(df$mother)
df$father <- as.factor(df$father)
df$grandfather <- as.factor(df$grandfather)
df$grandmother <- as.factor(df$grandmother)
df <- filter(df, group != 3)
mean_age <- df %>% 
  group_by(group) %>% 
  summarise(mean_age = round(mean(age), 2), sd_age = round(sd(age), 2), se_age = round(sd_age/sqrt(n()), 2))
count_gender <- df %>% group_by(group, gender) %>% summarise(count = n())
fisher.test(df$gender, df$group)
fisher.test(df$age, df$group)
wilcox.test(df$IgE ~ df$group)
wilcox.test(df$eosinophils ~ df$group)
fisher.test(df$group, df$hospitalization)
fisher.test(df$group, df$mother)
fisher.test(df$group, df$father)
fisher.test(df$group, df$grandmother)
fisher.test(df$group, df$grandfather)