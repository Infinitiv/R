library(tidyverse)

data <- read.csv('~/data/migel/data.csv')

shapiro.test(data$age)

age <- data %>%
  group_by(group) %>%
  summarise(median = median(age), low_quaantile = quantile(age, probs = c(0.25)), upper_quantile = quantile(age, probs = c(0.75)))

wilcox.test(data$age ~ data$group)
