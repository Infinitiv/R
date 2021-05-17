library(tidyverse)

d <- read.csv('~/scripts/asics/asics.csv')
d <- d %>%
  filter(points != 10)
d <- d %>%
  mutate(km = max/points)
groups <- d %>%
  group_by(time_start, task_id, points, max) %>%
  summarise(n = n())
