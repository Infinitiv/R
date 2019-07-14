library(tidyverse)

a <- 0.95

df <- read.csv('/home/markovnin/R/data/entrants/2018.csv')
entrants <- df %>% 
  mutate(budget_paid = ifelse(competition == 'paid', 'paid', 'budget')) %>%
  group_by(institute, specialty, budget_paid) %>% summarise(count = n())
enrolled <- df %>% 
  mutate(summa = (chemistry + biology + russian), mean = summa/3) %>% 
  group_by(specialty, competition, institute) %>% 
  summarise(enrolled = n(), min_full_summa = min(full_summa), min_summa = min(summa), mean = mean(mean, na.rm = T)) %>% 
  arrange(specialty, competition, desc(min_full_summa)) %>%
  mutate(delta_summa = min_full_summa - min_summa)
ggplot(enrolled) + geom_point(aes(x = competition, size = min_full_summa, col = institute, y = specialty), alpha = 0.75)
ggplot(filter(enrolled, competition == 'common')) + geom_point(aes(x = mean, y = institute, col = specialty))
ggplot(filter(enrolled, competition == 'paid')) + geom_point(aes(x = min_full_summa, y = enrolled, col = institute)) + geom_smooth(aes(x = min_full_summa, y = enrolled, col = institute), method = 'lm')
