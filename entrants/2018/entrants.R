library(tidyverse)

a <- 0.95

df <- read.csv('/home/markovnin/R/data/entrants/2018.csv')
df %>% group_by(institute) %>%
  summarise((enrolled = n()))

df %>% summarise(institutes = n())
enrolled <- df %>% 
  mutate(exam_summa = (chemistry + biology + russian), contest_summa = full_summa - exam_summa, mean = exam_summa/3) %>% 
  group_by(specialty, competition, institute, date) %>% 
  summarise(enrolled = n(), min_contest_summa = min(contest_summa), min_exam_summa = min(exam_summa), mean = mean(mean, na.rm = T)) %>% 
  arrange(specialty, competition, desc(min_contest_summa)) %>%
  mutate(delta_summa = min_contest_summa - min_exam_summa)
ggplot(enrolled) + geom_point(aes(x = competition, size = min_full_summa, col = institute, y = specialty), alpha = 0.75)
ggplot(filter(enrolled, competition == 'common')) + geom_point(aes(x = mean, y = institute, col = specialty))
ggplot(filter(enrolled, competition == 'paid')) + geom_point(aes(x = min_full_summa, y = enrolled, col = institute)) + geom_smooth(aes(x = min_full_summa, y = enrolled, col = institute), method = 'lm')
