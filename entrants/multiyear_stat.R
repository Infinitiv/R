library(tidyverse)
library(jsonlite)

df <- read.csv('~/R/data/entrants/2014-2019.csv')
target_enrolled <- df %>%
  filter(education_source_id == 'Целевая квота', !is.na(enrolled_name), status_id == 4) %>%
  select(year, application_number, direction_id, mean_ege) %>%
  arrange(year, direction_id, application_number) %>% 
  mutate(mean_ege = round(mean_ege, 2))
write.csv(target_enrolled, '~/R/data/entrants/target.csv')
