library(tidyverse)
library(jsonlite)
host = 'http://10.0.5.131:3003'
path = 'api/stats'
year = 2019
entrants <- data.frame(fromJSON(paste(host, path, year, 'entrants', sep = '/')))

enrolled_entrants <- entrants %>% 
  filter(!is.na(enrolled), status_id == 4)
enrolled_entrants <- enrolled_entrants %>% 
  mutate(full_summa = sum + achievements, sum_ege = mean_ege*ege_count)
f_1_2 <- enrolled_entrants %>% 
  group_by(enrolled_name) %>% 
  summarise(n = n())
enrolled_entrants %>% 
  filter(!is.na(olympic_type))
f_1_4 <- enrolled_entrants %>% 
  group_by(enrolled_name, exam_category) %>% 
  summarise(n = n())
f_1_4_12 <- enrolled_entrants %>% 
  filter(achievements > 0) %>% 
  group_by(enrolled_name, exam_category) %>% 
  summarise(n = n())
f_2_1 <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))
f_2_1_o <- enrolled_entrants %>% 
  filter(!is.na(olympic_type)) %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))
f_21_6 <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name) %>% 
  filter(exam_category == 'ЕГЭ', sum == min(sum)) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))
f_21_8 <- enrolled_entrants %>% 
  filter(is.na(olympic_type), exam_category != 'ВИ') %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege_ach = (sum(sum_ege) + sum(achievements))/sum(ege_count))
f_21_9 <- enrolled_entrants %>%
  filter(is.na(olympic_type)) %>%
  group_by(enrolled_name) %>%
  summarise(mean_exam = mean(sum)/3)
f_3_3 <- enrolled_entrants %>%
  filter(benefit_document_type == 'Ивалид') %>%
  group_by(enrolled_name) %>%
  summarise(mean_ege = mean(mean_ege, na.rm = T))
f_3_3 <- enrolled_entrants %>%
  filter(benefit_document_type == 'Сирота') %>%
  group_by(enrolled_name) %>%
  summarise(mean_ege = mean(mean_ege, na.rm = T))
