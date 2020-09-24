library(tidyverse)
library(jsonlite)
host = 'http://priem.isma.ivanovo.ru'
path = 'api/stats'
year = 2020
entrants <- data.frame(fromJSON(paste(host, path, year, 'entrants', sep = '/')))



enrolled_entrants <- entrants %>% 
  filter(!is.na(enrolled_name), status_id == 4)
enrolled_entrants <- enrolled_entrants %>% 
  mutate(full_summa = sum + achievements, sum_ege = mean_ege*ege_count)

gender_enrolled_entrants <- enrolled_entrants %>%
  group_by(direction_id, gender_id) %>%
  summarise(n = n())
budget_enrolled_entrants <- enrolled_entrants %>% filter(education_source_id != 15)
paid_enrolled_entrants <- enrolled_entrants %>% filter(education_source_id == 15)
target_enrolled_entrants <- enrolled_entrants %>% 
  filter(education_source_id == 16) %>%
  group_by(target_organization_name.) %>%
  summarise(n = n())

school_budget_enrolled <- budget_enrolled_entrants %>%
  filter(education_document_date > as.Date('2019-10-01')) %>%
  group_by(education_document_type) %>%
  summarise(n = n())

school_paid_enrolled <- paid_enrolled_entrants %>%
  filter(education_document_date > as.Date('2019-10-01')) %>%
  group_by(education_document_type) %>%
  summarise(n = n())

exam_category_budget_enrolled_entrants <- budget_enrolled_entrants %>%
  group_by(exam_category) %>%
  summarise(n = n())

exam_category_budget_enrolled_entrants <- budget_enrolled_entrants %>%
  group_by(exam_category) %>%
  summarise(n = n())

exam_category_paid_enrolled_entrants <- paid_enrolled_entrants %>%
  group_by(exam_category) %>%
  summarise(n = n())

education_source_id_entrants <- enrolled_entrants %>%
  group_by(education_source_id) %>%
  summarise(n = n())

benefit_document_type_entrants <- enrolled_entrants %>%
  group_by(direction_id, benefit_document_type) %>%
  summarise(n = n())

paid_exam_category <- paid_enrolled_entrants %>% 
  group_by(direction_id, exam_category) %>% 
  summarise(n = n())

budget_exam_category <- budget_enrolled_entrants %>% 
  group_by(direction_id, exam_category) %>% 
  summarise(n = n())

exam_category <- enrolled_entrants %>% 
  group_by(enrolled_name, exam_category) %>% 
  summarise(n = n())

achievements_category <- enrolled_entrants %>% 
  group_by(enrolled_name, achievements) %>% 
  summarise(n = n())

target_exam_category <- target_enrolled_entrants %>% 
  group_by(enrolled_name, target_organization_name., exam_category) %>% 
  summarise(n = n())

target_mean_ege <- target_enrolled_entrants %>% 
  group_by(enrolled_name, target_organization_name., exam_category) %>% 
  filter(exam_category == 'ЕГЭ') %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

target_mean_ege_exam <- target_enrolled_entrants %>% 
  group_by(enrolled_name, target_organization_name., exam_category) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

target_mean_full_summa <- target_enrolled_entrants %>% 
  group_by(enrolled_name, target_organization_name., exam_category) %>% 
  filter(exam_category == 'ЕГЭ') %>% 
  summarise(mean_full_summa = mean(full_summa/3, na.rm = T))

exam_category <- enrolled_entrants %>% 
  group_by(enrolled_name, exam_category, benefit_document_type) %>% 
  summarise(n = n())

mean_ege <- enrolled_entrants %>% 
  group_by(enrolled_name, olympic_type) %>% 
  filter(exam_category == 'ЕГЭ') %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

benefit_mean_ege <- enrolled_entrants %>% 
  group_by(enrolled_name, benefit_document_type, achievements) %>% 
  filter(exam_category == 'ЕГЭ') %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T), n = n())

mean_ege_exam <- enrolled_entrants %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege = mean(sum/3, na.rm = T))

mean_full_summa <- enrolled_entrants %>% 
  group_by(enrolled_name, benefit_document_type) %>% 
  filter(exam_category == 'ЕГЭ') %>% 
  summarise(mean_full_summa = mean(full_summa/3, na.rm = T))

mean_achivements <- enrolled_entrants %>%
  group_by(direction_id) %>%
  filter(!is.na(achievements), education_source_id != 15) %>%
  summarise(mean_achivements = mean(achievements, na.rm = T))

budget_mean_ege <- budget_enrolled_entrants %>% 
  group_by(direction_id) %>% 
  filter(exam_category == 'ЕГЭ') %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

budget_mean_full_summa <- budget_enrolled_entrants %>% 
  group_by(direction_id) %>% 
  filter(exam_category == 'ЕГЭ') %>% 
  summarise(mean_full_summa = mean(full_summa/3, na.rm = T))

region <- enrolled_entrants %>% filter(region_id != 37) %>% group_by(enrolled_name) %>% summarise(n = n())
    education_document_type <- enrolled_entrants %>% group_by(direction_id, education_document_type) %>% summarise(n = n())
exam_category <- enrolled_entrants %>% 
  group_by(direction_id, exam_category) %>% 
  summarise(n = n())

mean_ege <- enrolled_entrants %>% 
  group_by(enrolled_name) %>% 
  filter(exam_category == 'ЕГЭ', sum == min(sum)) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

mean_ege_exam <- enrolled_entrants %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege = mean(sum/3, na.rm = T))


f_21_6 <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name) %>% 
  filter(exam_category == 'ЕГЭ', sum == min(sum)) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

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
