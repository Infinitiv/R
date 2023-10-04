library(tidyverse)
library(jsonlite)
host = 'https://priem.ivgma.ru'
path = 'api/stats'
campaign = 1
entrant_applications <- data.frame(fromJSON(paste(host, path, campaign, 'entrant_applications', sep = '/')))

entrant_applications <- entrant_applications %>% filter(stage != 0)
entrant_applications$education_document_date <- as.Date(entrant_applications$education_document_date)

entrants <- data.frame(fromJSON(paste(host, path, campaign, 'entrants', sep = '/')))

write.csv2(entrants, '~/entrants2023.csv')

country <- entrant_applications %>% filter(nationality != 'Российская Федерация') %>% group_by(direction, education_source, nationality) %>% summarise(n = n())
country_enrolled <- enrolled %>% filter(nationality != 'Российская Федерация') %>% group_by(direction, education_source, nationality) %>% summarise(n = n())


entrant_applications <- entrant_applications %>% 
  arrange(application_number) %>%
  mutate(mean = round(sum/test_count, 2), 
         mean_ege = round(sum_ege/ege_count, 2), 
         sum_exam = sum - sum_ege,
         exam_count = test_count - ege_count,
         mean_exam = round(sum_exam/exam_count, 2))

# для департамента образования
budget_entrant_applications <- entrant_applications %>%
  filter(education_source != 'По договору об оказании платных образовательных услуг') %>%
  group_by(education_source) %>%
  summarise(n = n())

entrant_applications$source <- as.factor(entrant_applications$source)
print('По специальностям и источникам подачи - всего')
entrant_applications %>% group_by(direction, source) %>% summarise(n = n())
print('По специальностям и источникам подачи - бюджет')
entrant_applications %>% filter(education_source != 'С оплатой обучения') %>% group_by(direction, source) %>% summarise(n = n())
print('По специальностям и источникам подачи - внебюджет')
entrant_applications %>% filter(education_source == 'С оплатой обучения') %>% group_by(direction, competitive_group_name) %>% summarise(n = n())
print('По специальностям и источникам обучения')
entrant_applications %>% group_by(direction, education_source) %>% summarise(n = n())
print('Инвалиды по специальностям')
entrant_applications %>% filter(grepl('инвалид', benefit_documents), education_source != 'С оплатой обучения') %>% group_by(direction) %>% summarise(n = n())
entrant_applications %>% filter(grepl('инвалид', benefit_documents), education_source == 'С оплатой обучения') %>% group_by(direction) %>% summarise(n = n())
print('Специальная квота')
entrant_applications %>% filter(grepl('Специальная квота', competitive_group_name)) %>% group_by(direction) %>% summarise(n = n())


enrolled <- entrant_applications %>% filter(!is.na(enrolled_date))

test_type <- entrant_applications %>%
  filter(education_source != 'С оплатой обучения', education_document != 'Диплом о высшем профессиональном образовании') %>%
  group_by(test_type) %>%
  summarise(n = n())

benefits_by_competitive_groups <- entrant_applications %>%
  filter(education_source != 'С оплатой обучения', education_document != 'Диплом о высшем профессиональном образовании') %>%
  group_by(education_source, benefit_documents) %>%
  summarise(n = n())

competitive_groups_group <- enrolled %>% 
  group_by(competitive_group_name) %>% 
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))

enrolled$buget_paid[enrolled$education_source != 'С оплатой обучения'] <- 'Бюджет'
enrolled$buget_paid[enrolled$education_source == 'С оплатой обучения'] <- 'Внебюджет'

ped <- enrolled %>% filter(competitive_group_name == 'Педиатрия. Основные места в рамках КЦП')

#24
not_high_education_enrolled <- enrolled %>% 
  filter(education_document != 'Диплом о высшем профессиональном образовании')
hot_high_education_enrolled_amount <- not_high_education_enrolled %>%
  group_by(direction) %>%
  summarise(n = n())

#25
ege_type <- not_high_education_enrolled %>% 
  filter(test_type == 'ЕГЭ')
ege_type_amount <- ege_type %>%
  group_by(direction, education_source) %>% 
  summarise(n = n())

#26
paid_from_ege_type <- ege_type %>%
  group_by(direction, buget_paid) %>%
  summarise(n = n())

#27
mixed_type <- not_high_education_enrolled %>% 
  filter(test_type == 'ЕГЭ+ВИ')
mixed_type_type_amount <- mixed_type %>%
  group_by(direction, education_source) %>% 
  summarise(n = n())

#32
average_min_ege <- ege_type %>%
  group_by(direction, buget_paid, education_source) %>% 
  summarise(n = n(), 
            mean_ege = mean(mean_ege, na.rm = T), 
            min_mark_1 = min(mark_1), 
            min_mark_2 = min(mark_2), 
            min_mark_3 = min(mark_3), 
            min_mark_4 = min(mark_4))

#40
average_target_ege <- enrolled %>%
  group_by(direction, buget_paid, education_source) %>% 
  summarise(n = n(), 
            mean_ege = mean(mean_ege, na.rm = T), 
            min_mark_1 = min(mark_1), 
            min_mark_2 = min(mark_2), 
            min_mark_3 = min(mark_3), 
            min_mark_4 = min(mark_4))

education_source_group <- enrolled %>% 
  group_by(buget_paid) %>% 
  summarise(n = n(), 
            mean_ege = mean(mean_ege, na.rm = T), 
            mean_exam = mean(mean_exam, na.rm = T), 
            mean_ach = mean(achievement_sum, na.rm = T), 
            min = min(full_sum), 
            min_mark_1 = min(mark_1), 
            min_mark_2 = min(mark_2), 
            min_mark_3 = min(mark_3), 
            min_mark_4 = min(mark_4))

education_source_group <- enrolled %>% 
  group_by(education_source) %>% 
  summarise(n = n(), 
            mean_ege = mean(mean_ege, na.rm = T), 
            mean_exam = mean(mean_exam, na.rm = T), 
            mean_ach = mean(achievement_sum, na.rm = T), 
            min = min(full_sum), 
            min_mark_1 = min(mark_1), 
            min_mark_2 = min(mark_2), 
            min_mark_3 = min(mark_3), 
            min_mark_4 = min(mark_4))

mark_1 <- enrolled %>% 
  filter(test_type_1 == 'ЕГЭ') %>%
  group_by(direction, education_source) %>% 
  summarise(min_mark_1 = min(mark_1))

mark_2 <- enrolled %>% 
  filter(test_type_2 == 'ЕГЭ') %>%
  group_by(direction, education_source) %>% 
  summarise(min_mark_1 = min(mark_2))

mark_4 <- enrolled %>% 
  filter(test_type_4 == 'ЕГЭ') %>%
  group_by(direction, education_source) %>% 
  summarise(min_mark_1 = min(mark_4))

olympionics <- entrant_applications %>% 
  filter(grepl('Приравнивание к лицам, набравшим максимальное количество баллов по ЕГЭ', benefits)) %>% group_by(direction, education_source) %>%
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))


target_applications <- entrant_applications %>% 
  group_by(direction, competitive_group_name) %>%
  summarise(n = n())

target_enrolled <- enrolled %>% 
  group_by(direction, competitive_group_name) %>%
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))

achievemnts <- enrolled %>%
  filter(education_source == 'С оплатой обучения') %>%
  group_by(direction, education_source, achievements) %>%
  summarise(n = n())

achievemnts_mean <- enrolled %>%
  filter(education_source == 'С оплатой обучения') %>%
  group_by(direction) %>%
  summarise(mean = mean(achievement_sum))

education_applications <- entrant_applications %>% 
  group_by(direction, education_source, education_document) %>%
  summarise(n = n())

education_enrolled <- enrolled %>% 
  group_by(direction, education_source, education_document, test_type) %>%
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))

education_applcations <- entrant_applications %>% 
  filter(education_source == 'С оплатой обучения') %>%
  group_by(education_document) %>%
  summarise(n = n())

education_applcations_last <- entrant_applications %>% 
  filter(education_document_date > as.Date('2021-10-01'), education_source != 'С оплатой обучения') %>%
  group_by(education_document) %>%
  summarise(n = n())

education_enrolled_last <- enrolled %>% 
  filter(education_document_date > as.Date('2021-10-01'), education_source != 'С оплатой обучения') %>%
  group_by(education_document) %>%
  summarise(n = n())

write.csv2(entrant_applications, '~/entrants2022.csv')

directions <- enrolled %>%
  filter(education_source == 'С оплатой обучения') %>%
  group_by(direction) %>%
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))

disabled <- enrolled %>%
  filter(grepl('инвалид', benefit_documents), education_source != 'С оплатой обучения') %>% 
  group_by(direction, competitive_group_name) %>% 
  summarise(n = n(), mean = mean(mean_ege))

nationality <- enrolled %>%
  group_by(direction,education_source, nationality) %>% 
  summarise(n = n(), mean = mean(mean_ege))

