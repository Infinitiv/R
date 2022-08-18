library(tidyverse)
library(jsonlite)
host = 'https://priem.ivgma.ru'
path = 'api/stats'
campaign = 1
entrant_applications <- data.frame(fromJSON(paste(host, path, campaign, 'entrant_applications', sep = '/')))

entrant_applications <- entrant_applications %>% filter(stage != 2)

entrants <- data.frame(fromJSON(paste(host, path, campaign, 'entrants', sep = '/')))

write.csv2(entrants, '~/entrants2022.csv')



entrant_applications <- entrant_applications %>% 
  arrange(application_number) %>%
  mutate(mean = round(sum/test_count, 2), 
         mean_ege = round(sum_ege/ege_count, 2), 
         sum_exam = sum - sum_ege,
         exam_count = test_count - ege_count,
         mean_exam = round(sum_exam/exam_count, 2))

entrant_applications$source <- as.factor(entrant_applications$source)
print('По специальностям и источникам подачи - всего')
entrant_applications %>% group_by(source) %>% summarise(n = n())
print('По специальностям и источникам подачи - бюджет')
entrant_applications %>% filter(education_source != 'С оплатой обучения') %>% group_by(direction, source) %>% summarise(n = n())
print('По специальностям и источникам подачи - внебюджет')
entrant_applications %>% filter(education_source == 'С оплатой обучения') %>% group_by(direction, source) %>% summarise(n = n())
print('По специальностям и источникам обучения')
entrant_applications %>% group_by(direction, education_source) %>% summarise(n = n())
print('Инвалиды по специальностям')
entrant_applications %>% filter(grepl('инвалид', benefit_documents), education_source != 'С оплатой обучения') %>% group_by(direction) %>% summarise(n = n())
entrant_applications %>% filter(grepl('инвалид', benefit_documents), education_source == 'С оплатой обучения') %>% group_by(direction) %>% summarise(n = n())
print('Специальная квота')
entrant_applications %>% filter(grepl('Специальная квота', competitive_group_name)) %>% group_by(direction) %>% summarise(n = n())


enrolled <- entrant_applications %>% filter(!is.na(enrolled_date))

competitive_groups_group <- enrolled %>% 
  group_by(competitive_group_name) %>% 
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))

education_source_group <- enrolled %>% 
  filter(education_source == 'С оплатой обучения') %>% 
  group_by(direction, education_source, test_type) %>% 
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))

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

directions <- enrolled %>%
  filter(education_source == 'С оплатой обучения') %>%
  group_by(direction) %>%
  summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T), mean_exam = mean(mean_exam, na.rm = T), mean_ach = mean(achievement_sum, na.rm = T), min = min(full_sum))

disabled <- enrolled %>%
  filter(grepl('инвалид', benefit_documents), education_source == 'С оплатой обучения') %>% 
  group_by(direction, competitive_group_name) %>% 
  summarise(n = n(), mean = mean(mean_ege))

nationality <- enrolled %>%
  group_by(direction,education_source, nationality) %>% 
  summarise(n = n(), mean = mean(mean_ege))

path = 'api/campaigns'
campaigns <- data.frame(fromJSON(paste(host, path, sep = '/')))
competitive_groups <- campaigns %>%
  filter(campaigns.campaign_type_id == 1, campaigns.year_start == 2021) %>%
  select(campaigns.competitive_groups)
competitive_groups_names <- as.data.frame(competitive_groups$campaigns.competitive_groups) %>% 
  select(name, education_source_id) %>% filter(education_source_id == 14 | education_source_id == 16)

target_competitive_groups_names <- as.data.frame(competitive_groups$campaigns.competitive_groups) %>% 
  select(name, education_source_id) %>% filter(education_source_id == 16)

entrants %>% group_by(nationality) %>% summarise(n = n())
entrants %>% filter(grepl('Стоматология.', competitive_groups)) %>% nrow()
#лечебное дело
entrants %>% filter(grepl('Лечебное дело. Бюджет.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Лечебное дело. Внебюджет.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Лечебное дело. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarise(n = n())

entrants %>% filter(grepl('Лечебное дело. Бюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Лечебное дело. Целевые места.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Лечебное дело. Квота особого права.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Лечебное дело. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())

entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Лечебное дело. Бюджет.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Лечебное дело. Целевые места.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Лечебное дело. Квота особого права.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Лечебное дело. Внебюджет.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())

entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Лечебное дело. Бюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Лечебное дело. Целевые места.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Лечебное дело. Квота особого права.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Лечебное дело. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())

entrants %>% filter(grepl('Лечебное дело. Целевые места.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Лечебное дело. Квота особого права.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Лечебное дело. Квота особого права.', competitive_groups), benefit_document_type == 'Инвалид') %>% nrow()
entrants %>% filter(grepl('Лечебное дело. Внебюджет.', competitive_groups), benefit_document_type == 'Инвалид') %>% nrow()
entrants %>% filter(grepl('Лечебное дело. Квота особого права.', competitive_groups), benefit_document_type == 'Сирота') %>% nrow()

c06_entrants %>% filter(grepl('Лечебное дело', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Лечебное дело', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Лечебное дело', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument') %>% group_by(exam_category) %>% summarise(n = n())

c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% group_by(enrolled_name, exam_category) %>% summarise(n = n())
entrants %>% filter(grepl('Лечебное дело. Целевые места.', exeptioned_name)) %>% group_by(exeptioned_name, exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% group_by(enrolled_name, exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% group_by(enrolled_name) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% group_by(enrolled_name) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Лечебное дело. Бюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
entrants %>% filter(grepl('Лечебное дело. Бюджет.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Бюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Лечебное дело. Бюджет.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Лечебное дело. Бюджет.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Лечебное дело. Бюджет.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Лечебное дело. Бюджет.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
entrants %>% filter(grepl('Лечебное дело. Внебюджет.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% group_by(achievements) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% summarise(mean = mean(achievements))
c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Лечебное дело. Бюджет.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Целевые места.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Лечебное дело. Квота особого права.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Лечебное дело. Внебюджет.', enrolled_name)) %>% group_by(education_document_type, exam_category) %>% summarise(n = n(), mean_ege = mean(mean_ege))

enrolled_entrants %>% filter(!is.na(benefit_type)) %>% group_by(education_source_id) %>% summarise(n = n())
enrolled_entrants %>% group_by(education_document_type) %>% summarise(n = n())

print('подано зявлений для разных заказчиков целевой подготовки')
for(i in 1:length(target_competitive_groups_names$name)) {
  target_competitive_groups_name <- target_competitive_groups_names$name[i]
  count <- entrants %>% filter(grepl(target_competitive_groups_name, competitive_groups)) %>%
    nrow()
  print(paste(target_competitive_groups_name, count, sep = ' '))
}

#педиатрия
entrants %>% filter(grepl('Педиатрия. Бюджет.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Педиатрия. Внебюджет.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Педиатрия. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarise(n = n())
entrants %>% filter(grepl('Педиатрия. Целевые места.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Педиатрия. Квота особого права.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Педиатрия. Квота особого права.', competitive_groups), benefit_document_type == 'Инвалид') %>% nrow()
entrants %>% filter(grepl('Педиатрия. Внебюджет.', competitive_groups), benefit_document_type == 'Инвалид') %>% nrow()
entrants %>% filter(grepl('Педиатрия. Квота особого права.', competitive_groups), benefit_document_type == 'Сирота') %>% nrow()

entrants %>% filter(grepl('Педиатрия. Бюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Педиатрия. Целевые места.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Педиатрия. Квота особого права.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Педиатрия. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())

entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Педиатрия. Бюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Педиатрия. Целевые места.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Педиатрия. Квота особого права.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Педиатрия. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())

entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Педиатрия. Бюджет.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Педиатрия. Целевые места.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Педиатрия. Квота особого права.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Педиатрия. Внебюджет.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())

c06_entrants %>% filter(grepl('Педиатрия', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Педиатрия', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Педиатрия', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
entrants %>% filter(grepl('Педиатрия. Целевые места.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% group_by(enrolled_name, exam_category) %>% summarise(n = n())
entrants %>% filter(grepl('Педиатрия. Целевые места.', exeptioned_name)) %>% group_by(exeptioned_name, exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% group_by(enrolled_name, exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% group_by(enrolled_name) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% group_by(enrolled_name) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% summarise(mean = mean(achievements))
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Педиатрия. Бюджет.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% group_by(achievements) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Целевые места.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Педиатрия. Квота особого права.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Педиатрия. Внебюджет.', enrolled_name)) %>% group_by(education_document_type, exam_category) %>% summarise(n = n(), mean_ege = mean(mean_ege))

#стоматология
entrants %>% filter(grepl('Стоматология. Бюджет.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Стоматология. Внебюджет.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Стоматология. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarise(n = n())
entrants %>% filter(grepl('Стоматология. Целевые места.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Стоматология. Квота особого права.', competitive_groups)) %>% nrow()
entrants %>% filter(grepl('Стоматология. Квота особого права.', competitive_groups), benefit_document_type == 'Инвалид') %>% nrow()
entrants %>% filter(grepl('Стоматология. Внебюджет.', competitive_groups), benefit_document_type == 'Инвалид') %>% nrow()
entrants %>% filter(grepl('Стоматология. Квота особого права.', competitive_groups), benefit_document_type == 'Сирота') %>% nrow()

entrants %>% filter(grepl('Стоматология. Бюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Стоматология. Целевые места.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Стоматология. Квота особого права.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(grepl('Стоматология. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())

entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Стоматология. Бюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Стоматология. Целевые места.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Стоматология. Квота особого права.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())
entrants %>% filter(education_document_date > as.Date('2020-09-30'), grepl('Стоматология. Внебюджет.', competitive_groups)) %>% group_by(education_document_type) %>% summarize(n = n())

entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Стоматология. Бюджет.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Стоматология. Целевые места.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Стоматология. Квота особого права.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())
entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument', grepl('Стоматология. Внебюджет.', competitive_groups)) %>% group_by(exam_category) %>% summarize(n = n())

c06_entrants %>% filter(grepl('Стоматология', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Стоматология', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Стоматология', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Инвалид') %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name), benefit_document_type == 'Сирота') %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))

c06_entrants %>% filter(grepl('Стоматология. Целевые места.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Целевые места.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Целевые места.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Стоматология. Целевые места.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Стоматология. Целевые места.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Стоматология. Целевые места.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Стоматология. Целевые места.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Стоматология. Бюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Бюджет.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Бюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c06_entrants %>% filter(grepl('Стоматология. Бюджет.', enrolled_name)) %>% summarise(mean = mean(achievements))
c06_entrants %>% filter(grepl('Стоматология. Бюджет.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c06_entrants %>% filter(grepl('Стоматология. Бюджет.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c06_entrants %>% filter(grepl('Стоматология. Бюджет.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', exeptioned_name)) %>% group_by(exam_category) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% group_by(exam_category) %>% summarise(mean = mean(mean_ege))
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% summarise(mean = mean(achievements))
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% mutate(full_sum = sum + achievements) %>% summarise(min = min(full_sum))
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% summarise(mean_min = mean(min(biology_value), min(chemistry_value), min(russian_value)))
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% summarise(mean = mean(sum/3))

c06_entrants %>% filter(grepl('Стоматология. Бюджет.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% group_by(achievements) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Целевые места.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c06_entrants %>% filter(grepl('Стоматология. Квота особого права.', enrolled_name)) %>% group_by(education_document_type) %>% summarise(n = n())
c09_entrants %>% filter(grepl('Стоматология. Внебюджет.', enrolled_name)) %>% group_by(education_document_type, exam_category) %>% summarise(n = n(), mean_ege = mean(mean_ege), mean_ach = mean(achievements))

enrolled_entrants %>% filter(education_source_id != 15) %>% group_by(education_document_type) %>% summarize(n = n())
enrolled_entrants %>% filter(education_source_id == 15) %>% group_by(education_document_type) %>% summarize(n = n())
enrolled_entrants %>% filter(education_document_date > as.Date('2020-09-30'), education_source_id != 15) %>% group_by(education_document_type) %>% summarize(n = n())
enrolled_entrants %>% filter(education_document_date > as.Date('2020-09-30'), education_source_id == 15) %>% group_by(education_document_type) %>% summarize(n = n())

c06_entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument') %>% group_by(exam_category) %>% summarise(n = n())
c09_entrants %>% filter(education_document_type == 'MiddleEduDiplomaDocument' | education_document_type == 'SchoolCertificateDocument') %>% group_by(exam_category) %>% summarise(n = n())


print('подано зявлений через разные способы подачи документов')
for(j in 1:length(levels(entrants$source))) {
  source_name <- levels(entrants$source)[j]
  sum <- 0
  for(i in 1:length(competitive_groups_names[,1])){
    name <- as.character(competitive_groups_names[,1][i])
    sum <- sum + entrants %>% filter(source == source_name, grepl(name, competitive_groups)) %>%
      nrow()
  }
  print(paste(source_name, sum, sep = ' '))
}

print('подано согласий')
for(j in 1:length(levels(entrants$source))) {
  source_name <- levels(entrants$source)[j]
  sum <- 0
  for(i in 1:length(competitive_groups_names[,1])){
    name <- as.character(competitive_groups_names[,1][i])
    sum <- sum + entrants %>% filter(source == source_name, grepl(name, competitive_groups), agreement == name) %>%
      nrow()
  }
  print(paste(source_name, sum, sep = ' '))
}

print('количество отзывов')
for(j in 1:length(levels(entrants$source))) {
  source_name <- levels(entrants$source)[j]
  sum <- 0
  for(i in 1:length(competitive_groups_names[,1])){
    name <- as.character(competitive_groups_names[,1][i])
    sum <- sum + entrants %>% filter(source == source_name, grepl(name, competitive_groups), !is.na(return_documents_date)) %>%
      nrow()
  }
  print(paste(source_name, sum, sep = ' '))
}

print('зачислено через разные способы подачи документов')
for(j in 1:length(levels(enrolled_entrants$source))) {
  source_name <- levels(enrolled_entrants$source)[j]
  sum <- 0
  for(i in 1:length(competitive_groups_names[,1])){
    name <- as.character(competitive_groups_names[,1][i])
    sum <- sum + enrolled_entrants %>% filter(source == source_name, name == enrolled_name) %>%
      nrow()
  }
  print(paste(source_name, sum, sep = ' '))
}

#1 мониторинг
#выборки студентов
#зачисленные поступающие
enrolled_entrants <- entrants %>% 
  filter(!is.na(enrolled_name), status_id == 4)
#студенты, поступившие на бюджетную форму обучения
c06_entrants <- enrolled_entrants %>% 
  filter(grepl('Бюджет|Квота|Целевые', enrolled_name))
#студенты, поступившие на платную форму обучения
c09_entrants <- enrolled_entrants %>% 
  filter(grepl('Внебюджет', enrolled_name))
#студенты, поступившие в рамках целевой квоты
c10_entrants <- enrolled_entrants %>% 
  filter(grepl('Целевые', enrolled_name))
#студенты, получившие предыдущее образование в другом регионе
c11_entrants <- enrolled_entrants %>% 
  filter(region_with_type != 'Ивановская обл' | is.na(region_with_type) == T)
#иностранные студенты
c13_entrants <- enrolled_entrants %>% 
  filter(nationality != 'Российская Федерация')
#студенты с первым высшим образованием
c17_entrants <- enrolled_entrants %>% 
  filter(education_document_type != 'HighEduDiplomaDocument')
#студенты с первым высшим по результам ЕГЭ на общий конкурс
c18_entrants <- c17_entrants %>%
  filter(grepl('Бюджет|Внебюджет', enrolled_name), exam_category == 'ЕГЭ', olympic_type != 'Без ВИ' | is.na(olympic_type) == T)
#студенты с первым высшим на платную форму обучения
c19_entrants <- c18_entrants %>%
  filter(grepl('Внебюджет', enrolled_name))
#студенты с первым высшим, поступившие без ВИ
c22_entrants <- c17_entrants %>%
  filter(grepl('Бюджет|Внебюджет', enrolled_name), olympic_type == 'Без ВИ')
#студенты с первым высшим, принятые по результам ЕГЭ на бюджетную форму обучения
c29_entrants <- c18_entrants %>%
  filter(grepl('Бюджет', enrolled_name))
#студенты с первым высшим, принятые по результатам ЕГЭ на внебюджетную форму обучения
c31_entrants <- c19_entrants %>%
  filter(grepl('Внебюджет', enrolled_name))
#студенты с первым высшим, принятые по результатам ЕГЭ на целевые места
c33_entrants <- c10_entrants %>%
  filter(exam_category == 'ЕГЭ', olympic_type != 'Без ВИ' | is.na(olympic_type) == T)
#студенты, принятые на бюджет из числа получивших предыдущее образование в другом регионе
c36_entrants <- c11_entrants %>%
  filter(grepl('Бюджет|Квота|Целевые', enrolled_name))

#расчет показателей
#зачисленные поступающие - расчет общей суммы и суммы ЕГЭ
enrolled_entrants <- enrolled_entrants %>% 
  mutate(full_summa = sum + achievements, sum_ege = mean_ege*ege_count)
#общее количество зачисленных
c05 <- enrolled_entrants %>% 
  group_by(direction) %>% 
  summarise(n = n())
#количество зачисленных на бюджет
c06 <- c06_entrants %>% 
  group_by(direction) %>%
  summarise(n = n())
#количество зачисленных на внебюджет
c09 <- c09_entrants %>% 
  group_by(direction) %>%
  summarise(n = n())
#количество зачисленных на целевые места
c10 <- c10_entrants %>% 
  group_by(direction) %>%
  summarise(n = n())
#предыдущее образование получено в другом регионе
c11 <- c11_entrants %>% 
  group_by(direction) %>%
  summarise(n = n())
#иностранные граждане всего
c13 <- c13_entrants %>% 
  group_by(direction) %>%
  summarise(n = n())
#первое высшее образование
c17 <- c17_entrants %>%
  group_by(direction) %>%
  summarise(n = n())
#первое высшее по результатам ЕГЭ на общий конкурс
c18 <- c18_entrants %>%
  group_by(direction) %>%
  summarise(n = n())
#первое высшее на платную форму обучения
c19 <- c19_entrants %>%
  group_by(direction) %>%
  summarise(n = n())
#первое высшее без ВИ
c22 <- c22_entrants %>%
  group_by(direction) %>%
  summarise(n = n())
#средний балл ЕГЭ студентов бюджетной формы обучения, прнятых на обучение с учетом ЕГЭ
c29 <- c29_entrants %>%
  group_by(direction) %>% 
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))
#средний балл ЕГЭ студентов платной формы обучения, прнятых на обучение с учетом ЕГЭ
c31 <- c31_entrants %>%
  group_by(direction) %>% 
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))
#средний балл ЕГЭ студентов, принятых на целевое обучение с учетом ЕГЭ
c33 <- c33_entrants %>%
  group_by(direction) %>%
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))
#количество студентов, получивших предыдущее образование в другом регионе и поступившие на бюджет
c36 <- c36_entrants %>%
  group_by(direction) %>%
  summarise(n = n())

write.csv(c18_entrants, '~/c18_entrants.csv')
write.csv(c19_entrants, '~/c19_entrants.csv')

budget_entrants <- entrants %>%
  filter(grepl('Бюджет|Квота|Целевые', competitive_groups))
paid_entrants <- entrants %>%
  filter(grepl('Внебюджет', competitive_groups))
for(i in 1:length(competitive_groups_names[,1])){
 print(paste(competitive_groups_names[,1][i], entrants %>% filter(education_document_date > as.Date('2019-10-01') ,education_document_type == 'SchoolCertificateDocument', grepl(as.character(competitive_groups_names[,1][i]), competitive_groups)) %>%
   nrow(), sep= ' - '))
}

sum <- 0
for(i in 1:length(competitive_groups_names[,1])){
  sum <- sum + paid_entrants %>% filter(exam_category == 'ЕГЭ', grepl(as.character(competitive_groups_names[,1][i]), competitive_groups)) %>%
    nrow()
}

entrants %>% filter(education_document_type == 'HighEduDiplomaDocument', grepl("Лечебное дело. Внебюджет.", competitive_groups)) %>%
  nrow()
entrants %>% filter(education_document_type == 'HighEduDiplomaDocument', grepl("Педиатрия. Внебюджет.", competitive_groups)) %>%
  nrow()
entrants %>% filter(education_document_type == 'HighEduDiplomaDocument', grepl("Стоматология. Внебюджет.", competitive_groups)) %>%
  nrow()

school_ege_entrants <- entrants %>%
  group_by(education_document_type, exam_category) %>%
  summarise(n = n())



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
  filter(exam_category == 'ЕГЭ', sum == min(full_sum)) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

mean_ege_exam <- enrolled_entrants %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege = mean(sum/3, na.rm = T))


f_21_6 <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name) %>% 
  filter(exam_category == 'ЕГЭ', sum == min(full_sum)) %>% 
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
  filter(exam_category == 'ЕГЭ', sum == min(full_sum)) %>% 
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

write.csv(entrants, '~/2021.csv')
