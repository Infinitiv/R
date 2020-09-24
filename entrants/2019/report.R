  library(tidyverse)
  library(jsonlite)
  host = 'http://priem.isma.ivanovo.ru'
  path = 'api/stats'
  campaigns <- data.frame(fromJSON(paste(host, path, 'campaigns', sep = '/')))
  campaigns <- campaigns %>% arrange(year_start)
  entrants <- data.frame(fromJSON(paste(host, path, 2016, 'entrants', sep = '/')))
  for(i in 2:length(campaigns$year_start)){
    year <- campaigns$year_start[i]
    tmp <- data.frame(fromJSON(paste(host, path, year, 'entrants', sep = '/')))
    entrants <- rbind(entrants, tmp)
  }
  
  entrants$year <- as.factor(entrants$year)
  entrants$application_number <- as.factor(entrants$application_number)
  entrants$gender_id <- as.factor(entrants$gender_id)
  levels(entrants$gender_id) <- c('мужской', 'женский')
  entrants$birth_date <- as.Date(entrants$birth_date)
  entrants$region_id <- as.factor(entrants$region_id)
  entrants$registration_date <- as.Date(entrants$registration_date)
  entrants$nationality_type_id <- as.factor(entrants$nationality_type_id)
  entrants$status_id <- as.factor(entrants$status_id)
  entrants$return_documents_date <- as.Date(entrants$return_documents_date)
  entrants$direction_id <- as.factor(entrants$direction_id)
  levels(entrants$direction_id) <- c('Стоматология', 'Педиатрия', 'Лечебно дело')
  entrants$education_source_id <- as.factor(entrants$education_source_id)
  levels(entrants$education_source_id) <- c('Бюджет', 'Внебюджет', 'Целевая квота', 'Квота особого права')
  entrants$enrolled_date <- as.Date(entrants$enrolled_date)
  entrants$exeptioned_date <- as.Date(entrants$exeptioned_date)
  entrants$target_region. <- as.factor(entrants$target_region.)
  entrants$education_document_date <- as.Date(entrants$education_document_date)
  
  write.csv(entrants, '~/R/data/entrants/2020.csv')
  
  df <- read.csv('~/R/data/entrants/2020.csv')
df %>% filter(!is.na(enrolled_name), status_id == 4, education_source_id != 'Внебюджет') %>%
  group_by(direction_id, year) %>%
  summarise(mean = mean(mean_ege, na.rm = T))

current_entrants <- entrants %>% filter(year == 2020)

current_exam_form <- current_entrants %>% filter(!is.na(enrolled_name), status_id == 4, education_source_id != 'Внебюджет') %>% group_by(olympic_type, exam_category) %>% summarise(n = n())

current_exam_ege_mean <- current_entrants %>% filter(!is.na(enrolled_name), status_id == 4, education_source_id != 'Внебюджет') %>% group_by(olympic_type) %>% summarise(n = n(), mean_ege = mean(mean_ege, na.rm = T))

entrants2019 <- entrants %>% filter(year == 2019)

entrants2019_enrolled <- entrants2019 %>% filter(!is.na(enrolled_name), status_id == 4)

entrants2019_enrolled %>% group_by(direction_id, education_source_id) %>% summarise(min = min(sum + achievements))

target_by_organization <- entrants2019_enrolled %>% filter(education_source_id == 'Целевая квота') %>% group_by(direction_id, target_organization_name.) %>% summarise(n = n())
  
write.csv(target_by_organization, '~/R/data/entrants/target.csv')
entrants2019_budget <- entrants2019 %>% 
  filter(!is.na(enrolled_name), status_id == 4, education_source_id != 'Внебюджет')

entrants2019_budget_last_year <- entrants2019_budget %>% filter(education_document_date >= '2018-10-01')

entrants2019_paid_last_year <- entrants2019_paid %>% filter(education_document_date >= '2018-10-01')

entrants2019_paid <- entrants2019 %>% 
  filter(!is.na(enrolled_name), status_id == 4, education_source_id == 'Внебюджет')

entrants2019_budget %>% group_by(education_document_type, exam_category) %>% summarise(n = n())

entrants2019_paid %>% group_by(education_document_type, exam_category) %>% summarise(n = n())

entrants2019_budget %>% group_by(education_source_id, benefit_type) %>% summarise(n = n())

entrants2019_paid %>% group_by(education_source_id, benefit_type) %>% summarise(n = n())
entrants2019_budget %>% group_by(direction_id, benefit_document_type) %>% summarise(n = n())
entrants2019_enrolled %>% group_by(direction_id, gender_id) %>% summarise(n = n())

entrants2019_budget %>% group_by(education_document_type) %>% summarise(n = n())

entrants2019_budget_last_year %>% group_by(education_document_type) %>% summarise(n = n())

entrants2019_paid_last_year %>% group_by(education_document_type) %>% summarise(n = n())

entrants2019_paid %>% group_by(education_document_type) %>% summarise(n = n())

entrants2019 %>% filter(education_document_date >= as.Date("2018-09-01")) %>% 
  group_by(education_document_type, exam_category) %>% 
  summarise(n = n())

target_entrants_freq <- entrants %>% filter(!is.na(target_organization_name.)) %>% 
  group_by(year, enrolled_name) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n*100/sum(n)) %>%
  arrange(enrolled_name, year, target_region.)

target_entrants_iv_count <- entrants %>% filter(!is.na(target_organization_name.), target_region. == 37) %>% 
  group_by(year, direction_id) %>% 
  summarise(n = n(), min = min(sum + achievements)) %>% 
  arrange(direction_id, year)
    
ggplot(target_entrants_freq, aes(x = year, y = freq)) +
  geom_point(size = 2) +
  geom_text(aes(label = paste(round(freq, 2), '%', sep = '')), nudge_x = 0, nudge_y = 5) +
  facet_grid(cols = vars(target_region.), rows = vars(enrolled_name))
ggsave('Доля зачисленных целевиков по регионам.png', height = 10, width = 20)

ggplot(target_entrants_freq, aes(x = year, y = n)) +
  geom_point(size = 2) +
  geom_text(aes(label = n), nudge_x = 0, nudge_y = 5) +
  facet_grid(cols = vars(target_region.), rows = vars(enrolled_name))
ggsave('Количество зачисленных целевиков по регионам.png', height = 10, width = 20)

enrolled_entrants <- entrants %>% 
  filter(!is.na(enrolled_name), status_id == 4) %>% 
  mutate(full_summa = sum + achievements, sum_ege = mean_ege*ege_count)


exeptioned_entrants <- entrants %>%
  filter(!is.na(exeptioned_name))

enrolled_entrants %>% 
  filter(is.na(olympic_type), education_source_id != 'Внебюджет') %>% 
  group_by(year) %>%
  summarise(mean_ege = round(mean(mean_ege, na.rm = T), 2), count = n())

enrolled_entrants %>% 
  filter(is.na(olympic_type), education_source_id == 'Бюджет') %>% 
  group_by(year) %>%
  summarise(mean_ege = round(mean(mean_ege, na.rm = T), 2), count = n())

enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(year) %>%
  summarise(mean_ege = round(mean(mean_ege, na.rm = T), 2), count = n())

  ggplot(filter(enrolled_entrants, education_source_id == 16), aes(x = full_summa, color = year)) +
  geom_density() +
  facet_grid(rows = vars(direction_id))
ggsave('Плотность распределения баллов поступающих на целевые места.png', height = 10, width = 20)

enrolled_entrants_by_regions <- enrolled_entrants %>% 
  group_by(year, region_id) %>% 
  summarise(n = n()) %>% 
  mutate(sum = sum(n), freq = n*100/sum(n)) %>% 
  arrange(desc(freq)) %>% 
  filter(freq > 5)

entrants_by_regions <- entrants %>% 
  group_by(year, region_id) %>% 
  summarise(n = n()) %>% 
  mutate(sum = sum(n), freq = n*100/sum(n)) %>% 
  arrange(desc(freq)) %>% 
  filter(freq > 5)

ggplot(enrolled_entrants_by_regions, aes(x = region_id, y = freq, fill = year)) + 
  geom_col(position = 'dodge') +
  labs(x = 'Код региона', y = '%', fill = 'Приемная кампания') +
  ggtitle('Регионы, обеспечивающие более 5% зачисленных')
ggsave('Регионы, обеспечивающие более 5 процентов зачисленных.png', height = 10, width = 20)

ggplot(entrants_by_regions, aes(x = region_id, y = freq, fill = year)) + 
  geom_col(position = 'dodge') +
  labs(x = 'Код региона', y = '%', fill = 'Приемная кампания') +
  ggtitle('Регионы, обеспечивающие более 5% поступающих')
ggsave('Регионы, обеспечивающие более 5 процентов подавших документы.png', height = 10, width = 20)

  enrolled_entrants_rf <- entrants %>% 
  filter(!is.na(enrolled_name), status_id == 4, nationality_type_id == 1) %>% 
  mutate(full_summa = sum + achievements, sum_ege = mean_ege*ege_count)

count_enrolled_rf <- enrolled_entrants_rf %>%
  group_by(enrolled_name) %>%
  summarise(n = n())

ege_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name, year, education_source_id) %>%
  summarise(mean_ege = round(mean(mean_ege, na.rm = T), 2), count = n()) %>%
  arrange(education_source_id, enrolled_name, year)
ggplot(filter(ege_mean, education_source_id == 16), aes(x = count, y = mean_ege, color = year)) + 
  geom_point(cex = 5) + 
  geom_text(aes(label = round(mean_ege, 2)), nudge_x = 10, nudge_y = 0.1) +
  facet_grid(cols = vars(enrolled_name), rows = vars(education_source_id))
ggsave(filename = 'Средний балл ЕГЭ (целевая квота).png', height = 10, width = 20)

ege_exam_mean <- enrolled_entrants %>%
  filter(is.na(olympic_type)) %>%
  group_by(enrolled_name, year, education_source_id) %>%
  summarise(mean_exam = mean(sum)/3, count = n()) %>%
  arrange(education_source_id, enrolled_name, year)

ggplot(filter(ege_exam_mean, education_source_id == 14), aes(x = count, y = mean_exam, color = year)) + 
  geom_point(cex = 5) + 
  geom_text(aes(label = round(mean_exam, 2)), nudge_x = 5, nudge_y = 0.1) +
  facet_grid(cols = vars(enrolled_name), rows = vars(education_source_id))

ege_ach_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type), exam_category != 'ВИ') %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege_ach = (sum(sum_ege) + sum(achievements))/sum(ege_count))

min_full_sum <- enrolled_entrants %>%
  filter(is.na(olympic_type)) %>%
  group_by(enrolled_name, year, education_source_id) %>%
  summarise(min_full_sum = min(full_summa), count = n()) %>%
  arrange(education_source_id, enrolled_name, year)
min_full_sum$education_source_id <- as.factor(min_full_sum$education_source_id)

ggplot(filter(min_full_sum, education_source_id == 14), aes(x = count, y = min_full_sum, color = year)) + geom_point() + facet_grid(cols = vars(enrolled_name), rows = vars(education_source_id))

names(min_full_sum) <- c('Конкурс', 'Год', 'Источник финансирования', 'Проходной балл', 'Количество зачисленных')


count_enrolled <- enrolled_entrants %>%
  group_by(enrolled_name, target_region.) %>%
  summarise(n = n())

ege_target_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name, target_region.) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

ege_exam_target_mean <- enrolled_entrants %>%
  filter(is.na(olympic_type)) %>%
  group_by(enrolled_name, target_region.) %>%
  summarise(mean_exam = mean(sum)/3)

ege_ach_target_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type), exam_category != 'ВИ') %>% 
  group_by(enrolled_name, target_region.) %>% 
  summarise(mean_ege_ach = (sum(sum_ege) + sum(achievements))/sum(ege_count))

min_full_target_sum <- enrolled_entrants %>%
  filter(is.na(olympic_type)) %>%
  group_by(enrolled_name, target_region.) %>%
  summarise(min_full_sum = min(full_summa))


ege_benefit_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name, benefit_document_type) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

ege_exam_benefit_mean <- enrolled_entrants %>%
  filter(is.na(olympic_type)) %>%
  group_by(enrolled_name, benefit_document_type) %>%
  summarise(mean_exam = mean(sum)/3)

ege_ach_benefit_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type), exam_category != 'ВИ') %>% 
  group_by(enrolled_name, benefit_document_type) %>% 
  summarise(mean_ege_ach = (sum(sum_ege) + sum(achievements))/sum(ege_count))

min_full_benefit_sum <- enrolled_entrants %>%
  filter(is.na(olympic_type)) %>%
  group_by(enrolled_name, benefit_document_type) %>%
  summarise(min_full_sum = min(full_summa))


ege_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type), education_source_id != 15) %>% 
  group_by(direction_id) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))

ege_exam_mean <- enrolled_entrants %>%
  filter(is.na(olympic_type), education_source_id != 15) %>%
  group_by(direction_id) %>%
  summarise(mean_exam = mean(sum)/3)

ege_ach_mean <- enrolled_entrants %>% 
  filter(is.na(olympic_type), exam_category != 'ВИ', education_source_id != 15) %>% 
  group_by(direction_id) %>% 
  summarise(mean_ege_ach = (sum(sum_ege) + sum(achievements))/sum(ege_count))

min_full_sum <- enrolled_entrants %>%
  filter(is.na(olympic_type), education_source_id != 15) %>%
  group_by(direction_id) %>%
  summarise(min_full_sum = min(full_summa))




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

f_2_1_o <- enrolled_entrants %>% 
  filter(!is.na(olympic_type)) %>% 
  group_by(enrolled_name) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))
f_21_6 <- enrolled_entrants %>% 
  filter(is.na(olympic_type)) %>% 
  group_by(enrolled_name) %>% 
  filter(exam_category == 'ЕГЭ', sum == min(sum)) %>% 
  summarise(mean_ege = mean(mean_ege, na.rm = T))


f_3_3 <- enrolled_entrants %>%
  filter(benefit_document_type == 'Ивалид') %>%
  group_by(enrolled_name) %>%
  summarise(mean_ege = mean(mean_ege, na.rm = T))
f_3_3 <- enrolled_entrants %>%
  filter(benefit_document_type == 'Сирота') %>%
  group_by(enrolled_name) %>%
  summarise(mean_ege = mean(mean_ege, na.rm = T))