library(tidyverse)
library(jsonlite)
host = 'https://priem.ivgmu.ru'
path = 'api/stats'
campaign = 4

entrant_applications <- data.frame(fromJSON(paste(host, path, campaign, 'entrant_applications', sep = '/')))

entrant_applications <- entrant_applications %>% 
  arrange(application_number) %>%
  mutate(mean = round(sum/test_count, 2), 
         mean_ege = round(sum_ege/ege_count, 2), 
         sum_exam = sum - sum_ege,
         exam_count = test_count - ege_count,
         mean_exam = round(sum_exam/exam_count, 2))
entrant_applications$source <- as.factor(entrant_applications$source)

#форма 1.2
budget_applications <- entrant_applications %>%
  filter(education_source != 'По договору об оказании платных образовательных услуг')

paid_applications <- entrant_applications %>%
  filter(education_source == 'По договору об оказании платных образовательных услуг')

all_budget_applications <- budget_applications %>%
  group_by(direction_code, direction, education_source) %>%
  summarise(
    n = n(), 
    count_enrolled = sum(!is.na(enrolled_date)),
    count_exeptioned = sum(!is.na(exeptioned_date)),
    )

all_budget_applications_enrolled <- budget_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction_code, direction, education_source) %>%
  summarise(
    n = n(),
    count_exam_type_a = sum(test_type_1 == 'Аккредитация'),
    count_exam_type_e = sum(test_type_1 == 'ВИ'),
    mean_exam = mean(sum_exam, na.rm = T),
    mean_ach = mean(achievement_sum, na.rm = T),
    min = min(full_sum)
  )

budget_applications_enrolled <- budget_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction_code, direction) %>%
  summarise(
    n = n(),
    mean_exam = mean(sum_exam, na.rm = T),
    mean_ach = mean(achievement_sum, na.rm = T),
    min = min(full_sum)
  )

all_paid_applications <- paid_applications %>%
  group_by(direction_code, direction, nationality) %>%
  summarise(
    n = n(), 
    count_enrolled = sum(!is.na(enrolled_date)),
    count_exeptioned = sum(!is.na(exeptioned_date)),
  )

all_paid_applications_enrolled <- paid_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction_code, direction, nationality) %>%
  summarise(
    n = n(),
    count_exam_type_a = sum(test_type_1 == 'Аккредитация'),
    count_exam_type_e = sum(test_type_1 == 'ВИ'),
    mean_exam = mean(sum_exam, na.rm = T),
    mean_ach = mean(achievement_sum, na.rm = T),
    min = min(full_sum)
  )

paid_applications_enrolled <- paid_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction_code, direction) %>%
  summarise(
    n = n(),
    mean_exam = mean(sum_exam, na.rm = T),
    mean_ach = mean(achievement_sum, na.rm = T),
    min = min(full_sum)
  )

examless_budget_applications <- budget_applications %>%
  filter(examless_type == 'БВИ') %>%
  group_by(direction) %>%
  summarise(n = n())

grouped_budget_applications <- budget_applications %>%
  group_by(direction, education_source, examless_type) %>%
  summarise(n = n())

belgorod_budget_applications <- budget_applications %>%
  filter(region_with_type == 'Белгородская обл') %>%
  group_by(direction) %>%
  summarise(n = n())

kursk_bryansk_budget_applications <- budget_applications %>%
  filter(region_with_type == 'Курская обл' | region_with_type == 'Брянская обл') %>%
  group_by(direction) %>%
  summarise(n = n())

source_budget_applications <- budget_applications %>%
  group_by(direction, source) %>%
  summarise(n = n())

disabled_budget_applications <- budget_applications %>%
  filter(grepl('инвалид', benefit_documents))  %>%
  group_by(direction) %>%
  summarise(n = n())
           

enrolled <- entrant_applications %>% filter(!is.na(enrolled_date))

exeptioned <- entrant_applications %>% filter(!is.na(exeptioned_date))

f_1_3_1 <- enrolled %>%
  group_by(direction, education_source, education_document) %>%
  summarise(n = n())

target2021 <- enrolled %>%
  group_by(education_source_id) %>%
  summarise(n = n())

enrolled_mean <- enrolled %>%
  filter(education_source == 'Целевая квота') %>%
  group_by(direction, education_source) %>%
  summarise(mean = mean(full_sum, na.rm = T))

stomat_enrolled <- enrolled %>%
  filter(education_source == 'Целевая квота', direction == 'Стоматология') %>%
  select(full_sum)
  
ex <- exeptioned %>%
  group_by(direction, education_source) %>%
  summarise(n = n())

print('По специальностям и источникам подачи - всего')
entrant_applications %>% group_by(source) %>% summarise(n = n())
print('По специальностям и источникам подачи - бюджет')
entrant_applications %>% filter(education_source != 'По договору об оказании платных образовательных услуг') %>% group_by(direction, source) %>% summarise(n = n())
print('По специальностям и источникам подачи - внебюджет')
entrant_applications %>% filter(education_source == 'По договору об оказании платных образовательных услуг') %>% group_by(direction, source) %>% summarise(n = n())
print('По специальностям и источникам обучения')
entrant_applications %>% group_by(direction, education_source) %>% summarise(n = n())
print('Инвалиды по специальностям')
entrant_applications %>% filter(grepl('инвалид', benefit_documents), education_source != 'По договору об оказании платных образовательных услуг') %>% group_by(direction) %>% summarise(n = n())
entrant_applications %>% filter(grepl('инвалид', benefit_documents), education_source == 'По договору об оказании платных образовательных услуг') %>% group_by(direction) %>% summarise(n = n())
print('Специальная квота')
entrant_applications %>% filter(grepl('Отдельная квота', competitive_group_name)) %>% group_by(direction) %>% summarise(n = n())


write_csv2(entrant_applications, "~/2023_07_30_.csv")

entrants <- data.frame(fromJSON(paste(host, path, campaign, 'entrants', sep = '/')))

write_csv2(entrants, "~/2023_08_1.csv")

budget_education_level_groupped <- budget_applications %>%
  group_by(education_document) %>%
  summarise(n = n())
paid_education_level_groupped <- paid_applications %>%
  group_by(education_document) %>%
  summarise(n = n())

education_level_groupped <- enrolled %>%
  group_by(direction, education_source, education_document, test_type) %>%
  summarise(n = n())

education_level_groupped_2 <- enrolled %>%
  group_by(direction, education_source, education_document) %>%
  summarise(n = n(), mean_ege = round(mean(mean_ege, na.rm = T), 2), mean_exam = round(mean(mean_exam, na.rm = T), 2), mean_achievemnt = round(mean(achievement_sum, na.rm = T), 2))

enrolled %>% filter(education_source != 'По договору об оказании платных образовательных услуг') %>% group_by(direction, education_source) %>% summarise(n = n())

write_csv2(education_level_groupped, "~/table_1.csv")
write_csv2(education_level_groupped_2, "~/table_2.csv")
