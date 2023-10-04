library(tidyverse)
library(jsonlite)
host = 'https://priem.ivgma.ru'
path = 'api/stats'
campaign = 1

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

all_budget_applications <- budget_applications %>%
  group_by(direction) %>%
  summarise(n = n())

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

f_1_3_1 <- enrolled %>%
  group_by(direction, education_source, education_document) %>%
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