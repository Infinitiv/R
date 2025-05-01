library(tidyverse)
library(jsonlite)
host = 'https://priem.ivgmu.ru'
path = 'api/stats'
campaign = 3
entrant_applications <- data.frame(fromJSON(paste(host, path, campaign, 'entrant_applications', sep = '/')))

entrant_applications <- entrant_applications %>% filter(stage != 0)
entrant_applications$education_document_date <- as.Date(entrant_applications$education_document_date)

entrant_applications <- entrant_applications %>% 
  arrange(application_number) %>%
  mutate(mean = round(sum/test_count, 2), 
         mean_ege = round(sum_ege/ege_count, 2), 
         sum_exam = sum - sum_ege,
         exam_count = test_count - ege_count,
         mean_exam = round(sum_exam/exam_count, 2))

entrant_applications <- entrant_applications %>%
  mutate(basis = ifelse(education_source == 'По договору об оказании платных образовательных услуг', 'Внебюджет', 'Бюджет'))

enrolled <- entrant_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction, basis) %>%
  summarise(n = n())

vpo_1_2_1_1_min <- entrant_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction, education_source) %>%
  summarise(
    min_1 = min(ifelse(test_type_1 == 'ЕГЭ', mark_1, NA), na.rm = TRUE),
    min_2 = min(ifelse(test_type_1 == 'ЕГЭ', mark_2, NA), na.rm = TRUE),
    min_3 = min(ifelse(test_type_1 == 'ЕГЭ', mark_3, NA), na.rm = TRUE),
    mean = sum(c(min_1, min_2, min_3))/3
  )

vpo_2_3 <- entrant_applications %>%
  filter(!is.na(enrolled_date), education_source == 'Целевая квота') %>%
  group_by(competitive_group_name) %>%
  summarise(n = n())

vpo_2_8_sum <- entrant_applications %>%
  filter(!is.na(enrolled_date), direction != 'Сестринское дело', nationality != 'РОССИЯ') %>%
  summarise(
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом', education_document, ignore.case = T))
  )

vpo_2_8_sum <- entrant_applications %>%
  filter(!is.na(enrolled_date), direction == 'Сестринское дело', nationality != 'РОССИЯ') %>%
  summarise(
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом', education_document, ignore.case = T))
  )

vpo_2_8 <- entrant_applications %>%
  filter(!is.na(enrolled_date), direction == 'Сестринское дело', education_document_date > as.Date('2023-10-01')) %>%
  summarise(
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом', education_document, ignore.case = T))
  )

vpo_2_8 <- entrant_applications %>%
  filter(!is.na(enrolled_date), direction != 'Сестринское дело', education_document_date > as.Date('2023-10-01'), nationality != 'РОССИЯ') %>%
  summarise(
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом', education_document, ignore.case = T))
  )

f_2_1 <- entrant_applications %>%
  group_by(direction, basis) %>%
  summarise(
    n = n(),
    count_special = sum(education_source == 'Особая квота'),
    count_separated = sum(education_source == 'Отдельная квота'),
    count_target = sum(education_source == 'Целевая квота'),
    count_common = sum(education_source == 'Основные места в рамках КЦП' | education_source == 'По договору об оказании платных образовательных услуг'),
    count_epgu = sum(source == 'через ЕПГУ'),
    count_personal = sum(source == 'лично'),
    count_is = sum(source == 'через ИС'),
    count_disabled = sum(grepl('инвалид', benefit_documents, ignore.case = T)),
    count_svo = sum(grepl('СВО', benefit_documents))
    )

f_3_2 <- entrant_applications %>%
  filter(!is.na(enrolled_date), education_source == 'Особая квота') %>%
  group_by(direction) %>%
  summarise(
    n = n(),
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом', education_document, ignore.case = T)), 
    count_ege = sum(test_type == 'ЕГЭ'),
    count_exam = sum(test_type == 'ВИ'),
    count_ege_exam = sum(test_type == 'ЕГЭ+ВИ'),
    count_disabled = sum(grepl('инвалид', benefit_documents, ignore.case = T)),
    count_orphan = sum(grepl('сирот', benefit_documents, ignore.case = T)),
    )

f_3_3 <- entrant_applications %>%
  filter(!is.na(enrolled_date), education_source == 'Целевая квота') %>%
  group_by(direction) %>%
  summarise(
    n = n(),
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом', education_document, ignore.case = T)), 
    count_ege = sum(test_type == 'ЕГЭ'),
    count_exam = sum(test_type == 'ВИ'),
    count_ege_exam = sum(test_type == 'ЕГЭ+ВИ')
  )

f_3_4 <- entrant_applications %>%
  filter(!is.na(enrolled_date), (education_source == 'Основные места в рамках КЦП' | education_source == 'По договору об оказании платных образовательных услуг')) %>%
  group_by(direction, basis) %>%
  summarise(
    n = n(),
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом о среднем', education_document, ignore.case = T)), 
    count_ege = sum(test_type == 'ЕГЭ'),
    count_exam = sum(test_type == 'ВИ'),
    count_ege_exam = sum(test_type == 'ЕГЭ+ВИ')
  )

f_3_5 <- entrant_applications %>%
  filter(!is.na(enrolled_date), education_source == 'Отдельная квота') %>%
  group_by(direction, examless_type) %>%
  summarise(
    n = n(),
    count_school = sum(grepl('аттестат', education_document, ignore.case = T)),
    count_college = sum(grepl('диплом о среднем', education_document, ignore.case = T)), 
    count_ege = sum(test_type == 'ЕГЭ'),
    count_exam = sum(test_type == 'ВИ'),
    count_ege_exam = sum(test_type == 'ЕГЭ+ВИ')
  )

f_3_7 <- entrant_applications %>%
  filter(!is.na(enrolled_date), nationality != 'РОССИЯ') %>%
  group_by(direction, basis, nationality) %>%
  summarise(n = n())

f_4_1 <- entrant_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction, education_source) %>%
  summarise(
    mean_ege = round(mean(mean_ege, na.rm = T), 2)
  )

f_4_2 <- entrant_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction, basis) %>%
  summarise(
    count_3 = sum(grepl('ГТО', achievements)),
    count_5 = sum(grepl('аттестат|диплом', achievements, ignore.case = T)),
    count_7 = sum(grepl('олимпиады|конкурса', achievements, ignore.case = T)),
    count_9 = sum(grepl('Абилимпикс', achievements, ignore.case = T)),
    count_11 = sum(grepl('За особые успехи в учении', achievements, ignore.case = T)),
    count_12 = sum(grepl('Прохождение', achievements, ignore.case = T)),
    count_13 = sum(grepl('Пребывание', achievements, ignore.case = T)),
  )

f_4_3 <- entrant_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction, education_source) %>%
  summarise(
    min = (min(full_sum))
  )

f_4_4_1 <- entrant_applications %>%
  filter(!is.na(enrolled_date), test_type_1 == 'ЕГЭ') %>%
  group_by(direction, education_source, test_subject_1) %>%
  summarise(
    n = n(),
    mean_ege = round(mean(mark_1, na.rm = T), 2)
  )

f_4_4_2 <- entrant_applications %>%
  filter(!is.na(enrolled_date), test_type_2 == 'ЕГЭ') %>%
  group_by(direction, education_source, test_subject_2) %>%
  summarise(
    n = n(),
    mean_ege = round(mean(mark_2, na.rm = T), 2)
  )

f_4_4_3 <- entrant_applications %>%
  filter(!is.na(enrolled_date), test_type_3 == 'ЕГЭ') %>%
  group_by(direction, education_source, test_subject_3) %>%
  summarise(
    n = n(),
    mean_ege = round(mean(mark_3, na.rm = T), 2)
  )

f_6_1 <- entrant_applications %>%
  group_by(direction) %>%
  summarise(
    distinct = n_distinct(application_number)
  )

# entrant_applications$source <- as.factor(entrant_applications$source)
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

