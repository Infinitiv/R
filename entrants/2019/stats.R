library(tidyverse)
library(jsonlite)
host = 'http://priem.isma.ivanovo.ru'
path = 'api/stats'
campaign = 7
entrants <- data.frame(fromJSON(paste(host, path, campaign, 'entrants', sep = '/')))

path = 'api/campaigns'
campaigns <- data.frame(fromJSON(paste(host, path, sep = '/')))
competitive_groups <- campaigns %>%
  filter(campaigns.campaign_type_id == 1, campaigns.year_start == 2020) %>%
  select(campaigns.competitive_groups)
competitive_groups_names <- as.data.frame(competitive_groups$campaigns.competitive_groups) %>% 
  select(name)

#1 мониторинг
#выборки студентов
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
  filter(nationality != 1)
#студенты с первым высшим образованием
c17_entrants <- enrolled_entrants %>% 
  filter(education_document_type != 'HighEduDiplomaDocument')
#студенты с первым высшим по результам ЕГЭ на общий конкурс
c18_entrants <- c17_entrants %>%
  filter(grepl('Бюджет|Внебюджет', enrolled_name), exam_category == 'ЕГЭ', olympic_type != 'Без ВИ' | is.na(olympic_type) == T)
#студенты с первым высшим на платную форму обучения
c19_entrants <- c18_entrants %>%
  filter(grepl('Внебюджет', enrolled_name))
#студенты с первым высшим по результам ЕГЭ и дополнительных испытаний на общий конкурс
c20_entrants <- c17_entrants %>%
  filter(grepl('Бюджет|Внебюджет', enrolled_name), exam_category == 'смешанный', olympic_type != 'Без ВИ' | is.na(olympic_type) == T)
#студенты с первым высшим по результам ЕГЭ и дополнительных испытаний на платную форму обучения
c21_entrants <- c20_entrants %>%
  filter(grepl('Внебюджет', enrolled_name))
#студенты с первым высшим, поступившие без ВИ
c22_entrants <- c17_entrants %>%
  filter(grepl('Бюджет|Внебюджет', enrolled_name), olympic_type == 'Без ВИ')

#расчет показателей
#зачисленные поступающие
enrolled_entrants <- entrants %>% 
  filter(!is.na(enrolled_name), status_id == 4)
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
#первое высшее по результам ЕГЭ и дополнительных испытаний на общий конкурс
c20 <- c20_entrants %>%
  group_by(direction) %>%
  summarise(n = n())
#первое высшее по результам ЕГЭ и дополнительных испытаний на платную форму обучения
c21 <- c21_entrants %>%
  group_by(direction) %>%
  summarise(n = n())
#первое высшее без ВИ
c22 <- c22_entrants %>%
  group_by(direction) %>%
  summarise(n = n())


#средний балл ЕГЭ студентов бюджетной формы обучения, прнятых на обучение с учетом ЕГЭ
c29 <- c18_entrants %>%
  filter(grepl('Бюджет', enrolled_name)) %>%
  group_by(direction) %>% 
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))
#средний балл ЕГЭ студентов бюджетной формы обучения принятых на обучение с учетом ЕГЭ и дополнительных испытаний
c30 <- c20_entrants %>%
  filter(grepl('Бюджет', enrolled_name)) %>%
  group_by(direction) %>% 
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))
#средний балл ЕГЭ студентов платной формы обучения, прнятых на обучение с учетом ЕГЭ
c31 <- c19_entrants %>%
  filter(grepl('Внебюджет', enrolled_name)) %>%
  group_by(direction) %>% 
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))
#средний балл ЕГЭ студентов платной формы обучения принятых на обучение с учетом ЕГЭ и дополнительных испытаний
c32 <- c21_entrants %>%
  filter(grepl('Внебюджет', enrolled_name)) %>%
  group_by(direction) %>% 
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))
c33 <- c10_entrants %>%
  group_by(direction) %>%
  summarise(mean_mean_ege = mean(mean_ege, na.rm = T))



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
