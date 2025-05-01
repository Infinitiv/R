library(tidyverse)
library(jsonlite)
host = "https://priem.ivgmu.ru"
path = "api/stats"
campaign = 1
entrant_applications <- data.frame(fromJSON(paste(host, path, campaign, "entrant_applications", sep = "/")))

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
  mutate(
    basis = ifelse(
    education_source == "По договору об оказании платных образовательных услуг", 
    "Внебюджет",
    "Бюджет"
  )
  )

write_csv2(entrant_applications, '~/специалитет2023.csv')

sveden_priem_foreign <- entrant_applications %>%
  filter(nationality != "РОССИЯ") %>%
  group_by(direction, nationality, basis) %>%
  summarise(
    count_entrants = n(),
    count_enrolled = sum(!is.na(enrolled_date))
  )

sveden_priem_platn_enrolled <- entrant_applications %>%
  filter(!is.na(enrolled_date), basis == "Внебюджет") %>%
  group_by(direction) %>%
  summarise(
    count_russian = sum(nationality == "РОССИЯ"),
    count_foreign = sum(nationality != "РОССИЯ"),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
    min = min(full_sum, na.rm = T)
  )

library(knitr)

# Assuming sveden_priem_platn_enrolled is already created
sveden_priem_platn_enrolled %>%
  kable(
    caption = "Summary of Enrolled Students in Paid Programs", 
    format = "html",
    table.attr = "border=1",
    padding = 2,
  ) %>%
  write_file("~/sveden_priem_platn_enrolled.html")

sveden_priem_platn <- entrant_applications %>%
  filter(basis == "Внебюджет") %>%
  group_by(direction) %>%
  summarise(
    count_russian = sum(nationality == "РОССИЯ"),
    count_foreign = sum(nationality != "РОССИЯ")
  )

sveden_priem_platn_enrolled <- entrant_applications %>%
  filter(!is.na(enrolled_date), basis == "Внебюджет") %>%
  group_by(direction) %>%
  summarise(
    count_russian = sum(nationality == "РОССИЯ"),
    count_foreign = sum(nationality != "РОССИЯ"),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
    min = min(full_sum, na.rm = T)
  )

sveden_priem_platn_enrolled_russian <- entrant_applications %>%
  filter(!is.na(enrolled_date), basis == "Внебюджет", nationality == "РОССИЯ") %>%
  group_by(direction) %>%
  summarise(
    count_ege = sum(test_type == "ЕГЭ"),
    count_ege_exam = sum(test_type == "ЕГЭ+ВИ"),
    count_exam = sum(test_type == "ВИ"),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
    min = min(full_sum, na.rm = T)
  )

sveden_priem_platn_enrolled_foreign <- entrant_applications %>%
  filter(!is.na(enrolled_date), basis == "Внебюджет", nationality != "РОССИЯ") %>%
  group_by(direction) %>%
  summarise(
    count_ege = sum(test_type == "ЕГЭ"),
    count_ege_exam = sum(test_type == "ЕГЭ+ВИ"),
    count_exam = sum(test_type == "ВИ"),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
    min = min(full_sum, na.rm = T)
  )

education <- entrant_applications %>%
  group_by(direction, education_source, examless_type) %>%
  summarise(
    n = n(),
    count_school = sum(grepl("аттестат", education_document, ignore.case = T)),
    count_college = sum(grepl("диплом о среднем", education_document, ignore.case = T)), 
    count_high = sum(grepl("диплом специалиста|диплом бакалавра|диплом магистра", education_document, ignore.case = T)), 
  )

education_enrolled_att <- entrant_applications %>%
  filter(!is.na(enrolled_date), grepl("аттестат", education_document, ignore.case = T)) %>%
  group_by(direction, education_source, examless_type) %>%
  summarise(
    count_ege = sum(test_type == "ЕГЭ"),
    count_ege_exam = sum(test_type == "ЕГЭ+ВИ"),
    count_exam = sum(test_type == "ВИ"),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
  )

education_enrolled_college <- entrant_applications %>%
  filter(!is.na(enrolled_date), grepl("диплом о среднем", education_document, ignore.case = T)) %>%
  group_by(direction, education_source, examless_type) %>%
  summarise(
    count_ege = sum(test_type == "ЕГЭ"),
    count_ege_exam = sum(test_type == "ЕГЭ+ВИ"),
    count_exam = sum(test_type == "ВИ"),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
  )

education_enrolled_high <- entrant_applications %>%
  filter(!is.na(enrolled_date), grepl("диплом специалиста|диплом бакалавра|диплом магистра", education_document, ignore.case = T)) %>%
  group_by(direction, education_source, examless_type) %>%
  summarise(
    count_ege = sum(test_type == "ЕГЭ"),
    count_ege_exam = sum(test_type == "ЕГЭ+ВИ"),
    count_exam = sum(test_type == "ВИ"),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
  )

education_enrolled_att_all <- entrant_applications %>%
  filter(!is.na(enrolled_date), grepl("аттестат", education_document, ignore.case = T)) %>%
  group_by(direction, basis) %>%
  summarise(
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
  )

education_enrolled_college_all <- entrant_applications %>%
  filter(!is.na(enrolled_date), grepl("диплом о среднем", education_document, ignore.case = T)) %>%
  group_by(direction, basis) %>%
  summarise(
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
  )

education_enrolled_all <- entrant_applications %>%
  filter(!is.na(enrolled_date)) %>%
  group_by(direction, basis) %>%
  summarise(
    n = n(),
    mean_ege  = round(mean(mean_ege, na.rm = T), 2),
    mean_exam = round(mean(mean_exam, na.rm = T), 2),
    mean_achievement = round(mean(achievement_sum, na.rm = T), 2),
  )
