# Laboratory Findings Analysis
# This script analyzes the laboratory findings in celiac disease patients

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gt)

# Define paths
DATA_DIR <- "~/Yandex.Disk/data/gostuhina"
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
RESULTS_DIR <- file.path(DATA_DIR, "results")
TABLES_DIR <- file.path(RESULTS_DIR, "tables")
FIGURES_DIR <- file.path(RESULTS_DIR, "figures")
ANALYSIS_DIR <- "~/R/gostuhina/analysis"

# Source functions
source(file.path(ANALYSIS_DIR, "functions.R"))

# Read processed data
data_lab <- read_csv(file.path(PROCESSED_DIR, "data_lab.csv"))

data_lab <- data_lab %>%
  mutate(age_group = factor(age_group, 
                          levels = c("1-2 года", "3-7 лет", "8-11 лет", "12-17 лет"),
                          ordered = TRUE)) %>%
  mutate(across(c(`повышение АЛТ`, `повышение АСТ`, `снижение ферритина`,
                  `дефицит витамина В12`, `дефицит фолиевой кислоты`,
                  `дефицит витамина D`, `колебания ТТГ`, `колебания Т4`,
                  `повышение анти-ТПО`),
                ~factor(., levels = c("Normal", "Abnormal")))) %>%
  mutate(Пол = factor(Пол, levels = c("Male", "Female"))) %>%
  mutate(group = factor(group))

# List of tests to test
tests <- c("повышение АЛТ", "повышение АСТ", "снижение ферритина",
                  "дефицит витамина В12", "дефицит фолиевой кислоты",
                  "дефицит витамина D", "колебания ТТГ", "колебания Т4",
                  "повышение анти-ТПО")

# Calculate frequencies for all tests
frequency <- map_dfr(tests, ~calculate_frequencies(data_lab, .x))

# Calculate frequencies for all tests by age groups
frequencies_by_age <- map_dfr(tests, ~calculate_frequencies_by_age(data_lab, .x))

# Perform Fisher test for each test
fisher_results <- map_dfr(tests, ~perform_fisher_test(data_lab, .x)) %>%
  arrange(p_value)

# Perform Fisher test for each test by age groups
fisher_results_by_age <- map_dfr(tests, ~perform_fisher_test_by_age(data_lab, .x))

# Filter significant results and create formatted table
significant_tests <- fisher_results %>%
  filter(p_value < 0.05) %>%
  mutate(
    p_value = ifelse(p_value < 0.0001,
                     sprintf("%.2e", p_value),
                     sprintf("%.4f", p_value))
  ) 
significant_tests_table <- significant_tests %>%
  gt() %>%
  tab_header(
    title = "Различия в частоте показателей в группах пациентов с целиакией и без",
    subtitle = "Представлены показатели, для которых выявлены статистически значимые различия (p < 0.05)"
  ) %>%
  cols_label(
    Симптом = "Показатель",
    p_value = "Значение p"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(3)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.title.font.size = "16px",
    heading.subtitle.font.size = "14px",
    column_labels.font.weight = "bold",
    table.font.size = "12px"
  )

# Filter significant results and create formatted table by age groups
significant_tests_by_age <- fisher_results_by_age %>%
  filter(
    if_any(
      c(group_1.2.года, group_3.7.лет, group_8.11.лет,
        group_12.17.лет), ~(.) < 0.05
    )
  ) %>%
  mutate(
    across(starts_with("group_"),
           ~ifelse(. < 0.05,
                  ifelse(. < 0.0001,
                         sprintf("%.2e", .),
                         sprintf("%.4f", .)),
                  "Не значимо"))
  ) 
significant_tests_by_age_table <- significant_tests_by_age %>%
  gt() %>%
  tab_header(
    title = "Различия в частоте показателей между возрастными подгруппами в группах пациентов с целиакией и без",
    subtitle = "Представлены показатели, для которых выявлены статистически значимые различия хотя бы в одной подгруппе (p < 0.05)"
  ) %>%
  cols_label(
    Симптом = "Показатель",
    "group_1.2.года" = "1-2 года",
    "group_3.7.лет" = "3-7 лет",
    "group_8.11.лет" = "8-11 лет",
    "group_12.17.лет" = "12-17 лет"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(3)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.title.font.size = "16px",
    heading.subtitle.font.size = "14px",
    column_labels.font.weight = "bold",
    table.font.size = "12px"
  )

significant_tests_list_by_age <- significant_tests_by_age_table$`_data`$Симптом

significant_frequencies_of_tests_by_age <- frequencies_by_age %>%
  filter(Symptom %in% significant_tests_list_by_age) 
significant_frequencies_of_tests_by_age_table <- significant_frequencies_of_tests_by_age %>%
  gt() %>%
  tab_header(
    title = "Частота значимых показателей между возрастными подгруппами в группах пациентов с целиакией и без",
    subtitle = "Представлены показатели, для которых выявлены статистически значимые различия хотя бы в одной подгруппе (p < 0.05)"
  ) %>%
  cols_label(
    age_group = "Возраст",
    group = "Группа",
    Symptom = "Показатель",
    cases = "Положителен",
    percentage = "Процент"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(3)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.title.font.size = "16px",
    heading.subtitle.font.size = "14px",
    column_labels.font.weight = "bold",
    table.font.size = "12px"
  )

frequency_table <- frequency %>%
  gt() %>%
  tab_header(
    title = "Частота показателей в группах пациентов с целиакией и без",
  ) %>%
  cols_label(
    group = "Группа",
    Symptom = "Показатель",
    count = "Количество анализов",
    cases = "Положителен",
    percentage = "Процент"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "bottom",
        weight = px(3)
      )
    ),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.title.font.size = "16px",
    heading.subtitle.font.size = "14px",
    column_labels.font.weight = "bold",
    table.font.size = "12px"
  )

# Save results for p-values table
gtsave(significant_tests_by_age_table, 
       filename = file.path(TABLES_DIR, "significant_tests_by_age_table.html"))

gtsave(significant_tests_table, 
       filename = file.path(TABLES_DIR, "significant_tests_table.html"))

# Save results for frequencies table
gtsave(significant_frequencies_of_tests_by_age_table,
       filename = file.path(TABLES_DIR, "significant_frequencies_of_tests_by_age_table.html"))

# Save results for frequencies table
gtsave(frequency_table,
       filename = file.path(TABLES_DIR, "frequency_table.html"))