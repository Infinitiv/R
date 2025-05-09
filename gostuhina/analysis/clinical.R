# Clinical Symptoms Analysis
# This script analyzes the clinical symptoms in celiac disease patients

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(webshot2)

# Define paths
data_dir <- "~/Yandex.Disk/data/gostuhina"
processed_dir <- file.path(data_dir, "processed")
results_dir <- file.path(data_dir, "results")
tables_dir <- file.path(results_dir, "tables")
figures_dir <- file.path(results_dir, "figures")
analysis_dir <- "~/R/gostuhina/analysis"

# Source functions
source(file.path(analysis_dir, "functions.R"))

# Read processed data
data_clinical <- read_csv(file.path(processed_dir, "data_clinical.csv"))

data_clinical <- data_clinical %>%
  mutate(age_group = factor(age_group,
                            levels = c("1-2 года", "3-7 лет",
                                       "8-11 лет", "12-17 лет"),
                            ordered = TRUE)) %>%
  mutate(across(c(`Впервые выставленная целиакия`, `Боль в животе`,
                  `Вздутие живота`, Тошнота,
                  Рвота, `Другие проявления рефлюкса`, Диарея, Полифекалия,
                  Запор, `Неустойчивый стул`, `Недостаточность питания`,
                  `Задержка роста`, Ожирение, Стоматит,
                  `Кариес, темный налет на зубах`, `Атопический дерматит`,
                  Псориаз, `Герпетиформный дерматит`, `Другие дерматиты`,
                  `Слабость, снижение аппетита`, Алопеция, `Частые ОРВИ`,
                  `Боли в суставах`),
                ~factor(., levels = c("No", "Yes")))) %>%
  mutate(Пол = factor(Пол, levels = c("Male", "Female"))) %>%
  mutate(group = factor(group))

# List of symptoms to test
symptoms <- c("Боль в животе", "Вздутие живота", "Тошнота",
              "Рвота", "Другие проявления рефлюкса", "Диарея", "Полифекалия",
              "Запор", "Неустойчивый стул", "Недостаточность питания",
              "Задержка роста", "Ожирение", "Стоматит",
              "Кариес, темный налет на зубах", "Атопический дерматит",
              "Псориаз", "Герпетиформный дерматит", "Другие дерматиты",
              "Слабость, снижение аппетита", "Алопеция", "Частые ОРВИ",
              "Боли в суставах")

# Calculate frequencies for all symptoms
frequency <- map_dfr(symptoms, ~calculate_frequencies(data_clinical, .x))

# Calculate frequencies for all symptoms by age groups
frequencies_by_age <- map_dfr(symptoms, ~calculate_frequencies_by_age(data_clinical, .x))

# Perform Fisher test for each symptom
fisher_results <- map_dfr(symptoms, ~perform_fisher_test(data_clinical, .x)) %>%
  arrange(p_value)

# Perform Fisher test for each symptom by age groups
fisher_results_by_age <- map_dfr(symptoms, ~perform_fisher_test_by_age(data_clinical, .x))

# Filter significant results and create formatted table
significant_symptoms <- fisher_results %>%
  filter(p_value < 0.05) %>%
  mutate(
    p_value = ifelse(p_value < 0.0001,
                     sprintf("%.2e", p_value),
                     sprintf("%.4f", p_value))
  ) %>%
  gt() %>%
  tab_header(
    title = "Различия в частоте симптомов в группах пациентов с целиакией и без",
    subtitle = "Представлены симптомы, для которых выявлены статистически значимые различия (p < 0.05)"
  ) %>%
  cols_label(
    Симптом = "Симптом",
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
significant_symptoms_by_age <- fisher_results_by_age %>%
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
  ) %>%
  gt() %>%
  tab_header(
    title = "Различия в частоте симптомов между возрастными подгруппами в группах пациентов с целиакией и без",
    subtitle = "Представлены симптомы, для которых выявлены статистически значимые различия хотя бы в одной подгруппе (p < 0.05)"
  ) %>%
  cols_label(
    Симптом = "Симптом",
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

significant_symptoms_list_by_age <- significant_symptoms_by_age$`_data`$Симптом

significant_frequencies_by_age <- frequencies_by_age %>%
  filter(Symptom %in% significant_symptoms_list_by_age) %>%
  gt() %>%
  tab_header(
    title = "Частота значимых симптомов между возрастными подгруппами в группах пациентов с целиакией и без",
    subtitle = "Представлены симптомы, для которых выявлены статистически значимые различия хотя бы в одной подгруппе (p < 0.05)"
  ) %>%
  cols_label(
    age_group = "Возраст",
    group = "Группа",
    Symptom = "Симптом",
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
gtsave(significant_symptoms_by_age,
       filename = file.path(tables_dir, "significant_symptoms_by_age.html"))

significant_symptoms_by_age %>%
  gtsave(
    filename = file.path(tables_dir, "significant_symptoms_by_age.png")
  )

gtsave(significant_symptoms,
       filename = file.path(tables_dir, "significant_symptoms.html"))

significant_symptoms %>%
  gtsave(
    filename = file.path(tables_dir, "significant_symptoms.png")
  )

# Save results for frequencies table
gtsave(significant_frequencies_by_age,
       filename = file.path(tables_dir, "significant_frequencies_by_age.html"))

# Save as PNG with specific dimensions for better readability
gtsave(significant_frequencies_by_age,
  filename = file.path(tables_dir, "significant_frequencies_by_age.png"),
  vwidth = 1200,  # Increased width for better readability
  vheight = 800   # Adjusted height
)