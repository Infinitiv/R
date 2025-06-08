# Project Structure:
# ~/R/gostuhina/              # Scripts and analysis code
#   ├── main.R                # Main data preparation script
#   ├── analysis/             # Analysis scripts
#   │   ├── diagnostic.R      # Diagnostic markers analysis
#   │   ├── clinical.R        # Clinical symptoms analysis
#   │   └── laboratory.R      # Laboratory findings analysis
#   └── utils/                # Utility functions
#       └── helpers.R         # Helper functions
#
# ~/Yandex.Disk/data/gostuhina/  # Data storage
#   ├── raw/                  # Raw data files
#   │   └── gostuhina.csv     # Original dataset
#   ├── processed/            # Processed data files
#   │   ├── data_clean.csv    # Cleaned dataset
#   │   ├── data_diagnostic.csv
#   │   ├── data_clinical.csv
#   │   └── data_lab.csv
#   └── results/              # Analysis results
#       ├── figures/          # Generated plots
#       └── tables/           # Generated tables

# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(pROC)       # For ROC curve analysis and plotting

# Define paths
DATA_DIR <- "~/Yandex.Disk/data/gostuhina"
RAW_DATA_PATH <- file.path(DATA_DIR, "raw/gostuhina.csv")
PROCESSED_DIR <- file.path(DATA_DIR, "processed")

# Create necessary directories if they don't exist
dir.create(file.path(DATA_DIR, "raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DATA_DIR, "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DATA_DIR, "results/figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DATA_DIR, "results/tables"), showWarnings = FALSE, recursive = TRUE)

# Read the data
data <- read_csv(RAW_DATA_PATH)

# Data preparation
data_clean <- data %>%
  # Convert numeric columns with comma decimal separator to proper numeric
  mutate(across(c(`IgA ТТг <10`, `IgG ТТг <10`, 
                  `IgA к глиадину <12.5`, `IgG к глиадину <12.5`,
                  `IgA к ДПГ <10`, `IgG к ДПГ <10`),
                ~as.numeric(gsub(",", ".", .)))) %>%
  # Add age groups
  mutate(age_group = case_when(
    Возраст >= 1 & Возраст <= 2 ~ "1-2 года",
    Возраст >= 3 & Возраст <= 7 ~ "3-7 лет",
    Возраст >= 8 & Возраст <= 11 ~ "8-11 лет",
    Возраст >= 12 & Возраст <= 17 ~ "12-17 лет"
  )) %>%
  mutate(age_group = factor(age_group, 
                           levels = c("1-2 года", "3-7 лет", "8-11 лет", "12-17 лет"),
                           ordered = TRUE)) %>%
  # Convert binary columns to factor
  mutate(across(c(`Впервые выставленная целиакия`, `Кислотозависимые заболевания ЖКТ`,
                  ГФПА, ФНЖКТ, `Герпесвирусные инфекции`, Клостридиоз, ОРВИ,
                  `Поллиноз, БА, АтД`, ВЗК, `Другие аутоиммунные заболевания ЖКТ`,
                  `Аутоиммунный тиреоидит`, СД, `Недифференцированный колит`,
                  `Дефицит IgA`, Анемия, Аутизм, `Хромосомные аномалии`,
                  Эпилепсия, `Синдром Жильбера`, `Боль в животе`, `Вздутие живота`, Тошнота,
                  Рвота, `Другие проявления рефлюкса`, Диарея, Полифекалия,
                  Запор, `Неустойчивый стул`, `Недостаточность питания`,
                  `Задержка роста`, Ожирение, Стоматит,
                  `Кариес, темный налет на зубах`, `Атопический дерматит`,
                  Псориаз, `Другие дерматиты`, Алопеция, `Частые ОРВИ`,
                  `Боли в суставах`, `Семейный анамнез`),
                ~factor(., levels = c("No", "Yes")))) %>%
  # Convert laboratory test results to numeric and handle missing values
  mutate(across(c(`повышение АЛТ`, `повышение АСТ`, `снижение ферритина`,
                  `дефицит витамина В12`, `дефицит фолиевой кислоты`,
                  `дефицит витамина D`, `колебания ТТГ`, `колебания Т4`,
                  `повышение анти-ТПО`),
                ~factor(., levels = c("Normal", "Abnormal")))) %>%
  # Convert gender to factor
  mutate(Пол = factor(Пол, levels = c("Male", "Female"))) %>%
  # Convert group to factor
  mutate(group = factor(group)) %>%
  # Convert Marsh classification to ordered factor
  mutate(`Степень атрофии по Marsh` = factor(`Степень атрофии по Marsh`,
                                            levels = c("норма", "0", "1", "2", "3A", "3B", "3C", "другое")))

# Create analysis-ready datasets
# 1. Diagnostic markers (complete cases only)
data_diagnostic <- data_clean %>%
  filter(`Дефицит IgA` == "No") %>%
  select(`Номер пациента`, group, Пол, Возраст, age_group,
         `IgA ТТг <10`, `IgG ТТг <10`, 
         `IgA к глиадину <12.5`, `IgG к глиадину <12.5`,
         `IgA к ДПГ <10`, `IgG к ДПГ <10`,
         `Степень атрофии по Marsh`, `Впервые выставленная целиакия`)

data_diagnostic_iga <- data_clean %>%
  filter(`Дефицит IgA` == "Yes") %>%
  select(`Номер пациента`, group, Пол, Возраст, age_group,
         `IgA ТТг <10`, `IgG ТТг <10`, 
         `IgA к глиадину <12.5`, `IgG к глиадину <12.5`,
         `IgA к ДПГ <10`, `IgG к ДПГ <10`,
         `Степень атрофии по Marsh`, `Впервые выставленная целиакия`)

# 2. Clinical symptoms (replace NA with "No" as absence of symptom)
data_clinical <- data_clean %>%
  select(`Номер пациента`, group, Пол, Возраст, age_group,
         `Впервые выставленная целиакия`,
         `Боль в животе`, `Вздутие живота`, Тошнота, Рвота,
         `Другие проявления рефлюкса`, Диарея, Полифекалия, Запор,
         `Неустойчивый стул`, `Недостаточность питания`, `Задержка роста`,
         Ожирение, Стоматит, `Кариес, темный налет на зубах`,
         `Атопический дерматит`, Псориаз, `Герпетиформный дерматит`,
         `Другие дерматиты`, `Слабость, снижение аппетита`, Алопеция,
         `Частые ОРВИ`, `Боли в суставах`) %>%
  mutate(across(c(`Впервые выставленная целиакия`, `Боль в животе`, `Вздутие живота`,
                  Тошнота, Рвота,
                  `Другие проявления рефлюкса`, Диарея, Полифекалия, Запор,
                  `Неустойчивый стул`, `Недостаточность питания`, `Задержка роста`,
                  Ожирение, Стоматит, `Кариес, темный налет на зубах`,
                  `Атопический дерматит`, Псориаз, `Герпетиформный дерматит`,
                  `Другие дерматиты`, `Слабость, снижение аппетита`, Алопеция,
                  `Частые ОРВИ`, `Боли в суставах`),
                ~replace_na(., "No")))

# 3. Laboratory findings (replace NA with 0 as absence of finding)
data_lab <- data_clean %>%
  select(`Номер пациента`, group, Пол, Возраст, age_group,
         `повышение АЛТ`, `повышение АСТ`, `снижение ферритина`,
         `дефицит витамина В12`, `дефицит фолиевой кислоты`,
         `дефицит витамина D`, `колебания ТТГ`, `колебания Т4`,
         `повышение анти-ТПО`) %>%
  filter(if_any(c(`повышение АЛТ`, `повышение АСТ`, `снижение ферритина`,
                  `дефицит витамина В12`, `дефицит фолиевой кислоты`,
                  `дефицит витамина D`, `колебания ТТГ`, `колебания Т4`,
                  `повышение анти-ТПО`), ~!is.na(.)))

# Save prepared datasets
write_csv(data_clean, file.path(PROCESSED_DIR, "data_clean.csv"))
write_csv(data_diagnostic, file.path(PROCESSED_DIR, "data_diagnostic.csv"))
write_csv(data_diagnostic_iga, file.path(PROCESSED_DIR, "data_diagnostic_iga.csv"))
write_csv(data_clinical, file.path(PROCESSED_DIR, "data_clinical.csv"))
write_csv(data_lab, file.path(PROCESSED_DIR, "data_lab.csv"))