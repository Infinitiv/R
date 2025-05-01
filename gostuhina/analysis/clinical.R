# Clinical Symptoms Analysis
# This script analyzes the clinical symptoms in celiac disease patients

# Load required libraries
library(tidyverse)
library(ggplot2)
library(gt)
library(webshot2)

# Define paths
DATA_DIR <- "~/Yandex.Disk/data/gostuhina"
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
RESULTS_DIR <- file.path(DATA_DIR, "results")
TABLES_DIR <- file.path(RESULTS_DIR, "tables")
FIGURES_DIR <- file.path(RESULTS_DIR, "figures")

# Read processed data
data_clinical <- read_csv(file.path(PROCESSED_DIR, "data_clinical.csv"))

data_clinical <- data_clinical %>%
  mutate(age_group = factor(age_group, 
                          levels = c("1-2 года", "3-7 лет", "8-11 лет", "12-17 лет"),
                          ordered = TRUE)) %>%
  mutate(across(c(`Впервые выставленная целиакия`, `Боль в животе`, `Вздутие живота`, Тошнота,
                  Рвота, `Другие проявления рефлюкса`, Диарея, Полифекалия,
                  Запор, `Неустойчивый стул`, `Недостаточность питания`,
                  `Задержка роста`, Ожирение, Стоматит,
                  `Кариес, темный налет на зубах`, `Атопический дерматит`,
                  Псориаз, `Герпетиформный дерматит`, `Другие дерматиты`, `Слабость, снижение аппетита`, Алопеция, `Частые ОРВИ`,
                  `Боли в суставах`),
                ~factor(., levels = c("No", "Yes")))) %>%
  mutate(Пол = factor(Пол, levels = c("Male", "Female"))) %>%
  mutate(group = factor(group))

first_celiac <- data_clinical %>%
  filter(`Впервые выставленная целиакия` == 'Yes')

# Create age distribution plot
age_distribution <- first_celiac %>%
  count(Возраст) %>%
  rename(age = Возраст, count = n) %>%
  mutate(percentage = (count / sum(count) * 100)) %>%
  mutate(percentage_label = sprintf("%.1f%%", percentage))

# Create age distribution plot
age_plot <- ggplot(age_distribution, aes(x = age, y = percentage)) +
  geom_bar(stat = "identity", fill = "grey70") +
  geom_text(aes(label = percentage_label), vjust = -0.5, size = 3.5) +
  scale_x_continuous(breaks = seq(0, max(age_distribution$age), by = 1)) +
  scale_y_continuous(limits = c(0, max(age_distribution$percentage) + 2), 
                    breaks = seq(0, ceiling(max(age_distribution$percentage)), by = 5)) +
  labs(
    title = "Распределение пациентов с впервые выявленной целиакией по возрасту",
    x = "Возраст, лет",
    y = "Доля детей, %"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 11),
    axis.title = element_text(size = 10)
  )

# Save the plot as PNG
ggsave(
  filename = file.path(FIGURES_DIR, "age_distribution.png"),
  plot = age_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Calculate frequency by age group
age_group_frequency <- first_celiac %>%
  group_by(age_group) %>%
  summarise(
    total = n(),
    frequency = round(total / length(first_celiac$group) * 100, 1)
  ) %>%
  mutate(
    frequency_label = sprintf("%.1f%%", frequency)
  )

# Create publication-ready frequency table
freq_table <- age_group_frequency %>%
  select(age_group, total, frequency_label) %>%
  rename(
    "Возрастная группа" = age_group,
    "Всего детей" = total,
    "Частота, %" = frequency_label
  ) %>%
  gt() %>%
  tab_header(
    title = "Распределение целиакии по возрастным группам",
    subtitle = "Частота встречаемости впервые выявленной целиакии"
  ) %>%
  fmt_number(
    columns = "Всего детей",
    decimals = 0,
    use_seps = TRUE
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

# Save table as HTML and PNG
gtsave(freq_table, 
       filename = file.path(TABLES_DIR, "celiac_frequency_by_age.html"))

freq_table %>%
  gtsave(
    filename = file.path(TABLES_DIR, "celiac_frequency_by_age.png")
  )

# List of symptoms to test
symptoms <- c("Боль в животе", "Вздутие живота", "Тошнота",
             "Рвота", "Другие проявления рефлюкса", "Диарея", "Полифекалия",
             "Запор", "Неустойчивый стул", "Недостаточность питания",
             "Задержка роста", "Ожирение", "Стоматит",
             "Кариес, темный налет на зубах", "Атопический дерматит",
             "Псориаз", "Герпетиформный дерматит", "Другие дерматиты", 
             "Слабость, снижение аппетита", "Алопеция", "Частые ОРВИ",
             "Боли в суставах")

# Function to calculate frequencies and perform Fisher test for each symptom
calculate_frequencies <- function(data, symptom) {
  freq <- data %>%
    group_by(age_group, group) %>%
    summarise(
      Symptom = symptom,
      n = n(),
      cases = sum(!!sym(symptom) == "Yes"),
      percentage = sprintf("%.1f%%", (cases/n) * 100),
      .groups = "drop"
    )
}

perform_fisher_test <- function(data, symptom) {
  p_values <- data %>%
    group_by(age_group) %>%
    summarise(
      p_value = fisher.test(!!sym(symptom), group)$p.value,
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = age_group,
      values_from = p_value,
      names_prefix = "group_"
    )

  # Return results
  data.frame(
    Симптом = symptom,
    p_values
  )
}

# Calculate frequencies for all symptoms
frequencies <- map_dfr(symptoms, ~calculate_frequencies(data_clinical, .x))

# Perform Fisher test for each symptom
fisher_results <- map_dfr(symptoms, ~perform_fisher_test(data_clinical, .x))

# Filter significant results and create formatted table
significant_symptoms <- fisher_results %>%
  filter(
    if_any(
      c(group_1.2.года, group_3.7.лет, group_8.11.лет,
        group_12.17.лет), ~(.) < 0.05
    )
  ) %>%
  mutate(
    across(starts_with("group_"),
           ~ifelse(. < 0.05,
                   sprintf("%.7f", .),
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

significant_symptoms_list <- significant_symptoms$`_data`$Симптом

significant_frequencies <- frequencies %>%
  filter(Symptom %in% significant_symptoms_list) %>%
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
gtsave(significant_symptoms, 
       filename = file.path(TABLES_DIR, "significant_symptoms_by_age.html"))

significant_symptoms %>%
  gtsave(
    filename = file.path(TABLES_DIR, "significant_symptoms_by_age.png")
  )

# Save results for frequencies table
gtsave(significant_frequencies,
       filename = file.path(TABLES_DIR, "significant_frequencies.html"))

# Save as PNG with specific dimensions for better readability
gtsave(significant_frequencies,
       filename = file.path(TABLES_DIR, "significant_frequencies.png"),
       vwidth = 1200,  # Increased width for better readability
       vheight = 800   # Adjusted height
)