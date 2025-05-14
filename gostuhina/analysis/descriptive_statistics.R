# Descriptive statistics
# This script analyzes the descriptive statistics of patients

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
ANALYSIS_DIR <- "~/R/gostuhina/analysis"

# Read processed data
data_clinical <- read_csv(file.path(PROCESSED_DIR, "data_clinical.csv"))

data_descriptive <- data_clinical %>%
    select(Возраст, age_group, Пол, group, `Впервые выставленная целиакия`) %>%
    mutate(age_group = factor(age_group, 
                            levels = c("1-2 года", "3-7 лет", "8-11 лет", "12-17 лет"),
                            ordered = TRUE)) %>%
    mutate(Пол = factor(Пол, levels = c("Male", "Female"))) %>%
    mutate(group = factor(group)) %>%
    mutate(age_group = factor(age_group, 
                          levels = c("1-2 года", "3-7 лет", "8-11 лет", "12-17 лет"),
                          ordered = TRUE)) %>%
    mutate("Впервые выставленная целиакия" = factor(`Впервые выставленная целиакия`,
                                                levels = c("Yes", "No")))

descriptive_by_age <- data_descriptive %>%
  group_by(group, age_group) %>%
  summarise(
    n = n(),
    median = median(Возраст),
    q1 = quantile(Возраст, 0.25),
    q3 = quantile(Возраст, 0.75),
    mean = mean(Возраст),
    se = mean / sqrt(n),
    median_quantilies = sprintf("%.1f [%.1f-%.1f]", median, q1, q3),
    mean_se = sprintf("%.2f ± %.2f", mean, se)
    ) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  mutate(percentage = sprintf("%.1f%%", percentage)) %>%
  select(group, age_group, n, percentage, median_quantilies, mean_se)

sprintf("%.6f", wilcox.test(Возраст ~ group, data = data_descriptive)$p.value)
# t.test(Возраст ~ group, data = data_descriptive)

fisher.test(data_descriptive$Пол, data_descriptive$group)

first_celiac <- data_descriptive %>%
  filter(`Впервые выставленная целиакия` == 'Yes')

gender_distribution <- first_celiac %>%
    count(Пол) %>%
    rename(count = n) %>%
    mutate(percentage = (count / sum(count) * 100)) %>%
    mutate(percentage_label = sprintf("%.1f%%", percentage))

gender_distribution_by_age <- first_celiac %>%
  group_by(age_group) %>%
  count(Пол) %>%
  rename(count = n) %>%
  mutate(percentage = (count / sum(count) * 100)) %>%
  mutate(percentage_label = sprintf("%.1f%%", percentage))

gender_distribution_by_group <- data_descriptive %>%
  group_by(group) %>%
  count(Пол) %>%
  rename(count = n) %>%
  mutate(percentage = (count / sum(count) * 100)) %>%
  mutate(percentage_label = sprintf("%.1f%%", percentage))

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

fisher.test(first_celiac$age_group, first_celiac$Пол)

