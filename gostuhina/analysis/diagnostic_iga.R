# Diagnostic Markers Analysis
# This script analyzes the diagnostic markers for celiac disease

# Load required libraries
library(tidyverse)
library(pROC)
library(caret)
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
data_diagnostic <- read_csv(file.path(processed_dir, "data_diagnostic_iga.csv"))

data_diagnostic <- data_diagnostic %>%
  mutate(
    prognoz = ifelse(group == "celiac", 1, 0),
    `tTG-IgA` = ifelse(`IgA ТТг <10` < 10, 0, 1),
    `tTG-IgG` = ifelse(`IgG ТТг <10`< 10, 0, 1),
    `AGA-IgA` = ifelse(`IgA к глиадину <12.5` < 12.5, 0, 1),
    `AGA-IgG` = ifelse(`IgG к глиадину <12.5` < 12.5, 0, 1),
    `DPG-IgA` = ifelse(`IgA к ДПГ <10` < 10, 0, 1),
    `DPG-IgG` = ifelse(`IgG к ДПГ <10`< 10, 0, 1),
    `Впервые выставленная целиакия` = factor(`Впервые выставленная целиакия`, levels = c("No", "Yes")),
    `Степень атрофии по Marsh` = factor(`Степень атрофии по Marsh`,
                                               levels = c("0", "1", "2", "3A", "3B", "3C")),
    age_group = factor(age_group, levels = c("1-2 года", "3-7 лет", "8-11 лет", "12-17 лет"),
                       ordered = TRUE)
  )

tests <- c(
  "tTG-IgA",
  "tTG-IgG",
  "AGA-IgA",
  "AGA-IgG",
  "DPG-IgA",
  "DPG-IgG"
)
conf_matrix_by_tests <- map_dfr(tests, ~conf_matrix(data_diagnostic, .x))

conf_matrix_table <- conf_matrix_by_tests %>%
  gt() %>%
  tab_header(
    title = "Диагностические показатели тестов"
  ) %>%
  fmt_number(
    columns = c("Sensitivity", "Specificity", 
                "posPredValue", 
                "negPredValue"),
    decimals = 3
  ) %>%
  cols_label(
    Test = "Тест",
    Sensitivity = "Чувствительность",
    Specificity = "Специфичность",
    posPredValue = "Положительная прогностическая ценность",
    negPredValue = "Отрицательная прогностическая ценность",
  )

gtsave(conf_matrix_table, 
       filename = file.path(tables_dir, "conf_matrix_table_iga.html"))

data_first_time_celiac <- data_diagnostic %>%
  filter(`Впервые выставленная целиакия` == "Yes", !is.na(`Степень атрофии по Marsh`)) %>%
  mutate(
    prognoz_marsh = ifelse(grepl("3", `Степень атрофии по Marsh`), 1, 0)
  )

data_diagnostic_marsh <- data_first_time_celiac %>%
  filter(`Степень атрофии по Marsh` != '0')

# Kruskal-Wallis test
kruskal_result <- kruskal.test(`IgG ТТг <10` ~ `Степень атрофии по Marsh`, data = data_diagnostic_marsh)
kruskal_table <- tibble(
  "Тест" = "Краскела-Уоллиса",
  "p-значение" = format.pval(kruskal_result$p.value, digits = 4)
) %>%
  gt() %>%
  tab_header(
    title = "Результат теста Краскела-Уоллиса"
  )
gtsave(kruskal_table, 
       filename = file.path(tables_dir, "kruskal_table_tTG-IgG.html"))

# Pairwise Wilcoxon comparisons with Bonferroni adjustment
pairwise_result <- pairwise.wilcox.test(data_diagnostic_marsh$`IgG ТТг <10`, data_diagnostic_marsh$`Степень атрофии по Marsh`,
                                        p.adjust.method = "bonferroni")
pairwise_table <- as.data.frame(pairwise_result$p.value) %>%
  rownames_to_column(var = "Группа 1") %>%
  pivot_longer(-`Группа 1`, names_to = "Группа 2", values_to = "p-значение") %>%
  mutate(`p-значение` = ifelse(is.na(`p-значение`), "-", formatC(`p-значение`, format = "f", digits = 4))) %>%
  gt() %>%
  tab_header(
    title = "Попарные сравнения (тест Вилкоксона с поправкой Бонферрони)"
  )
gtsave(pairwise_table, 
       filename = file.path(tables_dir, "pairwise_table_tTG-IgG.html"))

# Kruskal-Wallis test
kruskal_result <- kruskal.test(`IgG к глиадину <12.5` ~ `Степень атрофии по Marsh`, data = data_diagnostic_marsh)
kruskal_table <- tibble(
  "Тест" = "Краскела-Уоллиса",
  "p-значение" = format.pval(kruskal_result$p.value, digits = 4)
) %>%
  gt() %>%
  tab_header(
    title = "Результат теста Краскела-Уоллиса"
  )
gtsave(kruskal_table, 
       filename = file.path(tables_dir, "kruskal_table_AGA-IgG.html"))

# Pairwise Wilcoxon comparisons with Bonferroni adjustment
pairwise_result <- pairwise.wilcox.test(data_diagnostic_marsh$`IgG к глиадину <12.5`, data_diagnostic_marsh$`Степень атрофии по Marsh`,
                                        p.adjust.method = "bonferroni")
pairwise_table <- as.data.frame(pairwise_result$p.value) %>%
  rownames_to_column(var = "Группа 1") %>%
  pivot_longer(-`Группа 1`, names_to = "Группа 2", values_to = "p-значение") %>%
  mutate(`p-значение` = ifelse(is.na(`p-значение`), "-", formatC(`p-значение`, format = "f", digits = 4))) %>%
  gt() %>%
  tab_header(
    title = "Попарные сравнения (тест Вилкоксона с поправкой Бонферрони)"
  )
gtsave(pairwise_table, 
       filename = file.path(tables_dir, "pairwise_table_AGG-IgG.html"))

# Kruskal-Wallis test
kruskal_result <- kruskal.test(`IgG к ДПГ <10` ~ `Степень атрофии по Marsh`, data = data_diagnostic_marsh)
kruskal_table <- tibble(
  "Тест" = "Краскела-Уоллиса",
  "p-значение" = format.pval(kruskal_result$p.value, digits = 4)
) %>%
  gt() %>%
  tab_header(
    title = "Результат теста Краскела-Уоллиса"
  )
gtsave(kruskal_table, 
       filename = file.path(tables_dir, "kruskal_table_DPG-IgG.html"))

# Pairwise Wilcoxon comparisons with Bonferroni adjustment
pairwise_result <- pairwise.wilcox.test(data_diagnostic_marsh$`IgG к ДПГ <10`, data_diagnostic_marsh$`Степень атрофии по Marsh`,
                                        p.adjust.method = "bonferroni")
pairwise_table <- as.data.frame(pairwise_result$p.value) %>%
  rownames_to_column(var = "Группа 1") %>%
  pivot_longer(-`Группа 1`, names_to = "Группа 2", values_to = "p-значение") %>%
  mutate(`p-значение` = ifelse(is.na(`p-значение`), "-", formatC(`p-значение`, format = "f", digits = 4))) %>%
  gt() %>%
  tab_header(
    title = "Попарные сравнения (тест Вилкоксона с поправкой Бонферрони)"
  )
gtsave(pairwise_table, 
       filename = file.path(tables_dir, "pairwise_table_DPG-IgG.html"))

medians <- data_diagnostic_marsh %>%
  group_by(`Степень атрофии по Marsh`) %>%
  summarise(median_value = median(`IgG ТТг <10`, na.rm = TRUE))

plot_box_marsh <- ggplot(data_diagnostic_marsh, aes(x = `Степень атрофии по Marsh`, y = `IgG ТТг <10`)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.colour = "red", outlier.shape = 16) +
  geom_text(data = medians, aes(label = round(median_value, 1), y = median_value), 
            vjust = -0.5, color = "black", size = 4) +
  labs(
    title = "IgG к tTG по степени атрофии по Marsh",
    subtitle = "Только впервые выставленная целиакия",
    x = "Степень атрофии по Marsh",
    y = "IgG к tTG, Ед/мл"
  ) +
  theme_minimal(base_size = 14)

ggsave("~/Yandex.Disk/data/gostuhina/results/figures/boxplot_marsh_tTG-IgG_2-3.png", 
       plot = plot_box_marsh, width = 10, height = 6, dpi = 300)

medians <- data_diagnostic_marsh %>%
  group_by(`Степень атрофии по Marsh`) %>%
  summarise(median_value = median(`IgG к глиадину <12.5`, na.rm = TRUE))

plot_box_marsh <- ggplot(data_diagnostic_marsh, aes(x = `Степень атрофии по Marsh`, y = `IgG к глиадину <12.5`)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.colour = "red", outlier.shape = 16) +
  geom_text(data = medians, aes(label = round(median_value, 1), y = median_value), 
            vjust = -0.5, color = "black", size = 4) +
  labs(
    title = "IgG к глиадину по степени атрофии по Marsh",
    subtitle = "Только впервые выставленная целиакия",
    x = "Степень атрофии по Marsh",
    y = "IgG к глиадину, Ед/мл"
  ) +
  theme_minimal(base_size = 14)

ggsave("~/Yandex.Disk/data/gostuhina/results/figures/boxplot_marsh_AGG-IgG_2-3.png", 
       plot = plot_box_marsh, width = 10, height = 6, dpi = 300)

medians <- data_diagnostic_marsh %>%
  group_by(`Степень атрофии по Marsh`) %>%
  summarise(median_value = median(`IgG к ДПГ <10`, na.rm = TRUE))

plot_box_marsh <- ggplot(data_diagnostic_marsh, aes(x = `Степень атрофии по Marsh`, y = `IgG к ДПГ <10`)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.colour = "red", outlier.shape = 16) +
  geom_text(data = medians, aes(label = round(median_value, 1), y = median_value), 
            vjust = -0.5, color = "black", size = 4) +
  labs(
    title = "IgG к ДПГ по степени атрофии по Marsh",
    subtitle = "Только впервые выставленная целиакия",
    x = "Степень атрофии по Marsh",
    y = "IgG к ДПГ, Ед/мл"
  ) +
  theme_minimal(base_size = 14)

ggsave("~/Yandex.Disk/data/gostuhina/results/figures/boxplot_marsh_DPG-IgG_2-3.png", 
       plot = plot_box_marsh, width = 10, height = 6, dpi = 300)