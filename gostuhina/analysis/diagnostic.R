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
data_diagnostic <- read_csv(file.path(processed_dir, "data_diagnostic.csv"))

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
                                               levels = c("норма", "0", "1", "2", "3A", "3B", "3C", "другое"))
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
       filename = file.path(tables_dir, "conf_matrix_table.html"))

tests <- c(
  "IgA ТТг <10",
  "IgG ТТг <10"
)

tests_by_age <- map_dfr(tests, ~descriptive_by_age(data_diagnostic, .x))

tests_by_age_table <- tests_by_age %>%
  gt() %>%
  tab_header(
    title = "Возрастные особенности концентрации антител к тканевой трансглутаминазе в дебюте целиакии"
  ) %>%
  cols_label(
    Test = "Тест",
    age_group = "Возраст",
    n = "Количество",
    median_quantilies = "Медиана и квартили",
    mean_se = "Среднее и ошибка",
  )

gtsave(tests_by_age_table, 
       filename = file.path(tables_dir, "tests_by_age_table.html"))


data_diagnostic %>%
  filter(`Впервые выставленная целиакия` == "Yes") %>%
  { cor.test(
      as.numeric(.$`IgA ТТг <10`), 
      as.numeric(.$`IgG ТТг <10`), 
      method = "spearman", 
      use = "complete.obs"
    ) }

# Create the plot and assign to a variable
plot_celiac <- data_diagnostic %>%
  filter(`Впервые выставленная целиакия` == "Yes") %>%
  ggplot(aes(
    x = as.numeric(`IgG ТТг <10`),
    y = as.numeric(`IgA ТТг <10`)
  )) +
  geom_point(shape = 1, color = "blue", size = 2, stroke = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.7) +
  labs(
    x = "IgG к tTG, Ед/мл",
    y = "IgA к tTG, Ед/мл",
    title = "Корреляция между IgA и IgG к tTG\n(только впервые выставленная целиакия)"
  ) +
  theme_minimal(base_size = 14)

# Save the plot as PNG
ggsave(
  filename = file.path(figures_dir, "IgA_IgG_scatter.png"),
  plot = plot_celiac,
  width = 8,
  height = 6,
  dpi = 300
)

data_first_time_celiac <- data_diagnostic %>%
  filter(`Впервые выставленная целиакия` == "Yes") %>%
  mutate(
    prognoz_marsh = ifelse(grepl("3", `Степень атрофии по Marsh`), 1, 0)
  )

fit_marsh <- glm(prognoz_marsh ~ `IgA ТТг <10`, data = data_first_time_celiac, family = "binomial")
roc_obj <- roc(data_first_time_celiac$prognoz_marsh, predict(fit_marsh, type = "response"))
plot(roc_obj)
auc(roc_obj)
coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
predict(fit_marsh, type = "response")
