library(tidyverse)
library(ggplot2)

data_dir <- "~/Yandex.Disk/data/gostuhina"
processed_dir <- file.path(data_dir, "processed")
results_dir <- file.path(data_dir, "results")
tables_dir <- file.path(results_dir, "tables")
figures_dir <- file.path(results_dir, "figures")
analysis_dir <- "~/R/gostuhina/analysis"

# Source functions
source(file.path(analysis_dir, "functions.R"))

# Read processed data
data <- read_csv(file.path(processed_dir, "data_start_diagnosis.csv"))

# Преобразование данных в длинный формат
data_long <- data %>%
  pivot_longer(cols = everything(), names_to = "Показатель", values_to = "Значение") %>%
  filter(!is.na("Значение"))

# Построение boxplot
boxplot_start_diagnosis <- ggplot(data_long, aes(x = Показатель, y = Значение)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", outlier.colour = "red", outlier.shape = 16) +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 1)), 
               vjust = -0.7, color = "black", size = 3) +
  labs(
    title = "Срок постановки диагноза целиакия \nв зависимости от первоначального диагноза, мес.",
    x = "Диагноз",
    y = "Месяцы"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("~/Yandex.Disk/data/gostuhina/results/figures/boxplot_start_diagnosis.png", 
       plot = boxplot_start_diagnosis, width = 10, height = 6, dpi = 300)
