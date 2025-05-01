# Laboratory Findings Analysis
# This script analyzes the laboratory findings in celiac disease patients

# Load required libraries
library(tidyverse)

# Define paths
DATA_DIR <- "~/Yandex.Disk/data/gostuhina"
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
RESULTS_DIR <- file.path(DATA_DIR, "results")

# Read processed data
data_lab <- read_csv(file.path(PROCESSED_DIR, "data_lab.csv"))

# Basic summary statistics
cat("\nSummary of Laboratory Findings:\n")
summary(data_lab)

# Calculate frequencies of abnormal findings by group
lab_freq <- data_lab %>%
  select(-`Номер пациента`) %>%
  group_by(group) %>%
  summarise(across(everything(), ~mean(. == "Abnormal") * 100)) %>%
  pivot_longer(-group, names_to = "Test", values_to = "Percentage")

# Save laboratory frequencies
write_csv(lab_freq, file.path(RESULTS_DIR, "tables/lab_frequencies.csv"))

# Create visualization of laboratory findings frequencies
ggplot(lab_freq, aes(x = Test, y = Percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Frequency of Abnormal Laboratory Findings by Group",
       x = "Laboratory Test",
       y = "Percentage",
       fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(RESULTS_DIR, "figures/lab_frequencies.pdf"), width = 12, height = 6)

# Analyze co-occurrence of laboratory findings
lab_cooccurrence <- data_lab %>%
  select(-`Номер пациента`, -group) %>%
  mutate(across(everything(), ~. == "Abnormal")) %>%
  cor(use = "pairwise.complete.obs")

# Save co-occurrence matrix
write.csv(lab_cooccurrence, file.path(RESULTS_DIR, "tables/lab_cooccurrence.csv"))

# Create heatmap of co-occurrence
library(pheatmap)
pheatmap(lab_cooccurrence,
         main = "Co-occurrence of Laboratory Findings",
         filename = file.path(RESULTS_DIR, "figures/lab_cooccurrence.pdf"),
         width = 10,
         height = 8)

# Analyze patterns of laboratory findings
lab_patterns <- data_lab %>%
  select(-`Номер пациента`) %>%
  group_by(group) %>%
  summarise(
    across(everything(), ~mean(. == "Abnormal") * 100),
    .groups = "drop"
  )

# Save patterns analysis
write_csv(lab_patterns, file.path(RESULTS_DIR, "tables/lab_patterns.csv")) 