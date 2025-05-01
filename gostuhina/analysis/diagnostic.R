# Diagnostic Markers Analysis
# This script analyzes the diagnostic markers for celiac disease

# Load required libraries
library(tidyverse)
library(pROC)

# Define paths
DATA_DIR <- "~/Yandex.Disk/data/gostuhina"
PROCESSED_DIR <- file.path(DATA_DIR, "processed")
RESULTS_DIR <- file.path(DATA_DIR, "results")

# Read processed data
data_diagnostic <- read_csv(file.path(PROCESSED_DIR, "data_diagnostic.csv"))

# Basic summary statistics
cat("\nSummary of Diagnostic Markers:\n")
summary(data_diagnostic)

# Calculate ROC curves for IgA and IgG
roc_iga <- roc(data_diagnostic$group, data_diagnostic$`IgA ТТг <10`)
roc_igg <- roc(data_diagnostic$group, data_diagnostic$`IgG ТТг <10`)

# Plot ROC curves
pdf(file.path(RESULTS_DIR, "figures/roc_curves.pdf"))
plot(roc_iga, main = "ROC Curves for Diagnostic Markers")
plot(roc_igg, add = TRUE, col = "red")
legend("bottomright", 
       legend = c("IgA ТТг", "IgG ТТг"),
       col = c("black", "red"),
       lwd = 2)
dev.off()

# Calculate and save AUC values
auc_values <- data.frame(
  Marker = c("IgA ТТг", "IgG ТТг"),
  AUC = c(auc(roc_iga), auc(roc_igg))
)

write_csv(auc_values, file.path(RESULTS_DIR, "tables/auc_values.csv"))

# Analyze Marsh classification distribution
marsh_dist <- data_diagnostic %>%
  group_by(group, `Степень атрофии по Marsh`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(group) %>%
  mutate(percentage = n / sum(n) * 100)

write_csv(marsh_dist, file.path(RESULTS_DIR, "tables/marsh_distribution.csv"))

# Create visualization of Marsh classification
ggplot(marsh_dist, aes(x = `Степень атрофии по Marsh`, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution of Marsh Classification by Group",
       x = "Marsh Classification",
       y = "Percentage",
       fill = "Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(RESULTS_DIR, "figures/marsh_distribution.pdf"), width = 10, height = 6) 