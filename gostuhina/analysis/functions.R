# Clinical Functions
# This file contains functions for clinical symptoms analysis

# Load required libraries
library(tidyverse)

# Function to calculate frequencies
calculate_frequencies <- function(data, symptom) {
  data %>%
    group_by(group) %>%
    summarise(
      Symptom = symptom,
      n = n(),
      cases = sum(!!sym(symptom) %in% c("Yes", "Abnormal")),
      percentage = sprintf("%.1f%%", (cases/n) * 100),
      .groups = "drop"
    )
}

# Function to calculate frequencies by age group
calculate_frequencies_by_age <- function(data, symptom) {
  fdata %>%
    group_by(age_group, group) %>%
    summarise(
      Symptom = symptom,
      n = n(),
      cases = sum(!!sym(symptom) %in% c("Yes", "Abnormal")),
      percentage = sprintf("%.1f%%", (cases/n) * 100),
      .groups = "drop"
    )
}

# Function to perform Fisher test with error handling
perform_fisher_test <- function(data, symptom) {
  p_values <- data %>%
    summarise(
      p_value = tryCatch(
        fisher.test(!!sym(symptom) %in% c("Yes", "Abnormal"), group)$p.value,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    )

  # Return results
  data.frame(
    Симптом = symptom,
    p_values
  )
} 

# Function to perform Fisher test by age group with error handling
perform_fisher_test_by_age <- function(data, symptom) {
  p_values <- data %>%
    group_by(age_group) %>%
    summarise(
      p_value = tryCatch(
        fisher.test(!!sym(symptom) %in% c("Yes", "Abnormal"), group)$p.value,
        error = function(e) NA_real_
      ),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = age_group,
      values_from = p_values,
      names_prefix = "group_"
    )

  # Return results
  data.frame(
    Симптом = symptom,
    p_values
  )
}
