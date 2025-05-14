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
      count = sum(!!sym(symptom) %in% c("Yes", "Abnormal")) + sum(!!sym(symptom) %in% c("No", "Normal")),
      cases = sum(!!sym(symptom) %in% c("Yes", "Abnormal")),
      percentage = sprintf("%.1f%%", (cases/count) * 100),
      .groups = "drop"
    )
}

# Function to calculate frequencies by age group
calculate_frequencies_by_age <- function(data, symptom) {
  data %>%
    group_by(age_group, group) %>%
    summarise(
      Symptom = symptom,
      n = n(),
      count = sum(!!sym(symptom) %in% c("Yes", "Abnormal")) + sum(!!sym(symptom) %in% c("No", "Normal")),
      cases = sum(!!sym(symptom) %in% c("Yes", "Abnormal")),
      percentage = sprintf("%.1f%%", (cases/count) * 100),
      .groups = "drop"
    )
}

# Function to perform Fisher test with error handling
perform_fisher_test <- function(data, symptom) {
  p_values <- data %>%
    summarise(
      p_value = tryCatch(
        fisher.test(!!sym(symptom), group)$p.value,
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
        fisher.test(!!sym(symptom), group)$p.value,
        error = function(e) NA_real_
      ),
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

conf_matrix <- function(data, test) {
  matrix <- confusionMatrix(
    factor(data[[test]]), 
    factor(data$prognoz), 
    positive = "1"
  )
  data.frame(
    Test = test,
    Sensitivity = matrix$byClass["Sensitivity"],
    Specificity = matrix$byClass["Specificity"],
    posPredValue = matrix$byClass["Pos Pred Value"],
    negPredValue = matrix$byClass["Neg Pred Value"]
  )
}

descriptive_by_age <- function(data, test) {
  descriptive <- data %>%
  filter(`Впервые выставленная целиакия` == "Yes") %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    median = median(!!sym(test), na.rm = T),
    q1 = quantile(!!sym(test), 0.25, na.rm = T),
    q3 = quantile(!!sym(test), 0.75, na.rm = T),
    mean = mean(!!sym(test), na.rm = T),
    se = mean / sqrt(n),
    median_quantilies = sprintf("%.1f [%.1f-%.1f]", median, q1, q3),
    mean_se = sprintf("%.2f ± %.2f", mean, se)
  ) %>%
  select(age_group, n, median_quantilies, mean_se)
  
  data.frame(
    Test = test,
    descriptive
  )
}