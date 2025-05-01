# Helper Functions
# This file contains utility functions used across the analysis

# Function to create a summary table of categorical variables
create_categorical_summary <- function(data, group_var, ...) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      across(c(...), 
             list(
               n = ~sum(. == "Yes", na.rm = TRUE),
               pct = ~mean(. == "Yes", na.rm = TRUE) * 100
             )),
      .groups = "drop"
    )
}

# Function to create a summary table of continuous variables
create_continuous_summary <- function(data, group_var, ...) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      across(c(...),
             list(
               mean = ~mean(., na.rm = TRUE),
               sd = ~sd(., na.rm = TRUE),
               median = ~median(., na.rm = TRUE),
               min = ~min(., na.rm = TRUE),
               max = ~max(., na.rm = TRUE)
             )),
      .groups = "drop"
    )
}

# Function to save a table to CSV with proper formatting
save_table <- function(data, filename) {
  write_csv(data, filename, na = "")
}

# Function to create a standardized plot theme
theme_custom <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      strip.text = element_text(size = 12)
    )
}

# Function to calculate and format p-values
format_pvalue <- function(p) {
  if (p < 0.001) {
    return("< 0.001")
  } else {
    return(sprintf("%.3f", p))
  }
}

# Function to create a correlation matrix with p-values
create_correlation_matrix <- function(data, method = "pearson") {
  # Calculate correlation matrix
  cor_matrix <- cor(data, use = "pairwise.complete.obs", method = method)
  
  # Calculate p-values
  p_matrix <- matrix(NA, nrow = ncol(data), ncol = ncol(data))
  for (i in 1:ncol(data)) {
    for (j in 1:ncol(data)) {
      if (i != j) {
        test <- cor.test(data[[i]], data[[j]], method = method)
        p_matrix[i, j] <- test$p.value
      }
    }
  }
  
  # Format results
  result <- matrix(NA, nrow = ncol(data), ncol = ncol(data))
  for (i in 1:ncol(data)) {
    for (j in 1:ncol(data)) {
      if (i != j) {
        result[i, j] <- sprintf("%.2f\n(p=%s)", 
                               cor_matrix[i, j],
                               format_pvalue(p_matrix[i, j]))
      } else {
        result[i, j] <- "1.00"
      }
    }
  }
  
  rownames(result) <- colnames(result) <- names(data)
  return(result)
} 