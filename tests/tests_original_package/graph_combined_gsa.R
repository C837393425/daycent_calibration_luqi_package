#!/usr/bin/env Rscript
# Script to graph and compare Likelihood_Combined_soboljansen.rds files between 04Aug2025 and 04Aug2025_O directories

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Set paths to the RDS files
path_04Aug2025_agg   <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025/GSA/soboljansen/Aggregated_Combined_soboljansen.rds"
path_04Aug2025_O_agg <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025_O/GSA/soboljansen/Aggregated_Combined_soboljansen.rds"
path_04Aug2025_likl   <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025/GSA/soboljansen/Likelihood_Combined_soboljansen.rds"
path_04Aug2025_O_likl <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025_O/GSA/soboljansen/Likelihood_Combined_soboljansen.rds"

# Output directory for plots
output_dir <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/tests_original_package"

# Read both RDS files
cat("=== Reading Aggregate Data ===\n")
aggregate_04Aug2025   <- readRDS(path_04Aug2025_agg)
aggregate_04Aug2025_O <- readRDS(path_04Aug2025_O_agg)
cat("=== Reading Likelihood Data ===\n\n")
likelihood_04Aug2025   <- readRDS(path_04Aug2025_likl)
likelihood_04Aug2025_O <- readRDS(path_04Aug2025_O_likl)

# Add source column to distinguish datasets
aggregate_04Aug2025$source <- "04Aug2025"
aggregate_04Aug2025_O$source <- "04Aug2025_O"

likelihood_04Aug2025$source <- "04Aug2025"
likelihood_04Aug2025_O$source <- "04Aug2025_O"

# Combine datasets for comparison
combined_data_agg  <- rbind(aggregate_04Aug2025, aggregate_04Aug2025_O)
combined_data_likl <- rbind(likelihood_04Aug2025, likelihood_04Aug2025_O)



  
  # 1. Density plot comparison
  p1 <- ggplot() +
    geom_density(data= aggregate_04Aug2025, aes(x = NH3, fill = source, color = "blue"), alpha = 0.7) +
    geom_density(data= aggregate_04Aug2025_O, aes(x = NH3, fill = source, color = "red"), alpha = 0.7) +
    labs(title = paste("Density Comparison:", NH3),
         x = NH3,
         y = "Density") +
    theme_minimal()
  
  # 2. Box plot comparison
  p2 <- ggplot(clean_data, aes(x = source, y = .data[[column_name]], fill = source)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Box Plot Comparison:", column_name),
         x = "Source",
         y = column_name) +
    theme_minimal() +
    scale_fill_manual(values = c("04Aug2025" = "blue", "04Aug2025_O" = "red"))
  
  # 3. Scatter plot if we have matching rows (by row index)
  if (nrow(likelihood_04Aug2025) == nrow(likelihood_04Aug2025_O)) {
    scatter_data <- data.frame(
      x = likelihood_04Aug2025[[column_name]][is.finite(likelihood_04Aug2025[[column_name]])],
      y = likelihood_04Aug2025_O[[column_name]][is.finite(likelihood_04Aug2025_O[[column_name]])]
    )
    
    if (nrow(scatter_data) > 0) {
      p3 <- ggplot(scatter_data, aes(x = x, y = y)) +
        geom_point(alpha = 0.6) +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        labs(title = paste("Scatter Plot:", column_name),
             x = "04Aug2025",
             y = "04Aug2025_O") +
        theme_minimal()
      
      # Combine all plots
      combined_plot <- grid.arrange(p1, p2, p3, ncol = 2, nrow = 2)
    } else {
      combined_plot <- grid.arrange(p1, p2, ncol = 2)
    }
  } else {
    combined_plot <- grid.arrange(p1, p2, ncol = 2)
  }
  
  # Save the plot
  plot_filename <- file.path(output_dir, paste0(output_prefix, "_", column_name, "_comparison.png"))
  ggsave(plot_filename, combined_plot, width = 12, height = 8, dpi = 300)
  cat("Saved plot:", plot_filename, "\n")
  
  # Print summary statistics
  cat("\n=== Summary Statistics for", column_name, "===\n")
  summary_stats <- clean_data %>%
    group_by(source) %>%
    summarise(
      count = n(),
      mean = mean(.data[[column_name]], na.rm = TRUE),
      median = median(.data[[column_name]], na.rm = TRUE),
      sd = sd(.data[[column_name]], na.rm = TRUE),
      min = min(.data[[column_name]], na.rm = TRUE),
      max = max(.data[[column_name]], na.rm = TRUE),
      .groups = 'drop'
    )
  print(summary_stats)
  
  return(combined_plot)
}

# Create plots for all numeric columns
cat("=== Creating Comparison Plots ===\n")
for (col in numeric_col_names) {
  if (col != "source") {  # Skip the source column we added
    create_comparison_plots(combined_data, col, "likelihood_soboljansen")
  }
}

# Create a summary comparison table
cat("\n=== Creating Summary Comparison ===\n")
summary_comparison <- data.frame()

for (col in numeric_col_names) {
  if (col != "source") {
    stats_04Aug2025 <- likelihood_04Aug2025[[col]][is.finite(likelihood_04Aug2025[[col]])]
    stats_04Aug2025_O <- likelihood_04Aug2025_O[[col]][is.finite(likelihood_04Aug2025_O[[col]])]
    
    if (length(stats_04Aug2025) > 0 && length(stats_04Aug2025_O) > 0) {
      summary_row <- data.frame(
        column = col,
        mean_04Aug2025 = mean(stats_04Aug2025, na.rm = TRUE),
        mean_04Aug2025_O = mean(stats_04Aug2025_O, na.rm = TRUE),
        median_04Aug2025 = median(stats_04Aug2025, na.rm = TRUE),
        median_04Aug2025_O = median(stats_04Aug2025_O, na.rm = TRUE),
        sd_04Aug2025 = sd(stats_04Aug2025, na.rm = TRUE),
        sd_04Aug2025_O = sd(stats_04Aug2025_O, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      summary_row$mean_diff <- summary_row$mean_04Aug2025 - summary_row$mean_04Aug2025_O
      summary_row$median_diff <- summary_row$median_04Aug2025 - summary_row$median_04Aug2025_O
      
      summary_comparison <- rbind(summary_comparison, summary_row)
    }
  }
}

# Save summary comparison
summary_file <- file.path(output_dir, "likelihood_soboljansen_summary_comparison.csv")
write.csv(summary_comparison, summary_file, row.names = FALSE)
cat("Saved summary comparison:", summary_file, "\n")

# Print the summary
cat("\n=== Summary Comparison Table ===\n")
print(summary_comparison)

# Check for identical data
cat("\n=== Data Comparison Check ===\n")
if (nrow(likelihood_04Aug2025) == nrow(likelihood_04Aug2025_O) && 
    ncol(likelihood_04Aug2025) == ncol(likelihood_04Aug2025_O)) {
  
  # Check if column names match
  col_match <- all(colnames(likelihood_04Aug2025) == colnames(likelihood_04Aug2025_O))
  cat("Column names match:", col_match, "\n")
  
  if (col_match) {
    # Check for identical values (excluding source column)
    common_cols <- setdiff(colnames(likelihood_04Aug2025), "source")
    identical_check <- sapply(common_cols, function(col) {
      identical(likelihood_04Aug2025[[col]], likelihood_04Aug2025_O[[col]])
    })
    
    cat("Columns with identical values:\n")
    print(identical_check[identical_check])
    
    if (length(identical_check[!identical_check]) > 0) {
      cat("\nColumns with different values:\n")
      print(names(identical_check[!identical_check]))
    }
    
    # Overall identical check
    overall_identical <- all(identical_check)
    cat("\nOverall datasets identical:", overall_identical, "\n")
  }
} else {
  cat("Datasets have different dimensions - cannot perform detailed comparison\n")
  cat("04Aug2025 dimensions:", nrow(likelihood_04Aug2025), "x", ncol(likelihood_04Aug2025), "\n")
  cat("04Aug2025_O dimensions:", nrow(likelihood_04Aug2025_O), "x", ncol(likelihood_04Aug2025_O), "\n")
}

cat("\n=== Analysis Complete ===\n")
cat("All plots and summary saved to:", output_dir, "\n") 