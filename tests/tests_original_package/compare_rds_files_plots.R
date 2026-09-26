#!/usr/bin/env Rscript
# Script to compare two Aggregated_Combined_soboljansen.rds files row by row and column by column

library(tools)
library(utils)

# Set paths to the two RDS files
file_04Aug2025   <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025/GSA/soboljansen/Results/Aggregated_Combined_soboljansen.rds"
file_04Aug2025_O <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025_O/GSA/soboljansen/Aggregated_Combined_soboljansen.rds"

cat("=== LOADING AND COMPARING AGGREGATED COMBINED FILES ===\n")
cat("File 1 (04Aug2025):", file_04Aug2025, "\n")
cat("File 2 (04Aug2025_O):", file_04Aug2025_O, "\n\n")

# Check if files exist
if (!file.exists(file_04Aug2025)) {
  stop(paste("File 1 does not exist:", file_04Aug2025))
}
if (!file.exists(file_04Aug2025_O)) {
  stop(paste("File 2 does not exist:", file_04Aug2025_O))
}

# Load the files
cat("Loading files...\n")
df1 <- readRDS(file_04Aug2025)
df2 <- readRDS(file_04Aug2025_O)

cat("File 1 dimensions:", nrow(df1), "rows x", ncol(df1), "columns\n")
cat("File 2 dimensions:", nrow(df2), "rows x", ncol(df2), "columns\n\n")

# Function to compare two data frames row by row
compare_rows <- function(df1, df2, file_name) {
  differences <- list()
  
  # Compare row names
  rows_only_in_df1 <- setdiff(rownames(df1), rownames(df2))
  rows_only_in_df2 <- setdiff(rownames(df2), rownames(df1))
  common_rows <- intersect(rownames(df1), rownames(df2))
  
  # Compare column names
  cols_only_in_df1 <- setdiff(colnames(df1), colnames(df2))
  cols_only_in_df2 <- setdiff(colnames(df2), colnames(df1))
  common_cols <- intersect(colnames(df1), colnames(df2))
  
  # Compare values in shared rows and columns
  diffs <- list()
  for (row in common_rows) {
    for (col in common_cols) {
      val1 <- df1[row, col]
      val2 <- df2[row, col]
      
      # Check if values are different considering 4 decimal places
      values_different <- FALSE
      
      if (is.numeric(val1) && is.numeric(val2)) {
        # Round to 4 decimal places and compare
        val1_rounded <- round(val1, 4)
        val2_rounded <- round(val2, 4)
        values_different <- !identical(val1_rounded, val2_rounded)
      } else {
        # For non-numeric values, use exact comparison
        values_different <- !identical(val1, val2)
      }
      
      if (values_different) {
        diffs[[paste(row, col, sep = ":")]] <- list(table1 = val1, table2 = val2)
      }
    }
  }
  
  # Convert diffs to differences format
  for (key in names(diffs)) {
    parts <- strsplit(key, ":")[[1]]
    row_name <- parts[1]
    col_name <- parts[2]
    
    differences[[length(differences) + 1]] <- list(
      file = file_name,
      type = "value_difference",
      row_name = row_name,
      column_name = col_name,
      value_04Aug2025 = diffs[[key]]$table1,
      value_04Aug2025_O = diffs[[key]]$table2,
      difference = if(is.numeric(diffs[[key]]$table1) && is.numeric(diffs[[key]]$table2)) 
        diffs[[key]]$table1 - diffs[[key]]$table2 else NA
    )
  }
  
  # Report columns that exist in one table but not the other
  if (length(cols_only_in_df1) > 0) {
    differences[[length(differences) + 1]] <- list(
      file = file_name,
      type = "columns_only_in_04Aug2025",
      details = paste("Columns only in 04Aug2025:", paste(cols_only_in_df1, collapse = ", "))
    )
  }
  
  if (length(cols_only_in_df2) > 0) {
    differences[[length(differences) + 1]] <- list(
      file = file_name,
      type = "columns_only_in_04Aug2025_O",
      details = paste("Columns only in 04Aug2025_O:", paste(cols_only_in_df2, collapse = ", "))
    )
  }
  
  # Report rows that exist in one table but not the other
  if (length(rows_only_in_df1) > 0) {
    differences[[length(differences) + 1]] <- list(
      file = file_name,
      type = "rows_only_in_04Aug2025",
      details = paste("Rows only in 04Aug2025:", paste(head(rows_only_in_df1, 10), collapse = ", "),
                     if(length(rows_only_in_df1) > 10) paste("... and", length(rows_only_in_df1) - 10, "more") else "")
    )
  }
  
  if (length(rows_only_in_df2) > 0) {
    differences[[length(differences) + 1]] <- list(
      file = file_name,
      type = "rows_only_in_04Aug2025_O", 
      details = paste("Rows only in 04Aug2025_O:", paste(head(rows_only_in_df2, 10), collapse = ", "),
                     if(length(rows_only_in_df2) > 10) paste("... and", length(rows_only_in_df2) - 10, "more") else "")
    )
  }
  
  return(differences)
}

# Perform the comparison
cat("=== COMPARING TABLES ROW BY ROW AND COLUMN BY COLUMN ===\n")

# Compare the two loaded tables
differences <- compare_rows(df1, df2, "Aggregated_Combined_soboljansen.rds")

# Display summary results
if (length(differences) > 0) {
  cat("\n=== DIFFERENCES FOUND ===\n")
  
  # Count different types of differences
  value_diffs <- sum(sapply(differences, function(x) x$type == "value_difference"))
  column_diffs <- sum(sapply(differences, function(x) grepl("columns_only", x$type)))
  row_diffs <- sum(sapply(differences, function(x) grepl("rows_only", x$type)))
  
  cat("Value differences:", value_diffs, "\n")
  cat("Column differences:", column_diffs, "\n") 
  cat("Row differences:", row_diffs, "\n")
  
  # Show first few value differences
  value_differences <- differences[sapply(differences, function(x) x$type == "value_difference")]
  if (length(value_differences) > 0) {
    cat("\n=== FIRST 10 VALUE DIFFERENCES ===\n")
    for (i in seq_len(min(10, length(value_differences)))) {
      diff <- value_differences[[i]]
      cat(sprintf("Row: %s, Column: %s\n", diff$row_name, diff$column_name))
      cat(sprintf("  04Aug2025: %s\n", as.character(diff$value_04Aug2025)))
      cat(sprintf("  04Aug2025_O: %s\n", as.character(diff$value_04Aug2025_O)))
      if (!is.na(diff$difference)) {
        cat(sprintf("  Difference: %g\n", diff$difference))
      }
      cat("\n")
    }
    if (length(value_differences) > 10) {
      cat("... and", length(value_differences) - 10, "more value differences\n")
    }
  }
  
  # Show structural differences
  struct_diffs <- differences[!sapply(differences, function(x) x$type == "value_difference")]
  if (length(struct_diffs) > 0) {
    cat("\n=== STRUCTURAL DIFFERENCES ===\n")
    for (diff in struct_diffs) {
      cat(diff$type, ":", diff$details, "\n")
    }
  }
  
} else {
  cat("\n=== NO DIFFERENCES FOUND ===\n")
  cat("The two tables are identical!\n")
}

# Save detailed results if differences were found
if (length(differences) > 0) {
  cat("\n=== SAVING RESULTS ===\n")
  
  # Save detailed results as RDS
  output_file_rds <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/tests_original_package/aggregated_combined_differences.rds"
  saveRDS(differences, output_file_rds)
  cat("Detailed differences saved to:", output_file_rds, "\n")
  
  # Save summary as CSV
  output_file_csv <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/tests_original_package/aggregated_combined_differences.csv"
  
  # Convert to data frame
  differences_df <- do.call(rbind, lapply(differences, function(diff) {
    data.frame(
      file = diff$file,
      type = diff$type,
      row_name = if(is.null(diff$row_name)) NA else diff$row_name,
      column_name = if(is.null(diff$column_name)) NA else diff$column_name,
      value_04Aug2025 = if(is.null(diff$value_04Aug2025)) NA else as.character(diff$value_04Aug2025),
      value_04Aug2025_O = if(is.null(diff$value_04Aug2025_O)) NA else as.character(diff$value_04Aug2025_O),
      difference = if(is.null(diff$difference)) NA else diff$difference,
      details = if(is.null(diff$details)) NA else diff$details,
      stringsAsFactors = FALSE
    )
  }))
  
  write.csv(differences_df, output_file_csv, row.names = FALSE)
  cat("Summary CSV saved to:", output_file_csv, "\n")
  
  cat("\nTotal differences found:", length(differences), "\n")
}

cat("\n=== COMPARISON COMPLETE ===\n")

