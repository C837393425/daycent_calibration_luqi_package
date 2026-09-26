#!/usr/bin/env Rscript
# Script to compare dc_aggRslt_#.rds files row by row between directories

library(tools)
library(utils)

# Set paths to the two directories
path_04Aug2025   <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025/GSA/soboljansen/Aggregated_Combined_soboljansen.rds"
path_04Aug2025_O <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025_O/GSA/soboljansen/Aggregated_Combined_soboljansen.rds"


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

df1 <- readRDS(path_04Aug2025)
df2 <- readRDS(path_04Aug2025_O)

file_differences <- compare_rows(df1, df2, "Aggregated_Combined_soboljansen.rds")

# Save results
if (length(file_differences) > 0) {
  output_file <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/tests_original_package/aggregated_combine_differences.csv"
  
  # Convert list of differences to data frame
  differences_df <- do.call(rbind, lapply(file_differences, function(diff) {
    data.frame(
      file = diff$file,
      type = diff$type,

  }
}


#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Likelihood_combined

# Set paths to the two directories
path_04Aug2025   <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025/GSA/soboljansen/Likelihood_Combined_soboljansen.rds"
path_04Aug2025_O <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025_O/GSA/soboljansen/Likelihood_Combined_soboljansen.rds"

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
      value_04Aug2025_O = diffs[[key]]$table2
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

df1 <- readRDS(path_04Aug2025)
df2 <- readRDS(path_04Aug2025_O)

row.names(df1) <- NULL
row.names(df2) <- NULL

df2$Variable[df2$Variable == "Individual-NH3"] <- "nh3_individual"
df2$Variable[df2$Variable == "Cumulative-NH3"] <- "nh3_cumulative"
df2$Variable[df2$Variable == "Individual-Urea"] <- "urea_individual"

file_differences <- compare_rows(df1, df2, "Likelihood_Combined_soboljansen.rds")

# Save results
if (length(file_differences) > 0) {
  output_file <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/tests_original_package/likelihood_combine_differences.csv"
  
  # Convert list of differences to data frame
  differences_df <- do.call(rbind, lapply(file_differences, function(diff) {
    data.frame(
      file = diff$file,
      type = diff$type,
  }
}

