#!/usr/bin/env Rscript
# Script to compare dc_aggRslt_#.rds files row by row between directories

library(tools)
library(utils)

# Set paths to the two directories
path_04Aug2025   <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025/GSA/soboljansen/Daily_Outputs"
path_04Aug2025_O <- "/data/rubelscratch/rubelogle/daycent_calibration/results/nh3_volatilization/04Aug2025_O/GSA/soboljansen/Daily_Outputs"

# Function to find all jobGroup directories
find_jobgroup_dirs <- function(base_path) {
  if (!dir.exists(base_path)) {
    warning(paste("Directory does not exist:", base_path))
    return(character(0))
  }
  
  dirs <- list.dirs(base_path, full.names = TRUE, recursive = FALSE)
  jobgroup_dirs <- dirs[grepl("jobGroup_\\d+", basename(dirs))]
  return(jobgroup_dirs)
}

# Function to find all dc_aggRslt_#.rds files in a directory
find_dc_aggRslt_files <- function(directory) {
  if (!dir.exists(directory)) {
    return(character(0))
  }
  
  files <- list.files(directory, pattern = "dc_dRslt_.*\\.rds$", full.names = TRUE)
  return(files)
}

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

# Main comparison function
compare_dc_aggRslt_files <- function() {
  cat("Finding jobGroup directories...\n")
  
  # Find all jobGroup directories in both paths
  jobgroups_04Aug2025 <- find_jobgroup_dirs(path_04Aug2025)
  jobgroups_04Aug2025_O <- find_jobgroup_dirs(path_04Aug2025_O)
  
  cat("Found", length(jobgroups_04Aug2025), "jobGroup directories in 04Aug2025\n")
  cat("Found", length(jobgroups_04Aug2025_O), "jobGroup directories in 04Aug2025_O\n")
  
  # Extract jobGroup numbers
  extract_jobgroup_number <- function(dir_path) {
    basename_dir <- basename(dir_path)
    match <- regmatches(basename_dir, regexpr("\\d+", basename_dir))
    return(match[1])
  }
  
  numbers_04Aug2025 <- sapply(jobgroups_04Aug2025, extract_jobgroup_number)
  numbers_04Aug2025_O <- sapply(jobgroups_04Aug2025_O, extract_jobgroup_number)
  
  # Find common jobGroup numbers
  common_numbers <- intersect(numbers_04Aug2025, numbers_04Aug2025_O)
  
  cat("Found", length(common_numbers), "matching jobGroup pairs\n")
  
  all_differences <- list()
  
  # Loop through each matching jobGroup
  for (num in common_numbers) {
    jobgroup_dir1 <- jobgroups_04Aug2025[numbers_04Aug2025 == num][1]
    jobgroup_dir2 <- jobgroups_04Aug2025_O[numbers_04Aug2025_O == num][1]
    
    cat("Processing jobGroup_", num, "...\n", sep = "")
    
    # Find all dc_aggRslt files in each directory
    files1 <- find_dc_aggRslt_files(jobgroup_dir1)
    files2 <- find_dc_aggRslt_files(jobgroup_dir2)
    
    # Extract file numbers for matching
    extract_file_number <- function(file_path) {
      basename_file <- basename(file_path)
      match <- regmatches(basename_file, regexpr("\\d+", basename_file))
      return(match[1])
    }
    
    if (length(files1) > 0 && length(files2) > 0) {
      numbers1 <- sapply(files1, extract_file_number)
      numbers2 <- sapply(files2, extract_file_number)
      
      # Find common file numbers
      common_file_numbers <- intersect(numbers1, numbers2)
      
      cat("  Found", length(common_file_numbers), "matching dc_aggRslt files\n")
      
      # Compare each matching file
      for (file_num in common_file_numbers) {
        file1 <- files1[numbers1 == file_num][1]
        file2 <- files2[numbers2 == file_num][1]
        
        tryCatch({
          df1 <- readRDS(file1)
          df2 <- readRDS(file2)
          
          file_differences <- compare_rows(df1, df2, paste0("jobGroup_", num, "/dc_dRslt_", file_num, ".rds"))
          
          if (length(file_differences) > 0) {
            all_differences <- c(all_differences, file_differences)
            cat("    dc_dRslt_", file_num, ".rds: Found ", length(file_differences), " differences\n", sep = "")
          } else {
            cat("    dc_dRslt_", file_num, ".rds: No differences found\n", sep = "")
            next  # Skip to next file
          }
          
        }, error = function(e) {
          cat("    Error reading dc_aggRslt_", file_num, ".rds: ", e$message, "\n", sep = "")
        })
      }
    } else {
      cat("  No dc_aggRslt files found in one or both directories\n")
    }
  }
  
  # Save results
  if (length(all_differences) > 0) {
    output_file <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/tests_original_package/dc_aggRslt_differences_daily.csv"
    
    # Convert list of differences to data frame
    differences_df <- do.call(rbind, lapply(all_differences, function(diff) {
      data.frame(
        file = diff$file,
        type = diff$type,
        row = if(is.null(diff$row_name)) NA else diff$row_name,
        column = if(is.null(diff$column_name)) NA else diff$column_name,
        column_name = if(is.null(diff$column_name)) NA else diff$column_name,
        row_name = if(is.null(diff$row_name)) NA else diff$row_name,
        value_04Aug2025 = if(is.null(diff$value_04Aug2025)) NA else as.character(diff$value_04Aug2025),
        value_04Aug2025_O = if(is.null(diff$value_04Aug2025_O)) NA else as.character(diff$value_04Aug2025_O),
        difference = if(is.null(diff$difference)) NA else diff$difference,
        details = if(is.null(diff$details)) NA else diff$details,
        stringsAsFactors = FALSE
      )
    }))
    
    write.csv(differences_df, output_file, row.names = FALSE)
    cat("\nTotal differences found:", length(all_differences), "\n")
    cat("Results saved to:", output_file, "\n")
  } else {
    cat("\nNo differences found between any files\n")
  }
  
  return(all_differences)
}

# Run the comparison
if (!interactive()) {
  if (!file.exists(path_04Aug2025) || !file.exists(path_04Aug2025_O)) {
    cat("Error: One or both directories do not exist.\n")
    quit(status = 1)
  }
  
  results <- compare_dc_aggRslt_files()
}

