#!/usr/bin/env Rscript

#' DayCent Schedule File Integrity Check
#' 
#' This script checks the integrity and structure of all DayCent schedule files
#' without actually running DayCent. It validates file existence, basic structure,
#' and dependencies to ensure files are ready for execution.
#' 
#' @author Yi Yang
#' @date 2025-08-12

# Load required libraries
suppressPackageStartupMessages({
  library(fs)
  library(dplyr)
  library(purrr)
  library(readr)
})

# Set paths
SCHEDULE_FILES_DIR <- "/data/rubelscratch/rubelogle/daycent_calibration/data/soil_organic_carbon/Daycent_ScheduleFiles"
LOG_FILE <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/run_daycent/integrity_check_log.txt"

# Initialize log
cat("DayCent Schedule File Integrity Check\n", file = LOG_FILE)
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file = LOG_FILE, append = TRUE)

#' Log message to both console and file
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_message <- sprintf("[%s] %s: %s", timestamp, level, message)
  
  cat(formatted_message, "\n")
  cat(formatted_message, "\n", file = LOG_FILE, append = TRUE)
}

#' Check if a schedule file has valid basic structure
#' @param sch_file Path to the schedule file
check_schedule_file_structure <- function(sch_file) {
  if (!file_exists(sch_file)) {
    return(list(valid = FALSE, error = "File does not exist"))
  }
  
  tryCatch({
    lines <- readLines(sch_file, warn = FALSE)
    
    if (length(lines) < 17) {
      return(list(valid = FALSE, error = "File too short (less than 17 lines)"))
    }
    
    # Check basic structure - extract numeric values before any comments
    starting_year <- suppressWarnings(as.numeric(trimws(strsplit(lines[1], "\\s+")[[1]][1])))
    last_year <- suppressWarnings(as.numeric(trimws(strsplit(lines[2], "\\s+")[[1]][1])))
    site_file <- trimws(strsplit(lines[3], "\\s+")[[1]][1])
    
    if (is.na(starting_year) || is.na(last_year)) {
      return(list(valid = FALSE, error = "Invalid year format in first two lines"))
    }
    
    if (starting_year >= last_year) {
      return(list(valid = FALSE, error = "Starting year >= Last year"))
    }
    
    if (nchar(site_file) == 0) {
      return(list(valid = FALSE, error = "Missing site file name"))
    }
    
    # Look for "Year Month Option" header
    header_line <- which(grepl("Year\\s+Month\\s+Option", lines, ignore.case = TRUE))
    if (length(header_line) == 0) {
      return(list(valid = FALSE, error = "Missing 'Year Month Option' header"))
    }
    
    return(list(
      valid = TRUE, 
      error = NULL,
      starting_year = starting_year,
      last_year = last_year,
      site_file = site_file,
      header_line = header_line[1],
      total_lines = length(lines)
    ))
    
  }, error = function(e) {
    return(list(valid = FALSE, error = paste("Read error:", e$message)))
  })
}

#' Check if required supporting files exist for a schedule file
#' @param sch_file Path to the schedule file
#' @param site_dir Directory containing the schedule file
check_supporting_files <- function(sch_file, site_dir) {
  # Parse schedule file to get site file name
  sch_check <- check_schedule_file_structure(sch_file)
  if (!sch_check$valid) {
    return(list(valid = FALSE, error = paste("Invalid schedule file:", sch_check$error)))
  }
  
  site_file <- sch_check$site_file
  missing_files <- character(0)
  
  # Check for site file (.100 extension)
  site_file_path <- file.path(site_dir, site_file)
  if (!file_exists(site_file_path)) {
    missing_files <- c(missing_files, site_file)
  }
  
  # Check for weather file (.wth extension)
  wth_files <- dir_ls(site_dir, regexp = "\\.wth$")
  if (length(wth_files) == 0) {
    missing_files <- c(missing_files, "*.wth (weather file)")
  }
  
  # Check for soils.in file
  soils_file <- file.path(site_dir, "soils.in")
  if (!file_exists(soils_file)) {
    missing_files <- c(missing_files, "soils.in")
  }
  
  return(list(
    valid = length(missing_files) == 0,
    error = if (length(missing_files) > 0) paste("Missing files:", paste(missing_files, collapse = ", ")) else NULL,
    site_file = site_file,
    weather_files = length(wth_files),
    has_soils = file_exists(soils_file)
  ))
}

#' Discover and check all sites and treatments
discover_and_check_sites <- function() {
  log_message("Discovering and checking sites and treatments...")
  
  if (!dir_exists(SCHEDULE_FILES_DIR)) {
    log_message(paste("Schedule files directory not found:", SCHEDULE_FILES_DIR), "ERROR")
    return(NULL)
  }
  
  # Get all site directories
  site_dirs <- dir_ls(SCHEDULE_FILES_DIR, type = "directory")
  
  all_results <- map_dfr(site_dirs, function(site_dir) {
    site_name <- basename(site_dir)
    log_message(paste("Checking site:", site_name))
    
    # Find all .sch files
    sch_files <- dir_ls(site_dir, regexp = "\\.sch$")
    
    if (length(sch_files) == 0) {
      log_message(paste("No schedule files found for site:", site_name), "WARNING")
      return(tibble(
        site = site_name,
        treatment = NA_character_,
        sch_file = NA_character_,
        structure_valid = FALSE,
        structure_error = "No schedule files found",
        dependencies_valid = FALSE,
        dependencies_error = "No schedule files to check"
      ))
    }
    
    # Check each schedule file
    site_results <- map_dfr(sch_files, function(sch_file) {
      treatment_name <- basename(tools::file_path_sans_ext(sch_file))
      
      # Check structure
      structure_check <- check_schedule_file_structure(sch_file)
      
      # Check dependencies
      deps_check <- check_supporting_files(sch_file, site_dir)
      
      tibble(
        site = site_name,
        treatment = treatment_name,
        sch_file = sch_file,
        structure_valid = structure_check$valid,
        structure_error = structure_check$error %||% "",
        dependencies_valid = deps_check$valid,
        dependencies_error = deps_check$error %||% "",
        starting_year = structure_check$starting_year %||% NA,
        last_year = structure_check$last_year %||% NA,
        site_file = structure_check$site_file %||% "",
        weather_files = deps_check$weather_files %||% 0,
        has_soils = deps_check$has_soils %||% FALSE
      )
    })
    
    return(site_results)
  })
  
  return(all_results)
}

#' Generate summary report
generate_summary_report <- function(results) {
  log_message("=== INTEGRITY CHECK SUMMARY ===")
  
  total_files <- nrow(results)
  structure_valid <- sum(results$structure_valid, na.rm = TRUE)
  dependencies_valid <- sum(results$dependencies_valid, na.rm = TRUE)
  fully_valid <- sum(results$structure_valid & results$dependencies_valid, na.rm = TRUE)
  
  log_message(sprintf("Total schedule files: %d", total_files))
  log_message(sprintf("Valid structure: %d (%.1f%%)", structure_valid, structure_valid/total_files*100))
  log_message(sprintf("Valid dependencies: %d (%.1f%%)", dependencies_valid, dependencies_valid/total_files*100))
  log_message(sprintf("Fully valid: %d (%.1f%%)", fully_valid, fully_valid/total_files*100))
  
  # Site summary
  site_summary <- results %>%
    group_by(site) %>%
    summarise(
      total_treatments = n(),
      valid_treatments = sum(structure_valid & dependencies_valid, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(site_valid = valid_treatments == total_treatments)
  
  log_message(sprintf("Sites: %d total", nrow(site_summary)))
  log_message(sprintf("Sites fully valid: %d", sum(site_summary$site_valid)))
  
  # Report problematic files
  if (fully_valid < total_files) {
    log_message("\n=== PROBLEMATIC FILES ===", "WARNING")
    
    problem_files <- results[!(results$structure_valid & results$dependencies_valid), ]
    for (i in seq_len(nrow(problem_files))) {
      file_info <- problem_files[i, ]
      issues <- c()
      
      if (!file_info$structure_valid) {
        issues <- c(issues, paste("Structure:", file_info$structure_error))
      }
      if (!file_info$dependencies_valid) {
        issues <- c(issues, paste("Dependencies:", file_info$dependencies_error))
      }
      
      log_message(sprintf("%s/%s: %s", file_info$site, file_info$treatment, 
                         paste(issues, collapse = "; ")), "ERROR")
    }
  }
  
  return(list(
    total_files = total_files,
    structure_valid = structure_valid,
    dependencies_valid = dependencies_valid,
    fully_valid = fully_valid,
    success_rate = fully_valid / total_files * 100,
    site_summary = site_summary
  ))
}

#' Main function
main <- function() {
  log_message("Starting DayCent schedule file integrity check...")
  
  # Discover and check all files
  results <- discover_and_check_sites()
  
  if (is.null(results) || nrow(results) == 0) {
    log_message("No schedule files found or error occurred", "ERROR")
    quit(status = 1)
  }
  
  # Generate summary
  summary_stats <- generate_summary_report(results)
  
  # Save detailed results
  results_file <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/run_daycent/integrity_check_results.rds"
  saveRDS(list(results = results, summary = summary_stats), results_file)
  log_message(paste("Detailed results saved to:", results_file))
  
  # Create CSV report for easy viewing
  csv_file <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/run_daycent/integrity_check_results.csv"
  write_csv(results, csv_file)
  log_message(paste("CSV report saved to:", csv_file))
  
  log_message("Integrity check completed")
  
  # Exit with appropriate status
  if (summary_stats$success_rate >= 95) {
    log_message("PASSED: Schedule files are ready for execution")
    quit(status = 0)
  } else {
    log_message(sprintf("FAILED: Only %.1f%% of files are valid (threshold: 95%%)", 
                       summary_stats$success_rate), "ERROR")
    quit(status = 2)
  }
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}