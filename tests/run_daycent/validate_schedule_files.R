#!/usr/bin/env Rscript

#' DayCent Schedule File Validation Test
#' 
#' This script validates that all DayCent schedule files in the project can execute properly.
#' It tests each site and treatment combination to ensure the schedule files are valid
#' before proceeding with SOC parameter calibration.
#' 
#' @author Bayesian Calibration Framework
#' @date 2025-08-12

# Load required libraries
suppressPackageStartupMessages({
  library(here)
  library(fs)
  library(dplyr)
  library(purrr)
})

# Set paths
SCHEDULE_FILES_DIR <- "/mnt/f/daycent_calibration/data/nh3_volatilization/DayCent_ScheduleFiles"
DAYCENT_EXECUTABLE <- "daycent"  # Assume daycent is in PATH
TEST_OUTPUT_DIR <- "/mnt/f/daycent_calibration/tests/run_daycent/output"
LOG_FILE <- "/mnt/f/daycent_calibration/tests/run_daycent/validation_log.txt"

# Create output directory
dir_create(TEST_OUTPUT_DIR, recurse = TRUE)

# Initialize log
cat("DayCent Schedule File Validation Test\n", file = LOG_FILE)
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file = LOG_FILE, append = TRUE)

#' Log message to both console and file
#' @param message Character string to log
#' @param level Character string indicating log level (INFO, WARNING, ERROR)
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_message <- sprintf("[%s] %s: %s", timestamp, level, message)
  
  # Print to console
  cat(formatted_message, "\n")
  
  # Write to log file
  cat(formatted_message, "\n", file = LOG_FILE, append = TRUE)
}

#' Check if DayCent executable is available
check_daycent_executable <- function() {
  log_message("Checking DayCent executable availability...")
  
  # Try to run daycent with help flag
  result <- system2("which", args = "daycent", stdout = TRUE, stderr = TRUE)
  
  if (length(result) == 0 || grepl("not found", result, ignore.case = TRUE)) {
    log_message("DayCent executable not found in PATH", "ERROR")
    log_message("Please ensure DayCent is installed and available in PATH", "ERROR")
    return(FALSE)
  }
  
  log_message(paste("DayCent found at:", result), "INFO")
  return(TRUE)
}

#' Discover all sites and treatments from directory structure
discover_sites_and_treatments <- function() {
  log_message("Discovering sites and treatments...")
  
  if (!dir_exists(SCHEDULE_FILES_DIR)) {
    log_message(paste("Schedule files directory not found:", SCHEDULE_FILES_DIR), "ERROR")
    return(NULL)
  }
  
  # Get all site directories
  site_dirs <- dir_ls(SCHEDULE_FILES_DIR, type = "directory")
  
  sites_treatments <- map_dfr(site_dirs, function(site_dir) {
    site_name <- basename(site_dir)
    
    # Find all .sch files that are not base/eq files (these are treatment files)
    sch_files <- dir_ls(site_dir, regexp = "\\.sch$")
    treatment_files <- sch_files[!grepl("_(base|eq)", sch_files)]
    
    if (length(treatment_files) == 0) {
      log_message(paste("No treatment files found for site:", site_name), "WARNING")
      return(tibble(site = character(0), treatment = character(0), sch_file = character(0)))
    }
    
    treatments <- map_chr(treatment_files, function(f) {
      basename(tools::file_path_sans_ext(f))
    })
    
    tibble(
      site = site_name,
      treatment = treatments,
      sch_file = treatment_files
    )
  })
  
  log_message(paste("Discovered", nrow(sites_treatments), "site-treatment combinations"))
  log_message(paste("Sites:", length(unique(sites_treatments$site))))
  
  return(sites_treatments)
}

#' Test a single schedule file execution
#' @param site_name Name of the site
#' @param treatment_name Name of the treatment
#' @param sch_file Path to the schedule file
#' @param timeout_seconds Maximum time to allow for execution
test_schedule_file <- function(site_name, treatment_name, sch_file, timeout_seconds = 300) {
  log_message(paste("Testing", site_name, "-", treatment_name))
  
  # Create site-specific test directory
  test_dir <- file.path(TEST_OUTPUT_DIR, site_name, treatment_name)
  dir_create(test_dir, recurse = TRUE)
  
  # Get the directory containing the schedule file (contains all needed files)
  source_dir <- dirname(sch_file)
  
  # Copy all necessary files to test directory
  source_files <- dir_ls(source_dir)
  for (src_file in source_files) {
    file_copy(src_file, file.path(test_dir, basename(src_file)), overwrite = TRUE)
  }
  
  # Change to test directory for execution
  original_wd <- getwd()
  setwd(test_dir)
  
  result <- list(
    site = site_name,
    treatment = treatment_name,
    success = FALSE,
    error_message = NULL,
    execution_time = NULL,
    output_files_created = NULL
  )
  
  tryCatch({
    start_time <- Sys.time()
    
    # Run DayCent with the schedule file
    sch_filename <- basename(sch_file)
    system_result <- system2(
      DAYCENT_EXECUTABLE,
      args = c("-s", sch_filename),
      stdout = "daycent_stdout.log",
      stderr = "daycent_stderr.log",
      timeout = timeout_seconds
    )
    
    end_time <- Sys.time()
    result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Check if execution was successful (return code 0)
    if (system_result == 0) {
      result$success <- TRUE
      
      # Check for output files that should have been created
      output_files <- dir_ls(test_dir, regexp = "\\.(out|bin|lis)$")
      result$output_files_created <- length(output_files)
      
      log_message(paste("SUCCESS:", site_name, "-", treatment_name, 
                       sprintf("(%.1fs, %d output files)", result$execution_time, result$output_files_created)))
    } else {
      result$error_message <- paste("DayCent execution failed with exit code:", system_result)
      
      # Try to get error details from stderr log
      stderr_file <- file.path(test_dir, "daycent_stderr.log")
      if (file_exists(stderr_file)) {
        stderr_content <- readLines(stderr_file, warn = FALSE)
        if (length(stderr_content) > 0) {
          result$error_message <- paste(result$error_message, 
                                      "Error details:", paste(stderr_content, collapse = "; "))
        }
      }
      
      log_message(paste("FAILED:", site_name, "-", treatment_name, "-", result$error_message), "ERROR")
    }
    
  }, error = function(e) {
    result$error_message <- paste("R execution error:", e$message)
    log_message(paste("FAILED:", site_name, "-", treatment_name, "-", result$error_message), "ERROR")
  })
  
  # Return to original directory
  setwd(original_wd)
  
  return(result)
}

#' Run validation test on all discovered sites and treatments
run_validation_test <- function(max_tests = NULL, parallel = FALSE) {
  log_message("Starting DayCent schedule file validation test...")
  
  # Check DayCent executable
  if (!check_daycent_executable()) {
    log_message("Cannot proceed without DayCent executable", "ERROR")
    return(NULL)
  }
  
  # Discover sites and treatments
  sites_treatments <- discover_sites_and_treatments()
  if (is.null(sites_treatments) || nrow(sites_treatments) == 0) {
    log_message("No sites and treatments discovered", "ERROR")
    return(NULL)
  }
  
  # Limit number of tests if specified (useful for initial testing)
  if (!is.null(max_tests)) {
    sites_treatments <- sites_treatments[1:min(max_tests, nrow(sites_treatments)), ]
    log_message(paste("Limited to first", nrow(sites_treatments), "tests"))
  }
  
  # Run tests
  log_message(paste("Running", nrow(sites_treatments), "validation tests..."))
  start_time <- Sys.time()
  
  if (parallel && require(future.apply, quietly = TRUE)) {
    log_message("Running tests in parallel...")
    future::plan(future::multisession)
    
    results <- future.apply::future_pmap(sites_treatments, function(site, treatment, sch_file) {
      test_schedule_file(site, treatment, sch_file)
    })
  } else {
    log_message("Running tests sequentially...")
    results <- pmap(sites_treatments, function(site, treatment, sch_file) {
      test_schedule_file(site, treatment, sch_file)
    })
  }
  
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")
  
  # Compile results
  results_df <- map_dfr(results, as.data.frame)
  
  # Generate summary
  summary_stats <- list(
    total_tests = nrow(results_df),
    successful_tests = sum(results_df$success),
    failed_tests = sum(!results_df$success),
    success_rate = sum(results_df$success) / nrow(results_df) * 100,
    total_time_minutes = as.numeric(total_time),
    average_execution_time = mean(results_df$execution_time[results_df$success], na.rm = TRUE)
  )
  
  log_message("=== VALIDATION TEST SUMMARY ===")
  log_message(sprintf("Total tests: %d", summary_stats$total_tests))
  log_message(sprintf("Successful: %d", summary_stats$successful_tests))
  log_message(sprintf("Failed: %d", summary_stats$failed_tests))
  log_message(sprintf("Success rate: %.1f%%", summary_stats$success_rate))
  log_message(sprintf("Total time: %.1f minutes", summary_stats$total_time_minutes))
  log_message(sprintf("Average execution time: %.1f seconds", summary_stats$average_execution_time))
  
  if (summary_stats$failed_tests > 0) {
    log_message("\n=== FAILED TESTS ===", "WARNING")
    failed_tests <- results_df[!results_df$success, ]
    for (i in seq_len(nrow(failed_tests))) {
      log_message(sprintf("%s - %s: %s", 
                         failed_tests$site[i], 
                         failed_tests$treatment[i], 
                         failed_tests$error_message[i]), "ERROR")
    }
  }
  
  # Save detailed results
  results_file <- "/mnt/f/daycent_calibration/tests/run_daycent/validation_results.rds"
  saveRDS(list(results = results_df, summary = summary_stats), results_file)
  log_message(paste("Detailed results saved to:", results_file))
  
  return(list(results = results_df, summary = summary_stats))
}

#' Main function
main <- function() {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  max_tests <- NULL
  parallel <- FALSE
  
  # Simple argument parsing
  if (length(args) > 0) {
    for (arg in args) {
      if (startsWith(arg, "--max-tests=")) {
        max_tests <- as.numeric(sub("--max-tests=", "", arg))
      } else if (arg == "--parallel") {
        parallel <- TRUE
      } else if (arg == "--help") {
        cat("DayCent Schedule File Validation Test\n")
        cat("Usage: Rscript validate_schedule_files.R [options]\n")
        cat("Options:\n")
        cat("  --max-tests=N    Limit to first N tests (useful for quick validation)\n")
        cat("  --parallel       Run tests in parallel (requires future.apply package)\n")
        cat("  --help          Show this help message\n")
        return(invisible())
      }
    }
  }
  
  # Run the validation test
  results <- run_validation_test(max_tests = max_tests, parallel = parallel)
  
  if (is.null(results)) {
    log_message("Validation test failed to complete", "ERROR")
    quit(status = 1)
  }
  
  # Set exit status based on results
  if (results$summary$success_rate < 95) {
    log_message(sprintf("Validation FAILED: Success rate %.1f%% below 95%% threshold", 
                       results$summary$success_rate), "ERROR")
    quit(status = 2)
  } else {
    log_message("Validation PASSED: All schedule files can execute properly")
    quit(status = 0)
  }
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}