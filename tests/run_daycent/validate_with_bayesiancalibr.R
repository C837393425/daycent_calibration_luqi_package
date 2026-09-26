#!/usr/bin/env Rscript

#' DayCent Schedule File Validation using bayesiancalibr Package
#' 
#' This script uses the existing bayesiancalibr package functions to validate 
#' DayCent schedule files by actually running them. It leverages the same functions
#' used in GSA Step 2 for consistent execution.
#' 
#' @author Bayesian Calibration Framework
#' @date 2025-08-12

# === PATH CONFIGURATION ===
# Determine project root directory (assumes script is in tests/run_daycent/)
get_script_dir <- function() {
  # Try multiple methods to get script location
  if (exists("sys.frame")) {
    tryCatch({
      return(dirname(dirname(dirname(normalizePath(sys.frame(1)$ofile)))))
    }, error = function(e) NULL)
  }
  
  # Try commandArgs method
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- NULL
  for (i in seq_along(args)) {
    if (grepl("^--file=", args[i])) {
      script_path <- sub("^--file=", "", args[i])
      break
    }
  }
  
  if (!is.null(script_path)) {
    return(dirname(dirname(dirname(normalizePath(script_path)))))
  }
  
  # Fallback: search from current directory
  return(getwd())
}

script_dir <- get_script_dir()

# Search for project root by looking for CLAUDE.md
while (!file.exists(file.path(script_dir, "CLAUDE.md")) && script_dir != dirname(script_dir)) {
  script_dir <- dirname(script_dir)
}

if (!file.exists(file.path(script_dir, "CLAUDE.md"))) {
  stop("Could not find project root directory containing CLAUDE.md")
}

# Set all paths relative to project root
LAIRICE_ROOT <- script_dir
CONFIG_FILE <- file.path(script_dir, "workflows", "configs", "nh3_volatilization.yaml")
SCRATCH_TEST_DIR <- file.path(script_dir, "scratch", "tests")
LOG_FILE <- file.path(script_dir, "tests", "run_daycent", "bayesiancalibr_validation_log.txt")
SCHEDULE_FILES_DIR <- file.path(script_dir, "data", "soil_organic_carbon", "Daycent_ScheduleFiles")
RESULTS_FILE <- file.path(script_dir, "tests", "run_daycent", "bayesiancalibr_validation_results.rds")
CUSTOM_LIB_PATH <- file.path(script_dir, "rlib")

# === LIBRARY SETUP ===
# Set custom library path and load required libraries
.libPaths(CUSTOM_LIB_PATH)
suppressPackageStartupMessages({
  library(bayesiancalibr)
  library(yaml)
  library(fs)
  library(dplyr)
  library(purrr)
})

# Create scratch test directory
dir_create(SCRATCH_TEST_DIR, recurse = TRUE)

# Initialize log
cat("DayCent Schedule File Validation using bayesiancalibr\n", file = LOG_FILE)
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file = LOG_FILE, append = TRUE)

#' Log message to both console and file
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_message <- sprintf("[%s] %s: %s", timestamp, level, message)
  
  cat(formatted_message, "\n")
  cat(formatted_message, "\n", file = LOG_FILE, append = TRUE)
}

#' Load and validate configuration
load_config <- function() {
  log_message("Loading configuration from config file...")
  
  if (!file_exists(CONFIG_FILE)) {
    log_message(paste("Config file not found:", CONFIG_FILE), "ERROR")
    return(NULL)
  }
  
  config <- read_yaml(CONFIG_FILE)
  
  # Build full paths
  daycent_exe <- file.path(LAIRICE_ROOT, config$daycent$executable)
  schedule_files_dir <- SCHEDULE_FILES_DIR
  
  # Validate DayCent executable
  if (!file_exists(daycent_exe)) {
    log_message(paste("DayCent executable not found:", daycent_exe), "ERROR")
    return(NULL)
  }
  
  # Validate schedule files directory
  if (!dir_exists(schedule_files_dir)) {
    log_message(paste("Schedule files directory not found:", schedule_files_dir), "ERROR")
    return(NULL)
  }
  
  log_message(paste("DayCent executable:", daycent_exe))
  log_message(paste("Schedule files directory:", schedule_files_dir))
  
  return(list(
    config = config,
    daycent_exe = daycent_exe,
    schedule_files_dir = schedule_files_dir
  ))
}

#' Discover all sites and treatment schedule files
discover_schedule_files <- function(schedule_files_dir) {
  log_message("Discovering schedule files...")
  
  # Get all site directories
  site_dirs <- dir_ls(schedule_files_dir, type = "directory")
  
  all_files <- map_dfr(site_dirs, function(site_dir) {
    site_name <- basename(site_dir)
    
    # Find all .sch files that are not base/eq files (treatment files)
    sch_files <- dir_ls(site_dir, regexp = "\\.sch$")
    treatment_files <- sch_files[!grepl("_(base|eq)", sch_files)]
    
    if (length(treatment_files) == 0) {
      log_message(paste("No treatment files found for site:", site_name), "WARNING")
      return(tibble())
    }
    
    map_dfr(treatment_files, function(sch_file) {
      treatment_name <- basename(tools::file_path_sans_ext(sch_file))
      tibble(
        site = site_name,
        treatment = treatment_name,
        sch_file = sch_file,
        site_dir = site_dir
      )
    })
  })
  
  log_message(paste("Found", nrow(all_files), "treatment schedule files across", 
                   length(unique(all_files$site)), "sites"))
  
  return(all_files)
}

#' Test a single schedule file using bayesiancalibr's run_DayCent function
test_single_schedule <- function(site, treatment, sch_file, site_dir, daycent_exe, config, timeout_seconds = 300) {
  log_message(paste("Testing", site, "-", treatment))
  
  # Create unique test directory in scratch area
  test_id <- paste0(site, "_", treatment, "_", format(Sys.time(), "%H%M%S"))
  test_dir <- file.path(SCRATCH_TEST_DIR, test_id)
  dir_create(test_dir, recurse = TRUE)
  
  # Ensure cleanup on exit (but keep failed tests for debugging)
  cleanup_test_dir <- TRUE
  on.exit({
    if (cleanup_test_dir && dir_exists(test_dir)) {
      unlink(test_dir, recursive = TRUE)
    }
  })
  
  # Copy only the treatment schedule file and essential non-schedule files
  site_files <- dir_ls(site_dir)
  
  # Copy the specific treatment schedule file
  file_copy(sch_file, file.path(test_dir, basename(sch_file)), overwrite = TRUE)
  
  # Copy essential non-schedule files (everything except .sch files that aren't the target)
  for (src_file in site_files) {
    filename <- basename(src_file)
    # Skip other .sch files, but copy everything else (parameters, inputs, etc.)
    if (!grepl("\\.sch$", filename) || filename == basename(sch_file)) {
      if (filename != basename(sch_file)) {  # Don't copy the target file twice
        file_copy(src_file, file.path(test_dir, filename), overwrite = TRUE)
      }
    }
  }
  
  # Copy all essential DayCent files from dot100_path
  dot100_path <- file.path(LAIRICE_ROOT, config$paths$dot100_path)
  if (dir_exists(dot100_path)) {
    dot100_files <- dir_ls(dot100_path)
    for (dot100_file in dot100_files) {
      file_copy(dot100_file, file.path(test_dir, basename(dot100_file)), overwrite = TRUE)
    }
    log_message(paste("Copied", length(dot100_files), "files from dot100Files directory"))
  } else {
    log_message(paste("Warning: dot100Files directory not found at", dot100_path), "WARNING")
  }
  
  # Change to test directory for execution (DayCent needs to run in directory with files)
  original_wd <- getwd()
  setwd(test_dir)
  
  result <- list(
    site = site,
    treatment = treatment,
    success = FALSE,
    error_message = NULL,
    execution_time = NULL,
    daycent_status = NULL,
    output_files_created = 0
  )
  
  tryCatch({
    start_time <- Sys.time()
    
    # Use bayesiancalibr's run_DayCent function
    # Note: run_DayCent expects to run in the directory containing the schedule file
    sch_filename <- basename(sch_file)
    
    log_message(paste("Running DayCent with schedule file:", sch_filename))
    
    # Capture output and run DayCent
    daycent_result <- run_DayCent(
      filepath_exe = daycent_exe,
      sch_file = sch_filename
    )
    
    end_time <- Sys.time()
    result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    result$daycent_status <- daycent_result
    
    # Check if DayCent ran successfully (run_DayCent returns 0 for success, 1 for failure)
    if (daycent_result == 0) {
      result$success <- TRUE
      
      # Count output files created
      output_files <- dir_ls(test_dir, regexp = "\\.(out|bin|lis)$")
      result$output_files_created <- length(output_files)
      
      log_message(paste("SUCCESS:", site, "-", treatment, 
                       sprintf("(%.1fs, %d output files)", result$execution_time, result$output_files_created)))
    } else {
      result$error_message <- "DayCent execution failed (returned status 1)"
      cleanup_test_dir <- FALSE  # Keep failed test directory for debugging
      
      # Try to get error details from stderr log
      sch_base <- tools::file_path_sans_ext(sch_filename)
      stderr_file <- paste0(sch_base, "_stderr.log")
      
      if (file_exists(stderr_file)) {
        stderr_lines <- readLines(stderr_file, warn = FALSE)
        if (length(stderr_lines) > 0) {
          # Get last few lines of stderr for error details
          error_lines <- tail(stderr_lines, 3)
          result$error_message <- paste(result$error_message, 
                                      "Errors:", paste(error_lines, collapse = "; "))
        }
      }
      
      log_message(paste("FAILED:", site, "-", treatment, "-", result$error_message), "ERROR")
      log_message(paste("Debug: Test directory preserved at", test_dir), "INFO")
    }
    
  }, error = function(e) {
    end_time <- Sys.time()
    result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    result$error_message <- paste("R execution error:", e$message)
    result$daycent_status <- 1
    cleanup_test_dir <- FALSE  # Keep failed test directory for debugging
    log_message(paste("FAILED:", site, "-", treatment, "-", result$error_message), "ERROR")
    log_message(paste("Debug: Test directory preserved at", test_dir), "INFO")
  })
  
  # Return to original directory
  setwd(original_wd)
  
  return(result)
}

#' Run validation test on all discovered schedule files
run_validation_test <- function(max_tests = NULL, sites_filter = NULL) {
  log_message("Starting DayCent schedule file validation using bayesiancalibr...")
  
  # Load configuration
  config_data <- load_config()
  if (is.null(config_data)) {
    log_message("Failed to load configuration", "ERROR")
    return(NULL)
  }
  
  daycent_exe <- config_data$daycent_exe
  schedule_files_dir <- config_data$schedule_files_dir
  
  # Discover schedule files
  schedule_files <- discover_schedule_files(schedule_files_dir)
  if (nrow(schedule_files) == 0) {
    log_message("No schedule files discovered", "ERROR")
    return(NULL)
  }
  
  # Filter by sites if specified
  if (!is.null(sites_filter)) {
    schedule_files <- schedule_files[schedule_files$site %in% sites_filter, ]
    log_message(paste("Filtered to", length(sites_filter), "sites:", paste(sites_filter, collapse = ", ")))
  }
  
  # Limit number of tests if specified
  if (!is.null(max_tests)) {
    schedule_files <- schedule_files[1:min(max_tests, nrow(schedule_files)), ]
    log_message(paste("Limited to first", nrow(schedule_files), "tests"))
  }
  
  # Run tests
  log_message(paste("Running", nrow(schedule_files), "validation tests..."))
  start_time <- Sys.time()
  
  # Run tests with error handling to continue through failures
  results <- vector("list", nrow(schedule_files))
  for (i in seq_len(nrow(schedule_files))) {
    tryCatch({
      row <- schedule_files[i, ]
      results[[i]] <- test_single_schedule(row$site, row$treatment, row$sch_file, row$site_dir, daycent_exe, config_data$config)
    }, error = function(e) {
      log_message(paste("Critical error in test", i, ":", e$message), "ERROR")
      # Create a failure result for this test
      results[[i]] <<- list(
        site = schedule_files$site[i],
        treatment = schedule_files$treatment[i],
        success = FALSE,
        error_message = paste("Critical test error:", e$message),
        execution_time = 0,
        daycent_status = 1,
        output_files_created = 0
      )
    })
  }
  
  end_time <- Sys.time()
  total_time <- difftime(end_time, start_time, units = "mins")
  
  # Compile results with better error handling
  results_df <- map_dfr(results, function(result) {
    # Ensure all required columns exist with default values
    default_result <- list(
      site = NA_character_,
      treatment = NA_character_,
      success = FALSE,
      error_message = "Unknown error",
      execution_time = 0,
      daycent_status = 1,
      output_files_created = 0
    )
    
    # Update with actual result, keeping defaults for missing fields
    if (!is.null(result)) {
      for (name in names(result)) {
        default_result[[name]] <- result[[name]]
      }
    }
    
    return(as.data.frame(default_result, stringsAsFactors = FALSE))
  })
  
  # Generate summary
  summary_stats <- list(
    total_tests = nrow(results_df),
    successful_tests = sum(results_df$success),
    failed_tests = sum(!results_df$success),
    success_rate = sum(results_df$success) / nrow(results_df) * 100,
    total_time_minutes = as.numeric(total_time),
    average_execution_time = mean(results_df$execution_time[results_df$success], na.rm = TRUE),
    total_output_files = sum(results_df$output_files_created, na.rm = TRUE)
  )
  
  log_message("=== VALIDATION TEST SUMMARY ===")
  log_message(sprintf("Total tests: %d", summary_stats$total_tests))
  log_message(sprintf("Successful: %d", summary_stats$successful_tests))
  log_message(sprintf("Failed: %d", summary_stats$failed_tests))
  log_message(sprintf("Success rate: %.1f%%", summary_stats$success_rate))
  log_message(sprintf("Total time: %.1f minutes", summary_stats$total_time_minutes))
  log_message(sprintf("Average execution time: %.1f seconds", summary_stats$average_execution_time))
  log_message(sprintf("Total output files created: %d", summary_stats$total_output_files))
  
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
  results_file <- RESULTS_FILE
  saveRDS(list(results = results_df, summary = summary_stats), results_file)
  log_message(paste("Detailed results saved to:", results_file))
  
  return(list(results = results_df, summary = summary_stats))
}

#' Main function
main <- function() {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  max_tests <- NULL
  sites_filter <- NULL
  
  # Simple argument parsing
  if (length(args) > 0) {
    for (arg in args) {
      if (startsWith(arg, "--max-tests=")) {
        max_tests <- as.numeric(sub("--max-tests=", "", arg))
      } else if (startsWith(arg, "--sites=")) {
        sites_string <- sub("--sites=", "", arg)
        sites_filter <- strsplit(sites_string, ",")[[1]]
      } else if (arg == "--help") {
        cat("DayCent Schedule File Validation using bayesiancalibr\n")
        cat("Usage: Rscript validate_with_bayesiancalibr.R [options]\n")
        cat("Options:\n")
        cat("  --max-tests=N       Limit to first N tests\n")
        cat("  --sites=site1,site2 Limit to specific sites (comma-separated)\n")
        cat("  --help             Show this help message\n")
        cat("\nExamples:\n")
        cat("  Rscript validate_with_bayesiancalibr.R --max-tests=5\n")
        cat("  Rscript validate_with_bayesiancalibr.R --sites=BatonRougeCot,glyndonMN\n")
        return(invisible())
      }
    }
  }
  
  # Run the validation test
  results <- run_validation_test(max_tests = max_tests, sites_filter = sites_filter)
  
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
    log_message("Validation PASSED: Schedule files can execute properly using bayesiancalibr functions")
    quit(status = 0)
  }
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}