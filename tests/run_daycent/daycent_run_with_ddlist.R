#!/usr/bin/env Rscript

#' DayCent Test Run with DDList100 using SOC Configuration
#' 
#' This script tests DayCent execution with DDList100 functionality using the
#' soil_organic_carbon.yaml configuration. It runs on a single site and treatment
#' that was successfully validated, then uses DDList100 to generate output files.
#' 
#' @author Bayesian Calibration Framework
#' @date 2025-08-14

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

# Set all paths relative to project root - ensure we use /data path
LAIRICE_ROOT <- "/data/rubelscratch/rubelogle/daycent_calibration"
CONFIG_FILE <- file.path(LAIRICE_ROOT, "workflows", "configs", "soil_organic_carbon.yaml")
DDLIST_TEST_DIR <- file.path(LAIRICE_ROOT, "scratch", "ddlist")
LOG_FILE <- file.path(LAIRICE_ROOT, "tests", "run_daycent", "ddlist_test_log.txt")
CUSTOM_LIB_PATH <- file.path(LAIRICE_ROOT, "rlib")

# Test site and treatment (using mandan_grazing site to debug GSA Step 2 error)
TEST_SITE <- "mandan_grazing"
TEST_TREATMENT <- "mandan_grazing_fertilized_grass"  # Same treatment that failed in GSA Step 2

# === LIBRARY SETUP ===
# Set custom library path and load required libraries
.libPaths(CUSTOM_LIB_PATH)
suppressPackageStartupMessages({
  library(bayesiancalibr)
  library(yaml)
  library(fs)
  library(dplyr)
})

# Create ddlist test directory
dir_create(DDLIST_TEST_DIR, recurse = TRUE)

# Initialize log
cat("DayCent DDList100 Test Run\n", file = LOG_FILE)
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n", file = LOG_FILE, append = TRUE)

#' Log message to both console and file
log_message <- function(message, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  formatted_message <- sprintf("[%s] %s: %s", timestamp, level, message)
  
  cat(formatted_message, "\n")
  cat(formatted_message, "\n", file = LOG_FILE, append = TRUE)
}

#' Load and validate SOC configuration
load_soc_config <- function() {
  log_message("Loading SOC configuration from config file...")
  
  if (!file_exists(CONFIG_FILE)) {
    log_message(paste("Config file not found:", CONFIG_FILE), "ERROR")
    return(NULL)
  }
  
  config <- read_yaml(CONFIG_FILE)
  
  # Build full paths
  daycent_exe <- file.path(LAIRICE_ROOT, config$daycent$executable)
  ddlist_exe <- file.path(LAIRICE_ROOT, config$daycent$list100)
  schedule_files_dir <- file.path(LAIRICE_ROOT, config$paths$expsites_dir)
  dot100_path <- file.path(LAIRICE_ROOT, config$paths$dot100_path)
  
  # Validate executables
  if (!file_exists(daycent_exe)) {
    log_message(paste("DayCent executable not found:", daycent_exe), "ERROR")
    return(NULL)
  }
  
  if (!file_exists(ddlist_exe)) {
    log_message(paste("DDList100 executable not found:", ddlist_exe), "ERROR")
    return(NULL)
  }
  
  # Validate directories
  if (!dir_exists(schedule_files_dir)) {
    log_message(paste("Schedule files directory not found:", schedule_files_dir), "ERROR")
    return(NULL)
  }
  
  if (!dir_exists(dot100_path)) {
    log_message(paste("dot100Files directory not found:", dot100_path), "ERROR")
    return(NULL)
  }
  
  log_message(paste("DayCent executable:", daycent_exe))
  log_message(paste("DDList100 executable:", ddlist_exe))
  log_message(paste("Schedule files directory:", schedule_files_dir))
  log_message(paste("dot100Files directory:", dot100_path))
  
  return(list(
    config = config,
    daycent_exe = daycent_exe,
    ddlist_exe = ddlist_exe,
    schedule_files_dir = schedule_files_dir,
    dot100_path = dot100_path
  ))
}


#' Run DayCent with DDList100 test
run_daycent_ddlist_test <- function() {
  log_message("Starting DayCent DDList100 test...")
  
  # Load configuration
  config_data <- load_soc_config()
  if (is.null(config_data)) {
    log_message("Failed to load SOC configuration", "ERROR")
    return(FALSE)
  }
  
  # Set up test paths
  site_dir <- file.path(config_data$schedule_files_dir, TEST_SITE)
  sch_file <- file.path(site_dir, paste0(TEST_TREATMENT, ".sch"))
  
  # Validate test site and treatment
  if (!dir_exists(site_dir)) {
    log_message(paste("Test site directory not found:", site_dir), "ERROR")
    return(FALSE)
  }
  
  if (!file_exists(sch_file)) {
    log_message(paste("Test schedule file not found:", sch_file), "ERROR")
    return(FALSE)
  }
  
  log_message(paste("Using test site:", TEST_SITE))
  log_message(paste("Using test treatment:", TEST_TREATMENT))
  log_message(paste("Schedule file:", sch_file))
  
  # Create unique test directory
  test_id <- paste0(TEST_SITE, "_", TEST_TREATMENT, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  test_dir <- file.path(DDLIST_TEST_DIR, test_id)
  dir_create(test_dir, recurse = TRUE)
  
  log_message(paste("Test directory:", test_dir))
  
  # Copy site files to test directory
  log_message("Copying site files...")
  site_files <- dir_ls(site_dir)
  for (src_file in site_files) {
    file_copy(src_file, file.path(test_dir, basename(src_file)), overwrite = TRUE)
  }
  log_message(paste("Copied", length(site_files), "site files"))
  
  # Copy all dot100 files
  log_message("Copying dot100Files...")
  dot100_files <- dir_ls(config_data$dot100_path)
  for (dot100_file in dot100_files) {
    file_copy(dot100_file, file.path(test_dir, basename(dot100_file)), overwrite = TRUE)
  }
  log_message(paste("Copied", length(dot100_files), "dot100 files"))
  
  # Change to test directory for execution
  original_wd <- getwd()
  setwd(test_dir)
  
  # Ensure cleanup on exit
  on.exit({
    setwd(original_wd)
  })
  
  result <- list(
    site = TEST_SITE,
    treatment = TEST_TREATMENT,
    daycent_success = FALSE,
    ddlist_success = FALSE,
    daycent_time = NULL,
    ddlist_time = NULL,
    daycent_output_files = 0,
    ddlist_output_files = 0,
    error_message = NULL
  )
  
  tryCatch({
    # Step 1: Run DayCent
    log_message("Step 1: Running DayCent...")
    start_time <- Sys.time()
    
    sch_filename <- paste0(TEST_TREATMENT, ".sch")
    daycent_result <- run_DayCent(
      filepath_exe = config_data$daycent_exe,
      sch_file = sch_filename
    )
    
    end_time <- Sys.time()
    result$daycent_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    if (daycent_result == 0) {
      result$daycent_success <- TRUE
      
      # Count DayCent output files
      daycent_output_files <- dir_ls(test_dir, regexp = "\\.(out|bin|lis)$")
      result$daycent_output_files <- length(daycent_output_files)
      
      log_message(paste("DayCent SUCCESS:", sprintf("%.1fs, %d output files", 
                       result$daycent_time, result$daycent_output_files)))
      
      # Step 2: Run DDList100
      log_message("Step 2: Running DDList100...")
      
      # Find .bin files created by DayCent
      bin_files <- dir_ls(test_dir, regexp = "\\.bin$")
      if (length(bin_files) > 0) {
        bin_file <- basename(bin_files[1])  # Use first .bin file
        bin_base <- tools::file_path_sans_ext(bin_file)
        lis_file <- paste0(bin_base, ".lis")  # Output .lis file (same base name)
        outvars_file <- file.path(config_data$dot100_path, "outvars.txt")  # Variables to output
        
        # Use the bayesiancalibr package run_ddlist100 function with local outvars.txt
        ddlist_result <- bayesiancalibr::run_ddlist100(
          ddlist_exe = config_data$ddlist_exe,
          bin_file = bin_file,
          lis_file = lis_file,
          outvars_file = "outvars.txt",  # Use local copy in test directory
          log_function = log_message
        )
        
        result$ddlist_success <- ddlist_result$success
        result$ddlist_time <- ddlist_result$execution_time
        
        if (ddlist_result$success) {
          # Count DDList100 output files
          ddlist_output_files <- dir_ls(test_dir, regexp = "\\.(lis|out|txt|csv)$")
          result$ddlist_output_files <- length(ddlist_output_files)
          
          # List all output files created
          log_message("All output files created:")
          all_outputs <- dir_ls(test_dir, regexp = "\\.(out|bin|lis|txt|csv)$")
          for (output_file in all_outputs) {
            log_message(paste("  -", basename(output_file)))
          }
          
        } else {
          result$error_message <- ddlist_result$error_message
          log_message(paste("DDList100 FAILED:", result$error_message), "ERROR")
        }
      } else {
        result$error_message <- "No .bin files found for DDList100 processing"
        log_message(result$error_message, "ERROR")
      }
      
    } else {
      result$error_message <- "DayCent execution failed"
      log_message(paste("DayCent FAILED: Exit code", daycent_result), "ERROR")
      
      # Try to get DayCent error details
      sch_base <- tools::file_path_sans_ext(sch_filename)
      stderr_file <- paste0(sch_base, "_stderr.log")
      if (file_exists(stderr_file)) {
        stderr_lines <- readLines(stderr_file, warn = FALSE)
        if (length(stderr_lines) > 0) {
          error_lines <- tail(stderr_lines, 3)
          result$error_message <- paste(result$error_message, 
                                      "Errors:", paste(error_lines, collapse = "; "))
        }
      }
    }
    
  }, error = function(e) {
    result$error_message <- paste("R execution error:", e$message)
    log_message(paste("CRITICAL ERROR:", result$error_message), "ERROR")
  })
  
  # Generate summary
  log_message("=== TEST SUMMARY ===")
  log_message(sprintf("Site: %s", result$site))
  log_message(sprintf("Treatment: %s", result$treatment))
  log_message(sprintf("DayCent Success: %s", result$daycent_success))
  log_message(sprintf("DDList100 Success: %s", result$ddlist_success))
  
  if (result$daycent_success) {
    log_message(sprintf("DayCent Time: %.1f seconds", result$daycent_time))
    log_message(sprintf("DayCent Output Files: %d", result$daycent_output_files))
  }
  
  if (result$ddlist_success) {
    log_message(sprintf("DDList100 Time: %.1f seconds", result$ddlist_time))
    log_message(sprintf("DDList100 Output Files: %d", result$ddlist_output_files))
  }
  
  if (!is.null(result$error_message)) {
    log_message(sprintf("Error: %s", result$error_message))
  }
  
  log_message(sprintf("Test Directory: %s", test_dir))
  log_message("Test files preserved for inspection")
  
  # Save results
  results_file <- file.path(DDLIST_TEST_DIR, paste0(test_id, "_results.rds"))
  saveRDS(result, results_file)
  log_message(paste("Results saved to:", results_file))
  
  # Return success status
  return(result$daycent_success && result$ddlist_success)
}

#' Main function
main <- function() {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) > 0 && args[1] == "--help") {
    cat("DayCent DDList100 Test Run\n")
    cat("Usage: Rscript daycent_run_with_ddlist.R\n")
    cat("\nThis script:\n")
    cat("1. Loads soil_organic_carbon.yaml configuration\n")
    cat("2. Tests DayCent execution on broadbalk_BF treatment\n")
    cat("3. Runs DDList100 to generate output files\n")
    cat("4. Saves all run files in /data/rubelscratch/rubelogle/daycent_calibration/scratch/ddlist\n")
    cat("\nOutput files are preserved for inspection.\n")
    return(invisible())
  }
  
  # Run the test
  success <- run_daycent_ddlist_test()
  
  if (success) {
    log_message("DayCent DDList100 test PASSED")
    quit(status = 0)
  } else {
    log_message("DayCent DDList100 test FAILED")
    quit(status = 1)
  }
}

# Run main function if script is executed directly
if (sys.nframe() == 0) {
  main()
}

# For interactive use, uncomment the line below:
# run_daycent_ddlist_test()