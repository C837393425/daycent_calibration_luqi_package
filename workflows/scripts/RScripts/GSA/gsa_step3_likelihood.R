#!/usr/bin/env Rscript
#=======================================================================================  
#  PURPOSE:   GSA Step 3: Likelihood Calculation - Generic CLI Interface
#
#  DESCRIPTION: Command-line interface for the generic GSA Step 3 likelihood calculation
#               function. Processes one parameter set (task_id) and calculates goodness-of-fit
#               metrics by comparing model outputs with observations.
#
#  USAGE:     Rscript gsa_step3_likelihood.R <config.yaml> <gsa_method> <task_id> [options]
#
#  ARGUMENTS:
#    config.yaml  - Path to YAML configuration file
#    gsa_method   - GSA method name (e.g., "soboljansen", "sobol")
#    task_id      - Task ID for the parameter set to process
#    
#  OPTIONS:
#    --date-stamp <stamp>  - Override date stamp from config
#    --start-id <id>       - Start ID offset (default: 0)
#    --verbose             - Enable verbose output
#    --help                - Show this help message
#
#  EXAMPLES:
#    Rscript gsa_step3_likelihood.R workflows/configs/nh3_volatilization.yaml soboljansen 1
#    Rscript gsa_step3_likelihood.R workflows/configs/nh3_volatilization.yaml sobol 5 --verbose
#
#  AUTHOR:    Claude Code (claude.ai/code)
#             Based on original implementation by Ram Gurung
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
#  Project: Land-CRAFT DayCent Calibration Framework
#=======================================================================================

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Get config path for library setup
if (length(args) >= 1) {
    config_path <- args[1]
} else {
    stop("Usage: Rscript gsa_step3_likelihood.R <config.yaml> <gsa_method> <task_id> [options]")
}

# Load YAML to get custom library path (minimal setup)
if (!require(yaml, quietly = TRUE)) {
    stop("yaml package not available. Please run step0_r_setup.R first.")
}
config <- yaml::read_yaml(config_path)

# Set custom library path (packages should already be installed by step0_r_setup.R)
if (!is.null(config$r_config$rlibpaths)) {
    .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
}

# Load required packages (assuming they are already installed)
suppressPackageStartupMessages({
  library(bayesiancalibr)
})

# Help function
show_help <- function() {
  cat("GSA Step 3: Likelihood Calculation - Generic CLI Interface\n\n")
  cat("USAGE:\n")
  cat("  Rscript gsa_step3_likelihood.R <config.yaml> <gsa_method> <task_id> [options]\n\n")
  cat("ARGUMENTS:\n")
  cat("  config.yaml  - Path to YAML configuration file\n")
  cat("  gsa_method   - GSA method name (e.g., 'soboljansen', 'sobol')\n")
  cat("  task_id      - Task ID for the parameter set to process\n\n")
  cat("OPTIONS:\n")
  cat("  --date-stamp <stamp>  - Override date stamp from config\n")
  cat("  --start-id <id>       - Start ID offset (default: 0)\n")
  cat("  --verbose             - Enable verbose output\n")
  cat("  --help                - Show this help message\n\n")
  cat("EXAMPLES:\n")
  cat("  Rscript gsa_step3_likelihood.R workflows/configs/soil_organic_carbon.yaml soboljansen 1\n")
  cat("  Rscript gsa_step3_likelihood.R workflows/configs/soil_organic_carbon.yaml sobol 5 --verbose\n\n")
  quit(status = 0)
}

# Check for help request
if (length(args) == 0 || "--help" %in% args || "-h" %in% args) {
  show_help()
}

# Validate required arguments
if (length(args) < 3) {
  cat("ERROR: Missing required arguments\n")
  cat("Run with --help for usage information\n")
  quit(status = 1)
}

# Parse required arguments
config_file <- args[1]
gsa_method <- args[2]
task_id <- as.numeric(args[3])

# Parse optional arguments
date_stamp <- NULL
start_id <- 0
verbose <- FALSE

i <- 4
while (i <= length(args)) {
  arg <- args[i]
  
  if (arg == "--date-stamp") {
    if (i + 1 <= length(args)) {
      date_stamp <- args[i + 1]
      i <- i + 2
    } else {
      cat("ERROR: --date-stamp requires a value\n")
      quit(status = 1)
    }
  } else if (arg == "--start-id") {
    if (i + 1 <= length(args)) {
      start_id <- as.numeric(args[i + 1])
      i <- i + 2
    } else {
      cat("ERROR: --start-id requires a value\n")
      quit(status = 1)
    }
  } else if (arg == "--verbose") {
    verbose <- TRUE
    i <- i + 1
  } else {
    cat("ERROR: Unknown option:", arg, "\n")
    quit(status = 1)
  }
}

# Adjust task_id with start_id offset
task_id <- start_id + task_id

# Validate configuration file exists
if (!file.exists(config_file)) {
  cat("ERROR: Configuration file not found:", config_file, "\n")
  quit(status = 1)
}

# Initialize and print header
if (verbose) {
  cat("====================================================================\n")
  cat("GSA Step 3: Likelihood Calculation - Generic CLI Interface\n")
  cat("====================================================================\n")
  cat("Working Directory:", getwd(), "\n")
  cat("Configuration file:", config_file, "\n")
  cat("GSA method:", gsa_method, "\n")
  cat("Task ID:", task_id, "\n")
  cat("Date stamp:", if (is.null(date_stamp)) "from config" else date_stamp, "\n")
  cat("Verbose mode:", verbose, "\n")
  cat("====================================================================\n\n")
}

# Load configuration using the same approach as Steps 1 and 2
tryCatch({
  config <- read_yaml_config(config_file)
  config <- resolve_config_paths(config)
  
  if (verbose) {
    cat("Configuration loaded successfully\n")
    cat("Project:", config$project$name, "\n")
    cat("Root directory:", config$paths$lairice_root, "\n")
  }
}, error = function(e) {
  cat("ERROR: Failed to load configuration file:", e$message, "\n")
  quit(status = 1)
})

# Validate GSA method
if (!gsa_method %in% config$gsa$gsa_methods) {
  cat("ERROR: Invalid GSA method:", gsa_method, "\n")
  cat("Valid methods:", paste(config$gsa$gsa_methods, collapse = ", "), "\n")
  quit(status = 1)
}

# Execute GSA Step 3 likelihood calculation
tryCatch({
  result <- gsa_step3_likelihood(
    config = config,
    gsa_method = gsa_method,
    task_id = task_id,
    date_stamp = date_stamp,
    verbose = verbose
  )
  
  if (result$validation) {
    if (verbose) {
      cat("\n====================================================================\n")
      cat("GSA Step 3 completed successfully!\n")
      cat("Likelihood file:", result$output_files$likelihood, "\n")
      cat("Runtime file:", result$output_files$runtime, "\n")
      cat("Runtime:", round(result$runtime_info$Time_sec, 2), "seconds\n")
      cat("====================================================================\n")
    }
    
    # Success output for SLURM monitoring
    cat("Success\n")
    quit(status = 0)
    
  } else {
    cat("ERROR: Likelihood calculation failed\n")
    if (!is.null(result$error)) {
      cat("Error message:", result$error, "\n")
    }
    quit(status = 1)
  }
  
}, error = function(e) {
  cat("ERROR: GSA Step 3 failed:", e$message, "\n")
  if (verbose) {
    cat("Call stack:\n")
    print(sys.calls())
  }
  quit(status = 1)
})

# This should never be reached
cat("ERROR: Unexpected end of script\n")
quit(status = 1)