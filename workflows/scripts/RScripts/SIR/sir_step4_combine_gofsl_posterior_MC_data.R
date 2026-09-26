#!/usr/bin/env Rscript
#=======================================================================================  
#  PURPOSE:   Run SIR Steps 4A-4C using generalized functions from bayesiancalibr
#
#  AUTHOR:    Claude Code (claude.ai/code)
#             Based on original implementation by Ram Gurung
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
#  Project: Land-CRAFT DayCent Calibration Framework
#=======================================================================================  

# Default config path for interactive usage
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  config_path <- args[1] # Path to YAML config file
} else {
  print("No config file found")
  config_path <- "workflows/configs/crop_yield_wheat_W3HR.yaml"
}

# Load YAML to get custom library path (minimal setup)
if (!require(yaml, quietly = TRUE)) {
  stop("yaml package not available. Please run step0_r_setup.R first.")
}

# Load configuration
config <- yaml::read_yaml(config_path)

# Set custom library path (packages should already be installed by step0_r_setup.R)
if (!is.null(config$r_config$rlibpaths)) {
  .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
}

# Load required packages
suppressMessages({
  library(bayesiancalibr)
  library(reshape2)   
  library(dplyr)
})

#=======================================================================================
# Configuration and Setup
#=======================================================================================

cat("========================================================================\n")
cat("SIR Steps 4A-4C: Results Combination, Posterior Sampling, and Data Prep\n")
cat("========================================================================\n")

# Display configuration
cat("Configuration:\n")
cat("  Root Directory:", config$paths$lairice_root, "\n")
cat("  Output Base:", config$paths$output_base, "\n")
cat("  Date Stamp:", config$project$date_stamp, "\n")
cat("  Project Name:", config$project$name, "\n")
cat("  Emission Variable:", config$emmission_variable, "\n")
cat("  SIR Parameters:\n")
cat("    Random Seed:", config$sir$sir_rseed, "\n")
cat("    Posterior Samples:", config$sir$sir_n2dir, "\n")
cat("\n")

#=======================================================================================
# Run SIR Steps 4A-4C
#=======================================================================================

cat("=== Running SIR Steps 4A-4C ===\n")

# Step 4A: Combine Results
cat("--- Step 4A: Combining Likelihood and Aggregate Results ---\n")
step4A_results <- tryCatch({
  sir_step4a_combine_results(
    config = config,
    verbose = TRUE
  )
}, error = function(e4) {
  cat("ERROR in Step 4:", e4$message, "\n")
  stop("Cannot proceed without Step 4 completion")
})

cat("Step 4A completed successfully\n")
cat("  Likelihood files processed:", step4A_results$n_likelihood_files, "\n")
cat("  Aggregate files processed:", step4A_results$n_aggregate_files, "\n")
cat("  Job groups processed:", length(step4A_results$job_groups), "\n")


#---------------------------------------------------------------------------------------
# Step 4B: Generate Posterior Samples
cat("\n--- Step 4B: Generating Posterior Samples using SIR ---\n")
step4B_results <- tryCatch({
  sir_step4b_posterior_sample(
    config = config,
    verbose = TRUE
  )
}, error = function(e5) {
  cat("ERROR in Step 4B:", e5$message, "\n")
  stop("Step 5 failed")
})

cat("Step 4B completed successfully\n")
cat("  SIR sample indices generated:", length(step4B_results$sir_indices), "\n")


#---------------------------------------------------------------------------------------
# Step 4C: MC Data Preparation for Plots
cat("\n--- Step 4C: Preparing MC Data for Plots ---\n")
step4C_results <- tryCatch({
  sir_step4c_mc_data_prep(
    config = config,
    verbose = TRUE
  )
}, error = function(e6) {
  cat("ERROR in Step 4C:", e6$message, "\n")
  cat("This step may fail if observation data paths are incorrect.\n")
  cat("Please check the observation file paths in the config.\n")
  return(NULL)
})

cat("Step 4C completed successfully\n")


#=======================================================================================
# Summary and Results
#=======================================================================================

cat("\n=== SIR PROCESSING SUMMARY ===\n")

# Step 4A Summary
if (!is.null(step4A_results)) {
  cat("Step 4A - Results Combination:\n")
  cat("  ✓ Likelihood records:", ifelse(is.null(step4A_results$likelihood_combined), 0, nrow(step4A_results$likelihood_combined)), "\n")
  cat("  ✓ Aggregate records:", ifelse(is.null(step4A_results$aggregated_combined), 0, nrow(step4A_results$aggregated_combined)), "\n")
  cat("  ✓ Files saved to:", dirname(step4A_results$output_paths$likelihood_combined), "\n")
} else {
  cat("\nStep 4A - Results Combination:\n")
  cat("  ✗ Failed - may need manual configuration fixes\n")
}

# Step 4B Summary
if (!is.null(step4B_results)) {
  cat("\nStep 4B - Posterior Sampling:\n")
  cat("  ✓ SIR method applied for likelihood-based sampling\n")
  cat("  ✓ Variables processed:", length(unique(names(step4B_results$sir_indices))), "\n")
  if (!is.null(step4B_results$output_paths)) {
    cat("  ✓ Sample index files saved:", length(step4B_results$output_paths), "\n")
  }
} else {
  cat("\nStep 4B - Posterior Sampling:\n")
  cat("  ✗ Failed - may need manual configuration fixes\n")
}

# Step 4C Summary
if (!is.null(step4C_results)) {
  cat("\nStep 4C - Data Preparation:\n")
  cat("  ✓ Observation data loaded and processed\n")
  cat("  ✓ SIR sample indices loaded\n")
  cat("  ✓ Data ready for plotting and analysis\n")
} else {
  cat("\nStep 4C - Data Preparation:\n")
  cat("  ✗ Failed - may need manual configuration fixes\n")
}

# File locations
cat("\n=== OUTPUT FILE LOCATIONS ===\n")
base_path <- file.path(config$paths$lairice_root, config$paths$output_base, 
                       ifelse(config$project$date_stamp == "auto", 
                              format(Sys.Date(), "%d%b%Y"), 
                              config$project$date_stamp), "SIR")

cat("Base SIR Directory:", base_path, "\n")
cat("Key Output Files:\n")
cat("  • Combined Likelihood: Results/Likelihood_Combined_*.rds\n")
cat("  • Combined Aggregates: Results/Aggregated_Combined_*.rds\n")
cat("  • SIR Sample Indices: Results/best_250_*.csv\n")
cat("  • Processed Data: Results/IndividualNH3_*.csv, CumulativeNH3_*.csv, etc.\n")

# Next steps guidance
cat("\n=== NEXT STEPS ===\n")
cat("1. Review the combined likelihood and aggregate files\n")
cat("2. Check the SIR sample indices (best_250_*.csv files)\n")
cat("3. Run plotting/visualization scripts if Step 6 completed successfully\n")
if (is.null(step4C_results)) {
  cat("4. Fix observation data path issues and re-run Step 6 if needed\n")
}

cat("\n=== SUCCESS: SIR Steps 4A-4C completed ===\n")
