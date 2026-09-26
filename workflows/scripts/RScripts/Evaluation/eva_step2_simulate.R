#!/usr/bin/env Rscript
#=======================================================================================
# Evaluation Step 2 - Individual Simulation Wrapper Script
#
# Runs DayCent for ONE posterior sample on evaluation (held-out) sites.
# Parameter values come from SIR/Results/best_param_set_{model}.csv
# (SampleID + calibrated parameter columns from SIR Step 4.5).
# Site files follow file_source.mode (filesystem or database). Results follow
# database_result.run_type (file_system or database).
#
# Requires Evaluation Step 1 (eva_step1_setup.R) to have created RunFile.rds.
# Usage: Rscript eva_step2_simulate.R <config_path> <sim_id> [daycent_exe] [scratch_dir]
#
# Arguments:
#   config_path  - Path to YAML configuration file
#   sim_id       - 1-based row index into best_param_set_{model}.csv
#   daycent_exe  - Optional: Path to DayCent executable (defaults to config)
#   scratch_dir  - Optional: Scratch directory path (defaults to config scratch_dir)
#
# Optional flag:
#   --partition-id N  - Scaling mode: run only sites assigned to partition N
#
# Author: DayCent Calibration Framework
# Date: August 2026
#=======================================================================================

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
    stop("Usage: Rscript eva_step2_simulate.R <config_path> <sim_id> [daycent_exe] [scratch_dir]")
}

config_path <- args[1]
sim_id <- as.numeric(args[2])

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

cat("Running Evaluation Step 2 with arguments:\n")
cat("Config file:", config_path, "\n")
cat("Simulation ID (posterior index):", sim_id, "\n")

# Parse optional --partition-id argument for scaling mode
partition_id_override <- NULL
if ("--partition-id" %in% args) {
  partition_id_idx <- which(args == "--partition-id")
  if (length(args) > partition_id_idx) {
    partition_id_override <- as.integer(args[partition_id_idx + 1])
    cat("Partition ID override:", partition_id_override, "(Scaling mode partition filtering)\n")
  }
}

# Optional positional arguments (skip flag tokens)
positional <- args[!grepl("^--", args)]
daycent_exe <- if (length(positional) >= 3) positional[3] else NULL
scratch_dir <- if (length(positional) >= 4) positional[4] else NULL

# Validate inputs
if (!file.exists(config_path)) {
  cat("ERROR: Configuration file not found:", config_path, "\n")
  quit(status = 1)
}

if (is.na(sim_id) || sim_id < 1) {
  cat("ERROR: sim_id must be a positive integer, got:", sim_id, "\n")
  quit(status = 1)
}

# Load configuration
cat("=== Individual Evaluation Step 2 Simulation ===\n")
cat("Loading configuration from:", config_path, "\n")

config <- tryCatch({
  read_yaml_config(config_path)
}, error = function(e) {
  cat("ERROR: Failed to load configuration:", e$message, "\n")
  quit(status = 1)
})

# Remap Evaluation schedule + .100 dirs onto the paths DayCent helpers read.
# Runs in the wrapper so it still works if rlib has an older package.
bind_evaluation_data_paths <- function(config) {
  pick_abs <- function(...) {
    for (p in list(...)) {
      if (!is.null(p) && is.character(p) && length(p) >= 1 && nzchar(p[[1]])) {
        p <- p[[1]]
        if (!grepl("^/", p) && !grepl("^[A-Za-z]:[/\\\\]", p)) {
          p <- file.path(config$paths$lairice_root, p)
        }
        return(p)
      }
    }
    NULL
  }
  eva <- config$evaluation
  if (is.null(eva)) eva <- list()

  sites_dir <- pick_abs(eva$evasites_dir, eva$expsites_dir, config$paths$evasites_dir)
  if (!is.null(sites_dir)) {
    config$paths$expsites_dir <- sites_dir
    config$paths$evasites_dir <- sites_dir
    cat("Using evaluation schedule directory:", sites_dir, "\n")
  }

  dot100_dir <- pick_abs(eva$evadot100_path, eva$dot100_path, config$paths$evadot100_path)
  if (!is.null(dot100_dir)) {
    config$paths$dot100_path <- dot100_dir
    config$paths$evadot100_path <- dot100_dir
    cat("Using evaluation dot100 directory:", dot100_dir, "\n")
  }
  config
}

# Resolve paths
config <- resolve_config_paths(config)
config <- bind_evaluation_data_paths(config)

# Set up DayCent executable path
if (is.null(daycent_exe)) {
  if ("daycent" %in% names(config) && "executable" %in% names(config$daycent)) {
    daycent_exe <- file.path(config$paths$lairice_root, config$daycent$executable)
  } else {
    cat("ERROR: DayCent executable not specified in config or command line\n")
    quit(status = 1)
  }
}

# Set up scratch directory
if (is.null(scratch_dir)) {
  if ("cluster" %in% names(config) && "scratch_dir" %in% names(config$cluster)) {
    scratch_dir <- file.path(config$cluster$scratch_dir, paste0("daycent_eva_", Sys.getpid()))
  } else {
    scratch_dir <- paste0("/tmp/daycent_eva_", Sys.getpid())
  }
}

# Display run information
cat("Project:", config$project$name, "\n")
cat("Simulation ID:", sim_id, "\n")
cat("File source mode:", if (!is.null(config$file_source$mode)) config$file_source$mode else "filesystem", "\n")
cat("DayCent executable:", daycent_exe, "\n")
cat("Scratch directory:", scratch_dir, "\n")
cat("Configuration validated successfully\n")

# Record start time
start_time <- Sys.time()
cat("Starting simulation at:", format(start_time), "\n")

# Estimate database operations for connection pooling (input DB, not result DB)
run_file_path <- file.path(
  eva_output_dir(config),
  "RunFile.rds"
)
expected_operations <- 1

if (file.exists(run_file_path)) {
  tryCatch({
    run_file <- readRDS(run_file_path)
    n_sites <- length(unique(run_file$siteID))
    expected_operations <- n_sites * 5
    cat("Estimated database operations:", expected_operations, "(", n_sites, "sites x 5 ops/site)\n")
  }, error = function(e) {
    cat("Could not load evaluation RunFile, using default expected_operations =", expected_operations, "\n")
  })
} else {
  cat("Evaluation RunFile not found yet, using default expected_operations =", expected_operations, "\n")
}

pooling_strategy <- determine_pooling_strategy(config, expected_operations = expected_operations, log_function = cat)
shared_con <- NULL

if (pooling_strategy$use_pooling) {
  cat("Setting up shared database connection...\n")
  shared_con <- get_shared_connection(config, log_function = cat)
  if (is.null(shared_con)) {
    cat("Warning: Failed to create shared connection, falling back to individual connections\n")
  }
}

clean_scratch <- TRUE
if ("cluster" %in% names(config) && "clean_scratch" %in% names(config$cluster)) {
  clean_scratch <- isTRUE(config$cluster$clean_scratch)
}

# Run individual simulation
cat("\n=== Running Individual Evaluation Step 2 Simulation ===\n")
result <- tryCatch({
  eva_step2_simulate_individual(
    config = config,
    sim_id = sim_id,
    daycent_exe = daycent_exe,
    scratch_dir = scratch_dir,
    clean_scratch = clean_scratch,
    start_id = 0,
    partition_id_override = partition_id_override,
    shared_connection = shared_con,
    verbose = TRUE
  )
}, error = function(e) {
  cat("ERROR: Individual Evaluation Step 2 simulation failed:", e$message, "\n")
  if (!is.null(shared_con)) {
    close_shared_connection(config, log_function = cat)
  }
  quit(status = 1)
})

# Record end time and duration
end_time <- Sys.time()
duration <- end_time - start_time
cat("\n=== Simulation Complete ===\n")
cat("End time:", format(end_time), "\n")
cat("Duration:", format(duration), "\n")

# Display results summary
if (result$status == 0) {
  cat("Status: SUCCESS\n")
  cat("Evaluation sim ID processed:", result$sim_id, "\n")
  if (!is.null(result$original_sample_id) && !is.na(result$original_sample_id)) {
    cat("Original SIR SampleID:", result$original_sample_id, "\n")
  }

  if (!is.null(result$daily_results)) {
    cat("Daily results shape:", nrow(result$daily_results), "rows x", ncol(result$daily_results), "columns\n")
  }

  if (!is.null(result$annual_results)) {
    cat("Annual results shape:", nrow(result$annual_results), "rows x", ncol(result$annual_results), "columns\n")
  }

  if (!is.null(result$aggregated_results)) {
    cat("Aggregated results shape:", nrow(result$aggregated_results), "rows x", ncol(result$aggregated_results), "columns\n")
  }

  if (!is.null(result$runtime_info)) {
    cat("Runtime information: 1 simulation record\n")
    cat("  Sites processed:", result$runtime_info$num_trts, "\n")
    cat("  Simulation years:", result$runtime_info$sim_years, "\n")
    cat("  Execution time:", result$runtime_info$Time_sec, "seconds\n")
  }

  if (!is.null(result$output_files) && length(result$output_files) > 0) {
    cat("Output files created:\n")
    for (i in seq_along(result$output_files)) {
      cat("  ", names(result$output_files)[i], ":", result$output_files[[i]], "\n")
    }
  }

  cat("\nIndividual Evaluation Step 2 simulation completed successfully!\n")

  if (!is.null(shared_con)) {
    close_shared_connection(config, log_function = cat)
  }

  quit(status = 0)

} else {
  cat("Status: FAILED\n")
  cat("Individual Evaluation Step 2 simulation failed with status code:", result$status, "\n")
  if (!is.null(result$runtime_info)) {
    cat("Error message:", result$runtime_info$message, "\n")
  }

  if (!is.null(shared_con)) {
    close_shared_connection(config, log_function = cat)
  }

  quit(status = 1)
}
