#!/usr/bin/env Rscript
#' @title Flexible DayCent Runner with Scratch Folder Management
#' @description Run DayCent simulations for specified sites/counties with parameter sampling
#' @details This script provides flexible execution of DayCent runs with options to:
#'   - Run specific NRI points or entire counties
#'   - Sample random subset of points from a county
#'   - Specify parameter set (sample ID) to use
#'   - Configure scratch folder location
#'   - Leverage all bayesiancalibr package functions
#'
#' @usage
#' Rscript run_daycent_flexible.R --config <config.yaml> --scratch <scratch_dir> [OPTIONS]
#'
#' @examples
#' # Run all NRI points in county 6001
#' Rscript run_daycent_flexible.R --config configs/crop_yield_corn_m2.yaml --county 6001
#'
#' # Run 5 random NRI points from county 6001
#' Rscript run_daycent_flexible.R --config configs/crop_yield_corn_m2.yaml --county 6001 --n-points 5
#'
#' # Run specific NRI points
#' Rscript run_daycent_flexible.R --config configs/crop_yield_corn_m2.yaml --sites NRI_001,NRI_002,NRI_003
#'
#' # Run with specific parameter set (sample ID)
#' Rscript run_daycent_flexible.R --config configs/crop_yield_corn_m2.yaml --county 6001 --sample-id 42
#'
#' # Custom scratch folder location
#' Rscript run_daycent_flexible.R --config configs/crop_yield_corn_m2.yaml --county 6001 --scratch /tmp/my_scratch

# =============================================================================
# SETUP AND LIBRARY LOADING
# =============================================================================

# Check for required packages and install if needed
check_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Package", pkg, "not found. Attempting to install...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Suppress warnings during library loading
suppressPackageStartupMessages({
  check_and_load("optparse")
  check_and_load("yaml")
})

# =============================================================================
# COMMAND LINE ARGUMENT PARSING
# =============================================================================

option_list <- list(
  make_option(c("-c", "--config"), type = "character", default = NULL,
              help = "Path to YAML configuration file (REQUIRED)", metavar = "FILE"),

  make_option(c("--county"), type = "character", default = NULL,
              help = "County number (aggregation_level) to run", metavar = "COUNTY"),

  make_option(c("--sites"), type = "character", default = NULL,
              help = "Comma-separated list of site IDs (e.g., 'NRI_001,NRI_002')", metavar = "SITES"),

  make_option(c("--n-points"), type = "integer", default = NULL,
              help = "Number of random points to select from county (requires --county)", metavar = "N"),

  make_option(c("--sample-id"), type = "integer", default = 1,
              help = "Parameter sample ID to use (default: 1)", metavar = "ID"),

  make_option(c("--scratch"), type = "character", default = NULL,
              help = "Scratch folder location (default: from config or /scratch/daycent_calibration)",
              metavar = "DIR"),

  make_option(c("--keep-files"), action = "store_true", default = FALSE,
              help = "Keep all simulation files in scratch folder after completion"),

  make_option(c("--verbose"), action = "store_true", default = FALSE,
              help = "Print detailed progress messages"),

  make_option(c("--seed"), type = "integer", default = NULL,
              help = "Random seed for point selection (default: system time)")
)

opt_parser <- OptionParser(
  usage = "Usage: %prog --config <config.yaml> [--county COUNTY | --sites SITES] [OPTIONS]",
  option_list = option_list,
  description = "\nFlexible DayCent runner with scratch folder management.\n\nExamples:\n  Run all points in county:     %prog --config config.yaml --county 6001\n  Run 5 random points:          %prog --config config.yaml --county 6001 --n-points 5\n  Run specific sites:           %prog --config config.yaml --sites NRI_001,NRI_002\n  Use parameter set 42:         %prog --config config.yaml --county 6001 --sample-id 42"
)

opt <- parse_args(opt_parser)

# =============================================================================
# VALIDATE ARGUMENTS
# =============================================================================

# Check required arguments
if (is.null(opt$config)) {
  print_help(opt_parser)
  stop("ERROR: --config is required", call. = FALSE)
}

if (is.null(opt$county) && is.null(opt$sites)) {
  print_help(opt_parser)
  stop("ERROR: Must specify either --county or --sites", call. = FALSE)
}

if (!is.null(opt$county) && !is.null(opt$sites)) {
  stop("ERROR: Cannot specify both --county and --sites", call. = FALSE)
}

if (!is.null(opt$`n-points`) && is.null(opt$county)) {
  stop("ERROR: --n-points requires --county to be specified", call. = FALSE)
}

# Check config file exists
if (!file.exists(opt$config)) {
  stop("ERROR: Configuration file not found: ", opt$config, call. = FALSE)
}

# =============================================================================
# LOAD CONFIGURATION
# =============================================================================

cat("Loading configuration from:", opt$config, "\n")

# Read and resolve configuration
suppressPackageStartupMessages({
  # Set custom library path if specified in config
  config_raw <- yaml::read_yaml(opt$config)
  if (!is.null(config_raw$r_config$rlibpaths)) {
    .libPaths(c(config_raw$r_config$rlibpaths, .libPaths()))
  }

  # Load bayesiancalibr package
  library(bayesiancalibr)
})

# Load and resolve config paths
config <- read_yaml_config(opt$config)
config <- resolve_config_paths(config)

if (opt$verbose) {
  cat("Configuration loaded successfully\n")
  cat("  Project:", config$project$name, "\n")
  cat("  Root directory:", config$paths$lairice_root, "\n")
}

# =============================================================================
# SETUP SCRATCH FOLDER
# =============================================================================

# Determine scratch folder location
if (!is.null(opt$scratch)) {
  scratch_dir <- opt$scratch
} else if (!is.null(config$cluster$scratch_dir)) {
  scratch_dir <- config$cluster$scratch_dir
} else {
  scratch_dir <- "/scratch/daycent_calibration"
}

# Create scratch directory with timestamp
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
scratch_run_dir <- file.path(scratch_dir, paste0("run_", run_timestamp))

if (!dir.exists(scratch_run_dir)) {
  dir.create(scratch_run_dir, recursive = TRUE)
  cat("Created scratch directory:", scratch_run_dir, "\n")
} else {
  cat("Using scratch directory:", scratch_run_dir, "\n")
}

# =============================================================================
# LOAD PARAMETERS
# =============================================================================

cat("\nLoading parameter set (Sample ID:", opt$`sample-id`, ")\n")

# Determine which parameter file to use (GSA or SIR)
# Try to auto-detect by checking which files exist
sir_file_found <- FALSE
gsa_file_found <- FALSE

# Check for SIR file first (try both naming conventions)
if (!is.null(config$sir)) {
  sir_mc_file <- file.path(
    config$paths$lairice_root,
    config$paths$output_base,
    config$project$date_stamp,
    "SIR",
    "mc_SIR_draw.rds"
  )

  if (!file.exists(sir_mc_file)) {
    sir_mc_file <- file.path(
      config$paths$lairice_root,
      config$paths$output_base,
      config$project$date_stamp,
      "SIR",
      "MC_draws.rds"
    )
  }

  if (file.exists(sir_mc_file)) {
    sir_file_found <- TRUE
  }
}

# Check for GSA file
if (!is.null(config$gsa)) {
  gsa_method <- "soboljansen"  # Default method
  if (!is.null(config$gsa$gsa_methods) && length(config$gsa$gsa_methods) > 0) {
    gsa_method <- config$gsa$gsa_methods[1]
  }

  gsa_mc_file <- file.path(
    config$paths$lairice_root,
    config$paths$output_base,
    config$project$date_stamp,
    "GSA",
    gsa_method,
    "MC_draws.rds"
  )

  if (file.exists(gsa_mc_file)) {
    gsa_file_found <- TRUE
  }
}

# Determine which mode to use based on what files exist
# Prefer SIR if both exist
use_sir_mode <- sir_file_found
use_gsa_mode <- gsa_file_found && !sir_file_found

if (use_sir_mode) {
  # SIR mode
  if (opt$verbose) {
    cat("Using SIR mode (file found:", sir_mc_file, ")\n")
  }

  mc_file <- sir_mc_file

  if (file.exists(mc_file)) {
    mc_draws <- readRDS(mc_file)

    # Extract the sample we want
    if (opt$`sample-id` > nrow(mc_draws)) {
      stop("ERROR: Sample ID ", opt$`sample-id`, " exceeds available samples (",
           nrow(mc_draws), ")", call. = FALSE)
    }

    job_params <- mc_draws[opt$`sample-id`, , drop = FALSE]

    # Prepare parameters using SIR-specific function
    params_df <- sir_step2_prepare_parameters(config, job_params, verbose = opt$verbose)

    if (opt$verbose) {
      cat("Loaded SIR parameters from:", sir_mc_file, "\n")
      cat("  Total samples available:", nrow(mc_draws), "\n")
      cat("  Parameters loaded:", nrow(params_df), "\n")
    }
  } else {
    cat("WARNING: SIR MC_draws file not found\n")

    # Load default parameters if no MC draws available
    if ("default_params" %in% names(config$input_files)) {
      default_file <- config$input_files$default_params
      if (file.exists(default_file)) {
        dflt <- read.csv(default_file, stringsAsFactors = FALSE)
        params_df <- dflt[, c("File", "Parameter", "Default")]
        names(params_df)[which(names(params_df) == "Default")] <- "value"
        if (opt$verbose) {
          cat("Loaded", nrow(params_df), "default parameters from:", default_file, "\n")
        }
      } else {
        params_df <- NULL
      }
    } else {
      params_df <- NULL
    }
  }

} else if (use_gsa_mode) {
  # GSA mode
  if (opt$verbose) {
    cat("Using GSA mode (file found:", gsa_mc_file, ")\n")
  }

  mc_file <- gsa_mc_file

  if (file.exists(mc_file)) {
    mc_draws <- readRDS(mc_file)

    # Extract the sample we want
    if (opt$`sample-id` > nrow(mc_draws)) {
      stop("ERROR: Sample ID ", opt$`sample-id`, " exceeds available samples (",
           nrow(mc_draws), ")", call. = FALSE)
    }

    job_params <- mc_draws[opt$`sample-id`, , drop = FALSE]

    # Prepare parameters using package function
    params_df <- prepare_parameter_set(config, job_params, verbose = opt$verbose)

    if (opt$verbose) {
      cat("Loaded GSA parameters from:", gsa_mc_file, "\n")
      cat("  Total samples available:", nrow(mc_draws), "\n")
      cat("  Parameters loaded:", nrow(params_df), "\n")
    }
  } else {
    cat("WARNING: GSA MC_draws.rds not found\n")

    # Load default parameters if no MC draws available
    if ("default_params" %in% names(config$input_files)) {
      default_file <- config$input_files$default_params
      if (file.exists(default_file)) {
        dflt <- read.csv(default_file, stringsAsFactors = FALSE)
        params_df <- dflt[, c("File", "Parameter", "Default")]
        names(params_df)[which(names(params_df) == "Default")] <- "value"
        if (opt$verbose) {
          cat("Loaded", nrow(params_df), "default parameters from:", default_file, "\n")
        }
      } else {
        params_df <- NULL
      }
    } else {
      params_df <- NULL
    }
  }

} else {
  cat("WARNING: Neither GSA nor SIR configuration found, using default parameters\n")

  # Load default parameters
  if ("default_params" %in% names(config$input_files)) {
    default_file <- config$input_files$default_params
    if (file.exists(default_file)) {
      dflt <- read.csv(default_file, stringsAsFactors = FALSE)
      params_df <- dflt[, c("File", "Parameter", "Default")]
      names(params_df)[which(names(params_df) == "Default")] <- "value"
      if (opt$verbose) {
        cat("Loaded", nrow(params_df), "default parameters from:", default_file, "\n")
      }
    } else {
      params_df <- NULL
    }
  } else {
    params_df <- NULL
  }
}

# =============================================================================
# GET RUN ORDER AND FILTER SITES
# =============================================================================

cat("\nRetrieving run order from database...\n")

# Establish database connection if needed
shared_connection <- get_shared_connection(config)

# Get full run order
run_order <- get_run_order_from_database(
  config = config,
  task_id = NULL,  # Get all runs
  shared_connection = shared_connection,
  log_function = if(opt$verbose) cat else function(...) NULL
)

if (is.null(run_order) || nrow(run_order) == 0) {
  close_shared_connection(shared_connection)
  stop("ERROR: Failed to retrieve run order from database", call. = FALSE)
}

# Filter sites based on user input
if (!is.null(opt$county)) {
  # Filter by county (aggregation_level)
  county_sites <- run_order[run_order$aggregation_level == opt$county, ]

  if (nrow(county_sites) == 0) {
    close_shared_connection(shared_connection)
    stop("ERROR: No sites found for county: ", opt$county, call. = FALSE)
  }

  cat("Found", nrow(county_sites), "NRI points in county", opt$county, "\n")

  # Sample random subset if requested
  if (!is.null(opt$`n-points`)) {
    if (opt$`n-points` > nrow(county_sites)) {
      cat("WARNING: Requested", opt$`n-points`, "points but only",
          nrow(county_sites), "available\n")
      selected_sites <- county_sites
    } else {
      # Set seed for reproducibility
      if (!is.null(opt$seed)) {
        set.seed(opt$seed)
        cat("Using random seed:", opt$seed, "\n")
      }

      sample_idx <- sample(nrow(county_sites), opt$`n-points`)
      selected_sites <- county_sites[sample_idx, ]
      cat("Randomly selected", opt$`n-points`, "points\n")
    }
  } else {
    selected_sites <- county_sites
  }

} else {
  # Filter by specific site IDs
  site_list <- strsplit(opt$sites, ",")[[1]]
  site_list <- trimws(site_list)  # Remove whitespace

  selected_sites <- run_order[run_order$siteID %in% site_list, ]

  if (nrow(selected_sites) == 0) {
    close_shared_connection(shared_connection)
    stop("ERROR: No matching sites found in run order", call. = FALSE)
  }

  # Check for missing sites
  missing_sites <- setdiff(site_list, selected_sites$siteID)
  if (length(missing_sites) > 0) {
    cat("WARNING: Sites not found in run order:", paste(missing_sites, collapse = ", "), "\n")
  }

  cat("Selected", nrow(selected_sites), "sites\n")
}

if (opt$verbose) {
  cat("\nSites to run:\n")
  print(selected_sites[, c("siteID", "aggregation_level")])
}

# =============================================================================
# RUN DAYCENT SIMULATIONS
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("STARTING DAYCENT SIMULATIONS\n")
cat(strrep("=", 70), "\n\n")

# Get DayCent executable path
daycent_exe <- file.path(config$paths$lairice_root, config$daycent$executable)
if (!file.exists(daycent_exe)) {
  close_shared_connection(shared_connection)
  stop("ERROR: DayCent executable not found: ", daycent_exe, call. = FALSE)
}

# Load output specifications
output_specs <- load_output_specifications(config, verbose = opt$verbose)

# Initialize results storage
all_results <- list()

# Loop through each site
for (i in seq_len(nrow(selected_sites))) {
  site_id <- selected_sites$siteID[i]

  cat("\n", strrep("-", 70), "\n")
  cat("Site", i, "of", nrow(selected_sites), ":", site_id, "\n")
  cat(strrep("-", 70), "\n")

  # Create site-specific scratch directory
  site_scratch_dir <- file.path(scratch_run_dir, site_id)
  dir.create(site_scratch_dir, recursive = TRUE, showWarnings = FALSE)

  # Run site simulations using package function
  tryCatch({
    result <- run_site_simulations_generic(
      site_id = site_id,
      run_file_site = selected_sites[i, , drop = FALSE],
      config = config,
      params_df = params_df,
      sim_dir_tid = site_scratch_dir,
      daycent_exe = daycent_exe,
      actual_task_id = opt$`sample-id`,
      agg_vars = output_specs$aggregated_variables,
      shared_connection = shared_connection,
      verbose = opt$verbose
    )

    all_results[[site_id]] <- result

    cat("✓ Site", site_id, "completed successfully\n")
    cat("  Simulations:", result$num_sites, "sites,",
        result$num_treatments, "treatments,",
        result$num_sim_years, "years\n")

  }, error = function(e) {
    cat("✗ Site", site_id, "FAILED:", conditionMessage(e), "\n")
    all_results[[site_id]] <- list(error = conditionMessage(e))
  })
}

# =============================================================================
# CLEANUP AND SUMMARY
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SIMULATION SUMMARY\n")
cat(strrep("=", 70), "\n\n")

# Count successes and failures
n_success <- sum(sapply(all_results, function(x) {
  if (is.list(x) && "error" %in% names(x)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}))
n_failed <- length(all_results) - n_success

cat("Total sites attempted:", length(all_results), "\n")
cat("Successful runs:", n_success, "\n")
cat("Failed runs:", n_failed, "\n")

if (n_failed > 0) {
  cat("\nFailed sites:\n")
  failed_sites <- names(all_results)[sapply(all_results, function(x) {
    is.list(x) && "error" %in% names(x)
  })]
  for (site in failed_sites) {
    cat("  -", site, ":", all_results[[site]]$error, "\n")
  }
}

# Save results summary
results_file <- file.path(scratch_run_dir, "simulation_results.rds")
saveRDS(all_results, results_file)
cat("\nResults saved to:", results_file, "\n")

# Cleanup scratch folder unless --keep-files specified
if (!opt$`keep-files`) {
  should_clean <- config$cluster$clean_scratch %||% TRUE

  if (should_clean) {
    cat("\nCleaning up scratch directory (use --keep-files to preserve)...\n")
    # Remove individual site directories but keep results summary
    for (site_id in selected_sites$siteID) {
      site_dir <- file.path(scratch_run_dir, site_id)
      if (dir.exists(site_dir)) {
        unlink(site_dir, recursive = TRUE)
      }
    }
    cat("Scratch files removed (results summary preserved)\n")
  }
} else {
  cat("\nScratch files preserved in:", scratch_run_dir, "\n")
}

# Close database connection
close_shared_connection(shared_connection)

# Final status message
cat("\n", strrep("=", 70), "\n")
if (n_failed == 0) {
  cat("ALL SIMULATIONS COMPLETED SUCCESSFULLY\n")
} else {
  cat("SIMULATIONS COMPLETED WITH ERRORS\n")
}
cat(strrep("=", 70), "\n\n")

# Exit with appropriate status code
if (n_failed > 0) {
  quit(status = 1, save = "no")
} else {
  quit(status = 0, save = "no")
}
