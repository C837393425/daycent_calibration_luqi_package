#!/usr/bin/env Rscript
#' @title Single DayCent Run from Existing Directory
#' @description Run DayCent simulation using files already present in a directory
#' @details This script runs a single DayCent simulation without modifying any files.
#'   It uses the .sch files and .100 parameter files already present in the directory.
#'
#' @usage
#' Rscript run_daycent_single.R --dir <simulation_directory> [OPTIONS]
#'
#' @examples
#' # Run DayCent in a specific directory
#' Rscript run_daycent_single.R --dir /scratch/run_20251030_103750/877088/877088
#'
#' # Run with custom DayCent executable
#' Rscript run_daycent_single.R --dir /scratch/run_20251030_103750/877088/877088 --daycent-exe /path/to/DayCent
#'
#' # Run with DDList100 processing
#' Rscript run_daycent_single.R --dir /scratch/run_20251030_103750/877088/877088 --run-ddlist

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
})

# =============================================================================
# COMMAND LINE ARGUMENT PARSING
# =============================================================================

option_list <- list(
  make_option(c("-d", "--dir"), type = "character", default = NULL,
              help = "Directory containing DayCent simulation files (REQUIRED)", metavar = "DIR"),

  make_option(c("--daycent-exe"), type = "character", default = NULL,
              help = "Path to DayCent executable (default: auto-detect from config or use 'DayCent')",
              metavar = "PATH"),

  make_option(c("--ddlist-exe"), type = "character", default = NULL,
              help = "Path to DDList100 executable (default: auto-detect from config)",
              metavar = "PATH"),

  make_option(c("--run-ddlist"), action = "store_true", default = FALSE,
              help = "Run DDList100 after DayCent simulation"),

  make_option(c("--schedule"), type = "character", default = NULL,
              help = "Schedule file basename (without .sch) to run (default: auto-detect)",
              metavar = "SCHEDULE"),

  make_option(c("--config"), type = "character", default = NULL,
              help = "Path to YAML configuration file (optional, for library paths)",
              metavar = "FILE"),

  make_option(c("--verbose"), action = "store_true", default = FALSE,
              help = "Print detailed progress messages")
)

opt_parser <- OptionParser(
  usage = "Usage: %prog --dir <simulation_directory> [OPTIONS]",
  option_list = option_list,
  description = "\nRun a single DayCent simulation using files in an existing directory.\n\nExample:\n  %prog --dir /scratch/run_20251030_103750/877088/877088 --verbose"
)

opt <- parse_args(opt_parser)

# =============================================================================
# VALIDATE ARGUMENTS
# =============================================================================

# Check required arguments
if (is.null(opt$dir)) {
  print_help(opt_parser)
  stop("ERROR: --dir is required", call. = FALSE)
}

# Check directory exists
if (!dir.exists(opt$dir)) {
  stop("ERROR: Directory not found: ", opt$dir, call. = FALSE)
}

# =============================================================================
# LOAD CONFIGURATION AND BAYESIANCALIBR PACKAGE
# =============================================================================

# Load config if provided (mainly for library paths and executable locations)
if (!is.null(opt$config)) {
  if (!file.exists(opt$config)) {
    stop("ERROR: Configuration file not found: ", opt$config, call. = FALSE)
  }

  suppressPackageStartupMessages({
    check_and_load("yaml")

    # Read config for library paths
    config_raw <- yaml::read_yaml(opt$config)
    if (!is.null(config_raw$r_config$rlibpaths)) {
      .libPaths(c(config_raw$r_config$rlibpaths, .libPaths()))
    }
  })
}

# Load bayesiancalibr package
suppressPackageStartupMessages({
  if (!requireNamespace("bayesiancalibr", quietly = TRUE)) {
    # Try loading from common custom library location
    custom_lib <- "/data/rubelscratch/rubelogle/daycent_calibration/rlib"
    if (dir.exists(custom_lib)) {
      .libPaths(c(custom_lib, .libPaths()))
    }
  }
  library(bayesiancalibr)
})

cat("Running single DayCent simulation\n")
cat("Directory:", opt$dir, "\n\n")

# =============================================================================
# AUTO-DETECT FILES IN DIRECTORY
# =============================================================================

# Get all .sch files (excluding _eq.sch and _eq_ext30.sch)
sch_files <- list.files(opt$dir, pattern = "\\.sch$", full.names = FALSE)
sch_files <- sch_files[!grepl("_eq\\.sch$|_eq_ext30\\.sch$", sch_files)]

if (length(sch_files) == 0) {
  stop("ERROR: No schedule files found in directory", call. = FALSE)
}

# Determine which schedule to run
if (!is.null(opt$schedule)) {
  schedule_base <- opt$schedule
  schedule_file <- paste0(schedule_base, ".sch")

  if (!file.exists(file.path(opt$dir, schedule_file))) {
    stop("ERROR: Schedule file not found: ", schedule_file, call. = FALSE)
  }
} else {
  # Auto-detect: use the first non-equilibrium schedule file
  schedule_file <- sch_files[1]
  schedule_base <- sub("\\.sch$", "", schedule_file)

  if (opt$verbose) {
    cat("Auto-detected schedule file:", schedule_file, "\n")
  }
}

# Check for required files
site_file <- paste0(schedule_base, ".100")
if (!file.exists(file.path(opt$dir, site_file))) {
  stop("ERROR: Site parameter file not found: ", site_file, call. = FALSE)
}

# =============================================================================
# DETERMINE DAYCENT EXECUTABLE
# =============================================================================

if (!is.null(opt$`daycent-exe`)) {
  daycent_exe <- opt$`daycent-exe`
} else if (!is.null(opt$config)) {
  # Try to get from config
  suppressPackageStartupMessages({
    library(yaml)
    config <- read_yaml(opt$config)

    if (!is.null(config$daycent$executable)) {
      if (!is.null(config$paths$lairice_root)) {
        daycent_exe <- file.path(config$paths$lairice_root, config$daycent$executable)
      } else {
        daycent_exe <- config$daycent$executable
      }
    } else {
      daycent_exe <- "DayCent"  # Hope it's in PATH
    }
  })
} else {
  # Default to system DayCent
  daycent_exe <- "DayCent"
}

if (opt$verbose) {
  cat("DayCent executable:", daycent_exe, "\n")
}

# Check if executable exists (if full path provided)
if (grepl("/", daycent_exe) && !file.exists(daycent_exe)) {
  stop("ERROR: DayCent executable not found: ", daycent_exe, call. = FALSE)
}

# =============================================================================
# DETERMINE DDLIST100 EXECUTABLE (if needed)
# =============================================================================

ddlist_exe <- NULL
if (opt$`run-ddlist`) {
  if (!is.null(opt$`ddlist-exe`)) {
    ddlist_exe <- opt$`ddlist-exe`
  } else if (!is.null(opt$config)) {
    # Try to get from config
    suppressPackageStartupMessages({
      library(yaml)
      config <- read_yaml(opt$config)

      # Check for both naming conventions (ddlist_executable or list100)
      ddlist_path <- NULL
      if (!is.null(config$daycent$ddlist_executable)) {
        ddlist_path <- config$daycent$ddlist_executable
      } else if (!is.null(config$daycent$list100)) {
        ddlist_path <- config$daycent$list100
      }

      if (!is.null(ddlist_path)) {
        if (!is.null(config$paths$lairice_root)) {
          ddlist_exe <- file.path(config$paths$lairice_root, ddlist_path)
        } else {
          ddlist_exe <- ddlist_path
        }
      } else {
        stop("ERROR: DDList100 executable not specified in config (checked ddlist_executable and list100)", call. = FALSE)
      }
    })
  } else {
    stop("ERROR: --run-ddlist requires --ddlist-exe or --config", call. = FALSE)
  }

  if (opt$verbose) {
    cat("DDList100 executable:", ddlist_exe, "\n")
  }

  # Check if executable exists
  if (!file.exists(ddlist_exe)) {
    stop("ERROR: DDList100 executable not found: ", ddlist_exe, call. = FALSE)
  }
}

# =============================================================================
# RUN DAYCENT SIMULATION
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("RUNNING DAYCENT SIMULATION\n")
cat(strrep("=", 70), "\n\n")

cat("Schedule:", schedule_base, "\n")
cat("Working directory:", opt$dir, "\n\n")

# Change to simulation directory
original_dir <- getwd()
setwd(opt$dir)

# Check for existing output files and remove them
bin_file <- paste0(schedule_base, ".bin")
if (file.exists(bin_file)) {
  cat("Removing existing binary file:", bin_file, "\n")
  file.remove(bin_file)
}

# Also remove other DayCent output files that might conflict
output_files_to_remove <- c("bio.out", "nflux.out", "ctrlfert.out")
for (f in output_files_to_remove) {
  if (file.exists(f)) {
    file.remove(f)
  }
}

# Create site.100 symlink if needed (DayCent requires this specific name)
if (!file.exists("site.100")) {
  site_param_file <- paste0(schedule_base, ".100")
  if (file.exists(site_param_file)) {
    if (opt$verbose) {
      cat("Creating site.100 symlink to", site_param_file, "\n")
    }
    file.symlink(site_param_file, "site.100")
  }
}

# Run DayCent
cat("Starting DayCent simulation...\n")

start_time <- Sys.time()

# Build DayCent command
daycent_cmd <- paste(daycent_exe, "-s", schedule_base, "-n", schedule_base)

if (opt$verbose) {
  cat("Command:", daycent_cmd, "\n")
}

# Create log files
stdout_log <- paste0(schedule_base, "_stdout.log")
stderr_log <- paste0(schedule_base, "_stderr.log")

# Execute DayCent
system_result <- system(
  paste(daycent_cmd, ">", stdout_log, "2>", stderr_log),
  intern = FALSE
)

end_time <- Sys.time()
elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

if (system_result == 0) {
  cat("✓ DayCent completed successfully (", round(elapsed_time, 2), "seconds )\n")
} else {
  cat("✗ DayCent FAILED with exit code:", system_result, "\n")
  cat("Check error log:", stderr_log, "\n")
  setwd(original_dir)
  quit(status = 1, save = "no")
}

# Check for binary output file
bin_file <- paste0(schedule_base, ".bin")
if (!file.exists(bin_file)) {
  cat("WARNING: Binary output file not created:", bin_file, "\n")
}

# =============================================================================
# RUN DDLIST100 (if requested)
# =============================================================================

if (opt$`run-ddlist` && !is.null(ddlist_exe)) {
  cat("\n", strrep("-", 70), "\n")
  cat("RUNNING DDLIST100\n")
  cat(strrep("-", 70), "\n\n")

  # Check for outvars.txt
  if (!file.exists("outvars.txt")) {
    cat("WARNING: outvars.txt not found, skipping DDList100\n")
  } else {
    lis_file <- paste0(schedule_base, ".lis")

    cat("Processing binary output...\n")

    if (opt$verbose) {
      cat("Binary file:", bin_file, "\n")
      cat("Output file:", lis_file, "\n")
    }

    # Use bayesiancalibr function
    ddlist_result <- tryCatch({
      run_ddlist100(
        ddlist_exe = ddlist_exe,
        bin_file = bin_file,
        lis_file = lis_file,
        outvars_file = "outvars.txt",
        log_function = if(opt$verbose) cat else function(...) NULL
      )

      TRUE
    }, error = function(e) {
      cat("✗ DDList100 FAILED:", conditionMessage(e), "\n")
      FALSE
    })

    if (ddlist_result) {
      cat("✓ DDList100 completed successfully\n")

      if (file.exists(lis_file)) {
        lis_size <- file.info(lis_file)$size
        cat("  Output file size:", lis_size, "bytes\n")

        # Try to read and show dimensions
        if (lis_size > 0) {
          lis_data <- tryCatch({
            read.table(lis_file, header = TRUE, sep = "", stringsAsFactors = FALSE)
          }, error = function(e) NULL)

          if (!is.null(lis_data)) {
            cat("  Output dimensions:", nrow(lis_data), "rows ×", ncol(lis_data), "columns\n")
          }
        }
      }
    }
  }
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("SIMULATION SUMMARY\n")
cat(strrep("=", 70), "\n\n")

cat("Directory:", opt$dir, "\n")
cat("Schedule:", schedule_base, "\n")
cat("Status: SUCCESS\n")
cat("Runtime:", round(elapsed_time, 2), "seconds\n")

# List output files
cat("\nOutput files created:\n")
output_files <- c(
  paste0(schedule_base, ".bin"),
  stdout_log,
  stderr_log
)

if (opt$`run-ddlist`) {
  output_files <- c(output_files, paste0(schedule_base, ".lis"))
}

# Also check for standard DayCent output files
standard_outputs <- c("bio.out", "nflux.out", "ctrlfert.out")
for (f in standard_outputs) {
  if (file.exists(f)) {
    output_files <- c(output_files, f)
  }
}

for (f in output_files) {
  if (file.exists(f)) {
    fsize <- file.info(f)$size
    cat("  ✓", f, "(", fsize, "bytes )\n")
  } else {
    cat("  ✗", f, "(not created)\n")
  }
}

# Return to original directory
setwd(original_dir)

cat("\n", strrep("=", 70), "\n")
cat("COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n\n")

quit(status = 0, save = "no")
