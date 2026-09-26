#!/usr/bin/env Rscript
# =======================================================================================
#  PURPOSE:   Step 0 - R Environment Setup for DayCent Calibration
#             Ensures all required packages are installed and bayesiancalibr is fresh
#
#  DESCRIPTION: 
#             This script should be run ONCE before starting any GSA pipeline to:
#             1. List all required CRAN packages for the project
#             2. Install missing CRAN packages to custom rlib
#             3. Remove and cleanly reinstall bayesiancalibr package
#             4. Validate all packages are working
#
#  USAGE:     Rscript step0_r_setup.R [config_path]
#             config_path: Optional path to YAML config (default: workflows/configs/nh3_volatilization.yaml)
#
#  AUTHOR:    Yi Yang
#             Colorado State University
#
# =======================================================================================

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Default config path if not provided
if (length(args) == 0) {
    config_path <- "workflows/configs/crop_yield_corn_all.yaml"
} else {
    config_path <- args[1]
}

cat("========================================================================\n")
cat("Step 0: R Environment Setup for DayCent Calibration\n")
cat("========================================================================\n")
cat("Config file:", config_path, "\n")
cat("Date:", Sys.time(), "\n")
cat("Working directory:", getwd(), "\n")
cat("========================================================================\n\n")

# Load YAML package first (install if needed)
if (!require(yaml, quietly = TRUE)) {
    cat("Installing yaml package...\n")
    install.packages("yaml", repos = "https://cran.rstudio.com/")
    library(yaml)
}

# Read configuration
tryCatch({
    config <- yaml::read_yaml(config_path)
    cat("Configuration loaded successfully\n")
}, error = function(e) {
    cat("ERROR: Failed to read config file:", e$message, "\n")
    quit(status = 1)
})

# Get custom library path
if (is.null(config$r_config$rlibpaths)) {
    cat("ERROR: rlibpaths not found in config\n")
    quit(status = 1)
}

custom_lib <- config$r_config$rlibpaths
cat("Custom R library path:", custom_lib, "\n")

# Verify directory exists
if (!dir.exists(custom_lib)) {
    cat("Creating custom library directory:", custom_lib, "\n")
    dir.create(custom_lib, recursive = TRUE)
}

# Set up library paths - put custom library first
.libPaths(new = c(custom_lib, .libPaths()))
cat("Updated .libPaths():\n")
for (i in seq_along(.libPaths())) {
    cat("  ", i, ":", .libPaths()[i], "\n")
}
cat("\n")

# ==================================================================================
# STEP 1: Define all required CRAN packages for the project
# ==================================================================================

cat("Step 1: Defining required CRAN packages...\n")
cat("========================================================================\n")

# Core packages from bayesiancalibr DESCRIPTION + scripts analysis
required_cran_packages <- c(
    # Core calibration packages
    "yaml",          # YAML configuration reading
    "devtools",      # For installing local packages
    "sensitivity",   # Global sensitivity analysis methods
    "lhs",          # Latin hypercube sampling
    "lme4",         # Mixed-effects models for likelihood calculations
    "reshape2",     # Data manipulation
    "stringr",      # String operations
    "boot",         # Bootstrap methods
    
    # Data manipulation and analysis
    "dplyr",        # Data manipulation
    
    # Plotting packages (for results visualization)
    "ggplot2",      # Graphics
    "gridExtra"     # Grid layouts for plots
    # Note: 'grid' is part of base R, no need to install
)

cat("Required CRAN packages:\n")
for (pkg in required_cran_packages) {
    cat("  -", pkg, "\n")
}
cat("Total:", length(required_cran_packages), "packages\n\n")

# ==================================================================================
# STEP 2: Install missing CRAN packages
# ==================================================================================

cat("Step 2: Installing missing CRAN packages...\n")
cat("========================================================================\n")

# Function to check if package is installed in custom library
is_installed_in_custom <- function(package_name, lib_path) {
    installed_packages <- installed.packages(lib.loc = lib_path)
    if (is.null(installed_packages) || nrow(installed_packages) == 0) {
        return(FALSE)
    }
    return(package_name %in% installed_packages[, "Package"])
}

# Function to safely install packages
safe_install_cran <- function(package_name, lib_path) {
    tryCatch({
        if (!is_installed_in_custom(package_name, lib_path)) {
            cat("Installing", package_name, "from CRAN...\n")
            install.packages(package_name, lib = lib_path, repos = "https://cran.rstudio.com/", quiet = TRUE)
            cat("✓", package_name, "installed successfully\n")
            return(TRUE)
        } else {
            cat("✓", package_name, "already installed in custom library\n")
            return(TRUE)
        }
    }, error = function(e) {
        cat("✗ Failed to install", package_name, ":", e$message, "\n")
        return(FALSE)
    })
}

# Install all CRAN packages
installation_results <- list()
for (pkg in required_cran_packages) {
    installation_results[[pkg]] <- safe_install_cran(pkg, custom_lib)
}

cat("\nCRAN Package Installation Summary:\n")
cat("------------------------------------------------------------------------\n")
success_count <- 0
for (pkg in names(installation_results)) {
    status <- if (installation_results[[pkg]]) "SUCCESS" else "FAILED"
    cat(sprintf("%-15s: %s\n", pkg, status))
    if (installation_results[[pkg]]) success_count <- success_count + 1
}
cat("------------------------------------------------------------------------\n")
cat(sprintf("Successful: %d/%d packages\n\n", success_count, length(required_cran_packages)))

if (success_count < length(required_cran_packages)) {
    cat("ERROR: Some CRAN packages failed to install\n")
    quit(status = 1)
}

# ==================================================================================
# STEP 3: Clean reinstall of bayesiancalibr package
# ==================================================================================

cat("Step 3: Clean reinstall of bayesiancalibr package...\n")
cat("========================================================================\n")

# Remove existing bayesiancalibr if present
if (is_installed_in_custom("bayesiancalibr", custom_lib)) {
    cat("Removing existing bayesiancalibr package...\n")
    tryCatch({
        remove.packages("bayesiancalibr", lib = custom_lib)
        cat("✓ Existing bayesiancalibr package removed\n")
    }, error = function(e) {
        cat("Warning: Could not remove existing bayesiancalibr:", e$message, "\n")
    })
}

# Install bayesiancalibr package from local source
bayesiancalibr_path <- file.path(dirname(custom_lib), "bayesiancalibr")
cat("bayesiancalibr source path:", bayesiancalibr_path, "\n")

if (!dir.exists(bayesiancalibr_path)) {
    cat("ERROR: bayesiancalibr source directory not found:", bayesiancalibr_path, "\n")
    quit(status = 1)
}

# Load devtools for installation
if (!require(devtools, quietly = TRUE)) {
    cat("ERROR: devtools package not available\n")
    quit(status = 1)
}

# Install the package
tryCatch({
    cat("Installing bayesiancalibr from local source...\n")
    # Use R CMD INSTALL for more reliable installation
    install_cmd <- paste0("R CMD INSTALL ", shQuote(bayesiancalibr_path), " --library=", shQuote(custom_lib))
    result <- system(install_cmd, intern = TRUE)
    
    # Check if installation was successful
    if (is_installed_in_custom("bayesiancalibr", custom_lib)) {
        cat("✓ bayesiancalibr installed successfully\n")
        bayesiancalibr_success <- TRUE
    } else {
        cat("✗ bayesiancalibr installation failed\n")
        bayesiancalibr_success <- FALSE
    }
}, error = function(e) {
    cat("✗ Failed to install bayesiancalibr:", e$message, "\n")
    bayesiancalibr_success <- FALSE
})

if (!bayesiancalibr_success) {
    cat("ERROR: bayesiancalibr installation failed\n")
    quit(status = 1)
}

# ==================================================================================
# STEP 4: Validate all packages
# ==================================================================================

cat("\nStep 4: Validating package installation...\n")
cat("========================================================================\n")

# Test loading all packages
all_packages <- c(required_cran_packages, "bayesiancalibr")
validation_results <- list()

for (pkg in all_packages) {
    tryCatch({
        library(pkg, character.only = TRUE, quietly = TRUE)
        validation_results[[pkg]] <- TRUE
        cat("✓", pkg, "loads successfully\n")
    }, error = function(e) {
        validation_results[[pkg]] <- FALSE
        cat("✗", pkg, "failed to load:", e$message, "\n")
    })
}

cat("\nFinal Validation Summary:\n")
cat("========================================================================\n")
valid_count <- sum(unlist(validation_results))
total_count <- length(validation_results)

cat(sprintf("Packages successfully validated: %d/%d\n", valid_count, total_count))

if (valid_count == total_count) {
    cat("✓ R environment setup completed successfully!\n\n")
    
    cat("Next steps:\n")
    cat("  1. All GSA step scripts will now use packages from custom library\n")
    cat("  2. Run GSA pipeline steps (Step 1, 2, 3, 4) as needed\n")
    cat("  3. Re-run this script if you make changes to bayesiancalibr package\n\n")
    
    cat("Package locations:\n")
    cat("  Custom library:", custom_lib, "\n")
    cat("  bayesiancalibr source:", bayesiancalibr_path, "\n")
    
    exit_status <- 0
} else {
    cat("✗ Some packages failed validation\n")
    cat("Please check the errors above and re-run this script\n")
    exit_status <- 1
}

cat("========================================================================\n")
cat("Step 0 completed at:", as.character(Sys.time()), "\n")
cat("========================================================================\n")

quit(status = exit_status)
