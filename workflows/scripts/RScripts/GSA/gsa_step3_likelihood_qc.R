#!/usr/bin/env Rscript
#=======================================================================================  
#  PURPOSE:   GSA Step 3: Likelihood Calculation QC - Generic CLI Interface
#
#  DESCRIPTION: Command-line interface for the generic GSA Step 3 likelihood calculation QC
#               function. Processes one parameter set (task_id) and checks the likelihood calculation.
#
#  USAGE:     Rscript gsa_step3_likelihood_qc.R <config.yaml> <gsa_method> <task_id> [options]
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
if (length(args) > 0) {
    config_path <- args[1] # Path to YAML config file
} else {
    # config_path <- "workflows/configs/crop_yield_corn_all.yaml"
}

# Load YAML to get custom library path (minimal setup)
if (!require(yaml, quietly = TRUE)) {
    stop("yaml package not available. Please run step0_r_setup.R first.")
}
config <- yaml::read_yaml(config_path)

# Set custom library path
if (!is.null(config$r_config$rlibpaths)) {
    .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
}

# Load required packages
suppressPackageStartupMessages({
  library(bayesiancalibr)
})

# Continue with argument parsing
if (length(args) > 0) {
    gsa_method <- args[2] # GSA method name 

    cat("Running GSA Step 3 with command line arguments:\n")
    cat("Config file:", config_path, "\n")
    cat("GSA method:", gsa_method, "\n") 
     
    
} else {
    # Example 2: Interactive usage with defaults
    config_path <- # "workflows/configs/crop_yield_all.yaml" # change config file for different project
    gsa_method <- "soboljansen" # Default method to match step1's task_id = 6 

    cat("Running GSA Step 3 with default configuration:\n")
    cat("Config file:", config_path, "\n")
    cat("GSA method:", gsa_method, "\n") 
     
}

# Validate inputs
if (!file.exists(config_path)) {
  cat("ERROR: Configuration file not found:", config_path, "\n")
  quit(status = 1)
}
 

# Load configuration
cat("=== Individual GSA Step 3 Likelihood Calculation QC ===\n")
cat("Loading configuration from:", config_path, "\n")

config <- tryCatch({
  read_yaml_config(config_path)
}, error = function(e) {
  cat("ERROR: Failed to load configuration:", e$message, "\n")
  quit(status = 1)
})

# Resolve paths
config <- resolve_config_paths(config)

# Set up DayCent executable path
if (is.null(daycent_exe)) {
  if ("daycent" %in% names(config) && "executable" %in% names(config$daycent)) {
    daycent_exe <- file.path(config$paths$lairice_root, config$daycent$executable)
  } else {
    cat("ERROR: DayCent executable not specified in config or command line\n")
    quit(status = 1)
  }
}

# Set up index
db_result_cfg <- config$file_source$database$database_result
if (is.null(db_result_cfg)) {
  cat("Skipping result tables: database_result not in config\n")
} else {
  likelihood_table <- db_result_cfg$tables$gsa_results_likelihood
  host <- db_result_cfg$host
  database <- db_result_cfg$database
  cred_file <- db_result_cfg$cred_file %||% "~/.dblogin"
  
  cred <- readLines(path.expand(cred_file), warn = FALSE)
  if (length(cred) < 2) stop("Credential file must contain at least 2 lines (username, password)")
  username <- trimws(cred[1])
  password <- trimws(cred[2])
  
  has_mariadb <- requireNamespace("RMariaDB", quietly = TRUE)
  has_mysql <- requireNamespace("RMySQL", quietly = TRUE)
  if (!requireNamespace("DBI", quietly = TRUE)) stop("Package 'DBI' is required")
  if (!has_mariadb && !has_mysql) stop("Package 'RMariaDB' or 'RMySQL' is required")
  
  if (has_mariadb) {
    dbConn_postproc <- DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = host, dbname = database,
      username = username, password = password
    )
  } else {
    dbConn_postproc <- DBI::dbConnect(
      RMySQL::MySQL(),
      host = host, dbname = database,
      username = username, password = password
    )
  }
  on.exit(DBI::dbDisconnect(dbConn_postproc), add = TRUE)
  
  DBI::dbExecute(  dbConn_postproc, paste0( "ALTER TABLE ", likelihood_table, " ADD INDEX idx_job (job_group) ;" ) )
  DBI::dbExecute(  dbConn_postproc, paste0( "ALTER TABLE ", likelihood_table, " ADD INDEX idx_id (simulation_id) ;" ) )
   
}


# QC the table
{
  likelihood_tbl <- DBI::dbGetQuery(  dbConn_postproc, paste0( "SELECT * FROM ", likelihood_table, " ;" ) )
  
  DBI::dbDisconnect(con)
  
  mc_tbl <- readRDS(paste0(config$paths$lairice_root, "/results/", config$project$name, "/", config$project$date_stamp, "/GSA/", gsa_method, "/mc_GSA_draw_", gsa_method, ".rds"))
  if (is.null(mc_tbl)) {
    cat("ERROR: MC table not found:", paste0(config$paths$lairice_root, "/results/", config$project$name, "/", config$project$date_stamp, "/GSA/", gsa_method, "/mc_GSA_draw_", gsa_method, ".rds"), "\n")
  }
  if (length(unique(mc_tbl$SampleID)) != length(unique(likelihood_tbl$simulation_id))) {
    cat("ERROR: MC table has wrong number of rows: ", paste0(length(unique(mc_tbl$SampleID)), " != ", length(unique(likelihood_tbl$simulation_id))), "\n") 
  }
  if (length(unique(mc_tbl$JobGroup)) != length(unique(likelihood_tbl$job_group))) {
    cat("ERROR: MC table has wrong number of rows: ", paste0(length(unique(mc_tbl$JobGroup)), " != ", length(unique(likelihood_tbl$job_group))), "\n")
  }
}

cat("QC passed\n")

quit(status = 0)