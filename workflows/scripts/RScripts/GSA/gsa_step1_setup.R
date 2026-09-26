#!/usr/bin/env Rscript
# =======================================================================================
#  PURPOSE:   Example script showing how to use the new GSA Step 1 setup function
#             with YAML configuration instead of the old command-line script
#
#  AUTHOR:    Yi Yang, Luqi Jiao Emanuele
#             Migration from original GSA_Step1_MCdraw_SetUp.R by Ram Gurung
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
# =======================================================================================

# Parse command line arguments first to get config path
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    config_path <- args[1] # Path to YAML config file
} else {
    # Default config path for interactive usage
    config_path <- "workflows/configs/crop_yield_corn_all.yaml"
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
suppressMessages({
    library(bayesiancalibr)
})

# Get task ID from command line arguments
if (length(args) > 1) {
    task_id <- as.numeric(args[2]) # GSA method index (1-8)
    cat("Running GSA Step 1 with command line arguments:\n")
    cat("Config file:", config_path, "\n")
    cat("Task ID:", task_id, "\n\n")
} else {
    # Default for interactive usage
    task_id <- 6 # soboljansen method
    cat("Running GSA Step 1 with default configuration:\n")
    cat("Config file:", config_path, "\n")
    cat("Task ID:", task_id, "(", "method will be determined from config", ")\n\n")
}

# Load and validate configuration (config already loaded by library setup)
tryCatch(
    {
        # Read YAML configuration using bayesiancalibr functions
        config <- read_yaml_config(config_path)

        # Resolve relative paths to absolute paths
        config <- resolve_config_paths(config)

        # Print configuration summary
        cat("Configuration loaded successfully:\n")
        cat("  Project:", config$project$name, "\n")
        cat("  Date:", config$project$date_stamp, "\n")
        cat("  Root directory:", config$paths$lairice_root, "\n")
        cat("  Available GSA methods:", paste(config$gsa$gsa_methods, collapse = ", "), "\n")
        cat("  Selected method:", config$gsa$gsa_methods[task_id], "\n")
        cat("  MC simulations:", config$gsa$nsim, "\n")
        cat("  Bootstrap samples:", config$gsa$nboot, "\n")
        cat("  Random seed:", config$gsa$rseed, "\n")
        cat("  Simulations per job group:", config$gsa$n2dir, "\n\n")

        # Run GSA Step 1 setup
        cat("Starting GSA Step 1 setup...\n")
        result <- gsa_step1_setup(
            config = config,
            task_id = task_id,
            verbose = TRUE
        )

        # Print results summary
        cat("\n")
        cat("====================================================================\n")
        cat("GSA Step 1 Setup Results:\n")
        cat("  GSA Method:", result$smethod, "\n")
        cat("  Output Directory:", result$gsa_method_dir, "\n")
        cat("  Number of Samples:", result$n_samples, "\n")
        cat("  Number of Job Groups:", result$n_job_groups, "\n")
        cat("  Runtime (seconds):", result$runtime_info$Time_sec, "\n")
        cat("  Status:", ifelse(result$runtime_info$status == 0, "SUCCESS", "FAILED"), "\n")
        cat("====================================================================\n")

        # The following files should now exist:
        cat("\nGenerated files:\n")
        cat("  GSA Object:", file.path(result$gsa_method_dir, paste0("GSA_obj_", result$smethod, ".rds")), "\n")
        cat("  MC Draws:", file.path(result$gsa_method_dir, paste0("mc_GSA_draw_", result$smethod, ".rds")), "\n")
        cat("  Run File:", file.path(result$gsa_method_dir, "RunFile.rds"), "\n")
        cat("  Runtime Info:", file.path(result$gsa_method_dir, paste0("RunTime_GSA_setup_", result$smethod, ".csv")), "\n")
 
        # Create database result tables (open connection, create tables, close connection)
        if (config$emmission_variable == "SOC") {
          if (config$file_source$database$database_result$run_type == "database") {
            db_result_cfg <- config$file_source$database$database_result
            if (is.null(db_result_cfg)) {
              cat("Skipping result tables: database_result not in config\n")
            } else {
              annual_table <- db_result_cfg$tables$gsa_results_annual
              run_status_table <- db_result_cfg$tables$gsa_results_run_status
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
              
              DBI::dbExecute(dbConn_postproc, paste0("DROP TABLE IF EXISTS ", annual_table, " ;"))
              DBI::dbExecute(
                dbConn_postproc,
                paste0(
                  "CREATE TABLE ", annual_table, " (",
                  " SampleID       int", ", ",
                  " SiteID         VARCHAR(20)", ", ",
                  " TreatmentID    VARCHAR(60)", ", ",
                  " year           SMALLINT", ", ",
                  " variable       VARCHAR(20)", ", ",
                  " Model          VARCHAR(20)", ", ",
                  " unit           VARCHAR(20)", ", ",
                  " d1             double", ", ",
                  " job_group      VARCHAR(30)", ", ",
                  " simulation_id  int",
                  ");"
                )
              )
              
              DBI::dbExecute(dbConn_postproc, paste0("DROP TABLE IF EXISTS ", run_status_table, " ;"))
              DBI::dbExecute(
                dbConn_postproc,
                paste0(
                  "CREATE TABLE ", run_status_table, " (",
                  "SampleID        INT", ", ",
                  "node            VARCHAR(100)", ", ",
                  "Start           DATETIME", ", ",
                  "End             DATETIME", ", ",
                  "Time_sec        DOUBLE", ", ",
                  "num_trts        INT", ", ",
                  "sim_years       INT", ", ",
                  "obs_years       INT", ", ",
                  "status          TINYINT", ", ",
                  "message         VARCHAR(255)", ", ",
                  "job_group       VARCHAR(30)", ", ",
                  "simulation_id   INT",
                  ");"
                )
              )
              
              DBI::dbDisconnect(con)
              
              cat("Result tables created:", annual_table, ",", run_status_table, "\n")
            }
          }
          
          # create tables for crops
        } else if (config$emmission_variable == "crop") {
          if (config$file_source$database$database_result$run_type == "database") {
              cat("Creating database result tables...\n")
              cred_file <- path.expand("~/.dblogin")
                  if (!file.exists(cred_file)) {
                  stop("Credential file not found: ", cred_file)
                  }
  
                  cred <- readLines(cred_file, warn = FALSE)
                  cred <- trimws(cred)
                  cred <- cred[nzchar(cred)]
                  if (length(cred) < 2) {
                  stop("Credential file must contain at least two non-empty lines (user, password): ", cred_file)
                  }
  
                  user     <- cred[1]
                  password <- cred[2]
  
  
                  host       = config$file_source$database$host
                  db_calib   = config$file_source$database$database
                  
                  gsa_results_weighted = config$file_source$database$database_result$tables$gsa_results_weighted
                  gsa_results_run_status = config$file_source$database$database_result$tables$gsa_results_run_status
                  gsa_results_likelihood = config$file_source$database$database_result$tables$gsa_results_likelihood
  
                  dbConn_result <- DBI::dbConnect(
                      RMariaDB::MariaDB(),
                      host = host, dbname = db_calib,
                      username = user, password = password
                  )
                  on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE)
                  
                  point_assisnments_path <- file.path(config$paths$lairice_root, "results", config$project$name, config$project$date_stamp, "GSA", config$gsa$gsa_methods[task_id], "point_assignments.rds")
                  point_assisnments_file <- readRDS(point_assisnments_path)
                  partition_id <- unique(point_assisnments_file$total_jobs)
                  # Create Annual Tables for each partition
                  for(part_id in 1:partition_id) {

                    annual_table = paste0("part", part_id, "_", config$file_source$database$database_result$tables$gsa_results_annual)

                    DBI::dbExecute(dbConn_result, paste0("DROP TABLE IF EXISTS ", annual_table, " ;"))
                    DBI::dbExecute(dbConn_result, paste0("CREATE TABLE ", annual_table, 
                    " ( SampleID         int", ", ",
                        " SiteID         VARCHAR(20)", ", ",
                        " TreatmentID    VARCHAR(60)", ", ",
                        " year           SMALLINT", ", ",
                        " variable       VARCHAR(20)", ", ",
                        " Model          VARCHAR(20)", ", ",
                        " unit           VARCHAR(20)", ", ",
                        " d1             double", ", ",
                        " job_group      VARCHAR(30)", ", ",
                        " simulation_id  int", ", ",
                        " partition_id   int",
                        ");"
                      ))
                  }

                  DBI::dbExecute(dbConn_result, paste0("DROP TABLE IF EXISTS ", gsa_results_weighted, " ;"))
                  DBI::dbExecute(dbConn_result, paste0("DROP TABLE IF EXISTS ", gsa_results_run_status, " ;"))
                  DBI::dbExecute(dbConn_result, paste0("DROP TABLE IF EXISTS ", gsa_results_likelihood, " ;"))
                  
                  # Create Weighted Table
                  DBI::dbExecute(dbConn_result, paste0("CREATE TABLE ", gsa_results_weighted, 
                  " ( aggregation_level  int(10)", ", ",
                      " year             int(4)", ", ",
                      " variable_name    VARCHAR(20)", ", ",
                      " weighted_value   double", ", ",
                      " n_sites          int(10)", ", ",
                      " total_weight     double", ", ",
                      " sample_id        double", ", ",
                      " crop_filtered    VARCHAR(10)", ", ",
                      " target_crop      text", ", ",
                      " job_group        VARCHAR(30)", ", ",
                      " simulation_id    int",
                      ");"
                  ))
  
                  # Create Run Status Table
                  DBI::dbExecute(dbConn_result, paste0("CREATE TABLE ", gsa_results_run_status, 
                  " ( SampleID         int", ", ",
                      " node           VARCHAR(100)", ", ",
                      " Start          DATETIME", ", ",
                      " End            DATETIME", ", ",
                      " Time_sec       DOUBLE", ", ",
                      " num_trts       int", ", ",
                      " sim_years      int", ", ",
                      " obs_years      int", ", ",
                      " status         int", ", ",
                      " message        VARCHAR(255)", ", ",
                      " job_group      VARCHAR(30)", ", ",
                      " simulation_id  int",
                      ");"
                  ))
  
                  # Create Likelihood Table
                  DBI::dbExecute(dbConn_result, paste0("CREATE TABLE ", gsa_results_likelihood, 
                  " ( SampleID                      int", ", ",
                      " Variable                    VARCHAR(20)", ", ",
                      " total_data_size             int(10)", ", ",
                      " used_data_size              int(10)", ", ",
                      " Pearson_Correlation         double", ", ",
                      " Bayesian_R2                 double", ", ",
                      " RMSE                        double", ", ",
                      " Bias                        double", ", ",
                      " ln_PCor                     double", ", ",
                      " ln_BayesianR2               double", ", ",
                      " ln_RMSE                     double", ", ",
                      " ln_Bias                     double", ", ",
                      " independent_logLkhood       double", ", ",
                      " independent_sigma           double", ", ",
                      " ln_independent_logLkhood    double", ", ",
                      " ln_independent_sigma        double", ", ",
                      " logLkhood_rS                double", ", ",
                      " sigma_site_rS               double", ", ",
                      " sigma_Resi_rS               double", ", ",
                      " AIC_rS                      double", ", ",
                      " BIC_rS                      double", ", ",
                      " DIC_rS                      double", ", ",
                      " RMSE_rS                     double", ", ",
                      " MAE_rS                      double", ", ",
                      " ln_logLkhood_rS             double", ", ",
                      " ln_sigma_site_rS            double", ", ",
                      " ln_sigma_Resi_rS            double", ", ",
                      " ln_AIC_rS                   double", ", ",
                      " ln_BIC_rS                   double", ", ",
                      " ln_DIC_rS                   double", ", ",
                      " ln_RMSE_rS                  double", ", ",
                      " ln_MAE_rS                   double", ", ",
                      " logLkhood_rSY               double", ", ",
                      " sigma_site_rSY              double", ", ",
                      " sigma_siteyr_rSY            double", ", ",
                      " sigma_Resi_rSY              double", ", ",
                      " AIC_rSY                     double", ", ",
                      " BIC_rSY                     double", ", ",
                      " DIC_rSY                     double", ", ",
                      " RMSE_rSY                    double", ", ",
                      " MAE_rSY                     double", ", ",
                      " ln_logLkhood_rSY            double", ", ",
                      " ln_sigma_site_rSY           double", ", ",
                      " ln_sigma_siteyr_rSY         double", ", ",
                      " ln_sigma_Resi_rSY           double", ", ",
                      " ln_AIC_rSY                  double", ", ",
                      " ln_BIC_rSY                  double", ", ",
                      " ln_DIC_rSY                  double", ", ",
                      " ln_RMSE_rSY                 double", ", ",
                      " ln_MAE_rSY                  double", ", ",
                      " Status                      int", ", ",
                      " Comment                     VARCHAR(255)", ", ", 
                      " job_group                   VARCHAR(30)", ", ",
                      " simulation_id               int",
                      ");"
                  ))
                  
                  DBI::dbDisconnect(dbConn_result)
                  cat("Result tables created: ", annual_table, ",", gsa_results_weighted, ",", gsa_results_run_status, ",", gsa_results_likelihood, "\n")
                  
          } else {
              cat("Skipping database result tables: run_type is not database\n")
          }
        }

        cat("\nNext steps:\n")
        cat("  1. Run GSA Step 2 (Model Simulation) using the generated MC draws and weighted mean for crop\n")
        cat("  2. Run GSA Step 3 (Likelihood Calculation) on simulation results\n")
        cat("  3. Run GSA Step 4 (Results Aggregation) to combine outputs\n")
    },
    error = function(e) {
        cat("ERROR:", e$message, "\n")
        quit(status = 1)
    }
)

cat("\nGSA Step 1 setup completed successfully!\n")
