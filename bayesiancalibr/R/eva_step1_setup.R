#' Evaluation Step 1: setup RunFile, folders, and optional result tables
#'
#' Prepares the Evaluation output tree so Step 2 simulations can be submitted
#' as an sbatch array. Does not run DayCent.
#'
#' @author DayCent Calibration Framework
#' @note Created August 2026

#' Evaluation directory map (mirrors SIR setup paths)
#'
#' @param config Configuration object
#' @param date_stamp Date stamp
#' @return Named list of directory paths
#' @noRd
eva_setup_paths <- function(config, date_stamp) {
  base_dir <- eva_output_dir(config, date_stamp)
  list(
    eva_method_dir = base_dir,
    daily_out_dir = file.path(base_dir, "Daily_Outputs"),
    annual_dir = file.path(base_dir, "Annual_Outputs"),
    aggregated_dir = file.path(base_dir, "Aggregated_Outputs"),
    weighted_mean_dir = file.path(base_dir, "Weighted_Mean_Outputs"),
    run_status_dir = file.path(base_dir, "Run_Status"),
    results_dir = file.path(base_dir, "Results"),
    figures_dir = file.path(base_dir, "Figures")
  )
}

#' Create Evaluation top-level directories
#'
#' @param paths List from \code{eva_setup_paths()}
#' @param verbose Logical
#' @noRd
eva_create_directories <- function(paths, verbose = TRUE) {
  if (verbose) cat("Creating Evaluation directories:\n")
  for (path_name in names(paths)) {
    path <- paths[[path_name]]
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
      if (verbose) cat("\t -", path, "\n")
    } else if (verbose) {
      cat("\t -", path, "(exists)\n")
    }
  }
  if (verbose) cat("\n")
}

#' Create job-group subfolders under Evaluation output dirs
#'
#' Used when \code{database_result.run_type} is \code{file_system}.
#'
#' @param paths List from \code{eva_setup_paths()}
#' @param job_groups Integer vector of job group IDs
#' @param config Configuration object
#' @param verbose Logical
#' @noRd
eva_create_job_group_directories <- function(paths, job_groups, config, verbose = TRUE) {
  job_groups <- sort(unique(as.integer(job_groups)))
  job_groups <- job_groups[!is.na(job_groups)]

  for (grp in job_groups) {
    grp_suffix <- paste0("jobGroup_", grp)
    dir.create(file.path(paths$daily_out_dir, grp_suffix), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(paths$aggregated_dir, grp_suffix), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(paths$annual_dir, grp_suffix), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(paths$run_status_dir, grp_suffix), recursive = TRUE, showWarnings = FALSE)
    if (identical(config$emmission_variable, "crop")) {
      dir.create(file.path(paths$weighted_mean_dir, grp_suffix), recursive = TRUE, showWarnings = FALSE)
    }
  }

  if (verbose) {
    cat("Created job-group subdirectories for", length(job_groups), "groups:",
        paste(job_groups, collapse = ", "), "\n")
  }
}

#' Connect to the evaluation result database
#'
#' @noRd
eva_connect_result_database <- function(config) {
  db_cfg <- config$file_source$database$database_result
  host <- config$file_source$database$host
  database <- config$file_source$database$database
  cred_file <- eva_null_coalesce(config$file_source$database$cred_file, "~/.dblogin")
  if (!is.null(db_cfg$host) && nzchar(db_cfg$host)) host <- db_cfg$host
  if (!is.null(db_cfg$database) && nzchar(db_cfg$database)) database <- db_cfg$database
  if (!is.null(db_cfg$cred_file) && nzchar(db_cfg$cred_file)) cred_file <- db_cfg$cred_file

  if (is.null(host) || is.null(database)) {
    stop("database_result host/database not fully specified in config")
  }

  cred_path <- path.expand(cred_file)
  if (!file.exists(cred_path)) {
    stop("Credential file not found: ", cred_path)
  }
  cred <- readLines(cred_path, warn = FALSE)
  cred <- trimws(cred)
  cred <- cred[nzchar(cred)]
  if (length(cred) < 2) {
    stop("Credential file must contain at least 2 lines (username, password)")
  }

  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required to create evaluation result tables")
  }
  has_mariadb <- requireNamespace("RMariaDB", quietly = TRUE)
  has_mysql <- requireNamespace("RMySQL", quietly = TRUE)
  if (!has_mariadb && !has_mysql) {
    stop("Package 'RMariaDB' or 'RMySQL' is required to create evaluation result tables")
  }

  if (has_mariadb) {
    DBI::dbConnect(RMariaDB::MariaDB(), host = host, dbname = database,
                   username = cred[1], password = cred[2])
  } else {
    DBI::dbConnect(RMySQL::MySQL(), host = host, dbname = database,
                   username = cred[1], password = cred[2])
  }
}

#' Create evaluation result tables when run_type is database
#'
#' @param config Configuration object
#' @param verbose Logical
#' @return Character vector of table names created
#' @export
eva_create_result_tables <- function(config, verbose = TRUE) {
  db_cfg <- config$file_source$database$database_result
  tbls <- if (!is.null(db_cfg) && "tables" %in% names(db_cfg)) db_cfg$tables else list()

  annual_table <- tbls$eva_results_annual
  status_table <- tbls$eva_results_run_status
  daily_table <- tbls$eva_results_daily
  weighted_table <- tbls$eva_results_weighted

  if (is.null(annual_table) || !nzchar(annual_table)) {
    stop("database_result.tables.eva_results_annual is required when run_type is database")
  }

  con <- eva_connect_result_database(config)
  on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

  created <- character(0)
  scaling_mode <- is_scaling_mode_enabled(config)

  create_annual_sql <- function(table_name, with_partition = FALSE) {
    partition_col <- if (with_partition) ", partition_id int" else ""
    paste0(
      "CREATE TABLE ", table_name, " (",
      " SampleID int, ",
      " SiteID VARCHAR(20), ",
      " TreatmentID VARCHAR(60), ",
      " year SMALLINT, ",
      " variable VARCHAR(20), ",
      " Model VARCHAR(20), ",
      " unit VARCHAR(20), ",
      " d1 double, ",
      " job_group VARCHAR(30), ",
      " simulation_id int, ",
      " original_sample_id int, ",
      " EvaSimID int",
      partition_col,
      ");"
    )
  }

  if (scaling_mode) {
    n_parts <- 1L
    pa_file <- file.path(eva_output_dir(config), "point_assignments.rds")
    if (file.exists(pa_file)) {
      pa <- readRDS(pa_file)
      if (!is.null(pa$total_jobs)) {
        n_parts <- max(as.integer(pa$total_jobs), na.rm = TRUE)
      }
    } else {
      points_per_job <- get_points_per_job_config(config)
      if (!is.null(points_per_job) && verbose) {
        cat("Warning: point_assignments.rds not found; creating a single partition annual table\n")
      }
    }
    for (part_id in seq_len(n_parts)) {
      annual_part <- paste0("part", part_id, "_", annual_table)
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", annual_part, " ;"))
      DBI::dbExecute(con, create_annual_sql(annual_part, with_partition = TRUE))
      created <- c(created, annual_part)
    }
  } else {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", annual_table, " ;"))
    DBI::dbExecute(con, create_annual_sql(annual_table, with_partition = FALSE))
    created <- c(created, annual_table)
  }

  if (!is.null(status_table) && nzchar(status_table)) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", status_table, " ;"))
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", status_table, " (",
      " EvaSimID INT, ",
      " SampleID INT, ",
      " node VARCHAR(100), ",
      " Start DATETIME, ",
      " End DATETIME, ",
      " Time_sec DOUBLE, ",
      " num_trts INT, ",
      " sim_years INT, ",
      " obs_years INT, ",
      " status TINYINT, ",
      " message VARCHAR(255), ",
      " job_group VARCHAR(30), ",
      " simulation_id INT, ",
      " partition_id INT",
      ");"
    ))
    created <- c(created, status_table)
  }

  if (!is.null(daily_table) && nzchar(daily_table)) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", daily_table, " ;"))
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", daily_table, " (",
      " SampleID int, ",
      " SiteID VARCHAR(20), ",
      " TreatmentID VARCHAR(60), ",
      " year SMALLINT, ",
      " variable VARCHAR(20), ",
      " Model VARCHAR(20), ",
      " unit VARCHAR(20), ",
      " d1 double, ",
      " job_group VARCHAR(30), ",
      " simulation_id int, ",
      " original_sample_id int",
      ");"
    ))
    created <- c(created, daily_table)
  }

  if (!is.null(weighted_table) && nzchar(weighted_table)) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", weighted_table, " ;"))
    DBI::dbExecute(con, paste0(
      "CREATE TABLE ", weighted_table, " (",
      " aggregation_level int(10), ",
      " year int(4), ",
      " variable_name VARCHAR(20), ",
      " weighted_value double, ",
      " n_sites int(10), ",
      " total_weight double, ",
      " sample_id double, ",
      " job_group VARCHAR(30), ",
      " simulation_id int, ",
      " original_sample_id int",
      ");"
    ))
    created <- c(created, weighted_table)
  }

  if (verbose) {
    cat("Created evaluation result tables:", paste(created, collapse = ", "), "\n")
  }
  created
}

#' Evaluation Step 1 setup
#'
#' Creates the Evaluation output tree, writes \code{RunFile.rds} for
#' evaluation sites, copies posterior parameter sets to
#' \code{mc_EVA_draw.rds}, and either creates job-group subfolders
#' (filesystem results) or database result tables (database results).
#'
#' @param config YAML configuration loaded with \code{read_yaml_config()}
#' @param date_stamp Optional date stamp override
#' @param create_table Logical; create DB result tables when run_type is database
#' @param verbose Logical
#' @return List with paths, run_file, posterior_jobs, n_samples, n_job_groups, runtime_info
#' @export
eva_step1_setup <- function(config, date_stamp = NULL, create_table = TRUE, verbose = TRUE) {
  start_time <- Sys.time()
  node <- Sys.info()[4]

  if (missing(config)) stop("Configuration object is required")

  config <- eva_apply_config_overrides(config)
  date_stamp <- eva_resolve_date_stamp(config, date_stamp)
  paths <- eva_setup_paths(config, date_stamp)
  eva_output_path <- paths$eva_method_dir
  run_type <- eva_result_run_type(config)
  file_mode <- eva_null_coalesce(config$file_source$mode, "filesystem")

  if (identical(file_mode, "filesystem") &&
      (is.null(config$paths$expsites_dir) || !dir.exists(config$paths$expsites_dir))) {
    stop("Evaluation schedule directory not found: ",
         eva_null_coalesce(config$paths$expsites_dir, "(unset)"),
         "\nSet paths.evasites_dir in the YAML (e.g. data/soil_organic_carbon/Daycent_ScheduleFiles_Evaluation).")
  }

  if (verbose) {
    cat("====================================================================\n")
    cat("Evaluation Step 1 Setup - RunFile and output layout\n")
    cat("Working Directory:", getwd(), "\n\n")
    cat("Configuration:\n")
    cat("\t Root Directory                          :", config$paths$lairice_root, "\n")
    cat("\t Date Stamp                              :", date_stamp, "\n")
    cat("\t File source mode                        :", file_mode, "\n")
    cat("\t Result run type                         :", run_type, "\n")
    cat("\t Evaluation sites dir                    :", config$paths$expsites_dir, "\n")
    cat("\t Evaluation dot100 dir                   :", config$paths$dot100_path, "\n")
    cat("\t DayCent mode                            :", eva_null_coalesce(config$daycent$mode, "unset"), "\n")
    cat("\t Chained run mode                        :", eva_null_coalesce(config$daycent$chained_run_mode, "unset"), "\n")
    if (!is.null(config$daycent$schedule_columns)) {
      cat("\t Schedule columns                        :", paste(config$daycent$schedule_columns, collapse = " -> "), "\n")
    }
    cat("\t Posterior model                         :", eva_null_coalesce(config$evaluation$posterior$model, "rSY"), "\n\n")
    cat("====================================================================\n")
  }

  eva_create_directories(paths, verbose = verbose)

  if (verbose) cat("Loading posterior parameter sets...\n")
  posterior_jobs <- eva_load_posterior_sample(config, date_stamp = date_stamp, verbose = verbose)
  posterior_file <- file.path(eva_output_path, "mc_EVA_draw.rds")
  saveRDS(posterior_jobs, posterior_file)
  if (verbose) cat("Saved evaluation posterior draws:", posterior_file, "\n")

  if (verbose) cat("Generating evaluation RunFile...\n")
  run_file <- eva_build_runfile(config, shared_connection = NULL, verbose = verbose)
  run_file_name <- file.path(eva_output_path, "RunFile.rds")
  saveRDS(run_file, run_file_name)
  if (verbose) {
    cat("Saved evaluation RunFile:", run_file_name, "\n")
    cat("  Rows:", nrow(run_file), "| sites:", length(unique(run_file$siteID)), "\n")
  }

  n_samples <- nrow(posterior_jobs)
  n_job_groups <- length(unique(posterior_jobs$EvaJobGroup))

  created_tables <- character(0)
  if (identical(run_type, "database")) {
    if (isTRUE(create_table)) {
      if (verbose) cat("Creating evaluation database result tables...\n")
      created_tables <- eva_create_result_tables(config, verbose = verbose)
    } else if (verbose) {
      cat("Skipping database result tables (create_table = FALSE)\n")
    }
  } else {
    if (verbose) cat("Filesystem result mode: creating job-group subfolders...\n")
    eva_create_job_group_directories(
      paths = paths,
      job_groups = posterior_jobs$EvaJobGroup,
      config = config,
      verbose = verbose
    )
  }

  end_time <- Sys.time()
  runtime_info <- data.frame(
    node = node,
    Start = start_time,
    End = end_time,
    Time_sec = as.numeric(difftime(end_time, start_time, units = "secs")),
    n_samples = n_samples,
    n_job_groups = n_job_groups,
    n_runfile_rows = nrow(run_file),
    n_sites = length(unique(run_file$siteID)),
    run_type = run_type,
    status = 0,
    message = "Evaluation Step 1 setup success",
    stringsAsFactors = FALSE
  )
  runtime_file <- file.path(eva_output_path, "RunTime_EVA_setup.csv")
  write.csv(runtime_info, runtime_file, row.names = FALSE)

  if (verbose) {
    cat("\n====================================================================\n")
    cat("Evaluation Step 1 Setup Results:\n")
    cat("  Output Directory:", eva_output_path, "\n")
    cat("  Number of posterior samples:", n_samples, "\n")
    cat("  Number of job groups:", n_job_groups, "\n")
    cat("  RunFile rows / sites:", nrow(run_file), "/", length(unique(run_file$siteID)), "\n")
    cat("  Result run type:", run_type, "\n")
    cat("  Runtime (seconds):", runtime_info$Time_sec, "\n")
    cat("  Status: SUCCESS\n")
    cat("====================================================================\n")
  }

  list(
    eva_method_dir = eva_output_path,
    date_stamp = date_stamp,
    run_file = run_file,
    posterior_jobs = posterior_jobs,
    n_samples = n_samples,
    n_job_groups = n_job_groups,
    run_type = run_type,
    created_tables = created_tables,
    runtime_info = runtime_info,
    paths = paths
  )
}

#' Load Evaluation Step 1 products for a Step 2 simulation
#'
#' Requires \code{eva_step1_setup()} to have been run. Does not create files.
#'
#' @param config Configuration object
#' @param date_stamp Optional date stamp override
#' @param verbose Logical
#' @return List with eva_output_path, run_file, posterior_jobs
#' @export
eva_step2_load_prepared_inputs <- function(config, date_stamp = NULL, verbose = TRUE) {
  config <- eva_apply_config_overrides(config)
  date_stamp <- eva_resolve_date_stamp(config, date_stamp)
  eva_output_path <- eva_output_dir(config, date_stamp)

  posterior_file <- file.path(eva_output_path, "mc_EVA_draw.rds")
  run_file_name <- file.path(eva_output_path, "RunFile.rds")

  if (!file.exists(run_file_name)) {
    stop("Evaluation RunFile.rds not found: ", run_file_name,
         "\nRun Evaluation Step 1 first: workflows/scripts/RScripts/Evaluation/eva_step1_setup.R")
  }
  if (!file.exists(posterior_file)) {
    stop("Evaluation posterior draws not found: ", posterior_file,
         "\nRun Evaluation Step 1 first: workflows/scripts/RScripts/Evaluation/eva_step1_setup.R")
  }

  run_file <- readRDS(run_file_name)
  posterior_jobs <- readRDS(posterior_file)

  if (verbose) {
    cat("Loaded Evaluation Step 1 products from:", eva_output_path, "\n")
    cat("  RunFile:", nrow(run_file), "rows,", length(unique(run_file$siteID)), "sites\n")
    cat("  Posterior draws:", nrow(posterior_jobs), "\n")
  }

  list(
    eva_output_path = eva_output_path,
    date_stamp = date_stamp,
    run_file = run_file,
    posterior_jobs = posterior_jobs
  )
}
