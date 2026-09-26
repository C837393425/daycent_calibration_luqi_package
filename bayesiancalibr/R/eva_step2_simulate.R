#' Evaluation Step 1: Posterior-parameter DayCent simulations
#'
#' Runs DayCent on held-out evaluation sites using posterior parameter sets from
#' SIR (`best_param_set_{model}.csv`). Site files can come from the filesystem
#' or the database (`file_source.mode`). Results can be written to the
#' filesystem or to `database_result` tables (`run_type`).
#'
#' @author DayCent Calibration Framework
#' @note Created August 2026

#' Null-coalesce helper (same convention as other package files)
#' @noRd
eva_null_coalesce <- function(lhs, rhs) {
  if (is.null(lhs) || length(lhs) == 0) {
    return(rhs)
  }
  lhs
}

#' Resolve project date stamp
#'
#' @param config Configuration object
#' @param date_stamp Optional override
#' @return Character date stamp
#' @noRd
eva_resolve_date_stamp <- function(config, date_stamp = NULL) {
  if (!is.null(date_stamp) && nzchar(date_stamp)) {
    return(date_stamp)
  }
  if (is.null(config$project$date_stamp) || identical(config$project$date_stamp, "auto")) {
    return(format(Sys.Date(), "%d%b%Y"))
  }
  config$project$date_stamp
}

#' Evaluation output directory
#'
#' @param config Configuration object
#' @param date_stamp Date stamp
#' @return Absolute path to results/.../Evaluation
#' @export
eva_output_dir <- function(config, date_stamp = NULL) {
  date_stamp <- eva_resolve_date_stamp(config, date_stamp)
  output_base <- config$paths$output_base
  if (is.null(output_base) || !nzchar(output_base)) {
    output_base <- file.path("results", config$project$name)
  }
  file.path(config$paths$lairice_root, output_base, date_stamp, "Evaluation")
}

#' SIR directory used as the posterior source
#'
#' @param config Configuration object
#' @param date_stamp Evaluation date stamp (used if source_date_stamp is unset)
#' @return Absolute path to results/.../SIR
#' @export
eva_sir_source_dir <- function(config, date_stamp = NULL) {
  date_stamp <- eva_resolve_date_stamp(config, date_stamp)
  source_date <- eva_null_coalesce(config$evaluation$posterior$source_date_stamp, date_stamp)
  output_base <- config$paths$output_base
  if (is.null(output_base) || !nzchar(output_base)) {
    output_base <- file.path("results", config$project$name)
  }
  file.path(config$paths$lairice_root, output_base, source_date, "SIR")
}

#' Overlay evaluation-specific file source / site / observation settings
#'
#' Leaves the original config unchanged except for the fields needed so that
#' `create_daycent_runfile_enhanced()` and `run_site_simulations_generic()`
#' use evaluation sites. Filesystem vs database input is still
#' `file_source.mode`, optionally overridden under `evaluation.file_source`.
#'
#' Schedule directory priority (first non-empty wins):
#' `evaluation.evasites_dir`, `evaluation.expsites_dir`, `paths.evasites_dir`.
#' The chosen path is copied onto `paths.expsites_dir` so shared DayCent helpers
#' pick up evaluation schedules.
#'
#' `.100` directory priority (first non-empty wins):
#' `evaluation.evadot100_path`, `evaluation.dot100_path`, `paths.evadot100_path`.
#' Copied onto `paths.dot100_path` for `copy_dot100_files()`.
#'
#' @param config Configuration object
#' @return Config with evaluation overrides applied
#' @export
eva_apply_config_overrides <- function(config) {
  eva <- config$evaluation
  if (is.null(eva)) {
    eva <- list()
  }

  pick_abs <- function(...) {
    for (p in list(...)) {
      if (!is.null(p) && is.character(p) && length(p) >= 1 && nzchar(p[[1]])) {
        p <- p[[1]]
        if (!file.path.is.absolute(p)) {
          p <- file.path(config$paths$lairice_root, p)
        }
        return(p)
      }
    }
    NULL
  }

  sites_dir <- pick_abs(eva$evasites_dir, eva$expsites_dir, config$paths$evasites_dir)
  if (!is.null(sites_dir)) {
    config$paths$expsites_dir <- sites_dir
    config$paths$evasites_dir <- sites_dir
  }

  dot100_dir <- pick_abs(eva$evadot100_path, eva$dot100_path, config$paths$evadot100_path)
  if (!is.null(dot100_dir)) {
    config$paths$dot100_path <- dot100_dir
    config$paths$evadot100_path <- dot100_dir
  }

  if (!is.null(eva$observation_data) && is.list(eva$observation_data)) {
    if (is.null(config$input_files$observation_data)) {
      config$input_files$observation_data <- list()
    }
    for (nm in names(eva$observation_data)) {
      p <- eva$observation_data[[nm]]
      if (!is.null(p) && is.character(p) && nzchar(p) && !file.path.is.absolute(p)) {
        p <- file.path(config$paths$lairice_root, p)
      }
      config$input_files$observation_data[[nm]] <- p
    }
  }

  if (!is.null(eva$file_source) && is.list(eva$file_source)) {
    if (!is.null(eva$file_source$mode) && nzchar(eva$file_source$mode)) {
      config$file_source$mode <- eva$file_source$mode
    }
    eva_db <- eva$file_source$database
    if (!is.null(eva_db) && is.list(eva_db)) {
      if (is.null(config$file_source$database)) {
        config$file_source$database <- list()
      }
      for (nm in names(eva_db)) {
        if (nm == "tables" && is.list(eva_db$tables)) {
          if (is.null(config$file_source$database$tables)) {
            config$file_source$database$tables <- list()
          }
          for (tn in names(eva_db$tables)) {
            config$file_source$database$tables[[tn]] <- eva_db$tables[[tn]]
          }
        } else {
          config$file_source$database[[nm]] <- eva_db[[nm]]
        }
      }
    }
  }

  config
}

#' Result storage mode for evaluation (filesystem vs database tables)
#'
#' Uses `file_source.database.database_result.run_type`. If `run_type` is
#' `"database"` but evaluation table names are missing, falls back to
#' `"file_system"`.
#'
#' @param config Configuration object
#' @return `"file_system"` or `"database"`
#' @export
eva_result_run_type <- function(config) {
  db_result_cfg <- NULL
  if (!is.null(config$file_source) && !is.null(config$file_source$database)) {
    db_result_cfg <- config$file_source$database$database_result
  }

  run_type <- "file_system"
  if (!is.null(db_result_cfg) && !is.null(db_result_cfg$run_type) && nzchar(db_result_cfg$run_type)) {
    run_type <- db_result_cfg$run_type
  }

  if (identical(run_type, "database")) {
    tbls <- if (!is.null(db_result_cfg)) db_result_cfg$tables else NULL
    has_annual <- !is.null(tbls) && !is.null(tbls$eva_results_annual) && nzchar(tbls$eva_results_annual)
    if (!has_annual) {
      run_type <- "file_system"
    }
  }

  run_type
}

#' Resolve the posterior parameter-set CSV written by SIR Step 4.5
#'
#' Default file is \code{SIR/Results/best_param_set_{model}.csv}, which already
#' contains SampleID plus the calibrated parameter columns.
#'
#' @param config Configuration object
#' @param date_stamp Date stamp for the evaluation (and default SIR source)
#' @return Absolute path to the parameter-set CSV
#' @export
eva_posterior_param_file <- function(config, date_stamp = NULL) {
  date_stamp <- eva_resolve_date_stamp(config, date_stamp)
  model <- eva_null_coalesce(config$evaluation$posterior$model, "rSY")
  sir_dir <- eva_sir_source_dir(config, date_stamp)

  sample_file <- config$evaluation$posterior$sample_file
  if (is.null(sample_file) || !nzchar(sample_file)) {
    sample_file <- file.path(sir_dir, "Results", paste0("best_param_set_", model, ".csv"))
  } else if (!file.path.is.absolute(sample_file)) {
    sample_file <- file.path(config$paths$lairice_root, sample_file)
  }
  sample_file
}

#' Align one posterior row to prior ParameterName order for SIR prepare
#'
#' \code{sir_step2_prepare_parameters()} assigns values by column position.
#' Reorder named parameter columns to match the prior file so CSV column
#' order cannot silently swap values.
#'
#' @param config Configuration object
#' @param job_params One-row data frame from the posterior parameter CSV
#' @return Data frame with SampleID followed by SIR parameters in prior order
#' @noRd
eva_align_job_params_to_prior <- function(config, job_params) {
  prior_file <- config$input_files$prior_file
  if (!file.exists(prior_file)) {
    stop("Prior file not found: ", prior_file)
  }
  prior <- read.csv(prior_file, stringsAsFactors = FALSE)
  sir_parms <- config$sir$sir_parameters
  prior <- prior[prior$ParameterName %in% sir_parms, ]
  param_names <- prior$ParameterName

  missing <- setdiff(param_names, names(job_params))
  if (length(missing) > 0) {
    stop("Posterior parameter file is missing columns required by sir_parameters: ",
         paste(missing, collapse = ", "))
  }

  keep <- c("SampleID", param_names)
  job_params[, keep, drop = FALSE]
}

#' Load posterior parameter sets from SIR Results
#'
#' Reads \code{best_param_set_{model}.csv} (SampleID + parameter columns).
#' \code{sim_id} in Step 1 is the 1-based row index into this table.
#'
#' @param config Configuration object
#' @param date_stamp Date stamp for the evaluation (and default SIR source)
#' @param verbose Logical
#' @return Data frame with parameter columns plus EvaSimID and EvaJobGroup
#' @export
eva_load_posterior_sample <- function(config, date_stamp = NULL, verbose = TRUE) {
  date_stamp <- eva_resolve_date_stamp(config, date_stamp)
  model <- eva_null_coalesce(config$evaluation$posterior$model, "rSY")
  sample_file <- eva_posterior_param_file(config, date_stamp)

  if (!file.exists(sample_file)) {
    stop("Posterior parameter-set file not found: ", sample_file,
         "\nExpected SIR/Results/best_param_set_", model, ".csv ",
         "(or set evaluation.posterior.sample_file).")
  }

  posterior_jobs <- read.csv(sample_file, stringsAsFactors = FALSE)
  if (!("SampleID" %in% names(posterior_jobs))) {
    stop("Posterior parameter-set CSV must have column 'SampleID': ", sample_file)
  }
  if (nrow(posterior_jobs) == 0) {
    stop("Posterior parameter-set CSV is empty: ", sample_file)
  }

  posterior_jobs$EvaSimID <- seq_len(nrow(posterior_jobs))

  n2dir <- eva_null_coalesce(config$evaluation$n2dir, config$sir$sir_n2dir)
  n2dir <- as.integer(eva_null_coalesce(n2dir, 1000))
  if (is.na(n2dir) || n2dir < 1) {
    n2dir <- 1000
  }
  posterior_jobs$EvaJobGroup <- ceiling(posterior_jobs$EvaSimID / n2dir)

  if (verbose) {
    cat("Loaded posterior parameter sets from:", sample_file, "\n")
    cat("  Model:", model, "\n")
    cat("  Posterior draws:", nrow(posterior_jobs), "\n")
    param_cols <- setdiff(names(posterior_jobs), c("SampleID", "JobGroup", "EvaSimID", "EvaJobGroup"))
    cat("  Parameter columns:", paste(param_cols, collapse = ", "), "\n")
  }

  attr(posterior_jobs, "sample_file") <- sample_file
  attr(posterior_jobs, "sir_dir") <- eva_sir_source_dir(config, date_stamp)
  attr(posterior_jobs, "model") <- model
  posterior_jobs
}

#' Build the evaluation RunFile (filesystem or database)
#'
#' Uses `create_daycent_runfile_enhanced()`, then keeps rows whose `group`
#' column matches `evaluation.site_group` when that column is present.
#'
#' @param config Configuration object (evaluation overlays should already be applied)
#' @param shared_connection Optional shared DB connection
#' @param verbose Logical
#' @return RunFile data frame
#' @export
eva_build_runfile <- function(config, shared_connection = NULL, verbose = TRUE) {
  log_fun <- if (verbose) cat else function(...) NULL
  file_mode <- eva_null_coalesce(config$file_source$mode, "filesystem")

  if (verbose) {
    cat("Building evaluation RunFile using file_source.mode =", file_mode, "\n")
  }

  run_file <- create_daycent_runfile_enhanced(
    config = config,
    ExpSite_path = config$paths$expsites_dir,
    shared_connection = shared_connection,
    log_function = log_fun
  )

  site_group <- eva_null_coalesce(config$evaluation$site_group, "evaluation")
  if ("group" %in% names(run_file) && !is.null(site_group) && nzchar(site_group)) {
    n_before <- nrow(run_file)
    run_file <- run_file[run_file$group == site_group, , drop = FALSE]
    if (verbose) {
      cat("Filtered RunFile to group '", site_group, "': ",
          nrow(run_file), " of ", n_before, " rows, ",
          length(unique(run_file$siteID)), " sites\n", sep = "")
    }
    if (nrow(run_file) == 0) {
      stop("No evaluation sites found after filtering group == '", site_group, "'")
    }
  } else if (verbose) {
    cat("RunFile has no 'group' column (or site_group is empty); using all ",
        nrow(run_file), " rows\n", sep = "")
  }

  run_file
}

#' @rdname eva_step2_load_prepared_inputs
#' @export
eva_step1_prepare_inputs <- function(config, date_stamp = NULL,
                                     shared_connection = NULL, verbose = TRUE) {
  eva_step2_load_prepared_inputs(config = config, date_stamp = date_stamp, verbose = verbose)
}

#' Write evaluation results to filesystem or database
#'
#' @noRd
eva_write_step1_results <- function(config, eva_output_path, group_id, eva_sim_id,
                                    original_sample_id, partition_id, scaling_mode,
                                    site_results, runtime_info, verbose = TRUE) {
  run_type <- eva_result_run_type(config)

  daily_output_file <- NULL
  agg_output_file <- NULL
  annual_output_file <- NULL
  weighted_mean_output_file <- NULL
  runtime_file <- NULL

  if (identical(run_type, "file_system")) {
    daily_output_dir <- file.path(eva_output_path, "Daily_Outputs", paste0("jobGroup_", group_id))
    agg_output_dir <- file.path(eva_output_path, "Aggregated_Outputs", paste0("jobGroup_", group_id))
    annual_output_dir <- file.path(eva_output_path, "Annual_Outputs", paste0("jobGroup_", group_id))
    weighted_mean_output_dir <- file.path(eva_output_path, "Weighted_Mean_Outputs", paste0("jobGroup_", group_id))
    run_status_dir <- file.path(eva_output_path, "Run_Status", paste0("jobGroup_", group_id))

    dir.create(daily_output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(agg_output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(annual_output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(weighted_mean_output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(run_status_dir, recursive = TRUE, showWarnings = FALSE)

    daily_output_file <- file.path(daily_output_dir, paste0("dc_dRslt_", eva_sim_id, ".rds"))
    if (!is.null(site_results$daily_results) && nrow(site_results$daily_results) > 0) {
      saveRDS(site_results$daily_results, daily_output_file)
    }

    agg_output_file <- file.path(agg_output_dir, paste0("dc_aggRslt_", eva_sim_id, ".rds"))
    if (!is.null(site_results$aggregated_results) &&
        is.data.frame(site_results$aggregated_results) &&
        nrow(site_results$aggregated_results) > 0 &&
        ncol(site_results$aggregated_results) > 1) {
      saveRDS(site_results$aggregated_results, agg_output_file)
    }

    if (scaling_mode && !is.null(partition_id)) {
      annual_output_file <- file.path(
        annual_output_dir,
        paste0("dc_annualRslt_", eva_sim_id, "_part", partition_id, ".rds")
      )
    } else {
      annual_output_file <- file.path(
        annual_output_dir,
        paste0("dc_annualRslt_", eva_sim_id, ".rds")
      )
    }
    if (!is.null(site_results$annual_results) && nrow(site_results$annual_results) > 0) {
      saveRDS(site_results$annual_results, annual_output_file)
    }

    weighted_mean_output_file <- file.path(
      weighted_mean_output_dir,
      paste0("dc_weightedMeanRslt_", eva_sim_id, ".rds")
    )
    if (!is.null(site_results$weighted_mean_results) &&
        is.data.frame(site_results$weighted_mean_results) &&
        nrow(site_results$weighted_mean_results) > 0) {
      saveRDS(site_results$weighted_mean_results, weighted_mean_output_file)
    }

    runtime_file <- file.path(run_status_dir, paste0("RunTime_EVA_Sim_", eva_sim_id, ".csv"))
    write.csv(runtime_info, runtime_file, row.names = FALSE)

    if (verbose) {
      cat("Wrote evaluation results to filesystem under", eva_output_path, "\n")
    }

  } else if (identical(run_type, "database")) {
    db_cfg <- config$file_source$database$database_result
    tbls <- if (!is.null(db_cfg) && "tables" %in% names(db_cfg)) db_cfg$tables else list()

    if (!requireNamespace("DBI", quietly = TRUE)) {
      stop("DBI package is required to write evaluation results to the database")
    }
    has_mariadb <- requireNamespace("RMariaDB", quietly = TRUE)
    has_mysql <- requireNamespace("RMySQL", quietly = TRUE)
    if (!has_mariadb && !has_mysql) {
      stop("RMariaDB or RMySQL is required to write evaluation results to the database")
    }

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
      stop("Credential file for database_result not found: ", cred_path)
    }
    cred <- readLines(cred_path, warn = FALSE)
    if (length(cred) < 2) {
      stop("Credential file for database_result must contain at least 2 lines (username, password)")
    }
    username <- trimws(cred[1])
    password <- trimws(cred[2])

    con <- if (has_mariadb) {
      DBI::dbConnect(RMariaDB::MariaDB(), host = host, dbname = database,
                     username = username, password = password)
    } else {
      DBI::dbConnect(RMySQL::MySQL(), host = host, dbname = database,
                     username = username, password = password)
    }
    on.exit(try(DBI::dbDisconnect(con), silent = TRUE), add = TRUE)

    if (scaling_mode && !is.null(partition_id)) {
      if (!is.null(site_results$annual_results) && nrow(site_results$annual_results) > 0 &&
          "eva_results_annual" %in% names(tbls)) {
        annual_table <- paste0("part", partition_id, "_", tbls$eva_results_annual)
        annual_to_write <- site_results$annual_results
        annual_to_write$job_group <- group_id
        annual_to_write$simulation_id <- eva_sim_id
        annual_to_write$partition_id <- partition_id
        annual_to_write$original_sample_id <- original_sample_id
        DBI::dbWriteTable(con, annual_table, annual_to_write, append = TRUE, row.names = FALSE)
      }
    } else if (!is.null(site_results$annual_results) && nrow(site_results$annual_results) > 0 &&
               "eva_results_annual" %in% names(tbls)) {
      annual_to_write <- site_results$annual_results
      annual_to_write$job_group <- group_id
      annual_to_write$simulation_id <- eva_sim_id
      annual_to_write$original_sample_id <- original_sample_id
      DBI::dbWriteTable(con, tbls$eva_results_annual, annual_to_write,
                        append = TRUE, row.names = FALSE)
    }

    if (!is.null(site_results$daily_results) && nrow(site_results$daily_results) > 0 &&
        "eva_results_daily" %in% names(tbls) && !is.null(tbls$eva_results_daily)) {
      daily_to_write <- site_results$daily_results
      daily_to_write$job_group <- group_id
      daily_to_write$simulation_id <- eva_sim_id
      daily_to_write$original_sample_id <- original_sample_id
      DBI::dbWriteTable(con, tbls$eva_results_daily, daily_to_write,
                        append = TRUE, row.names = FALSE)
    }

    if (!is.null(runtime_info) && nrow(runtime_info) > 0 &&
        "eva_results_run_status" %in% names(tbls) && !is.null(tbls$eva_results_run_status)) {
      run_status_to_write <- runtime_info
      run_status_to_write$job_group <- group_id
      run_status_to_write$simulation_id <- eva_sim_id
      run_status_to_write$partition_id <- partition_id
      DBI::dbWriteTable(con, tbls$eva_results_run_status, run_status_to_write,
                        append = TRUE, row.names = FALSE)
    }

    if (!is.null(site_results$weighted_mean_results) &&
        is.data.frame(site_results$weighted_mean_results) &&
        nrow(site_results$weighted_mean_results) > 0 &&
        "eva_results_weighted" %in% names(tbls) && !is.null(tbls$eva_results_weighted)) {
      w_to_write <- site_results$weighted_mean_results
      w_to_write$job_group <- group_id
      w_to_write$simulation_id <- eva_sim_id
      w_to_write$original_sample_id <- original_sample_id
      DBI::dbWriteTable(con, tbls$eva_results_weighted, w_to_write,
                        append = TRUE, row.names = FALSE)
    }

    if (verbose) {
      cat("Wrote evaluation results to database_result tables\n")
    }
  } else {
    warning("Unknown evaluation result run_type '", run_type,
            "'; expected file_system or database")
  }

  list(
    daily_output = daily_output_file,
    aggregated_output = agg_output_file,
    annual_output = annual_output_file,
    weighted_mean_output = weighted_mean_output_file,
    runtime_info = runtime_file,
    run_type = run_type
  )
}

#' Evaluation Step 2: simulate one posterior draw on evaluation sites
#'
#' Updates DayCent parameters from one row of the Step 1 posterior table
#' (\code{mc_EVA_draw.rds}, sourced from \code{best_param_set_{model}.csv})
#' and runs the evaluation-site chain. \code{sim_id} is the 1-based row index.
#' Requires \code{eva_step1_setup()} to have been run.
#'
#' @param config YAML configuration loaded with \code{read_yaml_config()}
#' @param sim_id Evaluation draw index (1 to number of posterior samples)
#' @param daycent_exe Path to DayCent executable
#' @param scratch_dir Scratch directory for temporary simulation files
#' @param clean_scratch Logical, remove scratch files after the run
#' @param start_id Offset added to \code{sim_id} (default 0)
#' @param partition_id_override Optional scaling-mode partition
#' @param shared_connection Optional shared database connection
#' @param verbose Logical
#' @return List with status, results, runtime_info, and output_files
#' @export
eva_step2_simulate_individual <- function(config, sim_id, daycent_exe, scratch_dir,
                                          clean_scratch = TRUE, start_id = 0,
                                          partition_id_override = NULL,
                                          shared_connection = NULL, verbose = TRUE) {
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE

  config <- eva_apply_config_overrides(config)

  scaling_mode <- is_scaling_mode_enabled(config)
  actual_eva_id <- start_id + sim_id

  if (scaling_mode && !is.null(partition_id_override)) {
    partition_id <- partition_id_override
  } else {
    partition_id <- NULL
  }

  output_result <- list(
    status = 1,
    daily_results = NULL,
    annual_results = NULL,
    aggregated_results = NULL,
    weighted_mean_results = NULL,
    runtime_info = NULL,
    sim_id = actual_eva_id,
    original_sample_id = NA_integer_,
    partition_id = partition_id,
    scaling_mode = scaling_mode,
    output_files = list()
  )
  sim_dir_tid <- NULL

  if (verbose) {
    cat("====================================================================\n")
    cat("Evaluation Step 2 - Individual Simulation\n")
    cat("Node:", node, "\n")
    cat("Working Directory:", getwd(), "\n\n")
    cat("Configuration:\n")
    cat("\t Project Name                :", config$project$name, "\n")
    cat("\t File source mode            :", eva_null_coalesce(config$file_source$mode, "filesystem"), "\n")
    cat("\t Result run type             :", eva_result_run_type(config), "\n")
    cat("\t Execution Mode              :", if (scaling_mode) "SCALING" else "LEGACY", "\n")
    cat("\t Evaluation sim ID           :", actual_eva_id, "\n")
    if (!is.null(partition_id)) {
      cat("\t Partition ID (filtering)    :", partition_id, "\n")
    }
    cat("\t DayCent Executable          :", daycent_exe, "\n")
    cat("\t Scratch Directory           :", scratch_dir, "\n")
    cat("\t Clean Scratch               :", clean_scratch, "\n")
    cat("\t Evaluation sites dir        :", config$paths$expsites_dir, "\n")
    cat("\t Evaluation dot100 dir       :", config$paths$dot100_path, "\n\n")
    cat("====================================================================\n")
  }

  tryCatch({
    prepared <- eva_step2_load_prepared_inputs(
      config = config,
      date_stamp = NULL,
      verbose = verbose
    )
    eva_output_path <- prepared$eva_output_path
    runFile <- prepared$run_file
    posterior_jobs <- prepared$posterior_jobs

    if (!(actual_eva_id %in% posterior_jobs$EvaSimID)) {
      stop("Evaluation sim_id ", actual_eva_id,
           " not found in posterior sample (n = ", nrow(posterior_jobs), ")")
    }

    job_params <- posterior_jobs[posterior_jobs$EvaSimID == actual_eva_id, , drop = FALSE]
    original_sample_id <- job_params$SampleID[1]
    group_id <- job_params$EvaJobGroup[1]
    output_result$original_sample_id <- original_sample_id

    if (verbose) {
      cat("Posterior EvaSimID:", actual_eva_id,
          "| original SIR SampleID:", original_sample_id,
          "| EvaJobGroup:", group_id, "\n")
    }

    job_params_for_prep <- eva_align_job_params_to_prior(config, job_params)
    params_df <- sir_step2_prepare_parameters(config, job_params_for_prep, verbose = verbose)

    sim_dir_tid <- file.path(scratch_dir, paste0("EVA_SIM_", actual_eva_id))
    dir.create(sim_dir_tid, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(sim_dir_tid)) {
      parent_dir <- dirname(sim_dir_tid)
      stop(
        "Failed to create simulation scratch directory: ", sim_dir_tid,
        "\n  Parent exists: ", dir.exists(parent_dir),
        " (", parent_dir, ")",
        "\n  Check cluster.scratch_dir in the YAML is present and writable on this node."
      )
    }

    if (verbose) cat("RunFile loaded with", nrow(runFile), "treatment records\n")

    # actual_sim_id is written into output SampleID; keep the original SIR SampleID
    # so evaluation outputs join back to the posterior sample.
    site_results <- sir_step2_run_sites(
      config = config,
      params_df = params_df,
      sim_dir_tid = sim_dir_tid,
      daycent_exe = daycent_exe,
      actual_sim_id = original_sample_id,
      runFile = runFile,
      sir_output_path = eva_output_path,
      group_id = group_id,
      partition_id = partition_id,
      shared_connection = shared_connection,
      verbose = verbose
    )

    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))

    runtime_info <- data.frame(
      EvaSimID = actual_eva_id,
      SampleID = original_sample_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = nrow(runFile),
      sim_years = site_results$num_sim_years,
      obs_years = site_results$num_obs_years,
      status = 0,
      message = "Execution Success..",
      stringsAsFactors = FALSE
    )

    written <- eva_write_step1_results(
      config = config,
      eva_output_path = eva_output_path,
      group_id = group_id,
      eva_sim_id = actual_eva_id,
      original_sample_id = original_sample_id,
      partition_id = partition_id,
      scaling_mode = scaling_mode,
      site_results = site_results,
      runtime_info = runtime_info,
      verbose = verbose
    )

    if (clean_scratch && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }

    complete <- TRUE
    output_result$status <- 0
    output_result$daily_results <- site_results$daily_results
    output_result$annual_results <- site_results$annual_results
    output_result$aggregated_results <- site_results$aggregated_results
    output_result$weighted_mean_results <- site_results$weighted_mean_results
    output_result$runtime_info <- runtime_info

    output_files_list <- list()
    if (!is.null(site_results$daily_results) && nrow(site_results$daily_results) > 0) {
      output_files_list$daily_output <- written$daily_output
    }
    if (!is.null(site_results$aggregated_results) &&
        is.data.frame(site_results$aggregated_results) &&
        nrow(site_results$aggregated_results) > 0 &&
        ncol(site_results$aggregated_results) > 1) {
      output_files_list$aggregated_output <- written$aggregated_output
    }
    if (!is.null(site_results$annual_results) && nrow(site_results$annual_results) > 0) {
      output_files_list$annual_output <- written$annual_output
    }
    if (!is.null(site_results$weighted_mean_results) &&
        is.data.frame(site_results$weighted_mean_results) &&
        nrow(site_results$weighted_mean_results) > 0) {
      output_files_list$weighted_mean_output <- written$weighted_mean_output
    }
    if (!is.null(written$runtime_info)) {
      output_files_list$runtime_info <- written$runtime_info
    }
    output_result$output_files <- output_files_list

    if (verbose) cat("--- Evaluation Step 2 Individual Simulation Successfully Completed ---\n")

  }, error = function(err) {
    if (verbose) {
      cat("--- Evaluation Step 2 Individual Simulation Failed ---\n")
      cat("Error:", as.character(err), "\n")
    }

    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    runtime_info <- data.frame(
      EvaSimID = actual_eva_id,
      SampleID = output_result$original_sample_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = 0,
      sim_years = 0,
      obs_years = 0,
      status = 1,
      message = as.character(err),
      stringsAsFactors = FALSE
    )
    output_result$runtime_info <<- runtime_info

    eva_output_path <- tryCatch(eva_output_dir(config, NULL), error = function(e) NULL)
    if (!is.null(eva_output_path)) {
      run_status_dir <- file.path(eva_output_path, "Run_Status")
      dir.create(run_status_dir, recursive = TRUE, showWarnings = FALSE)
      runtime_file <- file.path(run_status_dir, paste0("RunTime_EVA_Sim_", actual_eva_id, "_error.csv"))
      try(write.csv(runtime_info, runtime_file, row.names = FALSE), silent = TRUE)
      output_result$output_files$runtime_info <<- runtime_file
    }

    if (clean_scratch && !is.null(sim_dir_tid) && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
  })

  output_result
}

#' @rdname eva_step2_simulate_individual
#' @export
eva_step1_simulate_individual <- eva_step2_simulate_individual

