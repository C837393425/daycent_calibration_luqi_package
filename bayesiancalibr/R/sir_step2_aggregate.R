#' SIR Step 2 Aggregate: Weighted Mean Aggregation
#'
#' Performs weighted mean aggregation on annual results from SIR Step 2 simulations
#' in scaling mode. This function loads annual results from all job groups and samples,
#' applies weighted mean aggregation based on aggregation_level and aggregation_weight,
#' and saves the aggregated results to the Weighted_Mean_Outputs directory.
#'
#' @param config Configuration object loaded with read_yaml_config()
#' @param sample_ids Optional vector of sample IDs to process. If NULL, processes all samples found.
#' @param verbose Logical indicating whether to print progress messages
#' @return List with status information and summary statistics
#' @export
sir_step2_aggregate <- function(config, sample_ids = NULL, verbose = TRUE) {

  # Initialize timing
  start_time <- Sys.time()

  if (verbose) {
    cat("====================================================================\n")
    cat("SIR Step 2 Aggregate - Weighted Mean Aggregation\n")
    cat("====================================================================\n")
    cat("Project:", config$project$name, "\n")
    cat("Start Time:", format(start_time), "\n\n")
  }

  # Determine date stamp
  if (config$project$date_stamp == "auto") {
    date_stamp <- format(Sys.Date(), "%d%b%Y")
  } else {
    date_stamp <- config$project$date_stamp
  }

  # Setup paths
  sir_output_path <- file.path(config$paths$lairice_root, "results", config$project$name,
                               date_stamp, "SIR")

  if (!dir.exists(sir_output_path)) {
    stop("SIR output path not found: ", sir_output_path)
  }

  # Load RunFile for aggregation metadata
  run_file_path <- file.path(sir_output_path, "RunFile.rds")
  if (!file.exists(run_file_path)) {
    stop("RunFile.rds not found: ", run_file_path)
  }

  runFile <- readRDS(run_file_path)

  if (verbose) {
    cat("RunFile loaded with", nrow(runFile), "treatment records\n")
  }

  # Check if weighted aggregation is applicable
  if (!("aggregation_level" %in% names(runFile)) || !("aggregation_weight" %in% names(runFile))) {
    if (verbose) {
      cat("RunFile does not contain aggregation_level or aggregation_weight columns\n")
      cat("Weighted aggregation cannot be performed - exiting\n")
    }
    return(list(
      status = 0,
      message = "Weighted aggregation not applicable - missing aggregation columns",
      samples_processed = 0
    ))
  }

  # Load output specifications
  output_specs <- load_output_specifications(config, verbose = FALSE)

  # Check if any variables require weighted mean aggregation
  weighted_vars <- output_specs$variables[
    !is.na(output_specs$variables$weighted_mean_aggregation) &
    output_specs$variables$weighted_mean_aggregation == TRUE,
  ]

  if (nrow(weighted_vars) == 0) {
    if (verbose) {
      cat("No variables configured for weighted mean aggregation\n")
      cat("Exiting - no work to do\n")
    }
    return(list(
      status = 0,
      message = "No variables configured for weighted aggregation",
      samples_processed = 0
    ))
  }

  if (verbose) {
    cat("Found", nrow(weighted_vars), "variables configured for weighted mean aggregation\n")
    cat("Variables:", paste(weighted_vars$variable_name, collapse = ", "), "\n\n")
  }

  run_type <- config$file_source$database$database_result$run_type
  if (is.null(run_type) || length(run_type) == 0) {
    run_type <- "file_system"
    if (verbose) {
      cat("run_type not set in config; defaulting to file_system\n")
    }
  }

  samples_processed <- 0
  samples_failed <- 0

  if (identical(run_type, "file_system")) {
    annual_output_base <- file.path(sir_output_path, "Annual_Outputs")
    if (!dir.exists(annual_output_base)) {
      stop("Annual_Outputs directory not found: ", annual_output_base)
    }

    job_group_dirs <- list.dirs(annual_output_base, recursive = FALSE, full.names = TRUE)
    job_group_dirs <- job_group_dirs[grepl("jobGroup_", basename(job_group_dirs))]

    if (length(job_group_dirs) == 0) {
      stop("No job group directories found in Annual_Outputs")
    }

    if (verbose) {
      cat("Found", length(job_group_dirs), "job group directories\n\n")
    }

    if (is.null(sample_ids)) {
      sample_ids <- c()
      for (job_dir in job_group_dirs) {
        files <- list.files(job_dir, pattern = "^dc_annualRslt_\\d+(_part\\d+)?\\.rds$", full.names = FALSE)
        ids <- as.integer(sub("dc_annualRslt_(\\d+)(_part\\d+)?\\.rds", "\\1", files))
        sample_ids <- c(sample_ids, ids)
      }
      sample_ids <- sort(unique(sample_ids))

      if (verbose) {
        cat("Found", length(sample_ids), "samples to process\n")
      }
    }

    if (length(sample_ids) == 0) {
      stop("No samples found to process")
    }

    for (sample_id in sample_ids) {
      if (verbose) {
        cat("Processing sample", sample_id, "...\n")
        flush.console()
      }

      tryCatch({
        all_partition_files <- c()

        for (job_dir in job_group_dirs) {
          legacy_file <- file.path(job_dir, paste0("dc_annualRslt_", sample_id, ".rds"))
          partition_pattern <- paste0("dc_annualRslt_", sample_id, "_part.*\\.rds")
          partition_files <- list.files(job_dir, pattern = partition_pattern, full.names = TRUE)

          if (file.exists(legacy_file)) {
            all_partition_files <- c(all_partition_files, legacy_file)
          }
          if (length(partition_files) > 0) {
            all_partition_files <- c(all_partition_files, partition_files)
          }
        }

        if (length(all_partition_files) == 0) {
          if (verbose) {
            cat("  Warning: No annual results found for sample", sample_id, "- skipping\n")
          }
          samples_failed <- samples_failed + 1
          next
        }

        if (verbose && length(all_partition_files) > 1) {
          cat("  Merging", length(all_partition_files), "partition files for sample", sample_id, "\n")
        }

        annual_data_list <- lapply(all_partition_files, function(f) {
          tryCatch(readRDS(f), error = function(e) {
            if (verbose) cat("    Warning: Failed to read", basename(f), "\n")
            NULL
          })
        })
        annual_data_list <- annual_data_list[!sapply(annual_data_list, is.null)]

        if (length(annual_data_list) == 0) {
          if (verbose) {
            cat("  Warning: All partition files corrupted for sample", sample_id, "- skipping\n")
          }
          samples_failed <- samples_failed + 1
          next
        }

        annual_results <- do.call(rbind, annual_data_list)
        if (is.null(annual_results) || nrow(annual_results) == 0) {
          if (verbose) {
            cat("  Warning: Empty combined results for sample", sample_id, "- skipping\n")
          }
          samples_failed <- samples_failed + 1
          next
        }

        group_id <- sub("jobGroup_", "", basename(dirname(all_partition_files[1])))
        weighted_output_dir <- file.path(sir_output_path, "Weighted_Mean_Outputs", paste0("jobGroup_", group_id))
        dir.create(weighted_output_dir, recursive = TRUE, showWarnings = FALSE)

        method <- "SIR"
        weighted_results <- process_weighted_mean_aggregation_for_sample(
          config = config,
          target_crop = get_pipeline_target_crop(config, "sir"),
          annual_results = annual_results,
          sample_id = sample_id,
          job_group = group_id,
          weighted_output_dir = weighted_output_dir,
          output_specs = output_specs$variables,
          method = method,
          verbose = FALSE
        )

        should_cleanup <- all(weighted_vars$annual_output == FALSE)
        if (should_cleanup) {
          if (verbose) {
            cat("  Cleaning up annual partition files (annual_output=FALSE for all weighted vars)\n")
          }

          deleted_count <- 0
          for (partition_file in all_partition_files) {
            if (file.exists(partition_file)) {
              tryCatch({
                file.remove(partition_file)
                deleted_count <- deleted_count + 1
              }, error = function(e) {
                if (verbose) cat("    Warning: Failed to delete", basename(partition_file), "\n")
              })
            }
          }

          if (verbose) {
            cat("  Deleted", deleted_count, "of", length(all_partition_files), "partition files\n")
          }
        }

        samples_processed <- samples_processed + 1
        if (verbose && samples_processed %% 10 == 0) {
          cat("  Processed", samples_processed, "of", length(sample_ids), "samples\n")
        }
      }, error = function(e) {
        if (verbose) {
          cat("  Error processing sample", sample_id, ":", e$message, "\n")
        }
        samples_failed <- samples_failed + 1
      })
    }
  } else if (identical(run_type, "database")) {
    if (is.null(sample_ids) || length(sample_ids) == 0) {
      stop("sample_ids must be provided when run_type is 'database'")
    }

    cred_file <- path.expand(config$file_source$database$cred_file %||% "~/.dblogin")
    if (!file.exists(cred_file)) {
      stop("Credential file not found: ", cred_file)
    }
    cred <- readLines(cred_file, warn = FALSE)
    cred <- trimws(cred)
    cred <- cred[nzchar(cred)]
    if (length(cred) < 2) {
      stop("Credential file must contain at least two non-empty lines (user, password): ", cred_file)
    }
    user <- cred[1]
    password <- cred[2]

    host <- config$file_source$database$host
    db_calib <- config$file_source$database$database
    annual_table <- config$file_source$database$database_result$tables$sir_results_annual

    db_con <- DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = host,
      dbname = db_calib,
      username = user,
      password = password
    )
    on.exit(DBI::dbDisconnect(db_con), add = TRUE)

    # Discover annual tables to support scaling mode partition writes:
    # - base: sir_results_annual
    # - partitions: part{N}_sir_results_annual
    all_annual_tables <- c()
    part_like <- paste0("part%_", annual_table)
    part_rows <- DBI::dbGetQuery(db_con, paste0("SHOW TABLES LIKE '", part_like, "'"))
    if (nrow(part_rows) > 0) {
      part_tables <- as.character(part_rows[[1]])
      all_annual_tables <- unique(c(all_annual_tables, part_tables))
    }

    if (verbose) {
      cat("Using annual tables:", paste(all_annual_tables, collapse = ", "), "\n")
    }

    for (sample_id in sample_ids) {
      if (verbose) {
        cat("Processing sample", sample_id, "...\n")
      }

      tryCatch({
        annual_results <- NULL
        for (tbl in all_annual_tables) {
          tbl_rows <- DBI::dbGetQuery(
            db_con,
            paste0("SELECT * FROM ", tbl, " WHERE simulation_id = ", as.integer(sample_id), " ;")
          )
          if (!is.null(tbl_rows) && nrow(tbl_rows) > 0) {
            annual_results <- rbind(annual_results, tbl_rows)
          }
        }
        DBI::dbDisconnect(db_con)
        
        annual_results <- unique(annual_results)

        if (is.null(annual_results) || nrow(annual_results) == 0) {
          if (verbose) cat("  Warning: No annual rows found for sample", sample_id, "- skipping\n")
          samples_failed <- samples_failed + 1
          next
        }

        if (!("job_group" %in% names(annual_results))) {
          stop("annual results table must include 'job_group' column for SIR database aggregation")
        }

        group_id <- unique(annual_results$job_group)[1]
        weighted_output_dir <- file.path(sir_output_path, "Weighted_Mean_Outputs", paste0("jobGroup_", group_id))
        dir.create(weighted_output_dir, recursive = TRUE, showWarnings = FALSE)
        
        method <- "SIR"
        weighted_results <- process_weighted_mean_aggregation_for_sample(
          config = config,
          target_crop = get_pipeline_target_crop(config, "sir"),
          annual_results = annual_results,
          sample_id = sample_id,
          job_group = group_id,
          weighted_output_dir = weighted_output_dir,
          output_specs = output_specs$variables,
          method = method,
          verbose = FALSE
        )

        samples_processed <- samples_processed + 1
      }, error = function(e) {
        if (verbose) {
          cat("  Error processing sample", sample_id, ":", e$message, "\n")
        }
        samples_failed <- samples_failed + 1
      })
    }
  } else {
    stop("Invalid run_type: ", run_type, ". Expected 'file_system' or 'database'.")
  }

  # Final summary
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  if (verbose) {
    cat("\n====================================================================\n")
    cat("SIR Step 2 Aggregate - Completed\n")
    cat("====================================================================\n")
    cat("Samples processed:", samples_processed, "\n")
    cat("Samples failed:", samples_failed, "\n")
    cat("Total samples:", length(sample_ids), "\n")
    cat("Elapsed time:", round(elapsed_time, 2), "seconds\n")
    cat("====================================================================\n")
  }

  return(list(
    status = if (samples_failed == 0) 0 else 1,
    message = if (samples_failed == 0) "All samples processed successfully" else paste(samples_failed, "samples failed"),
    samples_processed = samples_processed,
    samples_failed = samples_failed,
    total_samples = length(sample_ids),
    elapsed_time_sec = elapsed_time
  ))
}
