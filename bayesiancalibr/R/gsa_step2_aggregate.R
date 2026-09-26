#' GSA Step 2 Aggregate: Weighted Mean Aggregation
#'
#' Performs weighted mean aggregation on annual results from GSA Step 2 simulations
#' in scaling mode. This function loads annual results from all job groups and samples,
#' applies weighted mean aggregation based on aggregation_level and aggregation_weight,
#' and saves the aggregated results to the Weighted_Mean_Outputs directory.
#'
#' @param config Configuration object loaded with read_yaml_config()
#' @param gsa_method GSA method name ("soboljansen", "sobol", "morris", etc.)
#' @param sample_ids Optional vector of sample IDs to process. If NULL, processes all samples found.
#' @param verbose Logical indicating whether to print progress messages
#' @return List with status information and summary statistics
#' @export
gsa_step2_aggregate <- function(config, gsa_method, sample_ids = NULL, verbose = TRUE) {

  # Initialize timing
  start_time <- Sys.time()

  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 2 Aggregate - Weighted Mean Aggregation\n")
    cat("====================================================================\n")
    cat("Project:", config$project$name, "\n")
    cat("GSA Method:", gsa_method, "\n")
    cat("Start Time:", format(start_time), "\n\n")
  }

  # Determine date stamp
  if (config$project$date_stamp == "auto") {
    date_stamp <- format(Sys.Date(), "%d%b%Y")
  } else {
    date_stamp <- config$project$date_stamp
  }

  # Setup paths
  gsa_output_path <- file.path(config$paths$lairice_root, "results", config$project$name,
                               date_stamp, "GSA", gsa_method)

  if (!dir.exists(gsa_output_path)) {
    stop("GSA output path not found: ", gsa_output_path)
  }

  # Load RunFile for aggregation metadata
  run_file_path <- file.path(gsa_output_path, "RunFile.rds")
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

  # Always define counters so the final summary/return can't fail
  # when `run_type` is misconfigured or a branch exits early.
  samples_processed <- 0
  samples_failed <- 0

  if (identical(run_type, "file_system")) {
    # Find all Annual_Outputs directories
    annual_output_base <- file.path(gsa_output_path, "Annual_Outputs")
    if (!dir.exists(annual_output_base)) {
      stop("Annual_Outputs directory not found: ", annual_output_base)
    }

    # Get all job group directories
    job_group_dirs <- list.dirs(annual_output_base, recursive = FALSE, full.names = TRUE)
    job_group_dirs <- job_group_dirs[grepl("jobGroup_", basename(job_group_dirs))]

    if (length(job_group_dirs) == 0) {
      stop("No job group directories found in Annual_Outputs")
    }

    if (verbose) {
      cat("Found", length(job_group_dirs), "job group directories\n\n")
    }

    # Collect all available sample IDs if not specified
    if (is.null(sample_ids)) {
      sample_ids <- c()
      for (job_dir in job_group_dirs) {
        files <- list.files(job_dir, pattern = "^dc_annualRslt_\\d+\\.rds$", full.names = FALSE)
        ids <- as.integer(sub("dc_annualRslt_(\\d+)\\.rds", "\\1", files))
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
    
  # Process each sample
  samples_processed <- 0
  samples_failed <- 0

  for (sample_id in sample_ids) {

    if (verbose) {
      cat("Processing sample", sample_id, "...\n")
    }

    tryCatch({

      # Find ALL partition files for this sample across all job groups
      # Support both legacy format (single file) and partition format (multiple files)
      all_partition_files <- c()

      for (job_dir in job_group_dirs) {
        # Look for legacy format first
        legacy_file <- file.path(job_dir, paste0("dc_annualRslt_", sample_id, ".rds"))
        
        # Look for partition format
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

      # Load and merge all partition files
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

      # Combine all partitions
      annual_results <- do.call(rbind, annual_data_list)

      if (is.null(annual_results) || nrow(annual_results) == 0) {
        if (verbose) {
          cat("  Warning: Empty combined results for sample", sample_id, "- skipping\n")
        }
        samples_failed <- samples_failed + 1
        next
      }

      # Determine job group from first file for output organization
      group_id <- sub("jobGroup_", "", basename(dirname(all_partition_files[1])))

      # Create weighted output directory
      weighted_output_dir <- file.path(gsa_output_path, "Weighted_Mean_Outputs", paste0("jobGroup_", group_id))
      dir.create(weighted_output_dir, recursive = TRUE, showWarnings = FALSE)

      # Perform weighted aggregation using existing function
      method <- "GSA"
      weighted_results <- process_weighted_mean_aggregation_for_sample(
        config = config,
        target_crop = get_pipeline_target_crop(config, "gsa"),
        annual_results = annual_results,
        sample_id = sample_id,
        job_group = group_id,
        weighted_output_dir = weighted_output_dir,
        output_specs = output_specs$variables,
        method = method,
        verbose = FALSE
      )

      # Cleanup: Delete annual partition files if annual_output is FALSE for all weighted variables
      # This saves disk space after aggregation is complete
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
    samples_processed <- 0
    sample_id = sample_ids

    # Set up paths
    lairice_root <- config$paths$lairice_root
    output_base <- if (!is.null(config$paths$output_base)) config$paths$output_base else "."
    gsa_method_dir <- file.path(lairice_root, output_base, date_stamp, "GSA", gsa_method)
    
    # Read parameter set data
    mc_file <- file.path(gsa_method_dir, paste0("mc_GSA_draw_", gsa_method, ".rds"))
    if (!file.exists(mc_file)) {
      stop("Monte Carlo draw file not found: ", mc_file)
    }
    
    all_jobs <- readRDS(mc_file)
    job_parms <- all_jobs[all_jobs$SampleID == sample_id, ]
    if (nrow(job_parms) == 0) {
      stop("Sample ID ", sample_id, " not found in parameter set")
    }
    group_id <- job_parms$JobGroup
    rm(all_jobs, job_parms)
    cred_file <- path.expand("~/.dblogin")
    if (!file.exists(cred_file)) {
    stop("Credential file not found: ", cred_file)
    }

    weighted_output_dir <- file.path(gsa_output_path, "Weighted_Mean_Outputs", paste0("jobGroup_", group_id))

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
    annual_table = config$file_source$database$database_result$tables$gsa_results_annual
    gsa_results_weighted = config$file_source$database$database_result$tables$gsa_results_weighted

    dbConn_result <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = host, dbname = db_calib,
        username = user, password = password
    )
    on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE)
    task_id = config$gsa$task_id
    point_assisnments_path <- file.path(config$paths$lairice_root, "results", config$project$name, config$project$date_stamp, "GSA", config$gsa$gsa_methods[task_id], "point_assignments.rds")
    point_assisnments_file <- readRDS(point_assisnments_path)
    partition_id <- unique(point_assisnments_file$total_jobs)
    rm(point_assisnments_file)

    # Load annual results from database
    annual_results_all <- NULL
    for(part_id in 1:partition_id) {

      annual_table = paste0("part", part_id, "_", config$file_source$database$database_result$tables$gsa_results_annual)

      annual_results <- DBI::dbGetQuery(dbConn_result, paste0("SELECT * FROM ", annual_table, 
                      " WHERE simulation_id IN (", paste(sample_id, collapse = ", "), ") ;"))
      annual_results <- unique(annual_results)
      annual_results_all <- rbind(annual_results_all, annual_results)
      rm(annual_results)
    }
    annual_results_all <- unique(annual_results_all)

    DBI::dbDisconnect(dbConn_result)

   # Aggregate the annual results
   method <- "GSA"
   weighted_results <- process_weighted_mean_aggregation_for_sample(
    config = config,
    target_crop = get_pipeline_target_crop(config, "gsa"),
    annual_results = annual_results_all,
    sample_id = sample_id,
    job_group = group_id,
    weighted_output_dir = weighted_output_dir,
    output_specs = output_specs$variables,
    method = method,
    verbose = FALSE
   )
   samples_processed <- samples_processed + 1

  } else {
    stop("Invalid run_type: ", run_type, ". Expected 'file_system' or 'database'.")
  }

  # Final summary
  end_time <- Sys.time()
  elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  if (verbose) {
    cat("\n====================================================================\n")
    cat("GSA Step 2 Aggregate - Completed\n")
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
