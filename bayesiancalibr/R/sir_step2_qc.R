#' SIR Step 2 Quality Control
#'
#' Monitors and validates SIR Step 2 simulation completion by aggregating run status
#' information across all job groups and providing detailed diagnostics.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param verbose Logical, whether to print progress messages
#'
#' @return List containing:
#'   \item{summary}{Overall completion summary}
#'   \item{run_status_df}{Combined run status dataframe}
#'   \item{job_groups}{Per-job group statistics}
#'   \item{diagnostics}{Detailed diagnostics and validation results}
#'   \item{validation}{Boolean indicating if QC passed}
#'   \item{recommendations}{Suggested next steps}
#'
#' @export
sir_step2_qc <- function(config, date_stamp = NULL, verbose = TRUE) {
  
  # Initialize timing and status
  start_time <- Sys.time()
  node <- Sys.info()[4]
  
  tryCatch({
    
    # Validate inputs
    if (missing(config)) stop("Configuration object is required")
    
    # Setup R library paths if specified
    if (!is.null(config$r_config$rlibpaths)) {
      .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
    }
    
    # Determine date stamp
    if (is.null(date_stamp)) {
      if (config$project$date_stamp == "auto") {
        date_stamp <- format(Sys.Date(), "%d%b%Y")
      } else {
        date_stamp <- config$project$date_stamp
      }
    }
    
    if (verbose) {
      cat("=== SIR Step 2 Quality Control ===\n")
      cat("Project:", config$project$name, "\n")
      cat("Date stamp:", date_stamp, "\n")
      cat("Node:", node, "\n")
      cat("Started at:", format(start_time), "\n\n")
    }
    
    # Set up paths
    lairice_root <- config$paths$lairice_root
    output_base <- if (!is.null(config$paths$output_base)) config$paths$output_base else "."
    sir_dir <- file.path(lairice_root, output_base, date_stamp, "SIR")
    
    # Check if SIR directory exists
    if (!dir.exists(sir_dir)) {
      stop("SIR directory does not exist: ", sir_dir)
    }
    
    # Read Monte Carlo draw data
    mc_file <- file.path(sir_dir, "mc_SIR_draw.rds")
    if (!file.exists(mc_file)) {
      stop("Monte Carlo draw file not found: ", mc_file)
    }
    
    X <- readRDS(mc_file)
    if (verbose) {
      cat("Monte Carlo draws loaded:", nrow(X), "simulations\n")
    }
    
    # Initialize run status collection
    rs_df <- NULL
    job_groups <- sort(unique(X$JobGroup))
    job_group_stats <- list()
    
    if (verbose) {
      cat("Processing", length(job_groups), "job groups...\n")
    }
    
    # Process each job group
    for (grp in job_groups) {
      if (verbose) {
        cat("  Processing job group", grp, "...")
      }
      
      # Run Status directory for this job group
      run_status_dir <- file.path(sir_dir, config$output_dirs$run_status, paste0("jobGroup_", grp))
      
      if (!dir.exists(run_status_dir)) {
        if (verbose) cat(" [MISSING DIRECTORY]\n")
        job_group_stats[[as.character(grp)]] <- list(
          job_group = grp,
          status = "missing_directory",
          n_files = 0,
          n_completed = 0,
          n_failed = 0,
          completion_rate = 0
        )
        next
      }
      
      # Get run status files
      rs_file_pattern <- "RunTime_SIR_Sim_"
      rs_file_lists <- list.files(path = run_status_dir, pattern = rs_file_pattern, full.names = TRUE)
      
      if (length(rs_file_lists) == 0) {
        if (verbose) cat(" [NO STATUS FILES]\n")
        job_group_stats[[as.character(grp)]] <- list(
          job_group = grp,
          status = "no_status_files",
          n_files = 0,
          n_completed = 0,
          n_failed = 0,
          completion_rate = 0
        )
        next
      }
      
      # Read and combine run status files
      temp_rs_list <- lapply(rs_file_lists, function(file) {
        tryCatch({
          read.csv(file, stringsAsFactors = FALSE)
        }, error = function(e) {
          if (verbose) cat(" [ERROR reading", basename(file), "]")
          NULL
        })
      })
      
      # Remove NULL entries (failed reads)
      temp_rs_list <- temp_rs_list[!sapply(temp_rs_list, is.null)]
      
      if (length(temp_rs_list) > 0) {
        temp_rs <- do.call(rbind, temp_rs_list)
        rs_df <- rbind(rs_df, temp_rs)
        
        # Calculate job group statistics
        n_completed <- sum(temp_rs$status == 0, na.rm = TRUE)
        n_failed <- sum(temp_rs$status != 0, na.rm = TRUE)
        n_total <- nrow(temp_rs)
        completion_rate <- n_completed / n_total * 100
        
        job_group_stats[[as.character(grp)]] <- list(
          job_group = grp,
          status = "processed",
          n_files = length(rs_file_lists),
          n_total = n_total,
          n_completed = n_completed,
          n_failed = n_failed,
          completion_rate = completion_rate
        )
        
        if (verbose) {
          cat(" [", n_completed, "/", n_total, " completed (", round(completion_rate, 1), "%)]\n")
        }
      } else {
        if (verbose) cat(" [ALL FILES CORRUPTED]\n")
        job_group_stats[[as.character(grp)]] <- list(
          job_group = grp,
          status = "corrupted_files",
          n_files = length(rs_file_lists),
          n_completed = 0,
          n_failed = 0,
          completion_rate = 0
        )
      }
    }
    
    # Generate overall summary
    if (is.null(rs_df) || nrow(rs_df) == 0) {
      summary <- list(
        total_expected = nrow(X),
        total_processed = 0,
        total_completed = 0,
        total_failed = 0,
        overall_completion_rate = 0,
        status = "no_data"
      )
      validation_passed <- FALSE
      recommendations <- c("No simulation data found", "Check if Step 2 has been executed", "Verify file paths and permissions")
    } else {
      total_completed <- sum(rs_df$status == 0, na.rm = TRUE)
      total_failed <- sum(rs_df$status != 0, na.rm = TRUE)
      total_processed <- nrow(rs_df)
      overall_completion_rate <- total_completed / nrow(X) * 100
      
      summary <- list(
        total_expected = nrow(X),
        total_processed = total_processed,
        total_completed = total_completed,
        total_failed = total_failed,
        overall_completion_rate = overall_completion_rate,
        status = ifelse(total_processed == nrow(X), "complete", "incomplete")
      )
      
      # Validation criteria
      validation_passed <- (total_completed >= nrow(X) * 0.95) && (total_failed < nrow(X) * 0.05)
      
      # Generate recommendations
      recommendations <- c()
      if (total_processed < nrow(X)) {
        recommendations <- c(recommendations, paste("Missing", nrow(X) - total_processed, "simulations"))
      }
      if (total_failed > 0) {
        recommendations <- c(recommendations, paste("Investigate", total_failed, "failed simulations"))
      }
      if (validation_passed) {
        recommendations <- c(recommendations, "Ready to proceed to Step 3")
      } else {
        recommendations <- c(recommendations, "Address failures before proceeding to Step 3")
      }
    }
    
    # Generate diagnostics
    diagnostics <- list(
      job_groups_processed = length(job_groups),
      job_groups_with_data = sum(sapply(job_group_stats, function(x) x$status == "processed")),
      average_completion_rate = mean(sapply(job_group_stats, function(x) x$completion_rate)),
      min_completion_rate = min(sapply(job_group_stats, function(x) x$completion_rate)),
      max_completion_rate = max(sapply(job_group_stats, function(x) x$completion_rate)),
      runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    )
    
    # Save aggregated run status
    output_file <- file.path(sir_dir, "SIR_Step2_Run_Status_Combined.rds")
    if (!is.null(rs_df)) {
      saveRDS(rs_df, output_file)
      if (verbose) {
        cat("\nRun status saved to:", output_file, "\n")
      }
    }
    
    # Print summary
    if (verbose) {
      cat("\n=== Quality Control Summary ===\n")
      cat("Total simulations expected:", summary$total_expected, "\n")
      cat("Total simulations processed:", summary$total_processed, "\n")
      cat("Total simulations completed:", summary$total_completed, "\n")
      cat("Total simulations failed:", summary$total_failed, "\n")
      cat("Overall completion rate:", round(summary$overall_completion_rate, 2), "%\n")
      cat("Validation passed:", validation_passed, "\n")
      cat("\nRecommendations:\n")
      for (rec in recommendations) {
        cat("  -", rec, "\n")
      }
      cat("\nQC completed in", round(diagnostics$runtime_seconds, 2), "seconds\n")
    }
    
    # Return comprehensive results
    return(list(
      summary = summary,
      run_status_df = rs_df,
      job_groups = job_group_stats,
      diagnostics = diagnostics,
      validation = validation_passed,
      recommendations = recommendations,
      output_file = if (!is.null(rs_df)) output_file else NULL,
      runtime_info = list(
        start_time = start_time,
        end_time = Sys.time(),
        node = node,
        runtime_seconds = diagnostics$runtime_seconds
      )
    ))
    
  }, error = function(e) {
    # Error handling
    error_msg <- paste("SIR Step 2 QC failed:", e$message)
    if (verbose) {
      cat("ERROR:", error_msg, "\n")
    }
    
    return(list(
      summary = list(status = "error", message = error_msg),
      run_status_df = NULL,
      job_groups = list(),
      diagnostics = list(),
      validation = FALSE,
      recommendations = c("Check error message", "Verify file paths", "Check configuration"),
      output_file = NULL,
      runtime_info = list(
        start_time = start_time,
        end_time = Sys.time(),
        node = node,
        error = error_msg
      )
    ))
  })
}