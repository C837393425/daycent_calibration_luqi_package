#' SIR Step 3: Likelihood Calculation
#'
#' Function for calculating likelihood values by comparing model outputs 
#' with observations across multiple variables and measurement types for SIR.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param task_id Task ID for the parameter set to process
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param verbose Logical, whether to print progress messages
#'
#' @return List containing:
#'   \item{gofs_df}{Goodness-of-fit dataframe with all variable results}
#'   \item{runtime_info}{Execution timing and node information}
#'   \item{validation}{Boolean indicating if calculation succeeded}
#'   \item{output_files}{List of created output files}
#'
#' @export
sir_step3_likelihood <- function(config, task_id, date_stamp = NULL, verbose = TRUE) {
  
  # Initialize timing and status
  start_time <- Sys.time()
  node <- Sys.info()[4]
  
  tryCatch({
    
    # Validate inputs
    if (missing(config)) stop("Configuration object is required")
    if (missing(task_id)) stop("task_id is required")
    
    # Setup R library paths if specified
    if (!is.null(config$r_config$rlibpaths)) {
      .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
    }
    
    # Load required libraries
    library(reshape2)
    library(lme4)
    
    # Determine date stamp
    if (is.null(date_stamp)) {
      if (config$project$date_stamp == "auto") {
        date_stamp <- format(Sys.Date(), "%d%b%Y")
      } else {
        date_stamp <- config$project$date_stamp
      }
    }
    
    if (verbose) {
      cat("=== SIR Step 3: Likelihood Calculation ===\n")
      cat("Project:", config$project$name, "\n")
      cat("Date stamp:", date_stamp, "\n")
      cat("Task ID:", task_id, "\n")
      cat("Node:", node, "\n")
      cat("Started at:", format(start_time), "\n\n")
    }
    
    # Set up paths - SIR uses different directory structure than GSA
    lairice_root <- config$paths$lairice_root
    output_base <- if (!is.null(config$paths$output_base)) config$paths$output_base else "."
    sir_method_dir <- file.path(lairice_root, output_base, date_stamp, "SIR")
    
    # Read parameter set data - SIR uses different file naming
    mc_file <- file.path(sir_method_dir, "mc_SIR_draw.rds")
    if (!file.exists(mc_file)) {
      stop("Monte Carlo draw file not found: ", mc_file)
    }
    
    all_jobs <- readRDS(mc_file)
    job_parms <- all_jobs[all_jobs$SampleID == task_id, ]
    if (nrow(job_parms) == 0) {
      stop("Task ID ", task_id, " not found in parameter set")
    }
    group_id <- job_parms$JobGroup
    
    run_type <- config$file_source$database$database_result$run_type
    if (is.null(run_type) || length(run_type) == 0) {
      run_type <- "file_system"
    }
    
    # Set up directory paths
    bmaf_srs_dir <- file.path(lairice_root, config$paths$bmaf_srs_dir)
    daily_out_dir <- file.path(sir_method_dir, config$output_dirs$daily_outputs, paste0("jobGroup_", group_id))
    agg_out_dir <- file.path(sir_method_dir, config$output_dirs$aggregated_outputs, paste0("jobGroup_", group_id))
    annual_out_dir <- file.path(sir_method_dir, config$output_dirs$annual_outputs, paste0("jobGroup_", group_id))
    period_out_dir <- file.path(sir_method_dir, "Period_Outputs", paste0("jobGroup_", group_id))
    likelihood_dir <- file.path(sir_method_dir, config$output_dirs$likelihood_outputs, paste0("jobGroup_", group_id))
    run_status_dir <- file.path(sir_method_dir, config$output_dirs$run_status, paste0("jobGroup_", group_id))
    observation_dir <- if (file.path.is.absolute(config$paths$observation_dir)) {
      config$paths$observation_dir
    } else {
      file.path(lairice_root, config$paths$observation_dir)
    }
    
    # Create output directory if it doesn't exist
    if (!dir.exists(likelihood_dir)) {
      dir.create(likelihood_dir, recursive = TRUE)
    }
    
    # Required functions are now part of the bayesiancalibr package
    # No need to source external files
    
    # Load observation data using generic loader
    obs_data <- load_observation_data(config, observation_dir, verbose)

    # Calculate likelihood for each observation variable
    if (verbose) {
      cat("Processing", length(config$likelihood_calculation$variables), "observation variables...\n")
    }

    gofs_list <- list()

    for (i in seq_along(config$likelihood_calculation$variables)) {
      var_config <- config$likelihood_calculation$variables[[i]]
      var_name <- var_config$name

      if (verbose) {
        cat("  Processing variable:", var_name, "...")
      }

      # Smart data loading: choose between weighted mean and annual outputs
      # based on output specifications and data availability
      annual_output_file <- file.path(annual_out_dir, paste0("dc_annualRslt_", task_id, ".rds"))

      wide_dRslt_var <- tryCatch({
        load_model_data_for_likelihood(
          config = config,
          var_config = var_config,
          task_id = task_id,
          group_id = group_id,
          method_dir = sir_method_dir,
          annual_output_file = annual_output_file,
          obs_data = obs_data,
          verbose = verbose
        )
      }, error = function(e) {
        if (identical(run_type, "database")) {
          # In database mode, fallback to RDS loading will not exist.
          stop(e)
        }
        
        # Fallback to traditional loading if smart loader fails (file_system only)
        if (verbose) {
          cat("\n    Warning: Smart data loader failed, using fallback:", e$message, "\n")
        }

        daily_output_file <- file.path(daily_out_dir, paste0("dc_dRslt_", task_id, ".rds"))
        agg_output_file <- file.path(agg_out_dir, paste0("dc_aggRslt_", task_id, ".rds"))
        period_output_file <- file.path(period_out_dir, paste0("dc_periodRslt_", task_id, ".rds"))

        if (var_config$matching_type == "period_totals" && file.exists(period_output_file)) {
          readRDS(period_output_file)
        } else if (file.exists(annual_output_file)) {
          readRDS(annual_output_file)
        } else if (file.exists(agg_output_file)) {
          readRDS(agg_output_file)
        } else if (file.exists(daily_output_file)) {
          readRDS(daily_output_file)
        } else {
          stop("No model output files found for task ", task_id)
        }
      })

      # Calculate likelihood for this variable
      var_gofs <- tryCatch({
        calculate_variable_likelihood(
          var_config = var_config,
          model_data = wide_dRslt_var,
          obs_data = obs_data,
          task_id = task_id,
          verbose = verbose
        )
      }, error = function(e) {
        stop("Error calculating likelihood for variable ", var_name, ": ", e$message)
      })
      
      # Add variable metadata
      var_gofs_df <- data.frame(
        "SampleID" = task_id,
        "Variable" = var_name,
        var_gofs,
        stringsAsFactors = FALSE
      )
      
      gofs_list[[var_name]] <- var_gofs_df
      
      if (verbose) {
        cat(" [DONE]\n")
      }
    }
    
    # Combine all goodness-of-fit results
    gofs_df <- do.call(rbind, gofs_list)

    output_file <- NULL
    # Save likelihood results
    if (identical(run_type, "file_system")) {
      output_file <- file.path(likelihood_dir, paste0("dc_gofsl_", task_id, ".rds"))
      saveRDS(gofs_df, output_file)
    } else if (identical(run_type, "database")) {
      # Write likelihood results to database
      cred_file_cfg <- config$file_source$database$cred_file
      if (is.null(cred_file_cfg) || length(cred_file_cfg) == 0) {
        cred_file_cfg <- "~/.dblogin"
      }
      cred_file <- path.expand(cred_file_cfg)
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
      sir_results_likelihood <- config$file_source$database$database_result$tables$sir_results_likelihood
      
      # Add job_group/simulation_id columns expected by downstream SQL consumers
      gofs_df$job_group <- group_id
      gofs_df$simulation_id <- task_id

      con <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = host,
        dbname = db_calib,
        username = user,
        password = password
      )
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbWriteTable(con, sir_results_likelihood, gofs_df, append = TRUE, row.names = FALSE)
      DBI::dbDisconnect(con)
    } else {
      stop("Unknown database_result$run_type: ", run_type)
    }
    
    # Calculate runtime
    end_time <- Sys.time()
    runtime_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create runtime record
    runtime_info <- data.frame(
      "SampleID" = task_id,
      "node" = node,
      "Start" = start_time,
      "End" = end_time,
      "Time_sec" = runtime_seconds,
      "status" = 0,
      "message" = "Success",
      stringsAsFactors = FALSE
    )
    
    # Save runtime information - SIR uses different file naming
    runtime_file <- file.path(run_status_dir, paste0("RunTime_SIR_gofs_", task_id, ".csv"))
    if (!dir.exists(run_status_dir)) {
      dir.create(run_status_dir, recursive = TRUE)
    }
    write.csv(runtime_info, runtime_file, row.names = FALSE)
    
    if (verbose) {
      cat("\nLikelihood calculation completed successfully!\n")
      if (!is.null(output_file)) {
        cat("Results saved to:", output_file, "\n")
      } else {
        cat("Results saved to database.\n")
      }
      cat("Runtime:", round(runtime_seconds, 2), "seconds\n")
    }
    
    # Return results
    return(list(
      gofs_df = gofs_df,
      runtime_info = runtime_info,
      validation = TRUE,
      output_files = list(likelihood = output_file, runtime = runtime_file)
    ))
    
  }, error = function(e) {
    # Error handling
    error_msg <- paste("SIR Step 3 failed:", e$message)
    if (verbose) {
      cat("ERROR:", error_msg, "\n")
    }
    
    # Calculate runtime for error case
    end_time <- Sys.time()
    runtime_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create error runtime record
    runtime_info <- data.frame(
      "SampleID" = task_id,
      "node" = node,
      "Start" = start_time,
      "End" = end_time,
      "Time_sec" = runtime_seconds,
      "status" = 1,
      "message" = error_msg,
      stringsAsFactors = FALSE
    )
    
    # Try to save error runtime information
    tryCatch({
      runtime_file <- file.path(run_status_dir, paste0("RunTime_SIR_gofs_", task_id, ".csv"))
      if (!dir.exists(run_status_dir)) {
        dir.create(run_status_dir, recursive = TRUE)
      }
      write.csv(runtime_info, runtime_file, row.names = FALSE)
    }, error = function(e2) {
      # Silent failure for error logging
    })
    
    return(list(
      gofs_df = NULL,
      runtime_info = runtime_info,
      validation = FALSE,
      output_files = list(),
      error = error_msg
    ))
  })
}