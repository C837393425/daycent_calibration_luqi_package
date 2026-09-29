#' GSA Step 3: Likelihood Calculation
#'
#' Generic function for calculating likelihood values by comparing model outputs 
#' with observations across multiple variables and measurement types.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param gsa_method GSA method name (e.g., "soboljansen", "sobol", etc.)
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
gsa_step3_likelihood <- function(config, gsa_method, task_id, date_stamp = NULL, verbose = TRUE) {
  
  # Initialize timing and status
  start_time <- Sys.time()
  node <- Sys.info()[4]
  
  tryCatch({
    
    # Validate inputs
    if (missing(config)) stop("Configuration object is required")
    if (missing(gsa_method)) stop("gsa_method is required")
    if (missing(task_id)) stop("task_id is required")
    if (!gsa_method %in% config$gsa$gsa_methods) {
      stop("gsa_method must be one of: ", paste(config$gsa$gsa_methods, collapse = ", "))
    }
    
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
      cat("=== GSA Step 3: Likelihood Calculation ===\n")
      cat("Project:", config$project$name, "\n")
      cat("Date stamp:", date_stamp, "\n")
      cat("GSA method:", gsa_method, "\n")
      cat("Task ID:", task_id, "\n")
      cat("Node:", node, "\n")
      cat("Started at:", format(start_time), "\n\n")
    }
    
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
    job_parms <- all_jobs[all_jobs$SampleID == task_id, ]
    if (nrow(job_parms) == 0) {
      stop("Task ID ", task_id, " not found in parameter set")
    }
    group_id <- job_parms$JobGroup
    
    if(config$file_source$database$database_result$run_type == "file_system") {
      # Set up directory paths
      bmaf_srs_dir <- file.path(lairice_root, config$paths$bmaf_srs_dir)
      daily_out_dir <- file.path(gsa_method_dir, config$output_dirs$daily_outputs, paste0("jobGroup_", group_id))
      agg_out_dir <- file.path(gsa_method_dir, config$output_dirs$aggregated_outputs, paste0("jobGroup_", group_id))
      annual_out_dir <- file.path(gsa_method_dir, config$output_dirs$annual_outputs, paste0("jobGroup_", group_id))
      period_out_dir <- file.path(gsa_method_dir, "Period_Outputs", paste0("jobGroup_", group_id))
      likelihood_dir <- file.path(gsa_method_dir, config$output_dirs$likelihood_outputs, paste0("jobGroup_", group_id))
      run_status_dir <- file.path(gsa_method_dir, config$output_dirs$run_status, paste0("jobGroup_", group_id))
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
            method_dir = gsa_method_dir,
            annual_output_file = annual_output_file,
            obs_data = obs_data,
            verbose = verbose
          )
        }, error = function(e) {
          # Fallback to traditional loading if smart loader fails
          if (verbose) {
            cat("\n    Warning: Smart data loader failed, using fallback:", e$message, "\n")
          }

          # Traditional loading logic as fallback
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
      
      # Save likelihood results
      output_file <- file.path(likelihood_dir, paste0("dc_gofsl_", task_id, ".rds"))
      saveRDS(gofs_df, output_file)
      
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
      
      # Save runtime information
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_gofs_", task_id, ".csv"))
      if (!dir.exists(run_status_dir)) {
        dir.create(run_status_dir, recursive = TRUE)
      }
      write.csv(runtime_info, runtime_file, row.names = FALSE)

    } else if(config$file_source$database$database_result$run_type == "database") {
      observation_dir <- if (file.path.is.absolute(config$paths$observation_dir)) {
        config$paths$observation_dir
      } else {
        file.path(lairice_root, config$paths$observation_dir)
      }

      # Load observation data for likelihood matching in database mode too
      obs_data <- load_observation_data(config, observation_dir, verbose)

      # Save in database (optional, if database_result is configured)
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
      gsa_results_annual = config$file_source$database$database_result$tables$gsa_results_annual


      # Calculate likelihood for each observation variable
      gofs_list <- list()

      for (i in seq_along(config$likelihood_calculation$variables)) {
        var_config <- config$likelihood_calculation$variables[[i]]
        var_name <- var_config$name

        if (verbose) {
          cat("  Processing variable:", var_name, "...")
        }

        # Smart data loading: choose between weighted mean and annual outputs
        # based on output specifications and data availability
        wide_dRslt_var <- tryCatch({
          load_model_data_for_likelihood(
            config = config,
            var_config = var_config,
            task_id = task_id,
            group_id = group_id,
            method_dir = gsa_method_dir,
            annual_output_file = gsa_results_annual,
            obs_data = obs_data,
            verbose = verbose
          )
        }, error = function(e) {
          # Fallback to traditional loading if smart loader fails
          if (verbose) {
            cat("\n    Warning: Smart data loader failed, using fallback:", e$message, "\n")
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
      gofs_df$job_group <- group_id
      gofs_df$simulation_id <- task_id
      # Save in database
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
      gsa_results_likelihood = config$file_source$database$database_result$tables$gsa_results_likelihood

      dbConn_result <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = host, dbname = db_calib,
        username = user, password = password
      )
      on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE) 

      # Save likelihood results to database
      DBI::dbWriteTable(dbConn_result, gsa_results_likelihood, gofs_df, append = TRUE, row.names = FALSE)
      DBI::dbDisconnect(dbConn_result)

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
      
      run_status_dir <- file.path(gsa_method_dir, config$output_dirs$run_status, paste0("jobGroup_", group_id))
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_gofs_", task_id, ".csv"))
      if (!dir.exists(run_status_dir)) {
        dir.create(run_status_dir, recursive = TRUE)
      }

      likelihood_dir <- file.path(gsa_method_dir, config$output_dirs$likelihood_outputs, paste0("jobGroup_", group_id))
      output_file <- file.path(likelihood_dir, paste0("dc_gofsl_", task_id, ".rds"))
    }
    
    if (verbose) {
      cat("\nLikelihood calculation completed successfully!\n")
      cat("Results saved to:", output_file, "\n")
      cat("Runtime:", round(runtime_seconds, 2), "seconds\n")
    }
    
    # Return results
    return(list(
      gofs_df = gofs_df,
      runtime_info = runtime_info,
      validation = TRUE,
      output_files = list(
        likelihood = output_file,
        runtime = runtime_file
      )
    ))
    
  }, error = function(e) {
    # Error handling
    error_msg <- paste("GSA Step 3 failed:", e$message)
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
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_gofs_", task_id, ".csv"))
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

#' Load Observation Data
#'
#' Generic function to load observation data for different measurement types.
#'
#' @param config Configuration object
#' @param observation_dir Path to observation data directory
#' @param verbose Logical, whether to print progress messages
#'
#' @return List of observation datasets
#'
#' @export
load_observation_data <- function(config, observation_dir, verbose = TRUE) {
  
  obs_data <- list()
  
  for (var_config in config$likelihood_calculation$variables) {
    var_name <- var_config$name
    obs_file <- if (file.path.is.absolute(var_config$observation_file)) {
      var_config$observation_file
    } else {
      file.path(observation_dir, var_config$observation_file)
    }
    
    if (verbose) {
      cat("Loading observation data for", var_name, "...")
    }
    
    if (!file.exists(obs_file)) {
      stop("Observation file not found: ", obs_file)
    }
    
    # Load observation data
    obs_df <- read.csv(obs_file, stringsAsFactors = FALSE)
    
    # Apply filters if specified
    if (!is.null(var_config$filter_conditions)) {
      for (filter_cond in var_config$filter_conditions) {
        col_name <- filter_cond$column
        operator <- filter_cond$operator
        value <- filter_cond$value
        
        if (operator == "!=") {
          obs_df <- obs_df[obs_df[[col_name]] != value, ]
        } else if (operator == "==") {
          obs_df <- obs_df[obs_df[[col_name]] == value, ]
        } else if (operator == ">") {
          obs_df <- obs_df[obs_df[[col_name]] > value, ]
        } else if (operator == "<") {
          obs_df <- obs_df[obs_df[[col_name]] < value, ]
        }
      }
    }
    
    # Add date conversions for temporal matching
    if (var_config$matching_type %in% c("individual_windows", "cumulative_windows")) {
      # Convert measurement windows to dates
      start_year_col <- var_config$date_columns$start[1]
      start_doy_col <- var_config$date_columns$start[2]
      end_year_col <- var_config$date_columns$end[1]
      end_doy_col <- var_config$date_columns$end[2]
      
      obs_df$meas_start_date <- as.Date(obs_df[[start_doy_col]] - 1, 
                                        origin = paste0(obs_df[[start_year_col]], "-01-01"))
      obs_df$meas_end_date <- as.Date(obs_df[[end_doy_col]] - 1, 
                                      origin = paste0(obs_df[[end_year_col]], "-01-01"))
      
      # Add measurement ID for individual windows
      if (var_config$matching_type == "individual_windows") {
        obs_df$MeasurementID <- paste0("M", 1:nrow(obs_df))
      }
      
    } else if (var_config$matching_type == "point_measurements") {
      # Convert point measurements to dates
      year_col <- var_config$date_columns$measurement[1]
      doy_col <- var_config$date_columns$measurement[2]
      
      obs_df$mes_date <- as.Date(obs_df[[doy_col]] - 1, 
                                 origin = paste0(obs_df[[year_col]], "-01-01"))
    } else if (var_config$matching_type == "period_totals") {
      period_col <- var_config$date_columns$measurement[[1]]
      obs_df[[period_col]] <- as.Date(obs_df[[period_col]])
    }
    
    obs_data[[var_name]] <- obs_df
    
    if (verbose) {
      cat(" [", nrow(obs_df), "observations]\n")
    }
  }
  
  return(obs_data)
}

#' Calculate Variable Likelihood
#'
#' Generic function to calculate goodness-of-fit statistics for any observation variable
#' based on configuration-specified matching and processing rules.
#'
#' @param var_config Configuration for this variable
#' @param model_data Model output data
#' @param obs_data Observation data list
#' @param task_id Current task ID
#' @param verbose Logical, whether to print progress messages
#'
#' @return Goodness-of-fit statistics dataframe
#'
#' @export
calculate_variable_likelihood <- function(var_config, model_data, obs_data, task_id, verbose = TRUE) {
  
  var_name <- var_config$name
  obs_df <- obs_data[[var_name]]
  if(!var_name %in% names(obs_data)){
    obs_df <- obs_data
  }
  
  # Extract model data based on configured model output variable(s)
  if (is.character(var_config$model_output) && length(var_config$model_output) == 1) {
    # Single model output variable
    wide_dRslt_var <- model_data[model_data$variable == var_config$model_output, ]
  } else if (is.character(var_config$model_output) && length(var_config$model_output) > 1) {
    # Multiple model output variables (e.g., surface + subsurface)
    wide_dRslt_var <- model_data[model_data$variable %in% var_config$model_output, ]
  } else {
    stop("Invalid model_output configuration for variable: ", var_name)
  }
  
  if (nrow(wide_dRslt_var) == 0) {
    stop("No model data found for variable: ", var_name, " with output: ", paste(var_config$model_output, collapse = ", "))
  }
  
  # Process data based on measurement type
  if (var_config$matching_type %in% c("annual_measurements", "period_totals")) {
    combined_data <- match_model_observations(
      model_data = wide_dRslt_var,
      obs_data = obs_df,
      var_config = var_config,
      verbose = verbose
    )
  } else {
    # Melt to long format for daily processing (NH3, Urea, etc.)
    # Use specific column name based on variable type for compatibility
    if (var_config$matching_type %in% c("individual_windows", "cumulative_windows")) {

      if (var_config$model_output == "NH3.N") {
        value_name <- "mod_NH3"   # N2O output column
      } else if (var_config$model_output == "DayCent_N2O") {
        value_name <- "mod_N2O"   # default to NH3 output column
      } else {
        stop("Unknown output for cumulative windows")
      }
      
    } else if (var_config$matching_type == "point_measurements") {
      value_name <- "mod_Urea"  # Urea functions expect mod_Urea
    } else {
      value_name <- "mod_value"  # Generic fallback
    }
    
    dRslt_var <- melt(wide_dRslt_var, 
                      id.vars = c("SampleID", "SiteID", "TreatmentID", "year", "variable", "Model", "unit"),
                      variable.name = "day", value.name = value_name)
    
    # Convert day column to day of year
    dRslt_var$dayofyr <- as.numeric(substr(as.character(dRslt_var$day), 2, nchar(as.character(dRslt_var$day))))
    dRslt_var$mod_date <- as.Date(dRslt_var$dayofyr - 1, origin = paste0(dRslt_var$year, "-01-01"))
    
    # Perform temporal matching based on measurement type
    combined_data <- match_model_observations(
      model_data = dRslt_var,
      obs_data = obs_df,
      var_config = var_config,
      verbose = verbose
    )
  }
  
  # Calculate residuals and log-transformed values
  combined_data$resi <- combined_data$obs - combined_data$mod
  
  # Observed N2O can be negative (net uptake), so shift values above zero before taking the log.
  
  if (var_config$model_output == "DayCent_N2O") {
    
    n2o_shift = min(c(combined_data$obs, combined_data$mod), na.rm = TRUE)
    
    if (n2o_shift <= 0) {
      N2O_shift = abs(n2o_shift) + 1e-2
    } else {
      N2O_shift = 0
    }
    
    combined_data$ln_mod <- log(combined_data$mod + N2O_shift)
    combined_data$ln_obs <- log(combined_data$obs + N2O_shift)
    
  } else {
  
    combined_data$ln_mod <- log(combined_data$mod + 1)
    combined_data$ln_obs <- log(combined_data$obs + 1)
  }
  
  combined_data$ln_resi <- combined_data$ln_obs - combined_data$ln_mod
  
  # Calculate goodness-of-fit statistics using existing function
  gofs_result <- calculate_gofs(merged_data = combined_data)
  
  return(gofs_result)
}

#' Match Model Outputs with Observations
#'
#' Generic function to match model outputs with observations based on 
#' measurement type and temporal matching rules.
#'
#' @param model_data Model data in long format
#' @param obs_data Observation data
#' @param var_config Variable configuration
#' @param verbose Logical, whether to print progress messages
#'
#' @return Combined model-observation dataframe
#'
#' @export
match_model_observations <- function(model_data, obs_data, var_config, verbose = TRUE) {
  
  matching_type <- var_config$matching_type
  
  if (matching_type == "individual_windows") {
    # Individual measurement windows (e.g., individual NH3 measurements)
    combined_data <- match_individual_windows(model_data, obs_data, var_config)
    
  } else if (matching_type == "cumulative_windows") {
    # Cumulative measurement windows (e.g., cumulative NH3 measurements)
    combined_data <- match_cumulative_windows(model_data, obs_data, var_config)
    
  } else if (matching_type == "point_measurements") {
    # Point measurements (e.g., Urea concentrations on specific dates)
    combined_data <- match_point_measurements(model_data, obs_data, var_config)
    
  } else if (matching_type == "annual_measurements") {
    # Annual measurements (e.g., SOC measurements at specific years)
    combined_data <- match_annual_measurements(model_data, obs_data, var_config)

  } else if (matching_type == "period_totals") {
    combined_data <- match_period_totals(model_data, obs_data, var_config)
    
  } else {
    stop("Unknown matching_type: ", matching_type)
  }
  
  return(combined_data)
}

#' Match Individual Windows
#'
#' Match model outputs to individual measurement windows.
#'
#' @param model_data Model data in long format
#' @param obs_data Observation data with date windows
#' @param var_config Variable configuration
#'
#' @return Combined dataframe
#'
#' @export
match_individual_windows <- function(model_data, obs_data, var_config) {
  
  # Use generic combine_mod_mes function for all variable types
  tryCatch({
    combined_result <- combine_mod_mes(model_data = model_data, obs_data = obs_data, var_config = var_config)
    combined_data <- combined_result$data
  }, error = function(e) {
    stop("Error in combine_mod_mes: ", e$message)
  })
  
  # Standardize column names based on configuration
  # The generic function creates consistent column names based on variable type
  if (var_config$model_output == "NH3.N") {
    mod_col_found <- "mod_NH3vol_gN_ha_day"
  } else {
    mod_col_found <- paste0("mod_", gsub("\\.", "_", var_config$model_output), "_avg")
  }
  
  obs_col <- var_config$value_column
  
  if (!mod_col_found %in% names(combined_data)) {
    stop("Could not find model column '", mod_col_found, "' in combined data. Available columns: ", paste(names(combined_data), collapse = ", "))
  }
  
  # Standardize column names
  combined_data$mod <- combined_data[[mod_col_found]]
  combined_data$obs <- combined_data[[obs_col]]
  
  return(combined_data)
}

#' Match Cumulative Windows
#'
#' Match model outputs to cumulative measurement windows.
#'
#' @param model_data Model data in long format
#' @param obs_data Observation data with date windows
#' @param var_config Variable configuration
#'
#' @return Combined dataframe
#'
#' @export
match_cumulative_windows <- function(model_data, obs_data, var_config) {
  
  # Use generic combine_mod_mes function for all variable types
  tryCatch({
    combined_result <- combine_mod_mes(model_data = model_data, obs_data = obs_data, var_config = var_config)
    combined_data <- combined_result$data
  }, error = function(e) {
    stop("Error in combine_mod_mes: ", e$message)
  })
  
  # Standardize column names (same logic as individual windows)
  # The generic function creates consistent column names based on variable type
  if (var_config$model_output == "NH3.N") {
    mod_col_found <- "mod_NH3vol_gN_ha_day"
  } else if (var_config$model_output == "DayCent_N2O")  {
    mod_col_found <- "mod_N2O_gN_ha_day"
  } else {
    mod_col_found <- paste0("mod_", gsub("\\.", "_", var_config$model_output), "_avg")
  }
  
  obs_col <- var_config$value_column
  
  if (!mod_col_found %in% names(combined_data)) {
    stop("Could not find model column '", mod_col_found, "' in combined data. Available columns: ", paste(names(combined_data), collapse = ", "))
  }
  
  # Standardize column names
  combined_data$mod <- combined_data[[mod_col_found]]
  combined_data$obs <- combined_data[[obs_col]]
  
  return(combined_data)
}

#' Match Point Measurements
#'
#' Match model outputs to point measurements on specific dates.
#'
#' @param model_data Model data in long format
#' @param obs_data Observation data with measurement dates
#' @param var_config Variable configuration
#'
#' @return Combined dataframe
#'
#' @export
match_point_measurements <- function(model_data, obs_data, var_config) {
  
  # Use generic combine_mod_mes function for all variable types
  tryCatch({
    combined_result <- combine_mod_mes(model_data = model_data, obs_data = obs_data, var_config = var_config)
    combined_data <- combined_result$data
  }, error = function(e) {
    stop("Error in combine_mod_mes: ", e$message)
  })
  
  # Standardize column names
  # The generic function creates consistent column names based on variable type
  if (length(var_config$model_output) > 1) {
    mod_col_found <- "mod_urea_gN_m2"  # Multiple urea variables
  } else if (var_config$model_output == "urea_left") {
    mod_col_found <- "mod_urea_gN_m2"  # Single urea variable
  } else {
    mod_col_found <- paste0("mod_", gsub("\\.", "_", var_config$model_output))
  }
  
  obs_col <- var_config$value_column
  
  if (!mod_col_found %in% names(combined_data)) {
    stop("Could not find model column '", mod_col_found, "' in combined data. Available columns: ", paste(names(combined_data), collapse = ", "))
  }
  
  # Standardize column names
  combined_data$mod <- combined_data[[mod_col_found]]
  combined_data$obs <- combined_data[[obs_col]]
  
  return(combined_data)
}
#' Match Annual Measurements
#'
#' Match annual model outputs (from annual_outputs) to annual observation measurements.
#' Designed specifically for annual calibration targets like SOC.
#'
#' @param model_data Annual model output data (from annual_outputs directory)
#' @param obs_data Observation data with annual measurements
#' @param var_config Variable configuration from likelihood_calculation config
#'
#' @return Combined dataframe with matched model and observation data
#'
#' @export
match_annual_measurements <- function(model_data, obs_data, var_config) {
  
  if (is.null(model_data) || nrow(model_data) == 0) {
    stop("No annual model data available for matching")
  }
  
  if (is.null(obs_data)) {
    stop("No observation data available for matching")
  }
  # Ensure we have a data frame (caller may pass the full obs list by mistake)
  if (is.list(obs_data) && !is.data.frame(obs_data)) {
    if (length(obs_data) == 1L && is.data.frame(obs_data[[1L]])) {
      obs_data <- obs_data[[1L]]
    } else {
      stop("obs_data must be a data frame for annual measurements; got a list with ", length(obs_data), " elements. Use obs_data[[var_name]] in the caller.")
    }
  }
  if (!is.data.frame(obs_data) || nrow(obs_data) == 0) {
    stop("No observation data available for matching")
  }
  
  # Extract configuration with defaults
  site_col <- if (!is.null(var_config$site_column)) var_config$site_column else "siteID"
  treatment_col <- var_config$treatment_column  # May be NULL for county-level data
  year_col <- var_config$date_columns$measurement[1]
  obs_value_col <- var_config$value_column

  # Model data should already be in annual format with columns:
  # SampleID, SiteID, TreatmentID, year, variable, Model, unit, d1

  # Prepare observation data - handle missing treatment column
  if (!is.null(treatment_col) && treatment_col %in% names(obs_data)) {
    # Include treatment column if specified and available
    obs_subset <- obs_data[, c(site_col, treatment_col, year_col, obs_value_col)]
    names(obs_subset) <- c("SiteID", "TreatmentID", "year", "obs_value")
  } else {
    # No treatment column - create dummy treatment based on site
    obs_subset <- obs_data[, c(site_col, year_col, obs_value_col)]
    names(obs_subset) <- c("SiteID", "year", "obs_value")
    if(config$project$name == "western_sugar_soc"){
      obs_subset$TreatmentID <- paste0(sub("^CN", "Contract", obs_subset$SiteID), ".sch")
    } else {
      obs_subset$TreatmentID <- paste0(obs_subset$SiteID, ".sch")  # Create treatment ID from site
    }
  }
  
  # Prepare model data
  model_subset <- model_data[, c("SampleID", "SiteID", "TreatmentID", "year", "d1")]
  names(model_subset) <- c("SampleID", "SiteID", "TreatmentID", "year", "mod_value")
  
  # Match model and observations by site, treatment, and year
  combined_data <- merge(model_subset, obs_subset, 
                        by = c("SiteID", "TreatmentID", "year"), 
                        all = FALSE)
  
  if (nrow(combined_data) == 0) {
    stop("No matching data found between model outputs and observations")
  }
  
  # Standardize column names for compatibility with likelihood calculation
  combined_data$mod <- combined_data$mod_value
  combined_data$obs <- combined_data$obs_value
  
  # Add required columns for likelihood calculation
  combined_data$variable <- var_config$name
  combined_data$model_output <- var_config$model_output
  combined_data$siteID <- combined_data$SiteID  # Add siteID column expected by calculate_gofs
  combined_data$SeasonID <- combined_data$year  # Add SeasonID column for random effects models
  
  return(combined_data)
}
