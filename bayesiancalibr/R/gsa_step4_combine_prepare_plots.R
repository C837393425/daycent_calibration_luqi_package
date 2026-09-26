#' GSA Step 4A: Combine Likelihood and Aggregate Results  
#'
#' Combines individual job group likelihood and aggregate output files into 
#' consolidated data frames for sensitivity analysis. This is the first part
#' of the consolidated GSA Step 4 workflow.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param gsa_method GSA method name (e.g., "soboljansen", "sobol", etc.)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param output_base Optional output base directory override (uses config if NULL)
#' @param save_files Logical, whether to save combined files to disk (default TRUE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return List containing:
#'   \item{likelihood_combined}{Combined likelihood data frame}
#'   \item{aggregated_combined}{Combined aggregated results data frame}
#'   \item{job_groups}{Vector of job groups processed}
#'   \item{n_likelihood_files}{Number of likelihood files processed}
#'   \item{n_aggregate_files}{Number of aggregate files processed}
#'   \item{output_paths}{Paths where files were saved (if save_files = TRUE)}
#'
#' @importFrom utils read.csv
#' @export

#' Detect if aggregate outputs are enabled from variable specification
#'
#' Reads the Output_Variables_Specification.csv file to determine if any variables
#' have aggregate_output enabled. Includes multiple fallback layers for full
#' backward compatibility with existing workflows.
#'
#' @param config Configuration object containing paths
#' @return Logical - TRUE if any variable has aggregate_output enabled, with
#'   safety fallbacks that default to TRUE for backward compatibility
#' @export
detect_aggregate_outputs_enabled <- function(config) {
  # Safety fallback - if config is missing specification, assume enabled
  if (is.null(config$model_outputs) || is.null(config$model_outputs$output_variables_spec)) {
    return(TRUE)  # Backward compatibility - assume enabled
  }
  
  spec_file <- file.path(config$paths$lairice_root, config$model_outputs$output_variables_spec)
  if (!file.exists(spec_file)) {
    warning("Output variables specification file not found: ", spec_file)
    return(TRUE)  # Backward compatibility - assume enabled if missing
  }
  
  tryCatch({
    spec_data <- read.csv(spec_file, stringsAsFactors = FALSE)
    if (!"aggregate_output" %in% names(spec_data)) {
      warning("aggregate_output column not found in specification file")
      return(TRUE)  # Backward compatibility - assume enabled if column missing
    }
    
    # Return TRUE if ANY variable has aggregate_output = TRUE
    return(any(spec_data$aggregate_output == TRUE, na.rm = TRUE))
  }, error = function(e) {
    warning("Error reading specification file: ", e$message)
    return(TRUE)  # Backward compatibility - assume enabled on error
  })
}

#' Extract likelihood methods needed from final_variable_list configuration
#'
#' @param final_variable_list Character vector of variable names from config
#' @return Character vector of unique likelihood method column names to extract
extract_likelihood_methods_from_config <- function(final_variable_list) {
  if (is.null(final_variable_list) || length(final_variable_list) == 0) {
    # Default to all methods if no final_variable_list specified
    return(c("ln_independent_logLkhood", "ln_logLkhood_rS", "ln_logLkhood_rSY"))
  }
  
  # Extract likelihood method patterns from final_variable_list
  likelihood_methods <- c()
  
  for (var_name in final_variable_list) {
    # Pattern matching for different likelihood method suffixes
    if (grepl("ln_independent_logLkhood$", var_name)) {
      likelihood_methods <- c(likelihood_methods, "ln_independent_logLkhood")
    } else if (grepl("ln_logLkhood_rS$", var_name)) {
      likelihood_methods <- c(likelihood_methods, "ln_logLkhood_rS")  
    } else if (grepl("ln_logLkhood_rSY$", var_name)) {
      likelihood_methods <- c(likelihood_methods, "ln_logLkhood_rSY")
    } else if (grepl("independent_logLkhood$", var_name)) {
      likelihood_methods <- c(likelihood_methods, "independent_logLkhood")
    } else if (grepl("logLkhood_rS$", var_name)) {
      likelihood_methods <- c(likelihood_methods, "logLkhood_rS")
    } else if (grepl("logLkhood_rSY$", var_name)) {
      likelihood_methods <- c(likelihood_methods, "logLkhood_rSY")
    }
  }
  
  # Remove duplicates and return
  unique_methods <- unique(likelihood_methods)
  
  # If no methods found, fall back to natural log methods (most common)
  if (length(unique_methods) == 0) {
    unique_methods <- c("ln_independent_logLkhood", "ln_logLkhood_rS", "ln_logLkhood_rSY")
  }
  
  return(unique_methods)
}

gsa_step4a_combine_results <- function(config, 
                                      gsa_method = NULL,
                                      date_stamp = NULL, 
                                      output_base = NULL,
                                      save_files = TRUE,
                                      verbose = TRUE,
                                      force_aggregate_processing = NULL) {
  
  # Validate inputs
  if (missing(config)) stop("Configuration object is required")
  
  # Set defaults from config
  if (is.null(gsa_method)) {
    if (is.null(config$gsa$gsa_methods) || length(config$gsa$gsa_methods) == 0) {
      stop("gsa_method must be specified or config$gsa$gsa_methods must be defined")
    }
    gsa_method <- config$gsa$gsa_methods[6]
    if (verbose) cat("Using default GSA method:", gsa_method, "\n")
  }
  
  if (is.null(date_stamp)) {
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
  }
  
  if (is.null(output_base)) {
    output_base <- config$paths$output_base
  }
  
  # Setup paths
  base_path <- file.path(config$paths$lairice_root, output_base, date_stamp, "GSA", gsa_method)
  
  # Determine aggregate processing mode with backward compatibility
  if (!is.null(force_aggregate_processing)) {
    aggregate_enabled <- force_aggregate_processing
    aggregate_mode <- ifelse(aggregate_enabled, "FORCED ON", "FORCED OFF")
  } else {
    # Try automatic detection
    aggregate_enabled <- detect_aggregate_outputs_enabled(config)
    aggregate_mode <- ifelse(aggregate_enabled, "AUTO-ENABLED", "AUTO-DISABLED")
  }
  
  # Additional safety check - if folder_of_interest has aggregate folder specified, respect it for backward compatibility
  # Handle both old format (list with [1],[2]) and new format (single string)
  has_aggregate_folder <- FALSE
  if (!is.null(config$folder_of_interest) && !is.null(config$folder_of_interest$folder_name)) {
    if (is.list(config$folder_of_interest$folder_name)) {
      # Old format: check if [1] is not null and not "null"
      if (length(config$folder_of_interest$folder_name) >= 1 &&
          !is.null(config$folder_of_interest$folder_name[[1]]) &&
          config$folder_of_interest$folder_name[[1]] != "null") {
        has_aggregate_folder <- TRUE
      }
    }
    # New format is single string - no aggregate folder specified
  }
  
  if (has_aggregate_folder) {
    # Existing config has explicit aggregate folder - respect it for backward compatibility
    aggregate_enabled <- TRUE
    aggregate_mode <- "BACKWARD-COMPATIBLE (aggregate folder specified)"
  }
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 4A: Combining Results\n")
    cat("GSA Method:", gsa_method, "\n")
    cat("Date Stamp:", date_stamp, "\n")
    cat("Base Path:", base_path, "\n")
    cat("Aggregate Processing:", aggregate_mode, "\n")
    cat("====================================================================\n")
  }
  
  # Read MC draw data frame to get job groups
  mc_draw_file <- file.path(base_path, paste0("mc_GSA_draw_", gsa_method, ".rds"))
  if (!file.exists(mc_draw_file)) {
    stop("MC draw file not found: ", mc_draw_file)
  }
  
  X <- readRDS(mc_draw_file)
  job_groups <- sort(unique(X$JobGroup))
  
  if (verbose) cat("Processing", length(job_groups), "job groups...\n")
  
  # Initialize result containers
  lkhd_df <- NULL
  aggr_df <- NULL
  weighted_df <- NULL
  n_likelihood_files <- 0
  n_aggregate_files <- 0
  n_weighted_files <- 0
  
  # Process each job group
  if(config$file_source$database$database_result$run_type == "file_system") {
    for (grp in job_groups) {
      if (verbose) cat("Processing job group", grp, "...\n")
      
      # Process likelihood files
      # Handle both old format (list with [1],[2]) and new format (single string)
      if (is.list(config$folder_of_interest$folder_name)) {
        # Old format: use [2] for likelihood
        lh_folder <- config$folder_of_interest$folder_name[[2]]
        lh_pattern <- config$folder_of_interest$file_name[[2]]
      } else {
        # New format: single string for likelihood
        lh_folder <- config$folder_of_interest$folder_name
        lh_pattern <- config$folder_of_interest$file_name
      }
      
      lh_path <- file.path(base_path, lh_folder, paste0("jobGroup_", grp))
      if (dir.exists(lh_path)) {
        lh_file_lists <- list.files(path = lh_path, pattern = lh_pattern, full.names = TRUE)
        if (length(lh_file_lists) > 0) {
          temp_lh <- do.call(rbind, lapply(lh_file_lists, readRDS))
          lkhd_df <- rbind(lkhd_df, temp_lh)
          n_likelihood_files <- n_likelihood_files + length(lh_file_lists)
        } else {
          if (verbose) cat("  Warning: No likelihood files found in", lh_path, "\n")
        }
      } else {
        if (verbose) cat("  Warning: Likelihood directory not found:", lh_path, "\n")
      }
      
      # Process aggregate files (conditional based on detection)
      if (aggregate_enabled) {
        # Determine aggregate folder path and pattern
        if (!is.null(config$folder_of_interest) && 
            !is.null(config$folder_of_interest$folder_name) &&
            length(config$folder_of_interest$folder_name) >= 1 &&
            !is.null(config$folder_of_interest$folder_name[1])) {
          # Use existing folder_of_interest config (backward compatibility)
          aggregate_folder <- config$folder_of_interest$folder_name[1]
          aggregate_pattern <- config$folder_of_interest$file_name[1]
        } else {
          # Use standard aggregate folder names
          aggregate_folder <- "Aggregated_Outputs"
          aggregate_pattern <- "dc_aggRslt"
        }
        
        aggregated_path <- file.path(base_path, aggregate_folder, paste0("jobGroup_", grp))
        if (dir.exists(aggregated_path)) {
          ag_file_lists <- list.files(path = aggregated_path, pattern = aggregate_pattern, full.names = TRUE)
          if (length(ag_file_lists) > 0) {
            temp_ag <- do.call(rbind, lapply(ag_file_lists, readRDS))
            aggr_df <- rbind(aggr_df, temp_ag)
            n_aggregate_files <- n_aggregate_files + length(ag_file_lists)
          } else {
            if (verbose) cat("  Warning: No aggregate files found in", aggregated_path, "\n")
          }
        } else {
          if (verbose) cat("  Warning: Aggregate directory not found:", aggregated_path, "\n")
        }
      } else {
        if (verbose && grp == job_groups[1]) {  # Only show message once
          cat("  Skipping aggregate file processing (disabled in specification)\n")
        }
      }

      if(config$emmission_variable == "crop") {
        # Process weighted mean files (always check for existence)
        weighted_mean_path <- file.path(base_path, "Weighted_Mean_Outputs", paste0("jobGroup_", grp))
        if (dir.exists(weighted_mean_path)) {
          wm_file_lists <- list.files(path = weighted_mean_path, pattern = "weighted.*\\.rds$", full.names = TRUE)
          if (length(wm_file_lists) > 0) {
            temp_wm <- do.call(rbind, lapply(wm_file_lists, readRDS))
            weighted_df <- rbind(weighted_df, temp_wm)
            n_weighted_files <- n_weighted_files + length(wm_file_lists)
            if (verbose && grp == job_groups[1]) {  # Only show message once
              cat("  Processing weighted mean files\n")
            }
          } else {
            if (verbose && grp == job_groups[1]) {  # Only show message once
              cat("  Warning: No weighted mean files found in", weighted_mean_path, "\n")
            }
          }
        } else {
          if (verbose && grp == job_groups[1]) {  # Only show message once
            cat("  No Weighted_Mean_Outputs directory found\n")
          }
        }
      }
    }
  } else if (config$file_source$database$database_result$run_type == "database") {
    if (verbose) {
      cat("Loading combined results from database_result tables (run_type = database)...\n")
    }
    db_cfg <- config$file_source$database$database_result
    cred_file <- path.expand(if (!is.null(db_cfg$cred_file)) db_cfg$cred_file else "~/.dblogin")
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
    host <- db_cfg$host
    db_calib <- db_cfg$database
    if (is.null(host) || length(host) == 0) {
      host <- config$file_source$database$host
    }
    if (is.null(db_calib) || length(db_calib) == 0) {
      db_calib <- config$file_source$database$database
    }
    likelihood_table <- db_cfg$tables$gsa_results_likelihood
    if (is.null(likelihood_table) || !nzchar(likelihood_table)) {
      stop("database_result.tables$gsa_results_likelihood must be set in config")
    }
    dbConn_result <- DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = host, dbname = db_calib,
      username = user, password = password
    )
    on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE)
    jg_sql <- paste(as.integer(job_groups), collapse = ",")
    qh <- paste0(
      "SELECT * FROM ", likelihood_table, ";"
    )
    lkhd_df <- DBI::dbGetQuery(dbConn_result, qh)
    lkhd_df <- unique(lkhd_df)
    n_likelihood_files <- nrow(lkhd_df)
    if (verbose) {
      cat("  Loaded", n_likelihood_files, "likelihood rows from", likelihood_table, "\n")
    }
    if (nrow(lkhd_df) == 0 && verbose) {
      cat("  Warning: No likelihood rows returned for job groups in", likelihood_table, "\n")
    }
    if (aggregate_enabled) {
      annual_base <- db_cfg$tables$gsa_results_annual
      pa_path <- file.path(base_path, "point_assignments.rds")
      if (!is.null(annual_base) && nzchar(annual_base) && file.exists(pa_path)) {
        pa <- readRDS(pa_path)
        if (!"total_jobs" %in% names(pa)) {
          if (verbose) {
            cat("  Warning: point_assignments has no total_jobs; skipping annual aggregate SQL load\n")
          }
        } else {
        n_part <- unique(pa$total_jobs)
        if (length(n_part) >= 1) {
          n_part <- as.integer(n_part[[1]])
          if (!is.na(n_part) && n_part >= 1) {
            ag_list <- vector("list", n_part)
            for (part_id in seq_len(n_part)) {
              atbl <- paste0("part", part_id, "_", annual_base)
              qa <- paste0(
                "SELECT * FROM `", atbl, "` WHERE job_group IN (", jg_sql, ")"
              )
              ag_list[[part_id]] <- DBI::dbGetQuery(dbConn_result, qa)
            }
            ag_list <- ag_list[vapply(ag_list, nrow, integer(1)) > 0]
            if (length(ag_list) > 0) {
              aggr_df <- do.call(rbind, ag_list)
              n_aggregate_files <- nrow(aggr_df)
              if (verbose) {
                cat("  Loaded", n_aggregate_files, "aggregate rows from partitioned annual tables\n")
              }
            }
          }
        }
        }
      } else if (verbose) {
        cat("  Warning: Skipping annual aggregate SQL load (missing gsa_results_annual or point_assignments.rds)\n")
      }
    } else if (verbose) {
      cat("  Skipping aggregate SQL load (aggregate processing disabled)\n")
    } 

    # if(config$emmission_variable == "crop") {
    #   # Process weighted mean files (always check for existence)
    #   weighted_table <- db_cfg$tables$gsa_results_weighted
    #   
    #   dbConn_result <- DBI::dbConnect(
    #     RMariaDB::MariaDB(),
    #     host = host, dbname = db_calib,
    #     username = user, password = password
    #   )
    #   
    #   on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE)
    #   jg_sql <- paste(as.integer(job_groups), collapse = ",")
    #   qh <- paste0(
    #     "SELECT * FROM ", weighted_table, ";"
    #   )
    #   weighted_df <- DBI::dbGetQuery(dbConn_result, qh)
    #   weighted_df <- unique(weighted_df)
    #   
    #   n_weighted_files <- nrow(weighted_df)
    #   if (verbose) {
    #     cat("  Loaded", n_weighted_files, "weighted mean rows from", weighted_table, "\n")
    #   }
    #   if (nrow(weighted_df) == 0 && verbose) {
    #     cat("  Warning: No weighted mean rows returned for job groups in", weighted_table, "\n")
    #   }
    # }

  } else {
    stop(
      "Invalid database_result.run_type: ",
      encodeString(as.character(config$file_source$database$database_result$run_type), quote = "\""),
      ". Expected 'file_system' or 'database'."
    )
  }

  # Clean row names
  if (!is.null(lkhd_df)) rownames(lkhd_df) <- NULL
  if (!is.null(aggr_df)) rownames(aggr_df) <- NULL
  if (!is.null(weighted_df)) rownames(weighted_df) <- NULL
  
  # Prepare output paths
  output_paths <- list()
  
  # Save files if requested
  if (save_files) {
    # Create Results directory if it doesn't exist
    results_dir <- file.path(base_path, "Results")
    if (!dir.exists(results_dir)) {
      dir.create(results_dir, recursive = TRUE)
    }
    
    # Save likelihood file
    if (!is.null(lkhd_df)) {
      # Determine likelihood filename - handle both old and new config formats
      if (!is.null(config$folder_of_interest) && !is.null(config$folder_of_interest$folder_name)) {
        if (is.list(config$folder_of_interest$folder_name)) {
          # Old format: use [2] for likelihood
          if (length(config$folder_of_interest$folder_name) >= 2 &&
              !is.null(config$folder_of_interest$folder_name[[2]])) {
            likelihood_filename <- paste0(config$folder_of_interest$folder_name[[2]], "_", gsa_method, ".rds")
          } else {
            likelihood_filename <- paste0("Likelihood_Outputs_", gsa_method, ".rds")
          }
        } else {
          # New format: single string
          likelihood_filename <- paste0(config$folder_of_interest$folder_name, "_", gsa_method, ".rds")
        }
      } else {
        likelihood_filename <- paste0("Likelihood_Outputs_", gsa_method, ".rds")
      }
      
      lkhd_file <- file.path(base_path, "Results", likelihood_filename)
      lkhd_df <- unique(lkhd_df)
      saveRDS(lkhd_df, lkhd_file)
      output_paths$likelihood_combined <- lkhd_file
      if (verbose) cat("Saved likelihood combined file:", lkhd_file, "\n")
    }
    
    # Save aggregate file (only if enabled and data exists)
    if (aggregate_enabled && !is.null(aggr_df)) {
      # Determine aggregate filename
      if (!is.null(config$folder_of_interest) && 
          !is.null(config$folder_of_interest$folder_name) &&
          length(config$folder_of_interest$folder_name) >= 1 &&
          !is.null(config$folder_of_interest$folder_name[1])) {
        aggregate_filename <- paste0(config$folder_of_interest$folder_name[1], "_", gsa_method, ".rds")
      } else {
        aggregate_filename <- paste0("Aggregated_Combined_", gsa_method, ".rds")
      }
      
      aggr_file <- file.path(base_path, "Results", aggregate_filename)
      aggr_df <- unique(aggr_df)
      saveRDS(aggr_df, aggr_file)
      output_paths$aggregated_combined <- aggr_file
      if (verbose) cat("Saved aggregated combined file:", aggr_file, "\n")
    } else if (verbose && !aggregate_enabled) {
      cat("Skipped saving aggregated file (aggregate processing disabled)\n")
    }

    # Save weighted mean file (only if data exists)
    if (!is.null(weighted_df)) {
      weighted_filename <- paste0("Weighted_Mean_Combined_", gsa_method, ".rds")
      weighted_file <- file.path(base_path, "Results", weighted_filename)
      weighted_df <- unique(weighted_df)
      saveRDS(weighted_df, weighted_file)
      output_paths$weighted_combined <- weighted_file
      if (verbose) cat("Saved weighted mean combined file:", weighted_file, "\n")
    } else if (verbose) {
      cat("No weighted mean files found to combine\n")
    }
  }
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 4A Complete\n")
    cat("Likelihood files processed:", n_likelihood_files, "\n")
    cat("Aggregate files processed:", n_aggregate_files, "\n")
    cat("Likelihood records:", ifelse(is.null(lkhd_df), 0, nrow(lkhd_df)), "\n")
    cat("Aggregate records:", ifelse(is.null(aggr_df), 0, nrow(aggr_df)), "\n")
    cat("Weighted mean records:", ifelse(is.null(weighted_df), 0, nrow(weighted_df)), "\n")
    cat("====================================================================\n")
  }
  
  return(list(
    likelihood_combined = lkhd_df,
    aggregated_combined = aggr_df,
    weighted_combined = weighted_df,
    job_groups = job_groups,
    n_likelihood_files = n_likelihood_files,
    n_aggregate_files = n_aggregate_files,
    n_weighted_files = n_weighted_files,
    output_paths = output_paths
  ))
}

#' GSA Step 4B: Calculate Total and First-Order Sensitivity Indices
#'
#' Calculates Sobol sensitivity indices using combined likelihood and aggregate
#' results from GSA Step 4A. This is the second part of the consolidated GSA Step 4
#' workflow. Processes multiple output variables and calculates both first-order 
#' and total-order sensitivity indices.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param gsa_methods Vector of GSA method names to process (default from config)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param output_base Optional output base directory override (uses config if NULL)
#' @param likelihood_vars List of likelihood variable configurations (default from config)
#' @param save_files Logical, whether to save sensitivity index files to disk (default TRUE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return List containing:
#'   \item{first_order_indices}{Data frame with first-order sensitivity indices}
#'   \item{total_order_indices}{Data frame with total-order sensitivity indices}
#'   \item{variables_processed}{Vector of variables analyzed}
#'   \item{parameters}{Vector of parameters analyzed}
#'   \item{gsa_methods}{Vector of GSA methods processed}
#'   \item{output_paths}{Paths where files were saved (if save_files = TRUE)}
#'
#' @importFrom sensitivity tell
#' @export
gsa_step4b_calculate_sensitivity <- function(config,
                                            gsa_methods = NULL,
                                            date_stamp = NULL,
                                            output_base = NULL,
                                            likelihood_vars = NULL,
                                            save_files = TRUE,
                                            verbose = TRUE,
                                            force_aggregate_processing = NULL) {
  
  # Validate inputs
  if (missing(config)) stop("Configuration object is required")
  
  # Set defaults from config
  if (is.null(gsa_methods)) {
    if (is.null(config$gsa$gsa_methods)) {
      stop("gsa_methods must be specified or config$gsa$gsa_methods must be defined")
    }
    gsa_methods <- config$gsa$gsa_methods
  }
  
  if (is.null(date_stamp)) {
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
  }
  
  if (is.null(output_base)) {
    output_base <- config$paths$output_base
  }
  
  # Default likelihood variable configuration
  if (is.null(likelihood_vars)) {
    likelihood_vars <- config$combine_tables_prepare_plots$likelihood_vars
  }
  
  # Determine aggregate processing mode with backward compatibility
  if (!is.null(force_aggregate_processing)) {
    aggregate_enabled <- force_aggregate_processing
    aggregate_mode <- ifelse(aggregate_enabled, "FORCED ON", "FORCED OFF")
  } else {
    # Try automatic detection
    aggregate_enabled <- detect_aggregate_outputs_enabled(config)
    aggregate_mode <- ifelse(aggregate_enabled, "AUTO-ENABLED", "AUTO-DISABLED")
  }
  
  # Additional safety check - if folder_of_interest has aggregate folder specified, respect it for backward compatibility
  # Handle both old format (list with [1],[2]) and new format (single string)
  has_aggregate_folder <- FALSE
  if (!is.null(config$folder_of_interest) && !is.null(config$folder_of_interest$folder_name)) {
    if (is.list(config$folder_of_interest$folder_name)) {
      # Old format: check if [1] is not null and not "null"
      if (length(config$folder_of_interest$folder_name) >= 1 &&
          !is.null(config$folder_of_interest$folder_name[[1]]) &&
          config$folder_of_interest$folder_name[[1]] != "null") {
        has_aggregate_folder <- TRUE
      }
    }
    # New format is single string - no aggregate folder specified
  }
  
  if (has_aggregate_folder) {
    # Existing config has explicit aggregate folder - respect it for backward compatibility
    aggregate_enabled <- TRUE
    aggregate_mode <- "BACKWARD-COMPATIBLE (aggregate folder specified)"
  }
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 4B: Sensitivity Index Calculation\n")
    cat("GSA Methods:", paste(gsa_methods, collapse = ", "), "\n")
    cat("Date Stamp:", date_stamp, "\n")
    cat("Aggregate Processing:", aggregate_mode, "\n")
    cat("====================================================================\n")
  }
  
  # Initialize result containers
  FSI <- NULL  # First-order indices
  TSI <- NULL  # Total-order indices
  output_paths <- list()
  
  # Process each GSA method
  for (i in 1:length(gsa_methods)) {
    smethod <- gsa_methods[i]
    
    if (verbose) cat("Processing GSA method:", smethod, "\n")
    
    # Setup paths
    lairice_root <- config$paths$lairice_root
    gsa_obj_path <- file.path(lairice_root, output_base, date_stamp, "GSA", smethod)
    
    # Load GSA object and MC draws
    gsa_obj_file <- file.path(gsa_obj_path, paste0("GSA_obj_", smethod, ".rds"))
    mc_draw_file <- file.path(gsa_obj_path, paste0("mc_GSA_draw_", smethod, ".rds"))
    
    if (!file.exists(gsa_obj_file)) {
      stop("GSA object file not found: ", gsa_obj_file)
    }
    if (!file.exists(mc_draw_file)) {
      stop("MC draw file not found: ", mc_draw_file)
    }
    
    si_obj1 <- readRDS(gsa_obj_file)
    X <- readRDS(mc_draw_file)
    
    # Determine which method's results to use for data
    if (smethod %in% c("sobol", "sobolEff")) {
      smethod2 <- "sobol"
    } else {
      smethod2 <- "soboljansen"
    }
    
    gsa_obj_path2 <- file.path(lairice_root, output_base, date_stamp, "GSA", smethod2)
    
    # Load combined likelihood and aggregate data
    # Determine likelihood filename - handle both old and new config formats
    if (!is.null(config$folder_of_interest) && !is.null(config$folder_of_interest$folder_name)) {
      if (is.list(config$folder_of_interest$folder_name)) {
        # Old format: use [2] for likelihood
        if (length(config$folder_of_interest$folder_name) >= 2 &&
            !is.null(config$folder_of_interest$folder_name[[2]])) {
          likelihood_filename <- paste0(config$folder_of_interest$folder_name[[2]], "_", smethod2, ".rds")
        } else {
          likelihood_filename <- paste0("Likelihood_Outputs_", smethod2, ".rds")
        }
      } else {
        # New format: single string
        likelihood_filename <- paste0(config$folder_of_interest$folder_name, "_", smethod2, ".rds")
      }
    } else {
      likelihood_filename <- paste0("Likelihood_Outputs_", smethod2, ".rds")
    }
    
    # Determine aggregate filename with backward compatibility (only if aggregate enabled)
    if (aggregate_enabled) {
      if (!is.null(config$folder_of_interest) && 
          !is.null(config$folder_of_interest$folder_name) &&
          length(config$folder_of_interest$folder_name) >= 1 &&
          !is.null(config$folder_of_interest$folder_name[1])) {
        aggregate_filename <- paste0(config$folder_of_interest$folder_name[1], "_", smethod2, ".rds")
      } else {
        aggregate_filename <- paste0("Aggregated_Combined_", smethod2, ".rds")
      }

      if(config$emmission_variable == "crop") {
        weighted_filename <- paste0("Weighted_Mean_Combined_", smethod2, ".rds")
      }
    }
    
    lkhd_file <- file.path(gsa_obj_path2, "Results", likelihood_filename)
    if (aggregate_enabled) {
      aggr_file <- file.path(gsa_obj_path2, "Results", aggregate_filename)
      if(config$emmission_variable == "crop") {
        weighted_file <- file.path(gsa_obj_path2, "Results", weighted_filename)
      }
    } else {
      aggr_file <- NULL  # Don't look for aggregate file when disabled
      if(config$emmission_variable == "crop") {
        weighted_file <- NULL
      }
    }
    
    if (!file.exists(lkhd_file)) {
      stop("Combined likelihood file not found: ", lkhd_file, 
           "\nPlease run gsa_step4a_combine_results() first.")
    }
    lkhd_df <- readRDS(lkhd_file)
    
    # Handle aggregate file conditionally based on detection
    if (aggregate_enabled && !is.null(aggr_file) && file.exists(aggr_file)) {
      aggr_df <- readRDS(aggr_file)
      if (verbose) {
        cat("  GSA Method:", smethod, "Simulation Used:", smethod2, "\n")
        cat("  Likelihood records:", nrow(lkhd_df), "\n")
        cat("  Aggregate records:", nrow(aggr_df), "\n")
      }
    } else {
      aggr_df <- NULL
      if (verbose) {
        cat("  GSA Method:", smethod, "Simulation Used:", smethod2, "\n")
        cat("  Likelihood records:", nrow(lkhd_df), "\n")
        if (!aggregate_enabled) {
          cat("  Aggregate processing disabled - using likelihood-only processing\n")
        } else {
          cat("  No aggregate file found - using likelihood-only processing\n")
        }
      }
    }
    
    # Process likelihood variables with dynamic method extraction
    likelihood_components <- list()
    
    # Extract likelihood methods needed from final_variable_list
    needed_methods <- extract_likelihood_methods_from_config(config$final_variable_list)
    columns_to_extract <- c("SampleID", needed_methods)
    
    if (verbose) {
      cat("  Likelihood methods to extract:", paste(needed_methods, collapse = ", "), "\n")
    }
    
    for (i in seq_along(likelihood_vars)) {
      var_config <- likelihood_vars[[i]]
      var_name <- var_config$name
      var_prefix <- var_config$prefix
      
      # Dynamic column extraction based on needed methods
      available_columns <- intersect(columns_to_extract, names(lkhd_df))
      
      if (length(available_columns) < 2) {  # At least SampleID + 1 method
        if (verbose) cat("  Warning: No required likelihood methods found in data for", var_name, "\n")
        next
      }
      
      var_subset <- lkhd_df[lkhd_df$Variable == var_name, available_columns]
      
      if (nrow(var_subset) > 0) {
        # Rename likelihood columns with prefix (skip SampleID)
        likelihood_cols <- available_columns[-1]  # Remove SampleID
        if (length(likelihood_cols) > 0) {
          names(var_subset)[match(likelihood_cols, names(var_subset))] <- 
            paste0(var_prefix, "_", likelihood_cols)
        }
        
        likelihood_components[[var_prefix]] <- var_subset
        if (verbose) {
          cat("  Found", nrow(var_subset), "records for", var_name, 
              "with methods:", paste(likelihood_cols, collapse = ", "), "\n")
        }
      } else {
        if (verbose) cat("  Warning: No records found for", var_name, "\n")
      }
    }
    
    # Merge likelihood components
    if (length(likelihood_components) > 0) {
      lkhd_merged <- likelihood_components[[1]]
      
      if (length(likelihood_components) > 1) {
        for (j in 2:length(likelihood_components)) {
          lkhd_merged <- merge(lkhd_merged, likelihood_components[[j]], 
                              by = "SampleID", all = TRUE)
        }
      }
    } else {
      stop("No likelihood variables found. Check variable names in data.")
    }
    
    # Conditionally merge with aggregate data (if available)
    if (!is.null(aggr_df)) {
      # Traditional merge with aggregate data
      all_var <- merge(x = lkhd_merged, y = aggr_df, by = "SampleID", all = TRUE)
      if (verbose) cat("  Combined data dimensions:", nrow(all_var), "x", ncol(all_var), "\n")
    } else {
      # Likelihood-only processing (efficient)
      all_var <- lkhd_merged
      if (verbose) cat("  Likelihood-only dimensions:", nrow(all_var), "x", ncol(all_var), "\n")
    }
    
    # Get variable list (exclude SampleID)
    all_vars <- names(all_var)[-1]
    
    # Filter to final_variable_list if specified in config (supports one or more methods)
    if (!is.null(config$final_variable_list) && length(config$final_variable_list) > 0) {
      # Support both exact matches and pattern matching for generic likelihood methods
      available_vars <- c()
      
      for (pattern in config$final_variable_list) {
        # First try exact match
        exact_matches <- intersect(pattern, all_vars)
        
        # Then try pattern matching (e.g., "ln_logLkhood_rSY" matches "soc_ln_logLkhood_rSY")
        pattern_matches <- all_vars[grepl(paste0(".*", pattern, "$"), all_vars)]
        
        # Combine matches
        matches <- unique(c(exact_matches, pattern_matches))
        available_vars <- unique(c(available_vars, matches))
      }
      
      if (length(available_vars) > 0) {
        var_list <- available_vars
        if (verbose) {
          cat("  Using final_variable_list from config (", length(config$final_variable_list), " method(s) requested)\n")
          cat("  Requested patterns:", paste(config$final_variable_list, collapse = ", "), "\n")
          cat("  Matched variables:", paste(available_vars, collapse = ", "), "\n")
        }
      } else {
        if (verbose) cat("  Warning: None of final_variable_list variables found in data. Using all variables.\n")
        var_list <- all_vars
      }
    } else {
      # Use all variables if final_variable_list not specified
      var_list <- all_vars
      if (verbose) cat("  No final_variable_list specified, processing all available variables\n")
    }
    
    if (verbose) cat("  Processing", length(var_list), "variables...\n")
    
    # Process each variable
    for (j in 1:length(var_list)) {
      var_using <- var_list[j]
      var_df <- all_var[, c("SampleID", var_using)]
      
      if (verbose) cat("    Processing:", var_using, "\n")
      
      # Remove missing values and order by SampleID
      var_df <- var_df[!is.na(var_df[, var_using]), ]
      temp_df <- var_df[order(var_df$SampleID), ]
      
      # Check if we have the expected number of samples
      if (nrow(temp_df) != nrow(si_obj1$X)) {
        if (verbose) {
          cat("      Warning: Expected", nrow(si_obj1$X), "samples, got", nrow(temp_df), "\n")
          cat("      Attempting to align with GSA design...\n")
        }
        
        # Try to align with GSA design
        expected_ids <- 1:nrow(si_obj1$X)
        temp_df_aligned <- data.frame(SampleID = expected_ids)
        temp_df_aligned <- merge(temp_df_aligned, temp_df, by = "SampleID", all.x = TRUE)
        
        if (sum(is.na(temp_df_aligned[, var_using])) > 0) {
          if (verbose) cat("      Error: Missing values after alignment. Skipping variable.\n")
          next
        }
        
        temp_df <- temp_df_aligned[order(temp_df_aligned$SampleID), ]
      }
      
      # Calculate sensitivity indices using tell()
      tryCatch({
        # Create a copy of the GSA object to avoid modifying the original
        si_obj_copy <- si_obj1
        GSAobj_i <- sensitivity::tell(x = si_obj_copy, y = temp_df[, var_using])
        
        # Extract first-order sensitivity indices
        if (!is.null(GSAobj_i$S) && nrow(GSAobj_i$S) > 0) {
          firstSI <- GSAobj_i$S
          firstSI$params <- row.names(firstSI)
          names(firstSI) <- c("frstsi", "frstsi.bias", "frstsi.std.error", 
                             "frstsi.lci", "frstsi.uci", "params")
          firstSI$GSA_Method <- smethod
          firstSI$Variable <- var_using
          FSI <- rbind(FSI, firstSI)
        }
        
        # Extract total-order sensitivity indices
        if (!is.null(GSAobj_i$T) && nrow(GSAobj_i$T) > 0) {
          totalSI <- GSAobj_i$T
          totalSI$params <- row.names(totalSI)
          names(totalSI) <- c("totsi", "totsi.bias", "totsi.std.error", 
                             "totsi.lci", "totsi.uci", "params")
          totalSI$GSA_Method <- smethod
          totalSI$Variable <- var_using
          TSI <- rbind(TSI, totalSI)
        }
        
      }, error = function(e) {
        if (verbose) cat("      Error calculating sensitivity indices:", e$message, "\n")
      })
    }
  }
  
  # Save results if requested
  if (save_files && !is.null(FSI) && !is.null(TSI)) {
    # Use the last processed method's directory for saving (typically soboljansen)
    cluster_gsa_method_dir <- file.path(lairice_root, output_base, date_stamp, "GSA", smethod, "Results")
    
    # Create Results directory if it doesn't exist
    if (!dir.exists(cluster_gsa_method_dir)) {
      dir.create(cluster_gsa_method_dir, recursive = TRUE)
    }
    
    fsi_file <- file.path(cluster_gsa_method_dir, config$combine_tables_names[1])
    tsi_file <- file.path(cluster_gsa_method_dir, config$combine_tables_names[2])
    
    saveRDS(FSI, fsi_file)
    saveRDS(TSI, tsi_file)
    
    output_paths$first_order_indices <- fsi_file
    output_paths$total_order_indices <- tsi_file
    
    if (verbose) {
      cat("Saved first-order sensitivity indices:", fsi_file, "\n")
      cat("Saved total-order sensitivity indices:", tsi_file, "\n")
    }
  }
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 5 Complete\n")
    if (!is.null(FSI)) {
      cat("First-order indices calculated:", nrow(FSI), "parameter-variable combinations\n")
      cat("Variables processed:", length(unique(FSI$Variable)), "\n")
      cat("Parameters analyzed:", length(unique(FSI$params)), "\n")
    }
    if (!is.null(TSI)) {
      cat("Total-order indices calculated:", nrow(TSI), "parameter-variable combinations\n")
    }
    cat("====================================================================\n")
  }
  
  return(list(
    first_order_indices = FSI,
    total_order_indices = TSI,
    variables_processed = if (!is.null(FSI)) unique(FSI$Variable) else character(0),
    parameters = if (!is.null(FSI)) unique(FSI$params) else character(0),
    gsa_methods = gsa_methods,
    output_paths = output_paths
  ))
}

