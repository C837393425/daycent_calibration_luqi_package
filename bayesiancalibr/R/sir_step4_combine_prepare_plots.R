#' SIR Step 4: Combine Likelihood and Aggregate Results
#'
#' Combines individual job group likelihood and aggregate output files into 
#' consolidated data frames for sensitivity analysis.
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
#'   \item{weighted_combined}{Combined weighted mean results data frame}
#'   \item{job_groups}{Vector of job groups processed}
#'   \item{n_likelihood_files}{Number of likelihood files processed}
#'   \item{n_aggregate_files}{Number of aggregate files processed}
#'   \item{n_weighted_files}{Number of weighted mean files processed}
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

sir_step4a_combine_results <- function(config, 
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
  base_path <- file.path(config$paths$lairice_root, output_base, date_stamp, "SIR")

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
    cat("SIR Step 4A: Combining Results\n")
    cat("GSA Method:", gsa_method, "\n")
    cat("Date Stamp:", date_stamp, "\n")
    cat("Base Path:", base_path, "\n")
    cat("Aggregate Processing:", aggregate_mode, "\n")
    cat("====================================================================\n")
  }
  
  
  # Read MC draw data frame to get job groups
  mc_draw_file <- file.path(base_path, paste0("mc_SIR_draw.rds"))
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
  if (config$file_source$database$database_result$run_type == "file_system") {
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
    likelihood_table <- db_cfg$tables$sir_results_likelihood
    if (is.null(likelihood_table) || !nzchar(likelihood_table)) {
      stop("database_result.tables$sir_results_likelihood must be set in config")
    }
    dbConn_result <- DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = host, dbname = db_calib,
      username = user, password = password
    )
    on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE) 
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
    cat("  Skipping aggregate SQL load (aggregate processing disabled)\n") 

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
      weighted_filename <- paste0("Weighted_Mean_Combined.rds")
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
    cat("SIR Step 4A Complete\n")
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

#' SIR Step 5: Generate Posterior Samples Using SIR Method
#'
#' Generates posterior samples using Sampling Importance Resampling (SIR) method
#' based on likelihood values from combined results. Processes different variable
#' types and model structures to generate sample indices.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param output_base Optional output base directory override (uses config if NULL)
#' @param rseed Random seed for reproducible sampling (default: 16389)
#' @param post_n Number of posterior samples to generate (default: 1000)
#' @param save_files Logical, whether to save sample indices to CSV files (default TRUE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return List containing:
#'   \item{sir_indices}{List of SIR sample indices for each variable/method combination}
#'   \item{likelihood_summaries}{Summary statistics for likelihood values by variable}
#'   \item{max_likelihood_samples}{Sample IDs with maximum likelihood for each variable/method}
#'   \item{output_paths}{Paths where CSV files were saved (if save_files = TRUE)}
#'
#' @export
sir_step4b_posterior_sample <- function(config,
                                       date_stamp = NULL,
                                       output_base = NULL,
                                       rseed = NULL,
                                       post_n = NULL,
                                       save_files = TRUE,
                                       verbose = TRUE) {
  
  # Validate inputs
  if (missing(config)) stop("Configuration object is required")
  
  # Set defaults from config
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
  local_sir_method_dir <- file.path(config$paths$lairice_root, output_base, date_stamp, "SIR")
  if (is.null(rseed)) {
    rseed = config$sir$sir_rseed
  }
  if (is.null(post_n)) {
    post_n = config$sir$sir_n2dir
  }
  
  if (verbose) {
    cat("====================================================================\n")
    cat("SIR Step 5: Generate Posterior Samples\n")
    cat("Date Stamp:", date_stamp, "\n")
    cat("SIR Directory:", local_sir_method_dir, "\n")
    cat("Random Seed:", rseed, "\n")
    cat("Posterior Samples:", post_n, "\n")
    cat("====================================================================\n")
  }
 
  
# Load likelihood data
lkhd_file <- file.path(local_sir_method_dir, "Results", config$combine_tables_names[3])  # Likelihood_Combined_soboljansen.rds
lkhd_df <- readRDS(lkhd_file)

lkhd_df <- dplyr::rename(lkhd_df, ln_logLkhood_ln_ind = ln_independent_logLkhood, logLkhood_ind = independent_logLkhood)


cat("  Likelihood records:", nrow(lkhd_df), "\n")
cat("  Variables in likelihood:", length(unique(lkhd_df$Variable)), "\n")
    
# Prepare likelihood data exactly as in gsa_step7_plot_results_likehood.R
likelihood_vars <- config$combine_tables_prepare_plots$likelihood_vars
likelihood_variable_names <- sapply(likelihood_vars, function(x) x$name)
likelihood_variable_prefixes <- sapply(likelihood_vars, function(x) x$prefix)

# Create separate dataframes for each variable type (matching the R script)
for (i in seq_along(likelihood_variable_names)) {
  var <- likelihood_variable_names[i]
  prefix <- likelihood_variable_prefixes[i]
  df_name <- paste0(prefix, "_lkhd_df")
  
  assign(df_name, lkhd_df[lkhd_df$Variable == var, ])
}


# Loop through each prefix and corresponding index
for (i in seq_along(likelihood_variable_prefixes)) {
  prefix <- likelihood_variable_prefixes[i]
  index <- i
  
  df_name <- paste0(prefix, "_lkhd_df")
  df <- get(df_name)
  
  # Compute the max values
  if(config$emmission_variable == "crop"){

    max_ind <- max(df$logLkhood_ind, na.rm = TRUE)
    max_ln_ind <- max(df$ln_logLkhood_ln_ind, na.rm = TRUE)
    assign(paste0("max_", index, "a"), max_ind)
    assign(paste0("max_", index, "b"), max_ln_ind)

  } else {
    max_ind <- max(df$ln_logLkhood_ln_ind, na.rm = TRUE)
    max_rS <- max(df$ln_logLkhood_rS, na.rm = TRUE)
    max_rSY <- max(df$ln_logLkhood_rSY, na.rm = TRUE)
    
    # Assign to new variable names like max_1b, max_1c, etc.
    assign(paste0("max_", index, "a"), max_ind)
    assign(paste0("max_", index, "b"), max_rS)
    assign(paste0("max_", index, "c"), max_rSY)

  }
}
  
  # Define the prefixes and corresponding max variable indices
  if(config$emmission_variable == "crop"){
    models <- c("ind", "ln_ind")
  } else {
    models <- c("ind", "rS", "rSY")
  }
n_best <- post_n

# Generate SIR indices exactly as in the R script
sir_indices <- list()
set.seed(rseed)
for (i in seq_along(likelihood_variable_prefixes)) {
  prefix <- likelihood_variable_prefixes[i]
  index <- i
  df_name <- paste0(prefix, "_lkhd_df")
  df <- get(df_name)
  
  for (model in models) {

    if(config$emmission_variable == "crop"){
      max_var <- if (model == "ind") paste0("max_", index, "a") else if (model == "ln_ind") paste0("max_", index, "b")
    } else {
      # Construct max variable name (e.g., max_1b, max_1c)
      max_var <- if (model == "ind") paste0("max_", index, "a") else if (model == "rS") paste0("max_", index, "b") else paste0("max_", index, "c")
    }
    
    max_val <- get(max_var)

    # Compute sampling probabilities only for finite log-likelihoods.
    # Crop workflows use raw log-likelihood for ind and ln-log for ln_ind.
    # Non-crop workflows (SOC, NH3, etc.) use ln-log columns for all models.
    if (config$emmission_variable == "crop") {
      if (model == "ln_ind") {
        ll <- df[["ln_logLkhood_ln_ind"]]
      } else {
        ll <- df[["logLkhood_ind"]]
      }
    } else if (model == "ind") {
      ll <- df[["ln_logLkhood_ln_ind"]]
    } else {
      ll <- df[[paste0("ln_logLkhood_", model)]]
    }
    valid <- is.finite(ll)

    if (!any(valid)) {
      warning("All log-likelihoods are NA/Inf for ", prefix, " (", model, "); skipping this combination.")
      next
    }

    prob <- numeric(length(ll))
    prob[valid] <- exp(ll[valid] - max_val)
    
    # Create SIR index variable name
    sir_name <- paste0("sirIndx_", prefix, "_", model)
    
    # Perform sampling (ensure size does not exceed available rows)
    sample_size <- min(n_best, nrow(df))
    sir_row_index <- sample(seq_len(nrow(df)), size = sample_size, replace = FALSE, prob = prob)
    sir_sample_ids <- df$SampleID[sir_row_index]

    # Assign SampleID values (not dataframe row positions)
    assign(sir_name, sir_sample_ids)
    sir_indices[[sir_name]] <- sir_sample_ids
  }
}
cat("Generated SIR indices for likelihood-based sampling\n")
  
  output_paths <- list()
  # Save SIR indices to CSV files if requested
  if (save_files) {
    if (verbose) cat("Saving SIR sample indices to CSV files...\n")

    prefixes <- likelihood_variable_prefixes
    if(config$emmission_variable == "crop"){
      suffixes <- c("ind", "ln_ind")
    } else {
      suffixes <- c("ind", "rS", "rSY")
    }

    for (prefix in prefixes) {
      for (suffix in suffixes) {
        # Construct the variable name dynamically
        var_name <- paste0("sirIndx_", prefix, "_", suffix)

        # Skip combinations for which no SIR indices were generated
        if (!exists(var_name, inherits = FALSE)) {
          if (verbose) {
            warning("No SIR indices available for ", var_name, "; skipping CSV write.")
          }
          next
        }

        sample_ids <- get(var_name)

        # Construct the file path
        file_name <- paste0("best_", n_best, "_", prefix, "_", suffix, ".csv")
        file_path <- file.path(local_sir_method_dir, "Results", file_name)

        # Write to CSV (x column holds SampleID values)
        write.csv(data.frame(x = sample_ids), file_path, row.names = FALSE)

        # Save path to output_paths list
        output_paths[[paste0(prefix, "_", suffix)]] <- file_path
      }
    }

    if (verbose) {
      for (prefix in prefixes) {
        cat("  Saved", prefix, "indices:\n")
        for (suffix in suffixes) {
          file_path <- output_paths[[paste0(prefix, "_", suffix)]]
          cat("    ", suffix, ":", file_path, "\n")
        }
      }
    }
  }

  
  if (verbose) {
  cat("====================================================================\n")
  cat("SIR Step 5 Complete\n")
  cat("Generated", n_best, "posterior samples for each variable/method combination:\n")
  cat("====================================================================\n")
}

  # Print summary statistics as in the R script
  for (i in seq_along(likelihood_variable_prefixes)) {
    prefix <- likelihood_variable_prefixes[i]
    index <- i
    df_name <- paste0(prefix, "_lkhd_df")
    df <- get(df_name)
    
    if(config$emmission_variable == "crop"){
      models <- c("ind", "ln_ind")
    } else {
      models <- c("ind", "rS", "rSY")
    }
    for (model in models) {
      cat("\n=== ", prefix, " Summary ===\n")
      if (config$emmission_variable == "crop" && model == "ind") {
        cat("logLkhood_ind summary:\n")
        print(summary(df[["logLkhood_ind"]]))
      } else if (model == "ind") {
        cat("ln_logLkhood_ln_ind summary:\n")
        print(summary(df[["ln_logLkhood_ln_ind"]]))
      } else {
        cat(paste0("ln_logLkhood_", model, " summary:\n"))
        print(summary(df[[paste0("ln_logLkhood_", model)]]))
      }
    }
  }

  return(list(
    sir_indices = sir_indices,
    output_paths = output_paths
  ))
}

#' Build one SOC model-vs-observation chunk for SIR plot prep
#'
#' Returns a data.frame with SampleID plus rows that have finite mod/obs pairs,
#' or NULL when no valid pairs exist.
build_soc_sir_plot_chunk <- function(task_id, annual_out_dir, soc_annual_obs) {
  result_file <- file.path(annual_out_dir, paste0("dc_annualRslt_", task_id, ".rds"))
  if (!file.exists(result_file)) {
    return(NULL)
  }

  wide_dRslt <- readRDS(result_file)
  wide_dRslt_SOC <- wide_dRslt[wide_dRslt$variable == "somsc", ]
  if (nrow(wide_dRslt_SOC) == 0) {
    return(NULL)
  }

  annualRslt_SOC <- reshape2::melt(
    wide_dRslt_SOC,
    id.vars = c("SampleID", "SiteID", "TreatmentID", "year", "variable", "Model", "unit"),
    variable.name = "day",
    value.name = "mod_SOC"
  )

  soc_annual_combined <- combine_mod_soc_annual(
    annualRslt_SOC = annualRslt_SOC,
    soc_annual = soc_annual_obs
  )

  soc_annual_data <- soc_annual_combined$data_SOC
  soc_annual_data$mod <- soc_annual_data$mod_SOC_gC_m2
  soc_annual_data$obs <- soc_annual_data$C_30cm_gm2
  soc_annual_data$resi <- soc_annual_data$obs - soc_annual_data$mod
  soc_annual_data$ln_mod <- log(soc_annual_data$mod + 1)
  soc_annual_data$ln_obs <- log(soc_annual_data$obs + 1)
  soc_annual_data$ln_resi <- soc_annual_data$ln_obs - soc_annual_data$ln_mod

  soc_annual_plot_rows <- soc_annual_data[
    is.finite(soc_annual_data$mod) & is.finite(soc_annual_data$obs),
  ]
  if (nrow(soc_annual_plot_rows) == 0) {
    return(NULL)
  }

  data.frame(SampleID = task_id, soc_annual_plot_rows)
}

#' Process SIR indices for SOC plot-prep output
process_soc_sir_indices <- function(sir_indices_df,
                                    soc_annual_obs,
                                    all_jobs,
                                    annual_outputs_dir,
                                    verbose = FALSE) {
  result <- NULL

  for (i in seq_len(nrow(sir_indices_df))) {
    task_id <- sir_indices_df$x[i]
    group_id <- all_jobs$JobGroup[all_jobs$SampleID == task_id]
    annual_out_dir <- file.path(annual_outputs_dir, paste0("jobGroup_", group_id))

    chunk <- build_soc_sir_plot_chunk(
      task_id = task_id,
      annual_out_dir = annual_out_dir,
      soc_annual_obs = soc_annual_obs
    )

    if (!is.null(chunk)) {
      result <- rbind(result, chunk)
    } else if (verbose) {
      cat("Warning: No valid mod/obs pairs for task_id", task_id, "- skipping\n")
    }

    cat(i, "\n")
  }

  result
}

#' SIR Step 6: MC Data Preparation for Plots
#'
#' Processes Monte Carlo simulation results and combines them with observational data
#' for plotting and analysis. Processes different variable types (NH3, cumulative NH3, Urea)
#' using SIR sample indices.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param output_base Optional output base directory override (uses config if NULL)
#' @param observation_dir Optional observation directory override (uses config if NULL)
#' @param save_files Logical, whether to save processed data to disk (default TRUE)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return List containing:
#'   \item{processed_data}{List of processed data frames for each sample type}
#'   \item{observation_data}{List of observation data frames}
#'   \item{sample_indices}{List of SIR sample indices}
#'   \item{output_paths}{Paths where files were saved (if save_files = TRUE)}
#'
#' @importFrom reshape2 melt
#' @export
sir_step4c_mc_data_prep <- function(config,
                                   date_stamp = NULL,
                                   output_base = NULL,
                                   observation_dir = NULL,
                                   save_files = TRUE,
                                   verbose = TRUE) {
  
  # Validate inputs
  if (missing(config)) stop("Configuration object is required")
  
  # Check for required packages
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("reshape2 package is required but not available")
  }
  
  # Set defaults from config
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
  
  if (is.null(observation_dir)) {
    observation_dir <- config$paths$observation_dir
  }
  
  # Setup paths
  lairice_root <- config$paths$lairice_root
  mc_dir <- file.path(lairice_root, output_base, date_stamp, "SIR")
  sample_dir <- file.path(mc_dir, "Results")
  observation_path <- file.path(lairice_root, observation_dir)
  likelihood_vars <- config$combine_tables_prepare_plots$likelihood_vars
  
  if (verbose) {
    cat("====================================================================\n")
    cat("SIR Step 6: MC Data Preparation for Plots\n")
    cat("Date Stamp:", date_stamp, "\n")
    cat("Sample Directory:", sample_dir, "\n")
    cat("Observation Directory:", observation_path, "\n")
    cat("====================================================================\n")
  }
  
  # Read MC draw data
  MCdraw_file <- file.path(mc_dir, "mc_SIR_draw.rds")
  if (!file.exists(MCdraw_file)) {
    stop("MC draw file not found: ", MCdraw_file)
  }
  
  all_jobs <- readRDS(MCdraw_file)
  if (verbose) cat("Loaded", nrow(all_jobs), "MC draw records\n")
  
  # Load observation data
  if (verbose) cat("Loading observation data...\n")

  
  # Process observation data
  if (config$emmission_variable == "NH3"){
    mes_NH3       = read.csv(file.path(observation_path, "NH3_allmeasurements_28Feb2025.csv"), stringsAsFactors = FALSE)
    cum_NH3       = read.csv(file.path(observation_path, "cumNH3_allmeasurements_28Feb2025.csv"), stringsAsFactors = FALSE)
    mes_Urea      = read.csv(file.path(observation_path, "Urea_measurements_28Feb2025.csv"), stringsAsFactors = FALSE)

    mes_NH3$meas_start_date  <- as.Date(mes_NH3$meas_doy_begin - 1, origin = paste0(mes_NH3$meas_start_year, "-01-01"))  
    mes_NH3$meas_end_date    <- as.Date(mes_NH3$meas_doy_end - 1, origin = paste0(mes_NH3$meas_end_year, "-01-01")) 
    mes_NH3$MeasurementID    <- paste0("M", 1:nrow(mes_NH3))

    cum_NH3$meas_start_date  <- as.Date(cum_NH3$meas_doy_begin - 1, origin = paste0(cum_NH3$meas_start_year, "-01-01"))  
    cum_NH3$meas_end_date    <- as.Date(cum_NH3$meas_doy_end - 1, origin = paste0(cum_NH3$meas_end_year, "-01-01")) 

    mes_Urea          = mes_Urea[mes_Urea$DAF != 0, ]
    mes_Urea$mes_date = as.Date(mes_Urea$dayofyr - 1, origin = paste0(mes_Urea$year, "-01-01"))  

      if (verbose) {
      cat("Loaded observation data:\n")
      cat("  Individual NH3:", nrow(mes_NH3), "records\n")
      cat("  Cumulative NH3:", nrow(cum_NH3), "records\n")
      cat("  Individual Urea:", nrow(mes_Urea), "records\n")
    }
  } else if(config$emmission_variable == "SOC"){
    soc_annual <- read.csv(file.path(observation_path, config$input_files$observation_data$soc_measurements_file), stringsAsFactors = FALSE)
 
  } else if(config$emmission_variable == "crop"){
    mes_crop_yield <- read.csv(file.path(observation_path, "ObservData_crop.csv"), stringsAsFactors = FALSE)
  }
  
  
  # Load SIR sample indices
  if (verbose) cat("Loading SIR sample indices...\n")
  
  if (config$emmission_variable == "NH3"){
    sirIndx_cumNH3_rS <- read.csv(file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_cumNH3_rS.csv")), stringsAsFactors = FALSE)
    sirIndx_cumNH3_rSY <- read.csv(file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_cumNH3_rSY.csv")), stringsAsFactors = FALSE)
    sirIndx_indUrea_rS <- read.csv(file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_indUrea_rS.csv")), stringsAsFactors = FALSE)
  } else if(config$emmission_variable == "SOC"){
    sirIndx_soc_annual_ind <- NULL
    sirIndx_soc_annual_rS <- NULL
    sirIndx_soc_annual_rSY <- NULL

    soc_ind_file  <- file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_soc_ind.csv"))
    soc_rS_file  <- file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_soc_rS.csv"))
    soc_rSY_file <- file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_soc_rSY.csv"))

    if (file.exists(soc_ind_file)) {
      sirIndx_soc_annual_ind <- read.csv(soc_ind_file, stringsAsFactors = FALSE)
    } else if (verbose) {
      warning("SIR index file not found for SOC independent model: ", soc_ind_file, " – skipping ind analysis.")
    }

    if (file.exists(soc_rS_file)) {
      sirIndx_soc_annual_rS <- read.csv(soc_rS_file, stringsAsFactors = FALSE)
    } else if (verbose) {
      warning("SIR index file not found for SOC random-site model: ", soc_rS_file, " – skipping rS analysis.")
    }

    if (file.exists(soc_rSY_file)) {
      sirIndx_soc_annual_rSY <- read.csv(soc_rSY_file, stringsAsFactors = FALSE)
    } else if (verbose) {
      warning("SIR index file not found for SOC random-site/year model: ", soc_rSY_file, " – skipping rSY analysis.")
    }
  } else if(config$emmission_variable == "crop"){
    crop_type = sapply(likelihood_vars, function(x) x$prefix)
    sirIndx_crop_annual_ind <- read.csv(file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_", crop_type, "_ind.csv")), stringsAsFactors = FALSE)
    sirIndx_crop_annual_ln_ind <- read.csv(file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_", crop_type, "_ln_ind.csv")), stringsAsFactors = FALSE)
    #sirIndx_crop_annual_rS <- read.csv(file.path(sample_dir, paste0("best_", config$sir$sir_n2dir, "_", crop_type, "_rS.csv")), stringsAsFactors = FALSE)
    #sirIndx_crop_annual_rSY <- read.csv(file.path(sample_dir, paste0("best_", config$sir$sir_n2dir,"_", crop_type, "_rSY.csv")), stringsAsFactors = FALSE)
  }
  
  # Analysis from different parameters with random site and random site/season model
  if (config$emmission_variable == "NH3"){
    # Analysis from Cumulative-NH3 with random site model:
    cumNH3_cumNH3_rS  <- NULL

    for(i in 1:nrow(sirIndx_cumNH3_rS)){
      task_id = sirIndx_cumNH3_rS$x[i]
      group_id = all_jobs$JobGroup[all_jobs$SampleID == task_id]
      daily_out_dir    <- file.path(lairice_root, output_base, date_stamp, "SIR", "Daily_Outputs", paste0("jobGroup_",group_id))
      
      wide_dRslt      = readRDS(file.path(daily_out_dir, paste0("dc_dRslt_",task_id,".rds")))
      wide_dRslt_NH3  = wide_dRslt[wide_dRslt$variable == "NH3.N", ]
      
      dRslt_NH3 <- melt(wide_dRslt_NH3, id.vars = c("SampleID", "SiteID", "TreatmentID", "year", "variable", "Model", "unit"),
                        variable.name = "day", value.name = "mod_NH3")
      
      dRslt_NH3$dayofyr  <- as.numeric(substr(as.character(dRslt_NH3$day), 2, nchar(as.character(dRslt_NH3$day))))
      dRslt_NH3$mod_date <- as.Date(dRslt_NH3$dayofyr - 1, origin = paste0(dRslt_NH3$year, "-01-01"))    
      
      
      # Cumulative measurement window (meas_start_date:meas_end_data)
      cumNH3_combined     = combine_mod_mes_NH3(dRslt_NH3 = dRslt_NH3,
                                                mes_NH3 = cum_NH3)
      
      cumNH3_data         = cumNH3_combined$data_NH3
      cumNH3_data$mod     = cumNH3_data$mod_NH3vol_gN_ha_day
      cumNH3_data$obs     = cumNH3_data$NH3vol_gN_ha_day
      cumNH3_data$resi   = cumNH3_data$obs - cumNH3_data$mod
      cumNH3_data$ln_mod  = log(cumNH3_data$mod + 1)
      cumNH3_data$ln_obs  = log(cumNH3_data$obs + 1)
      cumNH3_data$ln_resi = cumNH3_data$ln_obs - cumNH3_data$ln_mod
      
      temp_df2 = data.frame("SampleID" = task_id,
                            cumNH3_data)
      cumNH3_cumNH3_rS = rbind(cumNH3_cumNH3_rS, 
                              temp_df2)
      
      rm(wide_dRslt, wide_dRslt_NH3, dRslt_NH3,  cumNH3_combined, cumNH3_data, temp_df2)
      cat(i, "\n")
    }

    write.csv(cumNH3_cumNH3_rS, file.path(sample_dir, "CumulativeNH3_with_cumNH3_rS.csv"), row.names = FALSE)

    rm(cumNH3_cumNH3_rS)


    #-------------------------------------------------------------------------------
    # Analysis from cumulative-NH3 with random site/season model:
    cumNH3_cumNH3_rSY  <- NULL

    for(i in 1:nrow(sirIndx_cumNH3_rSY)){
      task_id = sirIndx_cumNH3_rSY$x[i]
      group_id = all_jobs$JobGroup[all_jobs$SampleID == task_id]
      daily_out_dir    <- file.path(lairice_root, output_base, date_stamp, "SIR", "Daily_Outputs", paste0("jobGroup_",group_id))
      
      wide_dRslt      = readRDS(file.path(daily_out_dir, paste0("dc_dRslt_",task_id,".rds")))
      wide_dRslt_NH3  = wide_dRslt[wide_dRslt$variable == "NH3.N", ]
      
      dRslt_NH3 <- melt(wide_dRslt_NH3, id.vars = c("SampleID", "SiteID", "TreatmentID", "year", "variable", "Model", "unit"),
                        variable.name = "day", value.name = "mod_NH3")
      
      dRslt_NH3$dayofyr  <- as.numeric(substr(as.character(dRslt_NH3$day), 2, nchar(as.character(dRslt_NH3$day))))
      dRslt_NH3$mod_date <- as.Date(dRslt_NH3$dayofyr - 1, origin = paste0(dRslt_NH3$year, "-01-01"))    
      
      # Cumulative measurement window (meas_start_date:meas_end_data)
      cumNH3_combined     = combine_mod_mes_NH3(dRslt_NH3 = dRslt_NH3,
                                                mes_NH3 = cum_NH3)
      
      cumNH3_data         = cumNH3_combined$data_NH3
      cumNH3_data$mod     = cumNH3_data$mod_NH3vol_gN_ha_day
      cumNH3_data$obs     = cumNH3_data$NH3vol_gN_ha_day
      cumNH3_data$resi   = cumNH3_data$obs - cumNH3_data$mod
      cumNH3_data$ln_mod  = log(cumNH3_data$mod + 1)
      cumNH3_data$ln_obs  = log(cumNH3_data$obs + 1)
      cumNH3_data$ln_resi = cumNH3_data$ln_obs - cumNH3_data$ln_mod
      
      temp_df2 = data.frame("SampleID" = task_id,
                            cumNH3_data)
      cumNH3_cumNH3_rSY = rbind(cumNH3_cumNH3_rSY, 
                                temp_df2)
      
      
      rm(wide_dRslt, wide_dRslt_NH3, dRslt_NH3, cumNH3_combined, cumNH3_data, temp_df2)
      cat(i, "\n")
    }

    write.csv(cumNH3_cumNH3_rSY, file.path(sample_dir, "CumulativeNH3_with_cumNH3_rSY.csv"), row.names = FALSE)

    rm(cumNH3_cumNH3_rSY)

    #-------------------------------------------------------------------------------
    # Analysis from Individual-Urea with random site model:
    indUrea_indUrea_rS <- NULL

    for(i in 1:nrow(sirIndx_indUrea_rS)){
      task_id = sirIndx_indUrea_rS$x[i]
      group_id = all_jobs$JobGroup[all_jobs$SampleID == task_id]
      daily_out_dir    <- file.path(lairice_root, output_base, date_stamp, "SIR", "Daily_Outputs", paste0("jobGroup_",group_id))
      
      wide_dRslt      = readRDS(file.path(daily_out_dir, paste0("dc_dRslt_",task_id,".rds")))
      # Read Modeled Urea and combine with the measured Urea for likelihood calculation:
      wide_dRslt_Urea <- wide_dRslt[wide_dRslt$variable != "NH3.N", ]
      
      dRslt_Urea <- melt(wide_dRslt_Urea, id.vars = c("SampleID", "SiteID", "TreatmentID", "year", "variable", "Model", "unit"),
                        variable.name = "day", value.name = "mod_Urea")
      
      dRslt_Urea$dayofyr  <- as.numeric(substr(as.character(dRslt_Urea$day), 2, nchar(as.character(dRslt_Urea$day))))
      dRslt_Urea$mod_date <- as.Date(dRslt_Urea$dayofyr - 1, origin = paste0(dRslt_Urea$year, "-01-01"))    
      
      indUrea_combined     = combine_mod_mes_Urea(dRslt_Urea = dRslt_Urea, 
                                                  mes_Urea = mes_Urea)
      
      indUrea_data         = indUrea_combined$data_Urea
      indUrea_data$mod     = indUrea_data$mod_urea_gN_m2
      indUrea_data$obs     = indUrea_data$Urea_gN_m2
      indUrea_data$resi    = indUrea_data$obs - indUrea_data$mod
      indUrea_data$ln_mod  = log(indUrea_data$mod + 1)
      indUrea_data$ln_obs  = log(indUrea_data$obs + 1)
      indUrea_data$ln_resi = indUrea_data$ln_obs - indUrea_data$ln_mod
      
      temp_df3 = data.frame("SampleID" = task_id,
                            indUrea_data)
      indUrea_indUrea_rS = rbind(indUrea_indUrea_rS, 
                                temp_df3)
      
      rm(wide_dRslt, wide_dRslt_Urea, dRslt_Urea, indUrea_combined, indUrea_data, temp_df3)
      cat(i, "\n")
    }

    write.csv(indUrea_indUrea_rS, file.path(sample_dir, "IndividualUrea_with_indUrea_rS.csv"), row.names = FALSE)

    rm(indUrea_indUrea_rS)

  } else if(config$emmission_variable == "SOC"){
    annual_outputs_dir <- file.path(lairice_root, output_base, date_stamp, "SIR", "Annual_Outputs")

    if (!is.null(sirIndx_soc_annual_rS) && nrow(sirIndx_soc_annual_rS) > 0) {
      soc_annual_soc_annual_rS <- process_soc_sir_indices(
        sir_indices_df = sirIndx_soc_annual_rS,
        soc_annual_obs = soc_annual,
        all_jobs = all_jobs,
        annual_outputs_dir = annual_outputs_dir,
        verbose = verbose
      )

      if (!is.null(soc_annual_soc_annual_rS) && nrow(soc_annual_soc_annual_rS) > 0) {
        write.csv(soc_annual_soc_annual_rS, file.path(sample_dir, "SOC_with_soc_annual_rS.csv"), row.names = FALSE)
      } else if (verbose) {
        warning("No SOC plot rows generated for random-site model; skipping SOC_with_soc_annual_rS.csv.")
      }

      rm(soc_annual_soc_annual_rS)
    } else if (verbose) {
      warning("No SIR indices loaded for SOC random-site model; skipping SOC_with_soc_annual_rS.csv generation.")
    }

    if (!is.null(sirIndx_soc_annual_rSY) && nrow(sirIndx_soc_annual_rSY) > 0) {
      soc_annual_soc_annual_rSY <- process_soc_sir_indices(
        sir_indices_df = sirIndx_soc_annual_rSY,
        soc_annual_obs = soc_annual,
        all_jobs = all_jobs,
        annual_outputs_dir = annual_outputs_dir,
        verbose = verbose
      )

      if (!is.null(soc_annual_soc_annual_rSY) && nrow(soc_annual_soc_annual_rSY) > 0) {
        write.csv(soc_annual_soc_annual_rSY, file.path(sample_dir, "SOC_with_soc_annual_rSY.csv"), row.names = FALSE)
      } else if (verbose) {
        warning("No SOC plot rows generated for random-site/year model; skipping SOC_with_soc_annual_rSY.csv.")
      }

      rm(soc_annual_soc_annual_rSY)
    } else if (verbose) {
      warning("No SIR indices loaded for SOC random-site/year model; skipping SOC_with_soc_annual_rSY.csv generation.")
    }

    if (!is.null(sirIndx_soc_annual_ind) && nrow(sirIndx_soc_annual_ind) > 0) {
      if ("exp2_schedule" %in% names(soc_annual) && !"treatment_schedule" %in% names(soc_annual)) {
        soc_annual_ind <- soc_annual %>% rename(treatment_schedule = exp2_schedule)
      } else {
        soc_annual_ind <- soc_annual
      }

      soc_annual_soc_annual_ind <- process_soc_sir_indices(
        sir_indices_df = sirIndx_soc_annual_ind,
        soc_annual_obs = soc_annual_ind,
        all_jobs = all_jobs,
        annual_outputs_dir = annual_outputs_dir,
        verbose = verbose
      )

      if (!is.null(soc_annual_soc_annual_ind) && nrow(soc_annual_soc_annual_ind) > 0) {
        write.csv(soc_annual_soc_annual_ind, file.path(sample_dir, "SOC_with_soc_annual_ind.csv"), row.names = FALSE)
      } else if (verbose) {
        warning("No SOC plot rows generated for independent model; skipping SOC_with_soc_annual_ind.csv.")
      }

      rm(soc_annual_soc_annual_ind)
    } else if (verbose) {
      warning("No SIR indices loaded for SOC independent model; skipping SOC_with_soc_annual_ind.csv generation.")
    }

  } else if(config$emmission_variable == "crop"){
    
    # Analysis from crop with random site and year model:
    crop_annual_crop_annual_ind <- NULL
    crop_annual_crop_annual_ln_ind <- NULL
    dbConn_weighted <- NULL
    weighted_table <- NULL

    if (config$file_source$database$database_result$run_type == "database") {
      weighted_table <- config$file_source$database$database_result$tables$sir_results_weighted
      host <- config$file_source$database$host
      db_calib <- config$file_source$database$database
      cred_file_cfg <- config$file_source$database$cred_file
      if (is.null(cred_file_cfg) || length(cred_file_cfg) == 0) {
        cred_file_cfg <- "~/.dblogin"
      }
      cred_file <- path.expand(cred_file_cfg)
      cred <- readLines(cred_file, warn = FALSE)
      cred <- trimws(cred)
      cred <- cred[nzchar(cred)]
      if (length(cred) < 2) stop("Credential file must contain at least 2 lines (username, password)")
      username <- cred[1]
      password <- cred[2]
      dbConn_weighted <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = host, dbname = db_calib,
        username = username, password = password
      )
      on.exit(
        if (!is.null(dbConn_weighted) && DBI::dbIsValid(dbConn_weighted)) {
          DBI::dbDisconnect(dbConn_weighted)
        },
        add = TRUE
      )
    }

    combine_sirIndx <- rbind(sirIndx_crop_annual_ind, sirIndx_crop_annual_ln_ind)

    for(i in 1:nrow(combine_sirIndx)){
      task_id = combine_sirIndx$x[i]
      group_id = all_jobs$JobGroup[all_jobs$SampleID == task_id]
      if(config$file_source$database$database_result$run_type == "file_system") {
        weighted_out_dir    <- file.path(lairice_root, output_base, date_stamp, "SIR", "Weighted_Mean_Outputs", paste0("jobGroup_",group_id))
        wide_dRslt      = readRDS(file.path(weighted_out_dir, paste0("weighted_cgrain_sample_",task_id,".rds")))
      } else if(config$file_source$database$database_result$run_type == "database") {
        qh <- paste0(
          "SELECT * FROM `", weighted_table, "` WHERE simulation_id = ", as.integer(task_id), ";"
        )
        wide_dRslt <- DBI::dbGetQuery(dbConn_weighted, qh)
      }
      # Read Modeled crop and combine with the measured crop for likelihood calculation:
      wide_dRslt_crop <- wide_dRslt[wide_dRslt$variable_name == "cgrain", ]
      
      weightedRslt_crop <- wide_dRslt_crop %>%
       select(sample_id, aggregation_level, year, total_weight, variable_name,  n_sites, weighted_value) %>%
       rename(mod_cgrain = weighted_value)

      crop_annual_combined     = combine_mod_crop_annual(weightedRslt_crop = weightedRslt_crop, 
                                                         crop_annual = mes_crop_yield)
      
      crop_annual_data         = crop_annual_combined$data_crop
      crop_annual_data$mod     = crop_annual_data$mod_cgrain
      crop_annual_data$obs     = crop_annual_data$cgrain_gm2
      crop_annual_data$resi    = crop_annual_data$obs - crop_annual_data$mod
      crop_annual_data$ln_mod  = log(crop_annual_data$mod + 1)
      crop_annual_data$ln_obs  = log(crop_annual_data$obs + 1)
      crop_annual_data$ln_resi = crop_annual_data$ln_obs - crop_annual_data$ln_mod

      #crop_annual_data_clean <- na.omit(crop_annual_data)

      if (nrow(crop_annual_data) > 0) {
        temp_df3 = data.frame("SampleID" = task_id,
                              crop_annual_data)
        crop_annual_crop_annual_ind = rbind(crop_annual_crop_annual_ind, temp_df3)
      } else {
        cat("Warning: No valid data for task_id", task_id, "- skipping\n")
      }

      rm(wide_dRslt, wide_dRslt_crop, weightedRslt_crop, crop_annual_combined, crop_annual_data, temp_df3)
      cat(i, "\n")
    }

    # ind
    crop_annual_ind <- crop_annual_crop_annual_ind %>%
      filter(SampleID %in% sirIndx_crop_annual_ind$x)
    # ln_ind
    crop_annual_ln_ind <- crop_annual_crop_annual_ind %>%
      filter(SampleID %in% sirIndx_crop_annual_ln_ind$x)

    write.csv(crop_annual_ind, file.path(sample_dir, paste0("crop_with_", crop_type, "_ind.csv")), row.names = FALSE)
    write.csv(crop_annual_ln_ind, file.path(sample_dir, paste0("crop_with_", crop_type, "_ln_ind.csv")), row.names = FALSE)

    rm(crop_annual_ind, crop_annual_ln_ind)


  }

  if (verbose) {
    cat("====================================================================\n")
    cat("SIR Step 6 Complete\n")
    cat("====================================================================\n")
  }
  
  observation_data <- if (config$emmission_variable == "NH3") {
    list(
      mes_NH3 = mes_NH3,
      cum_NH3 = cum_NH3,
      mes_Urea = mes_Urea
    )
  } else if (config$emmission_variable == "SOC") {
    list(
      soc_annual = soc_annual
    )
  } else {
    list()  # default empty list if neither condition matches
  }

  return(list(
    observation_data = observation_data
  ))


}
