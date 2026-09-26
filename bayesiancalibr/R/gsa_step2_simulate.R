#' Prepare Parameter Set for Simulation
#'
#' Creates a parameter data frame from job parameters and configuration
#'
#' @param config Configuration object
#' @param job_params Job-specific parameters from Monte Carlo draws
#' @param verbose Logical indicating whether to print progress
#' @return Data frame with File, Parameter, and value columns
#' @export
prepare_parameter_set <- function(config, job_params, verbose = TRUE) {
  
  # Read prior parameter file to get parameter names and file mappings
  prior_file <- config$input_files$prior_file
  if (!file.exists(prior_file)) {
    stop("Prior file not found: ", prior_file)
  }
  
  prior <- read.csv(prior_file, stringsAsFactors = FALSE)
  
  # Read default parameters if specified
  if ("default_params" %in% names(config$input_files)) {
    default_file <- config$input_files$default_params
    if (file.exists(default_file)) {
      dflt <- read.csv(default_file, stringsAsFactors = FALSE)
      dflt2 <- dflt[!(dflt$Parameter %in% prior$Parameter), ]
      dfltdf <- dflt2[, c("File", "Parameter", "Default")]
      names(dfltdf)[which(names(dfltdf) == "Default")] <- "value"
    } else {
      if (verbose) cat("Default parameters file not found, using only prior parameters\n")
      dfltdf <- data.frame(File = character(0), Parameter = character(0), value = numeric(0))
    }
  } else {
    dfltdf <- data.frame(File = character(0), Parameter = character(0), value = numeric(0))
  }
  
  # Create parameter data frame from job parameters
  temp_df <- prior[, c("File", "Parameter")]
  
  # Extract parameter values from job_params (exclude SampleID and JobGroup columns)
  param_values <- as.numeric(job_params[, !names(job_params) %in% c("SampleID", "JobGroup")])
  temp_df$value <- param_values
  
  # Combine with default parameters
  params_df <- rbind(temp_df, dfltdf)
  
  if (verbose) {
    cat("Prepared parameter set with", nrow(params_df), "parameters\n")
    cat("  Prior parameters:", nrow(temp_df), "\n")
    cat("  Default parameters:", nrow(dfltdf), "\n")
  }
  
  return(params_df)
}


# =============================================================================
# FUNCTIONS FROM output_processing_generic.R
# =============================================================================

#' Load Output Specifications from CSV Files
#'
#' Reads and validates the output variables and files specification files
#' to create a structured specification object for generic output processing.
#'
#' @param config Configuration object containing paths to specification files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing validated output specifications:
#'   \item{variables}{Data frame with variable specifications}
#'   \item{files}{Data frame with file specifications}
#'   \item{enabled_files}{Vector of enabled output file names}
#'   \item{daily_variables}{Data frame with variables marked for daily output}
#'   \item{aggregated_variables}{Data frame with variables marked for aggregation}
#'
#' @details
#' This function loads the specification files defined in the configuration:
#' - output_variables_spec: CSV file mapping variables to files, transforms, and observations
#' - output_files_spec: CSV file defining which output files to process
#'
#' The function validates that all required columns exist and that enabled files
#' have corresponding variable specifications.
#'
#' @examples
#' \dontrun{
#' config <- read_yaml_config("workflows/configs/nh3_volatilization.yaml")
#' config <- resolve_config_paths(config)
#' output_specs <- load_output_specifications(config, verbose = TRUE)
#' }
#'
#' @export
load_output_specifications <- function(config, verbose = TRUE) {
  
  if (verbose) cat("Loading output specifications...\n")
  
  # Validate configuration
  if (!"model_outputs" %in% names(config)) {
    stop("Configuration missing 'model_outputs' section")
  }
  
  model_outputs <- config$model_outputs
  
  if (!"output_variables_spec" %in% names(model_outputs)) {
    stop("Configuration missing 'model_outputs$output_variables_spec'")
  }
  
  if (!"output_files_spec" %in% names(model_outputs)) {
    stop("Configuration missing 'model_outputs$output_files_spec'")
  }
  
  # Load variables specification
  var_spec_file <- file.path(config$paths$lairice_root, model_outputs$output_variables_spec)
  if (!file.exists(var_spec_file)) {
    stop("Output variables specification file not found: ", var_spec_file)
  }
  
  variables_spec <- read.csv(var_spec_file, stringsAsFactors = FALSE)
  
  # Validate variables specification columns
  required_var_cols <- c("output_file", "variable_name", "variable_description", 
                        "unit", "daily_output", "annual_output", "aggregate_output", 
                        "aggregate_name", "transform_function")
  
  missing_var_cols <- setdiff(required_var_cols, names(variables_spec))
  if (length(missing_var_cols) > 0) {
    stop("Variables specification missing required columns: ", 
         paste(missing_var_cols, collapse = ", "))
  }
  
  # Load files specification
  files_spec_file <- file.path(config$paths$lairice_root, model_outputs$output_files_spec)
  if (!file.exists(files_spec_file)) {
    stop("Output files specification file not found: ", files_spec_file)
  }
  
  files_spec <- read.csv(files_spec_file, stringsAsFactors = FALSE)
  
  # Validate files specification columns
  required_file_cols <- c("output_file", "file_description", "enable_output")
  missing_file_cols <- setdiff(required_file_cols, names(files_spec))
  if (length(missing_file_cols) > 0) {
    stop("Files specification missing required columns: ", 
         paste(missing_file_cols, collapse = ", "))
  }
  
  # Get enabled files (robust to 1/"1"/TRUE/"TRUE")
  enable_col <- files_spec$enable_output
  enabled_mask <- enable_col %in% c(1, "1", TRUE, "TRUE", "true", "T", "t")
  enabled_files <- files_spec$output_file[enabled_mask]
  enabled_files <- enabled_files[!is.na(enabled_files) & nzchar(trimws(enabled_files))]
  if (length(enabled_files) == 0) {
    warning("No enabled output files found in output_files_spec (enable_output). ",
            "Check: ", files_spec_file)
  }
  
  # Filter variables to only those from enabled files
  enabled_variables <- variables_spec[variables_spec$output_file %in% enabled_files, ]
  
  # Separate daily, annual, period, and aggregated variables
  daily_variables <- enabled_variables[enabled_variables$daily_output == TRUE, ]
  annual_variables <- enabled_variables[enabled_variables$annual_output == TRUE, ]
  aggregated_variables <- enabled_variables[enabled_variables$aggregate_output == TRUE, ]
  if ("period_output" %in% names(enabled_variables)) {
    period_variables <- enabled_variables[enabled_variables$period_output %in% c(TRUE, "TRUE", "true", 1, "1"), ]
  } else {
    period_variables <- enabled_variables[enabled_variables$transform_function == "biweekly_sum", ]
  }
  
  if (verbose) {
    cat("  Variables specification loaded:", nrow(variables_spec), "variables\n")
    cat("  Files specification loaded:", nrow(files_spec), "files\n")
    cat("  Enabled files:", length(enabled_files), "\n")
    cat("  Daily output variables:", nrow(daily_variables), "\n")
    cat("  Annual output variables:", nrow(annual_variables), "\n")
    cat("  Period output variables:", nrow(period_variables), "\n")
    cat("  Aggregated variables:", nrow(aggregated_variables), "\n")
  }
  
  # Return structured specification object
  list(
    variables = variables_spec,
    files = files_spec,
    enabled_files = enabled_files,
    daily_variables = daily_variables,
    annual_variables = annual_variables,
    period_variables = period_variables,
    aggregated_variables = aggregated_variables
  )
}

#' Apply Transform Function to Data
#'
#' Applies the specified transform function to data values, supporting
#' various aggregation and transformation methods.
#'
#' @param data Numeric vector or data frame containing the data to transform
#' @param transform_name Character string specifying the transform function
#' @param verbose Logical indicating whether to print progress messages
#' @return Numeric value representing the transformed result
#'
#' @details
#' Supported transform functions:
#' - abs_sum_div_1e6: sum(abs(data)) / 1000000
#' - sum_div_1e6: sum(data) / 1000000
#' - mean_transform: mean(data, na.rm = TRUE)
#' - sum_transform: sum(data, na.rm = TRUE)
#' - computed_n2o: sum(abs(data$nit_N2O.N + data$dnit_N2O.N)) / 1000000
#'
#' @examples
#' \dontrun{
#' result <- apply_transform_function(c(1000, 2000, 3000), "abs_sum_div_1e6")
#' # Returns: 0.006
#' }
#'
#' @export
apply_transform_function <- function(data, transform_name, verbose = FALSE) {
  
  if (verbose) cat("Applying transform:", transform_name, "\n")
  
  result <- switch(transform_name,
    "abs_sum_div_1e6" = sum(abs(data), na.rm = TRUE) / 1000000,
    "sum_div_1e6" = sum(data, na.rm = TRUE) / 1000000,
    "mean_transform" = mean(data, na.rm = TRUE),
    "sum_transform" = sum(data, na.rm = TRUE),
    "computed_n2o" = {
      if (is.data.frame(data) && all(c("nit_N2O.N", "dnit_N2O.N") %in% names(data))) {
        # Match original computation order exactly: sum first, then abs
        daycent_n2o <- data$nit_N2O.N + data$dnit_N2O.N
        sum(abs(daycent_n2o), na.rm = TRUE) / 1000000
      } else {
        stop("computed_n2o requires data frame with nit_N2O.N and dnit_N2O.N columns")
      }
    },
    stop("Unknown transform function: ", transform_name)
  )
  
  if (verbose) cat("Transform result:", result, "\n")
  
  return(result)
}

#' Get Observation Years for Variable
#'
#' Extracts observation years for a specific variable and treatment from
#' the observation data files, using the variable specification.
#'
#' @param var_spec Single row data frame containing variable specification
#' @param trt_sch_file Treatment schedule file name
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return Numeric vector of observation years
#'
#' @details
#' This function reads the observation file specified in var_spec and extracts
#' the years where measurements exist for the given treatment. It handles
#' both single year columns and year range columns (start;end format).
#'
#' @examples
#' \dontrun{
#' obs_years <- get_observation_years_generic(var_spec, "treatment1.sch", 
#'                                           "data/observation", verbose = TRUE)
#' }
#'
#' @export
get_observation_years_generic <- function(var_spec, trt_sch_file, obs_data_dir, verbose = FALSE) {
  
  # Check if observation file is specified
  if (is.na(var_spec$observation_file) || var_spec$observation_file == "") {
    if (verbose) cat("No observation file specified for", var_spec$variable_name, "\n")
    return(numeric(0))
  }
  
  # Build observation file path
  obs_file_path <- file.path(obs_data_dir, var_spec$observation_file)
  
  if (!file.exists(obs_file_path)) {
    if (verbose) cat("Observation file not found:", obs_file_path, "\n")
    return(numeric(0))
  }
  
  if (verbose) cat("Loading observation data:", var_spec$observation_file, "\n")
  
  # Read observation data
  obs_data <- read.csv(obs_file_path, stringsAsFactors = FALSE)
  
  # Filter by treatment schedule if match column is specified
  if (!is.na(var_spec$observation_match_column) && var_spec$observation_match_column != "") {
    match_col <- var_spec$observation_match_column
    if (match_col %in% names(obs_data)) {
      obs_data <- obs_data[obs_data[[match_col]] == trt_sch_file, ]
    } else {
      if (verbose) cat("Match column", match_col, "not found in observation data\n")
      return(numeric(0))
    }
  }
  
  if (nrow(obs_data) == 0) {
    if (verbose) cat("No matching observations for treatment:", trt_sch_file, "\n")
    return(numeric(0))
  }
  
  # Extract years based on year columns specification
  if (is.na(var_spec$observation_year_columns) || var_spec$observation_year_columns == "") {
    if (verbose) cat("No year columns specified\n")
    return(numeric(0))
  }
  
  year_cols <- trimws(strsplit(var_spec$observation_year_columns, ";")[[1]])
  
  # Match original behavior: only use the FIRST year column (start years)
  # This matches the original R unique(vec1, vec2) behavior which only processes vec1
  # Each measurement period represents ONE observation regardless of duration
  first_year_col <- year_cols[1]
  
  if (first_year_col %in% names(obs_data)) {
    obs_years <- unique(obs_data[[first_year_col]])
    if (verbose) cat("Using only first year column:", first_year_col, "\n")
  } else {
    if (verbose) cat("First year column", first_year_col, "not found in observation data\n")
    obs_years <- c()
  }
  
  # Remove NA values and return sorted unique years
  obs_years <- sort(unique(obs_years[!is.na(obs_years)]))
  
  if (verbose) cat("Found observation years:", paste(obs_years, collapse = ", "), "\n")
  
  return(obs_years)
}

#' Initialize Aggregated Variables from Specifications
#'
#' Creates a named list of aggregated variables initialized to zero,
#' based on the variable specifications.
#'
#' @param output_specs Output specifications object from load_output_specifications()
#' @param verbose Logical indicating whether to print progress messages
#' @return Named list of aggregated variables initialized to zero
#'
#' @details
#' This function replaces the hardcoded aggregated variable initialization
#' with a dynamic approach based on the specifications.
#'
#' @examples
#' \dontrun{
#' output_specs <- load_output_specifications(config)
#' agg_vars <- initialize_aggregated_variables_generic(output_specs)
#' }
#'
#' @export
initialize_aggregated_variables_generic <- function(output_specs, verbose = FALSE) {
  
  agg_vars <- list()
  
  # Initialize all aggregated variables to zero
  if (nrow(output_specs$aggregated_variables) > 0) {
    for (i in 1:nrow(output_specs$aggregated_variables)) {
    var_row <- output_specs$aggregated_variables[i, ]
    agg_name <- var_row$aggregate_name
    
    if (!is.na(agg_name) && agg_name != "") {
      agg_vars[[agg_name]] <- 0
    }
  }
  
  } # End if (nrow(output_specs$aggregated_variables) > 0)
  
  if (verbose) {
    cat("Initialized aggregated variables:", length(agg_vars), "\n")
    cat("Variables:", paste(names(agg_vars), collapse = ", "), "\n")
  }
  
  return(agg_vars)
}

#' Create Aggregated Results Data Frame
#'
#' Creates a data frame with aggregated simulation results based on
#' the accumulated aggregated variables.
#'
#' @param aggregated_results Named list of aggregated variables
#' @param sample_id Sample ID for this simulation
#' @param verbose Logical indicating whether to print progress messages
#' @return Data frame with aggregated results
#'
#' @details
#' This function replaces the hardcoded aggregated results creation
#' with a dynamic approach that creates columns based on the variable names
#' in the aggregated variables list.
#'
#' @examples
#' \dontrun{
#' agg_result <- create_aggregated_results_generic(aggregated_results, 1234)
#' }
#'
#' @export
create_aggregated_results_generic <- function(aggregated_results, sample_id, verbose = FALSE) {
  
  # Create base data frame with SampleID
  result_df <- data.frame(SampleID = sample_id, stringsAsFactors = FALSE)
  
  # Add all aggregated variables as columns
  for (var_name in names(aggregated_results)) {
    result_df[[var_name]] <- aggregated_results[[var_name]]
  }
  
  if (verbose) {
    cat("Created aggregated results with", ncol(result_df) - 1, "variables\n")
  }
  
  return(result_df)
}


#' Run Site Simulations Generically
#'
#' Generic replacement for the hardcoded run_site_simulations() function.
#' This function provides the same interface but uses specification-driven
#' processing for model outputs instead of hardcoded logic.
#'
#' @param site_id Site identifier
#' @param run_file_site Run file for this site
#' @param config Configuration object
#' @param params_df Data frame with parameters for this simulation
#' @param sim_dir_tid Simulation directory for this task ID
#' @param daycent_exe Path to DayCent executable
#' @param actual_task_id Task ID for this simulation
#' @param agg_vars Aggregated variables list
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing simulation results
#'
#' @details
#' This function can be used as a drop-in replacement for the original
#' run_site_simulations() function to enable comparison testing between
#' the hardcoded and generic approaches.
#'
#' @examples
#' \dontrun{
#' result <- run_site_simulations_generic(site_id, run_file_site, config,
#'                                       params_df, sim_dir_tid, daycent_exe,
#'                                       actual_task_id, agg_vars, verbose = TRUE)
#'
#' # With shared database connection
#' shared_con <- get_shared_connection(config)
#' result <- run_site_simulations_generic(site_id, run_file_site, config,
#'                                       params_df, sim_dir_tid, daycent_exe,
#'                                       actual_task_id, agg_vars, shared_con,
#'                                       verbose = TRUE)
#' }
#'
#' @export
run_site_simulations_generic <- function(site_id, run_file_site, config, params_df,
                                        sim_dir_tid, daycent_exe, actual_task_id,
                                        agg_vars, shared_connection = NULL,
                                        common_template_dir = NULL,
                                        verbose = TRUE) {
  
  if (nrow(run_file_site) == 0) {
    stop("No run file rows for site: ", site_id,
         ". Check that RunFile.rds contains siteID '", site_id, "' and that paths are correct.")
  }
  
  # Initialize return values
  num_treatments <- 0
  num_sim_years <- 0
  num_obs_years <- 0
  annual_results <- NULL
  period_results <- NULL
  daily_results <- NULL
  aggregated_results <- NULL
  
  # Setup directories
  site_sim_dir <- file.path(sim_dir_tid, site_id)
  
  if (verbose) {
    cat("Scratch run path:", site_sim_dir, "\n")
  }
  
  # Setup DayCent run files
  if (dir.exists(site_sim_dir)) {
    unlink(site_sim_dir, recursive = TRUE)
  }
  dir.create(site_sim_dir, recursive = TRUE)

  # If a common template directory is provided, copy its contents into this site directory
  if (!is.null(common_template_dir)) {
    template_files <- list.files(common_template_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    if (length(template_files) > 0) {
      file.copy(from = template_files, to = site_sim_dir, recursive = TRUE, overwrite = TRUE)
    }
  }
  
  # Copy files based on configuration mode
  file_mode <- config$file_source$mode %||% "filesystem"
  
  if (file_mode == "database") {
    if (verbose) {
      cat("Using database file source mode\n")
    }
    
    # Get weather code and treatment info for this site
    site_run_info <- run_file_site[1, ]  # First row should have site info
    
    # Get weather_code based on file source mode
    if (config$file_source$mode == "database") {
      # Database mode - lookup weather_code directly from database
      weather_code <- get_weather_code_from_database(
        config = config,
        site_name = site_id,
        shared_connection = shared_connection,
        log_function = if(verbose) cat else function(...) NULL
      )
      
      # If database lookup fails, try RunFile as fallback
      if (is.null(weather_code) && "weather_code" %in% names(site_run_info)) {
        weather_code <- site_run_info$weather_code
      }
      
      # Final fallback for database mode
      if (is.null(weather_code)) {
        stop("Could not determine weather_code for site ", site_id, " in database mode")
      }
      
    } else {
      # Filesystem mode - get weather_code from RunFile
      if ("weather_code" %in% names(site_run_info)) {
        weather_code <- site_run_info$weather_code
      } else {
        stop("weather_code column missing from RunFile in filesystem mode")
      }
    }
    
    # Extract treatment name from treatment_schedule filename
    # Handle two cases:
    # 1. NULL treatment: "854585.sch" -> treatment_name should be NULL/empty
    # 2. Named treatment: "854585_TREATMENT.sch" -> treatment_name should be "TREATMENT"

    schedule_filename <- basename(site_run_info$treatment_schedule)
    schedule_base <- gsub("\\.sch$", "", schedule_filename)

    if (schedule_base == site_id) {
      # Case 1: filename is just "siteID.sch" -> NULL treatment
      treatment_name <- NULL
    } else if (startsWith(schedule_base, paste0(site_id, "_"))) {
      # Case 2: filename is "siteID_TREATMENT.sch" -> extract treatment
      treatment_name <- gsub(paste0("^", site_id, "_"), "", schedule_base)
    } else {
      # Fallback: use the whole filename without extension
      treatment_name <- schedule_base
    }
    
    # Copy files from database
    copy_status <- copy_files_from_database(
      site_name = site_id,
      treatment_name = treatment_name,
      weather_code = weather_code,
      target_dir = site_sim_dir,
      config = config,
      shared_connection = shared_connection,
      log_function = if(verbose) cat else function(...) NULL
    )
    
    if (copy_status != 0) {
      stop("Database files copy failed for ", site_id)
    } else {
      cat("---- Database files for ", site_id, " copied successfully. \n")
    }

  } else {
    # Filesystem mode (original behavior)
    site_dir_from <- file.path(config$paths$expsites_dir, site_id)
    
    if (verbose) {
      cat("Site source path:", site_dir_from, "\n")
      cat("Using filesystem file source mode\n")
    }
    
    # Copy site files (skip weather trimming in chained_schedule mode)
    copy_status <- copy_sitefiles(site_folder_from = site_dir_from, 
                                 site_folder_to = site_sim_dir,
                                 trim_weather = (config$daycent$mode != "chained_schedule"))
    if (copy_status != 0) {
      stop("Site files copy failed for ", site_id)
    } else {
      cat("---- Site files for ", site_id, " copied successfully. \n")
    }
    
  }
  
  # Set working directory
  old_wd <- getwd()
  on.exit({
  if (is.character(old_wd) && length(old_wd) == 1 && nzchar(old_wd)) {
    try(setwd(old_wd), silent = TRUE)
  }
}, add = TRUE)
  setwd(site_sim_dir)
  
  # Get treatment schedules from runFile (matching original logic)
  # For chained_schedule mode, RunFile may have schedule_columns (e.g. exp2_schedule) but no treatment_schedule 
  chained_mode <- (config$daycent$mode == "chained_schedule")
  if ("treatment_schedule" %in% names(run_file_site)) {
    trt_schs <- sort(unique(run_file_site$treatment_schedule))
    trt_schs <- trt_schs[!is.na(trimws(trt_schs))]
  } else {
    trt_schs <- character(0)
  }
  # If still no treatments, derive from last schedule column when RunFile has schedule_columns (e.g. from observation CSV)
  if (length(trt_schs) == 0 && !is.null(config$daycent$schedule_columns) && length(config$daycent$schedule_columns) > 0) {
    last_col <- config$daycent$schedule_columns[length(config$daycent$schedule_columns)]
    if (last_col %in% names(run_file_site)) {
      trt_schs <- trimws(as.character(run_file_site[[last_col]]))
      trt_schs <- trt_schs[!is.na(trt_schs) & nzchar(trt_schs)]
      if (length(trt_schs) > 0) {
        chained_mode <- TRUE  # RunFile has schedule columns, so run chained schedules
      }
    }
  }
  num_treatments <- length(trt_schs)
  
  if (num_treatments == 0) {
    stop("No treatments found for site: ", site_id,
         ". RunFile must have 'treatment_schedule' or, for chained_schedule mode, schedule column(s) ",
         " (e.g. ", paste(config$daycent$schedule_columns %||% "schedule_columns", collapse = ", "), "). ",
         "Check RunFile.rds and daycent schedule_columns in config.")
  }
  
  if (verbose) cat("Number of treatments:", num_treatments, "\n")
  
  # Run DayCent
  site100_params <- params_df[params_df$File == "site.100", ]
  # chained_mode already set above when computing trt_schs
  
  if (chained_mode) {
    # Chained schedule mode: site spinup (equil/base/...) once, then unique treatments.
    # Legacy full_chain_per_row reruns the entire chain for every RunFile row.
    treatment_row <- run_file_site[1, ]
    bh_sch_file <- if ("base_schedule" %in% names(treatment_row)) {
      val <- trimws(as.character(treatment_row$base_schedule[1]))
      if (is.na(val)) "" else val
    } else {
      ""
    }

    possible_site_files <- c(
      if (nzchar(bh_sch_file)) paste0(strip_dot_sch(sch_file_name = bh_sch_file), "_site.100") else character(0),
      paste0(site_id, ".100"),
      paste0(site_id, "_exp_day.100"),
      paste0(site_id, "_day.100"),
      paste0(sub("_.*$", "", site_id), ".100")
    )
    bh_site100 <- NULL
    for (candidate_file in possible_site_files) {
      if (file.exists(candidate_file)) {
        bh_site100 <- candidate_file
        break
      }
    }
    if (is.null(bh_site100)) {
      available_site_files <- list.files(pattern = "\\.100$")
      if (length(available_site_files) > 0) {
        bh_site100 <- available_site_files[1]
        if (verbose) cat("Warning: Using fallback site file:", bh_site100, "\n")
      } else {
        stop("No site.100 file found for site: ", site_id, " in directory: ", getwd())
      }
    }

    if (nrow(site100_params) > 0) {
      site_status <- update_site100_parameters(site100_file = bh_site100, paramsdf = site100_params)
      if (site_status != 0) {
        stop("ext_site.100 update failed for ", site_id, " with file ", bh_site100)
      }
      if (verbose) cat("---- ext_site.100  Files for ", site_id, " updated successfully.\n")
    }

    chained_run_mode <- config$daycent$chained_run_mode %||% "site_spinup"
    schedule_columns <- config$daycent$schedule_columns

    if (chained_run_mode == "site_spinup") {
      if (verbose) {
        cat("Running chained DayCent schedules in site_spinup mode for", site_id, "\n")
        cat("  RunFile rows:", nrow(run_file_site), "| unique treatments:", num_treatments, "\n")
        cat("  Schedule columns:", paste(schedule_columns, collapse = " -> "), "\n")
      }

      daycent_status <- run_chained_site_spinup_daycent(
        filepath_exe = daycent_exe,
        run_file_site = run_file_site,
        schedule_columns = schedule_columns,
        initial_ext_file = bh_site100,
        ext_suffix = "_ext",
        verbose = verbose
      )
    } else if (chained_run_mode == "full_chain_per_row") {
      site_dir_col <- if ("path" %in% names(run_file_site) &&
                          any(!is.na(run_file_site$path) & trimws(run_file_site$path) != "")) {
        "path"
      } else {
        NULL
      }

      if (verbose) {
        cat("Running chained DayCent schedules in full_chain_per_row mode for", nrow(run_file_site), "row(s)\n")
        cat("Schedule columns:", paste(schedule_columns, collapse = " -> "), "\n")
        cat("Keeping only last-schedule outputs (keep_intermediate=FALSE)\n")
      }

      daycent_status <- run_chained_daycent_schedules(
        filepath_exe = daycent_exe,
        obs_table = run_file_site,
        schedule_columns = schedule_columns,
        site_dir_column = site_dir_col,
        base_dir = if (!is.null(site_dir_col)) config$paths$lairice_root else NULL,
        ext_suffix = "_ext",
        initial_ext_file = bh_site100,
        verbose = verbose,
        keep_intermediate = FALSE
      )
    } else {
      stop("Invalid daycent$chained_run_mode: ", chained_run_mode,
           ". Must be 'site_spinup' or 'full_chain_per_row'.")
    }

    if (daycent_status != 0) {
      stop("DayCent chained schedule simulation failed for site: ", site_id)
    }
    cat("---- DayCent chained schedule execution successful for", site_id, "\n")
  }
  
  # Loop over treatments: for non-chained mode run DayCent each time;
  # for chained mode only post-process (DDList100, outputs)
  for (j in 1:length(trt_schs)) {
    num_treatments_processed <- j
    trt_sch_file <- trimws(trt_schs[j])
    
    if (verbose) cat("Processing treatment:", trt_sch_file, "\n")
    
    if (!chained_mode) {
      treatment_row <- run_file_site[run_file_site$treatment_schedule == trt_sch_file, ]
      if (nrow(treatment_row) == 0) {
        stop("No base_schedule found for treatment: ", trt_sch_file)
      }
      bh_sch_file <- trimws(treatment_row$base_schedule[1])
      
      possible_site_files <- c(
        paste0(strip_dot_sch(sch_file_name = bh_sch_file), "_site.100"),
        paste0(site_id, ".100"),
        paste0(site_id, "_exp_day.100"),
        paste0(site_id, "_day.100"),
        paste0(sub("_.*$", "", site_id), ".100")
      )
      bh_site100 <- NULL
      for (candidate_file in possible_site_files) {
        if (file.exists(candidate_file)) {
          bh_site100 <- candidate_file
          break
        }
      }
      if (is.null(bh_site100)) {
        available_site_files <- list.files(pattern = "\\.100$")
        if (length(available_site_files) > 0) {
          bh_site100 <- available_site_files[1]
          if (verbose) cat("Warning: Using fallback site file:", bh_site100, "\n")
        } else {
          stop("No site.100 file found for site: ", site_id, " in directory: ", getwd())
        }
      }
      
      if (verbose) cat("Base schedule:", bh_sch_file, "-> Site file:", bh_site100, "\n")
      
      if (nrow(site100_params) > 0) {
        site_status <- update_site100_parameters(site100_file = bh_site100, paramsdf = site100_params)
        if (site_status != 0) {
          stop("ext_site.100 update failed for ", site_id, " with file ", bh_site100)
        }
        if (verbose) cat("---- ext_site.100  Files for ", site_id, " updated successfully.\n")
      }
      
      daycent_status <- run_DayCent(filepath_exe = daycent_exe,
                                    sch_file = trt_sch_file,
                                    ext_site100_2read = bh_site100,
                                    ext_site100_2write = NULL)
      
      if (daycent_status != 0) {
        stop("DayCent simulation failed for ", site_id, "::", trt_sch_file)
      }
      cat("---- DayCent execution successful for", trt_sch_file, "\n")
    }
    
    # Run DDList100 if configured (for .lis file generation)
    if (!is.null(config$daycent$list100)) {
      # Extract treatment name from schedule file for .bin/.lis file naming
      trt_name <- sub("\\.sch$", "", trt_sch_file)
      bin_file <- paste0(trt_name, ".bin")
      lis_file <- paste0(trt_name, ".lis")
      
      # Check if .bin file exists before running DDList100
      if (file.exists(bin_file)) {
        # Construct DDList100 executable path
        ddlist_exe_path <- file.path(config$paths$lairice_root, config$daycent$list100)
        
        # Use outvars.txt from dot100_path or current directory
        outvars_file <- "outvars.txt"
        if (!file.exists(outvars_file)) {
          outvars_from_config <- file.path(config$paths$dot100_path, "outvars.txt")
          if (file.exists(outvars_from_config)) {
            file.copy(from = outvars_from_config, to = outvars_file, overwrite = TRUE)
            if (verbose) cat("  Copied outvars.txt from config\n")
          } else {
            if (verbose) cat("  Warning: outvars.txt not found, DDList100 may fail\n")
          }
        }
        
        if (verbose) cat("  Running DDList100 for", bin_file, "->", lis_file, "\n")
        
        # Execute DDList100
        ddlist_result <- run_ddlist100(
          ddlist_exe = ddlist_exe_path,
          bin_file = bin_file,
          lis_file = lis_file,
          outvars_file = outvars_file,
          log_function = function(msg) {
            if (verbose) cat("    DDList100:", msg, "\n")
          }
        )
        
        if (ddlist_result$success) {
          if (verbose) cat("  DDList100 SUCCESS:", lis_file, "created in", 
                          sprintf("%.1fs", ddlist_result$execution_time), "\n")
        } else {
          if (verbose) cat("  DDList100 FAILED:", ddlist_result$error_message, "\n")
          # Continue processing even if DDList100 fails (don't stop pipeline)
        }
      } else {
        if (verbose) cat("  Binary file", bin_file, "not found, skipping DDList100\n")
      }
    }
    
    # Process model outputs using generic function
    # Get aggregation metadata for this site from run_file_site
    agg_metadata <- NULL
    if ("aggregation_level" %in% names(run_file_site) && "aggregation_weight" %in% names(run_file_site)) {
      agg_metadata <- data.frame(
        site_name = site_id,
        aggregation_level = run_file_site$aggregation_level[1],
        aggregation_weight = run_file_site$aggregation_weight[1]
      )
    }
    
    treatment_results <- process_model_outputs_generic2(site_id, trt_sch_file, actual_task_id, 
                                                      config, agg_vars, agg_metadata, verbose = verbose)
    
    # Update results
    num_sim_years <- num_sim_years + treatment_results$num_sim_years
    num_obs_years <- num_obs_years + treatment_results$num_obs_years
    annual_results <- rbind(annual_results, treatment_results$annual_results)
    period_results <- rbind(period_results, treatment_results$period_results)
    daily_results <- rbind(daily_results, treatment_results$daily_results)
    
    # Accumulate aggregated results instead of overwriting (fixing critical bug)
    if (is.null(aggregated_results)) {
      # First treatment - initialize with this treatment's results
      aggregated_results <- treatment_results$aggregated_results
    } else {
      # Subsequent treatments - accumulate values for each variable
      for (var_name in names(treatment_results$aggregated_results)) {
        if (var_name %in% names(aggregated_results)) {
          aggregated_results[[var_name]] <- aggregated_results[[var_name]] + treatment_results$aggregated_results[[var_name]]
        } else {
          aggregated_results[[var_name]] <- treatment_results$aggregated_results[[var_name]]
        }
      }
    }
  }
  
  return(list(
    num_treatments = num_treatments,
    num_sim_years = num_sim_years,
    num_obs_years = num_obs_years,
    annual_results = annual_results,
    period_results = period_results,
    daily_results = daily_results,
    aggregated_results = aggregated_results
  ))
}

#' GSA Step 2 Simulate Individual - Generic Version
#'
#' Generic replacement for the hardcoded gsa_step2_simulate_individual() function.
#' This function provides the same interface but uses specification-driven
#' processing for model outputs instead of hardcoded logic.
#'
#' @param config A complete YAML configuration object loaded with read_yaml_config()
#' @param gsa_method The GSA method name (e.g., "soboljansen")
#' @param sim_id The simulation ID for this instance
#' @param daycent_exe Path to DayCent executable file
#' @param scratch_dir Scratch directory for temporary simulation files
#' @param clean_scratch Logical indicating whether to clean scratch files after simulation
#' @param start_id Starting ID offset for simulation numbering (default: 0)
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @return List containing simulation results
#'
#' @details
#' This function can be used as a drop-in replacement for the original
#' gsa_step2_simulate_individual() function to enable comparison testing between
#' the hardcoded and generic approaches.
#'
#' @examples
#' \dontrun{
#' result <- gsa_step2_simulate_individual_generic(config, "soboljansen", 1,
#'                                               daycent_exe, scratch_dir)
#'
#' # With shared database connection
#' shared_con <- get_shared_connection(config)
#' result <- gsa_step2_simulate_individual_generic(config, "soboljansen", 1,
#'                                               daycent_exe, scratch_dir,
#'                                               shared_connection = shared_con)
#' }
#'
#' @export
gsa_step2_simulate_individual_generic <- function(config, gsa_method, sim_id, daycent_exe,
                                                 scratch_dir, clean_scratch = TRUE,
                                                 start_id = 0, partition_id_override = NULL,
                                                 shared_connection = NULL, verbose = TRUE) {
  
  # Initialize timing and status tracking
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE
  
  # Detect execution mode
  scaling_mode <- is_scaling_mode_enabled(config)
  
  # Determine partition_id and actual_sim_id based on mode and arguments
  # Key change: sim_id is ALWAYS the SampleID (matches legacy behavior)
  actual_sim_id <- start_id + sim_id
  
  if (scaling_mode && !is.null(partition_id_override)) {
    # Scaling mode WITH partition specified: filter to partition subset
    partition_id <- partition_id_override
    
  } else if (scaling_mode && is.null(partition_id_override)) {
    # Scaling mode WITHOUT partition: run ALL sites (legacy behavior for this sample)
    partition_id <- NULL
    
  } else {
    # Legacy mode: no partition filtering
    partition_id <- NULL
  }
  
  # Initialize output variables
  output_result <- list(
    status = 1,  # Default to error
    daily_results = NULL,
    aggregated_results = NULL,
    runtime_info = NULL,
    sim_id = actual_sim_id,
    partition_id = partition_id,
    scaling_mode = scaling_mode,
    output_files = list()
  )
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 2 - Individual Simulation (Generic Version)\n")
    cat("Node:", node, "\n")
    cat("Working Directory:", getwd(), "\n\n")
    cat("Configuration:\n")
    cat("\t Project Name                :", config$project$name, "\n")
    cat("\t GSA Method                  :", gsa_method, "\n")
    cat("\t Execution Mode              :", if(scaling_mode) "SCALING" else "LEGACY", "\n")
    cat("\t Simulation ID (SampleID)    :", actual_sim_id, "\n")
    if (!is.null(partition_id)) {
      cat("\t Partition ID (filtering)    :", partition_id, "\n")
    } else if (scaling_mode) {
      cat("\t Partition ID                : ALL (no filtering)\n")
    }
    cat("\t DayCent Executable          :", daycent_exe, "\n")
    cat("\t Scratch Directory           :", scratch_dir, "\n")
    cat("\t Clean Scratch               :", clean_scratch, "\n\n")
    cat("====================================================================\n")
  }
  
  # Main simulation logic in tryCatch (matching original script structure)
  tryCatch({
    
    # Determine date stamp
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
    
    # Setup paths
    gsa_output_path <- file.path(config$paths$lairice_root, "results", config$project$name, 
                                date_stamp, "GSA", gsa_method)
    sim_dir_tid <- file.path(scratch_dir, paste0("SIM_", actual_sim_id))
    
    # Create scratch directories
    dir.create(sim_dir_tid, recursive = TRUE, showWarnings = FALSE)
    
    # Load Monte Carlo parameter draws (matching original logic)
    mc_draws_file <- file.path(gsa_output_path, paste0("mc_GSA_draw_", gsa_method, ".rds"))
    if (!file.exists(mc_draws_file)) {
      stop("Monte Carlo draws file not found: ", mc_draws_file)
    }
    
    all_jobs <- readRDS(mc_draws_file)
    
    # Get parameter set for this simulation (matching original logic)
    if (!(actual_sim_id %in% all_jobs$SampleID)) {
      stop("Simulation ID ", actual_sim_id, " not found in MC draws")
    }
    
    # Extract job parameters including JobGroup (matching original)
    job_params <- all_jobs[all_jobs$SampleID == actual_sim_id, ]
    group_id <- job_params$JobGroup
    
    params_df <- prepare_parameter_set(config, job_params, verbose = verbose)
    
    # Initialize aggregated variables using generic function
    output_specs <- load_output_specifications(config, verbose = FALSE)
    agg_vars <- initialize_aggregated_variables_generic(output_specs, verbose = FALSE)
    
    # Initialize results (generic counters)
    dRslt <- NULL
    annual_results <- NULL
    period_results <- NULL
    num_sim_years <- 0
    num_obs_years <- 0
    aggregated_results <- NULL
    
    # Load RunFile.rds (matching original logic)
    run_file_name <- file.path(gsa_output_path, "RunFile.rds")
    if (!file.exists(run_file_name)) {
      stop("RunFile.rds not found: ", run_file_name)
    }
    
    runFile <- readRDS(run_file_name)
    exp_SiteIDs <- sort(unique(runFile$siteID))

    # Filter sites if scaling mode is enabled AND partition_id is specified
    if (scaling_mode && !is.null(partition_id)) {
      # Load point assignments
      point_assignments_file <- file.path(gsa_output_path, "point_assignments.rds")

      if (file.exists(point_assignments_file)) {
        if (verbose) cat("Scaling mode with partition - loading point assignments...\n")

        # Get sites assigned to this partition
        assigned_sites <- get_assigned_sites_for_job(
          point_assignments_file = point_assignments_file,
          current_job_id = partition_id,
          verbose = verbose
        )

        # Filter exp_SiteIDs to only assigned sites
        exp_SiteIDs <- exp_SiteIDs[exp_SiteIDs %in% assigned_sites]

        if (verbose) {
          cat("Filtered to", length(exp_SiteIDs), "assigned sites for partition", partition_id, "\n")
        }
      } else {
        if (verbose) cat("Warning: Scaling mode enabled but point_assignments.rds not found\n")
      }
    } else if (scaling_mode && is.null(partition_id)) {
      if (verbose) cat("Scaling mode without partition - processing ALL", length(exp_SiteIDs), "sites\n")
    }

    # Observation data is now handled generically within each variable processing function

    if (verbose) cat("Processing", length(exp_SiteIDs), "sites with RunFile structure\n")

    # Build common template directory for shared DayCent input files
    # (dot100 files, updated *.100 files, outfiles.in) once per simulation.
    # Each site (filesystem or database mode) will receive a copy of this
    # template in its own site_sim_dir.
    common_template_dir <- file.path(sim_dir_tid, "template_common")
    if (dir.exists(common_template_dir)) {
      unlink(common_template_dir, recursive = TRUE)
    }
    dir.create(common_template_dir, recursive = TRUE, showWarnings = FALSE)

    template_old_wd <- getwd()
    on.exit({
      if (is.character(template_old_wd) && length(template_old_wd) == 1 && nzchar(template_old_wd)) {
        try(setwd(template_old_wd), silent = TRUE)
      }
    }, add = TRUE)
    setwd(common_template_dir)

    # Copy dot100 files once into the template directory
    dot100_status <- copy_dot100_files(
      dot100_directory = config$paths$dot100_path,
      simulation_directory = common_template_dir
    )
    if (dot100_status != 0) {
      stop("dot100 files copy failed when building common template directory")
    }

    # Update fix.100 parameters once per simulation
    fix100_params <- params_df[params_df$File == "fix.100", ]
    if (nrow(fix100_params) > 0) {
      fix_status <- update_fix100_parameters(paramsdf = fix100_params, save_copy = FALSE)
      if (fix_status != 0) {
        stop("fix.100 update failed when building common template directory")
      }
    }

    # Update fert.100 parameters once per simulation
    fert100_params <- params_df[params_df$File == "fert.100", ]
    if (nrow(fert100_params) > 0) {
      fert_status <- update_fert100_parameters(fert100_params, save_copy = FALSE)
      if (fert_status != 0) {
        stop("fert.100 update failed when building common template directory")
      }
    }

    # Update cult.100 parameters once per simulation
    cult100_params <- params_df[params_df$File == "cult.100", ]
    if (nrow(cult100_params) > 0) {
      k_clteff_params <- cult100_params[cult100_params$Parameter == "K_CLTEFF", ]
      if (nrow(k_clteff_params) > 0) {
        k_clteff_value <- k_clteff_params$value[1]
        cult_status <- update_cult100_parameters(k_clteff_value, save_copy = FALSE)
        if (cult_status != 0) {
          stop("cult.100 update failed when building common template directory")
        }
      }
    }

    # Update crop.100 parameters once per simulation (possibly multiple crops)
    crop100_params <- params_df[params_df$File == "crop.100", ]
    if (nrow(crop100_params) > 0) {
      crop_names <- get_pipeline_target_crop(config, "gsa")
      if (length(crop_names) > 0) {
        for (crop_n in crop_names) {
          crop_status <- update_crop100_parameters(crop100_params, crop_n, save_copy = FALSE)
          if (crop_status != 0) {
            stop("crop.100 update failed for crop ", crop_n, " when building common template directory")
          }
        }
      } else {
        stop("crop.100 parameters found but crop name not specified in configuration. ",
             "Please add 'gsa: crop: name: [CROP_NAME]' (or legacy 'daycent: crop: name') ",
             "to your configuration file.")
      }
    }

    # Copy outfiles.in once into the template directory
    outfiles_template <- config$model_outputs$outfiles_template %||% "nh3_outfiles.in"
    outfiles_from <- file.path(config$paths$dot100_path, outfiles_template)
    outfiles_to <- file.path(common_template_dir, "outfiles.in")
    if (file.exists(outfiles_from)) {
      file.copy(from = outfiles_from, to = outfiles_to, overwrite = TRUE)
      if (verbose) cat("Template outfiles.in copied successfully from ", outfiles_template, "\n")
    } else {
      fallback_outfiles <- file.path(config$paths$dot100_path, "no_outfiles.in")
      if (file.exists(fallback_outfiles)) {
        file.copy(from = fallback_outfiles, to = outfiles_to, overwrite = TRUE)
        warning("outfiles template not found (", outfiles_from, "); using no_outfiles.in")
      } else {
        warning("outfiles.in source file not found for template: ", outfiles_from)
      }
    }

    # Restore working directory for remainder of function
    setwd(template_old_wd)

    # Check if weighted mean aggregation is enabled (backward compatible)
    weighted_aggregation_enabled <- FALSE

    # Only enable weighted aggregation if:
    # 1. Output specifications contain weighted_mean_aggregation=TRUE
    # 2. RunFile contains aggregation_level and aggregation_weight columns
    if ("variables" %in% names(output_specs)) {
      weighted_vars <- output_specs$variables[!is.na(output_specs$variables$weighted_mean_aggregation) &
                                              output_specs$variables$weighted_mean_aggregation == TRUE, ]
      if (nrow(weighted_vars) > 0 &&
          "aggregation_level" %in% names(runFile) &&
          "aggregation_weight" %in% names(runFile)) {
        weighted_aggregation_enabled <- TRUE
      }
    }

    if (weighted_aggregation_enabled && verbose) {
      cat("Weighted mean aggregation enabled - processing sites by aggregation level\n")
    }

    if (weighted_aggregation_enabled) {
      # Aggregation level-based processing for weighted aggregation

      # Group sites by aggregation_level
      if (!("aggregation_level" %in% names(runFile))) {
        stop("RunFile missing aggregation_level column required for weighted aggregation")
      }

      # Filter runFile to only include sites assigned to this partition (if applicable)
      runFile_filtered <- runFile[runFile$siteID %in% exp_SiteIDs, ]

      aggregation_groups <- split(runFile_filtered, runFile_filtered$aggregation_level)
      aggregation_level_names <- names(aggregation_groups)

      if (verbose) {
        cat("Processing", length(aggregation_level_names), "aggregation levels:", paste(aggregation_level_names, collapse = ", "), "\n")
      }

      # Process each aggregation level separately
      for (level_id in aggregation_level_names) {
        level_sites <- aggregation_groups[[level_id]]
        level_site_ids <- level_sites$siteID

        if (verbose) {
          cat("Processing aggregation level", level_id, "with", length(level_site_ids), "sites\n")
        }

        # Store annual results for this aggregation level (for potential cleanup)
        level_annual_results <- NULL

        # Process all sites within this aggregation level
        for (site_id in level_site_ids) {
          site_id <- trimws(site_id)
          run_file_site <- runFile[runFile$siteID == site_id, ]

          if (verbose) cat("Processing site:", site_id, "in aggregation level:", level_id, "\n")

          # Run site simulations using generic function
          site_result <- run_site_simulations_generic(
            site_id = site_id,
            run_file_site = run_file_site,
            config = config,
            params_df = params_df,
            sim_dir_tid = sim_dir_tid,
            daycent_exe = daycent_exe,
            actual_task_id = actual_sim_id,
            agg_vars = agg_vars,
            shared_connection = shared_connection,
            common_template_dir = common_template_dir,
            verbose = verbose
          )

          # Update results
          num_sim_years <- num_sim_years + site_result$num_sim_years
          num_obs_years <- num_obs_years + site_result$num_obs_years
          annual_results <- rbind(annual_results, site_result$annual_results)
          period_results <- rbind(period_results, site_result$period_results)
          level_annual_results <- rbind(level_annual_results, site_result$annual_results)
          dRslt <- rbind(dRslt, site_result$daily_results)

          # Accumulate aggregated results across sites
          if (is.null(aggregated_results)) {
            aggregated_results <- site_result$aggregated_results
          } else {
            for (var_name in names(site_result$aggregated_results)) {
              if (var_name %in% names(aggregated_results)) {
                aggregated_results[[var_name]] <- aggregated_results[[var_name]] + site_result$aggregated_results[[var_name]]
              } else {
                aggregated_results[[var_name]] <- site_result$aggregated_results[[var_name]]
              }
            }
          }
        }

        # After all sites in this aggregation level are processed
        if (verbose) {
          cat("All sites in aggregation level", level_id, "completed.\n")
        }

        # Determine cleanup policy based on output specifications (backward compatible)
        tryCatch({
          if (exists("determine_cleanup_policy")) {
            cleanup_policy <- determine_cleanup_policy(output_specs$variables)

            # Cleanup annual results if policy requires it
            if (!is.null(cleanup_policy$delete_annual_after_aggregation) &&
                cleanup_policy$delete_annual_after_aggregation && verbose) {
              cat("Cleanup policy: deleting annual results for aggregation level", level_id, "to save disk space\n")
              # Note: Annual results cleanup would typically happen at the file level
              # For this simulation run, we keep them in memory but note the policy
            }
          } else {
            # Default: check if annual_output is FALSE for any weighted variables
            weighted_vars <- output_specs$variables[!is.na(output_specs$variables$weighted_mean_aggregation) &
                                                    output_specs$variables$weighted_mean_aggregation == TRUE, ]
            if (nrow(weighted_vars) > 0 && any(weighted_vars$annual_output == FALSE) && verbose) {
              cat("Note: Some weighted variables have annual_output=FALSE - cleanup would apply in production\n")
            }
          }
        }, error = function(e) {
          if (verbose) {
            cat("Warning: Cleanup policy determination failed:", e$message, "\n")
          }
        })
      }

      # Perform weighted aggregation after all aggregation levels are processed
      # This ensures all counties are included in the weighted mean output
      # ONLY in legacy mode - scaling mode defers aggregation to batch script
      if (!scaling_mode && crop_calibration_enabled(config, get_pipeline_target_crop(config, "gsa"))) {
        if (verbose) {
          cat("All aggregation levels completed. Performing weighted aggregation for all counties.\n")
        }

        tryCatch({
          if (exists("process_weighted_mean_aggregation_for_sample")) {
            # Create weighted output directory
            weighted_output_dir <- file.path(gsa_output_path, "Weighted_Mean_Outputs", paste0("jobGroup_", group_id))
            dir.create(weighted_output_dir, recursive = TRUE, showWarnings = FALSE)

            weighted_results <- process_weighted_mean_aggregation_for_sample(
              config = config,
              target_crop = get_pipeline_target_crop(config, "gsa"),
              annual_results = annual_results,  # Use complete annual_results with ALL counties
              sample_id = actual_sim_id,
              job_group = group_id,
              weighted_output_dir = weighted_output_dir,
              output_specs = output_specs$variables,
              method = "GSA",
              verbose = verbose
            )

            if (verbose) {
              cat("Weighted aggregation completed for sample", actual_sim_id, "\n")
            }
          } else {
            if (verbose) {
              cat("Warning: process_weighted_mean_aggregation_for_sample function not available\n")
            }
          }
        }, error = function(e) {
          stop("Weighted aggregation failed for sample ", actual_sim_id, ": ", e$message)
        })
      } else if (scaling_mode && verbose) {
        cat("Scaling mode: Skipping inline aggregation (will aggregate in batch via gsa_step2_aggregate.R)\n")
      }

    } else {
      # Original linear processing for backward compatibility (SOC/NH3 workflows)

      if (verbose) cat("Using standard site-by-site processing\n")

      for (i in 1:length(exp_SiteIDs)) {
        site_id <- trimws(exp_SiteIDs[i])
        run_file_site <- runFile[runFile$siteID == site_id, ]

        if (verbose) cat("Processing site:", site_id, "\n")

        # Run site simulations using generic function
        site_result <- run_site_simulations_generic(
          site_id = site_id,
          run_file_site = run_file_site,
          config = config,
          params_df = params_df,
          sim_dir_tid = sim_dir_tid,
          daycent_exe = daycent_exe,
          actual_task_id = actual_sim_id,
          agg_vars = agg_vars,
          shared_connection = shared_connection,
          common_template_dir = common_template_dir,
          verbose = verbose
        )

        # Update results
        num_sim_years <- num_sim_years + site_result$num_sim_years
        num_obs_years <- num_obs_years + site_result$num_obs_years
        annual_results <- rbind(annual_results, site_result$annual_results)
        period_results <- rbind(period_results, site_result$period_results)
        dRslt <- rbind(dRslt, site_result$daily_results)

        # Accumulate aggregated results across sites instead of overwriting (fixing critical bug)
        if (is.null(aggregated_results)) {
          # First site - initialize with this site's results
          aggregated_results <- site_result$aggregated_results
        } else {
          # Subsequent sites - accumulate values for each variable
          for (var_name in names(site_result$aggregated_results)) {
            if (var_name %in% names(aggregated_results)) {
              aggregated_results[[var_name]] <- aggregated_results[[var_name]] + site_result$aggregated_results[[var_name]]
            } else {
              aggregated_results[[var_name]] <- site_result$aggregated_results[[var_name]]
            }
          }
        }
      }
    }

    # Create aggregated results using generic function - only if there are aggregated variables
    agg_vars_specs <- output_specs$variables[output_specs$variables$aggregate_output == TRUE, ]
    if (nrow(agg_vars_specs) > 0) {
      agg_Rslt <- create_aggregated_results_generic(aggregated_results, actual_sim_id, verbose = FALSE)
    } else {
      agg_Rslt <- NULL  # No aggregated variables to process
    }
    
    # Initialize annual_output_file so it is always defined
    annual_output_file <- NULL
    
    if (config$file_source$database$database_result$run_type == "file_system") {
      # Save results (matching original directory structure and file formats)
      daily_output_dir <- file.path(gsa_output_path, "Daily_Outputs", paste0("jobGroup_", group_id))
      agg_output_dir <- file.path(gsa_output_path, "Aggregated_Outputs", paste0("jobGroup_", group_id))
      annual_output_dir <- file.path(gsa_output_path, "Annual_Outputs", paste0("jobGroup_", group_id))
      period_output_dir <- file.path(gsa_output_path, "Period_Outputs", paste0("jobGroup_", group_id))
      run_status_dir <- file.path(gsa_output_path, "Run_Status", paste0("jobGroup_", group_id))
      
      dir.create(daily_output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(agg_output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(annual_output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(period_output_dir, recursive = TRUE, showWarnings = FALSE)
      dir.create(run_status_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Save daily results as RDS (matching original format)
      daily_output_file <- file.path(daily_output_dir, paste0("dc_dRslt_", actual_sim_id, ".rds"))
      if (!is.null(dRslt) && nrow(dRslt) > 0) {
        saveRDS(dRslt, daily_output_file)
      }
      
      # Save aggregated results as RDS (matching original format) - only if there are aggregated variables
      agg_output_file <- file.path(agg_output_dir, paste0("dc_aggRslt_", actual_sim_id, ".rds"))
      if (!is.null(agg_Rslt) && nrow(agg_Rslt) > 0) {
        saveRDS(agg_Rslt, agg_output_file)
      }
      
      # Save annual results as RDS (new format for annual outputs)
      # Add partition suffix in scaling mode to avoid file collisions
      if (scaling_mode && !is.null(partition_id)) {
        annual_output_file <- file.path(annual_output_dir, 
          paste0("dc_annualRslt_", actual_sim_id, "_part", partition_id, ".rds"))
      } else {
        annual_output_file <- file.path(annual_output_dir, 
          paste0("dc_annualRslt_", actual_sim_id, ".rds"))
      }
      
      if (!is.null(annual_results) && nrow(annual_results) > 0) {
        saveRDS(annual_results, annual_output_file)
      }

      if (scaling_mode && !is.null(partition_id)) {
        period_output_file <- file.path(
          period_output_dir,
          paste0("dc_periodRslt_", actual_sim_id, "_part", partition_id, ".rds")
        )
      } else {
        period_output_file <- file.path(
          period_output_dir,
          paste0("dc_periodRslt_", actual_sim_id, ".rds")
        )
      }
      if (!is.null(period_results) && nrow(period_results) > 0) {
        saveRDS(period_results, period_output_file)
      }


      # Create runtime info
      end_time <- Sys.time()
      time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
      
      runtime_info <- data.frame(
        SampleID = actual_sim_id,
        node = node,
        Start = start_time,
        End = end_time,
        Time_sec = time_stamp,
        num_trts = nrow(runFile),
        sim_years = num_sim_years,
        obs_years = num_obs_years,
        status = 0,
        message = "Execution Success..",
        stringsAsFactors = FALSE
      )
      
      # Save runtime info
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_sim_id, ".csv"))
      write.csv(runtime_info, runtime_file, row.names = FALSE)

    } else if(config$file_source$database$database_result$run_type == "database") {
       # Save in database (optional, if database_result is configured)
        db_cfg <- config$file_source$database$database_result
        job_group_label <- group_id
        simulation_id <- actual_sim_id

        # Create runtime info
        end_time <- Sys.time()
        time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
        
        runtime_info <- data.frame(
          SampleID = actual_sim_id,
          node = node,
          Start = start_time,
          End = end_time,
          Time_sec = time_stamp,
          num_trts = nrow(runFile),
          sim_years = num_sim_years,
          obs_years = num_obs_years,
          status = 0,
          message = "Execution Success..",
          stringsAsFactors = FALSE
        )

        tryCatch({
          if (!requireNamespace("DBI", quietly = TRUE)) {
            if (verbose) cat("Warning: DBI package not available; skipping database_result write\n")
            stop("DBI not available")
          }
          has_mariadb <- requireNamespace("RMariaDB", quietly = TRUE)
          has_mysql <- requireNamespace("RMySQL", quietly = TRUE)
          if (!has_mariadb && !has_mysql) {
            if (verbose) cat("Warning: RMariaDB/RMySQL not available; skipping database_result write\n")
            stop("No database driver")
          }

          host <- config$file_source$database$host
          database <- config$file_source$database$database
          cred_file <- config$file_source$database$cred_file %||% "~/.dblogin"

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
            DBI::dbConnect(
              RMariaDB::MariaDB(),
              host = host,
              dbname = database,
              username = username,
              password = password
            )
          } else {
            DBI::dbConnect(
              RMySQL::MySQL(),
              host = host,
              dbname = database,
              username = username,
              password = password
            )
          }
          on.exit(DBI::dbDisconnect(con), add = TRUE)

          # Write annual results (if any)
          # Add partition suffix in scaling mode to avoid file collisions
          if (scaling_mode && !is.null(partition_id)) {
            if (!is.null(annual_results) && nrow(annual_results) > 0 &&
                "tables" %in% names(db_cfg) && "gsa_results_annual" %in% names(config$file_source$database$database_result$tables)) {
              annual_table <- paste0("part", partition_id, "_", config$file_source$database$database_result$tables$gsa_results_annual)
              annual_to_write <- annual_results
              annual_to_write$job_group <- job_group_label
              annual_to_write$simulation_id <- simulation_id
              annual_to_write$partition_id <- partition_id
              DBI::dbWriteTable(con, annual_table, annual_to_write, append = TRUE, row.names = FALSE)
            }
          } else { 
            if (!is.null(annual_results) && nrow(annual_results) > 0 &&
                "tables" %in% names(db_cfg) && "gsa_results_annual" %in% names(config$file_source$database$database_result$tables)) {
              annual_table <- config$file_source$database$database_result$tables$gsa_results_annual
              annual_to_write <- annual_results
              annual_to_write$job_group <- job_group_label
              annual_to_write$simulation_id <- simulation_id
              DBI::dbWriteTable(con, annual_table, annual_to_write, append = TRUE, row.names = FALSE)
            }
          }
           

          # Write daily results (if any)
          if (!is.null(dRslt) && nrow(dRslt) > 0 &&
              "tables" %in% names(db_cfg) && "gsa_results_daily" %in% names(db_cfg$tables)) {
            daily_table <- db_cfg$tables$gsa_results_daily
            daily_to_write <- dRslt
            daily_to_write$job_group <- job_group_label
            daily_to_write$simulation_id <- simulation_id
            DBI::dbWriteTable(con, daily_table, daily_to_write, append = TRUE, row.names = FALSE)
          }

          # Write run status results (if any)
          if (!is.null(runtime_info) && nrow(runtime_info) > 0 &&
              "tables" %in% names(db_cfg) && "gsa_results_run_status" %in% names(db_cfg$tables)) {
            run_status_table <- db_cfg$tables$gsa_results_run_status
            run_status_to_write <- runtime_info
            run_status_to_write$job_group <- job_group_label
            run_status_to_write$simulation_id <- simulation_id
            DBI::dbWriteTable(con, run_status_table, run_status_to_write, append = TRUE, row.names = FALSE)
          }

          DBI::dbDisconnect(con)

        }, error = function(e) {
          if (verbose) {
            cat("Warning: Failed to write GSA Step 2 results to database_result: ",
                conditionMessage(e), "\n")
          }
        })
    
      
    }

    # Clean up scratch directory if requested
    if (clean_scratch && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
    
    complete <- TRUE
    
    # Update output result
    output_result$status <- 0
    output_result$daily_results <- dRslt
    output_result$aggregated_results <- agg_Rslt
    output_result$runtime_info <- runtime_info
    
    # Only report files that were actually created
    output_files_list <- list()
    
    # Daily output file (conditional)
    if (!is.null(dRslt) && nrow(dRslt) > 0) {
      output_files_list$daily_output <- daily_output_file
    }
    
    # Aggregated output file (conditional)
    if (!is.null(agg_Rslt) && nrow(agg_Rslt) > 0) {
      output_files_list$aggregated_output <- agg_output_file
    }
    
    # Annual output file (conditional - only for file_system run_type)
    if (config$file_source$database$database_result$run_type == "file_system" &&
        !is.null(annual_results) && nrow(annual_results) > 0 &&
        !is.null(annual_output_file)) {
      output_files_list$annual_output <- annual_output_file
    }
    
    # Runtime info file (always created)
    run_status_dir <- file.path(gsa_output_path, "Run_Status", paste0("jobGroup_", group_id))
    runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_sim_id, "_error.csv"))
    output_files_list$runtime_info <- runtime_file
    
    output_result$output_files <- output_files_list
    
    if (verbose) cat("--- GSA Step 2 Individual Simulation Successfully Completed (Generic) ---\n")
    
  }, error = function(err) {
    
    if (verbose) {
      cat("--- GSA Step 2 Individual Simulation Failed (Generic) ---\n")
      cat("Error:", as.character(err), "\n")
    }
    
    # Calculate error runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create error runtime info (matching original structure)
    runtime_info <- data.frame(
      SampleID = actual_sim_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = ifelse(exists("num_trts"), num_trts, 0),
      sim_years = ifelse(exists("num_sim_years"), num_sim_years, 0),
      obs_years = ifelse(exists("num_obs_years"), num_obs_years, 0),
      status = 1,
      message = as.character(err),
      stringsAsFactors = FALSE
    )
    
    # Try to save error info if directory exists
    if (exists("run_status_dir") && dir.exists(run_status_dir) && config$file_source$database$database_result$run_type == "file_system") {
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_sim_id, "_error.csv"))
      write.csv(runtime_info, runtime_file, row.names = FALSE)

      
      output_result$output_files$runtime_info <- runtime_file
    } else if(config$file_source$database$database_result$run_type == "database") {
       # Save in database (optional, if database_result is configured)
        db_cfg <- config$file_source$database$database_result
        job_group_label <- group_id
        simulation_id <- actual_sim_id

        tryCatch({
          if (!requireNamespace("DBI", quietly = TRUE)) {
            if (verbose) cat("Warning: DBI package not available; skipping database_result write\n")
            stop("DBI not available")
          }
          has_mariadb <- requireNamespace("RMariaDB", quietly = TRUE)
          has_mysql <- requireNamespace("RMySQL", quietly = TRUE)
          if (!has_mariadb && !has_mysql) {
            if (verbose) cat("Warning: RMariaDB/RMySQL not available; skipping database_result write\n")
            stop("No database driver")
          }

          host <- config$file_source$database$host
          database <- config$file_source$database$database
          cred_file <- config$file_source$database$cred_file %||% "~/.dblogin"

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
            DBI::dbConnect(
              RMariaDB::MariaDB(),
              host = host,
              dbname = database,
              username = username,
              password = password
            )
          } else {
            DBI::dbConnect(
              RMySQL::MySQL(),
              host = host,
              dbname = database,
              username = username,
              password = password
            )
          }
          on.exit(DBI::dbDisconnect(con), add = TRUE)

          # Write run status (always one row) if table configured
          if ("tables" %in% names(config$file_source$database$database_result) && "gsa_results_run_status" %in% names(config$file_source$database$database_result$tables)) {
            status_table <- config$file_source$database$database_result$tables$gsa_results_run_status
            status_to_write <- runtime_info
            status_to_write$job_group <- job_group_label
            status_to_write$simulation_id <- simulation_id
            DBI::dbWriteTable(con, status_table, status_to_write, append = TRUE, row.names = FALSE)
          }

          DBI::dbDisconnect(con)

        }, error = function(e) {
          if (verbose) {
            cat("Warning: Failed to write GSA Step 2 results to database_result: ",
                conditionMessage(e), "\n")
          }
        })
    
      
    }
    
    output_result$runtime_info <- runtime_info
    
    # Clean up on error if requested
    if (clean_scratch && exists("sim_dir_tid") && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
  })
  
  return(output_result)
}

#' GSA Step 2 Simulate - Generic Version
#'
#' Generic replacement for the hardcoded gsa_step2_simulate() function.
#' This function provides the same interface but uses specification-driven
#' processing for model outputs instead of hardcoded logic.
#'
#' @param config A complete YAML configuration object loaded with read_yaml_config()
#' @param task_id The SLURM task ID for this simulation instance
#' @param daycent_exe Path to DayCent executable file
#' @param scratch_dir Scratch directory for temporary simulation files
#' @param clean_scratch Logical indicating whether to clean scratch files after simulation
#' @param start_id Starting ID offset for task numbering (default: 0)
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @return List containing simulation results
#'
#' @details
#' This function can be used as a drop-in replacement for the original
#' gsa_step2_simulate() function to enable comparison testing between
#' the hardcoded and generic approaches.
#'
#' @examples
#' \dontrun{
#' result <- gsa_step2_simulate_generic(config, 1, daycent_exe, scratch_dir)
#' }
#'
#' @export
gsa_step2_simulate_generic <- function(config, task_id, daycent_exe, scratch_dir, 
                                      clean_scratch = TRUE, start_id = 0, 
                                      partition_id_override = NULL, verbose = TRUE) {
  
  # Initialize timing and status tracking
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE
  
  # Adjust task_id with start_id offset
  actual_task_id <- start_id + task_id
  
  # Initialize output variables
  output_result <- list(
    status = 1,  # Default to error
    daily_results = NULL,
    aggregated_results = NULL,
    runtime_info = NULL,
    task_id = actual_task_id,
    output_files = list()
  )
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 2 - Generic Model Simulation\n")
    cat("Node:", node, "\n")
    cat("Working Directory:", getwd(), "\n\n")
    cat("Configuration:\n")
    cat("\t Project Name                :", config$project$name, "\n")
    cat("\t DayCent Executable          :", daycent_exe, "\n")
    cat("\t Scratch Directory           :", scratch_dir, "\n")
    cat("\t Clean Scratch               :", clean_scratch, "\n")
    cat("\t Actual Task ID              :", actual_task_id, "\n\n")
    cat("====================================================================\n")
  }
  
  # Main simulation logic in tryCatch
  tryCatch({
    
    # Determine date stamp
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
    
    # Get GSA method for this task
    gsa_method <- config$gsa$gsa_methods[actual_task_id]
    
    if (verbose) cat("GSA Method:", gsa_method, "\n")
    
    # Use the individual simulation function
    result <- gsa_step2_simulate_individual_generic(
      config = config,
      gsa_method = gsa_method,
      sim_id = actual_task_id,
      daycent_exe = daycent_exe,
      scratch_dir = scratch_dir,
      clean_scratch = clean_scratch,
      start_id = 0,  # sim_id already adjusted
      partition_id_override = partition_id_override,  # Pass through for scaling mode
      verbose = verbose
    )
    
    # Return the result from individual simulation
    return(result)
    
  }, error = function(err) {
    
    if (verbose) {
      cat("--- GSA Step 2 Generic Simulation Failed ---\n")
      cat("Error:", as.character(err), "\n")
    }
    
    # Calculate error runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create error runtime info
    runtime_info <- data.frame(
      SampleID = actual_task_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = 0,
      sim_years = 0,
      status = 1,
      message = as.character(err),
      stringsAsFactors = FALSE
    )
    
    output_result$runtime_info <- runtime_info
    
    return(output_result)
  })
}

# =============================================================================
# STREAMLINED BATCH PROCESSING FUNCTIONS (GENERIC2)
# =============================================================================

#' Process Model Outputs - Streamlined Batch Version
#'
#' A streamlined version of process_model_outputs_generic that uses batch processing
#' for better performance and easier debugging. This function reads all output files
#' once, then processes aggregated and daily outputs separately.
#'
#' @param site_id Site identifier
#' @param trt_sch_file Treatment schedule file name
#' @param actual_task_id Task ID for this simulation
#' @param config Configuration object
#' @param agg_vars Aggregated variables list (will be updated)
#' @param verbose Logical indicating whether to print progress messages
#' @return List with processed results and updated aggregated variables
#'
#' @details
#' This function implements a clean batch processing approach:
#' 1. Read all enabled output files once
#' 2. Add computed variables (like DayCent_N2O)
#' 3. Process all aggregated outputs in one pass
#' 4. Process all daily outputs in another pass
#' 
#' The function is completely driven by CSV specifications and produces identical
#' results to the original hardcoded implementation.
#'
#' @examples
#' \dontrun{
#' result <- process_model_outputs_generic2(site_id, trt_sch_file, actual_task_id,
#'                                          config, agg_vars, verbose = TRUE)
#' }
#'
#' @export
calculate_nh3_observation_years <- function(trt_sch_file, config, verbose = FALSE) {
  
  # Load cumulative NH3 data (matching original logic)
  nh3_file <- config$input_files$observation_data$cumulative_nh3
  
  if (!file.exists(nh3_file)) {
    if (verbose) cat("NH3 observation file not found:", nh3_file, "\n")
    return(0)
  }
  
  cumNH3 <- read.csv(nh3_file, stringsAsFactors = FALSE)
  
  # Get observation years for this treatment (matching original logic exactly)
  treatment_rows <- cumNH3$treatment_schedule == trt_sch_file
  if (sum(treatment_rows) == 0) {
    if (verbose) cat("No NH3 observations found for treatment:", trt_sch_file, "\n")
    return(0)
  }
  
  # Match original logic exactly - unique() with multiple arguments only uses first argument
  obs_years <- sort(unique(cumNH3$meas_start_year[treatment_rows],
                          cumNH3$meas_end_year[treatment_rows]))
  
  if (verbose) {
    cat("NH3 observation years for", trt_sch_file, ":", paste(obs_years, collapse = ", "), "\n")
  }
  
  return(length(obs_years))
}

#' Calculate Urea Observation Years for Treatment
#' @export
calculate_urea_observation_years <- function(trt_sch_file, config, verbose = FALSE) {
  
  # Load urea measurements data (matching original logic)
  urea_file <- config$input_files$observation_data$urea_measurements
  
  if (!file.exists(urea_file)) {
    if (verbose) cat("Urea observation file not found:", urea_file, "\n")
    return(0)
  }
  
  mes_urea <- read.csv(urea_file, stringsAsFactors = FALSE)
  
  # Get observation years for this treatment (matching original logic exactly)
  treatment_rows <- mes_urea$treatment_schedule == trt_sch_file
  if (sum(treatment_rows) == 0) {
    if (verbose) cat("No urea observations found for treatment:", trt_sch_file, "\n")
    return(0)
  }
  
  obs_years <- sort(unique(mes_urea$year[treatment_rows]))
  
  if (verbose) {
    cat("Urea observation years for", trt_sch_file, ":", paste(obs_years, collapse = ", "), "\n")
  }
  
  return(length(obs_years))
}

#' Process Model Outputs - Generic Version 2 (Streamlined)
#' @param agg_metadata Optional aggregation metadata for weighted mean processing
#' @export
process_model_outputs_generic2 <- function(site_id, trt_sch_file, actual_task_id, 
                                          config, agg_vars, agg_metadata = NULL, verbose = FALSE) {
  
  if (verbose) cat("=== Starting Streamlined Batch Processing ===\n")
  
  # 1. Load CSV specifications
  output_specs <- load_output_specifications(config, verbose = FALSE)
  
  # 2. Batch read all enabled output files (with template replacement)
  # Replace {treatment} template with actual treatment name
  treatment_name <- strip_dot_sch(sch_file_name = trt_sch_file)  # e.g., "broadbalk_BF.sch" -> "broadbalk_BF"
  enabled_files_resolved <- gsub("\\{treatment\\}", treatment_name, output_specs$enabled_files)
  
  if (verbose) {
    cat("Template replacement:\n")
    for (i in seq_along(output_specs$enabled_files)) {
      cat("  Original:", output_specs$enabled_files[i], "-> Resolved:", enabled_files_resolved[i], "\n")
    }
  }
  
  all_file_data <- read_all_output_files_batch(enabled_files_resolved, verbose = verbose)
  
  # 3. Add computed variables (like DayCent_N2O = nit_N2O.N + dnit_N2O.N)
  # Apply template replacement to variable specs as well
  output_specs_resolved <- output_specs
  output_specs_resolved$variables$output_file <- gsub("\\{treatment\\}", treatment_name, output_specs$variables$output_file)
  
  all_file_data <- add_computed_variables_batch(all_file_data, output_specs_resolved$variables, verbose = verbose)
  
  # 4. Split variables by processing type (using resolved specs)
  daily_vars <- output_specs_resolved$variables[output_specs_resolved$variables$daily_output == TRUE, ]
  annual_vars <- output_specs_resolved$variables[output_specs_resolved$variables$annual_output == TRUE, ]
  agg_vars_specs <- output_specs_resolved$variables[output_specs_resolved$variables$aggregate_output == TRUE, ]
  if ("period_output" %in% names(output_specs_resolved$variables)) {
    period_vars <- output_specs_resolved$variables[
      output_specs_resolved$variables$period_output %in% c(TRUE, "TRUE", "true", 1, "1"),
    ]
  } else {
    period_vars <- output_specs_resolved$variables[
      output_specs_resolved$variables$transform_function == "biweekly_sum",
    ]
  }
  
  if (verbose) {
    cat("Variables to process:\n")
    cat("  Daily outputs:", nrow(daily_vars), "\n")
    cat("  Annual outputs:", nrow(annual_vars), "\n")
    cat("  Period outputs:", nrow(period_vars), "\n")
    cat("  Aggregated outputs:", nrow(agg_vars_specs), "\n")
  }
  
  # 5. Process aggregated outputs (one focused function) - skip if none
  if (nrow(agg_vars_specs) > 0) {
    aggregated_results <- process_aggregated_outputs_batch(all_file_data, agg_vars_specs, agg_vars, verbose = verbose)
  } else {
    aggregated_results <- agg_vars  # Use initialized empty list
    if (verbose) cat("Skipping aggregated outputs (none specified)\n")
  }
  
  # 6. Process annual outputs (new focused function)  
  annual_result <- process_annual_outputs_batch(
    all_file_data = all_file_data,
    annual_vars = annual_vars,
    metadata = list(SampleID = actual_task_id, SiteID = site_id, TreatmentID = trt_sch_file),
    config = config,
    agg_metadata = agg_metadata,
    verbose = verbose
  )

  # 7. Process daily outputs (another focused function)  
  daily_result <- process_daily_outputs_batch(
    all_file_data = all_file_data,
    daily_vars = daily_vars,
    metadata = list(SampleID = actual_task_id, SiteID = site_id, TreatmentID = trt_sch_file),
    config = config,
    agg_metadata = agg_metadata,
    verbose = verbose
  )

  # 8. Process period outputs (e.g., 14-day methane totals)
  period_result <- process_period_outputs_batch(
    all_file_data = all_file_data,
    period_vars = period_vars,
    metadata = list(SampleID = actual_task_id, SiteID = site_id, TreatmentID = trt_sch_file),
    config = config,
    agg_metadata = agg_metadata,
    verbose = verbose
  )
  
  # 9. Calculate simulation years (generic approach using any available file with year data)
  num_sim_years <- 0
  for (file_name in names(all_file_data)) {
    if ("year" %in% names(all_file_data[[file_name]])) {
      num_sim_years <- length(unique(all_file_data[[file_name]]$year))
      if (verbose) cat("Simulation years calculated from:", file_name, "\n")
      break  # Use first available file with year data
    }
  }
  
  # Total observation years (avoid double counting - max of daily vs annual for same variables)
  total_obs_years <- max(
    daily_result$num_obs_years,
    annual_result$num_obs_years,
    period_result$num_obs_periods
  )
  
  if (verbose) {
    cat("=== Batch Processing Complete ===\n")
    cat("Annual result rows:", if(is.null(annual_result$annual_results)) 0 else nrow(annual_result$annual_results), "\n")
    cat("Period result rows:", if(is.null(period_result$period_results)) 0 else nrow(period_result$period_results), "\n")
    cat("Daily result rows:", if(is.null(daily_result$daily_results)) 0 else nrow(daily_result$daily_results), "\n")
    cat("Simulation years:", num_sim_years, "\n")
    cat("Observation years:", total_obs_years, "\n")
  }
  
  return(list(
    annual_results = annual_result$annual_results,
    period_results = period_result$period_results,
    daily_results = daily_result$daily_results,
    num_sim_years = num_sim_years,
    num_obs_years = total_obs_years,
    aggregated_results = aggregated_results
  ))
}

#' Read All Output Files in Batch
#'
#' Efficiently reads all enabled DayCent output files into memory for processing.
#' This eliminates repeated file I/O operations and improves performance.
#'
#' @param enabled_files Vector of enabled output file names
#' @param verbose Logical indicating whether to print progress messages
#' @return Named list of data frames, one for each successfully read file
#'
#' @details
#' For each file that exists:
#' - Reads the file using read.table with headers
#' - Adds a 'year' column calculated from the 'time' column
#' - Stores in a named list for easy access
#'
#' @export
read_all_output_files_batch <- function(enabled_files, verbose = FALSE) {
  all_data <- list()
  
  if (verbose) cat("Reading output files in batch:\n")
  
  for (file_name in enabled_files) {
    file_path <- file.path(getwd(), file_name)
    
    if (file.exists(file_path)) {
      if (verbose) cat("  Reading:", file_name, "\n")
      
      # Check file extension to determine reading method
      file_ext <- tolower(substr(file_name, nchar(file_name)-3, nchar(file_name)))
      
      if (file_ext == ".lis") {
        # Read .lis file using specialized function with time/year correction
        data <- read_lis_file(file_path, verbose = verbose)
      } else {
        # Read .out file using standard method
        data <- read.table(file_path, header = TRUE)
        if ("time" %in% names(data)) {
          data$year <- data$time %/% 1
        }
        if (!"day" %in% names(data) && "doy" %in% names(data)) {
          data$day <- data$doy
        }
      }
      
      all_data[[file_name]] <- data
    } else {
      if (verbose) cat("  File not found:", file_name, "\n")
    }
  }
  
  if (verbose) cat("Successfully read", length(all_data), "files\n")
  
  return(all_data)
}

#' Add Computed Variables to File Data
#'
#' Adds computed variables (like DayCent_N2O) to the appropriate output files
#' based on the variable specifications.
#'
#' @param all_file_data Named list of data frames from read_all_output_files_batch
#' @param variable_specs Data frame with variable specifications
#' @param verbose Logical indicating whether to print progress messages
#' @return Updated all_file_data with computed variables added
#'
#' @details
#' Currently handles:
#' - DayCent_N2O: computed as nit_N2O.N + dnit_N2O.N in nflux.out
#' 
#' Additional computed variables can be added by extending the logic here.
#'
#' @export
add_computed_variables_batch <- function(all_file_data, variable_specs, verbose = FALSE) {
  
  # Find computed variables that need special handling
  computed_vars <- variable_specs[
    variable_specs$transform_function %in% c("computed_n2o", "computed_ch4_emit"),
  ]
  
  if (nrow(computed_vars) > 0 && verbose) {
    cat("Adding computed variables:\n")
  }
  
  if (nrow(computed_vars) > 0) {
    for (i in 1:nrow(computed_vars)) {
      var_spec <- computed_vars[i, ]
      file_name <- var_spec$output_file
      var_name <- var_spec$variable_name
    
    # Check if we have the file and required source columns
    if (file_name %in% names(all_file_data)) {
      file_data <- all_file_data[[file_name]]
      
      if (var_spec$transform_function == "computed_n2o") {
        # Add DayCent_N2O = nit_N2O.N + dnit_N2O.N (matching original logic)
        if (all(c("nit_N2O.N", "dnit_N2O.N") %in% names(file_data))) {
          file_data$DayCent_N2O <- file_data$nit_N2O.N + file_data$dnit_N2O.N
          all_file_data[[file_name]] <- file_data
          
          if (verbose) cat("  Added", var_name, "to", file_name, "\n")
        } else {
          if (verbose) cat("  Cannot compute", var_name, "- missing source columns\n")
        }
      } else if (var_spec$transform_function == "computed_ch4_emit") {
        if (all(c("CH4_Ep", "CH4_Ebl") %in% names(file_data))) {
          file_data[[var_name]] <- file_data$CH4_Ep + file_data$CH4_Ebl
          all_file_data[[file_name]] <- file_data
          if (verbose) cat("  Added", var_name, "to", file_name, "\n")
        } else {
          if (verbose) cat("  Cannot compute", var_name, "- missing CH4_Ep/CH4_Ebl columns\n")
        }
      }
    }
    }
  }
  
  return(all_file_data)
}

#' Process Aggregated Outputs in Batch
#'
#' Processes all aggregated variables in a single pass, applying transform functions
#' and updating the aggregated variables list.
#'
#' @param all_file_data Named list of data frames from read_all_output_files_batch
#' @param agg_vars_specs Data frame with aggregated variable specifications
#' @param agg_vars Current aggregated variables list (will be updated)
#' @param verbose Logical indicating whether to print progress messages
#' @return Updated agg_vars list with new aggregated values
#'
#' @details
#' For each aggregated variable specification:
#' - Locates the variable in the appropriate output file
#' - Applies the specified transform function (abs_sum_div_1e6, computed_n2o, etc.)
#' - Updates the aggregated variables list
#' 
#' This matches the original aggregation logic exactly.
#'
#' @export
process_aggregated_outputs_batch <- function(all_file_data, agg_vars_specs, agg_vars, verbose = FALSE) {
  
  if (verbose) cat("Processing aggregated outputs:\n")
  
  if (nrow(agg_vars_specs) > 0) {
    for (i in 1:nrow(agg_vars_specs)) {
    var_spec <- agg_vars_specs[i, ]
    file_name <- var_spec$output_file
    var_name <- var_spec$variable_name
    agg_name <- var_spec$aggregate_name
    transform_func <- var_spec$transform_function
    
    # Skip if file data not available
    if (!file_name %in% names(all_file_data)) {
      if (verbose) cat("  Skipping", var_name, "- file", file_name, "not found\n")
      next
    }
    
    file_data <- all_file_data[[file_name]]
    
    # Skip if variable not in file
    if (!var_name %in% names(file_data)) {
      if (verbose) cat("  Skipping", var_name, "- variable not in", file_name, "\n")
      next
    }
    
    # Apply transform function (matching original logic)
    if (transform_func == "computed_n2o") {
      # Pass whole data frame for computed variables
      agg_value <- apply_transform_function(file_data, transform_func, verbose = FALSE)
    } else {
      # Pass specific column for regular variables
      agg_value <- apply_transform_function(file_data[[var_name]], transform_func, verbose = FALSE)
    }
    
    # Update aggregated variables (matching original accumulation)
    if (agg_name %in% names(agg_vars)) {
      agg_vars[[agg_name]] <- agg_vars[[agg_name]] + agg_value
    } else {
      agg_vars[[agg_name]] <- agg_value
    }
    
    if (verbose) cat("  Updated", agg_name, ":", agg_value, "(total:", agg_vars[[agg_name]], ")\n")
  }
  
  } # End if (nrow(agg_vars_specs) > 0)
  
  return(agg_vars)
}

#' Process Daily Outputs in Batch
#'
#' Processes all daily output variables in a single pass, creating daily output rows
#' that match the original format exactly.
#'
#' @param all_file_data Named list of data frames from read_all_output_files_batch
#' @param daily_vars Data frame with daily variable specifications
#' @param metadata List with SampleID, SiteID, TreatmentID for output rows
#' @param config Configuration object for observation data paths
#' @param agg_metadata Optional aggregation metadata for weighted mean processing
#' @param verbose Logical indicating whether to print progress messages
#' @return List with daily_results data frame and num_obs_years count
#'
#' @details
#' For each daily variable specification:
#' - Gets observation years for the variable/treatment combination
#' - Finds intersection of model years and observation years
#' - Creates daily output rows (366 days + 7 metadata columns)
#' - Format matches original exactly: SampleID, SiteID, TreatmentID, year, variable, Model, unit, d1...d366
#'
#' @export
process_daily_outputs_batch <- function(all_file_data, daily_vars, metadata, config, agg_metadata = NULL, verbose = FALSE) {
  
  daily_results <- NULL
  num_obs_years <- 0
  obs_data_dir <- file.path(config$paths$lairice_root, config$model_outputs$observation_data_dir)
  
  if (verbose) cat("Processing daily outputs:\n")
  
  if (nrow(daily_vars) > 0) {
    for (i in 1:nrow(daily_vars)) {
      var_spec <- daily_vars[i, ]
      file_name <- var_spec$output_file
      var_name <- var_spec$variable_name
      
      # Skip if file data not available
      if (!file_name %in% names(all_file_data)) {
        if (verbose) cat("  Skipping", var_name, "- file", file_name, "not found\n")
        next
      }
    
    file_data <- all_file_data[[file_name]]
    
    # Skip if variable not in file
    if (!var_name %in% names(file_data)) {
      if (verbose) cat("  Skipping", var_name, "- variable not in", file_name, "\n")
      next
    }
    
    # Get observation years for this variable using adaptive logic
    site_metadata <- list(site_name = metadata$SiteID, treatment_id = metadata$TreatmentID)
    obs_years <- get_observation_years_adaptive(var_spec, site_metadata, 
                                               aggregation_metadata = agg_metadata, 
                                               obs_data_dir, verbose = FALSE)
    num_obs_years <- num_obs_years + length(obs_years)
    
    # Get model years
    mod_years <- unique(file_data$year)
    
    # Find intersection of model and observation years (matching original logic)
    data_years <- intersect(mod_years, obs_years)
    
    # Skip if no matching years found (matching original logic exactly)
    if (length(data_years) < 1) {
      if (verbose) cat("  Skipping", var_name, "- no matching observation years\n")
      next
    }
    
    # Create daily output rows for each year (matching original format exactly)
    if (length(data_years) > 0) {
      if (verbose) cat("  Creating daily output for", var_name, ":", length(data_years), "years\n")
      
      for (yr in data_years) {
        year_data <- file_data[file_data$year == yr, ]
        
        # Create daily values (exactly like original)
        daily_values <- as.numeric(year_data[, var_name])
        # Pad with NA to make it 366 values
        daily_values <- c(daily_values, rep(NA, 366 - length(daily_values)))
        # Take only first 366 values in case there are more
        daily_values <- daily_values[1:366]
        
        # Create output row (exactly like original format)
        daily_row <- data.frame(
          SampleID = metadata$SampleID,
          SiteID = metadata$SiteID, 
          TreatmentID = metadata$TreatmentID,
          year = yr,
          variable = var_name,
          Model = "DayCent",
          unit = var_spec$unit,
          stringsAsFactors = FALSE
        )
        
        # Add daily columns d1, d2, ..., d366 (exactly like original)
        for (d in 1:366) {
          col_name <- paste("d", d, sep = "")
          daily_row[[col_name]] <- daily_values[d]
        }
        
        daily_results <- rbind(daily_results, daily_row)
      }
    } else {
      if (verbose) cat("  No matching years for", var_name, "\n")
    }
    }
  } else {
    if (verbose) cat("  No daily variables to process - skipping daily outputs\n")
  }
  
  return(list(
    daily_results = daily_results,
    num_obs_years = num_obs_years
  ))
}

#' Process Annual Outputs in Batch
#'
#' Processes all annual output variables in a single pass, creating annual output rows
#' that match the daily output format but with single annual values.
#'
#' @param all_file_data Named list of data frames from read_all_output_files_batch
#' @param annual_vars Data frame with annual variable specifications
#' @param metadata List with SampleID, SiteID, TreatmentID for output rows
#' @param config Configuration object for observation data paths
#' @param agg_metadata Optional aggregation metadata for weighted mean processing
#' @param verbose Logical indicating whether to print progress messages
#' @return List with annual_results data frame and num_obs_years count
#'
#' @export
process_annual_outputs_batch <- function(all_file_data, annual_vars, metadata, config, agg_metadata = NULL, verbose = FALSE) {
  
  annual_results <- NULL
  num_obs_years <- 0
  obs_data_dir <- file.path(config$paths$lairice_root, config$model_outputs$observation_data_dir)
  
  if (verbose) cat("Processing annual outputs:\n")
  
  if (nrow(annual_vars) > 0) {
    for (i in 1:nrow(annual_vars)) {
      var_spec <- annual_vars[i, ]
      file_name <- var_spec$output_file
      var_name <- var_spec$variable_name
      
      # Skip if file data not available
      if (!file_name %in% names(all_file_data)) {
        if (verbose) cat("  Skipping", var_name, "- file", file_name, "not found\n")
        next
      }
    
      file_data <- all_file_data[[file_name]]
    
      # Skip if variable not in file
      if (!var_name %in% names(file_data)) {
        if (verbose) cat("  Skipping", var_name, "- variable not in", file_name, "\n")
        next
      }
      
      # Get observation years for this variable using adaptive logic
      site_metadata <- list(site_name = metadata$SiteID, treatment_id = metadata$TreatmentID)
      obs_years <- get_observation_years_adaptive(var_spec, site_metadata, 
                                                 aggregation_metadata = agg_metadata, 
                                                 obs_data_dir, verbose = FALSE)
      num_obs_years <- num_obs_years + length(obs_years)
      
      # Get model years
      mod_years <- unique(file_data$year)
      
      # Find intersection of model and observation years
      data_years <- intersect(mod_years, obs_years)
      
      # Skip if no matching years found
      if (length(data_years) < 1) {
        if (verbose) cat("  Skipping", var_name, "- no matching observation years\n")
        next
      }
      
      # Create annual output rows for each year
      if (length(data_years) > 0) {
        if (verbose) cat("  Creating annual output for", var_name, ":", length(data_years), "years\n")
        
        for (yr in data_years) {
          year_data <- file_data[file_data$year == yr, ]
          
          # Calculate annual value based on transform function
          if (var_spec$transform_function == "sum_transform") {
            annual_value <- sum(year_data[[var_name]], na.rm = TRUE)
          } else if (var_spec$transform_function == "mean_transform") {
            annual_value <- mean(year_data[[var_name]], na.rm = TRUE)
          } else if (var_spec$transform_function == "end_of_year") {
            annual_value <- year_data[[var_name]][nrow(year_data)]  # Last value of year
          } else {
            annual_value <- mean(year_data[[var_name]], na.rm = TRUE)  # Default to mean
          }
          
          # Create annual output row (single column format)
          annual_row <- data.frame(
            SampleID = metadata$SampleID,
            SiteID = metadata$SiteID, 
            TreatmentID = metadata$TreatmentID,
            year = yr,
            variable = var_name,
            Model = "DayCent",
            unit = var_spec$unit,
            d1 = annual_value,  # Single annual column
            stringsAsFactors = FALSE
          )
          
          annual_results <- rbind(annual_results, annual_row)
        }
      }
    }
  } else {
    if (verbose) cat("  No annual variables to process - skipping annual outputs\n")
  }
  
  return(list(
    annual_results = annual_results,
    num_obs_years = num_obs_years
  ))
}
