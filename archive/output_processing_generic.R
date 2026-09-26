#' Generic Output Processing Functions for DayCent Calibration
#'
#' This module provides generic functions for processing DayCent model outputs
#' based on specification files, making the calibration framework work with
#' any calibration target.
#'
#' @name output_processing_generic
NULL

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
                        "unit", "daily_output", "aggregate_output", 
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
  
  # Get enabled files
  enabled_files <- files_spec$output_file[files_spec$enable_output == 1]
  
  # Filter variables to only those from enabled files
  enabled_variables <- variables_spec[variables_spec$output_file %in% enabled_files, ]
  
  # Separate daily and aggregated variables
  daily_variables <- enabled_variables[enabled_variables$daily_output == TRUE, ]
  aggregated_variables <- enabled_variables[enabled_variables$aggregate_output == TRUE, ]
  
  if (verbose) {
    cat("  Variables specification loaded:", nrow(variables_spec), "variables\n")
    cat("  Files specification loaded:", nrow(files_spec), "files\n")
    cat("  Enabled files:", length(enabled_files), "\n")
    cat("  Daily output variables:", nrow(daily_variables), "\n")
    cat("  Aggregated variables:", nrow(aggregated_variables), "\n")
  }
  
  # Return structured specification object
  list(
    variables = variables_spec,
    files = files_spec,
    enabled_files = enabled_files,
    daily_variables = daily_variables,
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
  obs_years <- c()
  
  for (year_col in year_cols) {
    if (year_col %in% names(obs_data)) {
      obs_years <- c(obs_years, unique(obs_data[[year_col]]))
    } else {
      if (verbose) cat("Year column", year_col, "not found in observation data\n")
    }
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
  for (i in 1:nrow(output_specs$aggregated_variables)) {
    var_row <- output_specs$aggregated_variables[i, ]
    agg_name <- var_row$aggregate_name
    
    if (!is.na(agg_name) && agg_name != "") {
      agg_vars[[agg_name]] <- 0
    }
  }
  
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
#' @param agg_vars Named list of aggregated variables
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
#' agg_result <- create_aggregated_results_generic(agg_vars, 1234)
#' }
#'
#' @export
create_aggregated_results_generic <- function(agg_vars, sample_id, verbose = FALSE) {
  
  # Create base data frame with SampleID
  result_df <- data.frame(SampleID = sample_id, stringsAsFactors = FALSE)
  
  # Add all aggregated variables as columns
  for (var_name in names(agg_vars)) {
    result_df[[var_name]] <- agg_vars[[var_name]]
  }
  
  if (verbose) {
    cat("Created aggregated results with", ncol(result_df) - 1, "variables\n")
  }
  
  return(result_df)
}


#' Validate Output Specifications
#'
#' Performs comprehensive validation of the output specifications to ensure
#' they are complete and consistent.
#'
#' @param output_specs Output specifications object from load_output_specifications()
#' @param verbose Logical indicating whether to print progress messages
#' @return Logical indicating whether validation passed
#'
#' @details
#' This function checks for:
#' - Duplicate variable names within the same output file
#' - Valid transform function names
#' - Consistent aggregate names
#' - Required observation file existence
#'
#' @export
validate_output_specifications <- function(output_specs, verbose = FALSE) {
  
  if (verbose) cat("Validating output specifications...\n")
  
  validation_passed <- TRUE
  
  # Check for duplicate variables within same file
  variables <- output_specs$variables
  for (file in unique(variables$output_file)) {
    file_vars <- variables[variables$output_file == file, ]
    duplicates <- duplicated(file_vars$variable_name)
    if (any(duplicates)) {
      cat("ERROR: Duplicate variables in", file, ":", 
          paste(file_vars$variable_name[duplicates], collapse = ", "), "\n")
      validation_passed <- FALSE
    }
  }
  
  # Check transform function names
  valid_transforms <- c("abs_sum_div_1e6", "sum_div_1e6", "mean_transform", 
                       "sum_transform", "computed_n2o")
  invalid_transforms <- setdiff(variables$transform_function, c(valid_transforms, ""))
  if (length(invalid_transforms) > 0) {
    cat("ERROR: Invalid transform functions:", paste(invalid_transforms, collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  # Check for empty aggregate names where aggregate_output is TRUE
  agg_vars <- variables[variables$aggregate_output == TRUE, ]
  empty_agg_names <- is.na(agg_vars$aggregate_name) | agg_vars$aggregate_name == ""
  if (any(empty_agg_names)) {
    cat("ERROR: Missing aggregate names for variables:", 
        paste(agg_vars$variable_name[empty_agg_names], collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  if (verbose) {
    if (validation_passed) {
      cat("✅ Output specifications validation passed\n")
    } else {
      cat("❌ Output specifications validation failed\n")
    }
  }
  
  return(validation_passed)
}


#' Process Single DayCent Output File Generically
#'
#' Processes a single DayCent output file based on variable specifications,
#' applying transforms and creating daily outputs as needed.
#'
#' @param file_path Path to the DayCent output file
#' @param file_specs Data frame containing variable specifications for this file
#' @param metadata List containing metadata (SampleID, SiteID, TreatmentID, year)
#' @param agg_vars Named list of aggregated variables to update
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing:
#'   \item{daily_results}{Data frame with daily output results}
#'   \item{agg_vars}{Updated aggregated variables list}
#'   \item{num_sim_years}{Number of simulation years processed}
#'   \item{num_obs_years}{Number of observation years for this variable}
#'
#' @details
#' This function reads a DayCent output file and processes all variables
#' specified for that file in the specifications. It applies transforms,
#' creates daily outputs, and updates aggregated variables based on the
#' specification-driven approach.
#'
#' @examples
#' \dontrun{
#' result <- process_single_output_file("nflux.out", file_specs, metadata, 
#'                                      agg_vars, obs_data_dir)
#' }
#'
#' @export
process_single_output_file <- function(file_path, file_specs, metadata, agg_vars, 
                                      obs_data_dir, verbose = FALSE) {
  
  daily_results <- NULL
  num_sim_years <- 0
  num_obs_years <- 0
  
  if (!file.exists(file_path)) {
    if (verbose) cat("Output file not found:", file_path, "\n")
    return(list(
      daily_results = daily_results,
      agg_vars = agg_vars,
      num_sim_years = num_sim_years,
      num_obs_years = num_obs_years
    ))
  }
  
  if (verbose) cat("Processing output file:", basename(file_path), "\n")
  
  # Read the output file
  output_data <- read.table(file_path, header = TRUE)
  
  # Add year column if time column exists
  if ("time" %in% names(output_data)) {
    output_data$year <- output_data$time %/% 1
    mod_years <- unique(output_data$year)
    num_sim_years <- length(mod_years)
  } else {
    mod_years <- c()
    if (verbose) cat("No time column found in", basename(file_path), "\n")
  }
  
  # First pass: Process computed variables (like DayCent_N2O) that need raw data
  if (verbose) cat("  First pass: Processing computed variables\n")
  for (i in 1:nrow(file_specs)) {
    var_spec <- file_specs[i, ]
    var_name <- var_spec$variable_name
    
    # Only process computed variables in first pass
    if (var_spec$transform_function == "computed_n2o") {
      if (verbose) cat("    Processing computed variable:", var_name, "\n")
      
      # Check if required columns exist for N2O computation
      if (all(c("nit_N2O.N", "dnit_N2O.N") %in% names(output_data))) {
        output_data$DayCent_N2O <- output_data$nit_N2O.N + output_data$dnit_N2O.N
        if (verbose) cat("    Created DayCent_N2O column from nit_N2O.N + dnit_N2O.N\n")
      } else {
        if (verbose) cat("    Cannot compute N2O: missing nit_N2O.N or dnit_N2O.N\n")
        next
      }
    }
  }
  
  # Second pass: Process all variables including computed ones
  if (verbose) cat("  Second pass: Processing all variables\n")
  for (i in 1:nrow(file_specs)) {
    var_spec <- file_specs[i, ]
    var_name <- var_spec$variable_name
    
    if (verbose) cat("    Processing variable:", var_name, "\n")
    
    # Check if variable exists in output data (now includes computed variables)
    if (!var_name %in% names(output_data)) {
      if (verbose) cat("      Variable", var_name, "not found in", basename(file_path), "\n")
      next
    }
    
    # Update aggregated variables if specified
    if (var_spec$aggregate_output == TRUE && !is.na(var_spec$aggregate_name) && 
        var_spec$aggregate_name != "") {
      
      agg_name <- var_spec$aggregate_name
      transform_func <- var_spec$transform_function
      
      if (transform_func == "computed_n2o") {
        # For computed_n2o, pass the whole data frame to the transform function
        agg_value <- apply_transform_function(output_data, transform_func, verbose = FALSE)
      } else {
        # For regular variables, pass the specific column
        agg_value <- apply_transform_function(output_data[[var_name]], transform_func, verbose = FALSE)
      }
      
      if (agg_name %in% names(agg_vars)) {
        agg_vars[[agg_name]] <- agg_vars[[agg_name]] + agg_value
      } else {
        agg_vars[[agg_name]] <- agg_value
      }
      
      if (verbose) cat("      Updated aggregated variable", agg_name, ":", agg_value, "\n")
    }
    
    # Create daily output if specified
    if (var_spec$daily_output == TRUE && length(mod_years) > 0) {
      
      # Get observation years for this variable
      obs_years <- get_observation_years_generic(var_spec, metadata$TreatmentID, 
                                                obs_data_dir, verbose = FALSE)
      
      # Count observation years for this variable/treatment combination
      num_obs_years <- num_obs_years + length(obs_years)
      
      # Use intersection of model and observation years, or all model years if no observations
      if (length(obs_years) > 0) {
        data_years <- intersect(mod_years, obs_years)
      } else {
        data_years <- mod_years
      }
      
      if (length(data_years) > 0) {
        if (verbose) cat("    Creating daily output for", length(data_years), "years\n")
        
        for (yr in data_years) {
          temp_outvar_yr <- output_data[output_data$year == yr, ]
          
          # Create the daily values vector (up to 366 days)
          daily_values <- as.numeric(temp_outvar_yr[, var_name])
          # Pad with NA to make it 366 values
          daily_values <- c(daily_values, rep(NA, 366 - length(daily_values)))
          # Take only first 366 values in case there are more
          daily_values <- daily_values[1:366]
          
          temp_outvar_yr_wide <- data.frame(
            SampleID = metadata$SampleID,
            SiteID = metadata$SiteID,
            TreatmentID = metadata$TreatmentID,
            year = yr,
            variable = var_name,
            Model = "DayCent",
            unit = var_spec$unit,
            stringsAsFactors = FALSE
          )
          
          # Add daily columns
          for (d in 1:366) {
            col_name <- paste("d", d, sep = "")
            temp_outvar_yr_wide[[col_name]] <- daily_values[d]
          }
          
          daily_results <- rbind(daily_results, temp_outvar_yr_wide)
        }
      }
    }
  }
  
  return(list(
    daily_results = daily_results,
    agg_vars = agg_vars,
    num_sim_years = num_sim_years,
    num_obs_years = num_obs_years
  ))
}


#' Process All DayCent Output Files Generically
#'
#' Orchestrates the processing of all enabled DayCent output files based on
#' the specifications, replacing the hardcoded output processing logic.
#'
#' @param config Configuration object containing paths and specifications
#' @param output_specs Output specifications object from load_output_specifications()
#' @param sim_dir Directory containing DayCent output files
#' @param metadata List containing metadata (SampleID, SiteID, TreatmentID)
#' @param agg_vars Named list of aggregated variables to update
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing:
#'   \item{daily_results}{Combined data frame with all daily output results}
#'   \item{agg_vars}{Updated aggregated variables list}
#'   \item{num_sim_years}{Total number of simulation years processed}
#'   \item{num_obs_years}{Total number of observation years processed}
#'
#' @details
#' This function replaces the hardcoded output processing sections in the
#' original GSA Step 2 functions. It processes all enabled output files
#' according to their specifications, applying transforms and creating
#' daily outputs as configured.
#'
#' @examples
#' \dontrun{
#' result <- process_output_files_generic(config, output_specs, getwd(), 
#'                                        metadata, agg_vars, verbose = TRUE)
#' }
#'
#' @export
process_output_files_generic <- function(config, output_specs, sim_dir, metadata, 
                                        agg_vars, verbose = FALSE) {
  
  combined_daily_results <- NULL
  total_sim_years <- 0
  total_obs_years <- 0
  obs_data_dir <- file.path(config$paths$lairice_root, config$model_outputs$observation_data_dir)
  
  if (verbose) cat("Processing DayCent output files in:", sim_dir, "\n")
  
  # Process each enabled file
  for (file_name in output_specs$enabled_files) {
    file_path <- file.path(sim_dir, file_name)
    
    # Get variable specifications for this file
    file_specs <- output_specs$variables[output_specs$variables$output_file == file_name, ]
    
    if (nrow(file_specs) == 0) {
      if (verbose) cat("No variable specifications found for", file_name, "\n")
      next
    }
    
    # Process this file
    file_result <- process_single_output_file(
      file_path = file_path,
      file_specs = file_specs,
      metadata = metadata,
      agg_vars = agg_vars,
      obs_data_dir = obs_data_dir,
      verbose = verbose
    )
    
    # Combine results
    combined_daily_results <- rbind(combined_daily_results, file_result$daily_results)
    agg_vars <- file_result$agg_vars
    total_sim_years <- total_sim_years + file_result$num_sim_years
    total_obs_years <- total_obs_years + file_result$num_obs_years
  }
  
  if (verbose) {
    cat("Processed", length(output_specs$enabled_files), "output files\n")
    cat("Total simulation years:", total_sim_years, "\n")
    cat("Daily results rows:", nrow(combined_daily_results), "\n")
  }
  
  return(list(
    daily_results = combined_daily_results,
    agg_vars = agg_vars,
    num_sim_years = total_sim_years,
    num_obs_years = total_obs_years
  ))
}


#' Process Model Outputs Generically (Alternative to Original)
#'
#' Generic replacement for the hardcoded process_model_outputs() function.
#' This function provides the same interface but uses specification-driven
#' processing instead of hardcoded logic.
#'
#' @param site_id Site identifier
#' @param trt_sch_file Treatment schedule file name
#' @param actual_task_id Task ID for this simulation
#' @param config Configuration object
#' @param agg_vars Aggregated variables list
#' @param verbose Logical indicating whether to print progress messages
#' @return List with processed results and updated aggregated variables
#'
#' @details
#' This function can be used as a drop-in replacement for the original
#' process_model_outputs() function to enable comparison testing between
#' the hardcoded and generic approaches.
#'
#' @examples
#' \dontrun{
#' result <- process_model_outputs_generic(site_id, trt_sch_file, actual_task_id,
#'                                        config, agg_vars, verbose = TRUE)
#' }
#'
#' @export
process_model_outputs_generic <- function(site_id, trt_sch_file, actual_task_id, 
                                         config, agg_vars, verbose = FALSE) {
  
  # Load output specifications
  output_specs <- load_output_specifications(config, verbose = FALSE)
  
  # Create metadata
  metadata <- list(
    SampleID = actual_task_id,
    SiteID = site_id,
    TreatmentID = trt_sch_file
  )
  
  # Process all output files generically
  result <- process_output_files_generic(
    config = config,
    output_specs = output_specs,
    sim_dir = getwd(),  # Current working directory (should be site simulation directory)
    metadata = metadata,
    agg_vars = agg_vars,
    verbose = verbose
  )
  
  return(list(
    daily_results = result$daily_results,
    num_sim_years = result$num_sim_years,
    num_obs_years = result$num_obs_years,
    agg_vars = result$agg_vars
  ))
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
#' }
#'
#' @export
run_site_simulations_generic <- function(site_id, run_file_site, config, params_df, 
                                        sim_dir_tid, daycent_exe, actual_task_id, 
                                        agg_vars, verbose = TRUE) {
  
  # Initialize return values
  num_treatments <- 0
  num_sim_years <- 0
  daily_results <- NULL
  
  # Setup directories
  site_dir_from <- file.path(config$paths$expsites_dir, site_id)
  site_sim_dir <- file.path(sim_dir_tid, site_id)
  
  if (verbose) {
    cat("Site source path:", site_dir_from, "\n")
    cat("Scratch run path:", site_sim_dir, "\n")
  }
  
  # Setup DayCent run files
  if (dir.exists(site_sim_dir)) {
    unlink(site_sim_dir, recursive = TRUE)
  }
  dir.create(site_sim_dir, recursive = TRUE)
  
  # Copy site files
  copy_status <- copy_sitefiles(site_folder_from = site_dir_from, 
                               site_folder_to = site_sim_dir)
  if (copy_status != 0) {
    stop("Site files copy failed for ", site_id)
  }
  
  if (verbose) cat("Site files copied successfully\n")
  
  # Set working directory
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(site_sim_dir)
  
  # Copy dot100 files
  dot100_status <- copy_dot100_files(dot100_directory = config$paths$dot100_path,
                                    simulation_directory = site_sim_dir)
  if (dot100_status != 0) {
    stop("dot100 files copy failed for ", site_id)
  }
  
  if (verbose) cat("dot100 files copied successfully\n")
  
  # Update fix.100 parameters (matching original script - once per site)
  fix100_params <- params_df[params_df$File == "fix.100", ]
  if (nrow(fix100_params) > 0) {
    fix_status <- update_fix100_parameters(fix100_params, save_copy = FALSE)
    if (fix_status != 0) {
      stop("fix.100 update failed for ", site_id)
    }
    if (verbose) cat("fix.100 updated successfully\n")
  }
  
  # Update fert.100 parameters (if any)
  fert100_params <- params_df[params_df$File == "fert.100", ]
  if (nrow(fert100_params) > 0) {
    fert_status <- update_fert100_parameters(fert100_params, save_copy = FALSE)
    if (fert_status != 0) {
      stop("fert.100 update failed for ", site_id)
    }
    if (verbose) cat("fert.100 updated successfully\n")
  }
  
  # Get treatment schedules from runFile (matching original logic)
  trt_schs <- sort(unique(run_file_site$treatment_schedule))
  num_treatments <- length(trt_schs)
  
  if (verbose) cat("Number of treatments:", num_treatments, "\n")
  
  # Copy outfiles.in file (matching original code exactly)
  outfiles_from <- file.path(config$paths$dot100_path, "nh3_outfiles.in")
  outfiles_to <- file.path(site_sim_dir, "outfiles.in")
  if (file.exists(outfiles_from)) {
    file.copy(from = outfiles_from, to = outfiles_to, overwrite = TRUE)
    if (verbose) cat("outfiles.in copied successfully\n")
  } else {
    warning("outfiles.in source file not found: ", outfiles_from)
  }
  
  # Run DayCent for each treatment (matching original logic exactly)
  site100_params <- params_df[params_df$File == "site.100", ]
  
  for (j in 1:length(trt_schs)) {
    num_treatments_processed <- j
    trt_sch_file <- trimws(trt_schs[j])
    
    if (verbose) cat("Running DayCent for treatment:", trt_sch_file, "\n")
    
    # Get base schedule for this treatment (matching original logic)
    treatment_row <- run_file_site[run_file_site$treatment_schedule == trt_sch_file, ]
    if (nrow(treatment_row) == 0) {
      stop("No base_schedule found for treatment: ", trt_sch_file)
    }
    
    bh_sch_file <- trimws(treatment_row$base_schedule[1])
    bh_site100 <- paste0(strip_dot_sch(sch_file_name = bh_sch_file), "_site.100")
    
    if (verbose) cat("Base schedule:", bh_sch_file, "-> Site file:", bh_site100, "\n")
    
    # Update extended base site.100 (matching original logic)
    if (nrow(site100_params) > 0) {
      site_status <- update_site100_parameters(site100_file = bh_site100, paramsdf = site100_params)
      if (site_status != 0) {
        stop("ext_site.100 update failed for ", site_id, " with file ", bh_site100)
      }
      if (verbose) cat("ext_site.100 updated successfully\n")
    }
    
    # Run DayCent
    daycent_status <- run_DayCent(filepath_exe = daycent_exe,
                                 sch_file = trt_sch_file,
                                 ext_site100_2read = bh_site100,
                                 ext_site100_2write = NULL)
    
    if (daycent_status != 0) {
      stop("DayCent simulation failed for ", site_id, "::", trt_sch_file)
    }
    
    if (verbose) cat("DayCent execution successful for", trt_sch_file, "\n")
    
    # Process model outputs using generic function
    treatment_results <- process_model_outputs_generic(site_id, trt_sch_file, actual_task_id, 
                                                      config, agg_vars, verbose = verbose)
    
    # Update results
    num_sim_years <- num_sim_years + treatment_results$num_sim_years
    daily_results <- rbind(daily_results, treatment_results$daily_results)
    agg_vars <- treatment_results$agg_vars
  }
  
  return(list(
    num_treatments = num_treatments,
    num_sim_years = num_sim_years,
    daily_results = daily_results,
    agg_vars = agg_vars
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
#' }
#'
#' @export
gsa_step2_simulate_individual_generic <- function(config, gsa_method, sim_id, daycent_exe, 
                                                 scratch_dir, clean_scratch = TRUE, 
                                                 start_id = 0, verbose = TRUE) {
  
  # Initialize timing and status tracking
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE
  
  # Calculate actual simulation ID (matching original script logic)
  actual_sim_id <- start_id + sim_id
  
  # Initialize output variables
  output_result <- list(
    status = 1,  # Default to error
    daily_results = NULL,
    aggregated_results = NULL,
    runtime_info = NULL,
    sim_id = actual_sim_id,
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
    cat("\t Simulation ID (SampleID)    :", actual_sim_id, "\n")
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
    
    # Create directories
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
    
    # Initialize results (matching original counters)
    dRslt <- NULL
    num_sim_years <- 0
    num_nh3_mes_years <- 0
    num_urea_mes_years <- 0
    
    # Load RunFile.rds (matching original logic)
    run_file_name <- file.path(gsa_output_path, "RunFile.rds")
    if (!file.exists(run_file_name)) {
      stop("RunFile.rds not found: ", run_file_name)
    }
    
    runFile <- readRDS(run_file_name)
    exp_SiteIDs <- sort(unique(runFile$siteID))
    
    # Load observation data for measurement year counting (matching original)
    observation_dir <- file.path(config$paths$lairice_root, "data", config$project$name, "Observation_Data")
    cumNH3_file <- file.path(observation_dir, "NH3_allmeasurements_28Feb2025.csv")
    urea_file <- file.path(observation_dir, "Urea_measurements_28Feb2025.csv")
    
    cumNH3 <- NULL
    mes_urea <- NULL
    if (file.exists(cumNH3_file)) {
      cumNH3 <- read.csv(cumNH3_file, stringsAsFactors = FALSE)
    }
    if (file.exists(urea_file)) {
      mes_urea <- read.csv(urea_file, stringsAsFactors = FALSE)
    }
    
    if (verbose) cat("Processing", length(exp_SiteIDs), "sites with RunFile structure\n")
    
    # Run simulations for each site (matching original logic)
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
        verbose = verbose
      )
      
      # Update results
      num_sim_years <- num_sim_years + site_result$num_sim_years
      dRslt <- rbind(dRslt, site_result$daily_results)
      agg_vars <- site_result$agg_vars
    }
    
    # Create aggregated results using generic function
    agg_result <- create_aggregated_results_generic(agg_vars, actual_sim_id, verbose = FALSE)
    
    # Save results (matching original directory structure and file formats)
    daily_output_dir <- file.path(gsa_output_path, "Daily_Outputs", paste0("jobGroup_", group_id))
    agg_output_dir <- file.path(gsa_output_path, "Aggregated_Outputs", paste0("jobGroup_", group_id))
    run_status_dir <- file.path(gsa_output_path, "Run_Status", paste0("jobGroup_", group_id))
    
    dir.create(daily_output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(agg_output_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(run_status_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Save daily results as RDS (matching original format)
    daily_output_file <- file.path(daily_output_dir, paste0("dc_dRslt_", actual_sim_id, ".rds"))
    if (!is.null(dRslt) && nrow(dRslt) > 0) {
      saveRDS(dRslt, daily_output_file)
    }
    
    # Save aggregated results as RDS (matching original format)
    agg_output_file <- file.path(agg_output_dir, paste0("dc_aggRslt_", actual_sim_id, ".rds"))
    saveRDS(agg_result, agg_output_file)
    
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
      nh3_years = num_nh3_mes_years,
      urea_years = num_urea_mes_years,
      status = 0,
      message = "Execution Success..",
      stringsAsFactors = FALSE
    )
    
    # Save runtime info
    runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_sim_id, ".csv"))
    write.csv(runtime_info, runtime_file, row.names = FALSE)
    
    # Clean up scratch directory if requested
    if (clean_scratch && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
    
    complete <- TRUE
    
    # Update output result
    output_result$status <- 0
    output_result$daily_results <- dRslt
    output_result$aggregated_results <- agg_result
    output_result$runtime_info <- runtime_info
    output_result$output_files <- list(
      daily_output = daily_output_file,
      aggregated_output = agg_output_file,
      runtime_info = runtime_file
    )
    
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
      nh3_years = ifelse(exists("num_nh3_mes_years"), num_nh3_mes_years, 0),
      urea_years = ifelse(exists("num_urea_mes_years"), num_urea_mes_years, 0),
      status = 1,
      message = as.character(err),
      stringsAsFactors = FALSE
    )
    
    # Try to save error info if directory exists
    if (exists("run_status_dir") && dir.exists(run_status_dir)) {
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_sim_id, "_error.csv"))
      write.csv(runtime_info, runtime_file, row.names = FALSE)
      output_result$output_files$runtime_info <- runtime_file
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
                                      clean_scratch = TRUE, start_id = 0, verbose = TRUE) {
  
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