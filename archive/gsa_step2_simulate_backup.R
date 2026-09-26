#' GSA Step 2 Simulation - Consolidated Functions
#'
#' This file contains ONLY the functions from:
#' - bayesiancalibr/R/gsa_step2_simulate.R
#' - bayesiancalibr/R/output_processing_generic.R
#'
#' Functions are organized with small utility functions at the top,
#' followed by larger orchestration functions at the bottom.

# =============================================================================
# FUNCTIONS FROM gsa_step2_simulate.R
# =============================================================================

#' Initialize Aggregated Variables
#'
#' Creates a list of aggregated variables for tracking cumulative outputs
#'
#' @return Named list of initialized aggregated variables
#' @export
initialize_aggregated_variables <- function() {
  list(
    aglivc = 0,
    aglivn = 0,
    N2O = 0,
    nit_N2O = 0,
    dnit_N2O = 0,
    dnit_N2 = 0,
    NO = 0,
    NH3 = 0,
    netMin1 = 0,
    netMin2 = 0,
    spHf = 0,
    urea = 0,
    crnf = 0
  )
}

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

#' Run Simulations for a Single Site
#'
#' Executes DayCent simulations for all treatments at a given experimental site
#'
#' @param site_id Site identifier
#' @param run_file_site Site-specific run file information
#' @param config Configuration object
#' @param params_df Parameter data frame
#' @param sim_dir_tid Task-specific simulation directory
#' @param daycent_exe Path to DayCent executable
#' @param actual_task_id Task ID for this simulation
#' @param agg_vars Aggregated variables list
#' @param verbose Logical indicating whether to print progress
#' @return List with simulation results and updated counters
#' @export
run_site_simulations <- function(site_id, run_file_site, config, params_df, 
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
  
  # Update parameters using enhanced generic dispatcher (now handles site.100 auto-detection)
  update_status <- update_daycent_parameters(params_df, simulation_dir = site_sim_dir, 
                                            verbose = FALSE)
  if (update_status != 0) {
    stop("Parameter update failed for ", site_id)
  }
  
  if (verbose) cat("Parameters updated successfully\n")
  
  # Get treatment schedules for this site
  trt_schs <- sort(unique(run_file_site$treatment_schedule))
  
  if (verbose) {
    cat("Running", length(trt_schs), "treatment(s)\n")
  }
  
  # Setup output files for NH3 measurements (could be made configurable)
  file.copy(from = file.path(config$paths$dot100_path, "nh3_outfiles.in"),
            to = file.path(site_sim_dir, "outfiles.in"),
            overwrite = TRUE)
  
  # Run treatments
  for (j in 1:length(trt_schs)) {
    num_treatments <- num_treatments + 1
    
    # Get base schedule information
    bh_sch_file <- trimws(run_file_site$base_schedule[j])
    bh_site100 <- paste0(strip_dot_sch(sch_file_name = bh_sch_file), "_site.100")
    
    # Update extended site.100 parameters
    site_update_status <- update_site100_parameters(site100_file = bh_site100, 
                                                    paramsdf = params_df)
    if (site_update_status != 0) {
      stop("Extended site.100 update failed for ", site_id)
    }
    
    # Run DayCent simulation
    trt_sch_file <- trimws(trt_schs[j])
    
    daycent_status <- run_DayCent(filepath_exe = daycent_exe,
                                 sch_file = trt_sch_file,
                                 ext_site100_2read = bh_site100,
                                 ext_site100_2write = NULL)
    
    if (daycent_status != 0) {
      stop("DayCent simulation failed for ", site_id, "::", trt_sch_file)
    }
    
    if (verbose) cat("DayCent execution successful for", trt_sch_file, "\n")
    
    # Process model outputs
    treatment_results <- process_model_outputs(site_id, trt_sch_file, actual_task_id, 
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

#' Process Model Outputs
#'
#' Reads and processes DayCent model outputs for a specific treatment
#'
#' @param site_id Site identifier
#' @param trt_sch_file Treatment schedule file name
#' @param actual_task_id Task ID for this simulation
#' @param config Configuration object
#' @param agg_vars Aggregated variables list
#' @param verbose Logical indicating whether to print progress
#' @return List with processed results and updated aggregated variables
#' @export
process_model_outputs <- function(site_id, trt_sch_file, actual_task_id, config, 
                                 agg_vars, verbose = TRUE) {
  
  daily_results <- NULL
  num_sim_years <- 0
  
  # Read nflux output
  if (file.exists("nflux.out")) {
    nflux <- read.table("nflux.out", header = TRUE)
    nflux$year <- nflux$time %/% 1
    
    num_sim_years <- length(unique(nflux$year))
    
    # Update aggregated variables
    nflux$DayCent_N2O <- nflux$nit_N2O.N + nflux$dnit_N2O.N
    agg_vars$N2O <- agg_vars$N2O + sum(abs(nflux$DayCent_N2O)) / 1000000
    agg_vars$nit_N2O <- agg_vars$nit_N2O + sum(abs(nflux$nit_N2O.N)) / 1000000
    agg_vars$dnit_N2O <- agg_vars$dnit_N2O + sum(abs(nflux$dnit_N2O.N)) / 1000000
    agg_vars$dnit_N2 <- agg_vars$dnit_N2 + sum(abs(nflux$dnit_N2.N)) / 1000000
    agg_vars$NO <- agg_vars$NO + sum(abs(nflux$NO.N)) / 1000000
    agg_vars$NH3 <- agg_vars$NH3 + sum(abs(nflux$NH3.N)) / 1000000
    agg_vars$netMin1 <- agg_vars$netMin1 + sum(abs(nflux$netNmin1.gN.m2.)) / 1000000
    agg_vars$netMin2 <- agg_vars$netMin2 + sum(abs(nflux$netNmin2.gN.m2.)) / 1000000
    
    # Process daily NH3 data (could be made configurable for other variables)
    mod_years <- unique(nflux$year)
    
    # This section could be made more generic by reading observation requirements from config
    for (yr in mod_years) {
      temp_outvar_yr <- nflux[nflux$year == yr, ]
      outvar_name <- "NH3.N"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gNH3_N_ha_day",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read ctrlfert output
  if (file.exists("ctrlfert.out")) {
    ctrlfert <- read.table("ctrlfert.out", header = TRUE)
    ctrlfert$year <- ctrlfert$time %/% 1
    
    # Update aggregated variables
    agg_vars$urea <- agg_vars$urea + sum(abs(ctrlfert$urea_left)) / 1000000
    agg_vars$spHf <- agg_vars$spHf + sum(abs(ctrlfert$spHf)) / 1000000
    agg_vars$crnf <- agg_vars$crnf + sum(abs(ctrlfert$fct_left)) / 1000000
    
    # Process daily urea data (could be made configurable)
    mod2_years <- unique(ctrlfert$year)
    
    for (yr in mod2_years) {
      temp_outvar_yr <- ctrlfert[ctrlfert$year == yr, ]
      outvar_name <- "urea_left"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gUrea_N_m2",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read bio output
  if (file.exists("bio.out")) {
    bio <- read.table("bio.out", header = TRUE)
    bio$year <- bio$time %/% 1
    
    agg_vars$aglivc <- agg_vars$aglivc + sum(abs(bio$aglivc)) / 1000000
    agg_vars$aglivn <- agg_vars$aglivn + sum(abs(bio$aglivn)) / 1000000
  }
  
  return(list(
    daily_results = daily_results,
    num_sim_years = num_sim_years,
    agg_vars = agg_vars
  ))
}

#' Create Aggregated Results Data Frame
#'
#' Creates a data frame with aggregated simulation results
#'
#' @param actual_task_id Task ID for this simulation
#' @param agg_vars Aggregated variables list
#' @return Data frame with aggregated results
create_aggregated_results <- function(actual_task_id, agg_vars) {
  data.frame(
    SampleID = actual_task_id,
    aglivc = agg_vars$aglivc,
    aglivn = agg_vars$aglivn,
    N2O = agg_vars$N2O,
    nit_N2O = agg_vars$nit_N2O,
    dnit_N2O = agg_vars$dnit_N2O,
    dnit_N2 = agg_vars$dnit_N2,
    NOx = agg_vars$NO,
    NH3 = agg_vars$NH3,
    netMin1 = agg_vars$netMin1,
    netMin2 = agg_vars$netMin2,
    spHf = agg_vars$spHf,
    urea = agg_vars$urea,
    crnf = agg_vars$crnf,
    stringsAsFactors = FALSE
  )
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
    # Only count simulation years from nflux.out (like original behavior)
    num_sim_years <- if (basename(file_path) == "nflux.out") length(mod_years) else 0
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
  num_nh3_mes_years <- 0
  num_urea_mes_years <- 0
  daily_results <- NULL
  aggregated_results <- NULL
  
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
  }else{
	    cat("---- site Files for ", site_id, " copied successfully. \n")
	}
  
  
  # Set working directory
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(site_sim_dir)
  
  # Copy dot100 files
  dot100_status <- copy_dot100_files(dot100_directory = config$paths$dot100_path,
                                    simulation_directory = site_sim_dir)
  if (dot100_status != 0) {
    stop("dot100 files copy failed for ", site_id)
  }else{
    cat("---- dot100 files for ", site_id, " copied successfully. \n")
  }
  
  # Update fix.100 parameters (matching original script - once per site)
  fix100_params <- params_df[params_df$File == "fix.100", ]
  if (nrow(fix100_params) > 0) {
    fix_status <- update_fix100_parameters(paramsdf = fix100_params, save_copy = FALSE)
    if (fix_status != 0) {
      stop("fix.100 update failed for ", site_id)
    }else{
      cat("---- fix.100 files for ", site_id, " updated successfully. \n")
    }
  }
  
  # Update fert.100 parameters (if any)
  fert100_params <- params_df[params_df$File == "fert.100", ]
  if (nrow(fert100_params) > 0) {
    fert_status <- update_fert100_parameters(fert100_params, save_copy = FALSE)
    if (fert_status != 0) {
      stop("fert.100 update failed for ", site_id)
    }else{
      cat("---- fert.100 files for ", site_id, " updated successfully. \n")
    }
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
      }else{
	      cat("---- ext_site.100  Files for ", site_id, " updated successfully." , "\n")
      }
    }
    
    # Run DayCent
    daycent_status <- run_DayCent(filepath_exe = daycent_exe,
                                  sch_file = trt_sch_file,
                                  ext_site100_2read = bh_site100,
                                  ext_site100_2write = NULL)
    
    if (daycent_status != 0) {
      stop("DayCent simulation failed for ", site_id, "::", trt_sch_file)
    }else{
      cat("---- DayCent execution successful for", trt_sch_file, "\n")
    }
    
    
    # Process model outputs using generic function
    treatment_results <- process_model_outputs_generic2(site_id, trt_sch_file, actual_task_id, 
                                                      config, agg_vars, verbose = verbose)
    
    # Update results
    num_sim_years <- num_sim_years + treatment_results$num_sim_years
    num_nh3_mes_years <- num_nh3_mes_years + treatment_results$num_nh3_mes_years
    num_urea_mes_years <- num_urea_mes_years + treatment_results$num_urea_mes_years
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
    
    # Initialize results (matching original counters)
    dRslt <- NULL
    num_sim_years <- 0
    num_nh3_mes_years <- 0
    num_urea_mes_years <- 0
    aggregated_results <- NULL
    
    # Load RunFile.rds (matching original logic)
    run_file_name <- file.path(gsa_output_path, "RunFile.rds")
    if (!file.exists(run_file_name)) {
      stop("RunFile.rds not found: ", run_file_name)
    }
    
    runFile <- readRDS(run_file_name)
    exp_SiteIDs <- sort(unique(runFile$siteID))
    
    # Load observation data for measurement year counting (matching original)
    observation_dir <- file.path(config$paths$lairice_root, "data", config$project$name, "Observation_Data")
    cumNH3_file <- file.path(observation_dir, "cumNH3_allmeasurements_28Feb2025.csv")
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
    
    # Create aggregated results using generic function
    agg_Rslt <- create_aggregated_results_generic(aggregated_results, actual_sim_id, verbose = FALSE)
    
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
    saveRDS(agg_Rslt, agg_output_file)
    
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
    output_result$aggregated_results <- agg_Rslt
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
#' @export
process_model_outputs_generic2 <- function(site_id, trt_sch_file, actual_task_id, 
                                          config, agg_vars, verbose = FALSE) {
  
  if (verbose) cat("=== Starting Streamlined Batch Processing ===\n")
  
  # 1. Load CSV specifications
  output_specs <- load_output_specifications(config, verbose = FALSE)
  
  # 2. Batch read all enabled output files
  all_file_data <- read_all_output_files_batch(output_specs$enabled_files, verbose = verbose)
  
  # 3. Add computed variables (like DayCent_N2O = nit_N2O.N + dnit_N2O.N)
  all_file_data <- add_computed_variables_batch(all_file_data, output_specs$variables, verbose = verbose)
  
  # 4. Split variables by processing type
  daily_vars <- output_specs$variables[output_specs$variables$daily_output == TRUE, ]
  agg_vars_specs <- output_specs$variables[output_specs$variables$aggregate_output == TRUE, ]
  
  if (verbose) {
    cat("Variables to process:\n")
    cat("  Daily outputs:", nrow(daily_vars), "\n")  
    cat("  Aggregated outputs:", nrow(agg_vars_specs), "\n")
  }
  
  # 5. Process aggregated outputs (one focused function)
  aggregated_results <- process_aggregated_outputs_batch(all_file_data, agg_vars_specs, agg_vars, verbose = verbose)
  
  # 6. Process daily outputs (another focused function)  
  daily_result <- process_daily_outputs_batch(
    all_file_data = all_file_data,
    daily_vars = daily_vars,
    metadata = list(SampleID = actual_task_id, SiteID = site_id, TreatmentID = trt_sch_file),
    config = config,
    verbose = verbose
  )
  
  # 7. Calculate simulation years (from nflux.out like original)
  num_sim_years <- if ("nflux.out" %in% names(all_file_data)) {
    length(unique(all_file_data[["nflux.out"]]$year))
  } else { 0 }
  
  if (verbose) {
    cat("=== Batch Processing Complete ===\n")
    cat("Daily result rows:", nrow(daily_result$daily_results), "\n")
    cat("Simulation years:", num_sim_years, "\n")
    cat("Observation years:", daily_result$num_obs_years, "\n")
  }
  
  # Calculate observation year counts (matching original implementation)
  num_nh3_mes_years <- calculate_nh3_observation_years(trt_sch_file, config, verbose)
  num_urea_mes_years <- calculate_urea_observation_years(trt_sch_file, config, verbose)
  
  if (verbose) {
    cat("NH3 observation years:", num_nh3_mes_years, "\n")
    cat("Urea observation years:", num_urea_mes_years, "\n")
  }
  
  return(list(
    daily_results = daily_result$daily_results,
    num_sim_years = num_sim_years,
    num_obs_years = daily_result$num_obs_years,
    num_nh3_mes_years = num_nh3_mes_years,
    num_urea_mes_years = num_urea_mes_years,
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
      
      # Read and add year column (matching original logic)
      data <- read.table(file_path, header = TRUE)
      if ("time" %in% names(data)) {
        data$year <- data$time %/% 1
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
  computed_vars <- variable_specs[variable_specs$transform_function == "computed_n2o", ]
  
  if (nrow(computed_vars) > 0 && verbose) {
    cat("Adding computed variables:\n")
  }
  
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
process_daily_outputs_batch <- function(all_file_data, daily_vars, metadata, config, verbose = FALSE) {
  
  daily_results <- NULL
  num_obs_years <- 0
  obs_data_dir <- file.path(config$paths$lairice_root, config$model_outputs$observation_data_dir)
  
  if (verbose) cat("Processing daily outputs:\n")
  
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
    
    # Get observation years for this variable (matching original logic)
    obs_years <- get_observation_years_generic(var_spec, metadata$TreatmentID, obs_data_dir, verbose = FALSE)
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
  
  return(list(
    daily_results = daily_results,
    num_obs_years = num_obs_years
  ))
}