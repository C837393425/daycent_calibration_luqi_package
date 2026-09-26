#' @title Step 3 Weighted Mean Data Adapter Functions
#' @description Functions to handle weighted mean data sources in likelihood calculations
#'
#' @details This module provides functions to:
#' - Detect when weighted mean outputs should be used based on output specifications
#' - Load and convert weighted mean data to format expected by likelihood calculations
#' - Maintain backward compatibility for projects without weighted mean aggregation
#'
#' The weighted mean adapter is only used when:
#' 1. Output_Variables_Specification.csv contains weighted_mean_aggregation=TRUE for the variable
#' 2. Weighted mean output files exist for the current sample
#' 3. The observation data requires county-level aggregated model outputs

#' Resolve GSA vs SIR database result table from method directory path
#' @noRd
resolve_step3_results_table <- function(tables, method_dir, table_type = c("weighted", "annual")) {
  table_type <- match.arg(table_type)
  sep <- .Platform$file.sep
  is_sir <- grepl(paste0(sep, "SIR"), method_dir, fixed = TRUE)

  if (table_type == "weighted") {
    primary <- if (is_sir) tables$sir_results_weighted else tables$gsa_results_weighted
    fallback <- if (is_sir) tables$gsa_results_weighted else tables$sir_results_weighted
  } else {
    primary <- if (is_sir) tables$sir_results_annual else tables$gsa_results_annual
    fallback <- if (is_sir) tables$gsa_results_annual else tables$sir_results_annual
  }

  if (is.null(primary) || length(primary) == 0 || is.na(primary)) {
    primary <- fallback
  }
  primary
}

#' Detect Data Source for Likelihood Calculation
#'
#' Determines whether to use weighted mean outputs or regular annual outputs
#' for likelihood calculation based on output specifications and data availability.
#'
#' @param config Configuration object
#' @param var_config Variable configuration from likelihood_calculation section
#' @param task_id Sample/task ID to process
#' @param group_id Job group ID
#' @param annual_output_file Path to annual output file
#' @param method_dir Method directory (GSA or SIR method directory)
#' @param verbose Logical, whether to print diagnostic messages
#'
#' @return List containing:
#'   \item{use_weighted_mean}{Boolean indicating if weighted mean should be used}
#'   \item{data_source}{Character: "weighted_mean" or "annual"}
#'   \item{file_path}{Path to the data file to use}
#'   \item{reason}{Character description of why this data source was chosen}
#'
#' @export
detect_data_source_for_likelihood <- function(config, var_config, task_id, group_id,
                                             annual_output_file, method_dir, verbose = TRUE) {

  # Default to annual outputs
  result <- list(
    use_weighted_mean = FALSE,
    data_source = "annual",
    file_path = annual_output_file,
    reason = "Default to annual outputs"
  )

  tryCatch({
    # Step 1: Check if weighted mean aggregation is enabled for this variable
    output_specs <- load_output_specifications(config, verbose = FALSE)

    if (!"variables" %in% names(output_specs)) {
      result$reason <- "No output specifications found"
      return(result)
    }

    # Find the variable specification
    var_spec <- output_specs$variables[output_specs$variables$variable_name == var_config$model_output, ]

    if (nrow(var_spec) == 0) {
      result$reason <- paste("Variable", var_config$model_output, "not found in output specifications")
      return(result)
    }

    # Check if weighted_mean_aggregation column exists and is TRUE for this variable
    if (!"weighted_mean_aggregation" %in% names(var_spec)) {
      result$reason <- paste("Variable", var_config$model_output, "has no weighted_mean_aggregation column")
      return(result)
    }

    if (is.na(var_spec$weighted_mean_aggregation[1]) || var_spec$weighted_mean_aggregation[1] != TRUE) {
      result$reason <- paste("Variable", var_config$model_output, "does not have weighted_mean_aggregation=TRUE")
      return(result)
    }

    if (verbose) {
      cat("  Variable", var_config$model_output, "has weighted_mean_aggregation=TRUE\n")
    }

    # Step 2: Check if weighted mean output files exist
    if(config$file_source$database$database_result$run_type == "file_system") {
      weighted_output_dir <- file.path(method_dir, "Weighted_Mean_Outputs", paste0("jobGroup_", group_id))
      weighted_output_file <- file.path(weighted_output_dir, paste0("weighted_", var_config$model_output, "_sample_", task_id, ".rds"))
      
      if (!file.exists(weighted_output_file)) {
        result$reason <- paste("Weighted mean file not found:", weighted_output_file)
        return(result)
      }

    } else if(config$file_source$database$database_result$run_type == "database") {
      tables <- config$file_source$database$database_result$tables
      weighted_output_file <- resolve_step3_results_table(tables, method_dir, "weighted")
    }

    

    if (verbose) {
      cat("  Weighted mean output file found:", weighted_output_file, "\n")
    }

    # Step 3: Use weighted mean outputs
    result <- list(
      use_weighted_mean = TRUE,
      data_source = "weighted_mean",
      file_path = weighted_output_file,
      reason = paste("Using weighted mean outputs for variable", var_config$model_output)
    )

    return(result)

  }, error = function(e) {
    if (verbose) {
      cat("  Warning: Error in data source detection:", e$message, "\n")
      cat("  Falling back to annual outputs\n")
    }
    result$reason <- paste("Error in detection, using annual outputs:", e$message)
    return(result)
  })
}

#' Load and Convert Weighted Mean Data to Annual Format
#'
#' Loads weighted mean output data and converts it to the format expected by
#' likelihood calculation functions (similar to annual output format).
#'
#' @param weighted_file_path Path to weighted mean RDS file
#' @param var_config Variable configuration from likelihood_calculation section
#' @param task_id Sample/task ID for validation
#' @param verbose Logical, whether to print diagnostic messages
#'
#' @return Data frame in annual output format with columns:
#'   \item{SampleID}{Sample ID}
#'   \item{SiteID}{Site ID (set to aggregation_level for county data)}
#'   \item{TreatmentID}{Treatment ID (set to aggregation_level for county data)}
#'   \item{year}{Year}
#'   \item{variable}{Variable name}
#'   \item{Model}{Model name (DayCent)}
#'   \item{unit}{Unit}
#'   \item{d1}{Model value (converted from weighted_value)}
#'
#' @export
load_and_convert_weighted_mean_data <- function(config, weighted_file_path, var_config, task_id,
                                               obs_data = NULL, verbose = TRUE) {

  task_id <- as.integer(task_id)

  if (verbose) {
    cat("  Loading weighted mean data from:", weighted_file_path, "\n")
  }

  # Load weighted mean data
  if(config$file_source$database$database_result$run_type == "file_system") {
    weighted_data <- readRDS(weighted_file_path)
  
  } else if(config$file_source$database$database_result$run_type == "database") {
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
    # Use table name from detect_data_source_for_likelihood (weighted_file_path)
    results_weighted <- weighted_file_path

      dbConn_result <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = host, dbname = db_calib,
        username = user, password = password
      )
      on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE) 

      # Load weighted average results from database
      weighted_data <- DBI::dbGetQuery(dbConn_result, paste0(
        "SELECT * FROM ", results_weighted,
        " WHERE simulation_id = ", task_id, " ;"))
      weighted_data <- unique(weighted_data)
      DBI::dbDisconnect(dbConn_result)
  }

  # Validate data structure
  required_cols <- c("aggregation_level", "year", "variable_name", "weighted_value", "sample_id")
  missing_cols <- required_cols[!required_cols %in% names(weighted_data)]

  if (length(missing_cols) > 0) {
    stop("Weighted mean data missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  id_col <- if ("sample_id" %in% names(weighted_data)) "sample_id" else "simulation_id"
  if (!task_id %in% as.integer(weighted_data[[id_col]])) {
    stop("Task ID ", task_id, " not found in weighted mean data")
  }

  # Filter for current sample
  sample_data <- weighted_data[as.integer(weighted_data[[id_col]]) == task_id, ]

  if (verbose) {
    cat("  Converting", nrow(sample_data), "weighted mean records to annual format\n")
  }

  # Convert to annual output format
  # Create base records for the county-level aggregated data
  base_records <- data.frame(
    SampleID = sample_data$sample_id,
    SiteID = as.character(sample_data$aggregation_level),  # Use county code as SiteID
    TreatmentID = paste0(sample_data$aggregation_level, ".sch"),  # Create treatment ID
    year = sample_data$year,
    variable = sample_data$variable_name,
    Model = "DayCent",
    unit = "gC_m2",  # Default unit, could be made configurable
    d1 = sample_data$weighted_value,  # Key conversion: weighted_value -> d1
    stringsAsFactors = FALSE
  )

  # For county-level aggregated data matching with site-level observations,
  # we need to replicate the county value for each site that has observations
  county_code <- unique(sample_data$aggregation_level)[1]

  # The observations contain siteID which is actually aggregation_level (county codes)
  # We only replicate the county value for counties that match the weighted mean aggregation_level
  if (!is.null(obs_data) && "siteID" %in% names(obs_data)) {
    observed_counties <- unique(obs_data$siteID)
    if (verbose) {
      cat("  Found observations for counties:", paste(observed_counties, collapse = ", "), "\n")
    }

    # Only create records for counties that match our aggregation level
    matching_counties <- observed_counties[observed_counties == county_code]

    if (length(matching_counties) > 0) {
      if (verbose) {
        cat("  Creating records for matching county:", county_code, "\n")
      }
      # Use the base records as-is since they already have the correct county code
      annual_format <- base_records
    } else {
      if (verbose) {
        cat("  Warning: No observations match county", county_code, "\n")
        cat("  Available observation counties:", paste(observed_counties, collapse = ", "), "\n")
      }
      # Still return the base records, but the likelihood calculation may not find matches
      annual_format <- base_records
    }
  } else {
    # No observation data provided, use county-level records as-is
    annual_format <- base_records
  }

  if (verbose) {
    cat("  Created", nrow(annual_format), "model records for county:", county_code, "\n")
  }

  # Validate conversion
  if (nrow(annual_format) == 0) {
    stop("No data found after conversion for task ", task_id)
  }

  if (verbose) {
    cat("  Converted to annual format with", nrow(annual_format), "records\n")
    cat("  Years:", paste(sort(unique(annual_format$year)), collapse = ", "), "\n")
    cat("  Aggregation level:", unique(annual_format$SiteID), "\n")
  }

  return(annual_format)
}

#' Smart Model Data Loader for Step 3
#'
#' Intelligently loads model data for likelihood calculation, choosing between
#' weighted mean outputs (for county-level aggregated data) and regular annual
#' outputs based on output specifications and data availability.
#'
#' @param config Configuration object
#' @param var_config Variable configuration from likelihood_calculation section
#' @param task_id Sample/task ID to process
#' @param group_id Job group ID
#' @param method_dir Method directory (GSA or SIR method directory)
#' @param annual_output_file Path to annual output file (fallback)
#' @param verbose Logical, whether to print diagnostic messages
#'
#' @return Data frame in annual output format suitable for likelihood calculation
#'

#' @export
load_model_data_for_likelihood <- function(config, var_config, task_id, group_id,
                                          method_dir, annual_output_file, obs_data = NULL, verbose = TRUE) {

  if (verbose) {
    cat("  Determining optimal data source for variable:", var_config$model_output, "\n")
  }

  if (!is.null(var_config$matching_type) && var_config$matching_type == "period_totals") {
    period_out_dir <- file.path(method_dir, "Period_Outputs", paste0("jobGroup_", group_id))
    period_output_file <- file.path(period_out_dir, paste0("dc_periodRslt_", task_id, ".rds"))
    if (config$file_source$database$database_result$run_type == "file_system") {
      if (!file.exists(period_output_file)) {
        stop("Period output file not found: ", period_output_file)
      }
      if (verbose) {
        cat("  Loading period output data from:", period_output_file, "\n")
      }
      model_data <- readRDS(period_output_file)
      model_data <- model_data[model_data$variable == var_config$model_output, , drop = FALSE]
      if (nrow(model_data) == 0) {
        stop("No period model data found for variable ", var_config$model_output)
      }
      return(model_data)
    }
  }

  # Detect which data source to use
  source_info <- detect_data_source_for_likelihood(
    config = config,
    var_config = var_config,
    task_id = task_id,
    group_id = group_id,
    annual_output_file = annual_output_file,
    method_dir = method_dir,
    verbose = verbose
  )

  if (verbose) {
    cat("  Data source decision:", source_info$data_source, "\n")
    cat("  Reason:", source_info$reason, "\n")
  }

  # Load and return appropriate data
  if (source_info$use_weighted_mean) {
    # Load and convert weighted mean data
    # Pass observation data to enable site replication for county-level data
    var_obs_data <- if (!is.null(obs_data)) obs_data[[var_config$name]] else NULL

    model_data <- load_and_convert_weighted_mean_data(
      config = config,
      weighted_file_path = source_info$file_path,
      var_config = var_config,
      task_id = task_id,
      obs_data = var_obs_data,
      verbose = verbose
    )
  } else {
    # Load regular annual data
    if (config$file_source$database$database_result$run_type == "file_system" &&
        !file.exists(source_info$file_path)) {
      stop("Annual output file not found: ", source_info$file_path)
    }

    if (verbose) {
      cat("  Loading annual output data from:", source_info$file_path, "\n")
    }
    if(config$file_source$database$database_result$run_type == "file_system") {
      model_data <- readRDS(source_info$file_path)
    } else if(config$file_source$database$database_result$run_type == "database") {
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
    tables <- config$file_source$database$database_result$tables
    results_annual <- resolve_step3_results_table(tables, method_dir, "annual")
    task_id <- as.integer(task_id)

      dbConn_result <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = host, dbname = db_calib,
        username = user, password = password
      )
      on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE) 

      # Load annual results from database
      model_data <- DBI::dbGetQuery(dbConn_result, paste0(
        "SELECT * FROM ", results_annual,
        " WHERE simulation_id = ", task_id, " ;"))
      model_data <- unique(model_data)
      DBI::dbDisconnect(dbConn_result)
    }

    # Validate annual data structure
    if (!"d1" %in% names(model_data)) {
      stop("Annual output data missing 'd1' column")
    }
  }

  # Final validation
  if (nrow(model_data) == 0) {
    stop("No model data loaded for task ", task_id)
  }

  if (verbose) {
    cat("  Loaded", nrow(model_data), "model data records\n")
  }

  return(model_data)
}
