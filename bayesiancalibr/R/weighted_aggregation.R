#' @title Weighted Mean Aggregation Functions for County-Level Results
#' @description Functions for aggregating site-level annual results using weights
#' to specified aggregation levels (e.g., county FIPS codes) with adaptive
#' observation year detection for backward compatibility.
#'
#' @name weighted-aggregation
NULL

#' Get County-Level Observation Years
#'
#' Extracts observation years for a specific county from observation data.
#' Used when weighted_mean_aggregation is enabled and observations are at county level.
#'
#' @param county_fips County FIPS code (aggregation_level)
#' @param var_spec Variable specification row from output specification
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return Vector of observation years for the specified county
#' @export
#' @examples
#' \dontrun{
#' obs_years <- get_county_observation_years(38001, var_spec, "data/observations")
#' }
get_county_observation_years <- function(county_fips, var_spec, obs_data_dir, verbose = FALSE) {

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

  if (verbose) cat("Loading county observation data:", var_spec$observation_file, "for county", county_fips, "\n")

  # Read observation data
  obs_data <- read.csv(obs_file_path, stringsAsFactors = FALSE)

  # For county-level observations, filter by siteID = county_fips
  if ("siteID" %in% names(obs_data)) {
    obs_data <- obs_data[obs_data$siteID == county_fips, ]
  } else {
    if (verbose) cat("siteID column not found in observation data for county filtering\n")
    return(numeric(0))
  }

  if (nrow(obs_data) == 0) {
    if (verbose) cat("No observations found for county:", county_fips, "\n")
    return(numeric(0))
  }

  # Extract years based on year columns specification
  if (is.na(var_spec$observation_year_columns) || var_spec$observation_year_columns == "") {
    if (verbose) cat("No year columns specified\n")
    return(numeric(0))
  }

  year_cols <- trimws(strsplit(var_spec$observation_year_columns, ";")[[1]])

  # Use the first year column (consistent with existing logic)
  first_year_col <- year_cols[1]

  if (first_year_col %in% names(obs_data)) {
    obs_years <- unique(obs_data[[first_year_col]])
    if (verbose) cat("Using county-level year column:", first_year_col, "for county", county_fips, "\n")
  } else {
    if (verbose) cat("Year column", first_year_col, "not found in observation data\n")
    obs_years <- c()
  }

  # Remove NA values and return sorted unique years
  obs_years <- sort(unique(obs_years[!is.na(obs_years)]))

  if (verbose) cat("Found county observation years for", county_fips, ":", paste(obs_years, collapse = ", "), "\n")

  return(obs_years)
}

normalize_weighted_target_crop_names <- function(target_crop) {
  target_crop <- trimws(as.character(target_crop))
  target_crop <- target_crop[!is.na(target_crop) & nzchar(target_crop)]
  unique(target_crop)
}

resolve_weighted_crop_filter_context <- function(config, matrix_file, target_crop,
                                                 log_function = cat) {
  crop_names <- normalize_weighted_target_crop_names(target_crop)
  crop_required <- crop_calibration_enabled(config, crop_names)

  if (!crop_required) {
    return(list(
      crop_filter_enabled = FALSE,
      site_crop_matrix = NULL,
      target_crop = NULL
    ))
  }

  if (length(crop_names) == 0) {
    stop("Crop calibration is enabled but target_crop is empty ",
         "(set gsa.crop.name / sir.crop.name, or legacy daycent.crop.name)")
  }

  if (!file.exists(matrix_file)) {
    stop("Crop calibration is enabled but required crop matrix file is missing: ", matrix_file)
  }

  site_crop_matrix <- readRDS(matrix_file)
  if (is.null(site_crop_matrix) || nrow(site_crop_matrix) == 0) {
    stop("Crop calibration is enabled but crop matrix is empty: ", matrix_file)
  }

  log_function("Crop filtering ENABLED:\n")
  log_function("  Matrix file loaded with ", nrow(site_crop_matrix), " entries\n")
  log_function("  Target crop: ", paste(crop_names, collapse = ", "), "\n")

  list(
    crop_filter_enabled = TRUE,
    site_crop_matrix = site_crop_matrix,
    target_crop = crop_names
  )
}

#' Adaptive Observation Year Detection
#'
#' Determines observation years based on whether weighted aggregation is enabled.
#' For weighted aggregation, uses county-level observations; otherwise uses site-level.
#' Maintains backward compatibility with existing workflows.
#'
#' @param var_spec Variable specification row from output specification
#' @param site_metadata List containing site information (site_name, treatment_id)
#' @param aggregation_metadata Data frame with site-county mappings (if available)
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return Vector of observation years appropriate for the aggregation level
#' @export
#' @examples
#' \dontrun{
#' obs_years <- get_observation_years_adaptive(var_spec, site_meta, agg_meta, "data/obs")
#' }
get_observation_years_adaptive <- function(var_spec, site_metadata, aggregation_metadata = NULL,
                                         obs_data_dir, verbose = FALSE) {

  # Check if weighted mean aggregation is enabled for this variable
  weighted_agg_enabled <- !is.na(var_spec$weighted_mean_aggregation) &&
                         var_spec$weighted_mean_aggregation == TRUE

  if (weighted_agg_enabled && !is.null(aggregation_metadata)) {
    # County-level aggregation: use county FIPS for observation years
    if (verbose) cat("Using county-level observation detection for", var_spec$variable_name, "\n")

    # Get county FIPS code for this site
    site_name <- site_metadata$site_name
    county_row <- aggregation_metadata[aggregation_metadata$site_name == site_name, ]

    if (nrow(county_row) == 0) {
      if (verbose) cat("Warning: No aggregation metadata found for site", site_name, "\n")
      return(numeric(0))
    }

    county_fips <- county_row$aggregation_level[1]

    # Get observation years at county level
    return(get_county_observation_years(county_fips, var_spec, obs_data_dir, verbose))

  } else {
    # Site-level processing: use existing logic
    if (verbose) cat("Using site-level observation detection for", var_spec$variable_name, "\n")

    # Call existing function (must be available in the package)
    return(get_observation_years_generic(var_spec, site_metadata$treatment_id, obs_data_dir, verbose))
  }
}

#' Get Aggregation Metadata from Database
#'
#' Retrieves site-county-weight mappings from the database run order table
#' for use in weighted aggregation calculations.
#'
#' @param config Configuration list containing database settings
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param log_function Function for logging messages (default: cat)
#' @return Data frame with site_name, aggregation_level, aggregation_weight
#' @export
#' @examples
#' \dontrun{
#' # Get aggregation metadata with individual connection
#' agg_meta <- get_aggregation_metadata_from_database(config)
#'
#' # Get aggregation metadata with shared connection
#' shared_con <- get_shared_connection(config)
#' agg_meta <- get_aggregation_metadata_from_database(config, shared_con)
#' }
get_aggregation_metadata_from_database <- function(config, shared_connection = NULL, log_function = cat) {

  # Get appropriate connection (shared or individual)
  conn_info <- get_connection_for_operation(config, shared_connection, log_function)
  con <- conn_info$connection

  if (is.null(con)) {
    return(NULL)
  }

  # Only close connection if we created an individual one
  if (conn_info$should_close) {
    on.exit(DBI::dbDisconnect(con))
  }

  db_config <- config$file_source$database
  table_name <- db_config$tables$run_order %||% "run_order"

  tryCatch({
    # Get aggregation metadata for active runs
    query <- paste0("SELECT site_name, aggregation_level, aggregation_weight ",
                   "FROM ", table_name, " ",
                   "WHERE active = 1 ",
                   "ORDER BY aggregation_level, site_name")

    result <- DBI::dbGetQuery(con, query)

    if (nrow(result) == 0) {
      log_function("No active runs found for aggregation metadata\n")
      return(NULL)
    }

    log_function("Retrieved aggregation metadata for ", nrow(result), " sites\n")
    return(result)

  }, error = function(e) {
    log_function("Failed to retrieve aggregation metadata from database: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Aggregate Sites to County Level
#'
#' Aggregates site-level annual results to county level using weighted means.
#' Supports both standard aggregation and crop-aware filtering for accurate
#' county-level calibration based on actual crop-growing sites.
#'
#' @param site_annual_data Data frame with site-level annual results
#' @param aggregation_metadata Data frame with site-county-weight mappings
#' @param variable_name Name of the variable to aggregate
#' @param log_function Function for logging messages (default: cat)
#' @param crop_filter_enabled Logical indicating if crop-aware filtering should be applied
#' @param site_crop_matrix Data frame with site-crop-year mappings (required if crop_filter_enabled = TRUE)
#' @param target_crop Character string specifying target crop (required if crop_filter_enabled = TRUE)
#' @return Data frame with county-level aggregated results
#' @export
#' @examples
#' \dontrun{
#' # Standard county aggregation
#' county_results <- aggregate_sites_to_county(site_data, agg_meta, "cgrain")
#'
#' # Crop-aware county aggregation
#' county_results <- aggregate_sites_to_county(site_data, agg_meta, "cgrain",
#'                                            crop_filter_enabled = TRUE,
#'                                            site_crop_matrix = matrix,
#'                                            target_crop = "C6")
#' }
aggregate_sites_to_county <- function(site_annual_data, aggregation_metadata,
                                     variable_name, log_function = cat,
                                     crop_filter_enabled = FALSE,
                                     site_crop_matrix = NULL,
                                     target_crop = NULL) {

  # Visible diagnostics when log_function is a no-op (e.g. verbose = FALSE)
  agg_fail <- function(...) {
    msg <- paste0(...)
    log_function(msg)
    message(msg, appendLF = FALSE)
  }

  # Validate inputs
  if (is.null(site_annual_data) || nrow(site_annual_data) == 0) {
    agg_fail("No site annual data provided for aggregation\n")
    return(NULL)
  }

  if (is.null(aggregation_metadata) || nrow(aggregation_metadata) == 0) {
    agg_fail("No aggregation metadata provided\n")
    return(NULL)
  }

  # Check if data is in long format (has 'variable' column) or wide format (variable as column)
  is_long_format <- "variable" %in% names(site_annual_data)
  
  if (is_long_format) {
    # Long format: check if variable exists in the 'variable' column
    if (!variable_name %in% site_annual_data$variable) {
      agg_fail("Variable '", variable_name, "' not found in site annual data\n")
      return(NULL)
    }
    
    # Required columns for long format
    required_site_cols <- c("SiteID", "year", "SampleID", "variable", "d1")
    missing_site_cols <- required_site_cols[!required_site_cols %in% names(site_annual_data)]
    if (length(missing_site_cols) > 0) {
      agg_fail("Missing columns in site data: ", paste(missing_site_cols, collapse = ", "), "\n")
      return(NULL)
    }
    
    # Filter to specific variable and rename columns for consistency
    site_annual_data <- site_annual_data[site_annual_data$variable == variable_name, ]
    names(site_annual_data)[names(site_annual_data) == "SiteID"] <- "site_name"
    names(site_annual_data)[names(site_annual_data) == "d1"] <- variable_name
    
  } else {
    # Wide format: original logic
    if (!variable_name %in% names(site_annual_data)) {
      agg_fail("Variable '", variable_name, "' not found in site annual data\n")
      return(NULL)
    }
    
    # Required columns for wide format
    required_site_cols <- c("site_name", "year", "SampleID", variable_name)
    missing_site_cols <- required_site_cols[!required_site_cols %in% names(site_annual_data)]
    if (length(missing_site_cols) > 0) {
      agg_fail("Missing columns in site data: ", paste(missing_site_cols, collapse = ", "), "\n")
      return(NULL)
    }
  }

  required_agg_cols <- c("site_name", "aggregation_level", "aggregation_weight")
  missing_agg_cols <- required_agg_cols[!required_agg_cols %in% names(aggregation_metadata)]
  if (length(missing_agg_cols) > 0) {
    agg_fail("Missing columns in aggregation metadata: ", paste(missing_agg_cols, collapse = ", "), "\n")
    return(NULL)
  }

  tryCatch({
    # Merge site data with aggregation metadata
    merged_data <- merge(site_annual_data, aggregation_metadata, by = "site_name", all.x = FALSE, all.y = FALSE)

    if (nrow(merged_data) == 0) {
      agg_fail("No matching sites found between annual data and aggregation metadata\n")
      return(NULL)
    }

    log_function("Merged ", nrow(merged_data), " site records for county aggregation\n")

    # Remove rows with missing values
    complete_data <- merged_data[!is.na(merged_data[[variable_name]]) &
                                !is.na(merged_data$aggregation_weight) &
                                !is.na(merged_data$year) &
                                !is.na(merged_data$SampleID), ]

    if (nrow(complete_data) < nrow(merged_data)) {
      log_function("Removed ", nrow(merged_data) - nrow(complete_data),
                  " records with missing values\n")
    }

    if (nrow(complete_data) == 0) {
      agg_fail("No complete records available for county aggregation\n")
      return(NULL)
    }

    # Apply crop-aware filtering if enabled
    if (crop_filter_enabled) {
      if (is.null(site_crop_matrix) || is.null(target_crop)) {
        agg_fail("Error: crop_filter_enabled = TRUE but site_crop_matrix or target_crop is NULL\n")
        return(NULL)
      }

      initial_records <- nrow(complete_data)
      log_function("Applying crop-aware filtering for target crop(s): ",
                  paste(target_crop, collapse = ", "), "\n")

      # Match matrix rows using character site id and integer year (avoids factor/numeric mismatch)
      site_id_chr <- as.character(site_crop_matrix$site_id)
      year_mat <- as.integer(site_crop_matrix$year)

      # Filter to include only sites that grew the target crop in each year
      crop_filtered_data <- list()
      n_no_matrix_row <- 0L
      n_matrix_false <- 0L

      for (i in 1:nrow(complete_data)) {
        row <- complete_data[i, ]
        site_name <- as.character(row$site_name)
        year <- as.integer(row$year)

        idx <- site_id_chr == site_name & year_mat == year
        crop_row <- site_crop_matrix[idx, , drop = FALSE]

        if (nrow(crop_row) == 0) {
          n_no_matrix_row <- n_no_matrix_row + 1L
          next
        }
        if (!any(crop_row$has_target_crop)) {
          n_matrix_false <- n_matrix_false + 1L
          next
        }
        crop_filtered_data[[length(crop_filtered_data) + 1]] <- row
      }

      if (length(crop_filtered_data) == 0) {
        agg_fail(
          "Crop filter removed all rows (", initial_records, " site-years). ",
          "No matrix match: ", n_no_matrix_row, "; matrix has has_target_crop=FALSE: ",
          n_matrix_false, ". Target crop(s): ",
          paste(target_crop, collapse = ", "),
          ". Rebuild site_crop_year_matrix.rds after GSA step 1 if you added multi-crop config.\n"
        )
        return(NULL)
      }

      # Combine filtered data
      complete_data <- do.call(rbind, crop_filtered_data)
      rownames(complete_data) <- NULL

      final_records <- nrow(complete_data)
      filtered_out <- initial_records - final_records
      log_function("Crop filtering: kept ", final_records, " records, filtered out ",
                  filtered_out, " records (", round(100 * filtered_out / initial_records, 1), "%)\n")
    }

    # Apply zero-yield filter for crop variables (removes model failures)
    # crop_yield_vars <- c("cgrain", "yield", "grain_yield")
    # if (crop_filter_enabled && variable_name %in% crop_yield_vars) {
    #  initial_records <- nrow(complete_data)

      # Filter out zero yields and NA values (model failures when crop was grown)
    #  complete_data <- complete_data[!is.na(complete_data[[variable_name]]) &
    #                                  complete_data[[variable_name]] > 0, ]

    #  zero_filtered_out <- initial_records - nrow(complete_data)
    #  if (zero_filtered_out > 0) {
    #    log_function("Zero-yield filtering: removed ", zero_filtered_out,
    #                " model failures (", round(100 * zero_filtered_out / initial_records, 2), "%)\n")
    #  }

    #  if (nrow(complete_data) == 0) {
    #    log_function("No records remaining after zero-yield filtering\n")
    #    return(NULL)
    #  }
    # }

    # Group by county + year + sample and calculate weighted means
    county_results <- list()

    # Get unique combinations of aggregation_level, year, and SampleID
    unique_combos <- unique(complete_data[c("aggregation_level", "year", "SampleID")])

    for (i in 1:nrow(unique_combos)) {
      combo <- unique_combos[i, ]
      county_fips <- combo$aggregation_level
      year <- combo$year
      sample_id <- combo$SampleID

      # Get data for this county-year-sample combination
      county_year_data <- complete_data[
        complete_data$aggregation_level == county_fips &
        complete_data$year == year &
        complete_data$SampleID == sample_id,
      ]

      if (nrow(county_year_data) > 0) {
        # Calculate weighted mean
        if (sum(county_year_data$aggregation_weight, na.rm = TRUE) == 0) {
          log_function("Warning: All weights are zero for county ", county_fips,
                      " year ", year, " sample ", sample_id, "\n")
          weighted_value <- NA
        } else {
          weighted_value <- weighted.mean(
            county_year_data[[variable_name]],
            county_year_data$aggregation_weight,
            na.rm = TRUE
          )
        }

        # Create result row (convert integer64 to regular integer to avoid corruption)
        result_row <- data.frame(
          aggregation_level = as.integer(county_fips),
          year = year,
          variable_name = variable_name,
          weighted_value = weighted_value,
          n_sites = nrow(county_year_data),
          total_weight = sum(county_year_data$aggregation_weight, na.rm = TRUE),
          sample_id = sample_id,
          crop_filtered = crop_filter_enabled,
          target_crop = if (crop_filter_enabled) {
            paste(target_crop, collapse = ",")
          } else {
            NA_character_
          },
          stringsAsFactors = FALSE
        )

        county_results[[length(county_results) + 1]] <- result_row
      }
    }

    # Combine all results
    if (length(county_results) > 0) {
      final_results <- do.call(rbind, county_results)
      rownames(final_results) <- NULL

      crop_filter_msg <- if (crop_filter_enabled) {
        paste(" (crop-filtered for", paste(target_crop, collapse = ","), ")")
      } else {
        ""
      }
      log_function("Successfully aggregated ", variable_name, " to ",
                  length(unique(final_results$aggregation_level)), " counties across ",
                  length(unique(final_results$year)), " years", crop_filter_msg, "\n")

      return(final_results)
    } else {
      agg_fail("No county aggregation results generated\n")
      return(NULL)
    }

  }, error = function(e) {
    agg_fail("Error in county aggregation: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Determine Cleanup Policy from Output Specification
#'
#' Analyzes output specification to determine whether annual results should be
#' deleted after weighted aggregation based on configuration.
#'
#' @param output_spec Data frame of variable rows, or the full list returned by
#'   \code{load_output_specifications()} (uses \code{$variables}).
#' @return List with cleanup policy decisions
#' @export
#' @examples
#' \dontrun{
#' policy <- determine_cleanup_policy(output_spec)
#' }
determine_cleanup_policy <- function(output_spec) {

  if (!inherits(output_spec, "data.frame") && is.list(output_spec) &&
      !is.null(output_spec$variables)) {
    output_spec <- output_spec$variables
  }
  if (!inherits(output_spec, "data.frame")) {
    stop(
      "determine_cleanup_policy: pass output_specs$variables or the full list from load_output_specifications()",
      call. = FALSE
    )
  }

  # Find variables with weighted aggregation enabled
  wma <- if ("weighted_mean_aggregation" %in% names(output_spec)) {
    output_spec$weighted_mean_aggregation
  } else {
    rep(NA, nrow(output_spec))
  }
  weighted_vars <- output_spec[!is.na(wma) & wma == TRUE, , drop = FALSE]

  if (nrow(weighted_vars) == 0) {
    return(list(
      has_weighted_aggregation = FALSE,
      delete_annual = FALSE,
      variables_to_aggregate = character(0)
    ))
  }

  # Check if any weighted variables have annual_output = FALSE
  # If so, we should delete annual results after aggregation for those variables
  delete_annual <- any(!is.na(weighted_vars$annual_output) &
                      weighted_vars$annual_output == FALSE)

  # Get list of variables that need weighted aggregation
  variables_to_aggregate <- weighted_vars$variable_name

  return(list(
    has_weighted_aggregation = TRUE,
    delete_annual = delete_annual,
    variables_to_aggregate = variables_to_aggregate,
    weighted_vars = weighted_vars
  ))
}

#' Process Weighted Mean Aggregation for Job Group
#'
#' Main orchestrator function for processing weighted mean aggregation.
#' Reads site-level annual data, aggregates to county level, and optionally
#' cleans up annual files based on configuration.
#'
#' @param config Configuration list
#' @param method_dir Path to GSA/SIR method directory
#' @param job_group Job group identifier
#' @param output_spec Data frame with output variable specifications
#' @param aggregation_metadata Data frame with site-county mappings
#' @param target_crop Target crop name(s) for crop-aware filtering
#' @param log_function Function for logging messages (default: cat)
#' @return Integer status code: 0 = success, 1 = failure
#' @export
#' @examples
#' \dontrun{
#' status <- process_weighted_mean_aggregation(config, method_dir, 1, output_spec, agg_meta,
#'                                            target_crop = "C6")
#' }
process_weighted_mean_aggregation <- function(config, method_dir, job_group, output_spec,
                                            aggregation_metadata, target_crop = NULL,
                                            log_function = cat) {

  log_function("Processing weighted mean aggregation for job group ", job_group, "\n")

  tryCatch({
    # Determine cleanup policy
    cleanup_policy <- determine_cleanup_policy(output_spec)

    if (!cleanup_policy$has_weighted_aggregation) {
      log_function("No variables configured for weighted mean aggregation\n")
      return(0)
    }

    log_function("Processing ", length(cleanup_policy$variables_to_aggregate),
                " variables for weighted mean aggregation\n")

    # Construct path to matrix file
    matrix_file <- file.path(method_dir, "site_crop_year_matrix.rds")
    crop_filter_context <- resolve_weighted_crop_filter_context(
      config = config,
      target_crop = target_crop,
      matrix_file = matrix_file,
      log_function = log_function
    )
    site_crop_matrix <- crop_filter_context$site_crop_matrix
    target_crop <- crop_filter_context$target_crop
    crop_filter_enabled <- crop_filter_context$crop_filter_enabled

    # Setup paths
    job_group_suffix <- paste0("jobGroup_", job_group)
    annual_dir <- file.path(method_dir, config$output_dirs$annual_outputs, job_group_suffix)
    weighted_dir <- file.path(method_dir, config$output_dirs$weighted_mean_outputs, job_group_suffix)

    # Check if annual outputs exist
    if (!dir.exists(annual_dir)) {
      log_function("Annual outputs directory not found: ", annual_dir, "\n")
      return(1)
    }

    # Find all annual output files
    annual_files <- list.files(annual_dir, pattern = "\\.rds$", full.names = TRUE)

    if (length(annual_files) == 0) {
      log_function("No annual output files found for aggregation in ", annual_dir, "\n")
      return(1)
    }

    log_function("Found ", length(annual_files), " annual output files to process\n")

    # Process each variable requiring weighted aggregation
    aggregation_success <- TRUE

    for (var_name in cleanup_policy$variables_to_aggregate) {
      log_function("Processing variable: ", var_name, "\n")

      # Collect all annual data for this variable across all sites
      all_site_data <- list()

      for (annual_file in annual_files) {
        annual_data <- readRDS(annual_file)

        # Check if the variable exists in the annual data
        if (var_name %in% names(annual_data)) {
          # Add site identifier if not present
          if (!"site_name" %in% names(annual_data)) {
            # Extract site name from filename (assuming pattern includes site name)
            site_name <- gsub(".*site_([^_]+).*", "\\1", basename(annual_file))
            annual_data$site_name <- site_name
          }

          # Filter data for this variable (only keep relevant columns)
          var_data <- annual_data[, c("site_name", "year", "SampleID", var_name)]
          all_site_data[[length(all_site_data) + 1]] <- var_data
        }
      }

      if (length(all_site_data) == 0) {
        log_function("No annual data found for variable: ", var_name, "\n")
        next
      }

      # Combine all site data
      combined_site_data <- do.call(rbind, all_site_data)

      # Aggregate to county level
      county_results <- aggregate_sites_to_county(
        site_annual_data = combined_site_data,
        aggregation_metadata = aggregation_metadata,
        variable_name = var_name,
        log_function = log_function,
        crop_filter_enabled = crop_filter_enabled,
        site_crop_matrix = site_crop_matrix,
        target_crop = target_crop
      )

      if (!is.null(county_results)) {
        # Save county-level results
        output_filename <- paste0("weighted_", var_name, "_jobGroup_", job_group, ".rds")
        output_path <- file.path(weighted_dir, output_filename)

        saveRDS(county_results, output_path)
        log_function("Saved weighted aggregation: ", output_filename, "\n")
      } else {
        log_function("Failed to aggregate variable: ", var_name, "\n")
        aggregation_success <- FALSE
      }
    }

    # Cleanup annual files if configured
    if (cleanup_policy$delete_annual && aggregation_success) {
      log_function("Cleaning up annual output files (delete_annual = TRUE)\n")
      for (annual_file in annual_files) {
        unlink(annual_file)
      }
      log_function("Deleted ", length(annual_files), " annual output files\n")
    }

    if (aggregation_success) {
      log_function("Completed weighted mean aggregation for job group ", job_group, "\n")
      return(0)
    } else {
      log_function("Some aggregation operations failed for job group ", job_group, "\n")
      return(1)
    }

  }, error = function(e) {
    log_function("Error in weighted mean aggregation: ", conditionMessage(e), "\n")
    return(1)
  })
}

#' Process Job Group Weighted Mean Aggregation
#'
#' Main entry point for processing weighted mean aggregation at the job group level.
#' Called after all samples in a job group have completed their annual processing.
#'
#' @param config Configuration list
#' @param method_dir Path to GSA/SIR method directory
#' @param job_group Job group identifier
#' @param log_function Function for logging messages (default: cat)
#' @return Integer status code: 0 = success, 1 = failure
#' @export
#' @examples
#' \dontrun{
#' status <- process_job_group_weighted_aggregation(config, method_dir, 1)
#' }
process_job_group_weighted_aggregation <- function(config, method_dir, job_group,
                                                   target_crop = NULL, log_function = cat) {

  log_function("Processing job group weighted mean aggregation for job group ", job_group, "\n")

  tryCatch({
    # Load output specifications
    output_specs <- load_output_specifications(config, verbose = FALSE)
    if (is.null(output_specs)) {
      log_function("Failed to load output specifications\n")
      return(1)
    }

    # Check if weighted aggregation is needed
    cleanup_policy <- determine_cleanup_policy(output_specs$variables)

    if (!cleanup_policy$has_weighted_aggregation) {
      log_function("No variables configured for weighted mean aggregation\n")
      return(0)
    }

    # Get aggregation metadata from database
    aggregation_metadata <- get_aggregation_metadata_from_database(config, log_function)
    if (is.null(aggregation_metadata)) {
      log_function("Failed to retrieve aggregation metadata\n")
      return(1)
    }

    # Process weighted aggregation
    status <- process_weighted_mean_aggregation(
      config = config,
      method_dir = method_dir,
      job_group = job_group,
      output_spec = output_specs$variables,
      aggregation_metadata = aggregation_metadata,
      target_crop = target_crop,
      log_function = log_function
    )

    return(status)

  }, error = function(e) {
    log_function("Error in job group weighted aggregation: ", conditionMessage(e), "\n")
    return(1)
  })
}

#' Process Weighted Mean Aggregation for Single Sample
#'
#' Processes weighted mean aggregation for a single sample's annual results.
#' Called during individual sample processing in GSA/SIR Step 2.
#'
#' @param config Configuration list
#' @param target_crop Target crop name
#' @param annual_results Data frame with site-level annual results for this sample
#' @param sample_id Sample identifier
#' @param job_group Job group identifier
#' @param weighted_output_dir Directory for weighted output files
#' @param output_specs Data frame with output variable specifications
#' @param verbose Logical indicating whether to print progress messages
#' @return Integer status code: 0 = success, 1 = failure
#' @export
process_weighted_mean_aggregation_for_sample <- function(config, target_crop, annual_results, sample_id,
                                                       job_group, weighted_output_dir,
                                                       output_specs, method, verbose = FALSE) {

  # Check if weighted aggregation is needed
  cleanup_policy <- determine_cleanup_policy(output_specs)

  if (!cleanup_policy$has_weighted_aggregation) {
    return(0)  # No weighted aggregation needed
  }

  if (verbose) cat("Processing weighted aggregation for sample", sample_id, "\n")

  tryCatch({
    # Get aggregation metadata from database
    aggregation_metadata <- get_aggregation_metadata_from_database(config,
                                                                  log_function = if(verbose) cat else function(...) NULL)

    if (is.null(aggregation_metadata)) {
      stop("Failed to retrieve aggregation metadata for sample ", sample_id)
    }

    # Construct path to matrix file
    results_dir <- dirname(dirname(weighted_output_dir))  # Go up from jobGroup_X/weighted_outputs to method_dir
    matrix_file <- file.path(results_dir, "site_crop_year_matrix.rds")
    crop_filter_context <- resolve_weighted_crop_filter_context(
      config = config,
      matrix_file = matrix_file,
      target_crop = target_crop,
      log_function = if (verbose) cat else function(...) NULL
    )
    site_crop_matrix <- crop_filter_context$site_crop_matrix
    target_crop <- crop_filter_context$target_crop
    crop_filter_enabled <- crop_filter_context$crop_filter_enabled

    # Process each variable requiring weighted aggregation
    for (var_name in cleanup_policy$variables_to_aggregate) {
      # Check if variable exists in the data (long format check)
      if ("variable" %in% names(annual_results) && var_name %in% annual_results$variable) {

        # Aggregate this variable to county level
        county_results <- aggregate_sites_to_county(
          site_annual_data = annual_results,
          aggregation_metadata = aggregation_metadata,
          variable_name = var_name,
          log_function = if(verbose) cat else function(...) NULL,
          crop_filter_enabled = crop_filter_enabled,
          site_crop_matrix = site_crop_matrix,
          target_crop = target_crop
        )

        if (!is.null(county_results)) {
          # Save county-level results for this sample

          if(config$file_source$database$database_result$run_type == "file_system") {
            
            output_filename <- paste0("weighted_", var_name, "_sample_", sample_id, ".rds")
            output_path <- file.path(weighted_output_dir, output_filename)

            saveRDS(county_results, output_path)
            if (verbose) cat("Saved weighted aggregation for sample", sample_id, "variable", var_name, "\n")

          } else if(config$file_source$database$database_result$run_type == "database") {
                county_results$job_group <- job_group
                county_results$simulation_id <- sample_id

                cred_file <- path.expand(config$file_source$database$cred_file)
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
                db_calib   = config$file_source$database$database_result$database

                dbConn_result <- DBI::dbConnect(
                    RMariaDB::MariaDB(),
                    host = host, dbname = db_calib,
                    username = user, password = password
                )
                  
                if (method == "GSA") {
                  results_weighted = config$file_source$database$database_result$tables$gsa_results_weighted
                } else if (method == "SIR") {
                  results_weighted = config$file_source$database$database_result$tables$sir_results_weighted
                } else {
                  stop("Cannot determine weighted results table from annual_table: ", annual_table)
                }

                on.exit(DBI::dbDisconnect(dbConn_result), add = TRUE)
                DBI::dbWriteTable(dbConn_result, results_weighted, county_results, append = TRUE, row.names = FALSE)
                DBI::dbDisconnect(dbConn_result)

                if (verbose) cat("Saved weighted aggregation for sample", sample_id, "variable", var_name, "to database\n")
              }
            }
          }
        }
    return(0)
  }, error = function(e) {
    stop("Error in sample-level weighted aggregation: ", conditionMessage(e))
  })
}
# Utility function for null coalescing operator (if not already defined)
if (!exists("%||%")) {
  `%||%` <- function(lhs, rhs) {
    if (!is.null(lhs) && length(lhs) > 0 && !is.na(lhs) && lhs != "") lhs else rhs
  }
}
