get_runfile_site_id_column <- function(runfile) {
  if ("site_name" %in% colnames(runfile)) {
    return("site_name")
  }
  if ("siteID" %in% colnames(runfile)) {
    return("siteID")
  }
  if ("site_id" %in% colnames(runfile)) {
    return("site_id")
  }
  stop("RunFile must contain a site identifier column ('site_name', 'siteID', or 'site_id')")
}

get_runfile_treatment_schedule_column <- function(runfile) {
  if ("treatment_schedule" %in% colnames(runfile)) {
    return("treatment_schedule")
  }
  if ("TreatmentID" %in% colnames(runfile)) {
    return("TreatmentID")
  }
  NULL
}

normalize_target_crop_names <- function(target_crop) {
  crop_names <- trimws(as.character(target_crop))
  crop_names <- crop_names[!is.na(crop_names) & nzchar(crop_names)]
  unique(crop_names)
}

#' Resolve target crop name(s) for a GSA or SIR pipeline
#'
#' Prefers pipeline-specific config (`gsa.crop.name` / `sir.crop.name`).
#' Falls back to legacy `daycent.crop.name` for older configs.
#'
#' @param config List containing configuration settings
#' @param pipeline Either `"gsa"` or `"sir"`
#' @return Character vector of normalized crop names (may be empty)
#' @keywords internal
get_pipeline_target_crop <- function(config, pipeline = c("gsa", "sir")) {
  pipeline <- match.arg(pipeline)

  crop_name <- if (identical(pipeline, "gsa")) {
    config$gsa$crop$name
  } else {
    config$sir$crop$name
  }

  crop_names <- normalize_target_crop_names(crop_name)
  if (length(crop_names) > 0) {
    return(crop_names)
  }

  # Legacy fallback for configs that still use daycent.crop.name
  normalize_target_crop_names(config$daycent$crop$name)
}

get_crop_detection_schedule_from_database <- function(config, site_name,
                                                      shared_connection = NULL,
                                                      log_function = cat) {
  conn_info <- get_connection_for_operation(config, shared_connection, log_function)
  con <- conn_info$connection

  if (is.null(con)) {
    stop("Database connection unavailable while fetching crop-detection schedule for site ", site_name)
  }

  if (conn_info$should_close) {
    on.exit(DBI::dbDisconnect(con))
  }

  db_config <- config$file_source$database
  table_name <- db_config$tables$schedule_files
  if (is.null(table_name) || !nzchar(table_name)) {
    table_name <- "schedule_files"
  }

  query <- paste0(
    "SELECT schedule_file_data ",
    "FROM ", table_name, " ",
    "WHERE site_name = ? AND (treatment_name IS NULL OR TRIM(treatment_name) = '')"
  )

  result <- DBI::dbGetQuery(con, query, params = list(site_name))

  if (nrow(result) == 0) {
    stop("No treatment schedule row found in ", table_name, " for site ", site_name)
  }

  if (nrow(result) > 1) {
    stop(
      "Expected exactly one treatment schedule row in ", table_name,
      " for site ", site_name, " but found ", nrow(result)
    )
  }

  result$schedule_file_data[1]
}

get_crop_detection_schedule_content <- function(config, site_name, treatment_schedule = NULL,
                                                shared_connection = NULL, log_function = cat) {
  if (!is.null(config$file_source$mode) && config$file_source$mode == "database") {
    return(
      get_crop_detection_schedule_from_database(
        config = config,
        site_name = site_name,
        shared_connection = shared_connection,
        log_function = log_function
      )
    )
  }

  if (is.null(treatment_schedule) || is.na(treatment_schedule) || !nzchar(trimws(treatment_schedule))) {
    stop("RunFile treatment_schedule is required for filesystem crop detection at site ", site_name)
  }

  expsites_dir <- config$paths$expsites_dir
  if (is.null(expsites_dir) || !nzchar(expsites_dir)) {
    stop("config$paths$expsites_dir must be set for filesystem crop detection")
  }

  if (!grepl("^/", expsites_dir)) {
    lairice_root <- config$paths$lairice_root
    if (is.null(lairice_root) || !nzchar(lairice_root)) {
      stop("config$paths$lairice_root must be set when expsites_dir is relative")
    }
    expsites_dir <- file.path(lairice_root, expsites_dir)
  }

  schedule_path <- file.path(expsites_dir, site_name, treatment_schedule)
  if (!file.exists(schedule_path)) {
    stop("Treatment schedule file not found for site ", site_name, ": ", schedule_path)
  }

  paste(readLines(schedule_path, warn = FALSE), collapse = "\n")
}

#' Build Site-Crop-Year Matrix for Crop-Aware Aggregation
#'
#' Builds a comprehensive lookup matrix during Step 1 setup that maps each site
#' to the years in which it grew the target crop. This enables efficient
#' crop-aware weighted aggregation during Step 2 cluster execution.
#'
#' @param config List containing configuration including database settings and target crop
#' @param runfile Data frame containing site information (must have site_name column)
#' @param target_crop Character string specifying the crop to search for (e.g., "C6")
#' @param shared_connection Optional shared database connection for efficiency
#' @param verbose Logical indicating whether to print progress messages
#'
#' @return Data frame with columns:
#'   - site_id: Site identifier (same as site_name)
#'   - year: Calendar year
#'   - has_target_crop: Logical indicating if target crop was planted at this site in this year
#'
#' @details
#' This function processes all sites in the RunFile and retrieves their treatment
#' schedule files from the database. It then parses each schedule to determine
#' which years each site grew the target crop.
#'
#' The resulting matrix is designed to be saved during Step 1 setup and loaded
#' during Step 2 cluster execution for fast crop-specific filtering during
#' county-level weighted aggregation.
#'
#' Progress is reported every 50 sites to monitor processing on large datasets.
#'
#' @examples
#' \dontrun{
#' # Build matrix for corn (C6) calibration
#' config <- yaml::read_yaml("config.yaml")
#' runfile <- read.csv("RunFile.csv")
#' matrix <- build_site_crop_year_matrix(config, runfile, "C6", verbose = TRUE)
#'
#' # Save for use in Step 2
#' saveRDS(matrix, "site_crop_year_matrix.rds")
#' }
#'
#' @export
build_site_crop_year_matrix <- function(config, runfile, target_crop, shared_connection = NULL, verbose = FALSE) {

  site_col <- get_runfile_site_id_column(runfile)
  schedule_col <- get_runfile_treatment_schedule_column(runfile)
  target_crop <- normalize_target_crop_names(target_crop)

  if (length(target_crop) == 0) {
    stop("target_crop must be specified and non-empty")
  }

  if (is.null(schedule_col) && (is.null(config$file_source$mode) || config$file_source$mode != "database")) {
    stop("RunFile must contain treatment_schedule metadata for filesystem crop detection")
  }

  runfile_lookup <- unique(runfile[, unique(c(site_col, schedule_col)), drop = FALSE])
  names(runfile_lookup)[names(runfile_lookup) == site_col] <- "site_id"
  if (!is.null(schedule_col) && schedule_col %in% names(runfile_lookup)) {
    names(runfile_lookup)[names(runfile_lookup) == schedule_col] <- "treatment_schedule"
  } else {
    runfile_lookup$treatment_schedule <- NA_character_
  }

  schedules_per_site <- tapply(
    trimws(as.character(runfile_lookup$treatment_schedule)),
    runfile_lookup$site_id,
    function(vals) {
      vals <- vals[!is.na(vals) & nzchar(vals)]
      length(unique(vals))
    }
  )
  bad_sites <- names(schedules_per_site)[schedules_per_site > 1]
  if (length(bad_sites) > 0) {
    stop(
      "Expected exactly one treatment_schedule per site for crop calibration. ",
      "Sites with multiple schedules: ", paste(bad_sites, collapse = ", ")
    )
  }

  total_sites <- nrow(runfile_lookup)

  if (verbose) {
    cat("Building site-crop-year matrix for", total_sites, "sites and target crop(s):",
        paste(target_crop, collapse = ", "), "\n")
  }

  # Initialize results list
  all_results <- list()

  # Process each site
  for (i in seq_len(nrow(runfile_lookup))) {
    site <- runfile_lookup$site_id[i]
    treatment_schedule <- runfile_lookup$treatment_schedule[i]

    if (verbose && (i %% 50 == 0 || i == 1 || i == total_sites)) {
      cat("Processing site", i, "of", total_sites, ":", site, "\n")
    }

    schedule_content <- tryCatch(
      get_crop_detection_schedule_content(
        config = config,
        site_name = site,
        treatment_schedule = treatment_schedule,
        shared_connection = shared_connection,
        log_function = function(...) {}  # Suppress individual file messages
      ),
      error = function(e) {
        stop("Failed to load crop-detection schedule for site ", site, ": ", conditionMessage(e))
      }
    )

    # Parse schedule for crop information
    crop_years <- tryCatch(
      parse_schedule_for_crops(schedule_content, target_crop, verbose = FALSE),
      error = function(e) {
        stop("Failed to parse crop-detection schedule for site ", site, ": ", conditionMessage(e))
      }
    )

    if (nrow(crop_years) > 0) {
      # Add site information to results
      site_results <- data.frame(
        site_id = site,
        year = crop_years$year,
        has_target_crop = crop_years$has_target_crop,
        stringsAsFactors = FALSE
      )

      all_results[[length(all_results) + 1]] <- site_results
    } else {
      stop("No crop events found in crop-detection schedule for site ", site)
    }
  }

  if (length(all_results) == 0) {
    stop("No sites had crop information available for crop-detection matrix generation")
  }

  # Combine all results
  matrix_data <- do.call(rbind, all_results)

  # Sort by site and year for efficient lookups
  matrix_data <- matrix_data[order(matrix_data$site_id, matrix_data$year), ]
  rownames(matrix_data) <- NULL

  if (verbose) {
    total_entries <- nrow(matrix_data)
    sites_with_data <- length(unique(matrix_data$site_id))
    years_covered <- range(matrix_data$year)
    target_entries <- sum(matrix_data$has_target_crop)

    cat("Matrix building complete:\n")
    cat("  Total entries:", total_entries, "\n")
    cat("  Sites with data:", sites_with_data, "out of", total_sites, "\n")
    cat("  Years covered:", years_covered[1], "to", years_covered[2], "\n")
    cat("  Entries with target crop:", target_entries, "(",
        round(100 * target_entries / total_entries, 1), "% )\n")
  }

  return(matrix_data)
}


#' Check if Crop Calibration is Enabled
#'
#' Determines if crop-aware weighted aggregation should be activated based on
#' configuration settings and parameter specifications.
#'
#' @param config List containing configuration settings
#' @param target_crop Character vector of target crop name(s). Prefer values from
#'   `gsa.crop.name` / `sir.crop.name` (or legacy `daycent.crop.name`).
#'
#' @return Logical indicating if crop calibration mode should be enabled
#'
#' @details
#' The function checks three criteria:
#' 1. `target_crop` is non-empty
#' 2. Crop parameters are present in prior file specifications
#' 3. Weighted mean aggregation is enabled for crop variables
#'
#' All three must be TRUE for crop calibration to be enabled.
#'
#' @examples
#' \dontrun{
#' config <- yaml::read_yaml("crop_config.yaml")
#' target_crop <- config$gsa$crop$name
#' if (crop_calibration_enabled(config, target_crop)) {
#'   cat("Crop-aware aggregation will be used\n")
#' }
#' }
#'
#' @export
crop_calibration_enabled <- function(config, target_crop) {

  # Check if crop name is specified (allow vector of crop names)
  crop_names <- normalize_target_crop_names(target_crop)
  has_crop_config <- length(crop_names) > 0

  if (!has_crop_config) {
    return(FALSE)
  }

  # Check if crop parameters are present in prior files
  has_crop_params <- FALSE

  # Check for prior files in multiple possible locations
  prior_file_content <- NULL

  # Method 1: parameters.prior_files (old structure)
  if ("parameters" %in% names(config) && "prior_files" %in% names(config$parameters)) {
    prior_files <- config$parameters$prior_files
    has_crop_params <- any(grepl("crop\\.100", names(prior_files), ignore.case = TRUE))
  }

  # Method 2: input_files.prior_file (current structure)
  if (!has_crop_params && "input_files" %in% names(config) && "prior_file" %in% names(config$input_files)) {
    prior_file_path <- config$input_files$prior_file
    if (file.exists(prior_file_path)) {
      tryCatch({
        prior_content <- read.csv(prior_file_path, stringsAsFactors = FALSE)
        # Check if any parameters are crop.100 parameters
        if ("File" %in% colnames(prior_content)) {
          has_crop_params <- any(grepl("crop\\.100", prior_content$File, ignore.case = TRUE))
        }
      }, error = function(e) {
        # If we can't read the prior file, assume FALSE
        has_crop_params <- FALSE
      })
    }
  }

  if (!has_crop_params) {
    return(FALSE)
  }

  # Check if weighted mean aggregation is enabled for crop variables
  has_crop_weighted_agg <- FALSE
  if ("model_outputs" %in% names(config) &&
      "output_variables_spec" %in% names(config$model_outputs)) {

    tryCatch({
      var_spec_file <- config$model_outputs$output_variables_spec
      if (file.exists(var_spec_file)) {
        var_spec <- read.csv(var_spec_file, stringsAsFactors = FALSE)

        # Look for crop yield variables with weighted mean aggregation enabled
        crop_vars <- c("cgrain", "yield", "grain_yield")  # Common crop yield variable names

        has_crop_weighted_agg <- any(
          var_spec$variable_name %in% crop_vars &
          var_spec$weighted_mean_aggregation == TRUE
        )
      }
    }, error = function(e) {
      # If we can't read the spec file, assume FALSE
      has_crop_weighted_agg <- FALSE
    })
  }

  return(has_crop_config && has_crop_params && has_crop_weighted_agg)
}
