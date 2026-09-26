#' Calendar period start for 14-day bins
#'
#' @param dates Date vector.
#' @return Date vector of period starts (Jan 1 + 14-day blocks).
#' @export
calendar_period_start <- function(dates) {
  dates <- as.Date(dates)
  years <- as.integer(format(dates, "%Y"))
  doy <- as.integer(format(dates, "%j"))
  as.Date(paste0(years, "-01-01")) + 14L * floor((doy - 1L) / 14L)
}

#' Build calendar dates from simulation year and day-of-year columns
#'
#' @param years Integer vector of simulation years.
#' @param doy Integer vector of day of year.
#' @return Date vector.
#' @export
dates_from_year_doy <- function(years, doy) {
  years <- as.integer(years)
  doy <- as.integer(doy)
  as.Date(doy - 1L, origin = paste0(years, "-01-01"))
}

#' Extract day-of-year column from DayCent daily output tables
#'
#' @param file_data Data frame from a DayCent .out file.
#' @return Integer day-of-year vector.
#' @keywords internal
extract_doy_column <- function(file_data) {
  day_cols <- intersect(names(file_data), c("day", "doy", "Day", "DOY"))
  if (length(day_cols) == 0) {
    stop("No day-of-year column found in daily output file")
  }
  as.integer(file_data[[day_cols[[1]]]])
}

#' Get observation period starts for a site/treatment
#'
#' @param var_spec Variable specification row from output CSV.
#' @param site_id Site identifier.
#' @param trt_sch_file Treatment schedule file name.
#' @param obs_data_dir Directory containing observation CSV files.
#' @param verbose Logical.
#' @return Date vector of period_start values present in observations.
#' @export
get_observation_periods_generic <- function(var_spec, site_id, trt_sch_file,
                                            obs_data_dir, verbose = FALSE) {
  if (is.na(var_spec$observation_file) || var_spec$observation_file == "") {
    if (verbose) {
      cat("No observation file specified for", var_spec$variable_name, "\n")
    }
    return(as.Date(character()))
  }

  obs_file_path <- file.path(obs_data_dir, var_spec$observation_file)
  if (!file.exists(obs_file_path)) {
    if (verbose) cat("Observation file not found:", obs_file_path, "\n")
    return(as.Date(character()))
  }

  obs_data <- read.csv(obs_file_path, stringsAsFactors = FALSE)
  site_col <- if ("site_ID" %in% names(obs_data)) {
    "site_ID"
  } else if ("siteID" %in% names(obs_data)) {
    "siteID"
  } else {
    stop("Observation file missing site_ID/siteID column: ", obs_file_path)
  }

  period_col <- var_spec$observation_year_columns
  if (is.na(period_col) || period_col == "") {
    stop("observation_year_columns must name the period_start column for period outputs")
  }
  if (!period_col %in% names(obs_data)) {
    stop("Observation file missing period column ", period_col, ": ", obs_file_path)
  }

  obs_data <- obs_data[obs_data[[site_col]] == site_id, , drop = FALSE]
  if (!is.na(var_spec$observation_match_column) && var_spec$observation_match_column != "") {
    match_col <- var_spec$observation_match_column
    if (!match_col %in% names(obs_data)) {
      stop("Observation file missing match column ", match_col)
    }
    obs_data <- obs_data[obs_data[[match_col]] == trt_sch_file, , drop = FALSE]
  }

  sort(unique(as.Date(obs_data[[period_col]])))
}

#' Aggregate daily model output to 14-day period totals
#'
#' @param file_data Daily model output data frame.
#' @param var_name Variable column to sum within each period.
#' @param obs_periods Date vector restricting output periods.
#' @return Data frame with period_start, year, period_total.
#' @export
aggregate_daily_to_period_totals <- function(file_data, var_name, obs_periods) {
  if (nrow(file_data) == 0) {
    return(data.frame())
  }
  if (!var_name %in% names(file_data)) {
    stop("Variable ", var_name, " not found in model output data")
  }
  if (!"year" %in% names(file_data)) {
    stop("Model output data missing year column")
  }

  doy <- extract_doy_column(file_data)
  dates <- dates_from_year_doy(file_data$year, doy)
  period_start <- calendar_period_start(dates)

  daily_df <- data.frame(
    year = as.integer(format(dates, "%Y")),
    period_start = period_start,
    value = as.numeric(file_data[[var_name]]),
    stringsAsFactors = FALSE
  )

  period_df <- aggregate(
    value ~ year + period_start,
    data = daily_df,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  names(period_df)[names(period_df) == "value"] <- "period_total"

  if (length(obs_periods) > 0) {
    period_df <- period_df[period_df$period_start %in% as.Date(obs_periods), , drop = FALSE]
  }

  period_df
}

#' Process period output variables in batch
#'
#' @inheritParams process_annual_outputs_batch
#' @return List with period_results data frame and num_obs_periods count.
#' @export
process_period_outputs_batch <- function(all_file_data, period_vars, metadata, config,
                                         agg_metadata = NULL, verbose = FALSE) {
  period_results <- NULL
  num_obs_periods <- 0
  obs_data_dir <- file.path(config$paths$lairice_root, config$model_outputs$observation_data_dir)

  if (verbose) cat("Processing period outputs:\n")

  if (nrow(period_vars) > 0) {
    for (i in seq_len(nrow(period_vars))) {
      var_spec <- period_vars[i, ]
      file_name <- var_spec$output_file
      var_name <- var_spec$variable_name

      if (!file_name %in% names(all_file_data)) {
        if (verbose) cat("  Skipping", var_name, "- file", file_name, "not found\n")
        next
      }

      file_data <- all_file_data[[file_name]]
      if (!var_name %in% names(file_data)) {
        if (verbose) cat("  Skipping", var_name, "- variable not in", file_name, "\n")
        next
      }

      obs_periods <- get_observation_periods_generic(
        var_spec = var_spec,
        site_id = metadata$SiteID,
        trt_sch_file = metadata$TreatmentID,
        obs_data_dir = obs_data_dir,
        verbose = FALSE
      )
      num_obs_periods <- num_obs_periods + length(obs_periods)

      period_df <- aggregate_daily_to_period_totals(file_data, var_name, obs_periods)
      if (nrow(period_df) == 0) {
        if (verbose) cat("  Skipping", var_name, "- no overlapping periods\n")
        next
      }

      if (verbose) {
        cat("  Creating period output for", var_name, ":", nrow(period_df), "periods\n")
      }

      for (j in seq_len(nrow(period_df))) {
        period_row <- data.frame(
          SampleID = metadata$SampleID,
          SiteID = metadata$SiteID,
          TreatmentID = metadata$TreatmentID,
          year = period_df$year[[j]],
          period_start = period_df$period_start[[j]],
          variable = var_name,
          Model = "DayCent",
          unit = var_spec$unit,
          d1 = period_df$period_total[[j]],
          stringsAsFactors = FALSE
        )
        period_results <- rbind(period_results, period_row)
      }
    }
  } else if (verbose) {
    cat("  No period variables to process - skipping period outputs\n")
  }

  list(
    period_results = period_results,
    num_obs_periods = num_obs_periods
  )
}

#' Match period-total model outputs with biweekly observations
#'
#' @param model_data Period model output data.
#' @param obs_data Observation data frame.
#' @param var_config Variable configuration from likelihood_calculation.
#' @return Combined dataframe with mod and obs columns.
#' @export
match_period_totals <- function(model_data, obs_data, var_config) {
  if (is.null(model_data) || nrow(model_data) == 0) {
    stop("No period model data available for matching")
  }
  if (is.null(obs_data) || nrow(obs_data) == 0) {
    stop("No observation data available for matching")
  }

  site_col <- if (!is.null(var_config$site_column)) var_config$site_column else "site_ID"
  treatment_col <- if (!is.null(var_config$treatment_column)) {
    var_config$treatment_column
  } else {
    "treatment_schedule"
  }
  period_col <- var_config$date_columns$measurement[[1]]
  obs_value_col <- var_config$value_column

  obs_subset <- obs_data[, c(site_col, treatment_col, period_col, obs_value_col), drop = FALSE]
  names(obs_subset) <- c("SiteID", "TreatmentID", "period_start", "obs_value")
  obs_subset$period_start <- as.Date(obs_subset$period_start)

  model_subset <- model_data[, c("SampleID", "SiteID", "TreatmentID", "period_start", "d1"), drop = FALSE]
  names(model_subset) <- c("SampleID", "SiteID", "TreatmentID", "period_start", "mod_value")
  model_subset$period_start <- as.Date(model_subset$period_start)

  combined_data <- merge(
    model_subset,
    obs_subset,
    by = c("SiteID", "TreatmentID", "period_start"),
    all = FALSE
  )

  if (nrow(combined_data) == 0) {
    stop("No matching data found between period model outputs and observations")
  }

  combined_data$mod <- combined_data$mod_value
  combined_data$obs <- combined_data$obs_value
  combined_data$variable <- var_config$name
  combined_data$model_output <- var_config$model_output
  combined_data$siteID <- combined_data$SiteID
  combined_data$SeasonID <- combined_data$period_start

  combined_data
}
