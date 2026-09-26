#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(bayesiancalibr)
})

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

assert_error <- function(expr, pattern) {
  err <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = function(e) e
  )

  if (is.null(err)) {
    stop("Expected an error but expression succeeded", call. = FALSE)
  }

  if (!grepl(pattern, conditionMessage(err), fixed = TRUE)) {
    stop(
      "Expected error containing '", pattern, "' but got: ",
      conditionMessage(err),
      call. = FALSE
    )
  }
}

make_schedule <- function(crops) {
  crop_lines <- paste(
    sprintf("%4d 120 CROP %s", seq_along(crops), crops),
    collapse = "\n"
  )

  paste(
    "1 Block",
    "2020 Last year",
    "10 Repeats # years",
    "2011 Output starting year",
    crop_lines,
    sep = "\n"
  )
}

make_event_schedule <- function(event_lines) {
  paste(
    "1 Block",
    "2025 Last year",
    "15 Repeats # years",
    "2011 Output starting year",
    paste(event_lines, collapse = "\n"),
    sep = "\n"
  )
}

quiet_log <- function(...) {}

main <- function() {
  tmp_db <- tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
  on.exit({
    try(DBI::dbDisconnect(con), silent = TRUE)
    if (file.exists(tmp_db)) {
      unlink(tmp_db)
    }
  }, add = TRUE)

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE schedule_files (",
      "site_name TEXT,",
      "treatment_name TEXT,",
      "schedule_file_data TEXT,",
      "notes TEXT",
      ")"
    )
  )

  site_a_schedule <- make_schedule(c("CM4", "SYBN", "CM4"))
  site_b_schedule <- make_schedule(c("SYBN", "CM4", "SYBN"))
  dup_schedule_a <- make_schedule(c("CM4", "CM4"))
  dup_schedule_b <- make_schedule(c("SYBN", "SYBN"))

  DBI::dbWriteTable(
    con,
    "schedule_files",
    data.frame(
      site_name = c("siteA", "siteB", "siteDup", "siteDup"),
      treatment_name = c("", "", "", ""),
      schedule_file_data = c(site_a_schedule, site_b_schedule, dup_schedule_a, dup_schedule_b),
      notes = "",
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )

  config <- list(
    file_source = list(
      mode = "database",
      database = list(
        tables = list(
          schedule_files = "schedule_files"
        )
      )
    )
  )

  cat("Running crop-matrix site-only lookup tests...\n")

  schedule_content <- bayesiancalibr:::get_crop_detection_schedule_from_database(
    config = config,
    site_name = "siteA",
    shared_connection = con,
    log_function = quiet_log
  )
  assert_true(identical(schedule_content, site_a_schedule), "Site-only schedule lookup returned wrong content")

  assert_error(
    bayesiancalibr:::get_crop_detection_schedule_from_database(
      config = config,
      site_name = "missingSite",
      shared_connection = con,
      log_function = quiet_log
    ),
    "No treatment schedule row found"
  )

  assert_error(
    bayesiancalibr:::get_crop_detection_schedule_from_database(
      config = config,
      site_name = "siteDup",
      shared_connection = con,
      log_function = quiet_log
    ),
    "Expected exactly one treatment schedule row"
  )

  runfile <- data.frame(
    siteID = c("siteA", "siteB"),
    treatment_schedule = c("siteA.sch", "siteB.sch"),
    stringsAsFactors = FALSE
  )

  same_year_schedule <- make_event_schedule(c(
    "   1 120 CROP CM4",
    "   1 250 HARV G"
  ))
  winter_schedule <- make_event_schedule(c(
    "   2 303 CROP W3SR",
    "   3 169 HARV G"
  ))
  last_fallback_schedule <- make_event_schedule(c(
    "   2 303 CROP W3SR",
    "   3 120 LAST",
    "   3 200 CROP GI3"
  ))
  incomplete_schedule <- make_event_schedule(c(
    "   2 303 CROP W3SR",
    "   3 200 CULT A"
  ))

  same_year_parsed <- bayesiancalibr::parse_schedule_for_crops(same_year_schedule, "CM4", verbose = FALSE)
  assert_true(
    identical(same_year_parsed$year, 2011),
    "Same-year crop cycle should map to the same harvest year"
  )
  assert_true(
    isTRUE(same_year_parsed$has_target_crop[1]),
    "Same-year crop cycle should mark target crop eligibility"
  )

  winter_parsed <- bayesiancalibr::parse_schedule_for_crops(winter_schedule, "W3SR", verbose = FALSE)
  assert_true(
    identical(winter_parsed$year, 2013),
    "Winter crop cycle should map to the harvest year, not the planting year"
  )
  assert_true(
    isTRUE(winter_parsed$has_target_crop[1]),
    "Winter crop cycle should mark target crop eligibility in harvest year"
  )

  fallback_parsed <- bayesiancalibr::parse_schedule_for_crops(last_fallback_schedule, "W3SR", verbose = FALSE)
  assert_true(
    identical(fallback_parsed$year, 2013),
    "Crop cycle without HARV should fall back to LAST year"
  )

  incomplete_parsed <- bayesiancalibr::parse_schedule_for_crops(incomplete_schedule, "W3SR", verbose = FALSE)
  assert_true(
    identical(incomplete_parsed$year, 2012),
    "Incomplete crop cycle should fall back to planting year"
  )

  site_crop_matrix <- bayesiancalibr::build_site_crop_year_matrix(
    config = config,
    runfile = runfile,
    target_crop = "CM4",
    shared_connection = con,
    verbose = FALSE
  )

  expected_matrix <- data.frame(
    site_id = c("siteA", "siteA", "siteA", "siteB", "siteB", "siteB"),
    year = c(2011, 2012, 2013, 2011, 2012, 2013),
    has_target_crop = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  assert_true(
    isTRUE(all.equal(site_crop_matrix, expected_matrix, check.attributes = FALSE)),
    "Site-crop-year matrix did not match expected CM4 years"
  )

  annual_results <- data.frame(
    site_name = c("siteA", "siteB", "siteA", "siteB"),
    year = c(2011L, 2011L, 2012L, 2012L),
    SampleID = c(1L, 1L, 1L, 1L),
    cgrain = c(100, 300, 200, 400),
    stringsAsFactors = FALSE
  )

  aggregation_metadata <- data.frame(
    site_name = c("siteA", "siteB"),
    aggregation_level = c(27129L, 27129L),
    aggregation_weight = c(2, 1),
    stringsAsFactors = FALSE
  )

  county_results <- bayesiancalibr::aggregate_sites_to_county(
    site_annual_data = annual_results,
    aggregation_metadata = aggregation_metadata,
    variable_name = "cgrain",
    log_function = quiet_log,
    crop_filter_enabled = TRUE,
    site_crop_matrix = site_crop_matrix,
    target_crop = "CM4"
  )

  assert_true(all(county_results$crop_filtered), "County aggregation should mark results as crop-filtered")
  assert_true(nrow(county_results) == 2, "Expected two county-year rows after crop filtering")
  assert_true(
    identical(as.integer(county_results$year), c(2011L, 2012L)),
    "County aggregation returned unexpected years"
  )
  assert_true(
    isTRUE(all.equal(as.numeric(county_results$weighted_value), c(100, 400), tolerance = 1e-8)),
    "County weighted means did not respect crop-year filtering"
  )

  prior_file <- tempfile(fileext = ".csv")
  output_spec_file <- tempfile(fileext = ".csv")
  matrix_file <- tempfile(fileext = ".rds")
  on.exit(unlink(c(prior_file, output_spec_file, matrix_file), force = TRUE), add = TRUE)

  write.csv(
    data.frame(
      File = "crop.100",
      ParameterName = "RUETB",
      stringsAsFactors = FALSE
    ),
    prior_file,
    row.names = FALSE
  )

  write.csv(
    data.frame(
      variable_name = "cgrain",
      weighted_mean_aggregation = TRUE,
      stringsAsFactors = FALSE
    ),
    output_spec_file,
    row.names = FALSE
  )

  strict_config <- config
  strict_config$daycent <- list(crop = list(name = "CM4"))
  strict_config$input_files <- list(prior_file = prior_file)
  strict_config$model_outputs <- list(output_variables_spec = output_spec_file)

  saveRDS(site_crop_matrix, matrix_file)

  crop_context <- bayesiancalibr:::resolve_weighted_crop_filter_context(
    config = strict_config,
    matrix_file = matrix_file,
    target_crop = "CM4",
    log_function = quiet_log
  )

  assert_true(isTRUE(crop_context$crop_filter_enabled), "Crop filter context should be enabled when matrix is present")
  assert_true(nrow(crop_context$site_crop_matrix) == nrow(site_crop_matrix), "Loaded crop matrix size mismatch")

  unlink(matrix_file)

  assert_error(
    bayesiancalibr:::resolve_weighted_crop_filter_context(
      config = strict_config,
      matrix_file = matrix_file,
      target_crop = "CM4",
      log_function = quiet_log
    ),
    "required crop matrix file is missing"
  )

  winter_site_a_schedule <- make_event_schedule(c(
    "   2 303 CROP W3SR",
    "   3 169 HARV G"
  ))
  winter_site_b_schedule <- make_event_schedule(c(
    "   3 120 CROP W3SR",
    "   3 250 HARV G"
  ))
  non_target_schedule <- make_event_schedule(c(
    "   3 100 CROP GI3",
    "   3 250 HARV G"
  ))

  DBI::dbExecute(con, "DELETE FROM schedule_files")
  DBI::dbWriteTable(
    con,
    "schedule_files",
    data.frame(
      site_name = c("winterA", "winterB", "otherC"),
      treatment_name = c("", "", ""),
      schedule_file_data = c(winter_site_a_schedule, winter_site_b_schedule, non_target_schedule),
      notes = "",
      stringsAsFactors = FALSE
    ),
    append = TRUE
  )

  winter_runfile <- data.frame(
    siteID = c("winterA", "winterB", "otherC"),
    treatment_schedule = c("winterA.sch", "winterB.sch", "otherC.sch"),
    stringsAsFactors = FALSE
  )

  winter_matrix <- bayesiancalibr::build_site_crop_year_matrix(
    config = config,
    runfile = winter_runfile,
    target_crop = "W3SR",
    shared_connection = con,
    verbose = FALSE
  )

  winter_rows <- winter_matrix[winter_matrix$site_id %in% c("winterA", "winterB"), , drop = FALSE]
  assert_true(
    isTRUE(any(winter_rows$site_id == "winterA" & winter_rows$year == 2013 & winter_rows$has_target_crop)),
    "Fall-planted winter crop should be eligible in harvest year"
  )
  assert_true(
    isTRUE(any(winter_rows$site_id == "winterB" & winter_rows$year == 2013 & winter_rows$has_target_crop)),
    "Same-year winter crop should stay eligible in the same year"
  )

  winter_annual_results <- data.frame(
    site_name = c("winterA", "winterB", "otherC"),
    year = c(2013L, 2013L, 2013L),
    SampleID = c(1L, 1L, 1L),
    cgrain = c(141.485, 446.815, 999.0),
    stringsAsFactors = FALSE
  )

  winter_agg_meta <- data.frame(
    site_name = c("winterA", "winterB", "otherC"),
    aggregation_level = c(29145L, 29145L, 29145L),
    aggregation_weight = c(17, 12.057, 99),
    stringsAsFactors = FALSE
  )

  winter_county_results <- bayesiancalibr::aggregate_sites_to_county(
    site_annual_data = winter_annual_results,
    aggregation_metadata = winter_agg_meta,
    variable_name = "cgrain",
    log_function = quiet_log,
    crop_filter_enabled = TRUE,
    site_crop_matrix = winter_matrix,
    target_crop = "W3SR"
  )

  expected_winter_mean <- (17 * 141.485 + 12.057 * 446.815) / (17 + 12.057)
  winter_2013 <- winter_county_results[winter_county_results$aggregation_level == 29145L &
                                          winter_county_results$year == 2013L, , drop = FALSE]
  assert_true(nrow(winter_2013) == 1, "Expected one winter county-year row")
  assert_true(
    identical(as.integer(winter_2013$n_sites), 2L),
    "Winter county-year should include both eligible target-crop sites"
  )
  assert_true(
    isTRUE(all.equal(as.numeric(winter_2013$weighted_value), expected_winter_mean, tolerance = 1e-8)),
    "Winter county weighted mean should use harvest-year eligibility"
  )

  cat("All crop-matrix site-only lookup tests passed.\n")
}

main()
