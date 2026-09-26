#!/usr/bin/env Rscript
# Verify all-period schedule crop codes vs expected historical blocks by decade.

.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")
suppressPackageStartupMessages({
  library(bayesiancalibr)
  library(DBI)
})

expected_crop_for_year <- function(mg_number, year) {
  if (year >= 2011) {
    return(paste0("CM", mg_number, "_11"))
  }
  if (year >= 2001) {
    return(paste0("CM", mg_number, "_01"))
  }
  if (year >= 1991) {
    return(paste0("CM", mg_number, "_91"))
  }
  if (year >= 1981) {
    return(paste0("CM", mg_number, "_81"))
  }
  paste0("CM", mg_number)
}

decade_label <- function(year) {
  if (year >= 2011) return("10-23")
  if (year >= 2001) return("01-10")
  if (year >= 1991) return("91-00")
  if (year >= 1981) return("81-90")
  "pre-81"
}

analyze_mg <- function(mg_id, mg_number, sample_sites = NULL, max_sites = 703) {
  config_path <- file.path(
    "/data/rubelscratch/rubelogle/daycent_calibration/workflows/configs",
    paste0("crop_yield_", mg_id, ".yaml")
  )
  config <- bayesiancalibr::read_yaml_config(config_path)
  config <- bayesiancalibr::resolve_config_paths(config)
  config$file_source$database$tables$run_order <- paste0("run_order_", mg_id, "_all_period")
  config$file_source$database$tables$schedule_files <- paste0("schedule_files_", mg_id, "_all_period")

  conn <- bayesiancalibr::get_shared_connection(config, log_function = function(...) NULL)
  on.exit(try(DBI::dbDisconnect(conn), silent = TRUE), add = TRUE)

  runfile <- bayesiancalibr::create_daycent_runfile_enhanced(
    config = config,
    ExpSite_path = config$paths$expsites_dir,
    shared_connection = conn,
    log_function = function(...) NULL
  )

  site_col <- if ("site_name" %in% names(runfile)) "site_name" else "site_id"
  sites <- unique(as.character(runfile[[site_col]]))
  if (!is.null(sample_sites)) {
    sites <- intersect(sites, sample_sites)
  }
  sites <- head(sites, max_sites)

  target_crops <- c(
    paste0("CM", mg_number, "_11"),
    paste0("CM", mg_number, "_81"),
    paste0("CM", mg_number, "_91"),
    paste0("CM", mg_number, "_01")
  )

  cycle_rows <- list()
  for (site in sites) {
    schedule_content <- bayesiancalibr:::get_crop_detection_schedule_from_database(
      config = config,
      site_name = site,
      shared_connection = conn,
      log_function = function(...) NULL
    )
    parsed <- bayesiancalibr::parse_schedule_for_crops(schedule_content, target_crops, verbose = FALSE)
    if (nrow(parsed) == 0) {
      next
    }
    corn_rows <- parsed[grepl("^CM", parsed$all_crops), , drop = FALSE]
    if (nrow(corn_rows) == 0) {
      next
    }
    for (i in seq_len(nrow(corn_rows))) {
      crops <- strsplit(corn_rows$all_crops[i], ",", fixed = TRUE)[[1]]
      corn_crop <- crops[grepl(paste0("^CM", mg_number), crops)][1]
      if (is.na(corn_crop) || !nzchar(corn_crop)) {
        next
      }
      yr <- corn_rows$year[i]
      cycle_rows[[length(cycle_rows) + 1L]] <- data.frame(
        mg_id = mg_id,
        site_id = site,
        year = yr,
        decade = decade_label(yr),
        planted_crop = corn_crop,
        expected_crop = expected_crop_for_year(mg_number, yr),
        match_expected = identical(corn_crop, expected_crop_for_year(mg_number, yr)),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(cycle_rows) == 0) {
    stop("No corn crop cycles found for ", mg_id)
  }

  cycles <- do.call(rbind, cycle_rows)
  rownames(cycles) <- NULL

  summary_by_decade <- aggregate(
    cbind(n = rep(1, nrow(cycles)), match = as.integer(cycles$match_expected)) ~ mg_id + decade + planted_crop,
    cycles,
    FUN = sum
  )
  summary_by_decade <- summary_by_decade[order(summary_by_decade$decade, -summary_by_decade$n), ]
  rownames(summary_by_decade) <- NULL

  mismatch_by_decade <- aggregate(
    cbind(
      cycles = rep(1, nrow(cycles)),
      mismatches = as.integer(!cycles$match_expected)
    ) ~ mg_id + decade,
    cycles,
    FUN = sum
  )
  mismatch_by_decade$mismatch_pct <- round(100 * mismatch_by_decade$mismatches / mismatch_by_decade$cycles, 1)
  rownames(mismatch_by_decade) <- NULL

  list(
    cycles = cycles,
    crop_counts = summary_by_decade,
    mismatch_by_decade = mismatch_by_decade
  )
}

cat("========================================================================\n")
cat("All-Period Schedule Crop Code Verification\n")
cat("========================================================================\n")

common_sites <- NULL
results <- list()
for (mg in list(list(id = "corn_m2", n = 2), list(id = "corn_m3", n = 3), list(id = "corn_m4", n = 4))) {
  cat("\n---", mg$id, "---\n")
  res <- analyze_mg(mg$id, mg$n, sample_sites = common_sites, max_sites = if (is.null(common_sites)) 703 else length(common_sites))
  if (is.null(common_sites)) {
    common_sites <- unique(res$cycles$site_id)
    cat("Using", length(common_sites), "sites with corn cycles for cross-MG comparison\n")
    results[[mg$id]] <- res
    for (mg2 in list(list(id = "corn_m3", n = 3), list(id = "corn_m4", n = 4))) {
      if (mg2$id == mg$id) next
    }
  } else {
    results[[mg$id]] <- res
  }
  print(res$mismatch_by_decade)
  cat("\nTop planted crops by decade:\n")
  print(res$crop_counts)
}

# Re-run all MGs on same site set for fair comparison
common_sites <- Reduce(intersect, lapply(
  c("corn_m2", "corn_m3", "corn_m4"),
  function(mg_id) {
    mg_n <- as.integer(sub("corn_m", "", mg_id))
    analyze_mg(mg_id, mg_n, max_sites = 50)$cycles$site_id
  }
))
common_sites <- unique(common_sites)[1:min(200, length(unique(common_sites)))]
cat("\n========================================================================\n")
cat("Cross-MG comparison on", length(common_sites), "shared sites\n")
cat("========================================================================\n")

compare <- lapply(list(c("corn_m2", 2), c("corn_m3", 3), c("corn_m4", 4)), function(x) {
  analyze_mg(x[1], as.integer(x[2]), sample_sites = common_sites, max_sites = length(common_sites))$mismatch_by_decade
})
compare_df <- do.call(rbind, compare)
rownames(compare_df) <- NULL
print(compare_df)

out_dir <- "/data/rubelscratch/rubelogle/daycent_calibration/results/historical_scaling/corn_all_mg/15Jun2026/diagnostics"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (mg_id in c("corn_m2", "corn_m3", "corn_m4")) {
  mg_n <- as.integer(sub("corn_m", "", mg_id))
  res <- analyze_mg(mg_id, mg_n, max_sites = 703)
  utils::write.csv(res$crop_counts, file.path(out_dir, paste0("schedule_crop_counts_", mg_id, ".csv")), row.names = FALSE)
  utils::write.csv(res$mismatch_by_decade, file.path(out_dir, paste0("schedule_mismatch_", mg_id, ".csv")), row.names = FALSE)
}

cat("\nWrote diagnostics to:", out_dir, "\n")
