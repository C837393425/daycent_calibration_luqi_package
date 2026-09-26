#!/usr/bin/env Rscript

# Copy DayMet weather_code values from regular run_order tables into
# matching *_all_period sibling tables.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
table_filter <- NULL
if (length(args) > 0) {
  tables_idx <- which(args == "--tables")
  if (length(tables_idx) > 0 && tables_idx < length(args)) {
    table_filter <- strsplit(args[tables_idx + 1], ",", fixed = TRUE)[[1]]
    table_filter <- trimws(table_filter)
    table_filter <- table_filter[nzchar(table_filter)]
  }
}

CANONICAL_ROOT <- "/data/rubelscratch/rubelogle/daycent_calibration"
WEATHER_BASE <- "/data/rubelscratch/rubelogle/DayMet_NLDAS_7col_weatherFiles"
MIN_GRID_PREFIX_LEN <- 4L
SAMPLE_SITES <- 5L

TABLE_PAIRS <- list(
  list(id = "corn_m2", source = "run_order_corn_m2", target = "run_order_corn_m2_all_period"),
  list(id = "corn_m3", source = "run_order_corn_m3", target = "run_order_corn_m3_all_period"),
  list(id = "corn_m4", source = "run_order_corn_m4", target = "run_order_corn_m4_all_period"),
  list(id = "corn_m5", source = "run_order_corn_m5", target = "run_order_corn_m5_all_period"),
  list(id = "corn_m6", source = "run_order_corn_m6", target = "run_order_corn_m6_all_period"),
  list(id = "soyb_m0", source = "run_order_soyb_m0", target = "run_order_soyb_m0_all_period"),
  list(id = "soyb_m1", source = "run_order_soyb_m1", target = "run_order_soyb_m1_all_period"),
  list(id = "soyb_m2", source = "run_order_soyb_m2", target = "run_order_soyb_m2_all_period"),
  list(id = "soyb_m3", source = "run_order_soyb_m3", target = "run_order_soyb_m3_all_period"),
  list(id = "soyb_m4", source = "run_order_soyb_m4", target = "run_order_soyb_m4_all_period"),
  list(id = "soyb_m5", source = "run_order_soyb_m5", target = "run_order_soyb_m5_all_period"),
  list(id = "soyb_m6", source = "run_order_soyb_m6", target = "run_order_soyb_m6_all_period")
)

connect_inv2024 <- function() {
  cred_file <- path.expand("~/.dblogin")
  if (!file.exists(cred_file)) {
    stop("Credential file not found: ", cred_file)
  }
  cred <- trimws(readLines(cred_file, warn = FALSE))
  cred <- cred[nzchar(cred)]
  if (length(cred) < 2) {
    stop("Credential file must contain username and password lines")
  }

  if (requireNamespace("RMariaDB", quietly = TRUE)) {
    DBI::dbConnect(
      RMariaDB::MariaDB(),
      host = "trillium.nrel.colostate.edu",
      dbname = "inv2024_calib",
      username = cred[1],
      password = cred[2]
    )
  } else {
    DBI::dbConnect(
      RMySQL::MySQL(),
      host = "trillium.nrel.colostate.edu",
      dbname = "inv2024_calib",
      username = cred[1],
      password = cred[2]
    )
  }
}

table_exists <- function(con, table_name) {
  table_name %in% DBI::dbListTables(con)
}

read_run_order_table <- function(con, table_name) {
  if (!table_exists(con, table_name)) {
    stop("Table does not exist: ", table_name)
  }
  DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_name))
}

grid_prefix_lengths <- function(weather_codes) {
  prefix <- sub("_.*$", "", as.character(weather_codes))
  nchar(prefix)
}

build_daymet_weather_path <- function(weather_code) {
  prefix <- sub("_.*$", "", weather_code)
  file.path(WEATHER_BASE, paste0("Daymet_", prefix), paste0(weather_code, ".wth"))
}

validate_site_sets <- function(source_df, target_df, pair_id) {
  source_sites <- unique(as.character(source_df$site_name))
  target_sites <- unique(as.character(target_df$site_name))

  missing_in_target <- setdiff(source_sites, target_sites)
  extra_in_target <- setdiff(target_sites, source_sites)

  if (length(missing_in_target) > 0 || length(extra_in_target) > 0) {
    stop(
      "site_name mismatch for ", pair_id,
      ": missing_in_target=", length(missing_in_target),
      ", extra_in_target=", length(extra_in_target)
    )
  }
}

validate_source_weather_codes <- function(source_df, pair_id) {
  if (!"weather_code" %in% names(source_df)) {
    stop("Source table missing weather_code column: ", pair_id)
  }
  prefix_lens <- grid_prefix_lengths(source_df$weather_code)
  if (any(prefix_lens < MIN_GRID_PREFIX_LEN, na.rm = TRUE)) {
    stop(
      "Source weather_code values do not look DayMet-like for ", pair_id,
      " (min prefix length=", min(prefix_lens, na.rm = TRUE), ")"
    )
  }
}

merge_weather_codes <- function(source_df, target_df) {
  source_lookup <- source_df[, c("site_name", "weather_code"), drop = FALSE]
  names(source_lookup) <- c("site_name", "source_weather_code")
  source_lookup$site_name <- as.character(source_lookup$site_name)

  merged <- merge(
    target_df,
    source_lookup,
    by = "site_name",
    all.x = TRUE,
    sort = FALSE
  )

  if (any(is.na(merged$source_weather_code))) {
    stop("Failed to match all target sites to source weather_code values")
  }

  audit <- data.frame(
    site_name = as.character(merged$site_name),
    old_weather_code = as.character(merged$weather_code),
    new_weather_code = as.character(merged$source_weather_code),
    stringsAsFactors = FALSE
  )
  audit$changed <- audit$old_weather_code != audit$new_weather_code

  merged$weather_code <- merged$source_weather_code
  merged$source_weather_code <- NULL

  list(updated = merged, audit = audit)
}

validate_sample_weather_files <- function(audit_df, pair_id) {
  sample_rows <- audit_df
  if (nrow(sample_rows) > SAMPLE_SITES) {
    sample_rows <- sample_rows[seq_len(SAMPLE_SITES), , drop = FALSE]
  }

  missing_files <- character(0)
  for (i in seq_len(nrow(sample_rows))) {
    weather_path <- build_daymet_weather_path(sample_rows$new_weather_code[i])
    if (!file.exists(weather_path)) {
      missing_files <- c(
        missing_files,
        paste0(sample_rows$site_name[i], " -> ", weather_path)
      )
    }
  }

  if (length(missing_files) > 0) {
    stop(
      "Sample weather files missing for ", pair_id, ":\n",
      paste(missing_files, collapse = "\n")
    )
  }

  invisible(TRUE)
}

replace_target_table <- function(con, table_name, updated_df) {
  if (DBI::dbExistsTable(con, table_name)) {
    DBI::dbRemoveTable(con, table_name)
  }
  DBI::dbWriteTable(con, table_name, updated_df, row.names = FALSE)
}

process_table_pair <- function(con, pair, output_dir, dry_run = FALSE) {
  cat("\n----------------------------------------------------------------\n")
  cat("Processing pair:", pair$id, "\n")
  cat("  source:", pair$source, "\n")
  cat("  target:", pair$target, "\n")

  source_df <- read_run_order_table(con, pair$source)
  target_df <- read_run_order_table(con, pair$target)

  validate_site_sets(source_df, target_df, pair$id)
  validate_source_weather_codes(source_df, pair$id)

  merged <- merge_weather_codes(source_df, target_df)
  audit <- merged$audit
  updated_df <- merged$updated

  snapshot_path <- file.path(output_dir, paste0(pair$id, "_target_before.csv"))
  audit_path <- file.path(output_dir, paste0(pair$id, "_weather_audit.csv"))
  utils::write.csv(target_df, snapshot_path, row.names = FALSE)
  utils::write.csv(audit, audit_path, row.names = FALSE)

  prefix_before <- grid_prefix_lengths(target_df$weather_code)
  prefix_after <- grid_prefix_lengths(updated_df$weather_code)

  cat("  rows           :", nrow(updated_df), "\n")
  cat("  changed codes  :", sum(audit$changed), "\n")
  cat("  prefix before  :", min(prefix_before), "-", max(prefix_before), "\n")
  cat("  prefix after   :", min(prefix_after), "-", max(prefix_after), "\n")
  cat("  audit file     :", audit_path, "\n")

  validate_sample_weather_files(audit, pair$id)

  if (isTRUE(dry_run)) {
    cat("  dry-run        : no database write performed\n")
    return(list(
      pair_id = pair$id,
      changed = sum(audit$changed),
      dry_run = TRUE
    ))
  }

  replace_target_table(con, pair$target, updated_df)
  cat("  updated table  :", pair$target, "\n")

  list(
    pair_id = pair$id,
    changed = sum(audit$changed),
    dry_run = FALSE
  )
}

main <- function() {
  .libPaths(c(file.path(CANONICAL_ROOT, "rlib"), .libPaths()))
  suppressPackageStartupMessages({
    library(DBI)
  })

  pairs <- TABLE_PAIRS
  if (!is.null(table_filter) && length(table_filter) > 0) {
    pairs <- Filter(function(pair) pair$id %in% table_filter, TABLE_PAIRS)
    if (length(pairs) == 0) {
      stop("No table pairs matched --tables filter: ", paste(table_filter, collapse = ", "))
    }
  }

  date_stamp <- format(Sys.Date(), "%d%b%Y")
  output_dir <- file.path(
    CANONICAL_ROOT,
    "results",
    "historical_scaling",
    "weather_code_update",
    date_stamp
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  cat("========================================================================\n")
  cat("Update All-Period Weather Codes\n")
  cat("========================================================================\n")
  cat("Mode       :", if (dry_run) "dry-run" else "apply", "\n")
  cat("Pairs      :", length(pairs), "\n")
  cat("Output dir :", output_dir, "\n")

  con <- connect_inv2024()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  results <- lapply(pairs, function(pair) {
    process_table_pair(con, pair, output_dir, dry_run = dry_run)
  })

  summary_df <- do.call(rbind, lapply(results, function(x) {
    data.frame(
      pair_id = x$pair_id,
      changed = x$changed,
      dry_run = x$dry_run,
      stringsAsFactors = FALSE
    )
  }))
  rownames(summary_df) <- NULL
  summary_path <- file.path(output_dir, "update_summary.csv")
  utils::write.csv(summary_df, summary_path, row.names = FALSE)

  cat("\n----------------------------------------------------------------\n")
  cat("Complete\n")
  cat("Summary:", summary_path, "\n")
  print(summary_df)
  cat("========================================================================\n")
}

main()
