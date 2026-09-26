#!/usr/bin/env Rscript

# Replace weather.wth placeholders in *_all_period schedule_files tables with
# site-specific DayMet filenames from matching run_order_*_all_period tables.

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

TABLE_PAIRS <- list(
  list(
    id = "corn_m2",
    run_order = "run_order_corn_m2_all_period",
    schedule_files = "schedule_files_corn_m2_all_period"
  ),
  list(
    id = "corn_m3",
    run_order = "run_order_corn_m3_all_period",
    schedule_files = "schedule_files_corn_m3_all_period"
  ),
  list(
    id = "corn_m4",
    run_order = "run_order_corn_m4_all_period",
    schedule_files = "schedule_files_corn_m4_all_period"
  ),
  list(
    id = "corn_m5",
    run_order = "run_order_corn_m5_all_period",
    schedule_files = "schedule_files_corn_m5_all_period"
  ),
  list(
    id = "corn_m6",
    run_order = "run_order_corn_m6_all_period",
    schedule_files = "schedule_files_corn_m6_all_period"
  ),
  list(
    id = "soyb_m0",
    run_order = "run_order_soyb_m0_all_period",
    schedule_files = "schedule_files_soyb_m0_all_period"
  ),
  list(
    id = "soyb_m1",
    run_order = "run_order_soyb_m1_all_period",
    schedule_files = "schedule_files_soyb_m1_all_period"
  ),
  list(
    id = "soyb_m2",
    run_order = "run_order_soyb_m2_all_period",
    schedule_files = "schedule_files_soyb_m2_all_period"
  ),
  list(
    id = "soyb_m3",
    run_order = "run_order_soyb_m3_all_period",
    schedule_files = "schedule_files_soyb_m3_all_period"
  ),
  list(
    id = "soyb_m4",
    run_order = "run_order_soyb_m4_all_period",
    schedule_files = "schedule_files_soyb_m4_all_period"
  ),
  list(
    id = "soyb_m5",
    run_order = "run_order_soyb_m5_all_period",
    schedule_files = "schedule_files_soyb_m5_all_period"
  ),
  list(
    id = "soyb_m6",
    run_order = "run_order_soyb_m6_all_period",
    schedule_files = "schedule_files_soyb_m6_all_period"
  )
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

read_table <- function(con, table_name) {
  if (!table_exists(con, table_name)) {
    stop("Table does not exist: ", table_name)
  }
  DBI::dbGetQuery(con, paste0("SELECT * FROM ", table_name))
}

extract_weather_filename <- function(schedule_text) {
  if (is.na(schedule_text) || !nzchar(schedule_text)) {
    return(NA_character_)
  }
  lines <- strsplit(schedule_text, "\n", fixed = TRUE)[[1]]
  idx <- grep("Weather choice", lines, ignore.case = TRUE)
  if (length(idx) == 0) {
    return(NA_character_)
  }
  trimws(lines[idx[1] + 1])
}

replace_schedule_weather_name <- function(schedule_text, weather_code) {
  if (is.na(schedule_text) || !nzchar(schedule_text)) {
    return(schedule_text)
  }
  new_weather_file <- paste0(weather_code, ".wth")
  gsub("weather\\.wth", new_weather_file, schedule_text, perl = TRUE)
}

validate_site_sets <- function(run_order_df, schedule_df, pair_id) {
  run_sites <- unique(as.character(run_order_df$site_name))
  sched_sites <- unique(as.character(schedule_df$site_name))

  missing_in_schedule <- setdiff(run_sites, sched_sites)
  extra_in_schedule <- setdiff(sched_sites, run_sites)
  if (length(missing_in_schedule) > 0 || length(extra_in_schedule) > 0) {
    stop(
      "site_name mismatch for ", pair_id,
      ": missing_in_schedule=", length(missing_in_schedule),
      ", extra_in_schedule=", length(extra_in_schedule)
    )
  }
}

validate_weather_codes <- function(run_order_df, pair_id) {
  if (!"weather_code" %in% names(run_order_df)) {
    stop("run_order table missing weather_code column: ", pair_id)
  }
  prefix_lens <- nchar(sub("_.*$", "", as.character(run_order_df$weather_code)))
  if (any(prefix_lens < 4L, na.rm = TRUE)) {
    stop(
      "run_order weather_code values do not look DayMet-like for ", pair_id,
      " (min prefix length=", min(prefix_lens, na.rm = TRUE), ")"
    )
  }
}

update_schedule_weather_names <- function(run_order_df, schedule_df) {
  weather_lookup <- setNames(
    as.character(run_order_df$weather_code),
    as.character(run_order_df$site_name)
  )

  audit <- data.frame(
    site_name = character(0),
    old_weather_file = character(0),
    new_weather_file = character(0),
    changed = logical(0),
    stringsAsFactors = FALSE
  )

  updated <- schedule_df
  for (i in seq_len(nrow(updated))) {
    site_name <- as.character(updated$site_name[i])
    weather_code <- weather_lookup[[site_name]]
    if (is.null(weather_code) || is.na(weather_code) || !nzchar(weather_code)) {
      stop("Missing weather_code for site ", site_name)
    }

    old_file <- extract_weather_filename(updated$schedule_file_data[i])
    new_text <- replace_schedule_weather_name(updated$schedule_file_data[i], weather_code)
    new_file <- extract_weather_filename(new_text)

    audit <- rbind(
      audit,
      data.frame(
        site_name = site_name,
        old_weather_file = old_file,
        new_weather_file = new_file,
        changed = !identical(old_file, new_file),
        stringsAsFactors = FALSE
      )
    )
    updated$schedule_file_data[i] <- new_text
  }

  list(updated = updated, audit = audit)
}

replace_schedule_table <- function(con, table_name, updated_df) {
  qi <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  DBI::dbBegin(con)
  on.exit(try(DBI::dbRollback(con), silent = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0("DELETE FROM ", qi(table_name)))
  DBI::dbWriteTable(con, table_name, updated_df, append = TRUE, row.names = FALSE)
  DBI::dbCommit(con)
  on.exit(NULL, add = FALSE)
}

process_table_pair <- function(con, pair, output_dir, dry_run = FALSE) {
  cat("\n----------------------------------------------------------------\n")
  cat("Processing pair:", pair$id, "\n")
  cat("  run_order      :", pair$run_order, "\n")
  cat("  schedule_files :", pair$schedule_files, "\n")

  run_order_df <- read_table(con, pair$run_order)
  schedule_df <- read_table(con, pair$schedule_files)

  validate_site_sets(run_order_df, schedule_df, pair$id)
  validate_weather_codes(run_order_df, pair$id)

  merged <- update_schedule_weather_names(run_order_df, schedule_df)
  audit <- merged$audit
  updated_df <- merged$updated

  snapshot_path <- file.path(output_dir, paste0(pair$id, "_schedule_before.csv"))
  audit_path <- file.path(output_dir, paste0(pair$id, "_schedule_weather_audit.csv"))
  utils::write.csv(schedule_df, snapshot_path, row.names = FALSE)
  utils::write.csv(audit, audit_path, row.names = FALSE)

  remaining_placeholder <- sum(grepl("weather\\.wth", updated_df$schedule_file_data, perl = TRUE))

  cat("  rows                 :", nrow(updated_df), "\n")
  cat("  changed rows         :", sum(audit$changed), "\n")
  cat("  remaining weather.wth:", remaining_placeholder, "\n")
  cat("  audit file           :", audit_path, "\n")

  if (remaining_placeholder > 0) {
    stop("Updated schedules still contain weather.wth for ", pair$id)
  }

  if (isTRUE(dry_run)) {
    cat("  dry-run              : no database write performed\n")
    return(list(pair_id = pair$id, changed = sum(audit$changed), dry_run = TRUE))
  }

  replace_schedule_table(con, pair$schedule_files, updated_df)
  cat("  updated table        :", pair$schedule_files, "\n")

  list(pair_id = pair$id, changed = sum(audit$changed), dry_run = FALSE)
}

main <- function() {
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
    "schedule_weather_update",
    date_stamp
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  cat("========================================================================\n")
  cat("Update All-Period Schedule Weather Filenames\n")
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
