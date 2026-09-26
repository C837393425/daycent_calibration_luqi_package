#!/usr/bin/env Rscript
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")

suppressPackageStartupMessages({
  library(DBI) 
  library(RMySQL) 
  library(dplyr)
})

cat("==============================================\n")
cat("Crop Schedule File Modifier (non-interactive)\n")
cat("==============================================\n")
cat("Target column: schedule_file_data\n")
cat("Change:        'CROP C6' -> 'CROP CM2_11'\n")
cat("Backup:        original table saved before update (backup_YYYYMMDD_HHMMSS)\n")
cat("==============================================\n\n")

## ---- DB connect ----
cred <- readLines("~/.dblogin")
connect_to_database <- function(dbname) {
  tryCatch({
    con <- dbConnect(MySQL(),
                     host = "trillium.nrel.colostate.edu",
                     dbname = dbname,
                     username = cred[1],
                     password = cred[2])
    cat("✓ Connected to database:", dbname, "\n")
    con
  }, error = function(e) {
    cat("✗ Failed to connect:", dbname, "\n  Error:", conditionMessage(e), "\n")
    NULL
  })
}
con <- connect_to_database("inv2024_calib")
if (is.null(con)) quit(status = 1)

## ---- Config ----
crop <- "corn"
if(crop == "corn") {
  maturity_groups <- c(2,3,4,5,6)
} else if (crop == "soyb") {
  maturity_groups <- c(0, 1, 2, 3, 4, 5, 6)
}

qi <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

## ---- Replace helper ----
modify_crop_codes <- function(df, column, pattern, replacement) {
  stopifnot(column %in% names(df))
  n <- nrow(df); mod_rows <- 0L; total_hits <- 0L
  cat("    Processing", n, "rows in column", column, "...\n")
  for (i in seq_len(n)) {
    x <- df[[column]][i]
    if (!is.na(x) && nzchar(x)) {
      hits <- gregexpr(pattern, x, perl = TRUE, ignore.case = TRUE)[[1]]
      k <- if (hits[1] == -1) 0L else length(hits)
      if (k > 0) {
        if (mod_rows < 3L) {
          lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
          eg <- grep(pattern, lines, perl = TRUE, ignore.case = TRUE, value = TRUE)
          cat(sprintf("      Row %d: %d occurrence(s)\n", i, k))
          if (length(eg)) {
            cat("        Example lines:\n")
            for (j in seq_len(min(2, length(eg)))) {
              cat("          Before:", trimws(eg[j]), "\n")
              cat("          After: ", trimws(gsub(pattern, replacement, eg[j], perl = TRUE, ignore.case = TRUE)), "\n")
            }
          }
        }
        df[[column]] [i] <- gsub(pattern, replacement, x, perl = TRUE, ignore.case = TRUE)
        mod_rows <- mod_rows + 1L
        total_hits <- total_hits + k
      }
    }
  }
  cat("    ✓ Modified", mod_rows, "rows with", total_hits, "total replacements\n")
  df
}

## ---- Update with backup+transaction ----
update_database_table <- function(con, table_name, df) {
  if (!nrow(df)) { cat("    No data to update\n"); return(FALSE) }
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_table <- paste0(table_name, "_backup_", stamp)

  tryCatch({
    dbBegin(con)

    # Backup preserving structure & indexes (FKs may not be copied by LIKE)
    dbExecute(con, sprintf("CREATE TABLE %s LIKE %s", qi(con, backup_table), qi(con, table_name)))
    dbExecute(con, sprintf("INSERT INTO %s SELECT * FROM %s", qi(con, backup_table), qi(con, table_name)))
    cat("    ✓ Created backup table:", backup_table, "\n")

    # Replace contents (keep schema)
    dbExecute(con, paste0("DELETE FROM ", qi(con, table_name)))
    DBI::dbWriteTable(con, name = table_name, value = df, append = TRUE, row.names = FALSE)

    dbCommit(con)
    cat("    ✓ Updated table with", nrow(df), "records\n")
    TRUE
  }, error = function(e) {
    dbRollback(con)
    cat("    ✗ Failed to update (rolled back):", conditionMessage(e), "\n")
    FALSE
  })
}

## ---- Main loop ----
updated <- list()

cat("Processing crop:", crop, "\n")
cat("Maturity groups:", paste(maturity_groups, collapse = ", "), "\n\n")

for (mg in maturity_groups) {

   if(crop == "corn") {
    target_column <- "schedule_file_data"
    target_crop_code <- paste0("CM", mg, "_11")
    pattern <- "CROP\\s+C6\\b"               # exact C6 token after 'CROP'
    replacement <- paste0("CROP ", target_crop_code)
  }else if(crop == "soyb") {
    target_column <- "schedule_file_data"
    target_crop_code <- paste0("SM", mg, "_11")
    pattern <- "CROP\\s+SYBN\\b"               
    replacement <- paste0("CROP ", target_crop_code)
  }

  cat("----------------------\n")
  cat("Processing maturity group:", mg, "\n")
  cat("----------------------\n")
  table_name <- paste0("schedule_files_", crop, "_m", mg, "_evaluate")
  cat("Table:", table_name, "\n")

  if (!dbExistsTable(con, table_name)) { cat("  ⚠️  Table missing, skipping.\n\n"); next }

  cat("  Reading table data...\n")
  df <- tryCatch(dbReadTable(con, table_name),
                 error = function(e) { cat("  ✗ Read error:", conditionMessage(e), "\n"); NULL })
  if (is.null(df)) { cat("  Skipping.\n\n"); next }
  cat("  ✓ Read", nrow(df), "records\n")

  if (!(target_column %in% names(df))) {
    cat("  ⚠️  Column '", target_column, "' not found; skipping.\n\n", sep = ""); next
  }

  # Pre-count
  pre_rows_with_c6 <- sum(grepl(pattern, df[[target_column]], perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
  cat("  Rows containing 'CROP C6' before change:", pre_rows_with_c6, "\n")

  # Modify
  cat("  Modifying codes in '", target_column, "'...\n", sep = "")
  df_mod <- modify_crop_codes(df, target_column, pattern, replacement)

  # Verify in-memory
  post_leftover <- sum(grepl(pattern, df_mod[[target_column]], perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
  new_rows <- sum(grepl(paste0("CROP\\s+", target_crop_code, "\\b"), df_mod[[target_column]],
                        perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
  cat("  Verification:\n")
  cat("    Leftover 'CROP C6' rows:", post_leftover, "\n")
  cat("    Rows with 'CROP ", target_crop_code, "': ", new_rows, "\n", sep = "")

  # Write (no prompt)
  cat("  Writing changes with automatic backup...\n")
  if (update_database_table(con, table_name, df_mod)) {
    updated[[length(updated) + 1L]] <- table_name
    cat("  ✅ Successfully updated", table_name, "\n")
  } else {
    cat("  ❌ Failed to update", table_name, "\n")
  }
  cat("\n")
}

dbDisconnect(con)

cat("==============================================\n")
cat("✅ PROCESSING COMPLETE (non-interactive)\n")
cat("==============================================\n")
cat("\nSummary:\n")
cat("- Processed", length(maturity_groups), "maturity groups\n")
cat("- Modified 'CROP C6' → 'CROP ", target_crop_code, "' in '", target_column, "'\n", sep = "")
if (length(updated)) {
  cat("- Updated tables (with backups created):\n  ",
      paste(updated, collapse = ", "), "\n", sep = "")
} else {
  cat("- No tables were updated.\n")
}
