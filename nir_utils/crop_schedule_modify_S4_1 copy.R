#!/usr/bin/env Rscript
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")

suppressPackageStartupMessages({
  library(DBI)
  library(RMySQL)
  library(dplyr)
})

Input.arg  = commandArgs(trailingOnly = TRUE)
id_chist     = as.numeric(Input.arg[1])
node       = Sys.info()["nodename"]

crop_mg <- NULL
all_period <- FALSE 
if (id_chist == 16) {            ## STATE NASS DATA 
  crop = "toba"
  maturity_groups <- NULL
} 
schedule_update_flag <- TRUE  # crop name change 


cat("==============================================\n")
cat("Crop Schedule File Modifier (non-interactive)\n")
cat("==============================================\n")
cat("Target column: schedule_file_data\n")
cat("Change:        'HARV SIL' -> 'HARV G90S' for Tobacco\n")
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

## ---- Update with transaction ----
update_database_table <- function(con, table_name, df) {
  if (!nrow(df)) { cat("    No data to update\n"); return(FALSE) }

  tryCatch({
    dbBegin(con)

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


# schedule crops need update
if (schedule_update_flag == TRUE) {
  target_column <- "schedule_file_data"
  pattern <- "HARV\\s+SIL\\b"
  replacement <- "HARV G90S"
  table_name <- paste0("schedule_files_", crop)

  cat("Processing crop:", crop, "\n")
  cat("Table:", table_name, "\n\n")

  if (!dbExistsTable(con, table_name)) {
    cat("  ⚠️  Table missing, exiting.\n")
    dbDisconnect(con)
    quit(status = 1)
  }

  cat("  Reading table data...\n")
  df <- tryCatch(dbReadTable(con, table_name),
                error = function(e) { cat("  ✗ Read error:", conditionMessage(e), "\n"); NULL })
  if (is.null(df)) {
    dbDisconnect(con)
    quit(status = 1)
  }
  cat("  ✓ Read", nrow(df), "records\n")

  if (!(target_column %in% names(df))) {
    cat("  ⚠️  Column '", target_column, "' not found; exiting.\n", sep = "")
    dbDisconnect(con)
    quit(status = 1)
  }

  pre_rows <- sum(grepl(pattern, df[[target_column]], perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
  cat("  Rows matching 'HARV SIL' before change:", pre_rows, "\n")

  cat("  Modifying harvest codes in '", target_column, "'...\n", sep = "")
  df_mod <- modify_crop_codes(df, target_column, pattern, replacement)

  post_leftover <- sum(grepl(pattern, df_mod[[target_column]], perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
  new_rows <- sum(grepl("HARV\\s+G90S\\b", df_mod[[target_column]], perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
  cat("  Verification:\n")
  cat("    Leftover rows matching 'HARV SIL':", post_leftover, "\n")
  cat("    Rows with 'HARV G90S':", new_rows, "\n")

  cat("  Writing changes to database...\n")
  updated <- update_database_table(con, table_name, df_mod)

  dbDisconnect(con)

  cat("==============================================\n")
  cat("✅ PROCESSING COMPLETE (non-interactive)\n")
  cat("==============================================\n")
  cat("\nSummary:\n")
  cat("- Modified 'HARV SIL' → 'HARV G90S' in '", target_column, "'\n", sep = "")
  if (updated) {
    cat("- Updated table:", table_name, "\n")
  } else {
    cat("- No tables were updated.\n")
  }
}

