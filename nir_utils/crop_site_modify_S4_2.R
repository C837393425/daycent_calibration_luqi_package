#!/usr/bin/env Rscript

# -----------------------------------------------
# Crop Site File Modifier (line-wise safe version)
# Replaces ONLY the first number on lines that end
# with the target label (e.g., "… DFCPAR(1)").
# -----------------------------------------------

.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")

suppressPackageStartupMessages({
  library(DBI)
  library(RMySQL)   # Your environment uses RMySQL
  library(dplyr)
})

cat("==============================================\n")
cat("Crop Site File Modifier\n")
cat("==============================================\n")
cat("Parameters to modify:\n") 
cat("  NADJMAX   -> 0.007\n")
cat("  NADJMIN   -> 0.003\n")
cat("  MINO3     -> 0\n")
cat("  DPHPAR    -> 5.630096\n")
cat("  IPPAR     -> 0.224045\n")
cat("  IDPAR(1)  -> 0.003658\n")
cat("  IDPAR(2)  -> 0\n")
cat("  IDPAR(3)  -> 0\n")
cat("  MAXDFC    -> 0.002375\n")
cat("  DFCPAR(1) -> -8\n")
cat("  DFCPAR(2) -> 1.877542\n")
cat("  DFCPAR(3) -> 0.134532\n")
cat("  VMAXCAP   -> 0.001446\n")
cat("  FSMPAR(1) -> 0.356001\n")
cat("  FSMPAR(2) -> 1.099951\n")
cat("==============================================\n")



Input.arg  = commandArgs(trailingOnly = TRUE)
id_chist     = as.numeric(Input.arg[1])
node       = Sys.info()["nodename"]


crop_mg <- NULL
all_period <- FALSE 
if (id_chist == 25) {
  crop = "corn" 
  maturity_groups = c(2, 3, 4, 5, 6)
  all_period = TRUE
} else if (id_chist == 13) {
  crop = "soyb" 
  maturity_groups = c(0, 1, 2, 3, 4, 5, 6)
  all_period = TRUE
} else if (id_chist == 27) {
  crop = "sorg" 
  maturity_groups <- c(1, 2, 3, 4, 5)
} else if (id_chist == 1) {             
  crop = "wheat_SW3"
  maturity_groups <- NULL
} else if (id_chist == 2) {             
  crop = "wheat_W3"
  maturity_groups <- NULL
} else if (id_chist == 3) {             
  crop = "wheat_W3SR"
  maturity_groups <- NULL
} else if (id_chist == 4) {             
  crop = "wheat_W3HR"
  maturity_groups <- NULL
} else if (id_chist == 14) {
  crop = "cott"
  maturity_groups <- c(1, 2, 3, 4)
} else if (id_chist == 15) {
  crop = "pnut"
  maturity_groups <- NULL
} else if (id_chist == 17) {
  crop = "sugb"
  maturity_groups <- NULL
} else if (id_chist == 114) {
  crop = "barl"
  maturity_groups <- NULL
} else if (id_chist == 112) {
  crop = "oats"
  maturity_groups <- NULL
} else if (id_chist == 18) {
  crop = "pota"
  maturity_groups <- NULL
} else if (id_chist == 113) {
  crop = "rice"
  maturity_groups <- c(1, 2, 3)
} else if (id_chist == 990046) {
  crop = "swpo"
  maturity_groups <- NULL
} else if (id_chist == 990052) {
  crop = "lent"
  maturity_groups <- NULL
} else if (id_chist == 16) {            ## STATE NASS DATA 
  crop = "toba"
  maturity_groups <- NULL
} else if (id_chist == 21) {             
  crop = "sunf"
  maturity_groups <- NULL
} else if (id_chist == 990042) {             
  crop = "drbe"
  maturity_groups <- NULL
} else if (id_chist == 990049) {             
  crop = "onio"
  maturity_groups <- NULL
} else if (id_chist == 990053) {             
  crop = "peas"
  maturity_groups <- NULL
} else if (id_chist == 990054) {             
  crop = "toma"
  maturity_groups <- NULL
} else if (id_chist == 146) {  # HAY CROP
  crop = "hay" 
  maturity_groups <- c("ALF", "ALF2", "G5", "G4", "G3", "GI3", "WC3", "G3CPI")
} 

# ---------- DB connection ----------
cred_file <- "~/.dblogin"
cred <- readLines(cred_file)

connect_to_database <- function(dbname) {
  tryCatch({
    con <- dbConnect(
      MySQL(),
      host = "trillium.nrel.colostate.edu",
      dbname = dbname,
      username = cred[1],
      password = cred[2]
    )
    cat("✓ Connected to database:", dbname, "\n")
    con
  }, error = function(e) {
    cat("✗ Failed to connect to database:", dbname, "\n")
    cat("  Error:", conditionMessage(e), "\n")
    NULL
  })
}

con <- connect_to_database("inv2024_calib")
if (is.null(con)) {
  cat("Cannot proceed without database connection\n")
  quit(status = 1)
}

# ---------- Config ----------

param_modifications <- list(
  # based on Ram's NH4, NIT/DENIT (early fall 2025) from Shannon 02/20/2026
  "NADJMAX"   = 0.007,
  "NADJMIN"   = 0.003,
  "MINO3"     = 0,
  "DPHPAR"    = 5.630096,
  "IPPAR"     = 0.224045,
  "IDPAR(1)"  = 0.003658,
  "IDPAR(2)"  = 0,
  "IDPAR(3)"  = 0,
  "MAXDFC"    = 0.002375,
  "DFCPAR(1)" = -8,
  "DFCPAR(2)" = 1.877542,
  "DFCPAR(3)" = 0.134532,
  "VMAXCAP"   = 0.001446,
  "FSMPAR(1)" = 0.356001,
  "FSMPAR(2)" = 1.099951
)

cat("\nProcessing crop:", crop, "\n")
cat("Maturity groups:", paste(maturity_groups, collapse = ", "), "\n")
cat("==============================================\n\n")

# ---------- Helpers ----------
fmt4 <- function(x) sprintf("%.4f", as.numeric(x))
esc <- function(x) gsub("([()])", "\\\\\\1", x)

# number: 1, 1., .5, -0.14, 8.0E-6, etc. (non-capturing)
NUM_RE_NC  <- "(?:[+-]?(?:\\d*\\.\\d+|\\d+\\.?)(?:[eE][+-]?\\d+)?)"
# same, capturing (for extracting old values)
NUM_RE_CAP <- "([+-]?(?:\\d*\\.\\d+|\\d+\\.?)(?:[eE][+-]?\\d+)?)"

split_lines <- function(txt) unlist(strsplit(txt, "\r\n|\n|\r", perl = TRUE))
join_lines  <- function(v) paste(v, collapse = "\n")

# Replace only the first number token at start-of-line, on lines that END with the label.
replace_param_linewise <- function(blob, label, new_val) {
  lines <- split_lines(blob)
  lab_pat_end <- paste0("\\b", esc(label), "\\s*$")  # ends with label
  idx <- grep(lab_pat_end, lines, perl = TRUE)
  if (length(idx) == 0) return(list(txt = blob, hits = integer(0), olds = character(0)))

  old_vals <- character(length(idx))
  for (k in seq_along(idx)) {
    i <- idx[k]
    # extract old numeric value at the start of the line
    m <- regexec(paste0("^\\s*", NUM_RE_CAP), lines[i], perl = TRUE)
    rr <- regmatches(lines[i], m)
    old_vals[k] <- if (length(rr) && length(rr[[1]]) >= 2) rr[[1]][2] else NA_character_

    # replace just the first numeric token at SOL
    lines[i] <- sub(paste0("^(\\s*)", NUM_RE_NC),
                    paste0("\\1", fmt4(new_val)),
                    lines[i], perl = TRUE)
  }
  list(txt = join_lines(lines), hits = idx, olds = old_vals)
}

# Core modifier for a table
modify_site_parameters <- function(site_data, table_name, maturity_group) {
  if (is.null(site_data) || nrow(site_data) == 0) {
    return(list(data = site_data, log = data.frame()))
  }

  changed_rows <- 0L
  total_repl   <- 0L
  row_change_log <- data.frame(
    table_name = character(),
    site_name  = character(),
    parameter  = character(),
    old_value  = character(),
    new_value  = character(),
    row_number = integer(),
    stringsAsFactors = FALSE
  )

  cat("    Processing", nrow(site_data), "rows...\n")

  for (i in seq_len(nrow(site_data))) {
    blob <- site_data$site_file_data[i]
    if (is.na(blob) || !nchar(blob)) next

    site_name <- if ("site_name" %in% names(site_data)) site_data$site_name[i] else paste0("row_", i)
    modified  <- FALSE
    n_this    <- 0L

    for (p in names(param_modifications)) {
      res <- replace_param_linewise(blob, p, param_modifications[[p]])
      if (!identical(res$txt, blob)) {
        # log each hit
        if (length(res$hits) > 0) {
          for (j in seq_along(res$hits)) {
            row_change_log <- rbind(row_change_log, data.frame(
              table_name = table_name,
              site_name  = site_name,
              parameter  = p,
              old_value  = res$olds[j],
              new_value  = fmt4(param_modifications[[p]]),
              row_number = i,
              stringsAsFactors = FALSE
            ))
          }
        }
        blob <- res$txt
        modified <- TRUE
        n_this <- n_this + length(res$hits)
      }
    }

    if (modified) {
      site_data$site_file_data[i] <- blob
      changed_rows <- changed_rows + 1L
      total_repl   <- total_repl + n_this

      if (changed_rows <= 2) {
        cat("      Row", i, "(", site_name, "): Modified", n_this, "parameter lines\n")
        # show the five lines after edit for verification
        for (p in names(param_modifications)) {
          pat <- paste0("(?mi)^\\s*.*\\b", esc(p), "\\s*$")
          m <- regexpr(pat, blob, perl = TRUE)
          if (m[1] > 0) cat("        ", regmatches(blob, m), "\n")
        }
      }
    }
  }

  cat("    ✓ Modified", changed_rows, "rows with", total_repl, "total parameter-line replacements\n")
  list(data = site_data, log = row_change_log)
}

# ---------- Table update ----------
update_database_table <- function(con, table_name, modified_data) {
  if (is.null(modified_data) || nrow(modified_data) == 0) {
    cat("    No data to update\n"); return(FALSE)
  }
  tryCatch({
    dbExecute(con, paste0("DELETE FROM ", DBI::dbQuoteIdentifier(con, table_name)))
    dbWriteTable(con, name = table_name, value = modified_data, append = TRUE, row.names = FALSE)
    cat("    ✓ Updated table with", nrow(modified_data), "records\n")
    TRUE
  }, error = function(e) {
    cat("    ✗ Failed to update table:", conditionMessage(e), "\n")
    FALSE
  })
}

# ---------- Run ----------
change_logs_by_mg <- list()

if(crop == "corn" | crop == "soyb" | crop == "sorg" | crop == "rice" | crop == "hay" | crop == "cott") {
  for (mg in maturity_groups) {
    cat("----------------------\n")
    cat("Processing maturity group:", mg, "\n")
    cat("----------------------\n")
    
    if(all_period == TRUE) {
      table_name <- paste0("site_files_", crop, "_m", mg, "_all_period")
    } else if(crop == "hay") {
      table_name <- paste0("site_files_", crop, "_", mg)
    } else {
      table_name <- paste0("site_files_", crop, "_m", mg)
    }
    cat("Table:", table_name, "\n")
    
    if (!dbExistsTable(con, table_name)) {
      cat("  ⚠️  Table does not exist, skipping...\n\n"); next
    }
    
    cat("  Reading table data...\n")
    site_data <- tryCatch(dbReadTable(con, table_name),
                          error = function(e) { cat("  ✗ Failed to read table:", conditionMessage(e), "\n"); NULL })
    if (is.null(site_data)) { cat("  Skipping due to read error...\n\n"); next }
    cat("  ✓ Read", nrow(site_data), "records\n")
    
    if (!"site_file_data" %in% names(site_data)) {
      cat("  ⚠️  Column 'site_file_data' not found, skipping...\n\n"); next
    }
    
    # Preview original (first record)
    cat("  Sample original (first record):\n")
    prev <- site_data$site_file_data[1]
    if (!is.na(prev) && nchar(prev) > 0) {
      p <- substr(prev, 1, 200); if (nchar(prev) > 200) p <- paste0(p, "...")
      cat("    >> ", p, "\n")
    }
    
    cat("  Modifying parameter values…\n")
    res <- modify_site_parameters(site_data, table_name, mg)
    modified_data   <- res$data
    table_change_log <- res$log
    change_logs_by_mg[[as.character(mg)]] <- table_change_log
    
    # Verification: print the five target lines from the first changed row
    if (nrow(table_change_log) > 0) {
      first_changed_row <- table_change_log$row_number[1]
      cat("  Verification (row", first_changed_row, "after edits):\n")
      check_blob <- modified_data$site_file_data[first_changed_row]
      for (nm in names(param_modifications)) {
        pat <- paste0("(?mi)^\\s*.*\\b", esc(nm), "\\s*$")
        m <- regexpr(pat, check_blob, perl = TRUE)
        if (m[1] > 0) cat("    ", regmatches(check_blob, m), "\n")
      }
    } else {
      cat("  ⚠️  No target labels found; nothing changed in this table.\n")
    }
    
    cat("  Updating database…\n")
    if (update_database_table(con, table_name, modified_data)) {
      cat("  ✅ Successfully updated", table_name, "\n\n")
    } else {
      cat("  ❌ Failed to update", table_name, "\n\n")
    }
  }
} else {
    cat("----------------------\n")
    cat("Processing crop:", crop, " \n")
    cat("----------------------\n")
    
    table_name <- paste0("site_files_", crop)
    cat("Table:", table_name, "\n")
    
    if (!dbExistsTable(con, table_name)) {
      cat("  ⚠️  Table does not exist, skipping...\n\n"); next
    }
    
    cat("  Reading table data...\n")
    site_data <- tryCatch(dbReadTable(con, table_name),
                          error = function(e) { cat("  ✗ Failed to read table:", conditionMessage(e), "\n"); NULL })
    if (is.null(site_data)) { cat("  Skipping due to read error...\n\n"); next }
    cat("  ✓ Read", nrow(site_data), "records\n")
    
    if (!"site_file_data" %in% names(site_data)) {
      cat("  ⚠️  Column 'site_file_data' not found, skipping...\n\n"); next
    }
    
    # Preview original (first record)
    cat("  Sample original (first record):\n")
    prev <- site_data$site_file_data[1]
    if (!is.na(prev) && nchar(prev) > 0) {
      p <- substr(prev, 1, 200); if (nchar(prev) > 200) p <- paste0(p, "...")
      cat("    >> ", p, "\n")
    }
    
    cat("  Modifying parameter values…\n")
    res <- modify_site_parameters(site_data, table_name, mg)
    modified_data   <- res$data
    table_change_log <- res$log 
    
    # Verification: print the five target lines from the first changed row
    if (nrow(table_change_log) > 0) {
      first_changed_row <- table_change_log$row_number[1]
      cat("  Verification (row", first_changed_row, "after edits):\n")
      check_blob <- modified_data$site_file_data[first_changed_row]
      for (nm in names(param_modifications)) {
        pat <- paste0("(?mi)^\\s*.*\\b", esc(nm), "\\s*$")
        m <- regexpr(pat, check_blob, perl = TRUE)
        if (m[1] > 0) cat("    ", regmatches(check_blob, m), "\n")
      }
    } else {
      cat("  ⚠️  No target labels found; nothing changed in this table.\n")
    }
    
    cat("  Updating database…\n")
    if (update_database_table(con, table_name, modified_data)) {
      cat("  ✅ Successfully updated", table_name, "\n\n")
    } else {
      cat("  ❌ Failed to update", table_name, "\n\n")
    }
  
}


dbDisconnect(con)

# ---------- Save change logs ----------
output_dir <- "/data/rubelscratch/rubelogle/daycent_calibration/docs/param_change_log"
if (!dir.exists(output_dir)) { dir.create(output_dir, recursive = TRUE); cat("✓ Created output directory:", output_dir, "\n") }

csv_files_created <- character(0)
total_changes <- 0L
for (mg in names(change_logs_by_mg)) {
  change_log <- change_logs_by_mg[[mg]]
  if (is.data.frame(change_log) && nrow(change_log) > 0) {
    csv_filename <- paste0("site_parameter_changes_", crop, "_m", mg, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    csv_filepath <- file.path(output_dir, csv_filename)
    write.csv(change_log, csv_filepath, row.names = FALSE)
    csv_files_created <- c(csv_files_created, csv_filepath)
    total_changes <- total_changes + nrow(change_log)
    cat("✅ Saved change log for maturity group", mg, "to:", csv_filepath, "\n")
  }
}

if (length(csv_files_created)) {
  cat("\n📁 CSV files created:\n"); for (f in csv_files_created) cat("   -", f, "\n")
  cat("   Total changes across all files:", total_changes, "\n")
} else {
  cat("⚠️  No changes were made, no CSV files created\n")
}

cat("==============================================\n")
cat("✅ PROCESSING COMPLETE\n")
cat("==============================================\n")
