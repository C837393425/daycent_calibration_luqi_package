#!/usr/bin/env Rscript

# -----------------------------------------------
# SOC Site .100 File Modifier (filesystem version)
# Applies the same NH4/NIT/DENIT parameter updates
# from crop_site_modify_S4_2.R to each site .100 file
# under data/soil_organic_carbon/Daycent_ScheduleFiles.
# Replaces ONLY the first number on lines that end
# with the target label (e.g., "... DFCPAR(1)").
# -----------------------------------------------

.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
site_filter <- setdiff(args, "--dry-run")

repo_root <- "/data/rubelscratch/rubelogle/daycent_calibration"
schedule_root <- file.path(repo_root, "data/soil_organic_carbon/Daycent_ScheduleFiles_Evaluation")

fmt4 <- function(x) sprintf("%.4f", as.numeric(x))

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

cat("==============================================\n")
cat("SOC Site .100 File Modifier\n")
cat("==============================================\n")
cat("Schedule root:", schedule_root, "\n")
cat("Dry run:", dry_run, "\n")
if (length(site_filter)) {
  cat("Site filter:", paste(site_filter, collapse = ", "), "\n")
}
cat("Parameters to modify:\n")
for (nm in names(param_modifications)) {
  cat(sprintf("  %-10s -> %s\n", nm, fmt4(param_modifications[[nm]])))
}
cat("==============================================\n\n")

esc <- function(x) gsub("([()])", "\\\\\\1", x)

NUM_RE_NC  <- "(?:[+-]?(?:\\d*\\.\\d+|\\d+\\.?)(?:[eE][+-]?\\d+)?)"
NUM_RE_CAP <- "([+-]?(?:\\d*\\.\\d+|\\d+\\.?)(?:[eE][+-]?\\d+)?)"

split_lines <- function(txt) unlist(strsplit(txt, "\r\n|\n|\r", perl = TRUE))
join_lines  <- function(v) paste(v, collapse = "\n")

replace_param_linewise <- function(blob, label, new_val) {
  lines <- split_lines(blob)
  lab_pat_end <- paste0("\\b", esc(label), "\\s*$")
  idx <- grep(lab_pat_end, lines, perl = TRUE)
  if (length(idx) == 0) {
    return(list(txt = blob, hits = integer(0), olds = character(0)))
  }

  old_vals <- character(length(idx))
  for (k in seq_along(idx)) {
    i <- idx[k]
    m <- regexec(paste0("^\\s*", NUM_RE_CAP), lines[i], perl = TRUE)
    rr <- regmatches(lines[i], m)
    old_vals[k] <- if (length(rr) && length(rr[[1]]) >= 2) rr[[1]][2] else NA_character_

    lines[i] <- sub(
      paste0("^(\\s*)", NUM_RE_NC),
      paste0("\\1", fmt4(new_val)),
      lines[i],
      perl = TRUE
    )
  }
  list(txt = join_lines(lines), hits = idx, olds = old_vals)
}

modify_site_file <- function(file_path) {
  site_name <- basename(dirname(file_path))
  file_label <- basename(file_path)

  blob <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
  if (!nchar(blob)) {
    return(list(modified = FALSE, log = data.frame(), blob = blob))
  }

  change_log <- data.frame(
    site_name = character(),
    file_name = character(),
    parameter = character(),
    old_value = character(),
    new_value = character(),
    line_number = integer(),
    stringsAsFactors = FALSE
  )

  modified <- FALSE
  for (p in names(param_modifications)) {
    res <- replace_param_linewise(blob, p, param_modifications[[p]])
    if (!identical(res$txt, blob)) {
      if (length(res$hits) > 0) {
        for (j in seq_along(res$hits)) {
          change_log <- rbind(change_log, data.frame(
            site_name = site_name,
            file_name = file_label,
            parameter = p,
            old_value = res$olds[j],
            new_value = fmt4(param_modifications[[p]]),
            line_number = res$hits[j],
            stringsAsFactors = FALSE
          ))
        }
      }
      blob <- res$txt
      modified <- TRUE
    }
  }

  list(modified = modified, log = change_log, blob = blob)
}

site_files <- list.files(
  schedule_root,
  pattern = "\\.100$",
  recursive = TRUE,
  full.names = TRUE
)
site_files <- sort(site_files)

if (length(site_filter)) {
  site_files <- site_files[
    vapply(site_files, function(fp) {
      any(vapply(site_filter, function(s) grepl(s, fp, fixed = TRUE), logical(1)))
    }, logical(1))
  ]
}

if (!length(site_files)) {
  cat("No .100 files found under", schedule_root, "\n")
  quit(status = 1)
}

cat("Found", length(site_files), ".100 file(s)\n\n")

all_change_log <- data.frame(
  site_name = character(),
  file_name = character(),
  parameter = character(),
  old_value = character(),
  new_value = character(),
  line_number = integer(),
  stringsAsFactors = FALSE
)

files_changed <- 0L
for (file_path in site_files) {
  rel_path <- sub(paste0("^", repo_root, "/?"), "", file_path)
  cat("----------------------\n")
  cat("Processing:", rel_path, "\n")

  res <- modify_site_file(file_path)
  if (!res$modified) {
    cat("  No target labels changed; skipping write\n\n")
    next
  }

  cat("  Modified", nrow(res$log), "parameter line(s)\n")
  for (nm in names(param_modifications)) {
    pat <- paste0("(?mi)^\\s*.*\\b", esc(nm), "\\s*$")
    m <- regexpr(pat, res$blob, perl = TRUE)
    if (m[1] > 0) {
      cat("    ", regmatches(res$blob, m), "\n")
    }
  }

  if (!dry_run) {
    backup_path <- paste0(file_path, ".backup_", format(Sys.Date(), "%Y%m%d"))
    if (!file.exists(backup_path)) {
      file.copy(file_path, backup_path)
      cat("  Backup:", sub(paste0("^", repo_root, "/?"), "", backup_path), "\n")
    } else {
      cat("  Backup already exists:", sub(paste0("^", repo_root, "/?"), "", backup_path), "\n")
    }
    writeLines(strsplit(res$blob, "\n", fixed = TRUE)[[1]], file_path)
    cat("  Wrote updated file\n")
  } else {
    cat("  Dry run: file not written\n")
  }

  all_change_log <- rbind(all_change_log, res$log)
  files_changed <- files_changed + 1L
  cat("\n")
}

output_dir <- file.path(repo_root, "docs/param_change_log")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("Created output directory:", output_dir, "\n")
}

if (nrow(all_change_log) > 0) {
  csv_filename <- paste0(
    "soc_site_parameter_changes_",
    format(Sys.Date(), "%Y%m%d"),
    if (dry_run) "_dryrun" else "",
    ".csv"
  )
  csv_filepath <- file.path(output_dir, csv_filename)
  write.csv(all_change_log, csv_filepath, row.names = FALSE)
  cat("Saved change log to:", csv_filepath, "\n")
} else {
  cat("No changes were made; no CSV file created\n")
}

cat("==============================================\n")
cat("PROCESSING COMPLETE\n")
cat("Files changed:", files_changed, "\n")
cat("Total parameter-line replacements:", nrow(all_change_log), "\n")
cat("==============================================\n")
