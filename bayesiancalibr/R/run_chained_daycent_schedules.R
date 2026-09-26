##' Run Chained DayCent Schedules from Observation Table
##'
##' Runs DayCent sequentially for each schedule column in an observation table,
##' chaining extended site.100 files so that the output from one schedule is
##' used as the input for the next schedule.
##'
##' This is designed for workflows like the Western Sugar SOC project where
##' columns such as \code{equil_schedule}, \code{base_schedule},
##' \code{su_schedule}, \code{exp1_schedule}, \code{exp2_schedule}, etc.
##' appear in an observation CSV, but it is written generically so that it can
##' be reused in other projects with different schedule column names.
##'
##' For each row (site) in the table and for each schedule column (in the
##' specified order), the function:
##' \enumerate{
##'   \item Runs DayCent for the schedule file in that column.
##'   \item Writes an extended site.100 file whose name is derived from the
##'         schedule file name: \code{<schedule_basename>_ext.100}.
##'   \item Uses the extended site.100 from the previous schedule as the
##'         \code{ext_site100_2read} input for the next schedule in the chain.
##' }
##'
##' @param filepath_exe Character string with path to DayCent executable.
##' @param obs_table Either a data frame containing observation data with one or
##'   more schedule columns, or a character string giving the path to a CSV
##'   file that can be read with \code{read.csv()}.
##' @param schedule_columns Optional character vector of column names in
##'   \code{obs_table} that contain schedule file names, in the order they
##'   should be executed. If \code{NULL} (default), all columns whose names
##'   end with \code{"_schedule"} are used, in the order they appear in the
##'   table.
##' @param site_dir_column Optional column name in \code{obs_table} that
##'   contains per-row site directory paths. If provided and the value is
##'   non-empty for a given row, the working directory is temporarily changed
##'   to that path while running the chain for that row.
##' @param base_dir Optional base directory to prepend to values in
##'   \code{site_dir_column}. If \code{NULL} (default), paths in
##'   \code{site_dir_column} are treated as they are.
##' @param ext_suffix Suffix to append to each schedule file base name (without
##'   \code{.sch}) to create the extended site.100 file base name. The suffix
##'   should not include the \code{.100} extension (default: \code{"_ext"}).
##' @param initial_ext_file Optional path to an existing extended site.100 file
##'   to use as \code{ext_site100_2read} for the first schedule (e.g. equilibrium)
##'   in each chain. If \code{NULL} (default), the first schedule uses the
##'   site-named site.100 file (\code{<siteID>.100}) when it exists and the
##'   \code{siteID} column is present in the observation table.
##' @param verbose Logical; if \code{TRUE} (default), progress messages are
##'   printed for each site and schedule.
##' @param keep_intermediate Logical; if \code{FALSE} (default), intermediate
##'   output files (.bin, extended site.100, logs) from schedules before the
##'   last one are deleted. Only the last schedule's outputs are kept. If
##'   \code{TRUE}, all schedule outputs are kept.
##'
##' @return Integer: 0 for success (all schedules completed successfully),
##'   1 for failure (any schedule failed). This matches the return type of
##'   \code{run_DayCent()} for consistency.
##'   
##' @details
##' Output Files:
##' This function produces the same TYPE of output files as \code{run_DayCent()}
##' (binary .bin files, extended site.100 files, log files), but for ALL schedules
##' in the chain, not just one. Each schedule in the chain produces:
##' \itemize{
##'   \item A .bin file: \code{<schedule_basename>.bin}
##'   \item An extended site.100 file: \code{<schedule_basename>_ext.100}
##'   \item Log files: \code{<schedule_basename>_stdout.log} and \code{<schedule_basename>_stderr.log}
##' }
##' 
##' For workflow integration (e.g., GSA Step 2), you typically want to process
##' outputs from the LAST schedule in the chain, which contains the final simulation
##' state after running through all previous schedules. The last schedule file name
##' can be obtained from the last element of \code{schedule_columns}.
##'
##' @examples
##' \dontrun{
##' obs_csv <- "data/western_sugar_soc/Observation_Data/MeasDat_SOC_02-16-2026.csv"
##' exe <- "/path/to/daycent_executable"
##'
##' # Use all *_schedule columns in the CSV, for all rows:
##' run_chained_daycent_schedules(
##'   filepath_exe = exe,
##'   obs_table = obs_csv
##' )
##'
##' # Explicit schedule columns and a base directory for site paths:
##' run_chained_daycent_schedules(
##'   filepath_exe = exe,
##'   obs_table = obs_csv,
##'   schedule_columns = c("equil_schedule", "base_schedule", "su_schedule",
##'                        "exp1_schedule", "exp2_schedule"),
##'   site_dir_column = "path",
##'   base_dir = "/data/rubelscratch/rubelogle/daycent_calibration"
##' )
##' }
##'
##' @export
run_chained_daycent_schedules <- function(filepath_exe,
                                          obs_table,
                                          schedule_columns = NULL,
                                          site_dir_column = NULL,
                                          base_dir = NULL,
                                          ext_suffix = "_ext",
                                          initial_ext_file = NULL,
                                          verbose = TRUE,
                                          keep_intermediate = FALSE) {
  
  # Basic validation of executable
  if (!file.exists(filepath_exe)) {
    stop("DayCent executable not found: ", filepath_exe)
  }
  
  # Load table if a path is provided
  if (is.character(obs_table) && length(obs_table) == 1) {
    if (!file.exists(obs_table)) {
      stop("Observation table file not found: ", obs_table)
    }
    obs_df <- utils::read.csv(obs_table, stringsAsFactors = FALSE)
  } else if (is.data.frame(obs_table)) {
    obs_df <- obs_table
  } else {
    stop("obs_table must be either a data.frame or a path to a CSV file")
  }
  
  if (nrow(obs_df) == 0) {
    if (verbose) {
      cat("Observation table has zero rows; nothing to run.\n")
    }
    return(invisible(list()))
  }
  
  # Determine schedule columns if not provided
  if (is.null(schedule_columns)) {
    schedule_columns <- grep("_schedule$", names(obs_df), value = TRUE)
    if (length(schedule_columns) == 0) {
      stop("No schedule columns found. Please provide schedule_columns or ",
           "ensure there are columns ending with '_schedule'.")
    }
  } else {
    missing_cols <- setdiff(schedule_columns, names(obs_df))
    if (length(missing_cols) > 0) {
      stop("The following schedule_columns are not present in obs_table: ",
           paste(missing_cols, collapse = ", "))
    }
  }
  
  if (verbose) {
    cat("Running chained DayCent schedules for", nrow(obs_df), "rows\n")
    cat("Schedule columns (in order):", paste(schedule_columns, collapse = ", "), "\n")
  }
  
  # Helper to build full directory path for a given row, if site_dir_column is used
  get_row_working_dir <- function(row_index) {
    if (is.null(site_dir_column) || !site_dir_column %in% names(obs_df)) {
      return(NULL)
    }
    
    raw_path <- obs_df[[site_dir_column]][row_index]
    if (is.na(raw_path) || raw_path == "") {
      return(NULL)
    }
    
    if (!is.null(base_dir)) {
      return(file.path(base_dir, raw_path))
    }
    
    return(raw_path)
  }
  
  # Save caller's working directory and restore on exit
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  
  # Track overall success (0 = success, 1 = failure)
  overall_status <- 0
  
  tryCatch({
    for (i in seq_len(nrow(obs_df))) {
      site_id <- if ("siteID" %in% names(obs_df)) obs_df$siteID[i] else NA
      
      # Determine working directory for this row, if provided
      row_wd <- get_row_working_dir(i)
      if (!is.null(row_wd)) {
        if (!dir.exists(row_wd)) {
          stop("Working directory for row ", i, " does not exist: ", row_wd)
        }
        setwd(row_wd)
        if (verbose) {
          cat("\n--- Row", i, "siteID:", ifelse(is.na(site_id), "<NA>", site_id),
              "working directory:", row_wd, "---\n")
        }
      } else if (verbose) {
        cat("\n--- Row", i, "siteID:", ifelse(is.na(site_id), "<NA>", site_id),
            "working directory (unchanged):", getwd(), "---\n")
      }
      
      # Track extended site file from previous schedule in the chain
      prev_ext_file <- initial_ext_file
      
      # Track files to clean up (intermediate schedules)
      intermediate_files <- list()
      
      # Get the last non-empty schedule column name
      last_schedule_col <- NULL
      for (col_idx in rev(seq_along(schedule_columns))) {
        col_name <- schedule_columns[col_idx]
        sch_file <- obs_df[[col_name]][i]
        sch_file <- trimws(ifelse(is.na(sch_file), "", sch_file))
        if (sch_file != "") {
          last_schedule_col <- col_name
          break
        }
      }
      
      for (col_idx in seq_along(schedule_columns)) {
        col_name <- schedule_columns[col_idx]
        sch_file <- obs_df[[col_name]][i]
        sch_file <- trimws(ifelse(is.na(sch_file), "", sch_file))
        
        if (sch_file == "") {
          if (verbose) {
            cat("  Skipping empty schedule in column", col_name, "for row", i, "\n")
          }
          next
        }
        
        # Determine base schedule name (without path and without .sch)
        sch_basename <- basename(sch_file)
        sch_base <- sub("\\.sch$", "", sch_basename)
        
        # Construct extended site.100 filename: <schedule_basename>_ext.100 (by default)
        ext_base <- paste0(sch_base, ext_suffix)
        ext_file <- paste0(ext_base, ".100")
        
        # Determine args for run_DayCent
        ext_read <- prev_ext_file
        
        # For the first schedule: when no initial_ext_file is provided, use site.100
        # named after the site (site_id.100), matching the schedule file convention
        if (is.null(ext_read) && !is.na(site_id) && trimws(as.character(site_id)) != "") {
          site100_file <- paste0(trimws(as.character(site_id)), ".100")
          if (file.exists(site100_file)) {
            ext_read <- site100_file
            if (verbose) {
              cat("    First schedule: using site-named site.100:", site100_file, "\n")
            }
          } else if (verbose) {
            cat("    First schedule: site.100 not found (", site100_file, "), DayCent will use schedule default\n", sep = "")
          }
        }
        
        ext_write <- ext_file
        
        # Track if this is the last schedule
        is_last_schedule <- (col_name == last_schedule_col)
        
        if (verbose) {
          cat("  Running schedule", sch_file, ifelse(is_last_schedule, "(LAST)", "(intermediate)"), "\n")
          if (!is.null(ext_read)) {
            cat("    Using extended site.100 as input :", ext_read, "\n")
          } else {
            cat("    No extended site.100 input for this schedule\n")
          }
          cat("    Writing extended site.100 to       :", ext_write, "\n")
        }
        
        run_status <- run_DayCent(
          filepath_exe = filepath_exe,
          sch_file = sch_file,
          ext_site100_2read = ext_read,
          ext_site100_2write = ext_write
        )
        
        # If any schedule fails, stop execution and return failure status
        # This matches run_DayCent behavior which stops on failure
        if (run_status != 0) {
          overall_status <- 1
          stop("DayCent run failed for schedule '", sch_file,
               "' (column ", col_name, ", row ", i, "). ",
               "See DayCent logs for details.")
        }
        
        # Track intermediate files for cleanup (if not keeping intermediate and not last schedule)
        # Note: We track files but don't delete them yet - we need the extended site.100 files
        # for the next schedule in the chain. We'll clean up after all schedules complete.
        if (!keep_intermediate && !is_last_schedule) {
          # Files to clean up: .bin, extended site.100, log files
          bin_file <- paste0(sch_base, ".bin")
          stdout_log <- paste0(sch_base, "_stdout.log")
          stderr_log <- paste0(sch_base, "_stderr.log")
          
          intermediate_files <- c(intermediate_files, list(
            bin = bin_file,
            ext_site100 = ext_file,
            stdout_log = stdout_log,
            stderr_log = stderr_log
          ))
        }
        
        # Update previous extended site file for next schedule in the chain
        prev_ext_file <- ext_file
      }
      
      # Clean up intermediate files AFTER all schedules complete successfully
      # This ensures we keep the extended site.100 files needed during chain execution
      if (!keep_intermediate && length(intermediate_files) > 0) {
        if (verbose) {
          cat("  Cleaning up intermediate output files from previous schedules...\n")
        }
        
        for (file_info in intermediate_files) {
          for (file_type in names(file_info)) {
            file_path <- file_info[[file_type]]
            if (file.exists(file_path)) {
              tryCatch({
                file.remove(file_path)
                if (verbose) {
                  cat("    Removed:", file_path, "\n")
                }
              }, error = function(e) {
                if (verbose) {
                  cat("    Warning: Could not remove", file_path, ":", e$message, "\n")
                }
              })
            }
          }
        }
        
        if (verbose) {
          cat("  Kept only the last schedule's output files.\n")
        }
      }
    }
  }, error = function(e) {
    if (verbose) {
      cat("ERROR: An error occurred during chained schedule execution: ", 
          conditionMessage(e), "\n")
    }
    overall_status <<- 1
  })
  
  # Return integer status code matching run_DayCent: 0 = success, 1 = failure
  return(overall_status)
}

##' Run Site Spinup Then Multiple Treatment Schedules
##'
##' For chained-schedule workflows (e.g. SOC calibration), runs all schedule
##' columns except the last once per site (equilibrium, base, etc.), then runs
##' each unique schedule in the last column as a treatment branch from the final
##' spinup extended site.100 file.
##'
##' @param filepath_exe Path to DayCent executable.
##' @param run_file_site Data frame of RunFile rows for a single site.
##' @param schedule_columns Character vector of schedule column names in run order.
##'   All columns except the last are site spinup; the last column lists treatments.
##' @param initial_ext_file Extended site.100 used as input for the first spinup schedule.
##' @param ext_suffix Suffix for extended site.100 files written during spinup (default \code{"_ext"}).
##' @param verbose Logical; print progress messages.
##' @return Integer \code{0} on success, \code{1} on failure (matches \code{run_DayCent()}).
##' @export
run_chained_site_spinup_daycent <- function(filepath_exe,
                                          run_file_site,
                                          schedule_columns,
                                          initial_ext_file,
                                          ext_suffix = "_ext",
                                          verbose = TRUE) {
  if (!file.exists(filepath_exe)) {
    stop("DayCent executable not found: ", filepath_exe)
  }
  if (nrow(run_file_site) < 1) {
    stop("run_file_site must contain at least one row")
  }
  if (is.null(schedule_columns) || length(schedule_columns) < 1) {
    stop("schedule_columns must contain at least one column name")
  }

  missing_cols <- setdiff(schedule_columns, names(run_file_site))
  if (length(missing_cols) > 0) {
    stop("run_file_site is missing schedule columns: ", paste(missing_cols, collapse = ", "))
  }

  spinup_cols <- schedule_columns[-length(schedule_columns)]
  treatment_col <- schedule_columns[length(schedule_columns)]
  spinup_row <- run_file_site[1, , drop = FALSE]

  prev_ext_file <- initial_ext_file
  overall_status <- 0

  if (verbose) {
    cat("Site spinup mode: running", length(spinup_cols), "spinup stage(s), then unique",
        treatment_col, "schedule(s)\n")
  }

  tryCatch({
    for (col_name in spinup_cols) {
      sch_file <- spinup_row[[col_name]][1]
      sch_file <- trimws(ifelse(is.na(sch_file), "", sch_file))
      if (sch_file == "") {
        if (verbose) {
          cat("  Skipping empty spinup schedule in column", col_name, "\n")
        }
        next
      }

      sch_base <- sub("\\.sch$", "", basename(sch_file))
      ext_file <- paste0(sch_base, ext_suffix, ".100")

      if (verbose) {
        cat("  Running site spinup schedule:", sch_file, "\n")
        if (!is.null(prev_ext_file)) {
          cat("    Using extended site.100 as input :", prev_ext_file, "\n")
        }
        cat("    Writing extended site.100 to       :", ext_file, "\n")
      }

      run_status <- run_DayCent(
        filepath_exe = filepath_exe,
        sch_file = sch_file,
        ext_site100_2read = prev_ext_file,
        ext_site100_2write = ext_file
      )
      if (run_status != 0) {
        overall_status <- 1
        stop("DayCent run failed for spinup schedule '", sch_file,
             "' (column ", col_name, "). See DayCent logs for details.")
      }
      prev_ext_file <- ext_file
    }

    trt_files <- trimws(as.character(run_file_site[[treatment_col]]))
    trt_files <- trt_files[!is.na(trt_files) & nzchar(trt_files)]
    trt_files <- sort(unique(trt_files))
    if (length(trt_files) == 0) {
      stop("No treatment schedules found in column ", treatment_col)
    }

    if (verbose) {
      cat("  Running", length(trt_files), "unique treatment schedule(s) from column",
          treatment_col, "\n")
    }

    for (trt_sch_file in trt_files) {
      if (verbose) {
        cat("  Running treatment schedule:", trt_sch_file, "\n")
        if (!is.null(prev_ext_file)) {
          cat("    Using extended site.100 as input :", prev_ext_file, "\n")
        }
      }

      run_status <- run_DayCent(
        filepath_exe = filepath_exe,
        sch_file = trt_sch_file,
        ext_site100_2read = prev_ext_file,
        ext_site100_2write = NULL
      )
      if (run_status != 0) {
        overall_status <- 1
        stop("DayCent run failed for treatment schedule '", trt_sch_file,
             "'. See DayCent logs for details.")
      }

      methane_out <- "methane.out"
      if (file.exists(methane_out)) {
        trt_stem <- sub("\\.sch$", "", basename(trt_sch_file))
        methane_copy <- paste0(trt_stem, "_methane.out")
        file.copy(from = methane_out, to = methane_copy, overwrite = TRUE)
        if (verbose) {
          cat("    Saved methane output to:", methane_copy, "\n")
        }
      }
    }
  }, error = function(e) {
    if (verbose) {
      cat("ERROR: Site spinup chained execution failed: ", conditionMessage(e), "\n")
    }
    overall_status <<- 1
  })

  return(overall_status)
}

