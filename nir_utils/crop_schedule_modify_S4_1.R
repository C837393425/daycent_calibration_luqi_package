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
} else if (id_chist == 114) {
  crop = "barl"
  maturity_groups <- NULL
} else if (id_chist == 112) {
  crop = "oats"
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
}else if (id_chist == 18) {
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
} else if (id_chist == 146) {   # HAY CROP
  crop = "hay" 
  maturity_groups <- c("ALF", "ALF2", "G5", "G4", "G3", "GI3", "WC3", "G3CPI")
} 

schedule_update_flag <- FALSE  # crop name change
weather_type <- "DayMet"


cat("==============================================\n")
cat("Crop Schedule File Modifier (non-interactive)\n")
cat("==============================================\n")
cat("Target column: schedule_file_data\n") 
cat("Change:        'CROP C{2-6}' -> 'CROP CM{2-6}_11' for Corn \n") #corn 
cat("Change:        'CROP SM{0-6}' -> 'CROP SM{0-6}_11' for Soyb SIR \n") #soyb
cat("Change:        'CROP SORG' -> 'CROP SORGM' for Sorg SIR \n") #sorg
cat("Change:        'weather.wth' -> 'weather_code.wth' for DayMet \n") #DayMet
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
if(schedule_update_flag == TRUE) {
  ## ---- Main loop ----
  updated <- list()

  cat("Processing crop:", crop, "\n")
  cat("Maturity groups:", paste(maturity_groups, collapse = ", "), "\n\n")
 
  for (mg in maturity_groups) {

    if(crop == "corn") {
      target_column <- "schedule_file_data"
      target_crop_code <- paste0("CM", mg, "_11")
      pattern <- paste0("CROP\\s+CM", mg, "\\b")               # match CM{maturity_group} after 'CROP'
      replacement <- paste0("CROP ", target_crop_code)
    }else if(crop == "soyb") {
      target_column <- "schedule_file_data"
      target_crop_code <- paste0("SM", mg, "_11")
      pattern <- paste0("CROP\\s+SM", mg, "\\b")               
      replacement <- paste0("CROP ", target_crop_code)
    }else if(crop == "sorg") {
      target_column <- "schedule_file_data"
      target_crop_code <- paste0("SORGM")
      pattern <- paste0("CROP\\s+SORG\\b")               
      replacement <- paste0("CROP ", target_crop_code)
    }

    cat("----------------------\n")
    cat("Processing maturity group:", mg, "\n")
    cat("----------------------\n")
    table_name <- paste0("schedule_files_", crop, "_m", mg)
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
    cat("  Rows matching pattern before change:", pre_rows_with_c6, "\n")

    # Modify
    cat("  Modifying codes in '", target_column, "'...\n", sep = "")
    df_mod <- modify_crop_codes(df, target_column, pattern, replacement)

    # Verify in-memory
    post_leftover <- sum(grepl(pattern, df_mod[[target_column]], perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
    new_rows <- sum(grepl(paste0("CROP\\s+", target_crop_code, "\\b"), df_mod[[target_column]],
                          perl = TRUE, ignore.case = TRUE), na.rm = TRUE)
    cat("  Verification:\n")
    cat("    Leftover rows matching pattern:", post_leftover, "\n")
    cat("    Rows with 'CROP ", target_crop_code, "': ", new_rows, "\n", sep = "")

    # Write (no prompt)
    cat("  Writing changes to database...\n")
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
  cat("- Modified 'CROP SM0_01' → 'CROP ", target_crop_code, "' in '", target_column, "'\n", sep = "")
  cat("- Modified 'CROP SORG' → 'CROP ", target_crop_code, "' in '", target_column, "'\n", sep = "")
  if (length(updated)) {
    cat("- Updated tables:\n  ",
        paste(updated, collapse = ", "), "\n", sep = "")
  } else {
    cat("- No tables were updated.\n")
  }

}


#----------------------------------------------------------------------------------------------------------------------------------------
# update the DayMet weather file in the schedule file
if(weather_type == "DayMet") {
 
  # Helper function for quoting identifiers (needed for update_database_table)
  qi <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
  
  
  cat("\n=== STEP 1: Connect to Source Database ===\n")
  source_con <- connect_to_database('inv2024_calib')
  if (is.null(source_con)) {
    cat("Cannot proceed without source database connection\n")
    return(FALSE)
  }
  
  if(crop == "corn" | crop == "soyb" | crop == "wheat" | crop == "sorg" | crop == "rice" | crop == "hay" | crop == "cott") {
    for(crop_mg_i in maturity_groups) {
      cat("\n", "----------------------", "Processing crop maturity group:", crop_mg_i, "----------------------", "\n")
      
      # run order table
      {
        # Query existing run order table
        if(all_period == TRUE) {
          query <- paste0(" SELECT * FROM run_order_", crop, "_m", crop_mg_i, "_all_period")
        } else if (all_period == FALSE & !crop %in% c("wheat", "hay")) {
          query <- paste0(" SELECT * FROM run_order_", crop, "_m", crop_mg_i)
        } else if(crop == "wheat" | crop == "hay") {
          query <- paste0(" SELECT * FROM run_order_", crop, "_", crop_mg_i)
        }
        
        run_order <- tryCatch({
          dbGetQuery(source_con, query)
        }, error = function(e) {
          cat("✗ Error extracting run order:", conditionMessage(e), "\n")
          return(NULL)
        })
        
      }
      
      # schedule file
      {
        # Query existing run order table
        if(all_period == TRUE) {
          query <- paste0(" SELECT * FROM schedule_files_", crop, "_m", crop_mg_i, "_all_period")
        } else if (all_period == FALSE & !crop %in% c("wheat", "hay")) {
          query <- paste0(" SELECT * FROM schedule_files_", crop, "_m", crop_mg_i)
        } else if(crop == "wheat" | crop == "hay") {
          query <- paste0(" SELECT * FROM schedule_files_", crop, "_", crop_mg_i)
        }
        schedule_files <- tryCatch({
          dbGetQuery(source_con, query)
        }, error = function(e) {
          cat("✗ Error extracting schedule files:", conditionMessage(e), "\n")
          return(NULL)
        })
      }
      
      # Update weather file in schedule_file_data
      {
        if (is.null(run_order) || is.null(schedule_files)) {
          cat("✗ Skipping weather update: missing run_order or schedule_files\n")
          next
        }
        
        cat("  Updating weather files in schedule_file_data...\n")
        cat("    run_order rows:", nrow(run_order), "\n")
        cat("    schedule_files rows:", nrow(schedule_files), "\n")
        
        # Create lookup: site_name -> weather_code from run_order
        weather_lookup <- setNames(run_order$weather_code, run_order$site_name)
        
        # Track modifications
        modified_count <- 0
        
        for (i in seq_len(nrow(schedule_files))) {
          site_name <- schedule_files$site_name[i] 
          
          # Find matching weather_code from run_order
          if (site_name %in% names(weather_lookup)) {
            weather_code <- weather_lookup[[site_name]]
            
            if (!is.na(weather_code) && nzchar(weather_code)) {
              # Get the schedule file data
              sched_data <- schedule_files$schedule_file_data[i]
              
              if (!is.na(sched_data) && nzchar(sched_data)) {
                # Replace placeholder weather file (e.g., "weather.wth") with site-specific file
                new_weather_file <- paste0(weather_code, ".wth")
                
                # Pattern to match existing weather file placeholder "weather.wth"
                weather_pattern <- "weather\\.wth"
                
                if (grepl(weather_pattern, sched_data, perl = TRUE)) {
                  # Show example before modification (only first 3)
                  if (modified_count < 3) {
                    old_match <- regmatches(sched_data, regexpr(weather_pattern, sched_data, perl = TRUE))
                    cat(sprintf("      Row %d (site_name=%s): %s -> %s\n", 
                                i, site_name, old_match, new_weather_file))
                  }
                  
                  schedule_files$schedule_file_data[i] <- gsub(
                    weather_pattern, 
                    new_weather_file, 
                    sched_data, 
                    perl = TRUE
                  )
                  modified_count <- modified_count + 1
                }
              }
            }
          }
        }
        
        cat("    ✓ Modified weather files in", modified_count, "rows\n")
        
        # Update database table with modified schedule_files
        if (modified_count > 0) {
          cat("  Writing changes to database...\n")
          table_name <- paste0("schedule_files_", crop, "_m", crop_mg_i)
          if(crop == "wheat" | crop == "hay") {
            table_name <- paste0("schedule_files_", crop, "_", crop_mg_i)
          }
          if (update_database_table(source_con, table_name, schedule_files)) {
            cat("  ✅ Successfully updated", table_name, "with new weather files\n")
          } else {
            cat("  ❌ Failed to update", table_name, "\n")
          }
        } else {
          cat("  ⚠️  No weather file modifications needed\n")
        }
      }
      
    }
  } else { 
    cat("\n", "----------------------", "Processing crop:", crop, "----------------------", "\n")
    
    # run order table
    {
      # Query existing run order table
      query <- paste0(" SELECT * FROM run_order_", crop)
      
      run_order <- tryCatch({
        dbGetQuery(source_con, query)
      }, error = function(e) {
        cat("✗ Error extracting run order:", conditionMessage(e), "\n")
        return(NULL)
      })
      
    }
    
    # schedule file
    {
      # Query existing run order table
      query <- paste0(" SELECT * FROM schedule_files_", crop)
      
      schedule_files <- tryCatch({
        dbGetQuery(source_con, query)
      }, error = function(e) {
        cat("✗ Error extracting schedule files:", conditionMessage(e), "\n")
        return(NULL)
      })
    }
    
    # Update weather file in schedule_file_data
    {
      if (is.null(run_order) || is.null(schedule_files)) {
        cat("✗ Skipping weather update: missing run_order or schedule_files\n")
        next
      }
      
      cat("  Updating weather files in schedule_file_data...\n")
      cat("    run_order rows:", nrow(run_order), "\n")
      cat("    schedule_files rows:", nrow(schedule_files), "\n")
      
      # Create lookup: site_name -> weather_code from run_order
      weather_lookup <- setNames(run_order$weather_code, run_order$site_name)
      
      # Track modifications
      modified_count <- 0
      
      for (i in seq_len(nrow(schedule_files))) {
        site_name <- schedule_files$site_name[i] 
        
        # Find matching weather_code from run_order
        if (site_name %in% names(weather_lookup)) {
          weather_code <- weather_lookup[[site_name]]
          
          if (!is.na(weather_code) && nzchar(weather_code)) {
            # Get the schedule file data
            sched_data <- schedule_files$schedule_file_data[i]
            
            if (!is.na(sched_data) && nzchar(sched_data)) {
              # Replace placeholder weather file (e.g., "weather.wth") with site-specific file
              new_weather_file <- paste0(weather_code, ".wth")
              
              # Pattern to match existing weather file placeholder "weather.wth"
              weather_pattern <- "weather\\.wth"
              
              if (grepl(weather_pattern, sched_data, perl = TRUE)) {
                # Show example before modification (only first 3)
                if (modified_count < 3) {
                  old_match <- regmatches(sched_data, regexpr(weather_pattern, sched_data, perl = TRUE))
                  cat(sprintf("      Row %d (site_name=%s): %s -> %s\n", 
                              i, site_name, old_match, new_weather_file))
                }
                
                schedule_files$schedule_file_data[i] <- gsub(
                  weather_pattern, 
                  new_weather_file, 
                  sched_data, 
                  perl = TRUE
                )
                modified_count <- modified_count + 1
              }
            }
          }
        }
      }
      
      cat("    ✓ Modified weather files in", modified_count, "rows\n")
      
      # Update database table with modified schedule_files
      if (modified_count > 0) {
        cat("  Writing changes to database...\n")
        table_name <- paste0("schedule_files_", crop)
        if (update_database_table(source_con, table_name, schedule_files)) {
          cat("  ✅ Successfully updated", table_name, "with new weather files\n")
        } else {
          cat("  ❌ Failed to update", table_name, "\n")
        }
      } else {
        cat("  ⚠️  No weather file modifications needed\n")
      }
    }
    
  }
  
  
  # Disconnect after loop completes
  dbDisconnect(source_con)
  
}

