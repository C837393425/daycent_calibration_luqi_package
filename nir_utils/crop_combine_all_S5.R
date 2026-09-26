#!/usr/bin/env Rscript

#' @title Crop Tables Combiner Across Maturity Groups
#' @description Combine run_order, site_files, and schedule_files tables across corn maturity groups
#' @author Claude Code
#' @date October 2025

# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")

# Load required libraries
library(DBI)
library(RMySQL)
library(dplyr)

cat("==============================================\n")
cat("Crop Tables Combiner Across Maturity Groups\n")
cat("==============================================\n")
cat("Purpose: Combine tables across corn maturity groups into unified tables\n")
cat("Tables: run_order, site_files, schedule_files\n")
cat("==============================================\n")


Input.arg  = commandArgs(trailingOnly = TRUE)
job_id     = as.numeric(Input.arg[1])
if(job_id == 1) {
  id_chist <- 25
} else if(job_id == 2) {
  id_chist <- 13
} else if(job_id == 3) {
  id_chist <- 111
} else if(job_id == 4) {
  id_chist <- 27
}  
# corn = 25
# soyb = 13
# sorg = 27
crop_mg <- NULL
if (id_chist == 25) {
  crop = "corn" 
  maturity_groups = c(2, 3, 4, 5, 6)
} else if (id_chist == 13) {
  crop = "soyb"  
  maturity_groups = c(0, 1, 2, 3, 4, 5, 6)
} else if (id_chist == 111) {
  crop = "wheat"  
  maturity_groups = c("W3SR", "W3HR")
} else if (id_chist == 27) {
  crop = "sorg"  
  maturity_groups = c(1, 2, 3, 4, 5)
} 

# Database connection parameters
cred_file <- "~/.dblogin"
cred <- readLines(cred_file)

connect_to_database <- function(dbname) {
  tryCatch({
    con <- dbConnect(
      MySQL(),
      host = 'trillium.nrel.colostate.edu',
      dbname = dbname,
      username = cred[1],
      password = cred[2]
    )
    cat("✓ Connected to database:", dbname, "\n")
    return(con)
  }, error = function(e) {
    cat("✗ Failed to connect to database:", dbname, "\n")
    cat("  Error:", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Configuration

cat("\nProcessing crop:", crop, "\n")
cat("Maturity groups:", paste(maturity_groups, collapse = ", "), "\n")
cat("==============================================\n\n")

# Connect to database
con <- connect_to_database('inv2024_calib')
if (is.null(con)) {
  cat("Cannot proceed without database connection\n")
  quit(status = 1)
}

# Ensure cleanup of connection
on.exit({
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
    cat("\n✓ Disconnected from database\n")
  }
})

#' Read table from database with error handling
#'
#' @param con Database connection
#' @param table_name Name of table to read
#' @param maturity_group Maturity group number for logging
#' @return Data frame or NULL if error
read_table_safe <- function(con, table_name, maturity_group = NULL) {
  tryCatch({
    if (dbExistsTable(con, table_name)) {
      data <- dbReadTable(con, table_name)
      mg_text <- if (!is.null(maturity_group)) paste0(" (m", maturity_group, ")") else ""
      cat("  ✓ Read", nrow(data), "records from", table_name, mg_text, "\n")
      return(data)
    } else {
      cat("  ⚠️  Table", table_name, "does not exist\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("  ✗ Error reading", table_name, ":", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Write combined table to database with backup and cleanup
#'
#' @param con Database connection
#' @param table_name Name of target table
#' @param data Data frame to write
#' @param backup_suffix Suffix for backup table
#' @return TRUE if successful, FALSE otherwise
write_combined_table <- function(con, table_name, data, backup_suffix = NULL) {
  tryCatch({
    # Check if combined table already exists and remove it
    if (dbExistsTable(con, table_name)) {
      cat("  Found existing combined table:", table_name, "\n")
      
      # Create backup if backup_suffix is provided
      if (!is.null(backup_suffix)) {
        backup_table <- paste0(table_name, "_backup_", backup_suffix)
        
        if (!dbExistsTable(con, backup_table)) {
          dbExecute(con, paste0("CREATE TABLE ", backup_table, " AS SELECT * FROM ", table_name))
          cat("  ✓ Created backup table:", backup_table, "\n")
        } else {
          cat("  ⚠️  Backup table already exists:", backup_table, "\n")
        }
      }
      
      # Drop existing combined table
      dbExecute(con, paste0("DROP TABLE ", table_name))
      cat("  ✓ Removed existing combined table:", table_name, "\n")
    }
    
    # Write new combined table
    dbWriteTable(con, table_name, data, row.names = FALSE)
    cat("  ✓ Created new combined table:", table_name, "with", nrow(data), "records\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("  ✗ Error writing combined table", table_name, ":", conditionMessage(e), "\n")
    return(FALSE)
  })
}

#' Create indexes on combined table
#'
#' @param con Database connection
#' @param table_name Name of table
#' @param table_type Type of table (run_order, site_files, schedule_files)
#' @return TRUE if successful, FALSE otherwise
create_combined_table_indexes <- function(con, table_name, table_type) {
  tryCatch({
    cat("  Creating indexes on", table_name, "...\n")
    
    if (table_type == "run_order") {
      # Indexes for run_order table
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_site_name ON ", table_name, " (site_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_active ON ", table_name, " (active)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_weather_code ON ", table_name, " (weather_code)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_aggregation_level ON ", table_name, " (aggregation_level)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_maturity_group ON ", table_name, " (maturity_group)"))
      
    } else if (table_type == "schedule_files") {
      # Indexes for schedule_files table
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_site_name ON ", table_name, " (site_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_treatment_name ON ", table_name, " (treatment_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_maturity_group ON ", table_name, " (maturity_group)"))
      
    } else if (table_type == "site_files") {
      # Indexes for site_files table
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_site_name ON ", table_name, " (site_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", table_name, "_maturity_group ON ", table_name, " (maturity_group)"))
    }
    
    cat("  ✓ Created indexes on", table_name, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("  ✗ Error creating indexes on", table_name, ":", conditionMessage(e), "\n")
    return(FALSE)
  })
}

#' Update execution_order column in run_order table
#'
#' @param con Database connection
#' @param table_name Name of run_order table
#' @return TRUE if successful, FALSE otherwise
update_execution_order <- function(con, table_name) {
  tryCatch({
    cat("  Updating execution_order column in", table_name, "...\n")
    
    # Get total number of rows
    result <- dbGetQuery(con, paste0("SELECT COUNT(*) as total FROM ", table_name))
    total_rows <- result$total
    
    cat("  Total rows:", total_rows, "\n")
    
    # Update execution_order to be sequential row numbers
    # We'll use a user variable to generate sequential numbers
    update_query <- paste0(
      "SET @row_number = 0; ",
      "UPDATE ", table_name, " SET execution_order = (@row_number := @row_number + 1)"
    )
    
    # Execute the update
    dbExecute(con, "SET @row_number = 0")
    rows_updated <- dbExecute(con, paste0("UPDATE ", table_name, " SET execution_order = (@row_number := @row_number + 1)"))
    
    cat("  ✓ Updated execution_order for", rows_updated, "rows\n")
    
    # Verify the update
    check_result <- dbGetQuery(con, paste0("SELECT MIN(execution_order) as min_order, MAX(execution_order) as max_order FROM ", table_name))
    cat("  ✓ Execution order range:", check_result$min_order, "to", check_result$max_order, "\n")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("  ✗ Error updating execution_order in", table_name, ":", conditionMessage(e), "\n")
    return(FALSE)
  })
}

#' Check and clean up existing combined tables
#'
#' @param con Database connection
#' @param crop Crop name
#' @return Number of tables cleaned up
cleanup_existing_combined_tables <- function(con, crop) {
  
  cat("Checking for existing combined tables...\n")
  
  # Define combined table names
  combined_tables <- c(
    paste0("run_order_", crop),
    paste0("schedule_files_", crop),
    paste0("site_files_", crop)
  )
  
  cleanup_count <- 0
  backup_suffix <- format(Sys.Date(), "%Y%m%d")
  
  for (table_name in combined_tables) {
    if (dbExistsTable(con, table_name)) {
      cat("  Found existing combined table:", table_name, "\n")
      
      # Create backup before cleanup
      backup_table <- paste0(table_name, "_backup_", backup_suffix)
      
      if (!dbExistsTable(con, backup_table)) {
        dbExecute(con, paste0("CREATE TABLE ", backup_table, " AS SELECT * FROM ", table_name))
        cat("    ✓ Created backup:", backup_table, "\n")
      } else {
        cat("    ⚠️  Backup already exists:", backup_table, "\n")
      }
      
      # Drop existing combined table
      dbExecute(con, paste0("DROP TABLE ", table_name))
      cat("    ✓ Removed existing table:", table_name, "\n")
      
      cleanup_count <- cleanup_count + 1
    }
  }
  
  if (cleanup_count > 0) {
    cat("  Cleaned up", cleanup_count, "existing combined tables\n")
  } else {
    cat("  No existing combined tables found\n")
  }
  
  cat("\n")
  return(cleanup_count)
}

# Main processing function
combine_tables_across_maturity_groups <- function() {
  
  # Define table types to process
  table_types <- c("run_order", "schedule_files", "site_files")
  
  for (table_type in table_types) {
    cat("----------------------\n")
    cat("Processing", table_type, "tables\n")
    cat("----------------------\n")
    
    # Initialize combined data
    combined_data <- data.frame()
    successful_reads <- 0
    
    # Read data from each maturity group
    for (mg in maturity_groups) {
      table_name <- paste0(table_type, "_", crop, "_m", mg)
      
      cat("Reading", table_name, "...\n")
      mg_data <- read_table_safe(con, table_name, mg)
      
      if (!is.null(mg_data) && nrow(mg_data) > 0) {
        # Add maturity group column
        mg_data$maturity_group <- mg
        
        # Add source table column for traceability
        mg_data$source_table <- table_name
        
        # Combine with overall dataset
        if (nrow(combined_data) == 0) {
          combined_data <- mg_data
        } else {
          # Ensure column compatibility
          common_cols <- intersect(names(combined_data), names(mg_data))
          if (length(common_cols) > 0) {
            combined_data <- rbind(
              combined_data[, common_cols],
              mg_data[, common_cols]
            )
          } else {
            cat("  ⚠️  No common columns between maturity groups for", table_type, "\n")
          }
        }
        
        successful_reads <- successful_reads + 1
      }
    }
    if(crop == "wheat") {
      for (mg in maturity_groups) {
        table_name <- paste0(table_type, "_", crop, "_", mg)
        
        cat("Reading", table_name, "...\n")
        mg_data <- read_table_safe(con, table_name, mg)
        
        if (!is.null(mg_data) && nrow(mg_data) > 0) {
          # Add maturity group column
          mg_data$maturity_group <- mg
          
          # Add source table column for traceability
          mg_data$source_table <- table_name
          
          # Combine with overall dataset
          if (nrow(combined_data) == 0) {
            combined_data <- mg_data
          } else {
            # Ensure column compatibility
            common_cols <- intersect(names(combined_data), names(mg_data))
            if (length(common_cols) > 0) {
              combined_data <- rbind(
                combined_data[, common_cols],
                mg_data[, common_cols]
              )
            } else {
              cat("  ⚠️  No common columns between maturity groups for", table_type, "\n")
            }
          }
          
          successful_reads <- successful_reads + 1
        }
      }
    }
    
    # Write combined table if we have data
    if (nrow(combined_data) > 0) {
      cat("\nCombining", table_type, "data...\n")
      cat("  Total records:", nrow(combined_data), "\n")
      cat("  Maturity groups processed:", successful_reads, "of", length(maturity_groups), "\n")
      
      # Define combined table name
      combined_table_name <- paste0(table_type, "_", crop)
      
      # Write to database (with backup)
      backup_suffix <- format(Sys.Date(), "%Y%m%d")
      success <- write_combined_table(con, combined_table_name, combined_data, backup_suffix)
      
      if (success) {
        # Create indexes
        create_combined_table_indexes(con, combined_table_name, table_type)
        
        # Update execution_order for run_order table
        if (table_type == "run_order") {
          update_execution_order(con, combined_table_name)
        }
        
        cat("  ✅ Successfully created combined table:", combined_table_name, "\n")
      } else {
        cat("  ❌ Failed to create combined table:", combined_table_name, "\n")
      }
      
    } else {
      cat("  ⚠️  No data found for", table_type, "across any maturity groups\n")
    }
    
    cat("\n")
  }
}

# Execute main processing
cat("Starting table combination process...\n\n")

# Clean up existing combined tables first
cleanup_existing_combined_tables(con, crop)

combine_tables_across_maturity_groups()

# Summary report
cat("==============================================\n")
cat("✅ TABLE COMBINATION COMPLETE\n")
cat("==============================================\n")

# Check final combined tables
cat("\nFinal combined tables:\n")
combined_tables <- c(
  paste0("run_order_", crop),
  paste0("schedule_files_", crop),
  paste0("site_files_", crop)
)

for (table_name in combined_tables) {
  if (dbExistsTable(con, table_name)) {
    result <- dbGetQuery(con, paste0("SELECT COUNT(*) as count, COUNT(DISTINCT maturity_group) as mg_count FROM ", table_name))
    cat("  ✓", table_name, ":", result$count, "records across", result$mg_count, "maturity groups\n")
  } else {
    cat("  ✗", table_name, ": Table not found\n")
  }
}

cat("\nSummary:\n")
cat("- Combined tables across", length(maturity_groups), "maturity groups for", crop, "\n")
cat("- Created backup tables before combining\n")
cat("- Added maturity_group and source_table columns for traceability\n")
cat("- Created indexes for optimal query performance\n")
cat("- Updated execution_order in run_order table with sequential row numbers\n")
cat("\nCombined table naming pattern: [table_type]_", crop, "\n")
cat("Backup tables created with format: [original_table]_backup_YYYYMMDD\n")
cat("Indexes include maturity_group for efficient filtering\n")
