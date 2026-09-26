#!/usr/bin/env Rscript

#' @title Database Index Creation Script
#' @description Script to create indexes on crop database tables for better query performance
#' @author Claude Code
#' @date October 2025

# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")

# Load required libraries
library(DBI)
library(RMySQL)

cat("==============================================\n")
cat("Database Index Creation Script\n")
cat("==============================================\n")
cat("Purpose: Create indexes on crop database tables for better performance\n")
cat("==============================================\n")


# Configuration

Input.arg  = commandArgs(trailingOnly = TRUE)
id_chist     = as.numeric(Input.arg[1])

crop_mg <- NULL
all_period <- FALSE 
if (id_chist == 25) {
  crop = "corn" 
  crop_mg = c(2, 3, 4, 5, 6)
  all_period = TRUE
} else if (id_chist == 13) {
  crop = "soyb" 
  crop_mg = c(0, 1, 2, 3, 4, 5, 6)
  all_period = TRUE
} else if (id_chist == 27) {
  crop = "sorg" 
  crop_mg = c(1, 2, 3, 4, 5)
} else if (id_chist == 1) {             
  crop = "wheat_SW3"
  crop_mg <- NULL
} else if (id_chist == 2) {             
  crop = "wheat_W3"
  crop_mg <- NULL
} else if (id_chist == 3) {             
  crop = "wheat_W3SR"
  crop_mg <- NULL
} else if (id_chist == 4) {             
  crop = "wheat_W3HR"
  crop_mg <- NULL
} else if (id_chist == 14) {
  crop = "cott"
  crop_mg <- c(1, 2, 3, 4)
} else if (id_chist == 114) {
  crop = "barl"
  crop_mg <- NULL
} else if (id_chist == 112) {
  crop = "oats"
  crop_mg <- NULL
} else if (id_chist == 18) {
  crop = "pota"
  crop_mg <- NULL
} else if (id_chist == 15) {
  crop = "pnut"
  crop_mg <- NULL
} else if (id_chist == 17) {
  crop = "sugb"
  crop_mg <- NULL
}else if (id_chist == 113) {
  crop = "rice"
  crop_mg <- c(1, 2, 3)
} else if (id_chist == 990046) {
  crop = "swpo"
  crop_mg <- NULL
} else if (id_chist == 990052) {
  crop = "lent"
  crop_mg <- NULL
} else if (id_chist == 16) {            ## STATE NASS DATA 
  crop = "toba"
  crop_mg <- NULL
} else if (id_chist == 21) {             
  crop = "sunf"
  crop_mg <- NULL
} else if (id_chist == 990042) {             
  crop = "drbe"
  crop_mg <- NULL
} else if (id_chist == 990049) {             
  crop = "onio"
  crop_mg <- NULL
} else if (id_chist == 990053) {             
  crop = "peas"
  crop_mg <- NULL
} else if (id_chist == 990054) {             
  crop = "toma"
  crop_mg <- NULL
} else if (id_chist == 146) {   # HAY CROP
  crop = "hay" 
  crop_mg <- c("ALF", "ALF2", "G5", "G4", "G3", "GI3", "WC3", "G3CPI")
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

#' Create database indexes for crop tables
#' @param con Database connection
#' @param crop Crop name (e.g., "corn", "soyb")
#' @param crop_mg_i Crop maturity group number
#' @return TRUE if successful, FALSE otherwise
create_database_indexes <- function(con, crop, crop_mg_i) {
  cat("\n=== Creating Database Indexes for", crop, "maturity group", crop_mg_i, "===\n")
  
  success <- TRUE
  
  # Define table names
  if(crop == "corn" | crop == "soyb" | crop == "sorg" | crop == "rice" | crop == "hay" | crop == "cott") {
    if(all_period == TRUE) {
      run_order_table <- paste0("run_order_", crop, "_m", crop_mg_i, "_all_period")
      schedule_files_table <- paste0("schedule_files_", crop, "_m", crop_mg_i, "_all_period")
      site_files_table <- paste0("site_files_", crop, "_m", crop_mg_i, "_all_period")
    } else if(crop == "hay") {
      run_order_table <- paste0("run_order_", crop, "_", crop_mg_i)
      schedule_files_table <- paste0("schedule_files_", crop, "_", crop_mg_i)
      site_files_table <- paste0("site_files_", crop, "_", crop_mg_i)
    } else {
      run_order_table <- paste0("run_order_", crop, "_m", crop_mg_i)
      schedule_files_table <- paste0("schedule_files_", crop, "_m", crop_mg_i)
      site_files_table <- paste0("site_files_", crop, "_m", crop_mg_i)
    } 
  } else {
    run_order_table <- paste0("run_order_", crop)
    schedule_files_table <- paste0("schedule_files_", crop)
    site_files_table <- paste0("site_files_", crop) 
  }
  
  
  # Create indexes on run_order table
  cat("Creating indexes on", run_order_table, "...\n")
  tryCatch({
    # Check if table exists first
    if (dbExistsTable(con, run_order_table)) {
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table, "_site_name ON ", run_order_table, " (site_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table, "_active ON ", run_order_table, " (active)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table, "_weather_code ON ", run_order_table, " (weather_code)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table, "_aggregation_level ON ", run_order_table, " (aggregation_level)"))
       
      cat("✓ Created indexes on", run_order_table, "\n")
    } else {
      cat("⚠ Warning: Table", run_order_table, "does not exist, skipping indexes\n")
    }
  }, error = function(e) {
    cat("✗ Error creating indexes on", run_order_table, ":", conditionMessage(e), "\n")
    success <<- FALSE
  })
  
  # Create indexes on schedule_files table
  cat("Creating indexes on", schedule_files_table, "...\n")
  tryCatch({
    # Check if table exists first
    if (dbExistsTable(con, schedule_files_table)) {
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", schedule_files_table, "_site_name ON ", schedule_files_table, " (site_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", schedule_files_table, "_treatment_name ON ", schedule_files_table, " (treatment_name)"))
       
      cat("✓ Created indexes on", schedule_files_table, "\n")
    } else {
      cat("⚠ Warning: Table", schedule_files_table, "does not exist, skipping indexes\n")
    }
  }, error = function(e) {
    cat("✗ Error creating indexes on", schedule_files_table, ":", conditionMessage(e), "\n")
    success <<- FALSE
  })
  
  # Create indexes on site_files table
  cat("Creating indexes on", site_files_table, "...\n")
  tryCatch({
    # Check if table exists first
    if (dbExistsTable(con, site_files_table)) {
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", site_files_table, "_site_name ON ", site_files_table, " (site_name)"))
       
      cat("✓ Created indexes on", site_files_table, "\n")
    } else {
      cat("⚠ Warning: Table", site_files_table, "does not exist, skipping indexes\n")
    }
  }, error = function(e) {
    cat("✗ Error creating indexes on", site_files_table, ":", conditionMessage(e), "\n")
    success <<- FALSE
  })
  
  return(success)
}

#' Create database indexes for crop tables
#' @param con Database connection
#' @param crop Crop name (e.g., "corn", "soyb")
#' @param crop_mg_i Crop maturity group number
#' @return TRUE if successful, FALSE otherwise
create_database_indexes_evaluation <- function(con, crop, crop_mg_i) {
  cat("\n=== Creating Database Indexes for", crop, "maturity group", crop_mg_i, "===\n")
  
  success <- TRUE
  
  # Define table names 
  if(crop == "corn" | crop == "soyb" | crop == "sorg" | crop == "rice" | crop == "hay" | crop == "cott") {
    if(all_period == TRUE) {
      run_order_table <- paste0("run_order_", crop, "_m", crop_mg_i, "_all_period")
      schedule_files_table <- paste0("schedule_files_", crop, "_m", crop_mg_i, "_all_period")
      site_files_table <- paste0("site_files_", crop, "_m", crop_mg_i, "_all_period")
    } else if(crop == "hay") {
      run_order_table <- paste0("run_order_", crop, "_", crop_mg_i)
      schedule_files_table <- paste0("schedule_files_", crop, "_", crop_mg_i)
      site_files_table <- paste0("site_files_", crop, "_", crop_mg_i)
    } else {
      run_order_table <- paste0("run_order_", crop, "_m", crop_mg_i)
      schedule_files_table <- paste0("schedule_files_", crop, "_m", crop_mg_i)
      site_files_table <- paste0("site_files_", crop, "_m", crop_mg_i)
    }
  } else {
    run_order_table <- paste0("run_order_", crop)
    schedule_files_table <- paste0("schedule_files_", crop)
    site_files_table <- paste0("site_files_", crop) 
  }
  
  # Create indexes on run_order table
  cat("Creating indexes on", run_order_table, "...\n")
  tryCatch({
    # Check if table exists first
    if (dbExistsTable(con, run_order_table)) { 
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table_evaluate, "_site_name ON ", run_order_table_evaluate, " (site_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table_evaluate, "_active ON ", run_order_table_evaluate, " (active)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table_evaluate, "_weather_code ON ", run_order_table_evaluate, " (weather_code)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", run_order_table_evaluate, "_aggregation_level ON ", run_order_table_evaluate, " (aggregation_level)"))
      cat("✓ Created indexes on", run_order_table_evaluate, "\n")
    } else {
      cat("⚠ Warning: Table", run_order_table_evaluate, "does not exist, skipping indexes\n")
    }
  }, error = function(e) {
    cat("✗ Error creating indexes on", run_order_table_evaluate, ":", conditionMessage(e), "\n")
    success <<- FALSE
  })
  
  # Create indexes on schedule_files table
  cat("Creating indexes on", schedule_files_table, "...\n")
  tryCatch({
    # Check if table exists first
    if (dbExistsTable(con, schedule_files_table)) { 
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", schedule_files_table_evaluate, "_site_name ON ", schedule_files_table_evaluate, " (site_name)"))
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", schedule_files_table_evaluate, "_treatment_name ON ", schedule_files_table_evaluate, " (treatment_name)"))
      cat("✓ Created indexes on", schedule_files_table_evaluate, "\n")
    } else {
      cat("⚠ Warning: Table", schedule_files_table_evaluate, "does not exist, skipping indexes\n")
    }
  }, error = function(e) {
    cat("✗ Error creating indexes on", schedule_files_table_evaluate, ":", conditionMessage(e), "\n")
    success <<- FALSE
  })
  
  # Create indexes on site_files table
  cat("Creating indexes on", site_files_table, "...\n")
  tryCatch({
    # Check if table exists first
    if (dbExistsTable(con, site_files_table)) { 
      dbExecute(con, paste0("CREATE INDEX IF NOT EXISTS idx_", site_files_table_evaluate, "_site_name ON ", site_files_table_evaluate, " (site_name)"))
      cat("✓ Created indexes on", site_files_table_evaluate,"\n")
    } else {
      cat("⚠ Warning: Table", site_files_table_evaluate, "does not exist, skipping indexes\n")
    }
  }, error = function(e) {
    cat("✗ Error creating indexes on", site_files_table_evaluate, ":", conditionMessage(e), "\n")
    success <<- FALSE
  })
  
  return(success)
}

# Main execution  
  cat("\nProcessing crop:", crop, "\n")
  cat("Maturity groups:", paste(crop_mg, collapse = ", "), "\n")
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
  
  overall_success <- TRUE
  
  # Process each maturity group
  if(crop == "corn" | crop == "soyb" | crop == "sorg" | crop == "rice" | crop == "hay" | crop == "cott") {
    for (crop_mg_i in crop_mg) {
      cat("----------------------\n")
      cat("Processing maturity group:", crop_mg_i, "\n")
      cat("----------------------\n")
      
      success <- create_database_indexes(con, crop, crop_mg_i)
      
      if (success) {
        cat("✅ Successfully created indexes for maturity group", crop_mg_i, "\n")
      } else {
        cat("❌ Failed to create some indexes for maturity group", crop_mg_i, "\n")
        overall_success <- FALSE
      }
      
      cat("\n")
    }
  } else {
     
      cat("----------------------\n")
      cat("Processing crop: ", crop, "\n")
      cat("----------------------\n")
      crop_mg_i = NULL
      success <- create_database_indexes(con, crop, crop_mg_i)
      
      if (success) {
        cat("✅ Successfully created indexes for crop: ", crop ,"\n")
      } else {
        cat("❌ Failed to create some indexes for crop ", crop ,"\n")
        overall_success <- FALSE
      }
      
      cat("\n") 
  }
    
  
  # Final summary
  cat("==============================================\n")
  if (overall_success) {
    cat("✅ INDEX CREATION COMPLETE\n")
    cat("==============================================\n")
    cat("\nSummary:\n")
    cat("- Processed", length(crop_mg), "maturity groups for", crop, "\n")
    cat("- Created indexes on run_order, schedule_files, and site_files tables\n")
    cat("- Indexes improve query performance for database operations\n")
    cat("\nIndexes created:\n")
    cat("- site_name indexes (all tables)\n")
    cat("- treatment_name indexes (schedule_files)\n")
    cat("- active, weather_code, aggregation_level indexes (run_order)\n")
  } else {
    cat("❌ INDEX CREATION COMPLETED WITH ERRORS\n")
    cat("==============================================\n")
    cat("Some indexes may not have been created. Check error messages above.\n")
  }


