#!/usr/bin/env Rscript

#' @title Populate Template Database from Existing Tables
#' @description Script to extract data from existing database tables and populate template tables
#' @author Claude Code
#' @date September 2025


# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")

# Load required libraries
library(DBI)
library(RMySQL)
library(dplyr)


cat("==============================================\n")
cat("Database Template Population Script\n")
cat("==============================================\n")

# Database connection parameters (modify these for your setup)
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


#' Extract and transform schedule file data
#' @param source_con Source database connection
#' @param recordids Vector of recordid2017 values to filter
#' @return Transformed schedule file data
extract_schedule_files <- function(source_con, recordids) {
  cat("\nExtracting schedule files...\n")
  
  # Query existing schedule file table
  query <- "
  SELECT 
    recordid2017,
    schl_data
  FROM SchlFiles_INV2022_LAIRice_PRDX_2011_2020_SEP2025
  WHERE recordid2017 IN ({recordids*})
  "
  
  result <- tryCatch({
    dbGetQuery(source_con, glue::glue_sql(query, recordids = recordids, .con = source_con))
  }, error = function(e) {
    cat("✗ Error extracting schedule files:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (is.null(result) || nrow(result) == 0) {
    cat("✗ No schedule files found for specified recordids\n")
    return(NULL)
  }
  
  # Transform to template format
  schedule_data <- result %>%
    mutate(
      site_name = as.character(recordid2017),
      # Create treatment names based on run parameters
      treatment_name = NA,
      
      # Use schl_data as schedule_file_data
      schedule_file_data = schl_data,
      
      # Add metadata
      notes = paste0("Migrated from recordid2017=", recordid2017)) %>%
    select(site_name, treatment_name, schedule_file_data, notes)
  
  cat("✓ Extracted", nrow(schedule_data), "schedule files\n")
  return(schedule_data)
}

#' Extract and transform site file data
#' @param source_con Source database connection  
#' @param recordids Vector of recordid2017 values to filter
#' @return Transformed site file data
extract_site_files <- function(source_con, recordids) {
  cat("\nExtracting site files...\n")
  
  # Query existing site file table
  query <- "
  SELECT 
    recordid2017,
    ext_site100
  FROM ExtSite_LAIRice_EndNRI_2010_SEP2025
  WHERE recordid2017 IN ({recordids*})
  "
  
  result <- tryCatch({
    dbGetQuery(source_con, glue::glue_sql(query, recordids = recordids, .con = source_con))
  }, error = function(e) {
    cat("✗ Error extracting site files:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (is.null(result) || nrow(result) == 0) {
    cat("✗ No site files found for specified recordids\n")
    return(NULL)
  }
  
  # Transform to template format
  site_data <- result %>%
    mutate(
      site_name = as.character(recordid2017),
      site_file_data = ext_site100,
      notes = paste0("Migrated from recordid2017=", recordid2017) ) %>%
    select(site_name, site_file_data, notes)
  
  cat("✓ Extracted", nrow(site_data), "site files\n")
  return(site_data)
}

#' Extract and transform run order data
#' @param source_con Source database connection
#' @param recordids Vector of recordid2017 values to filter  
#' @return Transformed run order data
extract_run_order <- function(source_con, recordids, rid_df_area) {
  cat("\nExtracting run order...\n")
  
  # Query existing run order table
  query <- "
  SELECT 
    runno,
    recordid2017,
    fips,
    fips_st,
    state_abbr,
    wth_cell_prism,
    soil_file_2020,
    corn_maturity_region,
    soyb_maturity_region
  FROM DayCent_Run_Order_Lookup  
  WHERE recordid2017 IN ({recordids*})
  ORDER BY runno
  "
  
  result <- tryCatch({
    dbGetQuery(source_con, glue::glue_sql(query, recordids = recordids, .con = source_con))
  }, error = function(e) {
    cat("✗ Error extracting run order:", conditionMessage(e), "\n")
    return(NULL)
  })
  
  if (is.null(result) || nrow(result) == 0) {
    cat("✗ No run order found for specified recordids\n")
    return(NULL)
  }
  
  combine <- merge(result, rid_df_area, by = "recordid2017")

  # Transform to template format
  run_order_data <- combine %>%
    mutate(
      execution_order = row_number(),
      site_name = as.character(recordid2017),
      treatment_name = NA,
      weather_code = wth_cell_prism,
      aggregation_level = fips,
      aggregation_weight = mean_xfact,
      active = 1,
      notes = paste0("Migrated from runno=", runno, ", fips=", fips, 
                     ", state=", state_abbr, ", soil=", soil_file_2020)
    ) %>%
    select(execution_order, site_name, treatment_name, weather_code, 
           aggregation_level, aggregation_weight, active, notes)
  
  cat("✓ Extracted", nrow(run_order_data), "run order entries\n")
  return(run_order_data)
}

#' Populate template database tables
#' @param target_con Target database connection
#' @param schedule_data Schedule file data
#' @param site_data Site file data
#' @param run_order_data Run order data
#' @return TRUE if successful, FALSE otherwise
populate_template_tables <- function(target_con, schedule_data, site_data, run_order_data) {
  cat("\nPopulating template tables...\n")
  
  success <- TRUE
  
  # Define table names
  run_order_table <- paste0("run_order_", crop, "_m", crop_mg_i, "_evaluate")
  schedule_files_table <- paste0("schedule_files_", crop, "_m", crop_mg_i, "_evaluate")
  site_files_table <- paste0("site_files_", crop, "_m", crop_mg_i, "_evaluate")
  
  # Function to drop table if it exists
  drop_table_if_exists <- function(con, table_name) {
    tryCatch({
      if (dbExistsTable(con, table_name)) {
        dbRemoveTable(con, table_name)
        cat("✓ Dropped existing table:", table_name, "\n")
      }
    }, error = function(e) {
      cat("⚠ Warning: Could not drop table", table_name, ":", conditionMessage(e), "\n")
    })
  }
  
  # Drop existing tables if they exist
  cat("Checking for existing tables and removing them...\n")
  drop_table_if_exists(target_con, run_order_table)
  drop_table_if_exists(target_con, schedule_files_table)
  drop_table_if_exists(target_con, site_files_table)

  # Populate run_order table
  if (!is.null(run_order_data)) {
    tryCatch({
      dbWriteTable(target_con, paste0("run_order_", crop, "_m", crop_mg_i, "_evaluate"), run_order_data, 
                   append = TRUE, row.names = FALSE)
      cat("✓ Populated run_order table with", nrow(run_order_data), "rows\n")
    }, error = function(e) {
      cat("✗ Failed to populate run_order table:", conditionMessage(e), "\n")
      success <<- FALSE
    })
  }
  
  # Populate schedule_files table
  if (!is.null(schedule_data)) {
    tryCatch({
      dbWriteTable(target_con, paste0("schedule_files_", crop, "_m", crop_mg_i, "_evaluate"), schedule_data,
                   append = TRUE, row.names = FALSE)
      cat("✓ Populated schedule_files table with", nrow(schedule_data), "rows\n")
    }, error = function(e) {
      cat("✗ Failed to populate schedule_files table:", conditionMessage(e), "\n")
      success <<- FALSE
    })
  }
  
  # Populate site_files table
  if (!is.null(site_data)) {
    tryCatch({
      dbWriteTable(target_con, paste0("site_files_", crop, "_m", crop_mg_i, "_evaluate"), site_data,
                   append = TRUE, row.names = FALSE)  
      cat("✓ Populated site_files table with", nrow(site_data), "rows\n")
    }, error = function(e) {
      cat("✗ Failed to populate site_files table:", conditionMessage(e), "\n")
      success <<- FALSE
    })
  }
  
  return(success)
}

# Main execution
{
  # Read in the recordid2017 values from the file
  crop = "corn"
  crop_mg = c(2, 3, 4, 5, 6)

  cat("\n=== STEP 1: Connect to Source Database ===\n")
  source_con <- connect_to_database('inv2024_calib')
  if (is.null(source_con)) {
    cat("Cannot proceed without source database connection\n")
    return(FALSE)
  }
  
  cat("\n=== STEP 2: Connect to Target Database ===\n") 
  target_con <- connect_to_database('inv2024_calib')
  if (is.null(target_con)) {
    cat("Cannot proceed without target database connection\n")
    dbDisconnect(source_con)
    return(FALSE)
  }

  # Ensure cleanup of connections
    on.exit({
      if (!is.null(source_con) && dbIsValid(source_con)) dbDisconnect(source_con)
      if (!is.null(target_con) && dbIsValid(target_con)) dbDisconnect(target_con)
    })

  for(crop_mg_i in crop_mg) {

    cat("\n", "----------------------", "Processing crop maturity group:", crop_mg_i, "----------------------", "\n")
    
    folder_name = paste0("/data/rubelscratch/rubelogle/daycent_calibration/data/crop_yield_", crop, "_m", crop_mg_i, "/", crop, "_m", crop_mg_i, "_rid_evaluate.csv")
    folder_name_area = paste0("/data/rubelscratch/rubelogle/daycent_calibration/data/crop_yield_", crop, "_m", crop_mg_i, "/", crop, "_m", crop_mg_i, "mean_area_rid_evaluate.csv")
    rid_df = read.csv(folder_name)
    rid_df_area = read.csv(folder_name_area)
    
    # Define recordid2017 values to filter (modify this list for your needs)
    TARGET_RECORDIDS <- unique(rid_df$recordid2017)
    
    #cat("Target recordid2017 values:", paste(TARGET_RECORDIDS, collapse = ", "), "\n")
            
    cat("\n=== STEP 3: Extract Data from Source Tables ===\n")
    
    # Extract data
    schedule_data <- extract_schedule_files(source_con, TARGET_RECORDIDS)
    site_data <- extract_site_files(source_con, TARGET_RECORDIDS)  
    run_order_data <- extract_run_order(source_con, TARGET_RECORDIDS, rid_df_area)
    
    if (is.null(schedule_data) && is.null(site_data) && is.null(run_order_data)) {
      cat("✗ No data extracted from source database\n")
      return(FALSE)
    }
    
    cat("\n=== STEP 4: Populate Template Database ===\n")
    
    # Populate template tables
    success <- populate_template_tables(target_con, schedule_data, site_data, run_order_data)


    
    if (success) {
      cat("\n=== SUCCESS ===\n")
      cat("✅ Template database populated successfully!\n")
      cat("\nNext steps:\n")
      cat("1. Review the populated data in your template database\n")
      cat("2. Run crop_file_index_S5.R to create database indexes\n")
      cat("3. Adjust the weather_code values if needed\n")
      cat("4. Test the database functions with the populated data\n")
      cat("5. Run the Phase 4 testing suite\n")
    } else {
      cat("\n=== ERRORS OCCURRED ===\n")
      cat("❌ Some operations failed. Check error messages above.\n")
    }
    
  }
}

# Execute main function
if (!interactive()) {
  cat("IMPORTANT: Before running this script, please modify:\n")
  cat("1. DB_CONFIG database names, usernames, passwords\n")
  cat("2. TARGET_RECORDIDS list with your actual recordid2017 values\n") 
  cat("3. Table names in the SQL queries (search for 'CHANGE THIS')\n")
  cat("4. Install required packages: install.packages(c('glue', 'dplyr'))\n")
  cat("\nTo run: Rscript populate_template_database.R\n")
  
  # Uncomment the line below to execute (after making the above changes)
  # main()
} else {
  cat("Script loaded in interactive mode.\n")
  cat("Modify the configuration and call main() to execute.\n")
}
