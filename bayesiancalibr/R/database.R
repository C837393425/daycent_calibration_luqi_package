#' @title Database-Driven File Management Functions
#' @description Core functions for database-driven DayCent file management
#' @details These functions enable database storage and retrieval of DayCent schedule files,
#' site files, and execution metadata as an alternative to filesystem-based management.
#' 
#' @name database-functions
NULL

#' Connect to DayCent Calibration Database
#'
#' Establishes connection to MySQL/MariaDB database for file management
#'
#' @param config List containing database configuration with elements:
#'   - file_source$database$host: Database host (default: localhost)
#'   - file_source$database$port: Database port (default: 3306)  
#'   - file_source$database$database: Database name
#'   - file_source$database$username: Username (or use DB_USER env var)
#'   - file_source$database$password: Password (or use DB_PASSWORD env var)
#' @param log_function Function for logging messages (default: cat)
#' @return Database connection object or NULL if connection fails
#' @export
#' @examples
#' \dontrun{
#' config <- list(file_source = list(database = list(
#'   host = "localhost",
#'   database = "daycent_calibration", 
#'   username = "daycent_user",
#'   password = "password"
#' )))
#' con <- connect_database(config)
#' if (!is.null(con)) {
#'   # Use connection
#'   DBI::dbDisconnect(con)
#' }
#' }
connect_database <- function(config, log_function = cat) {
  
  # Check if database mode is enabled
  if (is.null(config$file_source$mode) || config$file_source$mode != "database") {
    log_function("Database mode not enabled in configuration\n")
    return(NULL)
  }
  
  # Check for required packages - support both RMariaDB and RMySQL
  has_mariadb <- requireNamespace("RMariaDB", quietly = TRUE)
  has_mysql <- requireNamespace("RMySQL", quietly = TRUE)
  
  if (!requireNamespace("DBI", quietly = TRUE)) {
    log_function("Package 'DBI' is required but not available\n")
    return(NULL)
  }
  
  if (!has_mariadb && !has_mysql) {
    log_function("Package 'RMariaDB' or 'RMySQL' is required but not available\n")
    return(NULL)
  }
  
  db_config <- config$file_source$database
  
  # Get connection parameters from configuration
  host <- db_config$host %||% stop("Database host must be specified")
  database <- db_config$database %||% stop("Database name must be specified")
  cred_file <- db_config$cred_file %||% "~/.dblogin"
  
  # Read credentials from file
  tryCatch({
    if (!file.exists(path.expand(cred_file))) {
      stop("Credential file not found: ", path.expand(cred_file))
    }
    
    cred <- readLines(path.expand(cred_file), warn = FALSE)
    if (length(cred) < 2) {
      stop("Credential file must contain at least 2 lines (username, password)")
    }
    
    username <- trimws(cred[1])
    password <- trimws(cred[2])
    
    if (nchar(username) == 0 || nchar(password) == 0) {
      stop("Username and password cannot be empty")
    }
    
    log_function("Credentials loaded from: ", cred_file, "\n")
    
  }, error = function(e) {
    log_function("Failed to read credentials: ", conditionMessage(e), "\n")
    return(NULL)
  })
  
  # Attempt database connection - prefer RMariaDB over RMySQL
  tryCatch({
    if (has_mariadb) {
      con <- DBI::dbConnect(
        RMariaDB::MariaDB(),
        host = host,
        dbname = database,
        username = username,
        password = password
      )
    } else {
      # Fallback to RMySQL
      con <- DBI::dbConnect(
        RMySQL::MySQL(),
        host = host,
        dbname = database,
        username = username,
        password = password
      )
    }
    
    log_function("Database connection established successfully to ", database, " on ", host, "\n")
    return(con)
    
  }, error = function(e) {
    log_function("Failed to connect to database: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Get Run Order from Database
#'
#' Retrieves execution order data from database, replacing RunFile.rds functionality
#'
#' @param config List containing database configuration
#' @param task_id Optional specific task ID to retrieve (for GSA/SIR Step 2)
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param log_function Function for logging messages (default: cat)
#' @return Data frame with run order information or NULL if failed
#' @export
#' @examples
#' \dontrun{
#' # Get all active runs (individual connection)
#' run_data <- get_run_order_from_database(config)
#'
#' # Get specific task with shared connection
#' shared_con <- get_shared_connection(config)
#' task_data <- get_run_order_from_database(config, task_id = 1, shared_connection = shared_con)
#' }
get_run_order_from_database <- function(config, task_id = NULL, shared_connection = NULL, log_function = cat) {

  # Get appropriate connection (shared or individual)
  conn_info <- get_connection_for_operation(config, shared_connection, log_function)
  con <- conn_info$connection

  if (is.null(con)) {
    return(NULL)
  }

  # Only close connection if we created an individual one
  if (conn_info$should_close) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  db_config <- config$file_source$database
  table_name <- db_config$tables$run_order %||% "run_order"
  
  tryCatch({
    if (is.null(task_id)) {
      # Get all active runs in execution order
      # Create RunFile structure with properly typed aggregation columns
      query <- paste0("SELECT site_name as siteID, ",
                     "CONCAT(site_name, '_eq.sch') as equil_schedule, ",
                     "CONCAT(site_name, '_eq_ext30.sch') as equil_ext30_schedule, ",
                     "CONCAT(site_name, '_base.sch') as base_schedule, ",
                     "CASE WHEN treatment_name IS NULL OR treatment_name = '' ",
                     "THEN CONCAT(site_name, '.sch') ",
                     "ELSE CONCAT(site_name, '_', treatment_name, '.sch') ",
                     "END as treatment_schedule, ",
                     "CAST(aggregation_level AS CHAR) as aggregation_level, ",
                     "aggregation_weight ",
                     "FROM ", table_name, " ",
                     "WHERE active = 1 ORDER BY execution_order")
      
      result <- DBI::dbGetQuery(con, query)
      log_function("Retrieved ", nrow(result), " active runs from database\n")
      
    } else {
      # Get specific task by execution order
      # Create RunFile structure with properly typed aggregation columns
      query <- paste0("SELECT site_name as siteID, ",
                     "CONCAT(site_name, '_eq.sch') as equil_schedule, ",
                     "CONCAT(site_name, '_eq_ext30.sch') as equil_ext30_schedule, ",
                     "CONCAT(site_name, '_base.sch') as base_schedule, ",
                     "CASE WHEN treatment_name IS NULL OR treatment_name = '' ",
                     "THEN CONCAT(site_name, '.sch') ",
                     "ELSE CONCAT(site_name, '_', treatment_name, '.sch') ",
                     "END as treatment_schedule, ",
                     "CAST(aggregation_level AS CHAR) as aggregation_level, ",
                     "aggregation_weight ",
                     "FROM ", table_name, " ",
                     "WHERE active = 1 AND execution_order = ? ",
                     "ORDER BY execution_order LIMIT 1")
      
      result <- DBI::dbGetQuery(con, query, params = list(task_id))
      
      if (nrow(result) == 0) {
        log_function("No active run found for task_id: ", task_id, "\n")
        return(NULL)
      }
      
      log_function("Retrieved task ", task_id, " from database\n")
    }
    
    return(result)
    
  }, error = function(e) {
    log_function("Failed to retrieve run order from database: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Get Schedule File from Database
#'
#' Retrieves DayCent schedule file content from database
#'
#' @param config List containing database configuration
#' @param site_name Site identifier
#' @param treatment_name Treatment identifier
#' @param schedule_type Type of schedule file: "equilibrium", "equilibrium_ext30", "base", "treatment"
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param log_function Function for logging messages (default: cat)
#' @return Character string with file content or NULL if not found
#' @export
#' @examples
#' \dontrun{
#' # Get treatment schedule file (individual connection)
#' content <- get_schedule_file_from_database(config, "broadbalk", "BF", "treatment")
#'
#' # Get treatment schedule file with shared connection
#' shared_con <- get_shared_connection(config)
#' content <- get_schedule_file_from_database(config, "broadbalk", "BF", "treatment", shared_con)
#' }
get_schedule_file_from_database <- function(config, site_name, treatment_name, schedule_type, shared_connection = NULL, log_function = cat) {

  # Get appropriate connection (shared or individual)
  conn_info <- get_connection_for_operation(config, shared_connection, log_function)
  con <- conn_info$connection

  if (is.null(con)) {
    return(NULL)
  }

  # Only close connection if we created an individual one
  if (conn_info$should_close) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  db_config <- config$file_source$database
  table_name <- db_config$tables$schedule_files %||% "schedule_files"
  
  # Validate schedule_type
  valid_types <- c("equilibrium", "equilibrium_ext30", "base", "treatment")
  if (!schedule_type %in% valid_types) {
    log_function("Invalid schedule_type: ", schedule_type, ". Must be one of: ", 
                paste(valid_types, collapse = ", "), "\n")
    return(NULL)
  }
  
  tryCatch({
    # Template schema: site_name, treatment_name, schedule_file_data, notes

    # Handle NULL/empty treatment_name properly
    if (is.null(treatment_name) || treatment_name == "" || is.na(treatment_name)) {
      query <- paste0("SELECT schedule_file_data ",
                     "FROM ", table_name, " ",
                     "WHERE site_name = ? AND treatment_name IS NULL")
      result <- DBI::dbGetQuery(con, query, params = list(site_name))
    } else {
      query <- paste0("SELECT schedule_file_data ",
                     "FROM ", table_name, " ",
                     "WHERE site_name = ? AND treatment_name = ?")
      result <- DBI::dbGetQuery(con, query,
                               params = list(site_name, treatment_name))
    }

    if (nrow(result) == 0) {
      log_function("Schedule file not found: site=", site_name,
                  ", treatment=", treatment_name, "\n")
      return(NULL)
    }

    log_function("Retrieved schedule file for site=", site_name, ", treatment=", treatment_name, "\n")
    return(result$schedule_file_data[1])

  }, error = function(e) {
    log_function("Failed to retrieve schedule file from database: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Get Site Files from Database
#'
#' Retrieves DayCent site files (.100) and related files from database
#'
#' @param config List containing database configuration
#' @param site_name Site identifier
#' @param file_type Optional file type filter: "site100", "soils", "other"
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param log_function Function for logging messages (default: cat)
#' @return Data frame with file information or NULL if failed
#' @export
#' @examples
#' \dontrun{
#' # Get all site files for a site (individual connection)
#' files <- get_site_files_from_database(config, "broadbalk")
#'
#' # Get only .100 site files with shared connection
#' shared_con <- get_shared_connection(config)
#' site100 <- get_site_files_from_database(config, "broadbalk", "site100", shared_con)
#' }
get_site_files_from_database <- function(config, site_name, file_type = NULL, shared_connection = NULL, log_function = cat) {

  # Get appropriate connection (shared or individual)
  conn_info <- get_connection_for_operation(config, shared_connection, log_function)
  con <- conn_info$connection

  if (is.null(con)) {
    return(NULL)
  }

  # Only close connection if we created an individual one
  if (conn_info$should_close) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  db_config <- config$file_source$database
  table_name <- db_config$tables$site_files %||% "site_files"
  
  tryCatch({
    if (is.null(file_type)) {
      # Get all site files
      # Template schema: site_name, site_file_data, notes
      query <- paste0("SELECT site_file_data ",
                     "FROM ", table_name, " ",
                     "WHERE site_name = ?")
      
      result <- DBI::dbGetQuery(con, query, params = list(site_name))
      
    } else {
      # Get files of specific type
      valid_types <- c("site100", "soils", "other")
      if (!file_type %in% valid_types) {
        log_function("Invalid file_type: ", file_type, ". Must be one of: ", 
                    paste(valid_types, collapse = ", "), "\n")
        return(NULL)
      }
      
      # Template schema: site_name, site_file_data, notes (no file_type support)
      query <- paste0("SELECT site_file_data ",
                     "FROM ", table_name, " ",
                     "WHERE site_name = ?")
      
      result <- DBI::dbGetQuery(con, query, params = list(site_name))
    }
    
    if (nrow(result) == 0) {
      log_function("No site files found for site: ", site_name, 
                  if(!is.null(file_type)) paste(" (type:", file_type, ")"), "\n")
      return(NULL)
    }
    
    log_function("Retrieved ", nrow(result), " site files for ", site_name, "\n")
    return(result)
    
  }, error = function(e) {
    log_function("Failed to retrieve site files from database: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Build Weather File Path from Weather Code
#'
#' Constructs a weather file path using a configurable pattern.
#'
#' Supports different weather products (e.g., PRISM, Daymet/NLDAS) by allowing
#' the directory and filename template to be defined in configuration.
#'
#' Configuration (under config$file_source$database$weather):
#' - base_directory: root directory where weather folders live (required)
#' - pattern: path template with placeholders, default "prism_{grid}/{weather_code}.wth"
#' - grid_extraction: regex to extract {grid} from weather_code, default "^(\\d+)"
#' - validation_regex: regex that weather_code must match, default "^[A-Za-z0-9_.-]+$"
#'
#' Placeholders:
#' - {weather_code}: full weather_code string
#' - {grid}: optional, substituted only if present in pattern and extracted successfully
#'
#' @param config List containing database configuration with weather settings
#' @param weather_code Weather code or identifier used in the filename
#' @param log_function Function for logging messages (default: cat)
#' @return Character string with full weather file path or NULL if failed
#' @export
#' @examples
#' \dontrun{
#' # PRISM-style (default):
#' # pattern: "prism_{grid}/{weather_code}.wth"
#' # grid_extraction: "^(\\d+)"
#' path <- build_weather_file_path(config, "127_507")
#' # -> "/data/.../Daycent_weatherFiles_Inv2020/prism_127/127_507.wth"
#'
#' # Daymet/NLDAS-style:
#' # pattern: "Daymet_{grid}/{weather_code}.wth"
#' # grid_extraction: "^(\\d+)"
#' path <- build_weather_file_path(config, "11004_43638-9323_24713")
#' # -> "/data/.../Daymet_weather/Daymet_11004/11004_43638-9323_24713.wth"
#' }
build_weather_file_path <- function(config, weather_code, log_function = cat) {
  
  # Check if database mode is enabled
  if (is.null(config$file_source$mode) || config$file_source$mode != "database") {
    log_function("Database mode not enabled - weather path construction unavailable\n")
    return(NULL)
  }
  
  db_config <- config$file_source$database
  weather_cfg <- if (!is.null(db_config$weather)) db_config$weather else list()
  
  base_directory <- weather_cfg$base_directory
  if (is.null(base_directory) || base_directory == "") {
    log_function("Weather base directory not configured\n")
    return(NULL)
  }
  
  # Allow configurable validation; default to general safe characters
  validation_regex <- weather_cfg$validation_regex %||% "^[A-Za-z0-9_.-]+$"
  if (is.null(weather_code) || weather_code == "" || !grepl(validation_regex, weather_code)) {
    log_function("Invalid weather code format: ", weather_code,
                 " (expected to match regex '", validation_regex, "')\n")
    return(NULL)
  }
  
  # Allow configurable grid extraction and filename pattern
  pattern <- weather_cfg$pattern %||% "prism_{grid}/{weather_code}.wth"
  grid_extraction <- weather_cfg$grid_extraction %||% "^(\\d+)"
  
  tryCatch({
    grid_value <- NULL
    
    if (grepl("\\{grid\\}", pattern)) {
      grid_match <- regexec(grid_extraction, weather_code, perl = TRUE)
      grid_parts <- regmatches(weather_code, grid_match)[[1]]
      
      if (length(grid_parts) >= 2) {
        grid_value <- grid_parts[2]
      } else if (length(grid_parts) == 1 && grid_match[[1]] != -1) {
        # Regex without capture group - use the matched substring
        grid_value <- grid_parts[1]
      } else {
        stop("Failed to extract grid component from weather code using grid_extraction regex: ",
             grid_extraction)
      }
    }
    
    # Build relative path from pattern placeholders
    relative_path <- pattern
    relative_path <- gsub("\\{weather_code\\}", weather_code, relative_path, perl = TRUE)
    
    if (!is.null(grid_value)) {
      relative_path <- gsub("\\{grid\\}", grid_value, relative_path, perl = TRUE)
    }
    
    weather_path <- file.path(base_directory, relative_path)
    
    log_function("Built weather path: ", weather_path, "\n")
    return(weather_path)
    
  }, error = function(e) {
    log_function("Failed to build weather file path: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

#' Copy Files from Database to Target Directory
#'
#' Main function to copy all required files from database to simulation directory
#'
#' @param site_name Site identifier
#' @param treatment_name Treatment identifier
#' @param weather_code Weather code for weather file path construction
#' @param target_dir Target directory for file copying
#' @param config List containing database configuration
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param log_function Function for logging messages (default: cat)
#' @return Integer status code: 0 = success, 1 = failure
#' @export
#' @examples
#' \dontrun{
#' # Copy files with individual connections
#' status <- copy_files_from_database("broadbalk", "BF", "127_507",
#'                                   "/tmp/simulation", config)
#'
#' # Copy files with shared connection
#' shared_con <- get_shared_connection(config)
#' status <- copy_files_from_database("broadbalk", "BF", "127_507",
#'                                   "/tmp/simulation", config, shared_con)
#' }
copy_files_from_database <- function(site_name, treatment_name, weather_code,
                                   target_dir, config, shared_connection = NULL, log_function = cat) {
  
  log_function("Starting database file copy for site=", site_name, 
              ", treatment=", treatment_name, ", target=", target_dir, "\n")
  
  # Create target directory if it doesn't exist
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
    log_function("Created target directory: ", target_dir, "\n")
  }
  
  # Copy schedule files
  schedule_types <- c("equilibrium", "equilibrium_ext30", "base", "treatment")
  for (schedule_type in schedule_types) {
    
    content <- get_schedule_file_from_database(config, site_name, treatment_name,
                                             schedule_type, shared_connection, log_function)
    
    if (!is.null(content)) {
      # Determine output filename pattern
      filename <- switch(schedule_type,
        "equilibrium" = paste0(site_name, "_eq.sch"),
        "equilibrium_ext30" = paste0(site_name, "_eq_ext30.sch"),
        "base" = "base.sch",
        "treatment" = if (is.null(treatment_name) || treatment_name == "") {
          paste0(site_name, ".sch")
        } else {
          paste0(site_name, "_", treatment_name, ".sch")
        }
      )
      
      output_path <- file.path(target_dir, filename)
      
      tryCatch({
        writeLines(content, output_path)
        log_function("Wrote schedule file: ", filename, "\n")
      }, error = function(e) {
        log_function("Failed to write schedule file ", filename, ": ", conditionMessage(e), "\n")
        return(1)
      })
    }
  }
  
  # Copy site files
  site_files <- get_site_files_from_database(config, site_name, file_type = NULL, shared_connection, log_function)

  if (!is.null(site_files)) {
    for (i in 1:nrow(site_files)) {
      # Use site-specific filename as referenced by schedule files
      filename <- paste0(site_name, ".100")
      output_path <- file.path(target_dir, filename)

      tryCatch({
        writeLines(site_files$site_file_data[i], output_path)
        log_function("Wrote site file: ", filename, "\n")
      }, error = function(e) {
        log_function("Failed to write site file ", filename,
                    ": ", conditionMessage(e), "\n")
        return(1)
      })
    }
  }
  
  # Copy weather file (from filesystem using constructed path)
  weather_path <- build_weather_file_path(config, weather_code, log_function)
  
  if (!is.null(weather_path) && file.exists(weather_path)) {
    weather_filename <- basename(weather_path)
    target_weather_path <- file.path(target_dir, weather_filename)
    
    tryCatch({
      file.copy(weather_path, target_weather_path, overwrite = TRUE)
      log_function("Copied weather file: ", weather_filename, "\n")

      # Trim weather file to match schedule start year
      # Use the treatment schedule file (main simulation schedule)
      treatment_schedule_filename <- if (is.null(treatment_name) || treatment_name == "") {
        paste0(site_name, ".sch")
      } else {
        paste0(site_name, "_", treatment_name, ".sch")
      }

      schedule_file <- file.path(target_dir, treatment_schedule_filename)
      if (file.exists(schedule_file) && file.exists(target_weather_path)) {
        # Skip weather trim when config indicates chained_schedule mode (no single schedule start year)
        trim_weather <- is.null(config$daycent$mode) || config$daycent$mode != "chained_schedule"
        if (trim_weather) {
          trim_weather_file_by_schedule(
            weather_file_path = target_weather_path,
            schedule_file_path = schedule_file,
            log_function = log_function
          )
        }
      }

    }, error = function(e) {
      log_function("Failed to copy weather file: ", conditionMessage(e), "\n")
      return(1)
    })

  } else {
    log_function("Weather file not found or path construction failed\n")
    return(1)
  }

  log_function("Database file copy completed successfully\n")
  return(0)
}

#' Enhanced RunFile Creation with Database Mode Support
#'
#' Creates DayCent RunFile data structure from either database or filesystem,
#' providing seamless integration with existing calibration workflows.
#'
#' @param config List containing configuration with file_source$mode setting
#' @param ExpSite_path Character path to experimental sites directory (used in filesystem mode)
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param log_function Function for logging messages (default: cat)
#' @return Data frame with RunFile structure (siteID, scheduleFile, etc.)
#' @export
#' @examples
#' \dontrun{
#' # Database mode with individual connection
#' config <- list(file_source = list(mode = "database", database = list(...)))
#' run_file <- create_daycent_runfile_enhanced(config)
#'
#' # Database mode with shared connection
#' shared_con <- get_shared_connection(config)
#' run_file <- create_daycent_runfile_enhanced(config, shared_connection = shared_con)
#'
#' # Filesystem mode (original behavior)
#' config <- list(file_source = list(mode = "filesystem"))
#' run_file <- create_daycent_runfile_enhanced(config, ExpSite_path = "data/sites")
#' }
create_daycent_runfile_enhanced <- function(config, ExpSite_path = NULL, shared_connection = NULL, log_function = cat) {
  
  # Chained-schedule mode: build RunFile from observation CSV if it has schedule columns
  if (!is.null(config$daycent$mode) && config$daycent$mode == "chained_schedule" &&
      !is.null(config$daycent$schedule_columns) && length(config$daycent$schedule_columns) > 0) {
    obs_path <- config$input_files$observation_data$soc_measurements %||%
      config$input_files$observation_data$soc_annual %||%
      config$input_files$observation_data$ch4_measurements
    if (!is.null(obs_path)) {
      root <- config$paths$lairice_root %||% "."
      if (!grepl("^/", obs_path)) obs_path <- file.path(root, obs_path)
      if (file.exists(obs_path)) {
        log_function("Using chained_schedule mode: building RunFile from observation data\n")
        run_file <- read.csv(obs_path, stringsAsFactors = FALSE)
        if (!"siteID" %in% names(run_file) && "site_ID" %in% names(run_file)) {
          run_file$siteID <- run_file$site_ID
        }
        required <- c("siteID", config$daycent$schedule_columns)
        missing <- setdiff(required, names(run_file))
        if (length(missing) > 0) {
          stop("Observation file missing columns required for chained_schedule RunFile: ", paste(missing, collapse = ", "))
        }
        run_file <- unique(run_file[, required, drop = FALSE])
        log_function(
          "RunFile from ", basename(obs_path), ": ", nrow(run_file),
          " unique site/treatment rows, ", length(unique(run_file$siteID)), " sites\n"
        )
        return(run_file)
      }
    }
  }
  
  # Check file source mode
  if (is.null(config$file_source$mode) || config$file_source$mode == "filesystem") {
    # Filesystem mode - use existing functionality
    log_function("Using filesystem mode for RunFile creation\n")
    
    if (is.null(ExpSite_path)) {
      stop("ExpSite_path is required for filesystem mode")
    }
    
    # Call existing function (assumes it exists in the package)
    return(create_daycent_runfile(ExpSite_path))
    
  } else if (config$file_source$mode == "database") {
    # Database mode - get RunFile structure from database
    log_function("Creating RunFile from database...\n")
    
    run_file <- get_run_order_from_database(config, task_id = NULL, shared_connection, log_function)
    
    if (is.null(run_file)) {
      stop("Failed to retrieve RunFile data from database")
    }
    
    log_function("Retrieved ", nrow(run_file), " run entries from database\n")
    return(run_file)
    
  } else {
    stop("Invalid file_source mode: ", config$file_source$mode, ". Must be 'filesystem' or 'database'")
  }
}

#' Get Weather Code from Database
#'
#' Retrieves the weather code for a specific site from the run_order table
#'
#' @param config List containing database configuration
#' @param site_name Site identifier to lookup weather code for
#' @param shared_connection Optional shared database connection (for connection pooling)
#' @param log_function Function for logging messages (default: cat)
#' @return Character string with weather code or NULL if not found
#' @export
#' @examples
#' \dontrun{
#' # Get weather code with individual connection
#' weather_code <- get_weather_code_from_database(config, "854585")
#'
#' # Get weather code with shared connection
#' shared_con <- get_shared_connection(config)
#' weather_code <- get_weather_code_from_database(config, "854585", shared_con)
#' }
get_weather_code_from_database <- function(config, site_name, shared_connection = NULL, log_function = cat) {

  # Check if database mode is enabled
  if (is.null(config$file_source$mode) || config$file_source$mode != "database") {
    log_function("Database mode not enabled, cannot lookup weather code\n")
    return(NULL)
  }

  # Get appropriate connection (shared or individual)
  conn_info <- get_connection_for_operation(config, shared_connection, log_function)
  con <- conn_info$connection

  if (is.null(con)) {
    log_function("Failed to establish database connection for weather code lookup\n")
    return(NULL)
  }

  # Only close connection if we created an individual one
  if (conn_info$should_close) {
    on.exit(DBI::dbDisconnect(con))
  }
  
  db_config <- config$file_source$database
  table_name <- db_config$tables$run_order %||% "run_order"
  
  tryCatch({
    # Query for weather code for the specific site
    query <- paste0("SELECT weather_code FROM ", table_name, 
                   " WHERE site_name = '", site_name, "' AND active = 1 LIMIT 1")
    
    result <- DBI::dbGetQuery(con, query)
    
    if (nrow(result) > 0 && !is.null(result$weather_code) && result$weather_code != "") {
      weather_code <- result$weather_code
      log_function("Found weather code for site ", site_name, ": ", weather_code, "\n")
      return(weather_code)
    } else {
      log_function("No weather code found for site ", site_name, " in database\n")
      return(NULL)
    }
    
  }, error = function(e) {
    log_function("Error querying weather code from database: ", conditionMessage(e), "\n")
    return(NULL)
  })
}

# Utility function for null coalescing operator
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs) && length(lhs) > 0 && !is.na(lhs) && lhs != "") lhs else rhs
}
