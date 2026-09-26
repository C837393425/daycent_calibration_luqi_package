#' @title Database Connection Manager for DayCent Calibration
#' @description Smart connection pooling system that reduces database connection overhead
#' for large SLURM jobs while maintaining full backward compatibility with single job testing.
#' @details This system automatically detects job size and uses connection pooling only when
#' beneficial, falling back to individual connections for small jobs and testing.
#'
#' @name database-connection-manager
NULL

# Global environment to store shared database connections per job
.db_connection_env <- new.env(parent = emptyenv())

#' Get Shared Database Connection
#'
#' Creates or retrieves a shared database connection for the current job.
#' This function implements smart connection pooling that reduces database
#' connection overhead for large jobs while maintaining compatibility.
#'
#' @param config Configuration object containing database settings
#' @param job_id Optional job identifier (defaults to process ID + node name)
#' @param force_new Logical, whether to force creation of new connection
#' @param log_function Function for logging messages (default: cat)
#' @return Database connection object or NULL if connection fails
#' @export
#' @examples
#' \dontrun{
#' # Get shared connection (creates if doesn't exist)
#' shared_con <- get_shared_connection(config)
#'
#' # Use in database operations
#' result <- get_run_order_from_database(config, shared_connection = shared_con)
#'
#' # Clean up when job is done
#' close_shared_connection(config)
#' }
get_shared_connection <- function(config, job_id = NULL, force_new = FALSE, log_function = cat) {

  # Check if database mode is enabled
  if (is.null(config$file_source$mode) || config$file_source$mode != "database") {
    log_function("Database mode not enabled - shared connections not available\n")
    return(NULL)
  }

  # Generate unique job identifier
  if (is.null(job_id)) {
    job_id <- paste0(Sys.info()[["nodename"]], "_", Sys.getpid())
  }

  connection_key <- paste0("shared_con_", job_id)

  # Check if we already have a connection for this job
  if (!force_new && exists(connection_key, envir = .db_connection_env)) {
    existing_con <- get(connection_key, envir = .db_connection_env)

    # Validate existing connection
    if (validate_database_connection(existing_con, log_function)) {
      log_function("Reusing existing shared database connection for job: ", job_id, "\n")
      return(existing_con)
    } else {
      log_function("Existing shared connection invalid, creating new one\n")
      # Remove invalid connection
      rm(list = connection_key, envir = .db_connection_env)
    }
  }

  # Create new shared connection
  log_function("Creating new shared database connection for job: ", job_id, "\n")
  con <- connect_database(config, log_function)

  if (is.null(con)) {
    log_function("Failed to create shared database connection\n")
    return(NULL)
  }

  # Store connection in global environment
  assign(connection_key, con, envir = .db_connection_env)

  # Register cleanup function to close connection when R session ends
  reg.finalizer(.db_connection_env, function(e) {
    close_shared_connection(config, job_id, log_function = function(...) {})
  }, onexit = TRUE)

  log_function("Shared database connection created successfully for job: ", job_id, "\n")
  return(con)
}

#' Close Shared Database Connection
#'
#' Closes and removes the shared database connection for the current job.
#' Should be called at the end of SLURM jobs to clean up resources.
#'
#' @param config Configuration object
#' @param job_id Optional job identifier (defaults to process ID + node name)
#' @param log_function Function for logging messages (default: cat)
#' @return TRUE if connection was closed, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' # Close shared connection at end of job
#' close_shared_connection(config)
#' }
close_shared_connection <- function(config, job_id = NULL, log_function = cat) {

  # Generate unique job identifier
  if (is.null(job_id)) {
    job_id <- paste0(Sys.info()[["nodename"]], "_", Sys.getpid())
  }

  connection_key <- paste0("shared_con_", job_id)

  # Check if connection exists
  if (!exists(connection_key, envir = .db_connection_env)) {
    log_function("No shared connection found for job: ", job_id, "\n")
    return(FALSE)
  }

  # Get and close connection
  tryCatch({
    con <- get(connection_key, envir = .db_connection_env)

    if (!is.null(con)) {
      DBI::dbDisconnect(con)
      log_function("Shared database connection closed for job: ", job_id, "\n")
    }

    # Remove from environment
    rm(list = connection_key, envir = .db_connection_env)
    return(TRUE)

  }, error = function(e) {
    log_function("Error closing shared connection: ", conditionMessage(e), "\n")
    # Remove from environment even if disconnect failed
    rm(list = connection_key, envir = .db_connection_env)
    return(FALSE)
  })
}

#' Validate Database Connection
#'
#' Checks if a database connection is still valid and responsive.
#'
#' @param con Database connection object
#' @param log_function Function for logging messages (default: cat)
#' @return TRUE if connection is valid, FALSE otherwise
#' @export
validate_database_connection <- function(con, log_function = cat) {

  if (is.null(con)) {
    return(FALSE)
  }

  tryCatch({
    # Simple query to test connection
    result <- DBI::dbGetQuery(con, "SELECT 1 as test")
    return(nrow(result) == 1 && result$test == 1)

  }, error = function(e) {
    log_function("Database connection validation failed: ", conditionMessage(e), "\n")
    return(FALSE)
  })
}

#' Determine Connection Pooling Strategy
#'
#' Smart function to determine whether to use connection pooling based on
#' job size, configuration settings, and context.
#'
#' @param config Configuration object
#' @param expected_operations Expected number of database operations
#' @param force_pooling Force connection pooling regardless of other factors
#' @param log_function Function for logging messages (default: cat)
#' @return List with pooling decision and reasoning
#' @export
#' @examples
#' \dontrun{
#' # Check if we should use pooling for this job
#' strategy <- determine_pooling_strategy(config, expected_operations = 450)
#' if (strategy$use_pooling) {
#'   shared_con <- get_shared_connection(config)
#' }
#' }
determine_pooling_strategy <- function(config, expected_operations = 1,
                                     force_pooling = FALSE, log_function = cat) {

  # Default strategy
  strategy <- list(
    use_pooling = FALSE,
    reason = "Default behavior",
    threshold = 10
  )

  # Check if database mode is enabled
  if (is.null(config$file_source$mode) || config$file_source$mode != "database") {
    strategy$reason <- "Database mode not enabled"
    return(strategy)
  }

  # Check configuration settings
  if (!is.null(config$file_source$database$connection_pooling)) {
    pooling_config <- config$file_source$database$connection_pooling

    # Check if pooling is disabled
    if (!is.null(pooling_config$enabled) && !pooling_config$enabled) {
      strategy$reason <- "Connection pooling disabled in configuration"
      return(strategy)
    }

    # Check if forced to individual connections (for testing)
    if (!is.null(pooling_config$force_individual) && pooling_config$force_individual) {
      strategy$reason <- "Forced to individual connections by configuration"
      return(strategy)
    }

    # Get threshold from configuration
    if (!is.null(pooling_config$threshold)) {
      strategy$threshold <- pooling_config$threshold
    }
  }

  # Force pooling if requested
  if (force_pooling) {
    strategy$use_pooling <- TRUE
    strategy$reason <- "Forced by parameter"
    return(strategy)
  }

  # Determine based on expected operations
  if (expected_operations > strategy$threshold) {
    strategy$use_pooling <- TRUE
    strategy$reason <- paste0("Expected operations (", expected_operations,
                             ") exceeds threshold (", strategy$threshold, ")")
  } else {
    strategy$reason <- paste0("Expected operations (", expected_operations,
                             ") below threshold (", strategy$threshold, ")")
  }

  log_function("Connection pooling strategy: ",
              if(strategy$use_pooling) "ENABLED" else "DISABLED",
              " - ", strategy$reason, "\n")

  return(strategy)
}

#' Get Connection for Database Operation
#'
#' Smart function that returns either a shared connection or creates an individual
#' connection based on availability and configuration. This function provides
#' seamless backward compatibility.
#'
#' @param config Configuration object
#' @param shared_connection Optional shared connection (takes precedence)
#' @param log_function Function for logging messages (default: cat)
#' @return List with connection object and whether it should be closed
#' @export
#' @examples
#' \dontrun{
#' # Get appropriate connection (shared if available, individual otherwise)
#' conn_info <- get_connection_for_operation(config, shared_connection)
#' con <- conn_info$connection
#'
#' # ... perform database operations ...
#'
#' # Clean up if needed
#' if (conn_info$should_close) {
#'   DBI::dbDisconnect(con)
#' }
#' }
get_connection_for_operation <- function(config, shared_connection = NULL, log_function = cat) {

  # If shared connection is provided and valid, use it
  if (!is.null(shared_connection) && validate_database_connection(shared_connection, log_function)) {
    return(list(
      connection = shared_connection,
      should_close = FALSE,
      type = "shared"
    ))
  }

  # Try to get existing shared connection for this job
  job_id <- paste0(Sys.info()[["nodename"]], "_", Sys.getpid())
  connection_key <- paste0("shared_con_", job_id)

  if (exists(connection_key, envir = .db_connection_env)) {
    existing_con <- get(connection_key, envir = .db_connection_env)
    if (validate_database_connection(existing_con, log_function)) {
      return(list(
        connection = existing_con,
        should_close = FALSE,
        type = "shared_existing"
      ))
    }
  }

  # Fall back to individual connection
  individual_con <- connect_database(config, log_function)

  return(list(
    connection = individual_con,
    should_close = TRUE,
    type = "individual"
  ))
}

#' List Active Shared Connections
#'
#' Debugging function to list all active shared connections.
#'
#' @return Character vector of active connection keys
#' @export
list_shared_connections <- function() {
  return(ls(.db_connection_env))
}

#' Close All Shared Connections
#'
#' Emergency function to close all active shared connections.
#' Useful for cleanup during debugging or error recovery.
#'
#' @param log_function Function for logging messages (default: cat)
#' @return Number of connections closed
#' @export
close_all_shared_connections <- function(log_function = cat) {

  connection_keys <- ls(.db_connection_env)
  closed_count <- 0

  for (key in connection_keys) {
    tryCatch({
      con <- get(key, envir = .db_connection_env)
      if (!is.null(con)) {
        DBI::dbDisconnect(con)
        closed_count <- closed_count + 1
      }
      rm(list = key, envir = .db_connection_env)
    }, error = function(e) {
      log_function("Error closing connection ", key, ": ", conditionMessage(e), "\n")
      rm(list = key, envir = .db_connection_env)
    })
  }

  log_function("Closed ", closed_count, " shared database connections\n")
  return(closed_count)
}