#' Run DayCent Model
#'
#' Executes DayCent model simulation for a given schedule file.
#'
#' @param filepath_exe Character string with path to DayCent executable
#' @param sch_file Character string with DayCent schedule file name to be run
#' @param ext_site100_2read Character string with DayCent extended site.100 file name 
#'   to read. This site.100 will replace the site.100 file in schedule. Default to NULL.
#' @param ext_site100_2write Character string with DayCent extended site.100 file name
#'   to be written at the end of model simulation. Default to NULL.
#'
#' @return Integer: 0 for success, 1 for failure
#' @export
#'
#' @author Ram Gurung (06 November 2023) updated from Dissertation Work
#'   Natural Resource Ecology Laboratory, Colorado State University
#'
#' @examples
#' \dontrun{
#' result <- run_DayCent("/path/to/daycent", "schedule.sch")
#' }
run_DayCent <- function(filepath_exe,
                        sch_file,
                        ext_site100_2read = NULL,
                        ext_site100_2write = NULL){
  tryCatch({
    if(!file.exists(filepath_exe)){stop("Invalid DayCent Executiable : \n\t", filepath_exe, "\n")}
    if(!file.exists(sch_file)){stop("Invalid schidule file : \n\t", sch_file, "\n")}

    # Check Schedule File extension
    if(substring(sch_file, (nchar(sch_file) - 3), nchar(sch_file)) == ".sch"){
      
        sch <- substring(sch_file, 1, (nchar(sch_file) - 4))
      
    }else{
      
        sch = sch_file
        
    }
    DayCent_args    =  paste0("-s ", sch)
    
    # Extended site.100 file to generate/write
    if(!is.null(ext_site100_2write)){
      
      if(substring(ext_site100_2write, (nchar(ext_site100_2write) - 3), nchar(ext_site100_2write)) == ".100"){
        
        ext_site100_2write <- substring(ext_site100_2write, 1, (nchar(ext_site100_2write) - 4))
        
      }else{
        
        ext_site100_2write = ext_site100_2write
        
      }
      
      DayCent_args = c(DayCent_args, 
                       paste0("-W ", ext_site100_2write, ".100"))
      
    }

    # Extended site.100 file to use

    if(!is.null(ext_site100_2read)){
      
      if(substring(ext_site100_2read, (nchar(ext_site100_2read) - 3), nchar(ext_site100_2read)) != ".100"){
        
        ext_site100_2read = paste0(ext_site100_2read, ".100")
        
      }

      if(!file.exists(ext_site100_2read)){stop("Invalid Extended site.100 to read : \n\t", ext_site100_2read, "\n")}
      
      
      DayCent_args = c(DayCent_args, 
                       paste0("--site ", ext_site100_2read))

    }
    
    cat("DayCent Arguments: ", DayCent_args, "/n")
    
    dc_rs <- system2(command = filepath_exe,
                     args    = DayCent_args,
                     stdout  = paste0(sch, "_stdout.log"), 
                     stderr  = paste0(sch, "_stderr.log"), 
                     wait    = T)

    cat("\n")
    if(dc_rs != 0){
      stop("------- DayCent simulation failed for ", sch_file, "." , "\n")
      
    }else{
      cat("------- Execution success for ", sch_file, "." , "\n")
      
    }
    
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 1 to indicate failure
    
  })
}


#' Run DDList100 to Extract Output from DayCent Binary File
#'
#' Executes DDList100 to extract specified variables from a DayCent binary (.bin) file
#' and create a formatted list (.lis) file.
#'
#' @param ddlist_exe Character string with path to DDList100 executable
#' @param bin_file Character string with name of the .bin file (in current directory)
#' @param lis_file Character string with name of the output .lis file to create
#' @param outvars_file Character string with path to outvars.txt file specifying variables to extract
#' @param log_function Function to use for logging messages (default: message)
#'
#' @return List with components:
#'   \item{success}{Logical indicating if DDList100 execution was successful}
#'   \item{execution_time}{Numeric execution time in seconds}
#'   \item{error_message}{Character string with error message if execution failed}
#'   \item{output_file}{Character string with name of created output file}
#'
#' @export
#'
#' @author Bayesian Calibration Framework (14 August 2025)
#'   Natural Resource Ecology Laboratory, Colorado State University
#'
#' @examples
#' \dontrun{
#' result <- run_ddlist100("/path/to/ddlist100", "model.bin", "output.lis", "outvars.txt")
#' if (result$success) {
#'   message("DDList100 created: ", result$output_file)
#' }
#' }
run_ddlist100 <- function(ddlist_exe, bin_file, lis_file, outvars_file, log_function = message) {
  
  result <- list(
    success = FALSE,
    execution_time = NULL,
    error_message = NULL,
    output_file = lis_file
  )
  
  # Validate inputs
  if (!file.exists(bin_file)) {
    result$error_message <- paste("Binary file not found:", bin_file)
    log_function(result$error_message)
    return(result)
  }
  
  if (!file.exists(outvars_file)) {
    result$error_message <- paste("outvars.txt file not found:", outvars_file)
    log_function(result$error_message)
    return(result)
  }
  
  if (!file.exists(ddlist_exe)) {
    result$error_message <- paste("DDList100 executable not found:", ddlist_exe)
    log_function(result$error_message)
    return(result)
  }
  
  log_function(paste("Running DDList100 with:"))
  log_function(paste("  Executable:", ddlist_exe))
  log_function(paste("  Binary file:", bin_file))
  log_function(paste("  Output file:", lis_file))
  log_function(paste("  Variables file:", outvars_file))
  
  tryCatch({
    start_time <- Sys.time()
    
    # Run DDList100: DDlist100 <bin_file> <lis_file> <txt_file>
    ddlist_result <- system2(
      ddlist_exe,
      args = c(bin_file, lis_file, outvars_file),
      stdout = "ddlist_stdout.log",
      stderr = "ddlist_stderr.log"
    )
    
    end_time <- Sys.time()
    result$execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    if (ddlist_result == 0) {
      # Check if output file was created
      if (file.exists(lis_file)) {
        result$success <- TRUE
        log_function(paste("DDList100 SUCCESS: Created", lis_file, sprintf("(%.1fs)", result$execution_time)))
      } else {
        result$error_message <- paste("DDList100 completed but output file not found:", lis_file)
        log_function(result$error_message)
      }
    } else {
      result$error_message <- paste("DDList100 failed with exit code:", ddlist_result)
      
      # Try to get error details from stderr
      if (file.exists("ddlist_stderr.log")) {
        stderr_lines <- readLines("ddlist_stderr.log", warn = FALSE)
        if (length(stderr_lines) > 0) {
          result$error_message <- paste(result$error_message, 
                                      "Errors:", paste(stderr_lines, collapse = "; "))
        }
      }
      log_function(result$error_message)
    }
    
  }, error = function(e) {
    result$error_message <- paste("R error running DDList100:", e$message)
    log_function(result$error_message)
  })
  
  return(result)
}


#' Read DayCent .lis File with Time/Year Correction
#'
#' Reads a .lis file created by DDList100, handling the metadata header and 
#' correcting time values to proper simulation years. The first data row 
#' (representing end of pre-simulation year) is removed and time values 
#' are adjusted to match simulation years.
#'
#' @param lis_file Character string with path to .lis file to read
#' @param verbose Logical indicating whether to print progress messages (default: FALSE)
#'
#' @return Data frame with corrected year column and all variables from .lis file
#'
#' @details
#' .lis file structure:
#' - Line 1: Binary filename (metadata)
#' - Line 2: Space-separated variable names (header)
#' - Line 3: Empty line
#' - Line 4+: Data rows (space-separated values)
#' 
#' Time correction logic:
#' - First data row represents end of year before simulation start
#' - Remove first data row
#' - Convert time column: time 1845.00 → year 1844, time 1846.00 → year 1845, etc.
#'
#' @export
#'
#' @author Bayesian Calibration Framework (14 August 2025)
#'   Natural Resource Ecology Laboratory, Colorado State University
#'
#' @examples
#' \dontrun{
#' data <- read_lis_file("broadbalk_BF.lis")
#' head(data)
#' }
read_lis_file <- function(lis_file, verbose = FALSE) {
  
  if (verbose) cat("Reading .lis file:", lis_file, "\n")
  
  # Validate file exists
  if (!file.exists(lis_file)) {
    stop("LIS file not found: ", lis_file)
  }
  
  tryCatch({
    # Read all lines
    all_lines <- readLines(lis_file, warn = FALSE)
    
    if (length(all_lines) < 4) {
      stop("Invalid .lis file format: fewer than 4 lines")
    }
    
    # Parse header (line 2) - space-separated variable names
    header_line <- all_lines[2]
    var_names <- trimws(strsplit(header_line, "\\s+")[[1]])
    
    # Remove empty strings (from leading spaces)
    var_names <- var_names[var_names != ""]
    
    if (verbose) {
      cat("  Variables found:", length(var_names), "\n")
      cat("  First few variables:", paste(head(var_names), collapse = ", "), "\n")
    }
    
    # Read data starting from line 4 (skip metadata and empty line)
    data_lines <- all_lines[4:length(all_lines)]
    
    # Filter out empty lines
    data_lines <- data_lines[data_lines != "" & !is.na(data_lines)]
    
    if (length(data_lines) == 0) {
      stop("No data rows found in .lis file")
    }
    
    if (verbose) cat("  Data rows found:", length(data_lines), "\n")
    
    # Parse data into matrix
    data_matrix <- matrix(NA, nrow = length(data_lines), ncol = length(var_names))
    
    for (i in 1:length(data_lines)) {
      row_values <- trimws(strsplit(data_lines[i], "\\s+")[[1]])
      
      # Remove empty strings (important for consistent parsing)
      row_values <- row_values[row_values != ""]
      
      # Handle potential formatting issues
      if (length(row_values) != length(var_names)) {
        if (verbose) {
          cat("    Row", i, "mismatch: expected", length(var_names), "got", length(row_values), "\n")
        }
        # Pad with NA if too few values, truncate if too many
        if (length(row_values) < length(var_names)) {
          row_values <- c(row_values, rep(NA, length(var_names) - length(row_values)))
        } else {
          row_values <- row_values[1:length(var_names)]
        }
      }
      
      data_matrix[i, ] <- row_values
    }
    
    # Convert to data frame
    data_df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
    names(data_df) <- var_names
    
    if (verbose) cat("  Data dimensions before correction:", nrow(data_df), "x", ncol(data_df), "\n")
    
    # CRITICAL: Remove first row (pre-simulation year data) BEFORE numeric conversion
    if (nrow(data_df) > 1) {
      data_df <- data_df[-1, ]
      if (verbose) cat("  Removed first row (pre-simulation year)\n")
    } else {
      warning("Only one data row found - cannot remove first row")
    }
    
    # Convert numeric columns (excluding character columns like 'crpval')
    # Now test with actual simulation data (not pre-simulation data)
    for (col in names(data_df)) {
      if (col != "crpval") {
        # Test if first non-NA value can be converted to numeric
        first_val <- data_df[1, col]
        if (!is.na(first_val) && first_val != "" && !is.na(suppressWarnings(as.numeric(first_val)))) {
          data_df[[col]] <- as.numeric(data_df[[col]])
        }
      }
    }
    
    # Time/Year correction: time 1845.00 → year 1844
    if ("time" %in% names(data_df)) {
      # Ensure time column is numeric before floor operation
      if (!is.numeric(data_df$time)) {
        data_df$time <- as.numeric(data_df$time)
      }
      data_df$year <- floor(data_df$time) - 1
      if (verbose) {
        cat("  Time correction applied\n")
        cat("  Year range:", min(data_df$year, na.rm = TRUE), "-", max(data_df$year, na.rm = TRUE), "\n")
      }
    } else {
      warning("No 'time' column found for year correction")
    }
    
    if (verbose) cat("  Final data dimensions:", nrow(data_df), "x", ncol(data_df), "\n")
    
    return(data_df)
    
  }, error = function(e) {
    stop("Error reading .lis file '", lis_file, "': ", e$message)
  })
}


