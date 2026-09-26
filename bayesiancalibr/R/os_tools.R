#=======================================================================================  
#  PURPOSE:   collection of operating system (os) function in R
#
#  AUTHOR:    Ram Gurung 
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
#=======================================================================================  

read_config_file = function(which_sim){
  tryCatch({
    
    config_ini = ini::read.ini("config.ini")
    
    assign("date_stamp",        config_ini$task$date_stamp,                     envir = .GlobalEnv)
    assign("n_per_jobs",        as.numeric(config_ini$task$n_per_jobs),         envir = .GlobalEnv)
    
    os_info = Sys.info()
    assign("nodename",         as.character(os_info[4]),                       envir = .GlobalEnv)
    
    clstr   = ifelse(grepl("rubel", os_info["nodename"]), "NREL-rubel",
                     ifelse(grepl("wcnr-el", os_info["notename"]), "Local-Desktop",
                     ifelse(grepl("???Ceres", os_info["nodename"]), "ARS-Ceres", # Need update
                            ifelse(grepl("???Atlas", os_info["nodename"]),"ARS-Atlas", os_info["nodename"])))) # Need update
    
    if(clstr =="Local-Desktop"){
      simulation_path2 = file.path(config_ini$Local_Desktop$simulation_path, date_stamp)
      assign("simulation_path", simulation_path2,                               envir = .GlobalEnv)
      assign("scratch_path",    config_ini$Local_Desktop$scratch_path,     envir = .GlobalEnv)
      
      assign("daycent",         config_ini$Local_Desktop$daycent,          envir = .GlobalEnv)
      assign("daycent_list",    config_ini$Local_Desktop$daycent_list,     envir = .GlobalEnv)
      
      assign("input_path",      config_ini$Local_Desktop$input_path,      envir = .GlobalEnv)
      assign("dot100_path",     config_ini$Local_Desktop$dot100_path,      envir = .GlobalEnv)
      assign("ExpSite_path",    config_ini$Local_Desktop$ExpSite_path,     envir = .GlobalEnv)
      
    }else if(clstr == "NREL-rubel"){
      simulation_path2 = file.path(config_ini$NREL_rubel_cluster$simulation_path, date_stamp)
      assign("simulation_path", simulation_path2,                               envir = .GlobalEnv)
      assign("scratch_path",    config_ini$NREL_rubel_cluster$scratch_path,     envir = .GlobalEnv)
      
      assign("daycent",         config_ini$NREL_rubel_cluster$daycent,          envir = .GlobalEnv)
      assign("daycent_list",    config_ini$NREL_rubel_cluster$daycent_list,     envir = .GlobalEnv)
      
      assign("input_path",      config_ini$NREL_rubel_cluster$input_path,      envir = .GlobalEnv)
      assign("dot100_path",     config_ini$NREL_rubel_cluster$dot100_path,      envir = .GlobalEnv)
      assign("ExpSite_path",    config_ini$NREL_rubel_cluster$ExpSite_path,     envir = .GlobalEnv)
      
    }else if(clstr == "ARS-Ceres"){
      simulation_path2 = file.path(config_ini$ARS_ceres_cluster$simulation_path, date_stamp)
      assign("simulation_path", simulation_path2,                               envir = .GlobalEnv)
      assign("scratch_path",    config_ini$ARS_ceres_cluster$scratch_path,      envir = .GlobalEnv)
      
      assign("daycent",         config_ini$ARS_ceres_cluster$daycent,           envir = .GlobalEnv)
      assign("daycent_list",    config_ini$ARS_ceres_cluster$daycent_list,      envir = .GlobalEnv)
      
      assign("input_path",      config_ini$ARS_ceres_cluster$input_path,      envir = .GlobalEnv)
      assign("dot100_path",     config_ini$ARS_ceres_cluster$dot100_path,       envir = .GlobalEnv)
      assign("ExpSite_path",    config_ini$ARS_ceres_cluster$ExpSite_path,      envir = .GlobalEnv)
      
    }else if(clstr == "ARS-Atlas"){
      simulation_path2 = file.path(config_ini$ARS_atlas_cluster$simulation_path, date_stamp)
      assign("simulation_path", simulation_path2,                               envir = .GlobalEnv)
      assign("scratch_path",    config_ini$ARS_atlas_cluster$scratch_path,      envir = .GlobalEnv)
      
      assign("daycent",         config_ini$ARS_atlas_cluster$daycent,           envir = .GlobalEnv)
      assign("daycent_list",    config_ini$ARS_atlas_cluster$daycent_list,      envir = .GlobalEnv)
      
      assign("input_path",      config_ini$ARS_atlas_cluster$input_path,      envir = .GlobalEnv)
      assign("dot100_path",     config_ini$ARS_atlas_cluster$dot100_path,       envir = .GlobalEnv)
      assign("ExpSite_path",    config_ini$ARS_atlas_cluster$ExpSite_path,      envir = .GlobalEnv)
      
    }else{
      stop("Cluster ", clstr, " Not in the current list.")
      
    }
    
    if(which_sim == "GSA"){

      assign("mc_num",          as.numeric(config_ini$GSA$mc_num),              envir = .GlobalEnv)
      assign("prior_file",      config_ini$GSA$prior_file,                      envir = .GlobalEnv)
      assign("param_file",      config_ini$GSA$param_file,                      envir = .GlobalEnv)
      assign("output_file",     config_ini$GSA$output_file,                     envir = .GlobalEnv)
      
      assign("gsa_obj_file",    config_ini$GSA$gsa_obj_file,                    envir = .GlobalEnv)
      
    }else if(which_sim == "SIR"){
      
      assign("mc_num",          as.numeric(config_ini$SIR$mc_num),             envir = .GlobalEnv)
      assign("prior_file",      config_ini$SIR$prior_file,                      envir = .GlobalEnv)
      assign("param_file",      config_ini$SIR$param_file,                      envir = .GlobalEnv)
      assign("output_file",     config_ini$SIR$output_file,                     envir = .GlobalEnv)
      
    }else if(which_sim == "MC"){
      
      assign("param_file",      config_ini$MC$param_file,                       envir = .GlobalEnv)
      assign("output_file",     config_ini$MC$output_file,                      envir = .GlobalEnv)
      
    }else if(which_sim == "SR"){
      
      assign("output_file",     config_ini$SR$output_file,                      envir = .GlobalEnv)
      
    }else{
      
      stop("Simulation System: ", which_sim, " not recognize.")
      
    }
    
    # assign additional path & file variables
    
    # Experimental Sites path for simulation 
    sim_ExpSites_path = file.path(simulation_path, "ExpSites")
    assign("sim_ExpSites_path", sim_ExpSites_path, envir = .GlobalEnv)
    
    # dot100 path for simulation
    sim_dot100_path = file.path(simulation_path, "dot100Files")
    assign("sim_dot100_path", sim_dot100_path, envir = .GlobalEnv)
    
    # Intermediate File path
    sim_input_path = file.path(simulation_path, "InputData")
    assign("sim_input_path", sim_input_path, envir = .GlobalEnv)
    
    # Intermediate File path
    sim_intermediate_path = file.path(simulation_path, "IntermediateFiles", which_sim)
    assign("sim_intermediate_path", sim_intermediate_path, envir = .GlobalEnv)
    
    # Model Result Output path 
    sim_output_path = file.path(simulation_path, "OutputFiles", which_sim)
    assign("sim_output_path", sim_output_path, envir = .GlobalEnv)
    
    # define runFile with Experimental site to simulate
    runFile_file = file.path(sim_input_path, config_ini$task$run_file)
    assign("runFile_file", runFile_file,  envir = .GlobalEnv)
    
    # define default parameter file
    dflt_param_file = file.path(sim_input_path, config_ini$task$dflt_param_file)
    assign("dflt_param_file", dflt_param_file, envir = .GlobalEnv)
    
    
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 1 to indicate failure
    
  })
}

copy_project_files = function(){
  # copy simulation files from project folder to simulation folder
  tryCatch({
    
    #=============================================================================
    # Experimental site path & iles:
    if(!dir.exists(ExpSite_path)) {stop("Invalid site files Directory (from): Check for ", ExpSite_path, " directory. \n")}
    
    # Create destination directory if it doesn't exist and copy Experimental sites files
    if(dir.exists(sim_ExpSites_path)){
      unlink(sim_ExpSites_path, recursive = TRUE)
    }
    
    dir.create(sim_ExpSites_path, recursive = TRUE)
    
    # Get a list of all files in the source directory
    ExpSite_files <- list.files(path = ExpSite_path, full.names = TRUE)
    
    # Copy each file to the destination directory
    file.copy(from = ExpSite_files, to = sim_ExpSites_path, recursive = TRUE)
    
    #=============================================================================
    # dot100 path & files:
    if(!dir.exists(dot100_path)) {stop("Invalid site files Directory (from): Check for ", dot100_path, " directory. \n")}
    
    # Create destination directory if it doesn't exist and copy Experimental sites files
    if(dir.exists(sim_dot100_path)){
      unlink(sim_dot100_path, recursive = TRUE)
    }
    
    dir.create(sim_dot100_path, recursive = TRUE)
    
    # Get a list of all files in the source directory
    dot100_files <- list.files(path = dot100_path, full.names = TRUE)
    
    # Copy each file to the destination directory
    file.copy(from = dot100_files, to = sim_dot100_path, recursive = TRUE)
    
    #=============================================================================
    # Input Data path & files:
    if(!dir.exists(input_path)) {stop("Invalid site files Directory (from): Check for ", input_path, " directory. \n")}
    
    # Create destination directory if it doesn't exist and copy Experimental sites files
    if(dir.exists(sim_input_path)){
      unlink(sim_input_path, recursive = TRUE)
    }
    
    dir.create(sim_input_path, recursive = TRUE)
    
    # Get a list of all files in the source directory
    input_files <- list.files(path = input_path, full.names = TRUE)
    
    # Copy each file to the destination directory
    file.copy(from = input_files, to = sim_input_path, recursive = TRUE)
    #=============================================================================
    # Intermediate file path:
    if(!dir.exists(sim_intermediate_path)){
      dir.create(sim_intermediate_path, recursive = TRUE)
    }

    #=============================================================================
    # Model output file path:
    sim_output_path = file.path(simulation_path, "OutputFiles", which_sim)
    assign("sim_output_path", sim_output_path, envir = .GlobalEnv)
    
    if(!dir.exists(sim_output_path)){
      dir.create(sim_output_path, recursive = TRUE)
    }

    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 1 to indicate failure
    
  })

}

copy_dot100_files <- function(dot100_directory,
                              simulation_directory){
  #=======================================================================================
  #  Input Arguments:      
  #      - dot100_directory     :       a character string (file.path) with dot100 files 
  #`                                    for DayCent model simulation
  #      - simulation_directory :       a character string (file.path) where all files
  #                                     for DayCent model simulation
  #=======================================================================================
  tryCatch({
    
    if(!dir.exists(dot100_directory))     {stop("Invalid dot100 Directory: Check for ",     dot100_directory,     " directory. \n")}
    if(!dir.exists(simulation_directory)) {stop("Invalid simulation Directory: Check for ", simulation_directory, " directory. \n")}
    
    #============================================================================
    file.copy(from = file.path(dot100_directory, "outvars.txt"),
              to = file.path(simulation_directory, "outvars.txt"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "outvars_names.txt"),
              to = file.path(simulation_directory, "outvars_names.txt"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "no_outfiles.in"),
              to = file.path(simulation_directory, "outfiles.in"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "nflux_outfiles.in"),
              to = file.path(simulation_directory, "nflux_outfiles.in"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "crop.100"),
              to = file.path(simulation_directory, "crop.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "cult.100"),
              to = file.path(simulation_directory, "cult.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "fert.100"),
              to = file.path(simulation_directory, "fert.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "fire.100"),
              to = file.path(simulation_directory, "fire.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "fix.100"),
              to = file.path(simulation_directory, "fix.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "graz.100"),
              to = file.path(simulation_directory, "graz.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "harv.100"),
              to = file.path(simulation_directory, "harv.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "irri.100"),
              to = file.path(simulation_directory, "irri.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "omad.100"),
              to = file.path(simulation_directory, "omad.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "tree.100"),
              to = file.path(simulation_directory, "tree.100"),
              overwrite = TRUE)
    
    file.copy(from = file.path(dot100_directory, "trem.100"),
              to = file.path(simulation_directory, "trem.100"),
              overwrite = TRUE)
    
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 1 to indicate failure
    
  })
  
}

copy_sitefiles <- function(site_folder_from,
                           site_folder_to,
                           trim_weather = TRUE){
  #=======================================================================================
  #  Input Arguments:      
  #      - site_folder_from :       a character string (file.path) with site files 
  #`                                for DayCent model simulation to copy from
  #      - site_folder_to   :       a character string (file.path) where site iles
  #                                 for DayCent model simulation to copy to
  #      - trim_weather     :       if TRUE (default), trim weather files to schedule
  #                                 start years; set FALSE for chained_schedule mode
  #=======================================================================================
  tryCatch({
    
    if(!dir.exists(site_folder_from)) {stop("Invalid site files Directory (from): Check for ", site_folder_from, " directory. \n")}
    if(!dir.exists(site_folder_to))   {stop("Invalid site files Directory (to): Check for ",   site_folder_to,   " directory. \n")}
    
    #=======================================================================================================
    # copy site files 
    
    site_files <- list.files(path = site_folder_from, full.names = T)
    sim_files <- gsub(pattern = site_folder_from, replacement = site_folder_to, x = site_files)
    
    file.copy(from = site_files,
              to   = sim_files,
              overwrite = TRUE)

    # Trim weather files to match schedule start years (skip when trim_weather is FALSE, e.g. chained_schedule mode)
    if (trim_weather) {
      weather_files <- list.files(site_folder_to, pattern = "\\.wth$", full.names = TRUE)
      schedule_files <- list.files(site_folder_to, pattern = "\\.sch$", full.names = TRUE)

      # Trim each weather file against each schedule file
      # (In most cases there will be one weather file and one or more schedule files)
      for (wth_file in weather_files) {
        for (sch_file in schedule_files) {
          tryCatch({
            trim_weather_file_by_schedule(
              weather_file_path = wth_file,
              schedule_file_path = sch_file,
              log_function = function(...) NULL  # Silent operation in filesystem mode
            )
          }, error = function(e) {
            # Log warning but don't fail - weather might be for different schedule
            cat("Warning: Could not trim ", basename(wth_file), " with ", basename(sch_file),
                ": ", conditionMessage(e), "\n", sep = "")
          })
        }
      }
    }

    return(0)

  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 1 to indicate failure
    
  })

}

#' Trim Weather File to Match Schedule Start Year
#'
#' Trims weather file data to align with the schedule file's starting year.
#' DayCent reads weather files from the first line, so if the weather file
#' contains years before the schedule starts, those rows must be removed.
#'
#' @param weather_file_path Path to the weather file (.wth)
#' @param schedule_file_path Path to the schedule file (.sch)
#' @param log_function Function to use for logging messages (default: no logging)
#' @return Invisible NULL on success, stops with error on failure
#' @export
trim_weather_file_by_schedule <- function(weather_file_path, schedule_file_path, log_function = function(...) NULL) {

  # Validate inputs
  if (!file.exists(weather_file_path)) {
    stop("Weather file not found: ", weather_file_path)
  }

  if (!file.exists(schedule_file_path)) {
    stop("Schedule file not found: ", schedule_file_path)
  }

  # Read schedule start year from first line (extract first 4-digit year to avoid coercion on comment lines)
  schedule_first_line <- readLines(schedule_file_path, n = 1, warn = FALSE)
  first_year_match <- regmatches(schedule_first_line, regexpr("[0-9]{4}", schedule_first_line))
  schedule_start_year <- if (length(first_year_match) > 0) as.integer(first_year_match[1]) else NA

  if (is.na(schedule_start_year) || schedule_start_year < 1000 || schedule_start_year > 9999) {
    stop("Failed to parse valid start year from schedule file: ", schedule_file_path,
         "\nFirst line: ", schedule_first_line)
  }

  # Read weather file
  weather_data <- tryCatch({
    read.table(weather_file_path, header = FALSE, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Failed to read weather file: ", weather_file_path, "\n", conditionMessage(e))
  })

  # Validate weather file has at least 3 columns (year should be in column 3)
  if (ncol(weather_data) < 3) {
    stop("Weather file has fewer than 3 columns (expected year in column 3): ", weather_file_path)
  }

  # Get year range from weather file (column 3); coerce to numeric to avoid NAs-introduced-by-coercion warning
  weather_years <- suppressWarnings(as.numeric(weather_data[, 3]))
  weather_start_year <- min(weather_years, na.rm = TRUE)
  weather_end_year <- max(weather_years, na.rm = TRUE)
  if (!is.finite(weather_start_year) || !is.finite(weather_end_year)) {
    stop("Could not determine year range from weather file (column 3): ", weather_file_path)
  }

  # Validation: weather must start at or before schedule
  if (weather_start_year > schedule_start_year) {
    stop("Weather file starts at year ", weather_start_year,
         " but schedule starts at year ", schedule_start_year, ".\n",
         "Weather data is insufficient for the simulation period.\n",
         "Weather file: ", weather_file_path, "\n",
         "Schedule file: ", schedule_file_path)
  }

  # Check if trimming is needed
  if (weather_start_year < schedule_start_year) {
    # Trim weather data to start at schedule_start_year (use numeric comparison)
    weather_trimmed <- weather_data[!is.na(weather_years) & weather_years >= schedule_start_year, ]

    # Validate we have data after trimming
    if (nrow(weather_trimmed) == 0) {
      stop("No weather data remains after trimming to schedule start year ", schedule_start_year)
    }

    # Write trimmed data back (overwrite original file)
    tryCatch({
      write.table(weather_trimmed, weather_file_path,
                  row.names = FALSE, col.names = FALSE, quote = FALSE)

      log_function("Trimmed weather file: ", basename(weather_file_path),
                   " from year ", weather_start_year, " to ", schedule_start_year,
                   " (", nrow(weather_data) - nrow(weather_trimmed), " rows removed)\n")

    }, error = function(e) {
      stop("Failed to write trimmed weather file: ", weather_file_path, "\n", conditionMessage(e))
    })

  } else {
    # Weather file already starts at schedule year - no trimming needed
    log_function("Weather file matches schedule start year (", schedule_start_year, "): ",
                 basename(weather_file_path), " - no trimming needed\n")
  }

  invisible(NULL)
}

get_date_stamp = function(){
  # Returns a character object with the following format:
  #	: <current_mth>_<current_dom>_<current_year>
  #=======================================================================================  
  current_Date = as.POSIXlt(Sys.time())
  current_year = as.integer(format(current_Date, "%Y"))
  current_mth  = months(current_Date)
  current_dom  = as.integer(format(current_Date, "%d"))
  date_stamp   = paste0(current_dom, current_mth, current_year)
  return(date_stamp)
}

get_date_time_stamp = function(){
  # Returns a character object with the following format:
  #	: <current_mth>_<current_dom>_<current_year>_<24-hour>H<minute>M
  #=======================================================================================  
  current_Date = as.POSIXlt(Sys.time())
  current_year = as.integer(format(current_Date, "%Y"))
  current_mth  = months(current_Date)
  current_dom  = as.integer(format(current_Date, "%d"))
  current_hm   = paste0(as.integer(format(current_Date, "%H")),"H",
                        as.integer(format(current_Date, "%M")),"M")
  date_time_stamp   = paste0(current_dom, current_mth,  current_year, "_", current_hm)
  return(date_time_stamp)
}

#' Read YAML Configuration File
#'
#' Reads a YAML configuration file and returns the configuration as a list
#'
#' @param config_path Path to the YAML configuration file
#' @return List containing the configuration
#' @export
read_yaml_config <- function(config_path) {
  
  if (!file.exists(config_path)) {
    stop("Configuration file not found: ", config_path)
  }
  
  tryCatch({
    # Try to load yaml package
    if (!requireNamespace("yaml", quietly = TRUE)) {
      stop("yaml package is required but not installed. Please install it with: install.packages('yaml')")
    }
    
    config <- yaml::read_yaml(config_path)
    
    # Validate basic structure
    validate_config_structure(config)
    
    return(config)
    
  }, error = function(e) {
    stop("Error reading YAML config file: ", e$message)
  })
}

#' Validate Configuration Structure
#'
#' Validates that the configuration contains required fields
#'
#' @param config Configuration list from YAML
validate_config_structure <- function(config) {
  
  required_sections <- c("project", "paths", "gsa", "input_files", "output_dirs")
  
  for (section in required_sections) {
    if (!section %in% names(config)) {
      stop("Required configuration section '", section, "' is missing")
    }
  }
  
  # Validate project section
  if (!"name" %in% names(config$project)) {
    stop("project.name is required in configuration")
  }
  
  # Validate paths section
  if (!"lairice_root" %in% names(config$paths)) {
    stop("paths.lairice_root is required in configuration")
  }
  
  # Validate GSA section
  required_gsa <- c("gsa_methods", "nsim", "nboot", "rseed", "n2dir")
  for (field in required_gsa) {
    if (!field %in% names(config$gsa)) {
      stop("gsa.", field, " is required in configuration")
    }
  }
  
  # Validate that GSA methods is a list
  if (!is.list(config$gsa$gsa_methods) && !is.character(config$gsa$gsa_methods)) {
    stop("gsa.gsa_methods must be a list of GSA method names")
  }
  
  # Validate model_outputs section (optional)
  if ("model_outputs" %in% names(config)) {
    # Validate that model output variables are lists/vectors if present
    if ("daily_vars" %in% names(config$model_outputs)) {
      if (!is.list(config$model_outputs$daily_vars) && !is.character(config$model_outputs$daily_vars)) {
        stop("model_outputs.daily_vars must be a list or character vector")
      }
    }
    
    if ("aggregated_vars" %in% names(config$model_outputs)) {
      if (!is.list(config$model_outputs$aggregated_vars) && !is.character(config$model_outputs$aggregated_vars)) {
        stop("model_outputs.aggregated_vars must be a list or character vector")
      }
    }
  }
  
  # Validate input_files section
  if ("input_files" %in% names(config)) {
    # Check for required input files
    required_input_files <- c("prior_file")
    for (field in required_input_files) {
      if (!field %in% names(config$input_files)) {
        stop("input_files.", field, " is required in configuration")
      }
    }
    
    # Validate observation_data structure (if present)
    if ("observation_data" %in% names(config$input_files)) {
      if (!is.list(config$input_files$observation_data)) {
        stop("input_files.observation_data must be a list of observation file paths")
      }
    }
  }
  
  # Validate output_dirs section
  if ("output_dirs" %in% names(config)) {
    # Check that output_dirs is a list/named structure
    if (!is.list(config$output_dirs)) {
      stop("output_dirs must be a list of directory names")
    }
  }
  
  return(TRUE)
}

#' Get Absolute Paths from Configuration
#'
#' Converts relative paths in configuration to absolute paths
#'
#' @param config Configuration list
#' @return Updated configuration with absolute paths
#' @export
resolve_config_paths <- function(config) {
  
  root_dir <- config$paths$lairice_root
  
  # Convert relative paths to absolute paths
  relative_paths <- c("bmaf_srs_dir", "expsites_dir", "evasites_dir", "daycent_dir", "observation_dir", "dot100_path", "evadot100_path")
  
  for (path_name in relative_paths) {
    if (path_name %in% names(config$paths)) {
      if (!file.path.is.absolute(config$paths[[path_name]])) {
        config$paths[[path_name]] <- file.path(root_dir, config$paths[[path_name]])
      }
    }
  }
  
  # Convert input file paths
  for (file_name in names(config$input_files)) {
    # Handle nested observation_data structure
    if (file_name == "observation_data" && is.list(config$input_files[[file_name]])) {
      for (obs_file_name in names(config$input_files$observation_data)) {
        if (!file.path.is.absolute(config$input_files$observation_data[[obs_file_name]])) {
          config$input_files$observation_data[[obs_file_name]] <- file.path(root_dir, config$input_files$observation_data[[obs_file_name]])
        }
      }
    } else if (file_name == "observation_files" && is.list(config$input_files[[file_name]])) {
      # Skip observation_files list - these are just reference names, not file paths
      next
    } else {
      # Handle regular file paths (single strings only)
      file_value <- config$input_files[[file_name]]
      if (is.character(file_value) && length(file_value) == 1 && !file.path.is.absolute(file_value)) {
        config$input_files[[file_name]] <- file.path(root_dir, file_value)
      }
    }
  }
  
  return(config)
}

#' Check if Path is Absolute
#'
#' Helper function to check if a file path is absolute
#'
#' @param path File path to check (character string, not vector)
#' @return Logical indicating if path is absolute
#' @export
file.path.is.absolute <- function(path) {
  # Handle single path only (not vectors)
  if (length(path) != 1) {
    stop("file.path.is.absolute expects a single path, not a vector")
  }

  # Check for Unix/Linux absolute paths (start with /)
  if (substr(path, 1, 1) == "/") return(TRUE)

  # Check for Windows absolute paths (start with drive letter)
  if (grepl("^[A-Za-z]:", path)) return(TRUE)

  # Check for UNC paths (start with \\)
  if (substr(path, 1, 2) == "\\\\") return(TRUE)

  return(FALSE)
}
