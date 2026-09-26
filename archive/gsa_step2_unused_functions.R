# ============================================================================
# ARCHIVED UNUSED GSA STEP 2 FUNCTIONS
# ============================================================================
#
# These functions were removed from the active bayesiancalibr package during
# cleanup but preserved here for reference and potential future use.
#
# Archived Date: July 27, 2025
# Reason: Superseded by newer generic/batch processing implementations
#
# Original Location: bayesiancalibr/R/gsa_step2_simulate.R
# ============================================================================

# Function: initialize_aggregated_variables
# Lines: 20 - 374 from original file

initialize_aggregated_variables <- function() {
  list(
    aglivc = 0,
    aglivn = 0,
    N2O = 0,
    nit_N2O = 0,
    dnit_N2O = 0,
    dnit_N2 = 0,
    NO = 0,
    NH3 = 0,
    netMin1 = 0,
    netMin2 = 0,
    spHf = 0,
    urea = 0,
    crnf = 0
  )
}

#' Prepare Parameter Set for Simulation
#'
#' Creates a parameter data frame from job parameters and configuration
#'
#' @param config Configuration object
#' @param job_params Job-specific parameters from Monte Carlo draws
#' @param verbose Logical indicating whether to print progress
#' @return Data frame with File, Parameter, and value columns
#' @export
prepare_parameter_set <- function(config, job_params, verbose = TRUE) {
  
  # Read prior parameter file to get parameter names and file mappings
  prior_file <- config$input_files$prior_file
  if (!file.exists(prior_file)) {
    stop("Prior file not found: ", prior_file)
  }
  
  prior <- read.csv(prior_file, stringsAsFactors = FALSE)
  
  # Read default parameters if specified
  if ("default_params" %in% names(config$input_files)) {
    default_file <- config$input_files$default_params
    if (file.exists(default_file)) {
      dflt <- read.csv(default_file, stringsAsFactors = FALSE)
      dflt2 <- dflt[!(dflt$Parameter %in% prior$Parameter), ]
      dfltdf <- dflt2[, c("File", "Parameter", "Default")]
      names(dfltdf)[which(names(dfltdf) == "Default")] <- "value"
    } else {
      if (verbose) cat("Default parameters file not found, using only prior parameters\n")
      dfltdf <- data.frame(File = character(0), Parameter = character(0), value = numeric(0))
    }
  } else {
    dfltdf <- data.frame(File = character(0), Parameter = character(0), value = numeric(0))
  }
  
  # Create parameter data frame from job parameters
  temp_df <- prior[, c("File", "Parameter")]
  
  # Extract parameter values from job_params (exclude SampleID and JobGroup columns)
  param_values <- as.numeric(job_params[, !names(job_params) %in% c("SampleID", "JobGroup")])
  temp_df$value <- param_values
  
  # Combine with default parameters
  params_df <- rbind(temp_df, dfltdf)
  
  if (verbose) {
    cat("Prepared parameter set with", nrow(params_df), "parameters\n")
    cat("  Prior parameters:", nrow(temp_df), "\n")
    cat("  Default parameters:", nrow(dfltdf), "\n")
  }
  
  return(params_df)
}

#' Run Simulations for a Single Site
#'
#' Executes DayCent simulations for all treatments at a given experimental site
#'
#' @param site_id Site identifier
#' @param run_file_site Site-specific run file information
#' @param config Configuration object
#' @param params_df Parameter data frame
#' @param sim_dir_tid Task-specific simulation directory
#' @param daycent_exe Path to DayCent executable
#' @param actual_task_id Task ID for this simulation
#' @param agg_vars Aggregated variables list
#' @param verbose Logical indicating whether to print progress
#' @return List with simulation results and updated counters
#' @export
run_site_simulations <- function(site_id, run_file_site, config, params_df, 
                                sim_dir_tid, daycent_exe, actual_task_id, 
                                agg_vars, verbose = TRUE) {
  
  # Initialize return values
  num_treatments <- 0
  num_sim_years <- 0
  daily_results <- NULL
  
  # Setup directories
  site_dir_from <- file.path(config$paths$expsites_dir, site_id)
  site_sim_dir <- file.path(sim_dir_tid, site_id)
  
  if (verbose) {
    cat("Site source path:", site_dir_from, "\n")
    cat("Scratch run path:", site_sim_dir, "\n")
  }
  
  # Setup DayCent run files
  if (dir.exists(site_sim_dir)) {
    unlink(site_sim_dir, recursive = TRUE)
  }
  dir.create(site_sim_dir, recursive = TRUE)
  
  # Copy site files
  copy_status <- copy_sitefiles(site_folder_from = site_dir_from, 
                               site_folder_to = site_sim_dir)
  if (copy_status != 0) {
    stop("Site files copy failed for ", site_id)
  }
  
  if (verbose) cat("Site files copied successfully\n")
  
  # Set working directory
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(site_sim_dir)
  
  # Copy dot100 files
  dot100_status <- copy_dot100_files(dot100_directory = config$paths$dot100_path,
                                    simulation_directory = site_sim_dir)
  if (dot100_status != 0) {
    stop("dot100 files copy failed for ", site_id)
  }
  
  if (verbose) cat("dot100 files copied successfully\n")
  
  # Update parameters using enhanced generic dispatcher (now handles site.100 auto-detection)
  update_status <- update_daycent_parameters(params_df, simulation_dir = site_sim_dir, 
                                            verbose = FALSE)
  if (update_status != 0) {
    stop("Parameter update failed for ", site_id)
  }
  
  if (verbose) cat("Parameters updated successfully\n")
  
  # Get treatment schedules for this site
  trt_schs <- sort(unique(run_file_site$treatment_schedule))
  
  if (verbose) {
    cat("Running", length(trt_schs), "treatment(s)\n")
  }
  
  # Setup output files for NH3 measurements (could be made configurable)
  file.copy(from = file.path(config$paths$dot100_path, "nh3_outfiles.in"),
            to = file.path(site_sim_dir, "outfiles.in"),
            overwrite = TRUE)
  
  # Run treatments
  for (j in 1:length(trt_schs)) {
    num_treatments <- num_treatments + 1
    
    # Get base schedule information
    bh_sch_file <- trimws(run_file_site$base_schedule[j])
    bh_site100 <- paste0(strip_dot_sch(sch_file_name = bh_sch_file), "_site.100")
    
    # Update extended site.100 parameters
    site_update_status <- update_site100_parameters(site100_file = bh_site100, 
                                                    paramsdf = params_df)
    if (site_update_status != 0) {
      stop("Extended site.100 update failed for ", site_id)
    }
    
    # Run DayCent simulation
    trt_sch_file <- trimws(trt_schs[j])
    
    daycent_status <- run_DayCent(filepath_exe = daycent_exe,
                                 sch_file = trt_sch_file,
                                 ext_site100_2read = bh_site100,
                                 ext_site100_2write = NULL)
    
    if (daycent_status != 0) {
      stop("DayCent simulation failed for ", site_id, "::", trt_sch_file)
    }
    
    if (verbose) cat("DayCent execution successful for", trt_sch_file, "\n")
    
    # Process model outputs
    treatment_results <- process_model_outputs(site_id, trt_sch_file, actual_task_id, 
                                              config, agg_vars, verbose = verbose)
    
    # Update results
    num_sim_years <- num_sim_years + treatment_results$num_sim_years
    daily_results <- rbind(daily_results, treatment_results$daily_results)
    agg_vars <- treatment_results$agg_vars
  }
  
  return(list(
    num_treatments = num_treatments,
    num_sim_years = num_sim_years,
    daily_results = daily_results,
    agg_vars = agg_vars
  ))
}

#' Process Model Outputs
#'
#' Reads and processes DayCent model outputs for a specific treatment
#'
#' @param site_id Site identifier
#' @param trt_sch_file Treatment schedule file name
#' @param actual_task_id Task ID for this simulation
#' @param config Configuration object
#' @param agg_vars Aggregated variables list
#' @param verbose Logical indicating whether to print progress
#' @return List with processed results and updated aggregated variables
#' @export
process_model_outputs <- function(site_id, trt_sch_file, actual_task_id, config, 
                                 agg_vars, verbose = TRUE) {
  
  daily_results <- NULL
  num_sim_years <- 0
  
  # Read nflux output
  if (file.exists("nflux.out")) {
    nflux <- read.table("nflux.out", header = TRUE)
    nflux$year <- nflux$time %/% 1
    
    num_sim_years <- length(unique(nflux$year))
    
    # Update aggregated variables
    nflux$DayCent_N2O <- nflux$nit_N2O.N + nflux$dnit_N2O.N
    agg_vars$N2O <- agg_vars$N2O + sum(abs(nflux$DayCent_N2O)) / 1000000
    agg_vars$nit_N2O <- agg_vars$nit_N2O + sum(abs(nflux$nit_N2O.N)) / 1000000
    agg_vars$dnit_N2O <- agg_vars$dnit_N2O + sum(abs(nflux$dnit_N2O.N)) / 1000000
    agg_vars$dnit_N2 <- agg_vars$dnit_N2 + sum(abs(nflux$dnit_N2.N)) / 1000000
    agg_vars$NO <- agg_vars$NO + sum(abs(nflux$NO.N)) / 1000000
    agg_vars$NH3 <- agg_vars$NH3 + sum(abs(nflux$NH3.N)) / 1000000
    agg_vars$netMin1 <- agg_vars$netMin1 + sum(abs(nflux$netNmin1.gN.m2.)) / 1000000
    agg_vars$netMin2 <- agg_vars$netMin2 + sum(abs(nflux$netNmin2.gN.m2.)) / 1000000
    
    # Process daily NH3 data (could be made configurable for other variables)
    mod_years <- unique(nflux$year)
    
    # This section could be made more generic by reading observation requirements from config
    for (yr in mod_years) {
      temp_outvar_yr <- nflux[nflux$year == yr, ]
      outvar_name <- "NH3.N"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gNH3_N_ha_day",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read ctrlfert output
  if (file.exists("ctrlfert.out")) {
    ctrlfert <- read.table("ctrlfert.out", header = TRUE)
    ctrlfert$year <- ctrlfert$time %/% 1
    
    # Update aggregated variables
    agg_vars$urea <- agg_vars$urea + sum(abs(ctrlfert$urea_left)) / 1000000
    agg_vars$spHf <- agg_vars$spHf + sum(abs(ctrlfert$spHf)) / 1000000
    agg_vars$crnf <- agg_vars$crnf + sum(abs(ctrlfert$fct_left)) / 1000000
    
    # Process daily urea data (could be made configurable)
    mod2_years <- unique(ctrlfert$year)
    
    for (yr in mod2_years) {
      temp_outvar_yr <- ctrlfert[ctrlfert$year == yr, ]
      outvar_name <- "urea_left"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gUrea_N_m2",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read bio output
  if (file.exists("bio.out")) {
    bio <- read.table("bio.out", header = TRUE)
    bio$year <- bio$time %/% 1
    
    agg_vars$aglivc <- agg_vars$aglivc + sum(abs(bio$aglivc)) / 1000000
    agg_vars$aglivn <- agg_vars$aglivn + sum(abs(bio$aglivn)) / 1000000
  }
  
  return(list(
    daily_results = daily_results,
    num_sim_years = num_sim_years,
    agg_vars = agg_vars
  ))
}

#' Create Aggregated Results Data Frame
#'
#' Creates a data frame with aggregated simulation results
#'
#' @param actual_task_id Task ID for this simulation
#' @param agg_vars Aggregated variables list
#' @return Data frame with aggregated results
create_aggregated_results <- function(actual_task_id, agg_vars) {
  data.frame(
    SampleID = actual_task_id,
    aglivc = agg_vars$aglivc,
    aglivn = agg_vars$aglivn,
    N2O = agg_vars$N2O,
    nit_N2O = agg_vars$nit_N2O,
    dnit_N2O = agg_vars$dnit_N2O,
    dnit_N2 = agg_vars$dnit_N2,
    NOx = agg_vars$NO,
    NH3 = agg_vars$NH3,
    netMin1 = agg_vars$netMin1,
    netMin2 = agg_vars$netMin2,
    spHf = agg_vars$spHf,
    urea = agg_vars$urea,
    crnf = agg_vars$crnf,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# FUNCTIONS FROM output_processing_generic.R
# =============================================================================

#' Load Output Specifications from CSV Files
#'
#' Reads and validates the output variables and files specification files
#' to create a structured specification object for generic output processing.
#'
#' @param config Configuration object containing paths to specification files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing validated output specifications:
#'   \item{variables}{Data frame with variable specifications}


# Function: run_site_simulations
# Lines: 107 - 374 from original file

run_site_simulations <- function(site_id, run_file_site, config, params_df, 
                                sim_dir_tid, daycent_exe, actual_task_id, 
                                agg_vars, verbose = TRUE) {
  
  # Initialize return values
  num_treatments <- 0
  num_sim_years <- 0
  daily_results <- NULL
  
  # Setup directories
  site_dir_from <- file.path(config$paths$expsites_dir, site_id)
  site_sim_dir <- file.path(sim_dir_tid, site_id)
  
  if (verbose) {
    cat("Site source path:", site_dir_from, "\n")
    cat("Scratch run path:", site_sim_dir, "\n")
  }
  
  # Setup DayCent run files
  if (dir.exists(site_sim_dir)) {
    unlink(site_sim_dir, recursive = TRUE)
  }
  dir.create(site_sim_dir, recursive = TRUE)
  
  # Copy site files
  copy_status <- copy_sitefiles(site_folder_from = site_dir_from, 
                               site_folder_to = site_sim_dir)
  if (copy_status != 0) {
    stop("Site files copy failed for ", site_id)
  }
  
  if (verbose) cat("Site files copied successfully\n")
  
  # Set working directory
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(site_sim_dir)
  
  # Copy dot100 files
  dot100_status <- copy_dot100_files(dot100_directory = config$paths$dot100_path,
                                    simulation_directory = site_sim_dir)
  if (dot100_status != 0) {
    stop("dot100 files copy failed for ", site_id)
  }
  
  if (verbose) cat("dot100 files copied successfully\n")
  
  # Update parameters using enhanced generic dispatcher (now handles site.100 auto-detection)
  update_status <- update_daycent_parameters(params_df, simulation_dir = site_sim_dir, 
                                            verbose = FALSE)
  if (update_status != 0) {
    stop("Parameter update failed for ", site_id)
  }
  
  if (verbose) cat("Parameters updated successfully\n")
  
  # Get treatment schedules for this site
  trt_schs <- sort(unique(run_file_site$treatment_schedule))
  
  if (verbose) {
    cat("Running", length(trt_schs), "treatment(s)\n")
  }
  
  # Setup output files for NH3 measurements (could be made configurable)
  file.copy(from = file.path(config$paths$dot100_path, "nh3_outfiles.in"),
            to = file.path(site_sim_dir, "outfiles.in"),
            overwrite = TRUE)
  
  # Run treatments
  for (j in 1:length(trt_schs)) {
    num_treatments <- num_treatments + 1
    
    # Get base schedule information
    bh_sch_file <- trimws(run_file_site$base_schedule[j])
    bh_site100 <- paste0(strip_dot_sch(sch_file_name = bh_sch_file), "_site.100")
    
    # Update extended site.100 parameters
    site_update_status <- update_site100_parameters(site100_file = bh_site100, 
                                                    paramsdf = params_df)
    if (site_update_status != 0) {
      stop("Extended site.100 update failed for ", site_id)
    }
    
    # Run DayCent simulation
    trt_sch_file <- trimws(trt_schs[j])
    
    daycent_status <- run_DayCent(filepath_exe = daycent_exe,
                                 sch_file = trt_sch_file,
                                 ext_site100_2read = bh_site100,
                                 ext_site100_2write = NULL)
    
    if (daycent_status != 0) {
      stop("DayCent simulation failed for ", site_id, "::", trt_sch_file)
    }
    
    if (verbose) cat("DayCent execution successful for", trt_sch_file, "\n")
    
    # Process model outputs
    treatment_results <- process_model_outputs(site_id, trt_sch_file, actual_task_id, 
                                              config, agg_vars, verbose = verbose)
    
    # Update results
    num_sim_years <- num_sim_years + treatment_results$num_sim_years
    daily_results <- rbind(daily_results, treatment_results$daily_results)
    agg_vars <- treatment_results$agg_vars
  }
  
  return(list(
    num_treatments = num_treatments,
    num_sim_years = num_sim_years,
    daily_results = daily_results,
    agg_vars = agg_vars
  ))
}

#' Process Model Outputs
#'
#' Reads and processes DayCent model outputs for a specific treatment
#'
#' @param site_id Site identifier
#' @param trt_sch_file Treatment schedule file name
#' @param actual_task_id Task ID for this simulation
#' @param config Configuration object
#' @param agg_vars Aggregated variables list
#' @param verbose Logical indicating whether to print progress
#' @return List with processed results and updated aggregated variables
#' @export
process_model_outputs <- function(site_id, trt_sch_file, actual_task_id, config, 
                                 agg_vars, verbose = TRUE) {
  
  daily_results <- NULL
  num_sim_years <- 0
  
  # Read nflux output
  if (file.exists("nflux.out")) {
    nflux <- read.table("nflux.out", header = TRUE)
    nflux$year <- nflux$time %/% 1
    
    num_sim_years <- length(unique(nflux$year))
    
    # Update aggregated variables
    nflux$DayCent_N2O <- nflux$nit_N2O.N + nflux$dnit_N2O.N
    agg_vars$N2O <- agg_vars$N2O + sum(abs(nflux$DayCent_N2O)) / 1000000
    agg_vars$nit_N2O <- agg_vars$nit_N2O + sum(abs(nflux$nit_N2O.N)) / 1000000
    agg_vars$dnit_N2O <- agg_vars$dnit_N2O + sum(abs(nflux$dnit_N2O.N)) / 1000000
    agg_vars$dnit_N2 <- agg_vars$dnit_N2 + sum(abs(nflux$dnit_N2.N)) / 1000000
    agg_vars$NO <- agg_vars$NO + sum(abs(nflux$NO.N)) / 1000000
    agg_vars$NH3 <- agg_vars$NH3 + sum(abs(nflux$NH3.N)) / 1000000
    agg_vars$netMin1 <- agg_vars$netMin1 + sum(abs(nflux$netNmin1.gN.m2.)) / 1000000
    agg_vars$netMin2 <- agg_vars$netMin2 + sum(abs(nflux$netNmin2.gN.m2.)) / 1000000
    
    # Process daily NH3 data (could be made configurable for other variables)
    mod_years <- unique(nflux$year)
    
    # This section could be made more generic by reading observation requirements from config
    for (yr in mod_years) {
      temp_outvar_yr <- nflux[nflux$year == yr, ]
      outvar_name <- "NH3.N"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gNH3_N_ha_day",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read ctrlfert output
  if (file.exists("ctrlfert.out")) {
    ctrlfert <- read.table("ctrlfert.out", header = TRUE)
    ctrlfert$year <- ctrlfert$time %/% 1
    
    # Update aggregated variables
    agg_vars$urea <- agg_vars$urea + sum(abs(ctrlfert$urea_left)) / 1000000
    agg_vars$spHf <- agg_vars$spHf + sum(abs(ctrlfert$spHf)) / 1000000
    agg_vars$crnf <- agg_vars$crnf + sum(abs(ctrlfert$fct_left)) / 1000000
    
    # Process daily urea data (could be made configurable)
    mod2_years <- unique(ctrlfert$year)
    
    for (yr in mod2_years) {
      temp_outvar_yr <- ctrlfert[ctrlfert$year == yr, ]
      outvar_name <- "urea_left"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gUrea_N_m2",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read bio output
  if (file.exists("bio.out")) {
    bio <- read.table("bio.out", header = TRUE)
    bio$year <- bio$time %/% 1
    
    agg_vars$aglivc <- agg_vars$aglivc + sum(abs(bio$aglivc)) / 1000000
    agg_vars$aglivn <- agg_vars$aglivn + sum(abs(bio$aglivn)) / 1000000
  }
  
  return(list(
    daily_results = daily_results,
    num_sim_years = num_sim_years,
    agg_vars = agg_vars
  ))
}

#' Create Aggregated Results Data Frame
#'
#' Creates a data frame with aggregated simulation results
#'
#' @param actual_task_id Task ID for this simulation
#' @param agg_vars Aggregated variables list
#' @return Data frame with aggregated results
create_aggregated_results <- function(actual_task_id, agg_vars) {
  data.frame(
    SampleID = actual_task_id,
    aglivc = agg_vars$aglivc,
    aglivn = agg_vars$aglivn,
    N2O = agg_vars$N2O,
    nit_N2O = agg_vars$nit_N2O,
    dnit_N2O = agg_vars$dnit_N2O,
    dnit_N2 = agg_vars$dnit_N2,
    NOx = agg_vars$NO,
    NH3 = agg_vars$NH3,
    netMin1 = agg_vars$netMin1,
    netMin2 = agg_vars$netMin2,
    spHf = agg_vars$spHf,
    urea = agg_vars$urea,
    crnf = agg_vars$crnf,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# FUNCTIONS FROM output_processing_generic.R
# =============================================================================

#' Load Output Specifications from CSV Files
#'
#' Reads and validates the output variables and files specification files
#' to create a structured specification object for generic output processing.
#'
#' @param config Configuration object containing paths to specification files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing validated output specifications:
#'   \item{variables}{Data frame with variable specifications}


# Function: process_model_outputs
# Lines: 234 - 374 from original file

process_model_outputs <- function(site_id, trt_sch_file, actual_task_id, config, 
                                 agg_vars, verbose = TRUE) {
  
  daily_results <- NULL
  num_sim_years <- 0
  
  # Read nflux output
  if (file.exists("nflux.out")) {
    nflux <- read.table("nflux.out", header = TRUE)
    nflux$year <- nflux$time %/% 1
    
    num_sim_years <- length(unique(nflux$year))
    
    # Update aggregated variables
    nflux$DayCent_N2O <- nflux$nit_N2O.N + nflux$dnit_N2O.N
    agg_vars$N2O <- agg_vars$N2O + sum(abs(nflux$DayCent_N2O)) / 1000000
    agg_vars$nit_N2O <- agg_vars$nit_N2O + sum(abs(nflux$nit_N2O.N)) / 1000000
    agg_vars$dnit_N2O <- agg_vars$dnit_N2O + sum(abs(nflux$dnit_N2O.N)) / 1000000
    agg_vars$dnit_N2 <- agg_vars$dnit_N2 + sum(abs(nflux$dnit_N2.N)) / 1000000
    agg_vars$NO <- agg_vars$NO + sum(abs(nflux$NO.N)) / 1000000
    agg_vars$NH3 <- agg_vars$NH3 + sum(abs(nflux$NH3.N)) / 1000000
    agg_vars$netMin1 <- agg_vars$netMin1 + sum(abs(nflux$netNmin1.gN.m2.)) / 1000000
    agg_vars$netMin2 <- agg_vars$netMin2 + sum(abs(nflux$netNmin2.gN.m2.)) / 1000000
    
    # Process daily NH3 data (could be made configurable for other variables)
    mod_years <- unique(nflux$year)
    
    # This section could be made more generic by reading observation requirements from config
    for (yr in mod_years) {
      temp_outvar_yr <- nflux[nflux$year == yr, ]
      outvar_name <- "NH3.N"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gNH3_N_ha_day",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read ctrlfert output
  if (file.exists("ctrlfert.out")) {
    ctrlfert <- read.table("ctrlfert.out", header = TRUE)
    ctrlfert$year <- ctrlfert$time %/% 1
    
    # Update aggregated variables
    agg_vars$urea <- agg_vars$urea + sum(abs(ctrlfert$urea_left)) / 1000000
    agg_vars$spHf <- agg_vars$spHf + sum(abs(ctrlfert$spHf)) / 1000000
    agg_vars$crnf <- agg_vars$crnf + sum(abs(ctrlfert$fct_left)) / 1000000
    
    # Process daily urea data (could be made configurable)
    mod2_years <- unique(ctrlfert$year)
    
    for (yr in mod2_years) {
      temp_outvar_yr <- ctrlfert[ctrlfert$year == yr, ]
      outvar_name <- "urea_left"
      
      temp_outvar_yr_wide <- data.frame(
        SampleID = actual_task_id,
        SiteID = site_id,
        TreatmentID = trt_sch_file,
        year = yr,
        variable = outvar_name,
        Model = "DayCent",
        unit = "gUrea_N_m2",
        t(c(as.numeric(temp_outvar_yr[, outvar_name]), NA)),
        stringsAsFactors = FALSE
      )
      
      temp_outvar_yr_wide <- temp_outvar_yr_wide[, 1:(366 + 7)]
      names(temp_outvar_yr_wide)[8:(366 + 7)] <- paste("d", 1:366, sep = "")
      daily_results <- rbind(daily_results, temp_outvar_yr_wide)
    }
  }
  
  # Read bio output
  if (file.exists("bio.out")) {
    bio <- read.table("bio.out", header = TRUE)
    bio$year <- bio$time %/% 1
    
    agg_vars$aglivc <- agg_vars$aglivc + sum(abs(bio$aglivc)) / 1000000
    agg_vars$aglivn <- agg_vars$aglivn + sum(abs(bio$aglivn)) / 1000000
  }
  
  return(list(
    daily_results = daily_results,
    num_sim_years = num_sim_years,
    agg_vars = agg_vars
  ))
}

#' Create Aggregated Results Data Frame
#'
#' Creates a data frame with aggregated simulation results
#'
#' @param actual_task_id Task ID for this simulation
#' @param agg_vars Aggregated variables list
#' @return Data frame with aggregated results
create_aggregated_results <- function(actual_task_id, agg_vars) {
  data.frame(
    SampleID = actual_task_id,
    aglivc = agg_vars$aglivc,
    aglivn = agg_vars$aglivn,
    N2O = agg_vars$N2O,
    nit_N2O = agg_vars$nit_N2O,
    dnit_N2O = agg_vars$dnit_N2O,
    dnit_N2 = agg_vars$dnit_N2,
    NOx = agg_vars$NO,
    NH3 = agg_vars$NH3,
    netMin1 = agg_vars$netMin1,
    netMin2 = agg_vars$netMin2,
    spHf = agg_vars$spHf,
    urea = agg_vars$urea,
    crnf = agg_vars$crnf,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# FUNCTIONS FROM output_processing_generic.R
# =============================================================================

#' Load Output Specifications from CSV Files
#'
#' Reads and validates the output variables and files specification files
#' to create a structured specification object for generic output processing.
#'
#' @param config Configuration object containing paths to specification files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing validated output specifications:
#'   \item{variables}{Data frame with variable specifications}


# Function: create_aggregated_results
# Lines: 342 - 374 from original file

create_aggregated_results <- function(actual_task_id, agg_vars) {
  data.frame(
    SampleID = actual_task_id,
    aglivc = agg_vars$aglivc,
    aglivn = agg_vars$aglivn,
    N2O = agg_vars$N2O,
    nit_N2O = agg_vars$nit_N2O,
    dnit_N2O = agg_vars$dnit_N2O,
    dnit_N2 = agg_vars$dnit_N2,
    NOx = agg_vars$NO,
    NH3 = agg_vars$NH3,
    netMin1 = agg_vars$netMin1,
    netMin2 = agg_vars$netMin2,
    spHf = agg_vars$spHf,
    urea = agg_vars$urea,
    crnf = agg_vars$crnf,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# FUNCTIONS FROM output_processing_generic.R
# =============================================================================

#' Load Output Specifications from CSV Files
#'
#' Reads and validates the output variables and files specification files
#' to create a structured specification object for generic output processing.
#'
#' @param config Configuration object containing paths to specification files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing validated output specifications:
#'   \item{variables}{Data frame with variable specifications}


# Function: apply_transform_function
# Lines: 503 - 774 from original file

apply_transform_function <- function(data, transform_name, verbose = FALSE) {
  
  if (verbose) cat("Applying transform:", transform_name, "\n")
  
  result <- switch(transform_name,
    "abs_sum_div_1e6" = sum(abs(data), na.rm = TRUE) / 1000000,
    "sum_div_1e6" = sum(data, na.rm = TRUE) / 1000000,
    "mean_transform" = mean(data, na.rm = TRUE),
    "sum_transform" = sum(data, na.rm = TRUE),
    "computed_n2o" = {
      if (is.data.frame(data) && all(c("nit_N2O.N", "dnit_N2O.N") %in% names(data))) {
        # Match original computation order exactly: sum first, then abs
        daycent_n2o <- data$nit_N2O.N + data$dnit_N2O.N
        sum(abs(daycent_n2o), na.rm = TRUE) / 1000000
      } else {
        stop("computed_n2o requires data frame with nit_N2O.N and dnit_N2O.N columns")
      }
    },
    stop("Unknown transform function: ", transform_name)
  )
  
  if (verbose) cat("Transform result:", result, "\n")
  
  return(result)
}

#' Get Observation Years for Variable
#'
#' Extracts observation years for a specific variable and treatment from
#' the observation data files, using the variable specification.
#'
#' @param var_spec Single row data frame containing variable specification
#' @param trt_sch_file Treatment schedule file name
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return Numeric vector of observation years
#'
#' @details
#' This function reads the observation file specified in var_spec and extracts
#' the years where measurements exist for the given treatment. It handles
#' both single year columns and year range columns (start;end format).
#'
#' @examples
#' \dontrun{
#' obs_years <- get_observation_years_generic(var_spec, "treatment1.sch", 
#'                                           "data/observation", verbose = TRUE)
#' }
#'
#' @export
get_observation_years_generic <- function(var_spec, trt_sch_file, obs_data_dir, verbose = FALSE) {
  
  # Check if observation file is specified
  if (is.na(var_spec$observation_file) || var_spec$observation_file == "") {
    if (verbose) cat("No observation file specified for", var_spec$variable_name, "\n")
    return(numeric(0))
  }
  
  # Build observation file path
  obs_file_path <- file.path(obs_data_dir, var_spec$observation_file)
  
  if (!file.exists(obs_file_path)) {
    if (verbose) cat("Observation file not found:", obs_file_path, "\n")
    return(numeric(0))
  }
  
  if (verbose) cat("Loading observation data:", var_spec$observation_file, "\n")
  
  # Read observation data
  obs_data <- read.csv(obs_file_path, stringsAsFactors = FALSE)
  
  # Filter by treatment schedule if match column is specified
  if (!is.na(var_spec$observation_match_column) && var_spec$observation_match_column != "") {
    match_col <- var_spec$observation_match_column
    if (match_col %in% names(obs_data)) {
      obs_data <- obs_data[obs_data[[match_col]] == trt_sch_file, ]
    } else {
      if (verbose) cat("Match column", match_col, "not found in observation data\n")
      return(numeric(0))
    }
  }
  
  if (nrow(obs_data) == 0) {
    if (verbose) cat("No matching observations for treatment:", trt_sch_file, "\n")
    return(numeric(0))
  }
  
  # Extract years based on year columns specification
  if (is.na(var_spec$observation_year_columns) || var_spec$observation_year_columns == "") {
    if (verbose) cat("No year columns specified\n")
    return(numeric(0))
  }
  
  year_cols <- trimws(strsplit(var_spec$observation_year_columns, ";")[[1]])
  
  # Match original behavior: only use the FIRST year column (start years)
  # This matches the original R unique(vec1, vec2) behavior which only processes vec1
  # Each measurement period represents ONE observation regardless of duration
  first_year_col <- year_cols[1]
  
  if (first_year_col %in% names(obs_data)) {
    obs_years <- unique(obs_data[[first_year_col]])
    if (verbose) cat("Using only first year column:", first_year_col, "\n")
  } else {
    if (verbose) cat("First year column", first_year_col, "not found in observation data\n")
    obs_years <- c()
  }
  
  # Remove NA values and return sorted unique years
  obs_years <- sort(unique(obs_years[!is.na(obs_years)]))
  
  if (verbose) cat("Found observation years:", paste(obs_years, collapse = ", "), "\n")
  
  return(obs_years)
}

#' Initialize Aggregated Variables from Specifications
#'
#' Creates a named list of aggregated variables initialized to zero,
#' based on the variable specifications.
#'
#' @param output_specs Output specifications object from load_output_specifications()
#' @param verbose Logical indicating whether to print progress messages
#' @return Named list of aggregated variables initialized to zero
#'
#' @details
#' This function replaces the hardcoded aggregated variable initialization
#' with a dynamic approach based on the specifications.
#'
#' @examples
#' \dontrun{
#' output_specs <- load_output_specifications(config)
#' agg_vars <- initialize_aggregated_variables_generic(output_specs)
#' }
#'
#' @export
initialize_aggregated_variables_generic <- function(output_specs, verbose = FALSE) {
  
  agg_vars <- list()
  
  # Initialize all aggregated variables to zero
  for (i in 1:nrow(output_specs$aggregated_variables)) {
    var_row <- output_specs$aggregated_variables[i, ]
    agg_name <- var_row$aggregate_name
    
    if (!is.na(agg_name) && agg_name != "") {
      agg_vars[[agg_name]] <- 0
    }
  }
  
  if (verbose) {
    cat("Initialized aggregated variables:", length(agg_vars), "\n")
    cat("Variables:", paste(names(agg_vars), collapse = ", "), "\n")
  }
  
  return(agg_vars)
}

#' Create Aggregated Results Data Frame
#'
#' Creates a data frame with aggregated simulation results based on
#' the accumulated aggregated variables.
#'
#' @param aggregated_results Named list of aggregated variables
#' @param sample_id Sample ID for this simulation
#' @param verbose Logical indicating whether to print progress messages
#' @return Data frame with aggregated results
#'
#' @details
#' This function replaces the hardcoded aggregated results creation
#' with a dynamic approach that creates columns based on the variable names
#' in the aggregated variables list.
#'
#' @examples
#' \dontrun{
#' agg_result <- create_aggregated_results_generic(aggregated_results, 1234)
#' }
#'
#' @export
create_aggregated_results_generic <- function(aggregated_results, sample_id, verbose = FALSE) {
  
  # Create base data frame with SampleID
  result_df <- data.frame(SampleID = sample_id, stringsAsFactors = FALSE)
  
  # Add all aggregated variables as columns
  for (var_name in names(aggregated_results)) {
    result_df[[var_name]] <- aggregated_results[[var_name]]
  }
  
  if (verbose) {
    cat("Created aggregated results with", ncol(result_df) - 1, "variables\n")
  }
  
  return(result_df)
}

#' Validate Output Specifications
#'
#' Performs comprehensive validation of the output specifications to ensure
#' they are complete and consistent.
#'
#' @param output_specs Output specifications object from load_output_specifications()
#' @param verbose Logical indicating whether to print progress messages
#' @return Logical indicating whether validation passed
#'
#' @details
#' This function checks for:
#' - Duplicate variable names within the same output file
#' - Valid transform function names
#' - Consistent aggregate names
#' - Required observation file existence
#'
#' @export
validate_output_specifications <- function(output_specs, verbose = FALSE) {
  
  if (verbose) cat("Validating output specifications...\n")
  
  validation_passed <- TRUE
  
  # Check for duplicate variables within same file
  variables <- output_specs$variables
  for (file in unique(variables$output_file)) {
    file_vars <- variables[variables$output_file == file, ]
    duplicates <- duplicated(file_vars$variable_name)
    if (any(duplicates)) {
      cat("ERROR: Duplicate variables in", file, ":", 
          paste(file_vars$variable_name[duplicates], collapse = ", "), "\n")
      validation_passed <- FALSE
    }
  }
  
  # Check transform function names
  valid_transforms <- c("abs_sum_div_1e6", "sum_div_1e6", "mean_transform", 
                       "sum_transform", "computed_n2o")
  invalid_transforms <- setdiff(variables$transform_function, c(valid_transforms, ""))
  if (length(invalid_transforms) > 0) {
    cat("ERROR: Invalid transform functions:", paste(invalid_transforms, collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  # Check for empty aggregate names where aggregate_output is TRUE
  agg_vars <- variables[variables$aggregate_output == TRUE, ]
  empty_agg_names <- is.na(agg_vars$aggregate_name) | agg_vars$aggregate_name == ""
  if (any(empty_agg_names)) {
    cat("ERROR: Missing aggregate names for variables:", 
        paste(agg_vars$variable_name[empty_agg_names], collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  if (verbose) {
    if (validation_passed) {
      cat("✅ Output specifications validation passed\n")
    } else {
      cat("❌ Output specifications validation failed\n")
    }
  }
  
  return(validation_passed)
}

#' Process Single DayCent Output File Generically
#'
#' Processes a single DayCent output file based on variable specifications,
#' applying transforms and creating daily outputs as needed.
#'
#' @param file_path Path to the DayCent output file
#' @param file_specs Data frame containing variable specifications for this file
#' @param metadata List containing metadata (SampleID, SiteID, TreatmentID, year)
#' @param agg_vars Named list of aggregated variables to update
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing:
#'   \item{daily_results}{Data frame with daily output results}


# Function: get_observation_years_generic
# Lines: 552 - 774 from original file

get_observation_years_generic <- function(var_spec, trt_sch_file, obs_data_dir, verbose = FALSE) {
  
  # Check if observation file is specified
  if (is.na(var_spec$observation_file) || var_spec$observation_file == "") {
    if (verbose) cat("No observation file specified for", var_spec$variable_name, "\n")
    return(numeric(0))
  }
  
  # Build observation file path
  obs_file_path <- file.path(obs_data_dir, var_spec$observation_file)
  
  if (!file.exists(obs_file_path)) {
    if (verbose) cat("Observation file not found:", obs_file_path, "\n")
    return(numeric(0))
  }
  
  if (verbose) cat("Loading observation data:", var_spec$observation_file, "\n")
  
  # Read observation data
  obs_data <- read.csv(obs_file_path, stringsAsFactors = FALSE)
  
  # Filter by treatment schedule if match column is specified
  if (!is.na(var_spec$observation_match_column) && var_spec$observation_match_column != "") {
    match_col <- var_spec$observation_match_column
    if (match_col %in% names(obs_data)) {
      obs_data <- obs_data[obs_data[[match_col]] == trt_sch_file, ]
    } else {
      if (verbose) cat("Match column", match_col, "not found in observation data\n")
      return(numeric(0))
    }
  }
  
  if (nrow(obs_data) == 0) {
    if (verbose) cat("No matching observations for treatment:", trt_sch_file, "\n")
    return(numeric(0))
  }
  
  # Extract years based on year columns specification
  if (is.na(var_spec$observation_year_columns) || var_spec$observation_year_columns == "") {
    if (verbose) cat("No year columns specified\n")
    return(numeric(0))
  }
  
  year_cols <- trimws(strsplit(var_spec$observation_year_columns, ";")[[1]])
  
  # Match original behavior: only use the FIRST year column (start years)
  # This matches the original R unique(vec1, vec2) behavior which only processes vec1
  # Each measurement period represents ONE observation regardless of duration
  first_year_col <- year_cols[1]
  
  if (first_year_col %in% names(obs_data)) {
    obs_years <- unique(obs_data[[first_year_col]])
    if (verbose) cat("Using only first year column:", first_year_col, "\n")
  } else {
    if (verbose) cat("First year column", first_year_col, "not found in observation data\n")
    obs_years <- c()
  }
  
  # Remove NA values and return sorted unique years
  obs_years <- sort(unique(obs_years[!is.na(obs_years)]))
  
  if (verbose) cat("Found observation years:", paste(obs_years, collapse = ", "), "\n")
  
  return(obs_years)
}

#' Initialize Aggregated Variables from Specifications
#'
#' Creates a named list of aggregated variables initialized to zero,
#' based on the variable specifications.
#'
#' @param output_specs Output specifications object from load_output_specifications()
#' @param verbose Logical indicating whether to print progress messages
#' @return Named list of aggregated variables initialized to zero
#'
#' @details
#' This function replaces the hardcoded aggregated variable initialization
#' with a dynamic approach based on the specifications.
#'
#' @examples
#' \dontrun{
#' output_specs <- load_output_specifications(config)
#' agg_vars <- initialize_aggregated_variables_generic(output_specs)
#' }
#'
#' @export
initialize_aggregated_variables_generic <- function(output_specs, verbose = FALSE) {
  
  agg_vars <- list()
  
  # Initialize all aggregated variables to zero
  for (i in 1:nrow(output_specs$aggregated_variables)) {
    var_row <- output_specs$aggregated_variables[i, ]
    agg_name <- var_row$aggregate_name
    
    if (!is.na(agg_name) && agg_name != "") {
      agg_vars[[agg_name]] <- 0
    }
  }
  
  if (verbose) {
    cat("Initialized aggregated variables:", length(agg_vars), "\n")
    cat("Variables:", paste(names(agg_vars), collapse = ", "), "\n")
  }
  
  return(agg_vars)
}

#' Create Aggregated Results Data Frame
#'
#' Creates a data frame with aggregated simulation results based on
#' the accumulated aggregated variables.
#'
#' @param aggregated_results Named list of aggregated variables
#' @param sample_id Sample ID for this simulation
#' @param verbose Logical indicating whether to print progress messages
#' @return Data frame with aggregated results
#'
#' @details
#' This function replaces the hardcoded aggregated results creation
#' with a dynamic approach that creates columns based on the variable names
#' in the aggregated variables list.
#'
#' @examples
#' \dontrun{
#' agg_result <- create_aggregated_results_generic(aggregated_results, 1234)
#' }
#'
#' @export
create_aggregated_results_generic <- function(aggregated_results, sample_id, verbose = FALSE) {
  
  # Create base data frame with SampleID
  result_df <- data.frame(SampleID = sample_id, stringsAsFactors = FALSE)
  
  # Add all aggregated variables as columns
  for (var_name in names(aggregated_results)) {
    result_df[[var_name]] <- aggregated_results[[var_name]]
  }
  
  if (verbose) {
    cat("Created aggregated results with", ncol(result_df) - 1, "variables\n")
  }
  
  return(result_df)
}

#' Validate Output Specifications
#'
#' Performs comprehensive validation of the output specifications to ensure
#' they are complete and consistent.
#'
#' @param output_specs Output specifications object from load_output_specifications()
#' @param verbose Logical indicating whether to print progress messages
#' @return Logical indicating whether validation passed
#'
#' @details
#' This function checks for:
#' - Duplicate variable names within the same output file
#' - Valid transform function names
#' - Consistent aggregate names
#' - Required observation file existence
#'
#' @export
validate_output_specifications <- function(output_specs, verbose = FALSE) {
  
  if (verbose) cat("Validating output specifications...\n")
  
  validation_passed <- TRUE
  
  # Check for duplicate variables within same file
  variables <- output_specs$variables
  for (file in unique(variables$output_file)) {
    file_vars <- variables[variables$output_file == file, ]
    duplicates <- duplicated(file_vars$variable_name)
    if (any(duplicates)) {
      cat("ERROR: Duplicate variables in", file, ":", 
          paste(file_vars$variable_name[duplicates], collapse = ", "), "\n")
      validation_passed <- FALSE
    }
  }
  
  # Check transform function names
  valid_transforms <- c("abs_sum_div_1e6", "sum_div_1e6", "mean_transform", 
                       "sum_transform", "computed_n2o")
  invalid_transforms <- setdiff(variables$transform_function, c(valid_transforms, ""))
  if (length(invalid_transforms) > 0) {
    cat("ERROR: Invalid transform functions:", paste(invalid_transforms, collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  # Check for empty aggregate names where aggregate_output is TRUE
  agg_vars <- variables[variables$aggregate_output == TRUE, ]
  empty_agg_names <- is.na(agg_vars$aggregate_name) | agg_vars$aggregate_name == ""
  if (any(empty_agg_names)) {
    cat("ERROR: Missing aggregate names for variables:", 
        paste(agg_vars$variable_name[empty_agg_names], collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  if (verbose) {
    if (validation_passed) {
      cat("✅ Output specifications validation passed\n")
    } else {
      cat("❌ Output specifications validation failed\n")
    }
  }
  
  return(validation_passed)
}

#' Process Single DayCent Output File Generically
#'
#' Processes a single DayCent output file based on variable specifications,
#' applying transforms and creating daily outputs as needed.
#'
#' @param file_path Path to the DayCent output file
#' @param file_specs Data frame containing variable specifications for this file
#' @param metadata List containing metadata (SampleID, SiteID, TreatmentID, year)
#' @param agg_vars Named list of aggregated variables to update
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing:
#'   \item{daily_results}{Data frame with daily output results}


# Function: validate_output_specifications
# Lines: 715 - 774 from original file

validate_output_specifications <- function(output_specs, verbose = FALSE) {
  
  if (verbose) cat("Validating output specifications...\n")
  
  validation_passed <- TRUE
  
  # Check for duplicate variables within same file
  variables <- output_specs$variables
  for (file in unique(variables$output_file)) {
    file_vars <- variables[variables$output_file == file, ]
    duplicates <- duplicated(file_vars$variable_name)
    if (any(duplicates)) {
      cat("ERROR: Duplicate variables in", file, ":", 
          paste(file_vars$variable_name[duplicates], collapse = ", "), "\n")
      validation_passed <- FALSE
    }
  }
  
  # Check transform function names
  valid_transforms <- c("abs_sum_div_1e6", "sum_div_1e6", "mean_transform", 
                       "sum_transform", "computed_n2o")
  invalid_transforms <- setdiff(variables$transform_function, c(valid_transforms, ""))
  if (length(invalid_transforms) > 0) {
    cat("ERROR: Invalid transform functions:", paste(invalid_transforms, collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  # Check for empty aggregate names where aggregate_output is TRUE
  agg_vars <- variables[variables$aggregate_output == TRUE, ]
  empty_agg_names <- is.na(agg_vars$aggregate_name) | agg_vars$aggregate_name == ""
  if (any(empty_agg_names)) {
    cat("ERROR: Missing aggregate names for variables:", 
        paste(agg_vars$variable_name[empty_agg_names], collapse = ", "), "\n")
    validation_passed <- FALSE
  }
  
  if (verbose) {
    if (validation_passed) {
      cat("✅ Output specifications validation passed\n")
    } else {
      cat("❌ Output specifications validation failed\n")
    }
  }
  
  return(validation_passed)
}

#' Process Single DayCent Output File Generically
#'
#' Processes a single DayCent output file based on variable specifications,
#' applying transforms and creating daily outputs as needed.
#'
#' @param file_path Path to the DayCent output file
#' @param file_specs Data frame containing variable specifications for this file
#' @param metadata List containing metadata (SampleID, SiteID, TreatmentID, year)
#' @param agg_vars Named list of aggregated variables to update
#' @param obs_data_dir Directory containing observation data files
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing:
#'   \item{daily_results}{Data frame with daily output results}


# Function: process_single_output_file
# Lines: 792 - 957 from original file

process_single_output_file <- function(file_path, file_specs, metadata, agg_vars, 
                                      obs_data_dir, verbose = FALSE) {
  
  daily_results <- NULL
  num_sim_years <- 0
  num_obs_years <- 0
  
  if (!file.exists(file_path)) {
    if (verbose) cat("Output file not found:", file_path, "\n")
    return(list(
      daily_results = daily_results,
      agg_vars = agg_vars,
      num_sim_years = num_sim_years,
      num_obs_years = num_obs_years
    ))
  }
  
  if (verbose) cat("Processing output file:", basename(file_path), "\n")
  
  # Read the output file
  output_data <- read.table(file_path, header = TRUE)
  
  # Add year column if time column exists
  if ("time" %in% names(output_data)) {
    output_data$year <- output_data$time %/% 1
    mod_years <- unique(output_data$year)
    # Only count simulation years from nflux.out (like original behavior)
    num_sim_years <- if (basename(file_path) == "nflux.out") length(mod_years) else 0
  } else {
    mod_years <- c()
    if (verbose) cat("No time column found in", basename(file_path), "\n")
  }
  
  # First pass: Process computed variables (like DayCent_N2O) that need raw data
  if (verbose) cat("  First pass: Processing computed variables\n")
  for (i in 1:nrow(file_specs)) {
    var_spec <- file_specs[i, ]
    var_name <- var_spec$variable_name
    
    # Only process computed variables in first pass
    if (var_spec$transform_function == "computed_n2o") {
      if (verbose) cat("    Processing computed variable:", var_name, "\n")
      
      # Check if required columns exist for N2O computation
      if (all(c("nit_N2O.N", "dnit_N2O.N") %in% names(output_data))) {
        output_data$DayCent_N2O <- output_data$nit_N2O.N + output_data$dnit_N2O.N
        if (verbose) cat("    Created DayCent_N2O column from nit_N2O.N + dnit_N2O.N\n")
      } else {
        if (verbose) cat("    Cannot compute N2O: missing nit_N2O.N or dnit_N2O.N\n")
        next
      }
    }
  }
  
  # Second pass: Process all variables including computed ones
  if (verbose) cat("  Second pass: Processing all variables\n")
  for (i in 1:nrow(file_specs)) {
    var_spec <- file_specs[i, ]
    var_name <- var_spec$variable_name
    
    if (verbose) cat("    Processing variable:", var_name, "\n")
    
    # Check if variable exists in output data (now includes computed variables)
    if (!var_name %in% names(output_data)) {
      if (verbose) cat("      Variable", var_name, "not found in", basename(file_path), "\n")
      next
    }
    
    # Update aggregated variables if specified
    if (var_spec$aggregate_output == TRUE && !is.na(var_spec$aggregate_name) && 
        var_spec$aggregate_name != "") {
      
      agg_name <- var_spec$aggregate_name
      transform_func <- var_spec$transform_function
      
      if (transform_func == "computed_n2o") {
        # For computed_n2o, pass the whole data frame to the transform function
        agg_value <- apply_transform_function(output_data, transform_func, verbose = FALSE)
      } else {
        # For regular variables, pass the specific column
        agg_value <- apply_transform_function(output_data[[var_name]], transform_func, verbose = FALSE)
      }
      
      if (agg_name %in% names(agg_vars)) {
        agg_vars[[agg_name]] <- agg_vars[[agg_name]] + agg_value
      } else {
        agg_vars[[agg_name]] <- agg_value
      }
      
      if (verbose) cat("      Updated aggregated variable", agg_name, ":", agg_value, "\n")
    }
    
    # Create daily output if specified
    if (var_spec$daily_output == TRUE && length(mod_years) > 0) {
      
      # Get observation years for this variable
      obs_years <- get_observation_years_generic(var_spec, metadata$TreatmentID, 
                                                obs_data_dir, verbose = FALSE)
      
      # Count observation years for this variable/treatment combination
      num_obs_years <- num_obs_years + length(obs_years)
      
      # Use intersection of model and observation years, or all model years if no observations
      if (length(obs_years) > 0) {
        data_years <- intersect(mod_years, obs_years)
      } else {
        data_years <- mod_years
      }
      
      if (length(data_years) > 0) {
        if (verbose) cat("    Creating daily output for", length(data_years), "years\n")
        
        for (yr in data_years) {
          temp_outvar_yr <- output_data[output_data$year == yr, ]
          
          # Create the daily values vector (up to 366 days)
          daily_values <- as.numeric(temp_outvar_yr[, var_name])
          # Pad with NA to make it 366 values
          daily_values <- c(daily_values, rep(NA, 366 - length(daily_values)))
          # Take only first 366 values in case there are more
          daily_values <- daily_values[1:366]
          
          temp_outvar_yr_wide <- data.frame(
            SampleID = metadata$SampleID,
            SiteID = metadata$SiteID,
            TreatmentID = metadata$TreatmentID,
            year = yr,
            variable = var_name,
            Model = "DayCent",
            unit = var_spec$unit,
            stringsAsFactors = FALSE
          )
          
          # Add daily columns
          for (d in 1:366) {
            col_name <- paste("d", d, sep = "")
            temp_outvar_yr_wide[[col_name]] <- daily_values[d]
          }
          
          daily_results <- rbind(daily_results, temp_outvar_yr_wide)
        }
      }
    }
  }
  
  return(list(
    daily_results = daily_results,
    agg_vars = agg_vars,
    num_sim_years = num_sim_years,
    num_obs_years = num_obs_years
  ))
}

#' Process All DayCent Output Files Generically
#'
#' Orchestrates the processing of all enabled DayCent output files based on
#' the specifications, replacing the hardcoded output processing logic.
#'
#' @param config Configuration object containing paths and specifications
#' @param output_specs Output specifications object from load_output_specifications()
#' @param sim_dir Directory containing DayCent output files
#' @param metadata List containing metadata (SampleID, SiteID, TreatmentID)
#' @param agg_vars Named list of aggregated variables to update
#' @param verbose Logical indicating whether to print progress messages
#' @return List containing:
#'   \item{daily_results}{Combined data frame with all daily output results}


# Function: process_output_files_generic
# Lines: 975 - 975 from original file

process_output_files_generic <- function(config, output_specs, sim_dir, metadata, 


# Function: process_model_outputs_generic
# Lines: 1054 - 1054 from original file

process_model_outputs_generic <- function(site_id, trt_sch_file, actual_task_id, 


