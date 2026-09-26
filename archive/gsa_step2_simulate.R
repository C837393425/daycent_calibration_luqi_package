#' Generic GSA Step 2 - Model Simulation
#'
#' This function provides a generic interface for running DayCent model simulations
#' as part of Global Sensitivity Analysis. It can be used for any calibration target
#' by providing appropriate configuration.
#'
#' @param config A complete YAML configuration object loaded with read_yaml_config()
#' @param task_id The SLURM task ID for this simulation instance
#' @param daycent_exe Path to DayCent executable file
#' @param scratch_dir Scratch directory for temporary simulation files
#' @param clean_scratch Logical indicating whether to clean scratch files after simulation
#' @param start_id Starting ID offset for task numbering (default: 0)
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return List containing:
#'   \item{status}{Integer status code (0 = success, 1 = error)}
#'   \item{daily_results}{Data frame with daily model outputs}
#'   \item{aggregated_results}{Data frame with aggregated model outputs}
#'   \item{runtime_info}{Data frame with execution timing and statistics}
#'   \item{task_id}{The actual task ID used}
#'   \item{output_files}{List of paths to output files created}
#'
#' @details
#' This function performs the following operations:
#' 1. Sets up simulation environment based on configuration
#' 2. Loads Monte Carlo parameter draws for the specified task
#' 3. Copies DayCent input files to scratch directory
#' 4. Updates parameter files using the generic parameter dispatcher
#' 5. Runs DayCent simulations for all experimental sites
#' 6. Processes and saves model outputs
#' 7. Cleans up temporary files if requested
#'
#' The function supports multiple output variables and can be configured for different
#' calibration targets through the configuration file.
#'
#' @examples
#' \dontrun{
#' # Load configuration
#' config <- read_yaml_config("workflows/configs/nh3_volatilization.yaml")
#' config <- resolve_config_paths(config)
#' 
#' # Run simulation
#' result <- gsa_step2_simulate(
#'   config = config,
#'   task_id = 1,
#'   daycent_exe = "/path/to/daycent_executable",
#'   scratch_dir = "/tmp/scratch",
#'   clean_scratch = TRUE
#' )
#' }
#'
#' @export
gsa_step2_simulate <- function(config, task_id, daycent_exe, scratch_dir, 
                               clean_scratch = TRUE, start_id = 0, verbose = TRUE) {
  
  # Initialize timing and status tracking
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE
  
  # Adjust task_id with start_id offset
  actual_task_id <- start_id + task_id
  
  # Initialize output variables
  output_result <- list(
    status = 1,  # Default to error
    daily_results = NULL,
    aggregated_results = NULL,
    runtime_info = NULL,
    task_id = actual_task_id,
    output_files = list()
  )
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 2 - Generic Model Simulation\n")
    cat("Node:", node, "\n")
    cat("Working Directory:", getwd(), "\n\n")
    cat("Configuration:\n")
    cat("\t Project Name                :", config$project$name, "\n")
    cat("\t DayCent Executable          :", daycent_exe, "\n")
    cat("\t Scratch Directory           :", scratch_dir, "\n")
    cat("\t Clean Scratch               :", clean_scratch, "\n")
    cat("\t Actual Task ID              :", actual_task_id, "\n\n")
    cat("====================================================================\n")
  }
  
  # Main simulation logic in tryCatch
  tryCatch({
    
    # Determine date stamp
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
    
    # Get GSA method for this task
    gsa_method <- config$gsa$gsa_methods[actual_task_id]
    
    # Setup paths
    paths <- setup_gsa_paths(config, date_stamp, gsa_method)
    base_gsa_dir <- dirname(paths$gsa_method_dir)  # Remove method-specific part
    
    # Read Monte Carlo draws to determine GSA method for this task
    method_mapping <- config$gsa$gsa_methods
    method_index <- ((actual_task_id - 1) %% length(method_mapping)) + 1
    smethod <- method_mapping[method_index]
    
    if (verbose) cat("Selected GSA method:", smethod, "\n")
    
    # Update paths with correct GSA method
    paths <- setup_gsa_paths(config, date_stamp, smethod)
    
    # Load task-specific Monte Carlo draws
    mc_draw_file <- file.path(paths$gsa_method_dir, paste0("mc_GSA_draw_", smethod, ".rds"))
    if (!file.exists(mc_draw_file)) {
      stop("Monte Carlo draw file not found: ", mc_draw_file)
    }
    
    all_jobs <- readRDS(mc_draw_file)
    job_params <- all_jobs[all_jobs$SampleID == actual_task_id, ]
    
    if (nrow(job_params) == 0) {
      stop("No parameters found for task ID: ", actual_task_id)
    }
    
    group_id <- job_params$JobGroup
    
    if (verbose) {
      cat("Job Group:", group_id, "\n")
      cat("Number of parameters:", ncol(job_params) - 2, "\n")  # Exclude SampleID and JobGroup
    }
    
    # Setup directory paths for this job group
    daily_out_dir <- file.path(paths$daily_out_dir, paste0("jobGroup_", group_id))
    aggregated_dir <- file.path(paths$aggregated_dir, paste0("jobGroup_", group_id))
    run_status_dir <- file.path(paths$run_status_dir, paste0("jobGroup_", group_id))
    
    # Initialize counters and data collectors
    num_sites <- 0
    num_trts <- 0
    num_sim_years <- 0
    dRslt <- NULL
    
    # Initialize aggregated output variables (these could be made configurable)
    agg_vars <- initialize_aggregated_variables()
    
    # Create parameter data frame using generic approach
    params_df <- prepare_parameter_set(config, job_params, verbose = verbose)
    
    # Load run file and experimental sites
    run_file_path <- file.path(paths$gsa_method_dir, "RunFile.rds")
    if (!file.exists(run_file_path)) {
      stop("Run file not found: ", run_file_path)
    }
    
    run_file <- readRDS(run_file_path)
    exp_site_ids <- sort(unique(run_file$siteID))
    
    # Setup simulation directory
    sim_dir_tid <- file.path(scratch_dir, date_stamp, "GSA", paste0("tid", actual_task_id))
    
    # Main simulation loop over experimental sites
    for (i in 1:length(exp_site_ids)) {
      num_sites <- num_sites + 1
      
      site_id <- trimws(exp_site_ids[i])
      run_file_site <- run_file[run_file$siteID == site_id, ]
      
      if (verbose) {
        cat("\n*************** Running SiteID:", site_id, "***************\n")
      }
      
      # Run simulations for this site
      site_result <- run_site_simulations(
        site_id = site_id,
        run_file_site = run_file_site,
        config = config,
        params_df = params_df,
        sim_dir_tid = sim_dir_tid,
        daycent_exe = daycent_exe,
        actual_task_id = actual_task_id,
        agg_vars = agg_vars,
        verbose = verbose
      )
      
      # Update counters and results
      num_trts <- num_trts + site_result$num_treatments
      num_sim_years <- num_sim_years + site_result$num_sim_years
      dRslt <- rbind(dRslt, site_result$daily_results)
      agg_vars <- site_result$agg_vars
      
      # Clean up site directory if requested
      if (clean_scratch) {
        site_sim_dir <- file.path(sim_dir_tid, site_id)
        if (dir.exists(site_sim_dir)) {
          unlink(site_sim_dir, recursive = TRUE)
        }
      }
    }
    
    # Create aggregated results data frame
    agg_result <- create_aggregated_results(actual_task_id, agg_vars)
    
    # Save results
    daily_output_file <- file.path(daily_out_dir, paste0("dc_dRslt_", actual_task_id, ".rds"))
    agg_output_file <- file.path(aggregated_dir, paste0("dc_aggRslt_", actual_task_id, ".rds"))
    
    saveRDS(dRslt, daily_output_file)
    saveRDS(agg_result, agg_output_file)
    
    # Calculate runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create runtime info
    runtime_info <- data.frame(
      SampleID = actual_task_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = num_trts,
      sim_years = num_sim_years,
      status = 0,
      message = "Execution Success..",
      stringsAsFactors = FALSE
    )
    
    # Save runtime info
    runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_task_id, ".csv"))
    write.csv(runtime_info, runtime_file, row.names = FALSE)
    
    # Clean up main simulation directory if requested
    if (clean_scratch && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
    
    complete <- TRUE
    
    # Update output result
    output_result$status <- 0
    output_result$daily_results <- dRslt
    output_result$aggregated_results <- agg_result
    output_result$runtime_info <- runtime_info
    output_result$output_files <- list(
      daily_output = daily_output_file,
      aggregated_output = agg_output_file,
      runtime_info = runtime_file
    )
    
    if (verbose) cat("--- GSA Step 2 Simulation Successfully Completed ---\n")
    
  }, error = function(err) {
    
    if (verbose) {
      cat("--- GSA Step 2 Simulation Failed ---\n")
      cat("Error:", as.character(err), "\n")
    }
    
    # Calculate error runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create error runtime info
    runtime_info <- data.frame(
      SampleID = actual_task_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = ifelse(exists("num_trts"), num_trts, 0),
      sim_years = ifelse(exists("num_sim_years"), num_sim_years, 0),
      status = 1,
      message = as.character(err),
      stringsAsFactors = FALSE
    )
    
    # Try to save error info if directory exists
    if (exists("run_status_dir") && dir.exists(run_status_dir)) {
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_task_id, "_error.csv"))
      write.csv(runtime_info, runtime_file, row.names = FALSE)
      output_result$output_files$runtime_info <- runtime_file
    }
    
    output_result$runtime_info <- runtime_info
    
    # Clean up on error if requested
    if (exists("sim_dir_tid") && clean_scratch && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
  })
  
  return(output_result)
}


#' Initialize Aggregated Variables
#'
#' Creates a list of aggregated variables for tracking cumulative outputs
#'
#' @return Named list of initialized aggregated variables
#' @export
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


#' GSA Step 2 - Individual Simulation Function
#'
#' This function processes ONE individual Monte Carlo simulation, matching the
#' original GSA_Step2_ModelSim.R behavior exactly. It replicates the original
#' script's logic where task_id directly represents a simulation SampleID.
#'
#' @param config A complete YAML configuration object loaded with read_yaml_config()
#' @param gsa_method The GSA method name (e.g., "soboljansen", "sobol", etc.)
#' @param sim_id The individual simulation ID (1 to MC_nsim)
#' @param daycent_exe Path to DayCent executable file
#' @param scratch_dir Scratch directory for temporary simulation files
#' @param clean_scratch Logical indicating whether to clean scratch files after simulation
#' @param start_id Starting ID offset for task numbering (default: 0)
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#'
#' @return List containing:
#'   \item{status}{Integer status code (0 = success, 1 = error)}
#'   \item{daily_results}{Data frame with daily model outputs}
#'   \item{aggregated_results}{Data frame with aggregated model outputs}
#'   \item{runtime_info}{Data frame with execution timing and statistics}
#'   \item{sim_id}{The actual simulation ID used}
#'   \item{output_files}{List of paths to output files created}
#'
#' @details
#' This function replicates the exact behavior of the original GSA_Step2_ModelSim.R
#' script, where:
#' 1. task_id directly represents a simulation SampleID (not a GSA method)
#' 2. Each call processes exactly ONE Monte Carlo simulation
#' 3. Parameters are loaded from mc_GSA_draw_<method>.rds using SampleID lookup
#' 4. Job grouping and output organization matches the original exactly
#'
#' @examples
#' \dontrun{
#' # Process individual simulation 1234 for soboljansen method
#' result <- gsa_step2_simulate_individual(
#'   config = config,
#'   gsa_method = "soboljansen",
#'   sim_id = 1234,
#'   daycent_exe = "/path/to/daycent_executable",
#'   scratch_dir = "/tmp/scratch"
#' )
#' }
#'
#' @export
gsa_step2_simulate_individual <- function(config, gsa_method, sim_id, daycent_exe, 
                                         scratch_dir, clean_scratch = TRUE, 
                                         start_id = 0, verbose = TRUE) {
  
  # Initialize timing and status tracking
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE
  
  # Calculate actual simulation ID (matching original script logic)
  actual_sim_id <- start_id + sim_id
  
  # Initialize output variables
  output_result <- list(
    status = 1,  # Default to error
    daily_results = NULL,
    aggregated_results = NULL,
    runtime_info = NULL,
    sim_id = actual_sim_id,
    output_files = list()
  )
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 2 - Individual Simulation (Original Script Style)\n")
    cat("Node:", node, "\n")
    cat("Working Directory:", getwd(), "\n\n")
    cat("Configuration:\n")
    cat("\t Project Name                :", config$project$name, "\n")
    cat("\t GSA Method                  :", gsa_method, "\n")
    cat("\t Simulation ID (SampleID)    :", actual_sim_id, "\n")
    cat("\t DayCent Executable          :", daycent_exe, "\n")
    cat("\t Scratch Directory           :", scratch_dir, "\n")
    cat("\t Clean Scratch               :", clean_scratch, "\n\n")
    cat("====================================================================\n")
  }
  
  # Main simulation logic in tryCatch (matching original script structure)
  tryCatch({
    
    # Determine date stamp
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
    
    # Setup paths for the specified GSA method
    paths <- setup_gsa_paths(config, date_stamp, gsa_method)
    
    # Read Monte Carlo draws for this GSA method (matching original line 78-81)
    mc_draw_file <- file.path(paths$gsa_method_dir, paste0("mc_GSA_draw_", gsa_method, ".rds"))
    if (!file.exists(mc_draw_file)) {
      stop("Monte Carlo draw file not found: ", mc_draw_file)
    }
    
    all_jobs <- readRDS(mc_draw_file)
    
    # Get parameters for this specific simulation ID (matching original logic)
    job_params <- all_jobs[all_jobs$SampleID == actual_sim_id, ]
    
    if (nrow(job_params) == 0) {
      stop("No parameters found for simulation ID: ", actual_sim_id)
    }
    
    group_id <- job_params$JobGroup
    
    if (verbose) {
      cat("Job Group:", group_id, "\n")
      cat("Loaded parameters for simulation ID:", actual_sim_id, "\n")
    }
    
    # Initialize counters (matching original script variables lines 85-90)
    num_sites <- 0
    num_trts <- 0
    num_sim_years <- 0
    num_nh3_mes_years <- 0
    num_urea_mes_years <- 0
    dRslt <- NULL
    
    # Initialize aggregated output variables (matching original temp_ variables)
    temp_aglivc <- 0
    temp_aglivn <- 0
    temp_N2O <- 0
    temp_nit_N2O <- 0
    temp_dnit_N2O <- 0
    temp_dnit_N2 <- 0
    temp_NO <- 0
    temp_NH3 <- 0
    temp_netMin1 <- 0
    temp_netMin2 <- 0
    temp_spHf <- 0
    temp_urea <- 0
    temp_crnf <- 0
    
    # Create parameter data frame (matching original lines 112-120)
    params_df <- prepare_parameter_set(config, job_params, verbose = verbose)
    
    # Load run file and experimental sites
    run_file_path <- file.path(paths$gsa_method_dir, "RunFile.rds")
    if (!file.exists(run_file_path)) {
      stop("Run file not found: ", run_file_path)
    }
    
    run_file <- readRDS(run_file_path)
    exp_site_ids <- sort(unique(run_file$siteID))
    
    # Setup simulation directory (matching original line 128)
    sim_dir_tid <- file.path(scratch_dir, date_stamp, "GSA", paste0("tid", actual_sim_id))
    
    # Setup output directories for this job group
    daily_out_dir <- file.path(paths$daily_out_dir, paste0("jobGroup_", group_id))
    aggregated_dir <- file.path(paths$aggregated_dir, paste0("jobGroup_", group_id))
    run_status_dir <- file.path(paths$run_status_dir, paste0("jobGroup_", group_id))
    
    # Create output directories if they don't exist
    dir.create(daily_out_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(aggregated_dir, recursive = TRUE, showWarnings = FALSE)
    dir.create(run_status_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Main simulation loop over experimental sites (matching original loop structure)
    for (i in 1:length(exp_site_ids)) {
      num_sites <- num_sites + 1
      
      site_id <- trimws(exp_site_ids[i])
      run_file_site <- run_file[run_file$siteID == site_id, ]
      
      if (verbose) {
        cat("\n")
        cat("\n")
        cat("*************** Running SiteID:", site_id, "***************\n")
      }
      
      # Setup directories (matching original lines 161-176)
      site_dir_from <- file.path(config$paths$expsites_dir, site_id)
      site_sim_dir <- file.path(sim_dir_tid, site_id)
      
      if (verbose) {
        cat("site source path:", site_dir_from, "\n")
        cat("scratch run path:", site_sim_dir, "\n")
      }
      
      # Setup DayCent run files
      if (dir.exists(site_sim_dir)) {
        unlink(site_sim_dir, recursive = TRUE)
      }
      dir.create(site_sim_dir, recursive = TRUE)
      
      # Copy site files (matching original lines 180-189)
      copy_sitefiles_flag <- copy_sitefiles(site_folder_from = site_dir_from,
                                           site_folder_to = site_sim_dir)
      
      if (copy_sitefiles_flag != 0) {
        stop("---- site Files for ", site_id, " copy failed.")
      } else {
        if (verbose) cat("---- site Files for", site_id, "copied successfully.\n")
      }
      
      setwd(site_sim_dir)
      if (verbose) cat("\n")
      
      # Copy dot100 files (matching original lines 194-204)
      copy_dot100_flag <- copy_dot100_files(dot100_directory = config$paths$dot100_path,
                                           simulation_directory = site_sim_dir)
      
      if (copy_dot100_flag != 0) {
        stop("---- dot100 Files for ", site_id, " copy failed.")
      } else {
        if (verbose) cat("---- dot100 Files for", site_id, "copied successfully.\n")
      }
      
      # Update fix.100 (matching original lines 206-215)
      update_fix100_flag <- update_fix100_parameters(paramsdf = params_df)
      
      if (update_fix100_flag != 0) {
        stop("---- fix.100 Files for ", site_id, " update failed.")
      } else {
        if (verbose) cat("---- fix.100 Files for", site_id, "updated successfully.\n")
      }
      
      # Get treatment schedules (matching original lines 217-225)
      trt_schs <- sort(unique(run_file_site$treatment_schedule))
      if (verbose) {
        cat("\n")
        cat("========= Running Treatment(s):", length(trt_schs), "=========\n")
      }
      
      # Setup NH3 output files (matching original lines 223-225)
      file.copy(from = file.path(config$paths$dot100_path, "nh3_outfiles.in"),
                to = file.path(site_sim_dir, "outfiles.in"),
                overwrite = TRUE)
      
      # Treatment run loop (matching original lines 227-357)
      for (j in 1:length(trt_schs)) {
        num_trts <- num_trts + 1
        
        # Update extended base site.100 (matching original lines 231-243)
        bh_sch_file <- trimws(run_file_site$base_schedule[j])
        bh_site100 <- paste0(strip_dot_sch(sch_file_name = bh_sch_file), "_site.100")
        
        update_site100_flag <- update_site100_parameters(site100_file = bh_site100, 
                                                         paramsdf = params_df)
        
        if (update_site100_flag != 0) {
          stop("---- ext_site.100 Files for ", site_id, " update failed.")
        } else {
          if (verbose) cat("---- ext_site.100 Files for", site_id, "updated successfully.\n")
        }
        
        # Run DayCent (matching original lines 246-262)
        trt_sch_file <- trimws(trt_schs[j])
        
        DayCent_trt_flag <- run_DayCent(filepath_exe = daycent_exe,
                                       sch_file = trt_sch_file,
                                       ext_site100_2read = bh_site100,
                                       ext_site100_2write = NULL)
        
        if (verbose) cat("\n")
        if (DayCent_trt_flag != 0) {
          stop("------- DayCent simulation failed for ", site_id, "::", trt_sch_file, ".")
        } else {
          if (verbose) cat("------- Execution success for", site_id, "::", trt_sch_file, ".\n")
        }
        
        # Read DayCent outputs (matching original lines 265-356)
        
        # Process nflux.out (matching original lines 267-310)
        if (file.exists("nflux.out")) {
          nflux <- read.table("nflux.out", header = TRUE)
          nflux$year <- nflux$time %/% 1
          
          num_sim_years <- num_sim_years + length(unique(nflux$year))
          
          nflux$DayCent_N2O <- nflux$nit_N2O.N + nflux$dnit_N2O.N
          temp_N2O <- temp_N2O + sum(abs(nflux$DayCent_N2O)) / 1000000
          temp_nit_N2O <- temp_nit_N2O + sum(abs(nflux$nit_N2O.N)) / 1000000
          temp_dnit_N2O <- temp_dnit_N2O + sum(abs(nflux$dnit_N2O.N)) / 1000000
          temp_dnit_N2 <- temp_dnit_N2 + sum(abs(nflux$dnit_N2.N)) / 1000000
          temp_NO <- temp_NO + sum(abs(nflux$NO.N)) / 1000000
          temp_NH3 <- temp_NH3 + sum(abs(nflux$NH3.N)) / 1000000
          temp_netMin1 <- temp_netMin1 + sum(abs(nflux$netNmin1.gN.m2.)) / 1000000
          temp_netMin2 <- temp_netMin2 + sum(abs(nflux$netNmin2.gN.m2.)) / 1000000
          
          mod_years <- unique(nflux$year)
          
          # Read observation data for NH3 (this could be made more configurable)
          obs_file <- file.path(config$paths$observation_dir, "cumNH3_allmeasurements_28Feb2025.csv")
          if (file.exists(obs_file)) {
            cumNH3 <- read.csv(obs_file, stringsAsFactors = FALSE)
            obs_years <- sort(unique(c(cumNH3$meas_start_year[cumNH3$treatment_schedule == trt_sch_file],
                                      cumNH3$meas_end_year[cumNH3$treatment_schedule == trt_sch_file])))
            num_nh3_mes_years <- num_nh3_mes_years + length(obs_years)
            data_years <- intersect(mod_years, obs_years)
          } else {
            data_years <- mod_years  # Use all model years if no observation file
          }
          
          # Process NH3-N data (matching original lines 292-310)
          for (yr in data_years) {
            temp_outvar_yr <- nflux[nflux$year == yr, ]
            outvar_name <- "NH3.N"
            temp_outvar_yr_wide <- data.frame(
              SampleID = actual_sim_id,
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
            dRslt <- rbind(dRslt, temp_outvar_yr_wide)
          }
        }
        
        # Process ctrlfert.out (matching original lines 313-347)
        if (file.exists("ctrlfert.out")) {
          ctrlfert <- read.table("ctrlfert.out", header = TRUE)
          ctrlfert$year <- ctrlfert$time %/% 1
          
          temp_urea <- temp_urea + sum(abs(ctrlfert$urea_left)) / 1000000
          temp_spHf <- temp_spHf + sum(abs(ctrlfert$spHf)) / 1000000
          temp_crnf <- temp_crnf + sum(abs(ctrlfert$fct_left)) / 1000000
          
          mod2_years <- unique(ctrlfert$year)
          
          # Read urea observation data
          urea_obs_file <- file.path(config$paths$observation_dir, "Urea_measurements_28Feb2025.csv")
          if (file.exists(urea_obs_file)) {
            mes_urea <- read.csv(urea_obs_file, stringsAsFactors = FALSE)
            obs2_years <- sort(unique(mes_urea$year[mes_urea$treatment_schedule == trt_sch_file]))
            num_urea_mes_years <- num_urea_mes_years + length(obs2_years)
            data2_years <- intersect(mod2_years, obs2_years)
          } else {
            data2_years <- mod2_years  # Use all model years if no observation file
          }
          
          # Process urea_left data (matching original lines 328-347)
          for (yr in data2_years) {
            temp_outvar_yr <- ctrlfert[ctrlfert$year == yr, ]
            outvar_name <- "urea_left"
            temp_outvar_yr_wide <- data.frame(
              SampleID = actual_sim_id,
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
            dRslt <- rbind(dRslt, temp_outvar_yr_wide)
          }
        }
        
        # Process bio.out (matching original lines 350-355)
        if (file.exists("bio.out")) {
          bio <- read.table("bio.out", header = TRUE)
          bio$year <- bio$time %/% 1
          
          temp_aglivc <- temp_aglivc + sum(abs(bio$aglivc)) / 1000000
          temp_aglivn <- temp_aglivn + sum(abs(bio$aglivn)) / 1000000
        }
      }
      
      # Reset working directory and clean up site if requested
      setwd(config$paths$lairice_root)
      if (clean_scratch) {
        unlink(site_sim_dir, recursive = TRUE)
      }
    }
    
    # Create aggregated results (matching original lines 367-380)
    agg_Rslt <- data.frame(
      SampleID = actual_sim_id,
      aglivc = temp_aglivc,
      aglivn = temp_aglivn,
      N2O = temp_N2O,
      nit_N2O = temp_nit_N2O,
      dnit_N2O = temp_dnit_N2O,
      dnit_N2 = temp_dnit_N2,
      NOx = temp_NO,
      NH3 = temp_NH3,
      netMin1 = temp_netMin1,
      netMin2 = temp_netMin2,
      spHf = temp_spHf,
      urea = temp_urea,
      crnf = temp_crnf,
      stringsAsFactors = FALSE
    )
    
    # Save results (matching original lines 382-383)
    daily_output_file <- file.path(daily_out_dir, paste0("dc_dRslt_", actual_sim_id, ".rds"))
    agg_output_file <- file.path(aggregated_dir, paste0("dc_aggRslt_", actual_sim_id, ".rds"))
    
    saveRDS(dRslt, daily_output_file)
    saveRDS(agg_Rslt, agg_output_file)
    
    # Calculate runtime (matching original lines 385-391)
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create runtime info (matching original structure)
    runtime_info <- data.frame(
      SampleID = actual_sim_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = num_trts,
      sim_years = num_sim_years,
      nh3_years = num_nh3_mes_years,
      urea_years = num_urea_mes_years,
      status = 0,
      message = "Execution Success..",
      stringsAsFactors = FALSE
    )
    
    # Save runtime info (matching original line 393)
    runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_sim_id, ".csv"))
    write.csv(runtime_info, runtime_file, row.names = FALSE)
    
    # Reset working directory
    setwd(config$paths$lairice_root)
    
    # Clean up main simulation directory if requested
    if (clean_scratch && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
    
    complete <- TRUE
    
    # Update output result
    output_result$status <- 0
    output_result$daily_results <- dRslt
    output_result$aggregated_results <- agg_Rslt
    output_result$runtime_info <- runtime_info
    output_result$output_files <- list(
      daily_output = daily_output_file,
      aggregated_output = agg_output_file,
      runtime_info = runtime_file
    )
    
    if (verbose) cat("--- Individual GSA Step 2 Simulation Successfully Completed ---\n")
    
  }, error = function(err) {
    
    if (verbose) {
      cat("--- Individual GSA Step 2 Simulation Failed ---\n")
      cat("Error:", as.character(err), "\n")
    }
    
    # Calculate error runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create error runtime info (matching original error handling lines 404-414)
    runtime_info <- data.frame(
      SampleID = actual_sim_id,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      num_trts = ifelse(exists("num_trts"), num_trts, 0),
      sim_years = ifelse(exists("num_sim_years"), num_sim_years, 0),
      nh3_years = ifelse(exists("num_nh3_mes_years"), num_nh3_mes_years, 0),
      urea_years = ifelse(exists("num_urea_mes_years"), num_urea_mes_years, 0),
      status = 1,
      message = as.character(err),
      stringsAsFactors = FALSE
    )
    
    # Try to save error info if directory exists
    if (exists("run_status_dir") && dir.exists(run_status_dir)) {
      runtime_file <- file.path(run_status_dir, paste0("RunTime_GSA_Sim_", actual_sim_id, "_error.csv"))
      write.csv(runtime_info, runtime_file, row.names = FALSE)
      output_result$output_files$runtime_info <- runtime_file
    }
    
    output_result$runtime_info <- runtime_info
    
    # Reset working directory on error
    if (exists("config") && "paths" %in% names(config) && "lairice_root" %in% names(config$paths)) {
      setwd(config$paths$lairice_root)
    }
    
    # Clean up on error if requested
    if (exists("sim_dir_tid") && clean_scratch && dir.exists(sim_dir_tid)) {
      unlink(sim_dir_tid, recursive = TRUE)
    }
  })
  
  return(output_result)
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