#' SIR Step 1: Monte Carlo Draw Setup
#'
#' Sets up Sampling Importance Resampling (SIR) by generating Monte Carlo parameter draws
#' and organizing directory structure for parallel processing.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param verbose Logical, whether to print progress messages
#'
#' @return List containing:
#'   \item{sir_method_dir}{Path to SIR directory}
#'   \item{mc_draws}{Data frame with Monte Carlo draws}
#'   \item{n_samples}{Number of samples generated}
#'   \item{n_job_groups}{Number of job groups created}
#'   \item{runtime_info}{Runtime and status information}
#'
#' @importFrom lhs randomLHS
#' @importFrom stats qunif
#' @export
sir_step1_setup <- function(config, date_stamp = NULL, verbose = TRUE) {
  
  # Initialize timing and status
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE
  
  tryCatch({
    
    # Validate inputs
    if (missing(config)) stop("Configuration object is required")
    
    # Setup R library paths if specified
    if (!is.null(config$r_config$rlibpaths)) {
      .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
    }
    
    # Determine date stamp
    if (is.null(date_stamp)) {
      if (config$project$date_stamp == "auto") {
        date_stamp <- format(Sys.Date(), "%d%b%Y")
      } else {
        date_stamp <- config$project$date_stamp
      }
    }
    
    if (verbose) {
      cat("====================================================================\n")
      cat("SIR Step 1 Setup - Monte Carlo Draw Generation\n")
      cat("Working Directory:", getwd(), "\n\n")
      cat("Configuration:\n")
      cat("\t Root Directory                          :", config$paths$lairice_root, "\n")
      cat("\t Date Stamp                              :", date_stamp, "\n")
      cat("\t Number of MC simulations                :", config$sir$nsim, "\n")
      cat("\t Random Seed                             :", config$sir$rseed, "\n")
      cat("\t Number of MC Simulations per directory :", config$sir$n2dir, "\n\n")
      cat("====================================================================\n")
    }
    
    # Setup directory paths
    paths <- setup_sir_paths(config, date_stamp)
    sir_method_dir <- paths$sir_method_dir
    
    # Create necessary directories
    create_sir_directories(paths, verbose = verbose)
    
    # Generate run file with database support
    if (verbose) cat("Generating DayCent run file...\n")
    expsites_dir <- config$paths$expsites_dir
    run_file <- create_daycent_runfile_enhanced(config, ExpSite_path = expsites_dir, log_function = if(verbose) cat else function(...) NULL)
    run_file_name <- file.path(sir_method_dir, "RunFile.rds")
    if(config$sir$step1$model_type == "calibration") {
      run_file <- run_file %>%
       filter(group == "calibration")
      saveRDS(run_file, run_file_name)
    } else {
      saveRDS(run_file, run_file_name)
    }
    
    # Read parameter information and distribution assumptions
    if (verbose) cat("Reading parameter priors...\n")
    prior_file <- config$input_files$prior_file
    if (!file.exists(prior_file)) {
      stop("Prior file not found: ", prior_file)
    }
    prior <- read.csv(prior_file, stringsAsFactors = FALSE)
    
    # Filter for SIR parameters
    sir_parms <- config$sir$sir_parameters
    prior <- prior[prior$ParameterName %in% sir_parms, ]
    
    if (nrow(prior) == 0) {
      stop("No SIR parameters found in prior file. Check sir_parameters configuration.")
    }
    
    # Generate Monte Carlo draws
    if (verbose) cat("Generating Monte Carlo parameter draws...\n")
    mc_draws <- generate_sir_draws(prior, config$sir$nsim, config$sir$rseed, config = config, verbose = verbose)
    
    # Assign job groups
    if (verbose) cat("Organizing into job groups...\n")
    mc_draws <- assign_sir_job_groups(mc_draws, config$sir$n2dir)
    
    # Save MC draws
    if (verbose) cat("Saving MC draws...\n")
    saveRDS(mc_draws, file.path(sir_method_dir, "mc_SIR_draw.rds"))
    
    # Create job group subdirectories
    if(config$file_source$mode == "filesystem") {
      create_sir_job_group_directories(paths, unique(mc_draws$JobGroup), verbose = verbose)
    }

    # Generate site-crop-year matrix if crop calibration is enabled
    target_crop <- get_pipeline_target_crop(config, "sir")
    if (crop_calibration_enabled(config, target_crop)) {
      if (verbose) cat("Building site-crop-year matrix for crop:", paste(target_crop, collapse = ", "), "...\n")

      # Use shared connection for efficiency during matrix building
      shared_connection <- NULL
      tryCatch({
        shared_connection <- get_shared_connection(config, log_function = if(verbose) cat else function(...) NULL)

        site_crop_matrix <- build_site_crop_year_matrix(
          config = config,
          runfile = run_file,
          target_crop = target_crop,
          shared_connection = shared_connection,
          verbose = verbose
        )
        if (is.null(site_crop_matrix) || nrow(site_crop_matrix) == 0) {
          stop("Required site-crop-year matrix is empty")
        }

        # Save matrix for use in Step 2
        matrix_file <- file.path(sir_method_dir, "site_crop_year_matrix.rds")
        saveRDS(site_crop_matrix, matrix_file)

        if (verbose) {
          cat("Site-crop-year matrix saved to:", basename(matrix_file), "\n")
          cat("Matrix contains", nrow(site_crop_matrix), "site-year entries\n")
        }

      }, error = function(e) {
        stop("Failed to build required site-crop-year matrix: ", conditionMessage(e))
      }, finally = {
        # Clean up shared connection if we created one
        if (!is.null(shared_connection)) {
          tryCatch({
            DBI::dbDisconnect(shared_connection)
          }, error = function(e) {
            # Ignore connection cleanup errors
          })
        }
      })
    }

    # Generate point assignments if scaling mode is enabled
    if (is_scaling_mode_enabled(config)) {
      points_per_job <- get_points_per_job_config(config)

      if (!is.null(points_per_job)) {
        if (verbose) cat("Generating point assignments for scaling mode (", points_per_job, " points/job)...\n")

        # Get unique site IDs from run file
        site_ids <- unique(run_file$siteID)

        # Assign points to jobs
        point_assignments <- assign_points_to_jobs(
          site_ids = site_ids,
          points_per_job = points_per_job,
          verbose = verbose
        )

        # Save assignments
        assignments_file <- file.path(sir_method_dir, "point_assignments.rds")
        saveRDS(point_assignments, assignments_file)

        if (verbose) {
          cat("Point assignments saved to:", basename(assignments_file), "\n")
          cat("Total sites:", length(site_ids), "\n")
          cat("Jobs created:", max(point_assignments$job_id), "\n\n")
        }
      } else {
        if (verbose) cat("Warning: Scaling mode enabled but points_per_job not configured\n")
      }
    }

    # Calculate runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create runtime info
    runtime_info <- data.frame(
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      MC_nsim = nrow(mc_draws),
      n_jobGroup = length(unique(mc_draws$JobGroup)),
      status = 0,
      message = "Setup Complete..",
      stringsAsFactors = FALSE
    )
    
    # Save runtime info
    write.csv(runtime_info, 
              file.path(sir_method_dir, "RunTime_SIR_setup.csv"), 
              row.names = FALSE)
    
    if (verbose) cat("--- SIR Step 1 Setup Successfully Completed ---\n")
    complete <- TRUE
    
    # Return results
    return(list(
      sir_method_dir = sir_method_dir,
      mc_draws = mc_draws,
      n_samples = nrow(mc_draws),
      n_job_groups = length(unique(mc_draws$JobGroup)),
      runtime_info = runtime_info,
      paths = paths
    ))
    
  }, error = function(err) {
    
    # Calculate error runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create error runtime info
    runtime_info <- data.frame(
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      MC_nsim = NA,
      n_jobGroup = NA,
      status = 1,
      message = as.character(err),
      stringsAsFactors = FALSE
    )
    
    # Try to save error info if directory exists
    if (exists("sir_method_dir") && dir.exists(sir_method_dir)) {
      write.csv(runtime_info,
                file.path(sir_method_dir, "RunTime_SIR_setup_error.csv"),
                row.names = FALSE)
    }
    
    if (verbose) {
      cat("--- SIR Step 1 Setup Failed ---\n")
      cat("Error:", as.character(err), "\n")
    }
    
    stop("SIR Step 1 Setup Failed: ", err)
  })
}

#' Setup SIR Directory Paths
#'
#' @param config Configuration list
#' @param date_stamp Date stamp for the run
#' @return List of directory paths
setup_sir_paths <- function(config, date_stamp) {
  
  base_dir <- file.path(config$paths$lairice_root, "results", config$project$name, date_stamp, "SIR")
  
  if(config$emmission_variable == "crop") {
      list(
        sir_method_dir = base_dir,
        daily_out_dir = file.path(base_dir, config$output_dirs$daily_outputs),
        likelihood_dir = file.path(base_dir, config$output_dirs$likelihood_outputs),
        aggregated_dir = file.path(base_dir, config$output_dirs$aggregated_outputs),
        weighted_mean_dir = file.path(base_dir, config$output_dirs$weighted_mean_outputs),
        annual_dir = file.path(base_dir, config$output_dirs$annual_outputs),
        run_status_dir = file.path(base_dir, config$output_dirs$run_status),
        results_dir = file.path(base_dir, config$output_dirs$results),
        figures_dir = file.path(base_dir, config$output_dirs$figures)
    )
  } else {
    list(
        sir_method_dir = base_dir,
        daily_out_dir = file.path(base_dir, config$output_dirs$daily_outputs),
        likelihood_dir = file.path(base_dir, config$output_dirs$likelihood_outputs),
        aggregated_dir = file.path(base_dir, config$output_dirs$aggregated_outputs),
        annual_dir = file.path(base_dir, config$output_dirs$annual_outputs),
        run_status_dir = file.path(base_dir, config$output_dirs$run_status),
        results_dir = file.path(base_dir, config$output_dirs$results),
        figures_dir = file.path(base_dir, config$output_dirs$figures)
    )
  }
  
}

#' Create SIR Directories
#'
#' @param paths List of directory paths from setup_sir_paths
#' @param verbose Logical, whether to print created directories
create_sir_directories <- function(paths, verbose = TRUE) {
  
  if (verbose) cat("Creating directories:\n")
  
  for (path_name in names(paths)) {
    path <- paths[[path_name]]
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
      if (verbose) cat("\t -", path, "\n")
    }
  }
  
  if (verbose) cat("\n")
}

#' Generate Monte Carlo Draws for SIR
#'
#' @param prior Data frame with parameter prior information
#' @param nsim Number of simulations
#' @param rseed Random seed
#' @param config Configuration list (for PPDF handling)
#' @param verbose Logical, whether to print progress
#' @return Data frame with Monte Carlo draws
generate_sir_draws <- function(prior, nsim, rseed, config = NULL, verbose = TRUE) {

  # Check for PPDF parameters and handle them specially
  ppdf_detection <- detect_ppdf_parameters(prior)

  if (ppdf_detection$has_ppdf && !is.null(config) && "ppdf" %in% names(config)) {
    # Use PPDF-specific generation with biological filtering
    if (verbose) cat("  Detected PPDF parameters, using biological filtering approach...\n")

    # Separate PPDF and non-PPDF parameters
    ppdf_priors <- prior[ppdf_detection$ppdf_indices, , drop = FALSE]
    other_priors <- prior[ppdf_detection$non_ppdf_indices, , drop = FALSE]

    # Generate PPDF parameters with biological filtering
    ppdf_result_raw <- create_ppdf_pset(ppdf_priors, nsim, method = "SIR", config = config)
    ppdf_result <- ppdf_result_raw$pset_1  # Extract the actual parameter data frame

    # Generate other parameters using standard LHS
    if (nrow(other_priors) > 0) {
      set.seed(rseed + 1)  # Different seed for other parameters
      other_lhs <- lhs::randomLHS(n = nsim, k = nrow(other_priors))
      other_params <- matrix(0, nrow = nsim, ncol = nrow(other_priors))

      for (i in 1:nrow(other_priors)) {
        lower <- other_priors[i, "Lower"]
        upper <- other_priors[i, "Upper"]
        other_params[, i] <- round(qunif(other_lhs[, i], min = lower, max = upper), 6)
      }

      # Create data frame for other parameters
      other_df <- data.frame(other_params)
      names(other_df) <- other_priors$ParameterName

      # Combine PPDF and other parameters
      X <- cbind(other_df, ppdf_result)
    } else {
      # Only PPDF parameters
      X <- ppdf_result
    }

    # Reorder columns to match original prior order
    X <- X[, prior$ParameterName, drop = FALSE]

  } else {
    # Standard LHS sampling for non-PPDF parameters
    nParams <- nrow(prior)
    set.seed(rseed)

    # Generate Latin Hypercube samples
    m1 <- lhs::randomLHS(n = nsim, k = nParams)

    # Transform to parameter ranges
    M1 <- matrix(0, nrow = nsim, ncol = nParams)

    for (i in 1:nParams) {
      lower <- prior[i, "Lower"]
      upper <- prior[i, "Upper"]
      M1[, i] <- round(qunif(m1[, i], min = lower, max = upper), 6)
    }

    # Create data frame
    X <- data.frame(M1)
    names(X) <- prior$ParameterName
  }

  return(X)
}

#' Assign Job Groups to SIR Monte Carlo Draws
#'
#' @param mc_draws Data frame with Monte Carlo draws
#' @param n2dir Number of simulations per job group
#' @return Updated mc_draws with SampleID and JobGroup columns
assign_sir_job_groups <- function(mc_draws, n2dir) {
  
  SampleIDs <- 1:nrow(mc_draws)
  JobGroups <- 1 + SampleIDs %/% n2dir
  
  mc_draws <- cbind(SampleID = SampleIDs, JobGroup = JobGroups, mc_draws)
  
  return(mc_draws)
}

#' Create SIR Job Group Subdirectories
#'
#' @param paths List of directory paths
#' @param job_groups Vector of unique job group IDs
#' @param verbose Logical, whether to print progress
create_sir_job_group_directories <- function(paths, job_groups, verbose = TRUE) {
  
  # Create subdirectories for each job group
  for (grp in sort(job_groups)) {
    grp_suffix <- paste0("jobGroup_", grp)
    
    # Daily outputs
    do_dir <- file.path(paths$daily_out_dir, grp_suffix)
    if (dir.exists(do_dir)) unlink(do_dir, recursive = TRUE)
    dir.create(do_dir, recursive = TRUE)
    
    # Likelihood outputs  
    lh_dir <- file.path(paths$likelihood_dir, grp_suffix)
    if (dir.exists(lh_dir)) unlink(lh_dir, recursive = TRUE)
    dir.create(lh_dir, recursive = TRUE)
    
    # Aggregated outputs
    ag_dir <- file.path(paths$aggregated_dir, grp_suffix)
    if (dir.exists(ag_dir)) unlink(ag_dir, recursive = TRUE)
    dir.create(ag_dir, recursive = TRUE)
    
    # Annual outputs
    an_dir <- file.path(paths$annual_dir, grp_suffix)
    if (dir.exists(an_dir)) unlink(an_dir, recursive = TRUE)
    dir.create(an_dir, recursive = TRUE)
    
    # Run status
    rs_dir <- file.path(paths$run_status_dir, grp_suffix)
    if (dir.exists(rs_dir)) unlink(rs_dir, recursive = TRUE)
    dir.create(rs_dir, recursive = TRUE)

    # Weighted mean outputs
    if(config$emmission_variable == "crop") {
    wm_dir <- file.path(paths$weighted_mean_dir, grp_suffix)
      if (dir.exists(wm_dir)) unlink(wm_dir, recursive = TRUE)
      dir.create(wm_dir, recursive = TRUE)
    }
  }
  
  if (verbose) cat("Created subdirectories for", length(job_groups), "job groups\n")
}
