#' GSA Step 1: Monte Carlo Draw Setup
#'
#' Sets up Global Sensitivity Analysis by generating Monte Carlo parameter draws,
#' creating GSA objects, and organizing directory structure for parallel processing.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param task_id Task ID for GSA method selection (1-8), maps to gsa_methods in config
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param verbose Logical, whether to print progress messages
#'
#' @return List containing:
#'   \item{smethod}{Selected GSA method}
#'   \item{gsa_method_dir}{Path to GSA method directory}
#'   \item{mc_draws}{Data frame with Monte Carlo draws}
#'   \item{n_samples}{Number of samples generated}
#'   \item{n_job_groups}{Number of job groups created}
#'   \item{runtime_info}{Runtime and status information}
#'
#' @importFrom yaml read_yaml
#' @importFrom sensitivity sobol sobol2002 sobolSalt sobol2007 soboljansen sobolmartinez sobolEff
#' @importFrom lhs randomLHS
#' @importFrom stats qunif
#' @export
gsa_step1_setup <- function(config, task_id, date_stamp = NULL, verbose = TRUE) {
  
  # Initialize timing and status
  start_time <- Sys.time()
  node <- Sys.info()[4]
  complete <- FALSE
  
  tryCatch({
    
    # Validate inputs
    if (missing(config)) stop("Configuration object is required")
    if (missing(task_id)) stop("task_id is required")
    if (task_id < 1 || task_id > length(config$gsa$gsa_methods)) {
      stop("task_id must be between 1 and ", length(config$gsa$gsa_methods))
    }
    
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
    
    # Select GSA method
    smethod <- config$gsa$gsa_methods[task_id]
    
    if (verbose) {
      cat("====================================================================\n")
      cat("GSA Step 1 Setup - Monte Carlo Draw Generation\n")
      cat("Working Directory:", getwd(), "\n\n")
      cat("Configuration:\n")
      cat("\t Root Directory                          :", config$paths$lairice_root, "\n")
      cat("\t Date Stamp                              :", date_stamp, "\n")
      cat("\t GSA Method                              :", smethod, "\n")
      cat("\t Number of MC simulations                :", config$gsa$nsim, "\n")
      cat("\t Number of Bootstrap                     :", config$gsa$nboot, "\n")
      cat("\t Random Seed                             :", config$gsa$rseed, "\n")
      cat("\t Number of MC Simulations per directory :", config$gsa$n2dir, "\n\n")
      cat("====================================================================\n")
    }
    
    # Setup directory paths
    paths <- setup_gsa_paths(config, date_stamp, smethod)
    gsa_method_dir <- paths$gsa_method_dir
    
    # Create necessary directories
    create_gsa_directories(paths, verbose = verbose)
    
    # Generate run file with database support
    if (verbose) cat("Generating DayCent run file...\n")
    expsites_dir <- config$paths$expsites_dir
    run_file <- create_daycent_runfile_enhanced(config, ExpSite_path = expsites_dir, log_function = if(verbose) cat else function(...) NULL)
    run_file_name <- file.path(gsa_method_dir, "RunFile.rds")
    saveRDS(run_file, run_file_name)
    
    # Read parameter information and distribution assumptions
    if (verbose) cat("Reading parameter priors...\n")
    prior_file <- config$input_files$prior_file
    if (!file.exists(prior_file)) {
      stop("Prior file not found: ", prior_file)
    }
    prior <- read.csv(prior_file, stringsAsFactors = FALSE)
    
    # Generate Monte Carlo draws
    if (verbose) cat("Generating Monte Carlo parameter draws...\n")
    mc_draws <- generate_mc_draws(prior, config$gsa$nsim, config$gsa$nboot,
                                  config$gsa$rseed, smethod, verbose = verbose, config = config)
    
    # Enforce MNDDHRV < MXDDHRV constraint if both parameters are present
    if (!is.null(mc_draws$X) &&
        all(c("MNDDHRV", "MXDDHRV") %in% colnames(mc_draws$X))) {
      
      idx <- which(!is.na(mc_draws$X$MNDDHRV) &
                     !is.na(mc_draws$X$MXDDHRV) &
                     mc_draws$X$MXDDHRV <= mc_draws$X$MNDDHRV)
      
      if (length(idx) > 0L) {
        mnd <- mc_draws$X$MNDDHRV[idx]
        mxd <- mc_draws$X$MXDDHRV[idx]
        
        # Ensure MNDDHRV < MXDDHRV strictly by construction
        new_mn <- pmin(mnd, mxd)
        new_mx <- pmax(mnd, mxd)
        
        # If still equal after swap, nudge MXDDHRV slightly above MNDDHRV
        equal_idx <- which(new_mx <= new_mn)
        if (length(equal_idx) > 0L) {
          new_mx[equal_idx] <- new_mn[equal_idx] + 1e-6
        }
        
        mc_draws$X$MNDDHRV[idx] <- new_mn
        mc_draws$X$MXDDHRV[idx] <- new_mx
      }
    }
    
    # Assign job groups
    if (verbose) cat("Organizing into job groups...\n")
    mc_draws <- assign_job_groups(mc_draws, config$gsa$n2dir)
    
    # Save GSA objects and MC draws
    if (verbose) cat("Saving GSA objects and MC draws...\n")
    saveRDS(mc_draws$gsa_obj, file.path(gsa_method_dir, paste0("GSA_obj_", smethod, ".rds")))
    saveRDS(mc_draws$X, file.path(gsa_method_dir, paste0("mc_GSA_draw_", smethod, ".rds")))
    
    # Create job group subdirectories
    if(config$file_source$mode == "filesystem") {
      create_job_group_directories(paths, unique(mc_draws$X$JobGroup), verbose = verbose)
    }

    # Generate site-crop-year matrix if crop calibration is enabled
    target_crop <- get_pipeline_target_crop(config, "gsa")
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
        matrix_file <- file.path(gsa_method_dir, "site_crop_year_matrix.rds")
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
        assignments_file <- file.path(gsa_method_dir, "point_assignments.rds")
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
      smethod = smethod,
      node = node,
      Start = start_time,
      End = end_time,
      Time_sec = time_stamp,
      MC_nsim = nrow(mc_draws$X),
      n_jobGroup = length(unique(mc_draws$X$JobGroup)),
      status = 0,
      message = "Setup Complete..",
      stringsAsFactors = FALSE
    )
    
    # Save runtime info
    write.csv(runtime_info, 
              file.path(gsa_method_dir, paste0("RunTime_GSA_setup_", smethod, ".csv")), 
              row.names = FALSE)
    
    if (verbose) cat("--- GSA Step 1 Setup Successfully Completed ---\n")
    complete <- TRUE
    
    # Return results
    return(list(
      smethod = smethod,
      gsa_method_dir = gsa_method_dir,
      mc_draws = mc_draws$X,
      gsa_obj = mc_draws$gsa_obj,
      n_samples = nrow(mc_draws$X),
      n_job_groups = length(unique(mc_draws$X$JobGroup)),
      runtime_info = runtime_info,
      paths = paths
    ))
    
  }, error = function(err) {
    
    # Calculate error runtime
    end_time <- Sys.time()
    time_stamp <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # Create error runtime info
    runtime_info <- data.frame(
      smethod = if(exists("smethod")) smethod else NA,
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
    if (exists("gsa_method_dir") && dir.exists(gsa_method_dir)) {
      write.csv(runtime_info,
                file.path(gsa_method_dir, paste0("RunTime_GSA_setup_", 
                         if(exists("smethod")) smethod else "unknown", "_error.csv")),
                row.names = FALSE)
    }
    
    if (verbose) {
      cat("--- GSA Step 1 Setup Failed ---\n")
      cat("Error:", as.character(err), "\n")
    }
    
    stop("GSA Step 1 Setup Failed: ", err)
  })
}

#' Setup GSA Directory Paths
#'
#' @param config Configuration list
#' @param date_stamp Date stamp for the run
#' @param smethod GSA method name
#' @return List of directory paths
setup_gsa_paths <- function(config, date_stamp, smethod) {
  
  base_dir <- file.path(config$paths$lairice_root, "results", config$project$name, date_stamp, "GSA", smethod)
  
  list(
    gsa_method_dir = base_dir,
    daily_out_dir = file.path(base_dir, config$output_dirs$daily_outputs),
    likelihood_dir = file.path(base_dir, config$output_dirs$likelihood_outputs),
    aggregated_dir = file.path(base_dir, config$output_dirs$aggregated_outputs),
    weighted_mean_dir = file.path(base_dir, config$output_dirs$weighted_mean_outputs),
    annual_dir = file.path(base_dir, config$output_dirs$annual_outputs),
    run_status_dir = file.path(base_dir, config$output_dirs$run_status),
    results_dir = file.path(base_dir, config$output_dirs$results),
    figures_dir = file.path(base_dir, config$output_dirs$figures)
  )
}

#' Create GSA Directories
#'
#' @param paths List of directory paths from setup_gsa_paths
#' @param verbose Logical, whether to print created directories
create_gsa_directories <- function(paths, verbose = TRUE) {
  
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

#' Generate Monte Carlo Draws for GSA
#'
#' Creates Monte Carlo parameter samples using Latin Hypercube Sampling and
#' generates GSA object for sensitivity analysis. Automatically detects PPDF
#' parameters and applies biological filtering when present.
#'
#' @param prior Data frame with parameter prior information
#' @param nsim Number of simulations
#' @param nboot Number of bootstrap samples
#' @param rseed Random seed
#' @param smethod GSA method name
#' @param verbose Logical, whether to print progress
#' @param config Configuration list (optional, required for PPDF handling)
#' @return List with GSA object and parameter matrix X
generate_mc_draws <- function(prior, nsim, nboot, rseed, smethod, verbose = TRUE, config = NULL) {

  nParams <- nrow(prior)

  # Check for PPDF parameters and handle them specially
  ppdf_detection <- detect_ppdf_parameters(prior)

  if (ppdf_detection$has_ppdf && !is.null(config) && "ppdf" %in% names(config)) {
    if (verbose) cat("  Detected PPDF parameters, using biological filtering approach...\n")

    # Ensure we have complete PPDF set
    if (!ppdf_detection$has_complete_ppdf) {
      stop("Incomplete PPDF parameter set detected. Missing: ",
           paste(ppdf_detection$missing_ppdf, collapse=", "))
    }

    # Generate PPDF parameter sets with biological filtering
    ppdf_priors <- prior[ppdf_detection$ppdf_indices, ]
    ppdf_result <- create_ppdf_pset(ppdf_priors, n_pset = nsim, method = "GSA", config = config)

    # Generate regular parameters for non-PPDF parameters
    if (length(ppdf_detection$non_ppdf_indices) > 0) {
      other_priors <- prior[ppdf_detection$non_ppdf_indices, ]
      n_other_params <- nrow(other_priors)

      # Generate two sets of other parameters using LHS (for GSA consistency)
      set.seed(config$ppdf$gsa$seed_1)
      m1_other <- lhs::randomLHS(n = nsim, k = n_other_params)
      M1_other <- matrix(0, nrow = nsim, ncol = n_other_params)

      for (i in 1:n_other_params) {
        lower <- other_priors[i, "Lower"]
        upper <- other_priors[i, "Upper"]
        M1_other[, i] <- round(qunif(m1_other[, i], min = lower, max = upper), 6)
      }

      set.seed(config$ppdf$gsa$seed_2)
      m2_other <- lhs::randomLHS(n = nsim, k = n_other_params)
      M2_other <- matrix(0, nrow = nsim, ncol = n_other_params)

      for (i in 1:n_other_params) {
        lower <- other_priors[i, "Lower"]
        upper <- other_priors[i, "Upper"]
        M2_other[, i] <- round(qunif(m2_other[, i], min = lower, max = upper), 6)
      }

      # Create data frames for other parameters
      X1_other <- data.frame(M1_other)
      X2_other <- data.frame(M2_other)
      names(X1_other) <- other_priors$ParameterName
      names(X2_other) <- other_priors$ParameterName

      # Combine PPDF and other parameters
      X1 <- cbind(ppdf_result$pset_1, X1_other)
      X2 <- cbind(ppdf_result$pset_2, X2_other)
    } else {
      # Only PPDF parameters
      X1 <- ppdf_result$pset_1
      X2 <- ppdf_result$pset_2
    }

    # Reorder columns to match original prior order
    X1 <- X1[, prior$ParameterName]
    X2 <- X2[, prior$ParameterName]

    if (verbose) {
      cat("  PPDF filtering statistics:\n")
      cat("    Set 1 pass rate:", sprintf("%.1f%%", ppdf_result$filtering_stats$set_1$pass_rate * 100), "\n")
      cat("    Set 2 pass rate:", sprintf("%.1f%%", ppdf_result$filtering_stats$set_2$pass_rate * 100), "\n")
    }

  } else {
    # Original implementation for non-PPDF parameters
    if (verbose && ppdf_detection$has_ppdf) {
      cat("  PPDF parameters detected but no PPDF config provided, using standard LHS sampling...\n")
    }

    set.seed(rseed)

    # Generate Latin Hypercube samples
    m1 <- lhs::randomLHS(n = nsim, k = nParams)
    m2 <- lhs::randomLHS(n = nsim, k = nParams)

    # Transform to parameter ranges
    M1 <- matrix(0, nrow = nsim, ncol = nParams)
    M2 <- matrix(0, nrow = nsim, ncol = nParams)

    for (i in 1:nParams) {
      lower <- prior[i, "Lower"]
      upper <- prior[i, "Upper"]
      M1[, i] <- round(qunif(m1[, i], min = lower, max = upper), 6)
      M2[, i] <- round(qunif(m2[, i], min = lower, max = upper), 6)
    }

    # Create data frames
    X1 <- data.frame(M1)
    X2 <- data.frame(M2)
    names(X1) <- prior$ParameterName
    names(X2) <- prior$ParameterName
  }

  # Create GSA object based on method
  si_obj <- create_gsa_object(smethod, X1, X2, nboot)

  # Extract parameter matrix
  X <- as.data.frame(si_obj$X)
  names(X) <- prior$ParameterName

  return(list(gsa_obj = si_obj, X = X))
}

#' Create GSA Object Based on Method
#'
#' @param smethod GSA method name
#' @param X1 First parameter matrix
#' @param X2 Second parameter matrix  
#' @param nboot Number of bootstrap samples
#' @return GSA object from sensitivity package
create_gsa_object <- function(smethod, X1, X2, nboot) {
  
  switch(smethod,
    "sobol" = sensitivity::sobol(model = NULL, X1 = X1, X2 = X2, nboot = nboot),
    "sobol2002" = sensitivity::sobol2002(model = NULL, X1 = X1, X2 = X2, nboot = nboot),
    "sobolSaltA" = sensitivity::sobolSalt(model = NULL, X1 = X1, X2 = X2, schema = "A", nboot = nboot),
    "sobolSaltB" = sensitivity::sobolSalt(model = NULL, X1 = X1, X2 = X2, schema = "B", nboot = nboot),
    "sobol2007" = sensitivity::sobol2007(model = NULL, X1 = X1, X2 = X2, nboot = nboot),
    "soboljansen" = sensitivity::soboljansen(model = NULL, X1 = X1, X2 = X2, nboot = nboot),
    "sobolmartinez" = sensitivity::sobolmartinez(model = NULL, X1 = X1, X2 = X2, nboot = nboot),
    "sobolEff" = sensitivity::sobolEff(model = NULL, X1 = X1, X2 = X2, nboot = nboot),
    stop(smethod, " is not recognized.")
  )
}

#' Assign Job Groups to Monte Carlo Draws
#'
#' @param mc_draws List with GSA object and parameter matrix
#' @param n2dir Number of simulations per job group
#' @return Updated mc_draws with SampleID and JobGroup columns
assign_job_groups <- function(mc_draws, n2dir) {
  
  X <- mc_draws$X
  SampleIDs <- 1:nrow(X)
  JobGroups <- 1 + SampleIDs %/% n2dir
  
  X <- cbind(SampleID = SampleIDs, JobGroup = JobGroups, X)
  mc_draws$X <- X
  
  return(mc_draws)
}

#' Create Job Group Subdirectories
#'
#' @param paths List of directory paths
#' @param job_groups Vector of unique job group IDs
#' @param verbose Logical, whether to print progress
create_job_group_directories <- function(paths, job_groups, verbose = TRUE) {
  
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
    wm_dir <- file.path(paths$weighted_mean_dir, grp_suffix)
    if (dir.exists(wm_dir)) unlink(wm_dir, recursive = TRUE)
    dir.create(wm_dir, recursive = TRUE)
  }
  
  if (verbose) cat("Created subdirectories for", length(job_groups), "job groups\n")
}
