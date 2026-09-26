#' PPDF Parameter Tools for Temperature Response Curve Modeling
#'
#' This module provides functions for handling PPDF (Plant Production Driven by Temperature Factor)
#' parameters in DayCent calibration. PPDF parameters define temperature response curves
#' that must be biologically realistic, requiring special handling during parameter generation.
#'
#' @author Generated with Claude Code
#' @date 2025-09-16

#' Create temperature response curve for plant growth
#'
#' Calculates the temperature effect on plant growth using DayCent's PPDF equation.
#' This function is used to evaluate biological realism of PPDF parameter combinations.
#'
#' @param temperature Numeric vector of temperatures (degrees C)
#' @param ppdf1 Minimum temperature for plant growth (degrees C)
#' @param ppdf2 Optimum temperature for plant growth (degrees C)  
#' @param ppdf3 Maximum temperature effect factor for plant growth
#' @param ppdf4 Temperature coefficient for plant growth decline above optimum
#'
#' @return Numeric vector of growth factors (0-1 scale)
#'
#' @details The temperature response curve is calculated as:
#' frac = (ppdf2 - temperature) / (ppdf2 - ppdf1)
#' gpdf = exp((ppdf3/ppdf4) * (1 - frac^ppdf4)) * (frac^ppdf3)
#'
#' @examples
#' # Example temperature response curve
#' temps <- seq(0, 50, 1)
#' growth <- create_temp_curve(temps, ppdf1=15, ppdf2=35, ppdf3=1.0, ppdf4=4.0)
#' plot(temps, growth, type="l", xlab="Temperature (C)", ylab="Growth Factor")
#'
#' @export
create_temp_curve <- function(temperature, ppdf1, ppdf2, ppdf3, ppdf4) {
  # Validate inputs
  if (any(is.na(c(temperature, ppdf1, ppdf2, ppdf3, ppdf4)))) {
    stop("All parameters must be non-NA")
  }
  
  # For vectorized inputs, check if any violate constraints
  if (length(ppdf1) > 1 || length(ppdf2) > 1 || length(ppdf3) > 1 || length(ppdf4) > 1) {
    # Vectorized input - check for any invalid combinations
    if (any(ppdf2 <= ppdf1, na.rm = TRUE)) {
      warning("Some parameter combinations have ppdf2 <= ppdf1")
    }
    if (any(ppdf4 <= 0, na.rm = TRUE)) {
      warning("Some parameter combinations have ppdf4 <= 0")
    }
  } else {
    # Single value input - strict checking
    if (ppdf2 <= ppdf1) {
      stop("ppdf2 (optimum temperature) must be greater than ppdf1 (minimum temperature)")
    }
    if (ppdf4 <= 0) {
      stop("ppdf4 must be positive")
    }
  }
  
  # Calculate temperature fraction
  frac <- (ppdf2 - temperature) / (ppdf2 - ppdf1)
  
  # Calculate growth factor using DayCent equation
  gpdf <- exp((ppdf3 / ppdf4) * (1 - frac^ppdf4)) * (frac^ppdf3)
  
  # Handle edge cases and ensure non-negative values
  gpdf[is.na(gpdf)] <- 0
  gpdf[gpdf < 0] <- 0
  gpdf[is.infinite(gpdf)] <- 0
  
  return(gpdf)
}

#' Detect PPDF parameters in prior data
#'
#' Identifies PPDF1, PPDF2, PPDF3, and PPDF4 parameters in a prior file.
#' These parameters require special handling due to biological constraints.
#'
#' @param prior_data Data frame containing parameter information with a ParameterName column
#'
#' @return List with:
#' \itemize{
#'   \item has_ppdf: Logical indicating if any PPDF parameters were found
#'   \item ppdf_params: Character vector of detected PPDF parameter names
#'   \item ppdf_indices: Integer vector of row indices for PPDF parameters
#'   \item non_ppdf_indices: Integer vector of row indices for non-PPDF parameters
#' }
#'
#' @examples
#' # Create example prior data
#' prior_data <- data.frame(
#'   ParameterName = c("RUETB", "PPDF1", "PPDF2", "PPDF3", "PPDF4", "HIMAX"),
#'   Default = c(3.0, 25.0, 40.0, 1.0, 4.0, 0.50)
#' )
#' detection <- detect_ppdf_parameters(prior_data)
#' print(detection$ppdf_params)  # Should show PPDF1, PPDF2, PPDF3, PPDF4
#'
#' @export
detect_ppdf_parameters <- function(prior_data) {
  # Validate input
  if (!is.data.frame(prior_data)) {
    stop("prior_data must be a data frame")
  }
  
  if (!"ParameterName" %in% names(prior_data)) {
    stop("prior_data must contain a 'ParameterName' column")
  }
  
  # Define PPDF parameter patterns
  ppdf_patterns <- c("PPDF1", "PPDF2", "PPDF3", "PPDF4")
  
  # Find PPDF parameters
  ppdf_indices <- which(prior_data$ParameterName %in% ppdf_patterns)
  ppdf_params <- prior_data$ParameterName[ppdf_indices]
  
  # Find non-PPDF parameters
  non_ppdf_indices <- which(!prior_data$ParameterName %in% ppdf_patterns)
  
  # Check for complete PPDF set
  has_complete_ppdf <- all(ppdf_patterns %in% ppdf_params)
  
  result <- list(
    has_ppdf = length(ppdf_params) > 0,
    has_complete_ppdf = has_complete_ppdf,
    ppdf_params = ppdf_params,
    ppdf_indices = ppdf_indices,
    non_ppdf_indices = non_ppdf_indices,
    missing_ppdf = ppdf_patterns[!ppdf_patterns %in% ppdf_params]
  )
  
  # Warning for incomplete PPDF set
  if (result$has_ppdf && !result$has_complete_ppdf) {
    warning(paste("Incomplete PPDF parameter set detected. Missing:",
                  paste(result$missing_ppdf, collapse=", ")))
  }
  
  return(result)
}

#' Validate PPDF configuration
#'
#' Validates the PPDF section of a configuration file to ensure all required
#' parameters are present and have valid values.
#'
#' @param config List containing the full configuration
#'
#' @return List with validation results:
#' \itemize{
#'   \item valid: Logical indicating if configuration is valid
#'   \item errors: Character vector of validation errors
#'   \item warnings: Character vector of validation warnings
#' }
#'
#' @examples
#' # Example configuration
#' config <- list(
#'   ppdf = list(
#'     filtering = list(
#'       temperature_range = c(0, 50),
#'       temperature_step = 0.5,
#'       growth_at_0c = 0.00001,
#'       growth_at_10c = 0.001
#'     ),
#'     gsa = list(
#'       n_sample_multiplier = 10,
#'       seed_1 = 12345,
#'       seed_2 = 67890
#'     ),
#'     sir = list(
#'       n_sample_multiplier = 10,
#'       seed = 98765
#'     )
#'   )
#' )
#' validation <- validate_ppdf_config(config)
#'
#' @export
validate_ppdf_config <- function(config) {
  errors <- character(0)
  warnings <- character(0)
  
  # Check if ppdf section exists
  if (!"ppdf" %in% names(config)) {
    return(list(
      valid = TRUE,
      errors = character(0),
      warnings = "No PPDF configuration found - will use existing parameter generation"
    ))
  }
  
  ppdf_config <- config$ppdf
  
  # Required sections
  required_sections <- c("filtering", "gsa", "sir")
  missing_sections <- required_sections[!required_sections %in% names(ppdf_config)]
  if (length(missing_sections) > 0) {
    errors <- c(errors, paste("Missing PPDF sections:", paste(missing_sections, collapse=", ")))
  }
  
  # Validate filtering section
  if ("filtering" %in% names(ppdf_config)) {
    filtering <- ppdf_config$filtering
    required_filtering <- c("temperature_range", "temperature_step", "growth_at_0c", "growth_at_10c")
    missing_filtering <- required_filtering[!required_filtering %in% names(filtering)]
    if (length(missing_filtering) > 0) {
      errors <- c(errors, paste("Missing filtering parameters:", paste(missing_filtering, collapse=", ")))
    } else {
      # Validate filtering values
      if (length(filtering$temperature_range) != 2 || filtering$temperature_range[1] >= filtering$temperature_range[2]) {
        errors <- c(errors, "temperature_range must be a vector of length 2 with min < max")
      }
      if (filtering$temperature_step <= 0) {
        errors <- c(errors, "temperature_step must be positive")
      }
      if (filtering$growth_at_0c < 0 || filtering$growth_at_10c < 0) {
        errors <- c(errors, "growth thresholds must be non-negative")
      }
      if (filtering$growth_at_0c >= filtering$growth_at_10c) {
        warnings <- c(warnings, "growth_at_0c should typically be less than growth_at_10c")
      }
    }
  }
  
  # Validate GSA section
  if ("gsa" %in% names(ppdf_config)) {
    gsa <- ppdf_config$gsa
    required_gsa <- c("n_sample_multiplier", "seed_1", "seed_2")
    missing_gsa <- required_gsa[!required_gsa %in% names(gsa)]
    if (length(missing_gsa) > 0) {
      errors <- c(errors, paste("Missing GSA parameters:", paste(missing_gsa, collapse=", ")))
    } else {
      if (gsa$n_sample_multiplier <= 1) {
        errors <- c(errors, "n_sample_multiplier must be greater than 1")
      }
      if (!is.numeric(gsa$seed_1) || !is.numeric(gsa$seed_2)) {
        errors <- c(errors, "Seeds must be numeric")
      }
    }
  }
  
  # Validate SIR section
  if ("sir" %in% names(ppdf_config)) {
    sir <- ppdf_config$sir
    required_sir <- c("n_sample_multiplier", "seed")
    missing_sir <- required_sir[!required_sir %in% names(sir)]
    if (length(missing_sir) > 0) {
      errors <- c(errors, paste("Missing SIR parameters:", paste(missing_sir, collapse=", ")))
    } else {
      if (sir$n_sample_multiplier <= 1) {
        errors <- c(errors, "n_sample_multiplier must be greater than 1")
      }
      if (!is.numeric(sir$seed)) {
        errors <- c(errors, "Seed must be numeric")
      }
    }
  }
  
  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  ))
}

#' Apply biological filtering to PPDF parameter sets
#'
#' Filters PPDF parameter combinations to ensure biologically realistic
#' temperature response curves based on growth factors at specific temperatures.
#'
#' @param param_sets Data frame with columns PPDF1, PPDF2, PPDF3, PPDF4
#' @param filtering_config List with filtering parameters from configuration
#'
#' @return List with:
#' \itemize{
#'   \item filtered_sets: Data frame of parameter sets that pass biological constraints
#'   \item filtered_ids: Integer vector of original row IDs that passed filtering
#'   \item n_original: Number of original parameter sets
#'   \item n_filtered: Number of parameter sets after filtering
#'   \item pass_rate: Proportion of parameter sets that passed filtering
#' }
#'
#' @details Biological constraints applied:
#' \itemize{
#'   \item Growth factor at 0°C must be <= growth_at_0c threshold (typically 0.00001)
#'   \item Growth factor at 10°C must be >= growth_at_10c threshold (typically 0.001)
#' }
#'
#' @export
apply_biological_filtering <- function(param_sets, filtering_config) {
  # Validate inputs
  required_cols <- c("PPDF1", "PPDF2", "PPDF3", "PPDF4")
  missing_cols <- required_cols[!required_cols %in% names(param_sets)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse=", ")))
  }
  
  n_original <- nrow(param_sets)
  
  # Extract filtering parameters
  growth_at_0c_threshold <- filtering_config$growth_at_0c
  growth_at_10c_threshold <- filtering_config$growth_at_10c
  
  # Calculate growth factors at 0°C and 10°C for all parameter sets
  growth_0c <- create_temp_curve(0, param_sets$PPDF1, param_sets$PPDF2, 
                                param_sets$PPDF3, param_sets$PPDF4)
  growth_10c <- create_temp_curve(10, param_sets$PPDF1, param_sets$PPDF2, 
                                 param_sets$PPDF3, param_sets$PPDF4)
  
  # Apply biological constraints
  constraint_0c <- growth_0c <= growth_at_0c_threshold
  constraint_10c <- growth_10c >= growth_at_10c_threshold
  
  # Find parameter sets that satisfy both constraints
  valid_indices <- which(constraint_0c & constraint_10c)
  
  if (length(valid_indices) == 0) {
    stop("No parameter sets passed biological filtering. Consider relaxing constraints or increasing sample size.")
  }
  
  filtered_sets <- param_sets[valid_indices, ]
  
  result <- list(
    filtered_sets = filtered_sets,
    filtered_ids = valid_indices,
    n_original = n_original,
    n_filtered = length(valid_indices),
    pass_rate = length(valid_indices) / n_original
  )
  
  return(result)
}

#' Create PPDF parameter sets with biological filtering
#'
#' Generates PPDF parameter sets using method-specific sampling (GSA or SIR) and
#' applies biological filtering to ensure realistic temperature response curves.
#' This is the main function for PPDF parameter generation in calibration workflows.
#'
#' @param ppdf_priors Data frame containing PPDF parameter bounds (PPDF1-4 rows)
#' @param n_pset Number of parameter sets needed after filtering
#' @param method Character: "GSA" or "SIR" - determines sampling method and number of sets
#' @param config List containing the full configuration with ppdf section
#'
#' @return List containing:
#' For GSA method:
#' \itemize{
#'   \item pset_1: Data frame with PPDF1-4 columns (first parameter set)
#'   \item pset_2: Data frame with PPDF1-4 columns (second parameter set)
#'   \item method: "GSA"
#'   \item n_pset: Number of parameter sets in each set
#'   \item filtering_stats: Statistics about biological filtering
#' }
#' For SIR method:
#' \itemize{
#'   \item pset_1: Data frame with PPDF1-4 columns (single parameter set)
#'   \item method: "SIR"
#'   \item n_pset: Number of parameter sets
#'   \item filtering_stats: Statistics about biological filtering
#' }
#'
#' @details This function implements the workflow from the archive scripts:
#' 1. Extract PPDF parameter bounds from prior data
#' 2. Generate initial parameter sets (much larger than n_pset)
#' 3. Apply biological filtering using temperature response curves
#' 4. Sample exactly n_pset parameter sets from filtered results
#' 5. Return method-appropriate number of parameter sets
#'
#' @examples
#' \dontrun{
#' # Load configuration
#' config <- yaml::read_yaml("crop_calibration.yaml")
#' 
#' # Load prior data and extract PPDF parameters
#' prior_data <- read.csv(config$input_files$prior_file)
#' detection <- detect_ppdf_parameters(prior_data)
#' ppdf_priors <- prior_data[detection$ppdf_indices, ]
#'
#' # Generate PPDF parameter sets for GSA
#' ppdf_sets <- create_ppdf_pset(ppdf_priors, n_pset=1000, method="GSA", config)
#' 
#' # Access the two parameter sets for Sobol method
#' pset_1 <- ppdf_sets$pset_1
#' pset_2 <- ppdf_sets$pset_2
#' }
#'
#' @export
create_ppdf_pset <- function(ppdf_priors, n_pset, method, config) {
  # Validate inputs
  if (!method %in% c("GSA", "SIR")) {
    stop("method must be 'GSA' or 'SIR'")
  }
  
  # Validate configuration
  config_validation <- validate_ppdf_config(config)
  if (!config_validation$valid) {
    stop(paste("Invalid PPDF configuration:", paste(config_validation$errors, collapse="; ")))
  }
  
  if (!"ppdf" %in% names(config)) {
    stop("Configuration must contain ppdf section for PPDF parameter generation")
  }
  
  # Extract PPDF configuration
  ppdf_config <- config$ppdf
  filtering_config <- ppdf_config$filtering
  method_config <- ppdf_config[[tolower(method)]]
  
  # Validate PPDF priors structure
  expected_params <- c("PPDF1", "PPDF2", "PPDF3", "PPDF4")
  if (!all(expected_params %in% ppdf_priors$ParameterName)) {
    missing <- expected_params[!expected_params %in% ppdf_priors$ParameterName]
    stop(paste("Missing PPDF parameters in priors:", paste(missing, collapse=", ")))
  }
  
  # Extract parameter bounds
  get_param_bounds <- function(param_name) {
    row <- ppdf_priors[ppdf_priors$ParameterName == param_name, ]
    return(c(lower = row$Lower, upper = row$Upper))
  }
  
  bounds <- list(
    PPDF1 = get_param_bounds("PPDF1"),
    PPDF2 = get_param_bounds("PPDF2"),
    PPDF3 = get_param_bounds("PPDF3"),
    PPDF4 = get_param_bounds("PPDF4")
  )
  
  # Calculate initial sample size
  n_sample <- n_pset * method_config$n_sample_multiplier
  
  # Generate parameter sets based on method
  if (method == "GSA") {
    # GSA: Create two parameter sets using Latin Hypercube Sampling with different seeds

    # Load required library for LHS
    if (!requireNamespace("lhs", quietly = TRUE)) {
      stop("Package 'lhs' is required for GSA method. Please install it with: install.packages('lhs')")
    }

    # Set first seed and generate first parameter set using LHS
    set.seed(method_config$seed_1)
    m1 <- lhs::randomLHS(n = n_sample, k = 4)
    p1 <- matrix(0, nrow = n_sample, ncol = 4)

    for (i in 1:4) {
      param_name <- paste0("PPDF", i)
      p1[, i] <- qunif(m1[, i], min = bounds[[param_name]]["lower"], max = bounds[[param_name]]["upper"])
    }

    w1 <- data.frame(p1)
    names(w1) <- c("PPDF1", "PPDF2", "PPDF3", "PPDF4")

    # Set second seed and generate second parameter set using LHS
    set.seed(method_config$seed_2)
    m2 <- lhs::randomLHS(n = n_sample, k = 4)
    p2 <- matrix(0, nrow = n_sample, ncol = 4)

    for (i in 1:4) {
      param_name <- paste0("PPDF", i)
      p2[, i] <- qunif(m2[, i], min = bounds[[param_name]]["lower"], max = bounds[[param_name]]["upper"])
    }

    w2 <- data.frame(p2)
    names(w2) <- c("PPDF1", "PPDF2", "PPDF3", "PPDF4")
    
    # Apply biological filtering to both sets
    filtering_1 <- apply_biological_filtering(w1, filtering_config)
    filtering_2 <- apply_biological_filtering(w2, filtering_config)
    
    # Check if we have enough filtered samples
    max_ratio <- ifelse("max_sample_ratio" %in% names(method_config), method_config$max_sample_ratio, 1.1)
    
    check_sample_size <- function(filtering_result, set_name) {
      if (filtering_result$n_filtered < n_pset) {
        stop(paste(set_name, "filtered parameter set doesn't have enough samples.",
                  "Got", filtering_result$n_filtered, "but need", n_pset,
                  "Try increasing n_sample_multiplier"))
      } else if (filtering_result$n_filtered > max_ratio * n_pset) {
        stop(paste(set_name, "filtered parameter set has too many samples.",
                  "Got", filtering_result$n_filtered, "but only need", n_pset,
                  "Try reducing n_sample_multiplier"))
      } else {
        message(paste(set_name, "filtered parameter set has good amount of samples"))
      }
    }
    
    check_sample_size(filtering_1, "First")
    check_sample_size(filtering_2, "Second")
    
    # Sample exactly n_pset from filtered results
    id_sample_1 <- sample(filtering_1$filtered_ids, size = n_pset)
    id_sample_2 <- sample(filtering_2$filtered_ids, size = n_pset)
    
    final_pset_1 <- w1[id_sample_1, ]
    final_pset_2 <- w2[id_sample_2, ]
    
    # Return results for GSA
    return(list(
      pset_1 = final_pset_1,
      pset_2 = final_pset_2,
      method = "GSA",
      n_pset = n_pset,
      filtering_stats = list(
        set_1 = list(
          n_original = filtering_1$n_original,
          n_filtered = filtering_1$n_filtered,
          pass_rate = filtering_1$pass_rate
        ),
        set_2 = list(
          n_original = filtering_2$n_original,
          n_filtered = filtering_2$n_filtered,
          pass_rate = filtering_2$pass_rate
        )
      )
    ))
    
  } else if (method == "SIR") {
    # SIR: Create single parameter set using Latin Hypercube Sampling
    
    # Load required library
    if (!requireNamespace("lhs", quietly = TRUE)) {
      stop("Package 'lhs' is required for SIR method. Please install it with: install.packages('lhs')")
    }
    
    # Set seed and generate parameter set using LHS
    set.seed(method_config$seed)
    m1 <- lhs::randomLHS(n = n_sample, k = 4)
    p1 <- matrix(0, nrow = nrow(m1), ncol = 4)
    
    for (i in 1:4) {
      param_name <- paste0("PPDF", i)
      p1[, i] <- qunif(m1[, i], min = bounds[[param_name]]["lower"], max = bounds[[param_name]]["upper"])
    }
    
    w1 <- data.frame(p1)
    names(w1) <- c("PPDF1", "PPDF2", "PPDF3", "PPDF4")
    
    # Apply biological filtering
    filtering_1 <- apply_biological_filtering(w1, filtering_config)
    
    # Check sample size
    max_ratio <- ifelse("max_sample_ratio" %in% names(method_config), method_config$max_sample_ratio, 1.1)
    
    if (filtering_1$n_filtered < n_pset) {
      stop(paste("Filtered parameter set doesn't have enough samples.",
                "Got", filtering_1$n_filtered, "but need", n_pset,
                "Try increasing n_sample_multiplier"))
    } else if (filtering_1$n_filtered > max_ratio * n_pset) {
      stop(paste("Filtered parameter set has too many samples.",
                "Got", filtering_1$n_filtered, "but only need", n_pset,
                "Try reducing n_sample_multiplier"))
    } else {
      message("Filtered parameter set has good amount of samples")
    }
    
    # Sample exactly n_pset from filtered results
    id_sample_1 <- sample(filtering_1$filtered_ids, size = n_pset)
    final_pset_1 <- w1[id_sample_1, ]
    
    # Return results for SIR
    return(list(
      pset_1 = final_pset_1,
      method = "SIR",
      n_pset = n_pset,
      filtering_stats = list(
        set_1 = list(
          n_original = filtering_1$n_original,
          n_filtered = filtering_1$n_filtered,
          pass_rate = filtering_1$pass_rate
        )
      )
    ))
  }
}