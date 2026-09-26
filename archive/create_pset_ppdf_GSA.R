#' Creating parameter sets for PPDF1-4 for global sensitivity analysis
#' 
#' There are 4 parameters control the curve for temperature effect on 
#' plant growth. Setting uniform prior ranges for these 4 parameters
#' results in curves that don't make biological sense. Here I first create 
#' multiple curves and filter them to desired number for Sobol indexing
#' 
#' @note Need to source create_temp_curve.R
#' 
#' @param path_data_folder path of the folder containing ppdf parameter ranges 
#' @param path_file_ppdf file name of parameter ranges
#' @param n_sample number of initial samples
#' @param n_pset number of parameters sets needed for sobol GSA method
#' @param seed seed set for reproducibility 
#' 
#' @return two parameter sets in the global environment
#' ppdf_pset_1
#' ppdf_pset_2
#' 
#' @author Yi Yang \email{yi.yang@@colostate.edu}
#' 
#' @date created 08/02/2021
#' 
#' @keywords ppdf, sobol
#' 
#' @examples 
#' fn_ppdf_pset(path_data_folder, path_file_ppdf, n_sample, seed, n_pset)
#' 
#' @export

create_pset_ppdf_GSA <- function(path_data_folder,
                         path_file_ppdf,
                         n_sample,
                         n_pset,
                         seed_1,
                         seed_2) {
  
  # library
  suppressMessages(library(tidyverse))
  
  # source functions
  source('../utils_calibration/create_temp_curve.R')
  
  # load data, parameter ranges
  param_bounds <-
    read.csv(paste0(path_data_folder, path_file_ppdf), header = T)
  var_name <- param_bounds$parameter
  n_params <- length(var_name)
  
  # set seed
  set.seed(seed_1)
  # generating parameter sets
  ## sobols' method required 2 matrices
  m1 <- matrix(runif(n_params * n_sample), nrow = n_sample)
  p1 <- matrix(0, nrow = n_sample, ncol = n_params)
  for (i in 1:n_params) {
    pos <- which(param_bounds$parameter == var_name[i])
    lower <- param_bounds[pos, "lower"]
    upper <- param_bounds[pos, "upper"]
    p1[, i] <- qunif(m1[, i], min = lower, max = upper)
  }
  
  
  set.seed(seed_2)

  m2 <- matrix(runif(n_params * n_sample), nrow = n_sample)
  p2 <- matrix(0, nrow = n_sample, ncol = n_params)
  
  for (i in 1:n_params) {
    pos <- which(param_bounds$parameter == var_name[i])
    lower <- param_bounds[pos, "lower"]
    upper <- param_bounds[pos, "upper"]
    p2[, i] <- qunif(m2[, i], min = lower, max = upper)
  }
  
  w1 <- data.frame(p1)
  w2 <- data.frame(p2)
  names(w1) <- var_name
  names(w2) <- var_name
  
  # select parameter sets that make biological sense
  
  ## w1
  temperature <- seq(0, 50, 0.5)
  
  w1_rep <- w1 %>%
    slice(rep(1:n_sample, each = length(temperature))) %>%
    add_column(temperature = rep(seq(0, 50, 0.5), n_sample)) %>%
    add_column(ID = rep(1:n_sample, each = length(temperature))) %>%
    mutate(gpdf = create_temp_curve(temperature, PPDF1, PPDF2, PPDF3, PPDF4))
  w1_rep_0 <- w1_rep %>%
    as_tibble() %>%
    filter(temperature == 0) %>%
    filter(gpdf <= 0.00001)
  w1_rep_10 <- w1_rep %>%
    as_tibble() %>%
    filter(temperature == 10) %>%
    filter(gpdf >= 0.001)
  w1_rep_selected <- w1_rep %>%
    filter(ID %in% unique(w1_rep_0$ID) &
             ID %in% unique(w1_rep_10$ID))
  ## w2
  w2_rep <- w2 %>%
    slice(rep(1:n_sample, each = length(temperature))) %>%
    add_column(temperature = rep(seq(0, 50, 0.5), n_sample)) %>%
    add_column(ID = rep(1:n_sample, each = length(temperature))) %>%
    mutate(gpdf = create_temp_curve(temperature, PPDF1, PPDF2, PPDF3, PPDF4))
  w2_rep_0 <- w2_rep %>%
    as_tibble() %>%
    filter(temperature == 0) %>%
    filter(gpdf <= 0.00001)
  w2_rep_10 <- w2_rep %>%
    as_tibble() %>%
    filter(temperature == 10) %>%
    filter(gpdf >= 0.001)
  w2_rep_selected <- w2_rep %>%
    filter(ID %in% unique(w2_rep_0$ID) &
             ID %in% unique(w2_rep_10$ID))
  
  length_w1_rep_selected <- length(unique(w1_rep_selected$ID))
  length_w2_rep_selected <- length(unique(w2_rep_selected$ID))
  
  # check if parameter sets number > n_pset after filtering 
  if (length_w1_rep_selected < n_pset) {
    stop(
      "First filtered parameter set doesn't have enough samples.
      Try to increase the n_sample"
    )
  } else if (length_w1_rep_selected > 1.1 * n_pset) {
    stop("First filtered parameter set has too many samples.
         Try to reduce the n_sample")
  } else{
    message("First filtered parameter set has good amount of samples")
  }
  
  if (length_w2_rep_selected < n_pset) {
    stop(
      "Second filtered parameter set doesn't have enough samples.
      Try to increase the n_sample"
    )
  } else if (length_w2_rep_selected > 1.1 * n_pset) {
    stop("Second filtered parameter set has too many samples.
         Try to reduce the n_sample")
  } else{
    message("Second filtered parameter set has good amount of samples")
  }
  
  # sample n_pset from filtered parameter sets
  id_sample_w1 <- sample(unique(w1_rep_selected$ID), size = n_pset)
  id_sample_w2 <- sample(unique(w2_rep_selected$ID), size = n_pset)
  
  # store the two parameters sets in the global environment
  ppdf_pset_1 <<- slice(w1, id_sample_w1)
  ppdf_pset_2 <<- slice(w2, id_sample_w2)
}
