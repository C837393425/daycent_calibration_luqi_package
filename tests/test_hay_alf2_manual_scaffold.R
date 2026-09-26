.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_hay_ALF2.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_hay_alf2_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_hay_alf2_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/hay_alf2_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_hay_alf2_map.csv",
  "data/crop_yield_hay_ALF2/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_hay_ALF2/DayCent_Files/Crop_Parameters_Priors.csv",
  "data/crop_yield_hay_ALF2/DayCent_Files/Output_Variables_Specification.csv",
  "data/crop_yield_hay_ALF2/DayCent_Files/dot100Files/outvars_names.txt"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing hay ALF2 scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_hay_alf2_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_hay_ALF2")) {
  stop("Expected project.name to be crop_yield_hay_ALF2")
}

if (!identical(cfg$base_config$daycent$crop$name, "ALF2")) {
  stop("Expected daycent.crop.name to be ALF2")
}

if (!identical(cfg$base_config$project$date_stamp, "19Jun2026")) {
  stop("Expected project.date_stamp to be 19Jun2026")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected hay ALF2 manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected hay ALF2 local comparison.aggregation_grain to be county")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$target_variable, "agcprd")) {
  stop("Expected hay ALF2 local comparison.target_variable to be agcprd")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$exclude_zero_observations, TRUE)) {
  stop("Expected hay ALF2 local comparison.exclude_zero_observations to be TRUE")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_hay_alf2_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected hay ALF2 cluster comparison.aggregation_grain to be county")
}

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$target_variable, "agcprd")) {
  stop("Expected hay ALF2 cluster comparison.target_variable to be agcprd")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

expected_trials <- c(
  "hay_alf2_ruetb_2p90",
  "hay_alf2_ruetb_2p65",
  "hay_alf2_ruetb_2p50",
  "hay_alf2_ruetb_2p20"
)

if (!identical(trials$trial_id, expected_trials)) {
  stop("Expected enabled hay ALF2 RUETB trials: ", paste(expected_trials, collapse = ", "))
}

missing_parameter_files <- trials$parameter_file[!file.exists(trials$parameter_file)]
if (length(missing_parameter_files) > 0) {
  stop("Missing parameter files for enabled trials: ", paste(missing_parameter_files, collapse = ", "))
}

table_names <- vapply(
  seq_len(nrow(trials)),
  function(i) {
    table_info <- resolve_manual_table_names(
      cluster_cfg$base_config,
      cluster_cfg$manual_config,
      trials[i, , drop = FALSE]
    )
    table_info$annual_table_base
  },
  FUN.VALUE = character(1)
)

if (length(unique(table_names)) != nrow(trials)) {
  stop(
    "Expected each hay ALF2 RUETB sweep trial to use a distinct annual results table; got: ",
    paste(unique(table_names), collapse = ", ")
  )
}

output_spec <- read.csv("data/crop_yield_hay_ALF2/DayCent_Files/Output_Variables_Specification.csv", stringsAsFactors = FALSE)
if (!identical(output_spec$variable_name[1], "agcprd")) {
  stop("Expected Output_Variables_Specification.csv to use agcprd")
}

outvars <- readLines("data/crop_yield_hay_ALF2/DayCent_Files/dot100Files/outvars_names.txt")
if (!"agcprd" %in% outvars) {
  stop("Expected outvars_names.txt to include agcprd")
}

likelihood_vars <- cfg$base_config$likelihood_calculation$variables
if (!identical(likelihood_vars[[1]]$model_output, "agcprd")) {
  stop("Expected likelihood_calculation model_output to be agcprd")
}

cat("Hay ALF2 manual calibration scaffold OK\n")
