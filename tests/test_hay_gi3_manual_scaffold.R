.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_hay_GI3.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_hay_gi3_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_hay_gi3_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/hay_gi3_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_hay_gi3_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_1p00.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_1p25.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_1p50.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_1p75.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_2p00.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_2p25.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_2p50.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/hay_gi3_ruetb_2p75.csv",
  "data/crop_yield_hay_GI3/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_hay_GI3/DayCent_Files/Crop_Parameters_Priors.csv",
  "data/crop_yield_hay_GI3/DayCent_Files/Output_Variables_Specification.csv",
  "data/crop_yield_hay_GI3/DayCent_Files/dot100Files/outvars_names.txt"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing hay GI3 scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_hay_gi3_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_hay_GI3")) {
  stop("Expected project.name to be crop_yield_hay_GI3")
}

if (!identical(cfg$base_config$daycent$crop$name, "GI3")) {
  stop("Expected daycent.crop.name to be GI3")
}

if (!identical(cfg$base_config$project$date_stamp, "17Jun2026")) {
  stop("Expected project.date_stamp to be 17Jun2026")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected hay GI3 manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected hay GI3 local comparison.aggregation_grain to be county")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$target_variable, "accrst")) {
  stop("Expected hay GI3 local comparison.target_variable to be accrst")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$exclude_zero_observations, TRUE)) {
  stop("Expected hay GI3 local comparison.exclude_zero_observations to be TRUE")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_hay_gi3_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected hay GI3 cluster comparison.aggregation_grain to be county")
}

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$target_variable, "accrst")) {
  stop("Expected hay GI3 cluster comparison.target_variable to be accrst")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)
expected_trials <- "hay_gi3_ruetb_1p00"

if (!identical(trials$trial_id, expected_trials)) {
  stop("Expected enabled hay GI3 trial: ", expected_trials)
}

output_spec <- read.csv("data/crop_yield_hay_GI3/DayCent_Files/Output_Variables_Specification.csv", stringsAsFactors = FALSE)
if (!identical(output_spec$variable_name[1], "accrst")) {
  stop("Expected Output_Variables_Specification.csv to use accrst")
}

outvars <- readLines("data/crop_yield_hay_GI3/DayCent_Files/dot100Files/outvars_names.txt")
if (!"accrst" %in% outvars) {
  stop("Expected outvars_names.txt to include accrst")
}

likelihood_vars <- cfg$base_config$likelihood_calculation$variables
if (!identical(likelihood_vars[[1]]$model_output, "accrst")) {
  stop("Expected likelihood_calculation model_output to be accrst")
}

cat("Hay GI3 manual calibration scaffold OK\n")
