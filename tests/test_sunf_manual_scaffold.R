.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_sunf.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_sunf_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_sunf_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/sunf_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_sunf_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_1p50.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_1p70.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_1p90.csv",
  "data/crop_yield_sunf/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_sunf/DayCent_Files/Crop_Parameters_Priors.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing sunflower scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_sunf_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_sunf")) {
  stop("Expected project.name to be crop_yield_sunf")
}

if (!identical(cfg$base_config$daycent$crop$name, "SUN")) {
  stop("Expected daycent.crop.name to be SUN")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected sunflower manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected sunflower local comparison.aggregation_grain to be state")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_sunf_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected sunflower cluster comparison.aggregation_grain to be state")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (!identical(trials$trial_id, "ruetb_1p50")) {
  stop("Expected exactly one enabled trial: ruetb_1p50")
}

if (!grepl("calibrated", trials$description[trials$trial_id == "ruetb_1p50"], fixed = TRUE)) {
  stop("Expected ruetb_1p50 to be marked as calibrated")
}

if (!file.exists(trials$parameter_file[1])) {
  stop("Missing parameter file for enabled trial: ", trials$parameter_file[1])
}

cat("Sunflower manual calibration scaffold OK\n")
