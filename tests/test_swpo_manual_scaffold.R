.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_swpo.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_swpo_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_swpo_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/swpo_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_swpo_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_1p35.csv",
  "data/crop_yield_swpo/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_swpo/DayCent_Files/Crop_Parameters_Priors.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing sweet potato scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_swpo_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_swpo")) {
  stop("Expected project.name to be crop_yield_swpo")
}

if (!identical(cfg$base_config$daycent$crop$name, "SWPOT")) {
  stop("Expected daycent.crop.name to be SWPOT")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected sweet potato manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected sweet potato comparison.aggregation_grain to be county")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (!identical(trials$trial_id, "ruetb_1p35")) {
  stop("Expected enabled calibrated trial: ruetb_1p35")
}

cat("Sweet potato manual calibration scaffold OK\n")
