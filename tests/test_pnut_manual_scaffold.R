.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_pnut.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_pnut_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_pnut_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/pnut_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_pnut_map.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing peanut scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_pnut_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_pnut")) {
  stop("Expected project.name to be crop_yield_pnut")
}

if (!identical(cfg$base_config$daycent$crop$name, "PNUT")) {
  stop("Expected daycent.crop.name to be PNUT")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected peanut manual calibration to use merge_default_params = false")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

expected_trial_ids <- c("ruetb_1p75", "ruetb_1p70")
if (!identical(sort(trials$trial_id), sort(expected_trial_ids))) {
  stop(
    "Expected enabled RUETB sweep trials: ",
    paste(expected_trial_ids, collapse = ", ")
  )
}

for (parameter_file in trials$parameter_file) {
  if (!file.exists(parameter_file)) {
    stop("Missing parameter file for enabled trial: ", parameter_file)
  }
}

cat("Peanut manual calibration scaffold OK\n")
