.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_pota.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_pota_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_pota_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/pota_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_pota_map.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing potato scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_pota_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_pota")) {
  stop("Expected project.name to be crop_yield_pota")
}

if (!identical(cfg$base_config$daycent$crop$name, "POT")) {
  stop("Expected daycent.crop.name to be POT")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected potato manual calibration to use merge_default_params = false")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (!identical(trials$trial_id, "ruetb_1p50")) {
  stop("Expected enabled calibrated trial: ruetb_1p50")
}

for (parameter_file in trials$parameter_file) {
  if (!file.exists(parameter_file)) {
    stop("Missing parameter file for enabled trial: ", parameter_file)
  }
}

cat("Potato manual calibration scaffold OK\n")
