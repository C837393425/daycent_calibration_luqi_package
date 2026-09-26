.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_lent.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_lent_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_lent_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/lent_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_lent_map.csv",
  "data/crop_yield_lent/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_lent/DayCent_Files/Crop_Parameters_Priors.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing lentil scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_lent_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_lent")) {
  stop("Expected project.name to be crop_yield_lent")
}

if (!identical(cfg$base_config$daycent$crop$name, "LENT")) {
  stop("Expected daycent.crop.name to be LENT")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected lentil manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected lentil local comparison.aggregation_grain to be county")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_lent_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected lentil cluster comparison.aggregation_grain to be county")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (!identical(trials$trial_id, "default_run")) {
  stop("Expected enabled lentil trial: default_run")
}

cat("Lentil manual calibration scaffold OK\n")
