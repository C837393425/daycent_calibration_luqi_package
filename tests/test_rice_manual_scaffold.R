.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_rice.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_rice_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_rice_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/rice_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_rice_map.csv",
  "data/crop_yield_rice/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_rice/DayCent_Files/Crop_Parameters_Priors.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing rice scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_rice_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_rice")) {
  stop("Expected project.name to be crop_yield_rice")
}

expected_crops <- c("RICL", "RICM", "RICR", "RICA")
crop_names <- cfg$base_config$daycent$crop$name
if (is.list(crop_names)) {
  crop_names <- unlist(crop_names, use.names = FALSE)
}
if (!identical(sort(crop_names), sort(expected_crops))) {
  stop("Expected daycent.crop.name to be RICL, RICM, RICR, RICA")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected rice manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected rice comparison.aggregation_grain to be county")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (!identical(trials$trial_id, "default_run")) {
  stop("Expected exactly one enabled trial: default_run")
}

if (!grepl("default_parameters\\.csv$", trials$parameter_file[trials$trial_id == "default_run"])) {
  stop("Expected default_run to use trials/parameter_sets/default_parameters.csv")
}

cat("Rice manual calibration scaffold OK\n")
