.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_toma.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_toma_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_toma_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/toma_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_toma_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_2p20.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_2p25.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_2p30.csv",
  "data/crop_yield_toma/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_toma/DayCent_Files/Crop_Parameters_Priors.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing tomato scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_toma_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_toma")) {
  stop("Expected project.name to be crop_yield_toma")
}

if (!identical(cfg$base_config$daycent$crop$name, "JTOM")) {
  stop("Expected daycent.crop.name to be JTOM")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected tomato manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected tomato local comparison.aggregation_grain to be state")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_toma_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected tomato cluster comparison.aggregation_grain to be state")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

expected_trial_ids <- c("ruetb_2p25")
if (!identical(sort(trials$trial_id), sort(expected_trial_ids))) {
  stop(
    "Expected enabled tomato RUETB sweep trials: ",
    paste(expected_trial_ids, collapse = ", ")
  )
}

for (parameter_file in trials$parameter_file) {
  if (!file.exists(parameter_file)) {
    stop("Missing parameter file for enabled trial: ", parameter_file)
  }
}

cat("Tomato manual calibration scaffold OK\n")
