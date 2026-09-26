.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_drbe.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_drbe_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_drbe_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/drbe_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_drbe_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_1p00.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_1p10.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_1p20.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing dry bean scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_drbe_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_drbe")) {
  stop("Expected project.name to be crop_yield_drbe")
}

if (!identical(cfg$base_config$daycent$crop$name, "DBEAN")) {
  stop("Expected daycent.crop.name to be DBEAN")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected dry bean manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected dry bean local comparison.aggregation_grain to be state")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_drbe_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected dry bean cluster comparison.aggregation_grain to be state")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

expected_trial_ids <- c("ruetb_1p00", "ruetb_1p10", "ruetb_1p20")
if (!identical(sort(trials$trial_id), sort(expected_trial_ids))) {
  stop(
    "Expected enabled dry bean RUETB sweep trials: ",
    paste(expected_trial_ids, collapse = ", ")
  )
}

for (parameter_file in trials$parameter_file) {
  if (!file.exists(parameter_file)) {
    stop("Missing parameter file for enabled trial: ", parameter_file)
  }
}

table_names <- vapply(
  seq_len(nrow(trials)),
  function(i) {
    table_info <- resolve_manual_table_names(cluster_cfg$base_config, cluster_cfg$manual_config, trials[i, , drop = FALSE])
    table_info$annual_table_base
  },
  FUN.VALUE = character(1)
)

if (length(unique(table_names)) != length(expected_trial_ids)) {
  stop("Expected each dry bean RUETB sweep trial to use a distinct annual results table")
}

cat("Dry bean manual calibration scaffold OK\n")
