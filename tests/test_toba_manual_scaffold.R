.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_toba.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_toba_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_toba_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/toba_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_toba_map.csv",
  "data/crop_yield_toba/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_toba/DayCent_Files/Crop_Parameters_Priors.csv",
  "data/crop_yield_toba/DayCent_Files/Output_Variables_Specification.csv",
  "data/crop_yield_toba/DayCent_Files/dot100Files/outvars_names.txt"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing tobacco scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_toba_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_toba")) {
  stop("Expected project.name to be crop_yield_toba")
}

if (!identical(cfg$base_config$daycent$crop$name, "TOB")) {
  stop("Expected daycent.crop.name to be TOB")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected tobacco manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected tobacco local comparison.aggregation_grain to be state")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$target_variable, "cgrain")) {
  stop("Expected tobacco local comparison.target_variable to be cgrain")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$modeled_value_factor, 1)) {
  stop("Expected tobacco local comparison.modeled_value_factor to default to 1")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_toba_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected tobacco cluster comparison.aggregation_grain to be state")
}

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$target_variable, "cgrain")) {
  stop("Expected tobacco cluster comparison.target_variable to be cgrain")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (nrow(trials) < 1) {
  stop("Expected at least one enabled tobacco trial")
}

if (!all(grepl("^toba_ruetb_", trials$trial_id))) {
  stop("Expected enabled tobacco trials to use toba_ruetb_* trial_id naming")
}

missing_parameter_files <- trials$parameter_file[!file.exists(trials$parameter_file)]
if (length(missing_parameter_files) > 0) {
  stop("Missing parameter files for enabled trials: ", paste(missing_parameter_files, collapse = ", "))
}

output_spec <- read.csv("data/crop_yield_toba/DayCent_Files/Output_Variables_Specification.csv", stringsAsFactors = FALSE)
if (!identical(output_spec$variable_name[1], "cgrain")) {
  stop("Expected Output_Variables_Specification.csv to use cgrain")
}

outvars <- readLines("data/crop_yield_toba/DayCent_Files/dot100Files/outvars_names.txt")
if (!"cgrain" %in% outvars) {
  stop("Expected outvars_names.txt to include cgrain")
}

likelihood_vars <- cfg$base_config$likelihood_calculation$variables
if (!identical(likelihood_vars[[1]]$model_output, "cgrain")) {
  stop("Expected likelihood_calculation model_output to be cgrain")
}

cat("Tobacco manual calibration scaffold OK\n")
