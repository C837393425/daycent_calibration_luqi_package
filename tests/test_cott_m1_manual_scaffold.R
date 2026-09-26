.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_cott_m1.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_cott_m1_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_cott_m1_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/cott_m1_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_cott_m1_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/cott_m1_ruetb_1p05.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/cott_m1_ruetb_1p15.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/cott_m1_ruetb_1p25.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/cott_m1_ruetb_1p15_himax_0p30.csv",
  "data/crop_yield_cott_m1/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_cott_m1/DayCent_Files/Crop_Parameters_Priors.csv",
  "data/crop_yield_cott_m1/DayCent_Files/Output_Variables_Specification.csv",
  "data/crop_yield_cott_m1/DayCent_Files/dot100Files/outvars_names.txt"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing cotton m1 scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_cott_m1_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_cott_m1")) {
  stop("Expected project.name to be crop_yield_cott_m1")
}

if (!identical(cfg$base_config$project$date_stamp, "17Jun2026")) {
  stop("Expected project.date_stamp to be 17Jun2026")
}

if (!identical(cfg$base_config$daycent$crop$name, "COT")) {
  stop("Expected daycent.crop.name to be COT")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected cotton m1 manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$target_variable, "cgrain")) {
  stop("Expected cotton m1 local comparison.target_variable to be cgrain")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected cotton m1 local comparison.aggregation_grain to be state")
}

local_tables <- cfg$manual_config$manual_calibration$database_result$tables
if (!grepl("_state_", local_tables$annual_results, fixed = TRUE) ||
    !grepl("_state_", local_tables$weighted_results, fixed = TRUE) ||
    !grepl("_state_", local_tables$run_status, fixed = TRUE)) {
  stop("Expected cotton m1 local manual output tables to be labeled state-level")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_cott_m1_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$target_variable, "cgrain")) {
  stop("Expected cotton m1 cluster comparison.target_variable to be cgrain")
}

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
  stop("Expected cotton m1 cluster comparison.aggregation_grain to be state")
}

cluster_tables <- cluster_cfg$manual_config$manual_calibration$database_result$tables
if (!grepl("_state_", cluster_tables$annual_results, fixed = TRUE) ||
    !grepl("_state_", cluster_tables$weighted_results, fixed = TRUE) ||
    !grepl("_state_", cluster_tables$run_status, fixed = TRUE)) {
  stop("Expected cotton m1 cluster manual output tables to be labeled state-level")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

expected_trials <- "cott_m1_ruetb_1p15"

if (!identical(trials$trial_id, expected_trials)) {
  stop("Expected enabled cotton m1 trial: ", expected_trials)
}

missing_parameter_files <- trials$parameter_file[!file.exists(trials$parameter_file)]
if (length(missing_parameter_files) > 0) {
  stop("Missing parameter files for enabled trials: ", paste(missing_parameter_files, collapse = ", "))
}

spec <- read.csv("data/crop_yield_cott_m1/DayCent_Files/Output_Variables_Specification.csv")
if (!identical(spec$variable_name[1], "cgrain")) {
  stop("Expected cotton m1 output variable to be cgrain")
}

outvars <- readLines("data/crop_yield_cott_m1/DayCent_Files/dot100Files/outvars_names.txt")
if (!"cgrain" %in% outvars) {
  stop("Expected cotton m1 outvars_names.txt to include cgrain")
}

likelihood <- cfg$base_config$likelihood_calculation$variables[[1]]
if (!identical(likelihood$model_output, "cgrain")) {
  stop("Expected cotton m1 likelihood model_output to be cgrain")
}

map <- read.csv("workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_cott_m1_map.csv")
expected_values <- c(
  RUETB = 1.55,
  BIOMAX = 300.0,
  HIMAX = 0.35,
  "PPDF(1)" = 27.0,
  "PPDF(2)" = 40.0,
  "PPDF(3)" = 1.0,
  "PPDF(4)" = 2.5
)

for (parameter in names(expected_values)) {
  actual <- as.numeric(map$value[map$Parameter == parameter])
  if (!identical(actual, expected_values[[parameter]])) {
    stop("Expected ", parameter, "=", expected_values[[parameter]], " in cotton m1 parameter map")
  }
}

cat("Cotton m1 manual calibration scaffold OK\n")
