.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

required_files <- c(
  "workflows/configs/crop_yield_rice_m1.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m1_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m1_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/rice_m1_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_rice_m1_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/rice_m1_ruetb_2p60_himax_0p62.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/rice_m1_ruetb_2p90_himax_0p62.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/rice_m1_ruetb_3p10_himax_0p62.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/rice_m1_ruetb_3p30_himax_0p62.csv",
  "data/crop_yield_rice_m1/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_rice_m1/DayCent_Files/Crop_Parameters_Priors.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing rice m1 scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m1_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_rice_m1")) {
  stop("Expected project.name to be crop_yield_rice_m1")
}

if (!identical(cfg$base_config$daycent$crop$name, "RICA")) {
  stop("Expected daycent.crop.name to be RICA")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected rice m1 manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected rice m1 local comparison.aggregation_grain to be county")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m1_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected rice m1 cluster comparison.aggregation_grain to be county")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (!identical(trials$trial_id, "ruetb_3p10_himax_0p62")) {
  stop("Expected exactly one enabled trial: ruetb_3p10_himax_0p62")
}

if (!grepl("calibrated", trials$description[trials$trial_id == "ruetb_3p10_himax_0p62"], fixed = TRUE)) {
  stop("Expected ruetb_3p10_himax_0p62 to be marked as calibrated")
}

if (!file.exists(trials$parameter_file[1])) {
  stop("Missing parameter file for enabled trial: ", trials$parameter_file[1])
}

map <- read.csv("workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_rice_m1_map.csv")
ruetb <- map$value[map$Parameter == "RUETB"]
himax <- map$value[map$Parameter == "HIMAX"]
if (!identical(as.numeric(ruetb), 3.10) || !identical(as.numeric(himax), 0.62)) {
  stop("Expected manual_parameters_rice_m1_map.csv RUETB=3.10 HIMAX=0.62")
}

cat("Rice m1 manual calibration scaffold OK\n")
