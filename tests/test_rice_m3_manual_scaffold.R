.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

read_crop100_parameter <- function(crop100_file, crop_name, parameter_name) {
  crop_lines <- readLines(crop100_file)
  crop_pos <- grep(paste0("^", crop_name, "\\s"), crop_lines)
  if (length(crop_pos) != 1) {
    stop("Expected one ", crop_name, " block in ", crop100_file, "; found ", length(crop_pos))
  }

  next_crop_pos <- grep("^\\S+\\s+#", crop_lines)
  next_crop_pos <- next_crop_pos[next_crop_pos > crop_pos]
  block_end <- if (length(next_crop_pos) > 0) next_crop_pos[1] - 1L else length(crop_lines)
  block_lines <- crop_lines[(crop_pos + 1L):block_end]

  param_pattern <- paste0("^\\s*([0-9.+-eE]+)\\s+", parameter_name, "\\b")
  param_match <- grep(param_pattern, block_lines, value = TRUE)
  if (length(param_match) == 0) {
    stop("Parameter ", parameter_name, " not found in ", crop_name, " block")
  }

  as.numeric(sub(param_pattern, "\\1", param_match[1]))
}

required_files <- c(
  "workflows/configs/crop_yield_rice_m3.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m3_local.yaml",
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m3_cluster.yaml",
  "workflows/scripts/ManualCalibration/trials/rice_m3_trials.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_rice_m3_defaults.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_rice_m3_map.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_pct_m10.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_pct_p10.csv",
  "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_pct_p20.csv",
  "data/crop_yield_rice_m3/Observation_Data/ObservData_crop.csv",
  "data/crop_yield_rice_m3/DayCent_Files/Crop_Parameters_Priors.csv",
  "data/crop_yield_rice_m3/DayCent_Files/dot100Files/crop.100"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing rice m3 scaffold files: ", paste(missing_files, collapse = ", "))
}

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m3_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cfg$base_config$project$name, "crop_yield_rice_m3")) {
  stop("Expected project.name to be crop_yield_rice_m3")
}

expected_crops <- c("RICM", "RICR")
if (!identical(cfg$base_config$daycent$crop$name, expected_crops)) {
  stop("Expected daycent.crop.name to be RICM and RICR")
}

if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
  stop("Expected rice m3 manual calibration to use merge_default_params = false")
}

if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected rice m3 local comparison.aggregation_grain to be county")
}

cluster_cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_rice_m3_cluster.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

if (!identical(cluster_cfg$manual_config$manual_calibration$comparison$aggregation_grain, "county")) {
  stop("Expected rice m3 cluster comparison.aggregation_grain to be county")
}

trials <- read_trials_table(cfg$manual_config, cfg$config_dir, verbose = FALSE)

if (!identical(trials$trial_id, "ruetb_p50_ppdf_28_42")) {
  stop("Expected exactly one enabled trial: ruetb_p50_ppdf_28_42")
}

if (!grepl("calibrated", trials$description[trials$trial_id == "ruetb_p50_ppdf_28_42"], fixed = TRUE)) {
  stop("Expected ruetb_p50_ppdf_28_42 to be marked as calibrated")
}

map <- read.csv("workflows/scripts/ManualCalibration/trials/parameter_sets/manual_parameters_rice_m3_map.csv")
if (!identical(as.numeric(map$value[map$Crop == "RICM" & map$Parameter == "RUETB"]), 3.90)) {
  stop("Expected manual_parameters_rice_m3_map.csv RICM RUETB=3.90")
}
if (!identical(as.numeric(map$value[map$Crop == "RICR" & map$Parameter == "RUETB"]), 4.65)) {
  stop("Expected manual_parameters_rice_m3_map.csv RICR RUETB=4.65")
}
if (!identical(as.numeric(map$value[map$Crop == "RICR" & map$Parameter == "HIMAX"]), 0.62)) {
  stop("Expected manual_parameters_rice_m3_map.csv RICR HIMAX=0.62")
}

pct_trial_file <- "workflows/scripts/ManualCalibration/trials/parameter_sets/ruetb_pct_p10.csv"
parameter_bundle <- load_manual_calibration_parameter_table(
  config = cfg$base_config,
  helper_env = helper_env,
  manual_parameter_file = pct_trial_file,
  merge_default_params = FALSE,
  verbose = FALSE
)

if (!"Crop" %in% names(parameter_bundle$params_df)) {
  stop("Expected percentage trial parameter file to include Crop column")
}

template_dir <- file.path(tempdir(), "rice_m3_pct_p10_template")
helper_env$build_validation_common_template(
  config = cfg$base_config,
  params_df = parameter_bundle$params_df,
  template_dir = template_dir,
  verbose = FALSE
)

crop100_file <- file.path(template_dir, "crop.100")
ricm_ruetb <- read_crop100_parameter(crop100_file, "RICM", "RUETB")
ricr_ruetb <- read_crop100_parameter(crop100_file, "RICR", "RUETB")

if (!identical(ricm_ruetb, 2.86)) {
  stop("Expected RICM RUETB=2.86 after +10% trial; got ", ricm_ruetb)
}

if (!identical(ricr_ruetb, 3.41)) {
  stop("Expected RICR RUETB=3.41 after +10% trial; got ", ricr_ruetb)
}

if (identical(ricm_ruetb, ricr_ruetb)) {
  stop("Expected distinct RUETB values for RICM and RICR")
}

unlink(template_dir, recursive = TRUE)

cat("Rice m3 manual calibration scaffold OK\n")
