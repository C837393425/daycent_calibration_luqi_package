.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

variants <- c("m2", "m3", "m4")

for (variant in variants) {
  project_name <- paste0("crop_yield_cott_", variant)
  required_files <- c(
    file.path("workflows/configs", paste0(project_name, ".yaml")),
    file.path("workflows/scripts/ManualCalibration", paste0("manual_calibration_cott_", variant, "_local.yaml")),
    file.path("workflows/scripts/ManualCalibration", paste0("manual_calibration_cott_", variant, "_cluster.yaml")),
    file.path("workflows/scripts/ManualCalibration/trials", paste0("cott_", variant, "_trials.csv")),
    file.path("workflows/scripts/ManualCalibration/trials/parameter_sets", paste0("manual_parameters_cott_", variant, "_map.csv")),
    file.path("data", project_name, "Observation_Data/ObservData_crop.csv"),
    file.path("data", project_name, "DayCent_Files/Crop_Parameters_Priors.csv"),
    file.path("data", project_name, "DayCent_Files/Output_Variables_Specification.csv"),
    file.path("data", project_name, "DayCent_Files/dot100Files/outvars_names.txt")
  )

  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0) {
    stop("Missing cotton ", variant, " scaffold files: ", paste(missing_files, collapse = ", "))
  }

  local_cfg <- read_manual_calibration_config(
    file.path("workflows/scripts/ManualCalibration", paste0("manual_calibration_cott_", variant, "_local.yaml")),
    helper_env = helper_env,
    verbose = FALSE
  )
  cluster_cfg <- read_manual_calibration_config(
    file.path("workflows/scripts/ManualCalibration", paste0("manual_calibration_cott_", variant, "_cluster.yaml")),
    helper_env = helper_env,
    verbose = FALSE
  )

  if (!identical(local_cfg$base_config$project$name, project_name)) {
    stop("Expected project.name to be ", project_name)
  }
  if (!identical(local_cfg$base_config$project$date_stamp, "17Jun2026")) {
    stop("Expected ", project_name, " project.date_stamp to be 17Jun2026")
  }
  if (!identical(local_cfg$base_config$daycent$crop$name, "COT")) {
    stop("Expected ", project_name, " daycent.crop.name to be COT")
  }
  if (!identical(local_cfg$base_config$file_source$database$tables$run_order, paste0("run_order_cott_", variant))) {
    stop("Expected ", project_name, " run_order table to be run_order_cott_", variant)
  }
  if (!identical(local_cfg$base_config$file_source$database$tables$schedule_files, paste0("schedule_files_cott_", variant))) {
    stop("Expected ", project_name, " schedule_files table to be schedule_files_cott_", variant)
  }
  if (!identical(local_cfg$base_config$file_source$database$tables$site_files, paste0("site_files_cott_", variant))) {
    stop("Expected ", project_name, " site_files table to be site_files_cott_", variant)
  }

  for (cfg in list(local_cfg, cluster_cfg)) {
    if (!identical(cfg$manual_config$manual_calibration$merge_default_params, FALSE)) {
      stop("Expected cotton ", variant, " manual calibration to use merge_default_params = false")
    }
    if (!identical(cfg$manual_config$manual_calibration$comparison$target_variable, "cgrain")) {
      stop("Expected cotton ", variant, " comparison.target_variable to be cgrain")
    }
    if (!identical(cfg$manual_config$manual_calibration$comparison$aggregation_grain, "state")) {
      stop("Expected cotton ", variant, " comparison.aggregation_grain to be state")
    }
  }

  expected_trials_by_variant <- list(
    m2 = "cott_m2_ruetb_1p15",
    m3 = "cott_m3_ruetb_1p10",
    m4 = "cott_m4_ruetb_1p00"
  )
  expected_trials <- expected_trials_by_variant[[variant]]

  trials <- read_trials_table(local_cfg$manual_config, local_cfg$config_dir, verbose = FALSE)
  if (!identical(trials$trial_id, expected_trials)) {
    stop("Expected enabled cotton ", variant, " trial: ", expected_trials)
  }
  missing_parameter_files <- trials$parameter_file[!file.exists(trials$parameter_file)]
  if (length(missing_parameter_files) > 0) {
    stop("Missing parameter files for cotton ", variant, " trials: ", paste(missing_parameter_files, collapse = ", "))
  }
  for (parameter_file in trials$parameter_file) {
    params <- read.csv(parameter_file, stringsAsFactors = FALSE)
    if (nrow(params) != 1 ||
        !identical(params$File[1], "crop.100") ||
        !identical(params$Parameter[1], "RUETB")) {
      stop("Expected cotton ", variant, " trial parameter file to modify only crop.100 RUETB: ", parameter_file)
    }
  }

  output_spec <- read.csv(file.path("data", project_name, "DayCent_Files/Output_Variables_Specification.csv"), stringsAsFactors = FALSE)
  if (!identical(output_spec$variable_name[1], "cgrain")) {
    stop("Expected ", project_name, " output variable to be cgrain")
  }

  outvars <- readLines(file.path("data", project_name, "DayCent_Files/dot100Files/outvars_names.txt"))
  if (!"cgrain" %in% outvars) {
    stop("Expected ", project_name, " outvars_names.txt to include cgrain")
  }

  map <- read.csv(file.path("workflows/scripts/ManualCalibration/trials/parameter_sets", paste0("manual_parameters_cott_", variant, "_map.csv")))
  expected_values <- c(RUETB = 1.55, BIOMAX = 300.0, HIMAX = 0.35)
  for (parameter in names(expected_values)) {
    actual <- as.numeric(map$value[map$Parameter == parameter])
    if (!identical(actual, expected_values[[parameter]])) {
      stop("Expected cotton ", variant, " ", parameter, "=", expected_values[[parameter]], " in parameter map")
    }
  }
}

cat("Cotton m2/m3/m4 manual calibration scaffolds OK\n")
