.libPaths(c("rlib", .libPaths()))
source("workflows/scripts/ManualCalibration/manual_calibration_helpers.R")

helper_env <- new.env(parent = globalenv())
sys.source("workflows/scripts/crop_yield_all/Validation/validation_helpers.R", envir = helper_env)

cfg <- read_manual_calibration_config(
  "workflows/scripts/ManualCalibration/manual_calibration_drbe_local.yaml",
  helper_env = helper_env,
  verbose = FALSE
)

cfg$manual_config$manual_calibration$comparison$aggregation_grain <- "state"

weighted_results <- data.frame(
  sample_id = c(0L, 0L, 0L, 0L),
  aggregation_level = c(6011L, 6013L, 16001L, 16005L),
  year = c(2019L, 2019L, 2019L, 2019L),
  variable_name = rep("cgrain", 4),
  weighted_value = c(100, 200, 90, 110),
  n_sites = c(2L, 3L, 4L, 6L),
  total_weight = c(1, 3, 2, 2),
  stringsAsFactors = FALSE
)

alias_root <- tempfile("drbe_state_alias_")
dir.create(alias_root, recursive = TRUE)
trial_output <- list(
  scratch_root = file.path(alias_root, "scratch"),
  date_stamp = "03Jun2026"
)

run_file <- data.frame(
  siteID = c("site_a", "site_b", "site_c"),
  aggregation_level = c(6011L, 6013L, 16001L),
  aggregation_weight = c(1, 1, 1),
  stringsAsFactors = FALSE
)

simulation_config <- prepare_manual_simulation_config(
  config = cfg$base_config,
  manual_cfg = cfg$manual_config,
  run_file = run_file,
  trial_output = trial_output,
  verbose = FALSE
)

alias_file <- file.path(
  simulation_config$paths$lairice_root,
  simulation_config$model_outputs$observation_data_dir,
  "ObservData_crop.csv"
)

if (!file.exists(alias_file)) {
  stop("Expected state-level comparison to create a simulation observation alias")
}

alias_obs <- read.csv(alias_file, stringsAsFactors = FALSE)
expected_alias_ids <- c("6011", "6013", "16001")
if (!identical(sort(unique(as.character(alias_obs$siteID))), sort(expected_alias_ids))) {
  stop("Expected simulation observation alias to expand state NASS rows to county FIPS siteID values")
}

if (!all(c(2019L, 2020L, 2021L, 2022L, 2023L) %in% alias_obs$meas_year[alias_obs$siteID == 6011L])) {
  stop("Expected county 06011 to inherit California observation years")
}

alias_mtime <- file.info(alias_file)$mtime
alias_rows_first <- nrow(alias_obs)

simulation_config_reuse <- prepare_manual_simulation_config(
  config = cfg$base_config,
  manual_cfg = cfg$manual_config,
  run_file = run_file,
  trial_output = trial_output,
  verbose = FALSE
)

alias_obs_reuse <- read.csv(alias_file, stringsAsFactors = FALSE)
if (!identical(alias_obs_reuse, alias_obs)) {
  stop("Expected state-level simulation observation alias to be reused without modification")
}

if (!identical(file.info(alias_file)$mtime, alias_mtime)) {
  stop("Expected existing state-level simulation observation alias to be left untouched on reuse")
}

if (nrow(alias_obs_reuse) != alias_rows_first) {
  stop("Expected reused state-level simulation observation alias to keep the same row count")
}

comparison <- create_manual_comparison_dataset(
  config = cfg$base_config,
  manual_cfg = cfg$manual_config,
  weighted_results = weighted_results,
  helper_env = helper_env,
  verbose = FALSE
)

if (is.null(comparison) || nrow(comparison) != 2) {
  stop("Expected state-level dry bean comparison to match California and Idaho 2019 observations")
}

comparison <- comparison[order(as.integer(comparison$siteID)), , drop = FALSE]

if (!identical(as.character(comparison$siteID), c("6", "16"))) {
  stop("Expected comparison site IDs to be state FIPS 6 and 16")
}

expected_ca <- weighted.mean(c(100, 200), c(1, 3))
expected_id <- weighted.mean(c(90, 110), c(2, 2))

if (!isTRUE(all.equal(comparison$mod_cgrain[comparison$siteID == "6"], expected_ca))) {
  stop("California state modeled cgrain was not weighted by county total_weight")
}

if (!isTRUE(all.equal(comparison$mod_cgrain[comparison$siteID == "16"], expected_id))) {
  stop("Idaho state modeled cgrain was not weighted by county total_weight")
}

cat("Manual state-level comparison OK\n")
