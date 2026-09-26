#!/usr/bin/env Rscript

canonical_root <- "/data/rubelscratch/rubelogle/daycent_calibration"
canonical_rlib <- file.path(canonical_root, "rlib")
if (dir.exists(canonical_rlib)) {
  .libPaths(c(canonical_rlib, .libPaths()))
}

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("yaml package is required for historical scaling scaffold test")
}

helper_path <- file.path(
  canonical_root,
  "workflows/scripts/HistoricalScaling/historical_scaling_helpers.R"
)
source(helper_path)

corn_cfg_path <- file.path(
  canonical_root,
  "workflows/scripts/HistoricalScaling/configs/corn_all_mg.yaml"
)
batch_cfg <- read_historical_scaling_batch_config(corn_cfg_path, verbose = FALSE)
hs <- batch_cfg$historical_scaling

if (!identical(hs$crop_family, "corn")) {
  stop("Expected corn crop_family in corn_all_mg config")
}

if (length(hs$maturity_groups) != 4) {
  stop("Expected four enabled corn maturity groups (M2-M5)")
}

mg2 <- hs$maturity_groups[[1]]
if (!identical(mg2$mg_id, "corn_m2")) {
  stop("Expected first corn maturity group to be corn_m2")
}

if (!identical(mg2$reference_crop, "CM2_11")) {
  stop("Expected corn_m2 reference crop CM2_11")
}

mg2_targets <- build_mg_target_crop_names(mg2)
expected_mg2_targets <- c("CM2_11", "CM2_81", "CM2_91", "CM2_01")
if (!identical(sort(mg2_targets), sort(expected_mg2_targets))) {
  stop(
    "Expected corn-only target crops for corn_m2: ",
    paste(expected_mg2_targets, collapse = ", "),
    "; got: ",
    paste(mg2_targets, collapse = ", ")
  )
}
if ("G4" %in% mg2_targets) {
  stop("Expected G4 to be excluded from historical scaling target crops")
}

if (!identical(mg2$database_tables$run_order, "run_order_corn_m2_all_period")) {
  stop("Expected all-period run_order table for corn_m2")
}

scalar_df <- read_scalar_table(batch_cfg)
ref_scalar <- get_scalar_value(scalar_df, hs$scalar$column, hs$scalar$reference_year)
scaled_2001 <- compute_historical_ruetb(6.08, get_scalar_value(scalar_df, hs$scalar$column, 2001), ref_scalar)
if (abs(scaled_2001 - 5.55) > 0.01) {
  stop("Expected CM2_01 scaled RUETB near 5.55, got ", scaled_2001)
}

batch_paths <- build_batch_result_root(batch_cfg)
if (!dir.exists(batch_paths$result_root)) {
  stop("Expected batch result root to be created")
}

m6_cfg_path <- file.path(
  canonical_root,
  "workflows/scripts/HistoricalScaling/configs/corn_m6_default_shared_scalars.yaml"
)
if (!file.exists(m6_cfg_path)) {
  stop("Expected corn m6 historical scaling config: ", m6_cfg_path)
}

m6_batch_cfg <- read_historical_scaling_batch_config(m6_cfg_path, verbose = FALSE)
m6_hs <- m6_batch_cfg$historical_scaling
if (!identical(m6_hs$crop_family, "corn")) {
  stop("Expected corn crop_family in corn_m6_default_shared_scalars config")
}
if (!identical(m6_hs$outputs$experiment_name, "m6_default_shared_scalars")) {
  stop("Expected m6_default_shared_scalars experiment name")
}
if (length(m6_hs$maturity_groups) != 1) {
  stop("Expected one enabled corn m6 maturity group")
}

mg6 <- m6_hs$maturity_groups[[1]]
if (!identical(mg6$mg_id, "corn_m6")) {
  stop("Expected m6 maturity group to be corn_m6")
}
if (!identical(mg6$map_column, "MG6")) {
  stop("Expected corn_m6 map column MG6")
}
if (!identical(mg6$reference_crop, "CM6_11")) {
  stop("Expected corn_m6 reference crop CM6_11")
}
mg6_targets <- build_mg_target_crop_names(mg6)
expected_mg6_targets <- c("CM6_11", "CM6_81", "CM6_91", "CM6_01")
if (!identical(sort(mg6_targets), sort(expected_mg6_targets))) {
  stop(
    "Expected corn-only target crops for corn_m6: ",
    paste(expected_mg6_targets, collapse = ", "),
    "; got: ",
    paste(mg6_targets, collapse = ", ")
  )
}

m6_scalar_df <- read_scalar_table(m6_batch_cfg)
m6_ref_scalar <- get_scalar_value(m6_scalar_df, m6_hs$scalar$column, m6_hs$scalar$reference_year)
m6_scaled_2001 <- compute_historical_ruetb(4.0245, get_scalar_value(m6_scalar_df, m6_hs$scalar$column, 2001), m6_ref_scalar)
if (abs(m6_scaled_2001 - 3.67) > 0.01) {
  stop("Expected CM6_01 unified-scalar RUETB near 3.67, got ", m6_scaled_2001)
}

fit_script <- readLines(file.path(canonical_root, "workflows/scripts/HistoricalScaling/fit_mg_decade_scalars.R"), warn = FALSE)
refine_script <- readLines(file.path(canonical_root, "workflows/scripts/HistoricalScaling/fit_mg_decade_scalars_refine.R"), warn = FALSE)
if (!any(grepl("corn_m6\\s*=\\s*\"MG6\"", fit_script))) {
  stop("Expected fit_mg_decade_scalars.R to support corn_m6 = \"MG6\"")
}
if (!any(grepl("corn_m6\\s*=\\s*\"MG6\"", refine_script))) {
  stop("Expected fit_mg_decade_scalars_refine.R to support corn_m6 = \"MG6\"")
}

helper_lines <- readLines(file.path(canonical_root, "workflows/scripts/HistoricalScaling/historical_scaling_helpers.R"), warn = FALSE)
if (!any(grepl("paste0\\(\"#SBATCH --account=\"", helper_lines, fixed = FALSE))) {
  stop("Expected historical scaling SLURM helper to write account as #SBATCH --account")
}

tmp_root <- tempfile("corn_m6_historical_scaling_")
dir.create(file.path(tmp_root, "trial", "maturity_groups", "corn_m6", "default_run"), recursive = TRUE)
dir.create(file.path(tmp_root, "trial", "combined", "figures_by_decade"), recursive = TRUE)

comparison_df <- data.frame(
  year = c(1981, 1991, 2001, 2011),
  mod_cgrain = c(100, 100, 100, 100),
  nass_cgrain = c(110, 100, 90, 100)
)
utils::write.csv(
  comparison_df,
  file.path(tmp_root, "trial", "maturity_groups", "corn_m6", "default_run", "comparison_cgrain.csv"),
  row.names = FALSE
)

metrics_df <- data.frame(
  label = c("81-90", "91-00", "01-10", "10-23"),
  matched_rows = c(1, 1, 1, 1),
  r2 = c(NA_real_, NA_real_, NA_real_, NA_real_),
  rmse = c(NA_real_, NA_real_, NA_real_, NA_real_),
  bias = c(25, 0, -25, 0),
  status = "success",
  mg_id = "corn_m6",
  decade = c("81-90", "91-00", "01-10", "10-23")
)
utils::write.csv(
  metrics_df,
  file.path(tmp_root, "trial", "combined", "figures_by_decade", "metrics_per_mg_decade.csv"),
  row.names = FALSE
)

m6_scalar_only <- data.frame(
  Year_start = c(1981, 1991, 2001, 2011),
  MG6 = c(0.93, 1.04, 1.15, 1.26)
)
m6_scalar_path <- file.path(tmp_root, "m6_scalar_only.csv")
utils::write.csv(m6_scalar_only, m6_scalar_path, row.names = FALSE)

m6_refined_path <- file.path(tmp_root, "m6_scalar_refined.csv")
m6_refine_audit_path <- file.path(tmp_root, "m6_scalar_refine_audit.csv")
m6_refine_cmd <- c(
  file.path(canonical_root, "workflows/scripts/HistoricalScaling/fit_mg_decade_scalars_refine.R"),
  file.path(tmp_root, "trial"),
  m6_scalar_path,
  m6_refined_path,
  "15",
  file.path(canonical_root, "data/corn_maturity_group_MAP_parameters.csv"),
  "7",
  m6_refine_audit_path,
  "corn_m6"
)
m6_refine_output <- system2("Rscript", m6_refine_cmd, stdout = TRUE, stderr = TRUE)
m6_refine_status <- attr(m6_refine_output, "status") %||% 0L
if (!identical(as.integer(m6_refine_status), 0L)) {
  stop(
    "Expected m6-only scalar refinement to succeed; status ",
    m6_refine_status,
    "\n",
    paste(m6_refine_output, collapse = "\n")
  )
}
m6_refined <- utils::read.csv(m6_refined_path, check.names = FALSE)
if (!identical(names(m6_refined), c("Year_start", "MG6"))) {
  stop("Expected m6-only refinement output to contain Year_start and MG6 columns")
}

cat("Historical scaling scaffold checks passed\n")
