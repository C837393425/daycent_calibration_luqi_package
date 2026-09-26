#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript rebuild_hay_g3_trial_outputs_from_db.R <trial_id>")
}

trial_id <- args[[1]]
repo <- "/data/rubelscratch/rubelogle/daycent_calibration"
config_path <- file.path(repo, "workflows/scripts/ManualCalibration/manual_calibration_hay_g3_cluster.yaml")

setwd(repo)
.libPaths(c(file.path(repo, "rlib"), .libPaths()))

source(file.path(repo, "workflows/scripts/ManualCalibration/manual_calibration_helpers.R"))
helper_env <- new.env(parent = globalenv())
sys.source(
  file.path(repo, "workflows/scripts/crop_yield_all/Validation/validation_helpers.R"),
  envir = helper_env
)

cfg_bundle <- read_manual_calibration_config(config_path, helper_env = helper_env, verbose = FALSE)
manual_cfg <- cfg_bundle$manual_config
config <- cfg_bundle$base_config

trials_path <- resolve_manual_path(
  manual_cfg$manual_calibration$trials_file,
  cfg_bundle$config_dir,
  must_work = TRUE
)
trials_df <- read.csv(trials_path, stringsAsFactors = FALSE)
trial_row <- trials_df[trials_df$trial_id == trial_id, , drop = FALSE]
if (nrow(trial_row) != 1) {
  stop("Trial not found: ", trial_id)
}

trial_output <- build_trial_output_paths(config, manual_cfg, trial_row)
table_info <- resolve_manual_table_names(config, manual_cfg, trial_row)
run_file_path <- trial_output$runfile_csv
if (!file.exists(run_file_path)) {
  stop("Runfile not found: ", run_file_path)
}
run_file <- read.csv(run_file_path, stringsAsFactors = FALSE)

annual_results <- load_manual_annual_results_from_database(config, table_info, partition_count = 1L)
cat("Loaded annual rows:", nrow(annual_results), "from", table_info$annual_table_base, "\n")

context <- list(
  config = config,
  manual_cfg = manual_cfg,
  trial_row = trial_row,
  trial_output = trial_output,
  run_file = run_file,
  helper_env = helper_env
)

backup_suffix <- "_prefilter"
for (path in c(trial_output$weighted_csv, trial_output$comparison_csv, trial_output$metrics_csv)) {
  if (file.exists(path)) {
    file.copy(path, paste0(path, backup_suffix), overwrite = TRUE)
  }
}

finalized <- finalize_manual_trial_outputs(context, annual_results, verbose = TRUE)

write.csv(finalized$metrics_df, trial_output$metrics_csv, row.names = FALSE)
write.csv(finalized$comparison_results, trial_output$comparison_csv, row.names = FALSE)
write.csv(finalized$weighted_results, trial_output$weighted_csv, row.names = FALSE)

create_scatter_plot(
  comparison_results = finalized$comparison_results,
  metrics_df = finalized$metrics_df,
  plot_path = file.path(trial_output$output_dir, "modeled_vs_observed_1to1.png"),
  target_variable = manual_cfg$manual_calibration$comparison$target_variable,
  comparison_cfg = manual_cfg$manual_calibration$comparison
)

cat("\n=== New metrics ===\n")
print(finalized$metrics_df[, c("trial_id", "matched_rows", "r2", "rmse", "bias")])

metrics_backup <- paste0(trial_output$metrics_csv, backup_suffix)
if (file.exists(metrics_backup)) {
  old_metrics <- read.csv(metrics_backup, stringsAsFactors = FALSE)
  cat("\n=== Previous metrics ===\n")
  print(old_metrics[, c("trial_id", "matched_rows", "r2", "rmse", "bias")])
}

if (!is.null(finalized$weighted_results) && nrow(finalized$weighted_results) > 0) {
  cat("\nWeighted crop_filtered:", paste(unique(finalized$weighted_results$crop_filtered), collapse = ", "), "\n")
  obs_years <- 2011:2014
  sub <- finalized$weighted_results[
    finalized$weighted_results$year %in% obs_years,
    c("aggregation_level", "year", "n_sites", "weighted_value", "crop_filtered")
  ]
  cat("Sample county-years (2011-2014):\n")
  print(head(sub, 8))
}

weighted_backup <- paste0(trial_output$weighted_csv, backup_suffix)
if (file.exists(weighted_backup) && file.exists(trial_output$weighted_csv)) {
  old_w <- read.csv(weighted_backup, stringsAsFactors = FALSE)
  new_w <- read.csv(trial_output$weighted_csv, stringsAsFactors = FALSE)
  merge_cols <- c("aggregation_level", "year", "variable_name")
  cmp <- merge(
    old_w[, c(merge_cols, "weighted_value", "n_sites", "crop_filtered")],
    new_w[, c(merge_cols, "weighted_value", "n_sites", "crop_filtered")],
    by = merge_cols,
    suffixes = c("_old", "_new")
  )
  cmp$value_delta <- cmp$weighted_value_new - cmp$weighted_value_old
  cmp$n_sites_delta <- cmp$n_sites_new - cmp$n_sites_old
  cat("\n=== Weighted comparison (obs years 2011-2014) ===\n")
  obs_cmp <- cmp[cmp$year %in% 2011:2014, ]
  cat(
    "Mean modeled value change:",
    round(mean(obs_cmp$value_delta, na.rm = TRUE), 2), "g C/m2\n"
  )
  cat(
    "Mean n_sites change:",
    round(mean(obs_cmp$n_sites_delta, na.rm = TRUE), 1), "\n"
  )
  cat("Max abs value change:", round(max(abs(obs_cmp$value_delta), na.rm = TRUE), 2), "\n")
}

cat("\nWrote refreshed outputs to:", trial_output$output_dir, "\n")
