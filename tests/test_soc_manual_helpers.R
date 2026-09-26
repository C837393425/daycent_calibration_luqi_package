#!/usr/bin/env Rscript

helper_path <- "workflows/scripts/soil_organic_carbon_manual/soc_manual_helpers.R"
source(helper_path)

tmp_root <- file.path(tempdir(), "soc_manual_helper_test")
if (dir.exists(tmp_root)) {
  unlink(tmp_root, recursive = TRUE)
}
dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)

site_dir <- file.path(tmp_root, "testsite")
dir.create(site_dir)
file.create(file.path(site_dir, c(
  "testsite_eq.sch",
  "testsite_base.sch",
  "testsite_trt1.sch",
  "testsite_trt2.sch",
  "testsite.100"
)))

run_table <- build_soc_site_run_table(site_dir)
stopifnot(nrow(run_table) == 2L)
stopifnot(all(run_table$equil_schedule == "testsite_eq.sch"))
stopifnot(all(run_table$base_schedule == "testsite_base.sch"))
stopifnot(setequal(run_table$treatment_schedule, c("testsite_trt1.sch", "testsite_trt2.sch")))

cfg_true <- list(outputs = list(keep_scratch_dirs = TRUE))
cfg_false <- list(outputs = list())
stopifnot(isTRUE(get_keep_scratch_dirs(cfg_true)))
stopifnot(isFALSE(get_keep_scratch_dirs(cfg_false)))

param_csv <- file.path(tmp_root, "params.csv")
write.csv(
  data.frame(
    File = c("fix.100", "cult.100"),
    Parameter = c("DEC_4", "K_CLTEFF"),
    value = c(0.003, 8.1)
  ),
  param_csv,
  row.names = FALSE
)

params <- read_soc_manual_parameter_table(param_csv)
stopifnot(nrow(params) == 2L)
stopifnot("K_CLTEFF" %in% params$Parameter)

resolved_site <- find_soc_site_file(
  site_dir = site_dir,
  site_id = "testsite",
  base_schedule = "testsite_base.sch",
  equil_schedule = "testsite_eq.sch"
)
stopifnot(basename(resolved_site) == "testsite.100")

alt_site_dir <- file.path(tmp_root, "lethbridgeABC")
dir.create(alt_site_dir)
file.create(file.path(alt_site_dir, c(
  "lethbridge_abc_eq.sch",
  "lethbridge_abc_base.sch",
  "lethbridge_abc_ppp_0n.sch",
  "lethbridge_abc.100"
)))

alt_run_table <- build_soc_site_run_table(alt_site_dir)
stopifnot(nrow(alt_run_table) == 1L)
resolved_alt_site <- find_soc_site_file(
  site_dir = alt_site_dir,
  site_id = "lethbridgeABC",
  base_schedule = "lethbridge_abc_base.sch",
  equil_schedule = "lethbridge_abc_eq.sch"
)
stopifnot(basename(resolved_alt_site) == "lethbridge_abc.100")

nobase_site_dir <- file.path(tmp_root, "nobase_site")
dir.create(nobase_site_dir)
file.create(file.path(nobase_site_dir, c(
  "nobase_eq.sch",
  "nobase_trt.sch",
  "nobase.100"
)))

nobase_run_table <- build_soc_site_run_table(nobase_site_dir)
stopifnot(nrow(nobase_run_table) == 1L)
stopifnot(identical(nobase_run_table$base_schedule, ""))

multibase_site_dir <- file.path(tmp_root, "swiftcurrent")
dir.create(multibase_site_dir)
file.create(file.path(multibase_site_dir, c(
  "swiftcurrent_eq.sch",
  "swiftcurrent_base.sch",
  "swiftcurrent_native_sod_base.sch",
  "swiftcurrent_ppp_np.sch",
  "swiftcurrent_native_sod.sch",
  "swiftcurrent.100"
)))

multibase_run_table <- build_soc_site_run_table(multibase_site_dir)
stopifnot(nrow(multibase_run_table) == 2L)
stopifnot(
  multibase_run_table$base_schedule[multibase_run_table$treatment_schedule == "swiftcurrent_ppp_np.sch"] ==
    "swiftcurrent_base.sch"
)
stopifnot(
  multibase_run_table$base_schedule[multibase_run_table$treatment_schedule == "swiftcurrent_native_sod.sch"] ==
    "swiftcurrent_native_sod_base.sch"
)

output_paths <- prepare_soc_manual_output_paths(
  project_root = tmp_root,
  experiment_name = "example_run",
  date_stamp = "29Jun2026"
)
stopifnot(dir.exists(output_paths$experiment_root))

stopifnot(file.exists("workflows/scripts/soil_organic_carbon_manual/run_soc_manual_single.R"))
stopifnot(file.exists("workflows/scripts/soil_organic_carbon_manual/run_soc_manual_batch.R"))
stopifnot(file.exists("workflows/scripts/soil_organic_carbon_manual/soc_manual_config_example.yaml"))

cat("PASS: soc manual helper tests\n")
