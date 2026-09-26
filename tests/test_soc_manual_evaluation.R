#!/usr/bin/env Rscript

.libPaths(c("rlib", .libPaths()))

source("workflows/scripts/soil_organic_carbon_manual/soc_manual_evaluation_helpers.R")
source("workflows/scripts/soil_organic_carbon_manual/soc_manual_helpers.R")

tmp_root <- file.path(tempdir(), "soc_manual_evaluation_test")
if (dir.exists(tmp_root)) {
  unlink(tmp_root, recursive = TRUE)
}
dir.create(tmp_root, recursive = TRUE, showWarnings = FALSE)

write_fake_lis <- function(path, rows) {
  lines <- c(
    "fake metadata",
    "time somsc",
    "",
    apply(rows, 1, function(x) paste(x, collapse = " "))
  )
  writeLines(lines, path)
}

site_dir <- file.path(tmp_root, "sites", "demo_site")
dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

write_fake_lis(
  file.path(site_dir, "demo_eq.lis"),
  matrix(c(
    0, 90,
    1, 100,
    2, 110
  ), ncol = 2, byrow = TRUE)
)
write_fake_lis(
  file.path(site_dir, "demo_base.lis"),
  matrix(c(
    9, 115,
    10, 120,
    11, 130
  ), ncol = 2, byrow = TRUE)
)
write_fake_lis(
  file.path(site_dir, "demo_trt.lis"),
  matrix(c(
    2000, 140,
    2001, 150,
    2002, 175
  ), ncol = 2, byrow = TRUE)
)

obs_df <- data.frame(
  siteID = c("demo_site", "demo_site"),
  treatment_schedule = c("demo_trt.sch", "demo_trt.sch"),
  meas_year = c(2000, 2001),
  C_30cm_gm2 = c(140, 180),
  stringsAsFactors = FALSE
)

matched_df <- match_soc_manual_lis_to_obs(
  lis_df = read_soc_manual_lis_somsc(file.path(site_dir, "demo_trt.lis")),
  trt_obs = obs_df
)
stopifnot(nrow(matched_df) == 2L)
stopifnot(identical(as.numeric(matched_df$mod), c(150, 175)))

timeline_df <- build_soc_manual_treatment_timeline(
  site_dir = site_dir,
  equil_schedule = "demo_eq.sch",
  base_schedule = "demo_base.sch",
  treatment_schedule = "demo_trt.sch"
)
stopifnot(nrow(timeline_df) == 2L)
stopifnot(identical(unique(timeline_df$phase), "treatment"))
stopifnot(identical(timeline_df$plot_time, timeline_df$time))

bin_only_site_dir <- file.path(tmp_root, "bin_only_site")
dir.create(bin_only_site_dir, recursive = TRUE, showWarnings = FALSE)
file.create(file.path(bin_only_site_dir, "demo_trt.bin"))
writeLines("time somsc", file.path(bin_only_site_dir, "outvars.txt"))

generator_calls <- character(0)
fake_generator <- function(bin_file, lis_file, outvars_file, site_dir, ddlist_exe = NULL, verbose = TRUE) {
  generator_calls <<- c(generator_calls, paste(basename(bin_file), basename(lis_file), basename(outvars_file), basename(site_dir), sep = "|"))
  write_fake_lis(
    lis_file,
    matrix(c(
      2000, 140,
      2001, 150
    ), ncol = 2, byrow = TRUE)
  )
  lis_file
}

resolved_lis <- resolve_soc_manual_lis_file(
  site_dir = bin_only_site_dir,
  schedule_name = "demo_trt.sch",
  lis_generator = fake_generator,
  verbose = FALSE
)
stopifnot(file.exists(resolved_lis))
stopifnot(length(generator_calls) == 1L)
stopifnot(basename(resolved_lis) == "demo_trt.lis")

experiment_root <- file.path(tmp_root, "experiment")
dir.create(file.path(experiment_root, "sites"), recursive = TRUE, showWarnings = FALSE)

site_a_dir <- file.path(experiment_root, "sites", "site_a")
site_b_dir <- file.path(experiment_root, "sites", "site_b")
dir.create(site_a_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(site_b_dir, recursive = TRUE, showWarnings = FALSE)

file.create(file.path(site_a_dir, c("site_a_eq.sch", "site_a_base.sch", "site_a_trt.sch", "site_a.100")))
file.create(file.path(site_b_dir, c("site_b_eq.sch", "site_b_base.sch", "site_b_trt.sch", "site_b.100")))

write_fake_lis(
  file.path(site_a_dir, "site_a_eq.lis"),
  matrix(c(0, 10, 1, 11), ncol = 2, byrow = TRUE)
)
write_fake_lis(
  file.path(site_a_dir, "site_a_base.lis"),
  matrix(c(5, 12, 6, 13), ncol = 2, byrow = TRUE)
)
write_fake_lis(
  file.path(site_a_dir, "site_a_trt.lis"),
  matrix(c(2000, 20, 2001, 21), ncol = 2, byrow = TRUE)
)
write_fake_lis(
  file.path(site_b_dir, "site_b_eq.lis"),
  matrix(c(0, 30, 1, 31), ncol = 2, byrow = TRUE)
)
write_fake_lis(
  file.path(site_b_dir, "site_b_base.lis"),
  matrix(c(5, 32, 6, 33), ncol = 2, byrow = TRUE)
)
write_fake_lis(
  file.path(site_b_dir, "site_b_trt.lis"),
  matrix(c(2000, 40, 2001, 41), ncol = 2, byrow = TRUE)
)

write.csv(
  data.frame(
    siteID = "site_a",
    equil_schedule = "site_a_eq.sch",
    base_schedule = "site_a_base.sch",
    treatment_schedule = "site_a_trt.sch",
    stringsAsFactors = FALSE
  ),
  file.path(experiment_root, "run_table.csv"),
  row.names = FALSE
)

obs_path <- file.path(experiment_root, "obs.csv")
write.csv(
  data.frame(
    siteID = c("site_a", "site_b"),
    treatment_schedule = c("site_a_trt.sch", "site_b_trt.sch"),
    meas_year = c(2000, 2000),
    C_30cm_gm2 = c(21, 41),
    stringsAsFactors = FALSE
  ),
  obs_path,
  row.names = FALSE
)

all_sites_1to1 <- build_soc_manual_1to1_data(
  experiment_root = experiment_root,
  observation_file = obs_path,
  verbose = FALSE
)
stopifnot(setequal(unique(all_sites_1to1$siteID), c("site_a", "site_b")))

all_sites_timeline <- build_soc_manual_timeline_data(
  experiment_root = experiment_root,
  verbose = FALSE
)
stopifnot(setequal(unique(all_sites_timeline$siteID), c("site_a", "site_b")))

site_treatment_plot_df <- data.frame(
  siteID = c("site_a", "site_a", "site_b", "site_b"),
  treatment_schedule = c("trt_1.sch", "trt_1.sch", "trt_2.sch", "trt_2.sch"),
  mod = c(10, 11, 20, 21),
  obs = c(9, 12, 19, 22),
  stringsAsFactors = FALSE
)
site_treatment_plot <- plot_soc_manual_1to1_by_site_treatment(site_treatment_plot_df)
plot_grob <- ggplot2::ggplotGrob(site_treatment_plot)
stopifnot(inherits(plot_grob, "gtable"))

cat("PASS: soc manual evaluation helper tests\n")
