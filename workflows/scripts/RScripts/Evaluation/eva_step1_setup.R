#!/usr/bin/env Rscript
# =======================================================================================
#  PURPOSE:   Evaluation Step 1 setup
#
#             Creates the Evaluation RunFile, posterior-draw table, and either
#             filesystem output folders or database result tables so Step 2 can
#             be submitted as an sbatch array. Does not run DayCent.
#
#  Usage:     Rscript eva_step1_setup.R <config_path> [date_stamp]
# =======================================================================================
create_table <- TRUE

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    config_path <- args[1]
} else {
    config_path <- "workflows/configs/soil_organic_carbon.yaml"
}

if (!require(yaml, quietly = TRUE)) {
    stop("yaml package not available. Please run step0_r_setup.R first.")
}
config <- yaml::read_yaml(config_path)

if (!is.null(config$r_config$rlibpaths)) {
    .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
}

suppressMessages({
    library(bayesiancalibr)
})

if (length(args) > 1) {
    date_stamp <- args[2]
    cat("Running Evaluation Step 1 with command line arguments:\n")
    cat("Config file:", config_path, "\n")
    cat("Date stamp override:", date_stamp, "\n\n")
} else {
    date_stamp <- NULL
    cat("Running Evaluation Step 1 with default configuration:\n")
    cat("Config file:", config_path, "\n")
    cat("Date stamp: using config default\n\n")
}

# Remap Evaluation schedule + .100 dirs onto the paths DayCent helpers read.
bind_evaluation_data_paths <- function(config) {
  pick_abs <- function(...) {
    for (p in list(...)) {
      if (!is.null(p) && is.character(p) && length(p) >= 1 && nzchar(p[[1]])) {
        p <- p[[1]]
        if (!grepl("^/", p) && !grepl("^[A-Za-z]:[/\\\\]", p)) {
          p <- file.path(config$paths$lairice_root, p)
        }
        return(p)
      }
    }
    NULL
  }
  eva <- config$evaluation
  if (is.null(eva)) eva <- list()

  sites_dir <- pick_abs(eva$evasites_dir, eva$expsites_dir, config$paths$evasites_dir)
  if (!is.null(sites_dir)) {
    config$paths$expsites_dir <- sites_dir
    config$paths$evasites_dir <- sites_dir
    cat("Using evaluation schedule directory:", sites_dir, "\n")
  }

  dot100_dir <- pick_abs(eva$evadot100_path, eva$dot100_path, config$paths$evadot100_path)
  if (!is.null(dot100_dir)) {
    config$paths$dot100_path <- dot100_dir
    config$paths$evadot100_path <- dot100_dir
    cat("Using evaluation dot100 directory:", dot100_dir, "\n")
  }
  config
}

tryCatch(
    {
        config <- read_yaml_config(config_path)
        config <- resolve_config_paths(config)
        config <- bind_evaluation_data_paths(config)

        cat("Configuration loaded successfully:\n")
        cat("  Project:", config$project$name, "\n")
        cat("  Date:", ifelse(is.null(date_stamp), config$project$date_stamp, date_stamp), "\n")
        cat("  Root directory:", config$paths$lairice_root, "\n")
        cat("  Evaluation schedule dir:", config$paths$expsites_dir, "\n")
        cat("  Evaluation dot100 dir:", config$paths$dot100_path, "\n")
        cat("  File source mode:", config$file_source$mode, "\n")
        cat("  Result run type:", eva_result_run_type(config), "\n")
        if (!is.null(config$evaluation$posterior$model)) {
            cat("  Posterior model:", config$evaluation$posterior$model, "\n")
        }
        if (!is.null(config$daycent$mode)) {
            cat("  DayCent mode:", config$daycent$mode, "\n")
        }
        cat("\n")

        cat("Starting Evaluation Step 1 setup...\n")
        result <- eva_step1_setup(
            config = config,
            date_stamp = date_stamp,
            create_table = create_table,
            verbose = TRUE
        )

        cat("\nGenerated files:\n")
        cat("  Posterior draws:", file.path(result$eva_method_dir, "mc_EVA_draw.rds"), "\n")
        cat("  Run File:", file.path(result$eva_method_dir, "RunFile.rds"), "\n")
        cat("  Runtime Info:", file.path(result$eva_method_dir, "RunTime_EVA_setup.csv"), "\n")
        if (identical(result$run_type, "database")) {
            if (length(result$created_tables) > 0) {
                cat("  Database tables:", paste(result$created_tables, collapse = ", "), "\n")
            } else {
                cat("  Database tables: none created (create_table = FALSE or names missing)\n")
            }
        } else {
            cat("  Filesystem job-group folders under:", result$eva_method_dir, "\n")
        }

        cat("\nNext steps:\n")
        cat("  1. Submit Evaluation Step 2 as an array (one sim_id per posterior row):\n")
        cat("       Rscript workflows/scripts/RScripts/Evaluation/eva_step2_simulate.R \\\n")
        cat("         ", config_path, " <sim_id>\n", sep = "")
        cat("     Array range: 1-", result$n_samples, "\n", sep = "")
    },
    error = function(e) {
        cat("ERROR:", e$message, "\n")
        quit(status = 1)
    }
)

cat("\nEvaluation Step 1 setup completed successfully!\n")
