#!/usr/bin/env Rscript
# =======================================================================================
#  PURPOSE:   SIR Step 4.5 - Add hyperparameter (variance-component) Monte Carlo error
#             to the SOC posterior predictions for the independent, rS, and rSY models.
#
#             For each posterior SampleID (from the SIR resample), the fitted rSY/rS/ind
#             variance components are used to draw log-scale error terms that are added
#             to ln(mod + 1). Back-transforming gives a posterior predictive SOC value
#             per observation that reflects BOTH parameter uncertainty (across the 1000
#             SIR samples) AND the model error structure (site, site:year, residual).
#
#             Model error structure (all on the ln_resi = ln_obs - ln_mod scale):
#               - independent : ln_mod + eps                         (residual only)
#               - rS          : ln_mod + b_site + eps                (site + residual)
#               - rSY         : ln_mod + b_site + b_siteyr + eps     (site + site:year + residual)
#
#  AUTHOR:    Luqi Jiao Emanuele
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
# =======================================================================================

# Parse command line arguments first to get config path
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    config_path <- args[1] # Path to YAML config file
} else {
    # Default config path for interactive usage
    # config_path <- "workflows/configs/soil_organic_carbon.yaml"
    config_path <- "workflows/configs/soil_organic_carbon.yaml"
}

# Load YAML to get custom library path (minimal setup)
if (!require(yaml, quietly = TRUE)) {
    stop("yaml package not available. Please run step0_r_setup.R first.")
}
config <- yaml::read_yaml(config_path)

# Set custom library path (packages should already be installed by step0_r_setup.R)
if (!is.null(config$r_config$rlibpaths)) {
    .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
}

# Load required packages (assuming they are already installed)
suppressMessages({
    library(bayesiancalibr)
    library(dplyr)
    library(digest)
})
 
# Optional date stamp override from command line
if (length(args) > 1) {
    date_stamp <- args[2]
    cat("Running SIR Step 1 with command line arguments:\n")
    cat("Config file:", config_path, "\n")
    cat("Date stamp override:", date_stamp, "\n\n")
} else {
    # Use config default
    date_stamp <- NULL
    cat("Running SIR Step 1 with default configuration:\n")
    cat("Config file:", config_path, "\n")
    cat("Date stamp: using config default\n\n")
}

# Read YAML configuration using bayesiancalibr functions
config <- yaml::read_yaml(config_path)

# Print configuration summary
cat("Configuration loaded successfully:\n")
cat("  Project:", config$project$name, "\n")  


# =======================================================================================
# MODEL DEFINITION (follows the package, bayesiancalibr/R/likelihood_calculations.R)
# =======================================================================================
# The draws below reproduce the EXACT error structure fitted by calculate_gofs() on the
# ln-transformed residual ln_resi = ln(obs + 1) - ln(mod + 1). The package fits, on the
# log scale:
#
#   independent : ln_lmerFit not used; ln_ind_sigma = sqrt(mean(ln_resi^2))
#                 => ln_resi ~ N(0, ln_independent_sigma^2)                       [residual only]
#
#   rS          : lmer(ln_resi ~ -1 + (1|siteID))
#                 => ln_resi = b_site + eps
#                    b_site ~ N(0, ln_sigma_site_rS^2), eps ~ N(0, ln_sigma_Resi_rS^2)
#
#   rSY         : lmer(ln_resi ~ -1 + (1|siteID/SeasonID))
#                 => ln_resi = b_site + b_site:season + eps
#                    b_site        ~ N(0, ln_sigma_site_rSY^2)
#                    b_site:season ~ N(0, ln_sigma_siteyr_rSY^2)
#                    eps           ~ N(0, ln_sigma_Resi_rSY^2)
#
# Grouping keys (from gsa_step3_likelihood.R: siteID = SiteID, SeasonID = year):
#   - siteID          -> grouping variable "siteID"        (random site intercept)
#   - siteID:SeasonID -> grouping variable "siteID:year"   (nested site x year intercept)
#   - residual        -> one draw per observation row (site x treatment x year)
#
# Predictive back-transform: ln_mod_sim = ln_mod + b_site + b_site:year + eps
#                            mod_sim    = exp(ln_mod_sim) - 1
#
# NOTE: digest-based deterministic seeding is only a mechanism to make a single random
# effect identical across all rows sharing a grouping key; it does not change the model.
# =======================================================================================

# Deterministic seed from a character key. Identical keys always produce identical draws,
# which is how one random effect is shared across all rows in the same group.
# Keys should include an emission/project prefix so SOC vs crop_yield_corn_m2 etc.
# do not share the same random draws when site/SampleID strings overlap.
make_seed <- function(x) {
  h <- tolower(digest::digest(x))
  seed <- strtoi(substr(h, 1, 7), base = 16L)
  seed %% .Machine$integer.max
}

# One N(0, sigma) draw with a deterministic, key-based seed.
draw_effect <- function(sigma, key) {
  if (length(sigma) == 0 || is.na(sigma) || !is.finite(sigma) || sigma <= 0) {
    return(0)
  }
  set.seed(make_seed(key))
  rnorm(n = 1, mean = 0, sd = sigma)
}

# ln-scale variance-component column names, matching the package output of calculate_gofs().
sigma_cols_for_model <- function(model) {
  switch(model,
    "ind"    = list(site = NA_character_, siteyr = NA_character_, resi = "independent_sigma"),
    "ln_ind" = list(site = NA_character_, siteyr = NA_character_, resi = "ln_independent_sigma"),
    "rS"     = list(site = "ln_sigma_site_rS", siteyr = NA_character_, resi = "ln_sigma_Resi_rS"),
    "rSY"    = list(site = "ln_sigma_site_rSY", siteyr = "ln_sigma_siteyr_rSY", resi = "ln_sigma_Resi_rSY"),
    stop("Unknown model: ", model)
  )
}

# Add hyperparameter (variance-component) MC error to one model's posterior table.
#
#   annual_df     : posterior mod/obs table (SampleID x site x treatment x year)
#   lkhd_df       : combined likelihood table (one row per SampleID)
#   model         : "ind", "ln_ind", "rS", or "rSY"
#   emission_key  : project/emission prefix so seeds differ across workflows
#
# Returns annual_df with site_draw, siteyr_draw, resi_draw, ln_mod_sim, mod_sim.
#   - ind    : natural-scale residual  -> mod_sim = mod + draws
#   - others : ln-scale residual        -> ln_mod_sim = ln_mod + draws;
#                                        mod_sim = exp(ln_mod_sim) - 1
add_hyper_error_draws <- function(annual_df, lkhd_df, model, emission_key) {

  sc <- sigma_cols_for_model(model)
  use_ln_scale <- !identical(model, "ind")

  # Keep only the sigma columns we need, indexed by SampleID.
  keep_cols <- c("SampleID", stats::na.omit(unlist(sc)))
  sig_lookup <- lkhd_df[, keep_cols, drop = FALSE]

  annual_df$site_draw   <- 0
  annual_df$siteyr_draw <- 0
  annual_df$resi_draw   <- 0

  sample_ids <- unique(annual_df$SampleID)

  for (sid in sample_ids) {
    rows <- which(annual_df$SampleID == sid)

    sig_row <- sig_lookup[sig_lookup$SampleID == sid, , drop = FALSE]
    if (nrow(sig_row) == 0) next

    sigma_site   <- if (!is.na(sc$site))   sig_row[[sc$site]][1]   else NA
    sigma_siteyr <- if (!is.na(sc$siteyr)) sig_row[[sc$siteyr]][1] else NA
    sigma_resi   <- if (!is.na(sc$resi))   sig_row[[sc$resi]][1]   else NA

    for (k in rows) {
      site_id <- annual_df$siteID[k]
      trt_id  <- annual_df$treatment_schedule[k]
      site_yr <- annual_df$meas_year[k]
      mod_val <- if (use_ln_scale) annual_df$ln_mod[k] else annual_df$mod[k]

      # Site effect: shared across all observations at the same site (rS, rSY).
      if (!is.na(sc$site)) {
        annual_df$site_draw[k] <- draw_effect(
          sigma_site,
          paste(emission_key, model, "site", site_id, sid, sep = "|")
        )
      }

      # Site:year effect: shared across observations at the same site-year (rSY).
      if (!is.na(sc$siteyr)) {
        annual_df$siteyr_draw[k] <- draw_effect(
          sigma_siteyr,
          paste(emission_key, model, "siteyr", site_id, site_yr, sid, sep = "|")
        )
      }

      # Residual effect: unique per observation.
      annual_df$resi_draw[k] <- draw_effect(
        sigma_resi,
        paste(emission_key, model, "resi", site_id, trt_id, site_yr, sid, mod_val, k, sep = "|")
      )
    }
    cat(".")
  }
  cat("\n")

  if (use_ln_scale) {
    annual_df$ln_mod_sim <- annual_df$ln_mod + annual_df$site_draw +
      annual_df$siteyr_draw + annual_df$resi_draw
    annual_df$mod_sim <- exp(annual_df$ln_mod_sim) - 1
  } else {
    # Natural-scale independent model (crop ind)
    annual_df$mod_sim <- annual_df$mod + annual_df$site_draw +
      annual_df$siteyr_draw + annual_df$resi_draw
    if ("ln_mod" %in% names(annual_df)) {
      annual_df$ln_mod_sim <- log(pmax(annual_df$mod_sim + 1, .Machine$double.eps))
    }
  }

  annual_df
}

# Aggregate posterior draws to per-observation mean + 95% CIs.
#
# Two nested intervals (matching the black/grey bars in measured-vs-modeled plots):
#   - parameter-only (black): quantiles of `mod` across SIR SampleIDs
#   - posterior predictive (grey): quantiles of `mod_sim` (parameter + hyperparameter error)
summarize_posterior_predictive <- function(pred_df) {
  pred_df %>%
    group_by(siteID, treatment_schedule, meas_year) %>%
    summarise(
      obs_mean = mean(obs, na.rm = TRUE),

      # Black bars: parameter uncertainty only (SIR posterior of mod)
      mod_param_mean = mean(mod, na.rm = TRUE),
      mod_param_lpi  = quantile(mod, 0.025, na.rm = TRUE),
      mod_param_upi  = quantile(mod, 0.975, na.rm = TRUE),

      # Grey bars: posterior predictive (mod + hyperparameter MC draws)
      mod_mean = mean(mod_sim, na.rm = TRUE),
      mod_lpi  = quantile(mod_sim, 0.025, na.rm = TRUE),
      mod_upi  = quantile(mod_sim, 0.975, na.rm = TRUE),

      ln_mod_param_mean = mean(ln_mod, na.rm = TRUE),
      ln_mod_param_lpi  = quantile(ln_mod, 0.025, na.rm = TRUE),
      ln_mod_param_upi  = quantile(ln_mod, 0.975, na.rm = TRUE),
      ln_mod_mean = mean(ln_mod_sim, na.rm = TRUE),
      ln_mod_lpi  = quantile(ln_mod_sim, 0.025, na.rm = TRUE),
      ln_mod_upi  = quantile(ln_mod_sim, 0.975, na.rm = TRUE),

      n_draws = sum(is.finite(mod_sim)),
      .groups = "drop"
    ) %>%
    mutate(
      covers_1to1_param = is.finite(mod_param_lpi) & is.finite(mod_param_upi) &
        is.finite(obs_mean) & obs_mean >= mod_param_lpi & obs_mean <= mod_param_upi,
      covers_1to1 = is.finite(mod_lpi) & is.finite(mod_upi) & is.finite(obs_mean) &
        obs_mean >= mod_lpi & obs_mean <= mod_upi
    )
}

# =======================================================================================
# DATA
# =======================================================================================
results_dir <- file.path(
  config$paths$lairice_root, config$paths$output_base,
  config$project$date_stamp, "SIR", "Results"
)

## Combined likelihood table (variance components per SampleID)
likelihood_file <- file.path(results_dir, "Likelihood_Outputs_soboljansen.rds")
likelihood <- readRDS(likelihood_file)
likelihood <- likelihood[likelihood$Variable == config$likelihood_calculation$variables[[1]]$name, ]

## Posterior SOC mod/obs tables per model (written by SIR Step 4C)
## Which models to process comes from config$likelihood_model (e.g. ["rSY"]).
likelihood_models <- unlist(config$likelihood_calculation$variables[[1]]$model, use.names = FALSE)
if (is.null(likelihood_models) || length(likelihood_models) < 1) {
  stop("Config is missing likelihood_model. Example:\n",
       "likelihood_model:\n  - \"rSY\"")
}
likelihood_models <- as.character(likelihood_models)


emission_name <- as.character(config$emmission_variable)[1]

var_name <- config$combine_tables_prepare_plots$likelihood_vars[[1]]$prefix

model_files <- setNames(
  as.list(paste0(emission_name, "_with_", var_name, "_", likelihood_models, ".csv")),
  likelihood_models
)
cat("  Likelihood models:", paste(likelihood_models, collapse = ", "), "\n")
cat("  Model files:\n")
for (m in names(model_files)) {
  cat("   ", m, "->", model_files[[m]], "\n")
}

# Unique seed namespace per emission/project (e.g. SOC::soil_organic_carbon)
emission_seed_key <- paste(
  as.character(config$emmission_variable)[1],
  as.character(config$project$name)[1],
  sep = "::"
)
cat("  Emission seed key:", emission_seed_key, "\n")

 


# =======================================================================================
# PROCESS: hyperparameter MC draws for each model
# =======================================================================================
for (model in names(model_files)) {

  cat("\n=========================================================================\n")
  cat("Model:", model, "- adding hyperparameter MC error draws\n")

  soc_file <- file.path(results_dir, model_files[[model]])
  if (!file.exists(soc_file)) {
    cat("  Skipping (file not found):", soc_file, "\n")
    next
  }

  annual_df <- read.csv(soc_file, stringsAsFactors = FALSE)

  # Restrict the likelihood lookup to the samples present in this model's table.
  lkhd_model <- likelihood[likelihood$SampleID %in% unique(annual_df$SampleID), ]

  pred_df <- add_hyper_error_draws(
    annual_df    = annual_df,
    lkhd_df      = lkhd_model,
    model        = model,
    emission_key = emission_seed_key
  )

  summary_df <- summarize_posterior_predictive(pred_df)

  # Save the full posterior predictive draws and the per-observation summary.
  pred_out    <- file.path(results_dir, paste0(config$emmission_variable, "_posterior_predictive_", model, ".csv"))
  summary_out <- file.path(results_dir, paste0(config$emmission_variable, "_posterior_predictive_summary_", model, ".csv"))

  write.csv(pred_df, pred_out, row.names = FALSE)
  write.csv(summary_df, summary_out, row.names = FALSE)

  coverage_param_pct <- 100 * mean(summary_df$covers_1to1_param, na.rm = TRUE)
  coverage_pred_pct  <- 100 * mean(summary_df$covers_1to1, na.rm = TRUE)
  cat(sprintf("  Draws written: %s\n", pred_out))
  cat(sprintf("  Summary written: %s\n", summary_out))
  cat(sprintf("  1:1 coverage parameter-only (black): %.1f%% of %d observations\n",
              coverage_param_pct, nrow(summary_df)))
  cat(sprintf("  1:1 coverage posterior predictive (grey): %.1f%% of %d observations\n",
              coverage_pred_pct, nrow(summary_df)))

  best_param_file <- file.path(results_dir, paste0("best_", config$sir$sir_n2dir, "_", config$combine_tables_prepare_plots$likelihood_vars[[1]]$prefix, "_", model, ".csv"))
  mc_draw_file <- file.path(config$paths$lairice_root, config$paths$output_base, config$project$date_stamp, "SIR", paste0("mc_SIR_draw.rds"))

  best_param_df <- read.csv(best_param_file, stringsAsFactors = FALSE)
  colnames(best_param_df) <- "SampleID"
  mc_draw_df <- readRDS(mc_draw_file)

  best_param_set <- mc_draw_df %>%
    filter(SampleID %in% best_param_df$SampleID) %>%
    select(-JobGroup)
  
  write.csv(best_param_set, file.path(results_dir, paste0("best_param_set_", model, ".csv")), row.names = FALSE)
  cat(sprintf("  Best parameter set written: %s\n", best_param_file))
}

cat("\nSIR Step 4.5 hyperparameter uncertainty complete.\n")

