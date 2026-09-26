#=======================================================================================
# SIR pipeline scripts — summary (workflows/scripts/RScripts/SIR)
#
# Core logic lives in the bayesiancalibr package; scripts here are CLI wrappers.
# Typical order: Step 0 (from GSA folder) → 1 → 2 (simulate [+ QC])
#                → 2 aggregate (crop scaling only) → 3 (likelihood [+ QC])
#                → 4 (combine GOF + posterior MC) → 4.5 (uncertainty; often SOC).
# Config: YAML under workflows/configs/ (crop, SOC, NH3, etc.).
# Note: environment setup usually uses GSA/gsa_step0_r_setup.R (shared rlib).
#=======================================================================================

# --------------------------------------------------------------------------------------
# sir_step1_setup.R
# --------------------------------------------------------------------------------------
# Creates SIR Monte Carlo parameter draws (not Sobol-expanded like GSA).
# - Subsets parameters from priors via config$sir$sir_parameters
# - Draws nsim samples (config$sir$nsim), assigns SampleID / JobGroup
# - Writes mc_SIR_draw.rds, creates SIR output dirs (Annual_Outputs, Run_Status, ...)
# - Optional crop scaling: point_assignments.rds; may create partitioned DB result tables
# Usage: Rscript sir_step1_setup.R <config_path>

# --------------------------------------------------------------------------------------
# sir_step2_simulate.R
# --------------------------------------------------------------------------------------
# Runs DayCent for ONE SIR parameter set (one sim_id / SampleID).
# - Loads mc_SIR_draw.rds, runs sites (filesystem or database input modes)
# - Optional --partition-id for scaling mode (subset of sites per partition)
# - Writes annual / aggregated / run-status to file_system or database_result tables
# - Connection pooling only when file_source.mode == "database"
# Usage: Rscript sir_step2_simulate.R <config_path> <sim_id>
#          [daycent_exe] [scratch_dir] [--partition-id N]

# --------------------------------------------------------------------------------------
# sir_step2_simulate_qc.R
# --------------------------------------------------------------------------------------
# QC after Step 2 simulations (partition-level when using DB partitions).
# - database run_type: run_status QC — status=0 success, status=1 error; if both exist
#   for the same (partition_id, SampleID), treat as retried OK; warn if only status=1
#   also add indexes on partition annual tables
# - file_system run_type: verify dc_annualRslt_<id>.rds exist under Annual_Outputs
#   for 1:sir$nsim (accepts scientific notation filenames like 1e+05)
# Usage: Rscript sir_step2_simulate_qc.R <config_path> <partition_id>

# --------------------------------------------------------------------------------------
# sir_step2_aggregate.R
# --------------------------------------------------------------------------------------
# Weighted-mean aggregation across NRI/site points (crop scaling workflows).
# - Loads annual Step 2 results (file_system partitions or DB part* tables)
# - Applies weighted aggregation per output-specs
# - Writes Weighted_Mean_Outputs or DB weighted table for use in Step 3
# Usage: Rscript sir_step2_aggregate.R <config_path> [sample_ids]
#   sample_ids optional e.g. "1,2,3,10-20"; default = all found

# --------------------------------------------------------------------------------------
# sir_step2_aggregate_qc.R
# --------------------------------------------------------------------------------------
# QC / indexing after weighted aggregation path is populated.
# - database: connect to sir weighted result table; create useful indexes
#   (job_group, simulation_id) so later steps query efficiently
# - can be extended similarly to GSA aggregate QC for row-count checks
# Usage: Rscript sir_step2_aggregate_qc.R <config_path>

# --------------------------------------------------------------------------------------
# sir_step3_likelihood.R
# --------------------------------------------------------------------------------------
# Likelihood / GOF for ONE parameter set (task_id = SampleID / sim id).
# - Compares model outputs to observations (SOC, crop yield, NH3, per config)
# - Writes Likelihood_Outputs under the SIR tree for that sample
# Usage: Rscript sir_step3_likelihood.R <config.yaml> <task_id>
#          [--date-stamp ...] [--start-id ...] [--verbose]

# --------------------------------------------------------------------------------------
# sir_step3_likelihood_qc.R
# --------------------------------------------------------------------------------------
# QC for Step 3 likelihood products before Step 4 combine.
# - Checks expected likelihood files / completeness for samples
# Usage: Rscript sir_step3_likelihood_qc.R <config.yaml> <task_id> [options]

# --------------------------------------------------------------------------------------
# sir_step4_combine_gofsl_posterior_MC_data.R
# --------------------------------------------------------------------------------------
# SIR Steps 4A–4C: combine and form the posterior sample for Bayesian calibration.
# - Combines per-job-group likelihood (and related) results across all MC samples
# - Builds gof / likelihood summaries and resamples parameter sets with Sequential
#   Importance Resampling (SIR) to obtain posterior MC draws
# - Writes combined likelihood objects and posterior parameter / prediction products
#   under SIR Results (names vary by project: crop, SOC, NH3)
# Usage: Rscript sir_step4_combine_gofsl_posterior_MC_data.R <config_path>

# --------------------------------------------------------------------------------------
# sir_step4.5_uncertainty.R
# --------------------------------------------------------------------------------------
# Optional post-posterior uncertainty step (commonly used for SOC).
# - Takes SIR-resampled posterior SampleIDs and adds hyperparameter / variance-
#   component Monte Carlo error on the log residual scale
# - Structures: independent residual; rS (site + residual); rSY (site + site:year + residual)
# - Produces posterior predictive distributions that include model error, not only
#   parameter uncertainty across SIR samples
# Usage: Rscript sir_step4.5_uncertainty.R <config_path>

#=======================================================================================
# Output tree (conceptual)
#   {output_base}/{date_stamp}/SIR/
#     mc_SIR_draw.rds
#     point_assignments.rds          # scaling mode
#     Annual_Outputs/jobGroup_*/
#     Aggregated_Outputs/jobGroup_*/
#     Weighted_Mean_Outputs/...        # after step2_aggregate (crop scaling)
#     Likelihood_Outputs/jobGroup_*/
#     Run_Status/jobGroup_*/
#     Results/                         # after step4 (+ optional step 4.5)
#=======================================================================================
#
# GSA vs SIR (quick contrast)
# - GSA: Sobol designs with method name in path (GSA/{method}/); ends in sensitivity indices
# - SIR: single SIR MC draw set; nsim = total samples; ends in likelihood-weighted
#   posterior resampling (and optional uncertainty expansion)
#=======================================================================================
