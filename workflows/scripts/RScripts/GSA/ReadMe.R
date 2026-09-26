#=======================================================================================
# GSA pipeline scripts — summary (workflows/scripts/RScripts/GSA)
#
# Core logic lives in the bayesiancalibr package; scripts here are CLI wrappers.
# Typical order: Step 0 → 1 → 2 (simulate [+ QC]) → 2 aggregate (scaling only)
#                → 3 (likelihood [+ QC]) → 4 (combine + TSI).
# Config: YAML under workflows/configs/ (crop, SOC, NH3, etc.).
#=======================================================================================

# --------------------------------------------------------------------------------------
# gsa_step0_r_setup.R
# --------------------------------------------------------------------------------------
# One-time / pre-run environment setup.
# - Installs missing CRAN packages into the project rlib
# - Removes and reinstalls the bayesiancalibr package so the library matches this repo
# - Validates that required packages load
# Usage: Rscript gsa_step0_r_setup.R [config_path]

# --------------------------------------------------------------------------------------
# gsa_step1_setup.R
# --------------------------------------------------------------------------------------
# Creates Monte Carlo parameter designs for one GSA method.
# - Reads priors and config (nsim, nboot, method list)
# - Builds Sobol-type sample matrix via sensitivity package (total runs often ~ N*(p+2))
# - Writes mc_GSA_draw_<method>.rds, job-group folders, optional point_assignments
#   (for crop scaling mode)
# Usage: Rscript gsa_step1_setup.R <config_path> <task_id>
#   task_id = 1-based index into config$gsa$gsa_methods (e.g. 6 = soboljansen)

# --------------------------------------------------------------------------------------
# gsa_step2_simulate.R
# --------------------------------------------------------------------------------------
# Runs DayCent for ONE parameter set (one sim_id) for a chosen GSA method.
# - Loads MC draws, site list (filesystem or database input modes)
# - Optional --partition-id for scaling mode (subset of sites per partition)
# - Writes annual / aggregated / run-status outputs (file_system or database_result)
# - May use DB connection pooling when file_source.mode == "database"
# Usage: Rscript gsa_step2_simulate.R <config_path> <gsa_method> <sim_id>
#          [daycent_exe] [scratch_dir] [--partition-id N]

# --------------------------------------------------------------------------------------
# gsa_step2_simulate_qc.R
# --------------------------------------------------------------------------------------
# QC after Step 2 simulations for one method/partition.
# - database run_type: check run_status (0=success, 1=error; ignore status=1 if
#   same SampleID also has status=0), add indexes on partition annual tables
# - file_system run_type: verify dc_annualRslt_<id>.rds (including 1e+05-style ids)
#   exist under Annual_Outputs for expected sample count
# Usage: Rscript gsa_step2_simulate_qc.R <config_path> <partition_id> <task_id> [n_sample]

# --------------------------------------------------------------------------------------
# gsa_step2_aggregate.R
# --------------------------------------------------------------------------------------
# Weighted-mean aggregation across NRI/site points (crop scaling workflows).
# - Loads annual Step 2 results for sample_id(s)
# - Applies weighted aggregation per output-specs (aggregation_level / weight)
# - Writes Weighted_Mean_Outputs (or database weighted table)
# Usage: Rscript gsa_step2_aggregate.R <config_path> <gsa_method> [sample_ids]
#   sample_ids optional e.g. "1,2,3,10-20"; default = all found

# --------------------------------------------------------------------------------------
# gsa_step2_aggregate_qc.R
# --------------------------------------------------------------------------------------
# QC after Step 2 aggregation / related outputs.
# - database: check weighted-table sample count vs expected n_sample; add indexes
# - file_system: verify dc_aggRslt_<id>.rds exist under Aggregated_Outputs for 1:n_sample
# Usage: Rscript gsa_step2_aggregate_qc.R <config_path> <task_id> <n_sample>

# --------------------------------------------------------------------------------------
# gsa_step3_likelihood.R
# --------------------------------------------------------------------------------------
# Likelihood / GOF for ONE parameter set (task_id = SampleID / sim id).
# - Compares model outputs (annual or weighted, per config) to observations
# - Writes Likelihood_Outputs for that sample under the GSA method tree
# Usage: Rscript gsa_step3_likelihood.R <config.yaml> <gsa_method> <task_id>
#          [--date-stamp ...] [--start-id ...] [--verbose]

# --------------------------------------------------------------------------------------
# gsa_step3_likelihood_qc.R
# --------------------------------------------------------------------------------------
# QC that Step 3 likelihood products exist / look complete for a method (and/or samples).
# - Useful before Step 4 combine, which needs full likelihood coverage
# Usage: Rscript gsa_step3_likelihood_qc.R <config.yaml> <gsa_method> [options]

# --------------------------------------------------------------------------------------
# gsa_step4_combine_TSI_calculation.R
# --------------------------------------------------------------------------------------
# Final GSA analysis for one method (two parts in one script):
# - Step 4A: combine per-job-group likelihood (and aggregate) results into full matrices
# - Step 4B: compute first-order and total-order sensitivity indices (TSI) from the
#   Sobol design and model responses; write Results / figures as configured
# Usage: Rscript gsa_step4_combine_TSI_calculation.R <config.yaml> <gsa_method> <task_id>
#          [options]

#=======================================================================================
# Output tree (conceptual)
#   {output_base}/{date_stamp}/GSA/{method}/
#     mc_GSA_draw_{method}.rds
#     point_assignments.rds          # scaling mode
#     Annual_Outputs/jobGroup_*/
#     Aggregated_Outputs/jobGroup_*/   # site-aggregated site-level products
#     Weighted_Mean_Outputs/...        # after step2_aggregate (crop scaling)
#     Likelihood_Outputs/jobGroup_*/
#     Results/                         # after step4
#=======================================================================================
