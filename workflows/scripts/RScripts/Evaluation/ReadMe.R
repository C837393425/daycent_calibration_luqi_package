#=======================================================================================
# Evaluation pipeline scripts — summary (workflows/scripts/RScripts/Evaluation)
#
# Core logic lives in the bayesiancalibr package; scripts here are CLI wrappers.
# Purpose: run held-out evaluation sites using posterior parameter sets from SIR
#          (best_param_set_{model}.csv from SIR Step 4.5).
#
# Typical order (after SIR Steps 1–4.5):
#   Step 1 setup     — RunFile, folders or DB tables (no DayCent)
#   Step 2 simulate  — sbatch array: one posterior row per task
#
# Config: YAML under workflows/configs/. See the `evaluation:` block.
#
# Cluster wrappers: workflows/scripts/ShellScripts/Evaluation/
#=======================================================================================

# --------------------------------------------------------------------------------------
# eva_step1_setup.R
# --------------------------------------------------------------------------------------
# One-time setup before the Step 2 array, same role as SIR Step 1.
# - Builds evaluation RunFile (chained_schedule + group == evaluation.site_group)
# - Copies posterior parameter rows from SIR/Results/best_param_set_{model}.csv
#   to Evaluation/mc_EVA_draw.rds
# - file_system results: creates
#     results/<project>/<date_stamp>/Evaluation/{Annual_Outputs,...}/jobGroup_*
# - database results (database_result.run_type: "database"): DROP/CREATE
#     eva_results_annual and eva_results_run_status (and optional daily/weighted)
# Usage: Rscript eva_step1_setup.R <config_path> [date_stamp]
#
# Example:
#   Rscript workflows/scripts/RScripts/Evaluation/eva_step1_setup.R \
#     workflows/configs/soil_organic_carbon.yaml

# --------------------------------------------------------------------------------------
# eva_step2_simulate.R
# --------------------------------------------------------------------------------------
# Runs DayCent for ONE posterior parameter set (one sim_id).
# - Requires Step 1 (RunFile.rds + mc_EVA_draw.rds)
# - sim_id is the 1-based row index into best_param_set_{model}.csv / mc_EVA_draw.rds
# - file_source.mode: "filesystem" | "database"  (site/schedule inputs)
# - database_result.run_type: "file_system" | "database"  (where outputs are stored)
# Usage: Rscript eva_step2_simulate.R <config_path> <sim_id>
#          [daycent_exe] [scratch_dir] [--partition-id N]
#
# Example (SOC, posterior index 1, after Step 1):
#   Rscript workflows/scripts/RScripts/Evaluation/eva_step2_simulate.R \
#     workflows/configs/soil_organic_carbon.yaml 1
#
# --------------------------------------------------------------------------------------
# sir_step5_report_SOC.qmd  (Evaluation observed vs modeled report)
# --------------------------------------------------------------------------------------
# Quarto report that uses Evaluation Annual_Outputs only (not SIR likelihood plots).
# Requires Evaluation Step 2 results under results/.../Evaluation/Annual_Outputs/.
# Example:
#   quarto render workflows/scripts/RScripts/Evaluation/sir_step5_report_SOC.qmd
#=======================================================================================
