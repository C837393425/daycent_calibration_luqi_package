#!/bin/bash
#=======================================================================================
# SIR Step 4.5: Hyperparameter (variance-component) uncertainty - SLURM
#
# Adds residual Monte Carlo error to the ind / ln_ind posterior prediction tables
# produced by SIR Step 4.
#
# USAGE:
#   sbatch sir_step4.5_uncertainty_rubel.sh <config.yaml> [date_stamp]
#
# Arguments:
#   config.yaml  - Required: basename, name.yaml, or path under workflows/configs/
#   date_stamp   - Optional: override date stamp from config (e.g. 12Mar2026)
#
# Examples:
#   sbatch workflows/scripts/ShellScripts/SIR/sir_step4.5_uncertainty_rubel.sh \
#       crop_yield_corn_all.yaml
#   sbatch workflows/scripts/ShellScripts/SIR/sir_step4.5_uncertainty_rubel.sh \
#       crop_yield_corn_m2.yaml 
#
# R: sir_step4.5_uncertainty.R <config> [date_stamp]
#
#=======================================================================================

#SBATCH --job-name=sirS4.5
#SBATCH --partition=longrun
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
##SBATCH --mail-type=ALL
##SBATCH --mail-user=yi.yang@colostate.edu

#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/SIR_Step4.5_stdout_%j.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/SIR_Step4.5_stderr_%j.log
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null

# =======================================================================================
# Project root (absolute for SLURM spool compatibility)
# =======================================================================================
PROJECT_ROOT="/data/rubelscratch/rubelogle/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

# =======================================================================================
# Config (required $1); optional date_stamp ($2)
# =======================================================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch $0 <config.yaml> [date_stamp]"
  echo "  bash   $0 <config.yaml> [date_stamp]"
  echo ""
  echo "Examples:"
  echo "  sbatch $0 crop_yield_corn_all.yaml"
  echo "  sbatch $0 crop_yield_corn_m2.yaml"
  echo "  sbatch $0 crop_yield_corn_m2.yaml 12Mar2026"
  exit 1
fi

CONFIG_ARG="$1"
DATE_STAMP="${2:-}"

resolve_config_path() {
  local arg="$1"
  local path=""

  if [[ "$arg" == /* ]] && [[ -f "$arg" ]]; then
    path="$arg"
  elif [[ -f "$arg" ]]; then
    path="$(cd "$(dirname "$arg")" && pwd)/$(basename "$arg")"
  elif [[ -f "$CONFIGS_DIR/$arg" ]]; then
    path="$CONFIGS_DIR/$arg"
  elif [[ "$arg" != *.yaml ]] && [[ -f "$CONFIGS_DIR/${arg}.yaml" ]]; then
    path="$CONFIGS_DIR/${arg}.yaml"
  elif [[ -f "$PROJECT_ROOT/$arg" ]]; then
    path="$PROJECT_ROOT/$arg"
  elif [[ -f "$PROJECT_ROOT/workflows/configs/$arg" ]]; then
    path="$PROJECT_ROOT/workflows/configs/$arg"
  else
    path=""
  fi
  echo "$path"
}

CONFIG_PATH="$(resolve_config_path "$CONFIG_ARG")"

if [[ -z "$CONFIG_PATH" || ! -f "$CONFIG_PATH" ]]; then
  echo "ERROR: Config file not found for argument: '$CONFIG_ARG'"
  echo "Looked under: as given, $CONFIGS_DIR/<name>[.yaml], $PROJECT_ROOT/..."
  echo "Usage: sbatch $0 <config.yaml> [date_stamp]"
  exit 1
fi

# Prefer shared RScripts tree
R_SCRIPT="$PROJECT_ROOT/workflows/scripts/RScripts/SIR/sir_step4.5_uncertainty.R"
if [[ ! -f "$R_SCRIPT" ]]; then
  R_SCRIPT="$PROJECT_ROOT/workflows/scripts/crop_yield_all/SIR/sir_step4.5_uncertainty.R"
fi

echo "=== SIR Step 4.5 Hyperparameter Uncertainty Started ==="
date
echo "Job ID: ${SLURM_JOB_ID:-local}"
echo "Node: ${SLURM_JOB_NODELIST:-$(hostname)}"
echo "Config argument: $CONFIG_ARG"
echo "Configuration: $CONFIG_PATH"
if [[ -n "$DATE_STAMP" ]]; then
  echo "Date stamp override: $DATE_STAMP"
fi
echo "Working Directory: $(pwd)"
echo "R Script: $R_SCRIPT"

if [[ ! -f "$R_SCRIPT" ]]; then
  echo "ERROR: R script not found: $R_SCRIPT"
  exit 1
fi

if [[ -n "$DATE_STAMP" ]]; then
  CMD=(Rscript --vanilla "$R_SCRIPT" "$CONFIG_PATH" "$DATE_STAMP")
else
  CMD=(Rscript --vanilla "$R_SCRIPT" "$CONFIG_PATH")
fi

echo ""
echo "=== Starting SIR Step 4.5 Hyperparameter Uncertainty ==="
echo "Command: ${CMD[*]}"
echo ""

"${CMD[@]}"

EXIT_STATUS=$?

echo ""
echo "=== SIR Step 4.5 Job Completed ==="
date
echo "Exit Status: $EXIT_STATUS"

exit $EXIT_STATUS
