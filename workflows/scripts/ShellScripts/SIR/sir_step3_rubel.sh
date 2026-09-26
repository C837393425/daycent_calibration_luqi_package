#!/bin/bash
#=======================================================================================
# SIR Step 3: Likelihood Calculation - SLURM Array Job Script
#
# Each array task processes ONE parameter set (task_id = SLURM_ARRAY_TASK_ID).
#
# USAGE:
#   sbatch --array=1-N sir_step3_rubel.sh <config.yaml> [extra args...]
#   where N is the total number of simulations from SIR Step 1 / Step 2
#
# Arguments:
#   config.yaml  - Required: basename, name.yaml, or path under workflows/configs/
#   extra args   - Optional: forwarded to R (e.g. --date-stamp 05Aug2026)
#
# Examples:
#   sbatch --array=1-100000 workflows/scripts/ShellScripts/SIR/sir_step3_rubel.sh \
#       crop_yield_corn_all.yaml
#   sbatch --array=1-250000 workflows/scripts/ShellScripts/SIR/sir_step3_rubel.sh \
#       crop_yield_corn_m2.yaml 
#
# R: sir_step3_likelihood.R <config> <task_id> [options]
#    (this script always passes --verbose)
#
#=======================================================================================

#SBATCH --job-name=sirS3
#SBATCH --partition=rubel
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --exclude=rubel-031,rubel-041
#SBATCH --mem-per-cpu=1GB
##SBATCH --mail-type=ALL
##SBATCH --mail-user=yi.yang@colostate.edu

##SBATCH --output=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/SIR_Step3_stdout_%A-%a.log
##SBATCH --error=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/SIR_Step3_stderr_%A-%a.log
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

# =======================================================================================
# Project root (absolute for SLURM spool compatibility)
# =======================================================================================
PROJECT_ROOT="/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

# =======================================================================================
# Config (required $1); remaining args forwarded to R
# =======================================================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch --array=1-N $0 <config.yaml> [extra args...]"
  echo ""
  echo "Examples:"
  echo "  sbatch --array=1-100000 $0 crop_yield_corn_all.yaml"
  echo "  sbatch --array=1-250000 $0 crop_yield_corn_m2.yaml"
  echo "  sbatch --array=1-5      $0 soil_organic_carbon.yaml"
  echo ""
  echo "NOTE: N must match MC_nsim from SIR Step 1"
  exit 1
fi

CONFIG_ARG="$1"
shift
EXTRA_ARGS=("$@")

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
  echo "Usage: sbatch --array=1-N $0 <config.yaml>"
  exit 1
fi

TASK_ID="${SLURM_ARRAY_TASK_ID:-}"
if [[ -z "$TASK_ID" ]]; then
  echo "ERROR: SLURM_ARRAY_TASK_ID is not set."
  echo "Submit with an array, e.g.:"
  echo "  sbatch --array=1-100000 $0 $CONFIG_ARG"
  exit 1
fi

# Prefer shared RScripts tree
R_SCRIPT="$PROJECT_ROOT/workflows/scripts/RScripts/SIR/sir_step3_likelihood.R"
if [[ ! -f "$R_SCRIPT" ]]; then
  R_SCRIPT="$PROJECT_ROOT/workflows/scripts/crop_yield_all/SIR/sir_step3_likelihood.R"
fi

echo "=== SIR Step 3 Likelihood Calculation Started ==="
date
echo "Job ID: $SLURM_JOB_ID"
echo "Array Task ID (Parameter Set): $TASK_ID"
echo "Node: $SLURM_JOB_NODELIST"
echo "Config argument: $CONFIG_ARG"
echo "Configuration: $CONFIG_PATH"
echo "Working Directory: $(pwd)"
echo "R Script: $R_SCRIPT"
if [[ ${#EXTRA_ARGS[@]} -gt 0 ]]; then
  echo "Extra arguments: ${EXTRA_ARGS[*]}"
fi

if [[ ! -f "$R_SCRIPT" ]]; then
  echo "ERROR: R script not found: $R_SCRIPT"
  exit 1
fi

echo ""
echo "=== Starting SIR Step 3 Likelihood Calculation ==="
# R expects: config, task_id [, options]
echo "Command: Rscript --vanilla $R_SCRIPT $CONFIG_PATH $TASK_ID --verbose ${EXTRA_ARGS[*]}"
echo ""

Rscript --vanilla \
  "$R_SCRIPT" \
  "$CONFIG_PATH" \
  "$TASK_ID" \
  --verbose \
  "${EXTRA_ARGS[@]}"

EXIT_STATUS=$?

echo ""
echo "=== SIR Step 3 Job Completed ==="
date
echo "Exit Status: $EXIT_STATUS"

exit $EXIT_STATUS
