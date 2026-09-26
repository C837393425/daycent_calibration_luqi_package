#!/bin/bash
#=======================================================================================
#  PURPOSE:   GSA Step 3: Likelihood Calculation - SLURM Array Job Script
#
#  DESCRIPTION: Runs GSA Step 3 likelihood for one parameter set per array task.
#               task_id = SLURM_ARRAY_TASK_ID.
#
#  USAGE:
#    sbatch --array=1-N gsa_step3_rubel.sh <config.yaml> [gsa_method] [extra args...]
#    where N is the total number of simulations from GSA Step 1
#
#  ARGUMENTS:
#    config.yaml  - Required: config basename, name.yaml, or path under workflows/configs/
#    gsa_method   - Optional: method name (default: soboljansen)
#    extra args   - Optional: forwarded to R (e.g. --verbose is added by default)
#
#  EXAMPLES:
#    sbatch --array=1-66560 workflows/scripts/ShellScripts/GSA/gsa_step3_rubel.sh \
#        crop_yield_corn_all.yaml 
#    sbatch --array=1-15360 workflows/scripts/ShellScripts/GSA/gsa_step3_rubel.sh \
#        crop_yield_corn_all.yaml sobol
#
#  AUTHOR:    Claude Code (claude.ai/code)
#             Based on original implementation by Ram Gurung
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
#  Project: Land-CRAFT DayCent Calibration Framework
#=======================================================================================

#SBATCH --job-name=gsaS3
#SBATCH --partition=rubel
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=2GB
##SBATCH --time=02:00:00
##SBATCH --mail-type=ALL
##SBATCH --mail-user=yi.yang@colostate.edu

##SBATCH --output=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/GSA_Step3_stdout_%A-%a.log
##SBATCH --error=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/GSA_Step3_stderr_%A-%a.log
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

# =======================================================================================
# Project root (absolute for SLURM spool compatibility)
# =======================================================================================
PROJECT_ROOT="/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

# =======================================================================================
# Config (required $1); GSA method (optional $2, default soboljansen)
# =======================================================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch --array=1-N $0 <config.yaml> [gsa_method]"
  echo ""
  echo "Examples:"
  echo "  sbatch --array=1-66560%750 $0 crop_yield_corn_all.yaml"
  echo "  sbatch --array=1-66560     $0 crop_yield_corn_C6.yaml soboljansen"
  echo "  sbatch --array=1-15360     $0 crop_yield_corn_all.yaml sobol"
  echo ""
  echo "NOTE: N must match the simulation count from GSA Step 1"
  exit 1
fi

CONFIG_ARG="$1"
shift

# Optional method: second arg if present and not a --flag
if [[ -n "${1:-}" && "$1" != --* ]]; then
  GSA_METHOD="$1"
  shift
else
  GSA_METHOD="soboljansen"
fi

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
  echo "Usage: sbatch --array=1-N $0 <config.yaml> [gsa_method]"
  exit 1
fi

TASK_ID="${SLURM_ARRAY_TASK_ID:-}"
if [[ -z "$TASK_ID" ]]; then
  echo "ERROR: SLURM_ARRAY_TASK_ID is not set."
  echo "Submit with an array, e.g.:"
  echo "  sbatch --array=1-66560 $0 $CONFIG_ARG [$GSA_METHOD]"
  exit 1
fi

# Prefer shared RScripts tree
R_SCRIPT="$PROJECT_ROOT/workflows/scripts/RScripts/GSA/gsa_step3_likelihood.R"
if [[ ! -f "$R_SCRIPT" ]]; then
  R_SCRIPT="$PROJECT_ROOT/workflows/scripts/crop_yield_all/GSA/gsa_step3_likelihood.R"
fi

echo "=== GSA Step 3 Likelihood Calculation Started ==="
date
echo "Job ID: $SLURM_JOB_ID"
echo "Array Task ID (Parameter Set): $TASK_ID"
echo "Node: $SLURM_JOB_NODELIST"
echo "Config argument: $CONFIG_ARG"
echo "Configuration: $CONFIG_PATH"
echo "GSA Method: $GSA_METHOD (default soboljansen if not provided)"
echo "Working Directory: $(pwd)"
echo "R Script: $R_SCRIPT"

if [[ ! -f "$R_SCRIPT" ]]; then
  echo "ERROR: R script not found: $R_SCRIPT"
  exit 1
fi

echo ""
echo "=== Starting GSA Step 3 Likelihood Calculation ==="
# R expects: config, gsa_method, task_id [, options]
echo "Command: Rscript --vanilla $R_SCRIPT $CONFIG_PATH $GSA_METHOD $TASK_ID --verbose ${EXTRA_ARGS[*]}"
echo ""

Rscript --vanilla \
  "$R_SCRIPT" \
  "$CONFIG_PATH" \
  "$GSA_METHOD" \
  "$TASK_ID" \
  --verbose \
  "${EXTRA_ARGS[@]}"

EXIT_STATUS=$?

echo ""
echo "=== GSA Step 3 Job Completed ==="
date
echo "Exit Status: $EXIT_STATUS"

exit $EXIT_STATUS
