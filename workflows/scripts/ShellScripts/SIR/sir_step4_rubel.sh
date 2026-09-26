#!/bin/bash
#=======================================================================================
# SIR Step 4: Combine gofsl / posterior / MC data (SLURM)
#
# USAGE:
#   sbatch sir_step4_rubel.sh <config.yaml> [extra args...]
#
# Arguments:
#   config.yaml  - Required: basename, name.yaml, or path under workflows/configs/
#   extra args   - Optional: forwarded to R (currently unused by default R script)
#
# Examples:
#   sbatch workflows/scripts/ShellScripts/SIR/sir_step4_rubel.sh crop_yield_corn_all.yaml
#   sbatch workflows/scripts/ShellScripts/SIR/sir_step4_rubel.sh crop_yield_corn_m2.yaml
#   sbatch workflows/scripts/ShellScripts/SIR/sir_step4_rubel.sh soil_organic_carbon.yaml
#
# R: sir_step4_combine_gofsl_posterior_MC_data.R <config>
#
#=======================================================================================

#SBATCH --job-name=sirS4
#SBATCH --partition=hipri
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
##SBATCH --exclude=rubel-030
#SBATCH --mem-per-cpu=10GB
#SBATCH --array=1
##SBATCH --mail-type=ALL
##SBATCH --mail-user=yi.yang@colostate.edu

#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/SIR_Step4_stdout_%A-%a.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/SIR_Step4_stderr_%A-%a.log
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null

# =======================================================================================
# Project root (absolute for SLURM spool compatibility)
# =======================================================================================
PROJECT_ROOT="/data/rubelscratch/rubelogle/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

# =======================================================================================
# Config (required $1); remaining args forwarded to R
# =======================================================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch $0 <config.yaml>"
  echo "  bash   $0 <config.yaml>"
  echo ""
  echo "Examples:"
  echo "  sbatch $0 crop_yield_corn_all.yaml"
  echo "  sbatch $0 crop_yield_corn_m2.yaml"
  echo "  sbatch $0 soil_organic_carbon.yaml"
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
  echo "Usage: sbatch $0 <config.yaml>"
  exit 1
fi

# Prefer shared RScripts tree
R_SCRIPT="$PROJECT_ROOT/workflows/scripts/RScripts/SIR/sir_step4_combine_gofsl_posterior_MC_data.R"
if [[ ! -f "$R_SCRIPT" ]]; then
  R_SCRIPT="$PROJECT_ROOT/workflows/scripts/crop_yield_all/SIR/sir_step4_combine_gofsl_posterior_MC_data.R"
fi

echo "=== SIR Step 4 Combination gofsl posterior Started ==="
date
echo "Job ID: ${SLURM_JOB_ID:-local}"
echo "Node: ${SLURM_JOB_NODELIST:-$(hostname)}"
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
echo "=== Starting SIR Step 4 Combination gofsl posterior ==="
echo "Command: Rscript --vanilla $R_SCRIPT $CONFIG_PATH ${EXTRA_ARGS[*]}"
echo ""

Rscript --vanilla \
  "$R_SCRIPT" \
  "$CONFIG_PATH" \
  "${EXTRA_ARGS[@]}"

EXIT_STATUS=$?

echo ""
echo "=== SIR Step 4 Job Completed ==="
date
echo "Exit Status: $EXIT_STATUS"

exit $EXIT_STATUS
