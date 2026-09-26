#!/bin/bash

####========================================================
#### SIR Step 2 Aggregate QC
####========================================================
#
# USAGE:
#   sbatch workflows/scripts/ShellScripts/SIR/sir_step2_aggregate_qc.sh <config.yaml>
#
# Arguments:
#   config.yaml  - Required: basename, name.yaml, or path under workflows/configs/
#
# Examples:
#   sbatch .../sir_step2_aggregate_qc.sh crop_yield_corn_all.yaml
#   sbatch .../sir_step2_aggregate_qc.sh crop_yield_corn_m2.yaml
#   sbatch .../sir_step2_aggregate_qc.sh soil_organic_carbon.yaml
#
# R: sir_step2_aggregate_qc.R <config_path>
#
####========================================================

#SBATCH -J sirS2_agg_qc
#SBATCH -p rubel
##SBATCH --mem-per-cpu=10G
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --array=1
##SBATCH --dependency=afterany:5967150
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null
#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/sir_S2_agg_qc_stdout_%A-%a.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/sir_S2_agg_qc_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=ALL
##SBATCH --exclude=rubel-030

# =========================================================
# Project roots (absolute for SLURM spool compatibility)
# =========================================================
PROJECT_ROOT="/data/rubelscratch/rubelogle/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"
R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/RScripts/SIR/sir_step2_aggregate_qc.R"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/crop_yield_all/SIR/sir_step2_aggregate_qc.R"
fi

# =========================================================
# Config (required $1)
# =========================================================
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

# =========================================================
# Run
# =========================================================
echo "======================================================================"
echo "SIR Step 2 Aggregate QC"
echo "Config argument: $CONFIG_ARG"
echo "Config file:     $CONFIG_PATH"
echo "R script:        $R_FULL_PATH_RUN"
echo "Node:            $(hostname)"
echo "Date:            $(date)"
echo "======================================================================"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  echo "ERROR: R script not found: $R_FULL_PATH_RUN"
  exit 1
fi

echo "Executing: Rscript \"$R_FULL_PATH_RUN\" \"$CONFIG_PATH\""
Rscript "$R_FULL_PATH_RUN" "$CONFIG_PATH"

EXIT_STATUS=$?
echo "SIR Step 2 Aggregate QC finished with exit status: $EXIT_STATUS"
exit $EXIT_STATUS
