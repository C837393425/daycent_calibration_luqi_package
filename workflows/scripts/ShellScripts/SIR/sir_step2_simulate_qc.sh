#!/bin/bash

####========================================================
#### SIR Step 2 Simulation QC
####========================================================
#
# USAGE:
#   sbatch --array=1-N workflows/scripts/ShellScripts/SIR/sir_step2_simulate_qc.sh \
#       <config.yaml>
#
# Arguments:
#   config.yaml  - Required: basename, name.yaml, or path under workflows/configs/
#
# SLURM_ARRAY_TASK_ID is used as partition_id (database partition QC).
#
# Examples:
#   sbatch --array=1-4  .../sir_step2_simulate_qc.sh crop_yield_corn_m2.yaml
#   sbatch --array=1-25 .../sir_step2_simulate_qc.sh crop_yield_corn_all.yaml
#   sbatch --array=1    .../sir_step2_simulate_qc.sh soil_organic_carbon.yaml
#
# R: sir_step2_simulate_qc.R <config_path> <partition_id>
#
####========================================================

#SBATCH -J sirS2_sim_qc
#SBATCH -p rubel
##SBATCH --mem-per-cpu=10G
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --array=1
##SBATCH --dependency=afterany:5400534
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null
#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/sir_S2_sim_qc_stdout_%A-%a.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/sir_S2_sim_qc_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=ALL
##SBATCH --exclude=rubel-030

# =========================================================
# Project roots (absolute for SLURM spool compatibility)
# =========================================================
PROJECT_ROOT="/data/rubelscratch/rubelogle/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"
R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/RScripts/SIR/sir_step2_simulate_qc.R"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/crop_yield_all/SIR/sir_step2_simulate_qc.R"
fi

# =========================================================
# Config (required $1)
# =========================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch --array=1-N $0 <config.yaml>"
  echo ""
  echo "Examples:"
  echo "  sbatch --array=1-4  $0 crop_yield_corn_m2.yaml"
  echo "  sbatch --array=1-25 $0 crop_yield_corn_all.yaml"
  echo ""
  echo "Note: Array task = partition_id."
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
  echo "Usage: sbatch --array=1-N $0 <config.yaml>"
  exit 1
fi

# partition_id from SLURM array
PARTITION_ID="${SLURM_ARRAY_TASK_ID:-1}"

# =========================================================
# Run
# =========================================================
echo "======================================================================"
echo "SIR Step 2 Simulation QC"
echo "Config argument: $CONFIG_ARG"
echo "Config file:     $CONFIG_PATH"
echo "R script:        $R_FULL_PATH_RUN"
echo "Partition ID:    $PARTITION_ID (from SLURM_ARRAY_TASK_ID)"
echo "Node:            $(hostname)"
echo "Date:            $(date)"
echo "======================================================================"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  echo "ERROR: R script not found: $R_FULL_PATH_RUN"
  exit 1
fi

echo "Executing: Rscript \"$R_FULL_PATH_RUN\" \"$CONFIG_PATH\" \"$PARTITION_ID\""
Rscript "$R_FULL_PATH_RUN" "$CONFIG_PATH" "$PARTITION_ID"

EXIT_STATUS=$?
echo "SIR Step 2 Simulation QC finished with exit status: $EXIT_STATUS"
exit $EXIT_STATUS
