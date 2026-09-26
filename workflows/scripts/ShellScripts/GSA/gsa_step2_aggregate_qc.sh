#!/bin/bash

####========================================================
#### GSA Step 2 Aggregate QC
####========================================================
#
# USAGE:
#   sbatch workflows/scripts/ShellScripts/GSA/gsa_step2_aggregate_qc.sh <config.yaml> [task_id] [n_sample]
#
# Examples:
#   sbatch .../gsa_step2_aggregate_qc.sh crop_yield_corn_all.yaml 
#   sbatch .../gsa_step2_aggregate_qc.sh crop_yield_corn_all.yaml 3 # different GSA method other than 6 (default)
#
# Config may be:
#   - basename with or without .yaml  (looked up under workflows/configs/)
#   - absolute or relative path to a YAML file
#
# Optional args:
#   task_id  - GSA method index (default: SLURM_ARRAY_TASK_ID, else 6)
#   n_sample - expected sample count for QC (recommended for file_system checks)
#
####========================================================

#SBATCH -J gsaS2_agg_qc
#SBATCH -p rubel
#SBATCH --mem-per-cpu=10G
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --array=1
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null
#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/gsa_S2_agg_qc_stdout_%A-%a.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/gsa_S2_agg_qc_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=ALL
#SBATCH --exclude=rubel-030

# =========================================================
# Project roots (absolute for SLURM spool compatibility)
# =========================================================
PROJECT_ROOT="/data/rubelscratch/rubelogle/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"
R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/RScripts/GSA/gsa_step2_aggregate_qc.R"

# Fallback if shared RScripts copy is missing
if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/crop_yield_all/GSA/gsa_step2_aggregate_qc.R"
fi

# =========================================================
# Config (required $1)
# =========================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch $0 <config.yaml> [task_id] [n_sample]"
  echo ""
  echo "Examples:"
  echo "  sbatch $0 crop_yield_corn_all.yaml"
  echo "  sbatch $0 crop_yield_corn_C6.yaml 6 66560"
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
  else
    path=""
  fi
  echo "$path"
}

CONFIG_PATH="$(resolve_config_path "$CONFIG_ARG")"

if [[ -z "$CONFIG_PATH" || ! -f "$CONFIG_PATH" ]]; then
  echo "ERROR: Config file not found for argument: '$CONFIG_ARG'"
  echo "Looked under:"
  echo "  - as given (absolute/relative)"
  echo "  - $CONFIGS_DIR/<name>"
  echo "  - $CONFIGS_DIR/<name>.yaml"
  echo ""
  echo "Usage: sbatch $0 <config.yaml> [task_id] [n_sample]"
  exit 1
fi

# =========================================================
# Optional: task_id ($2), n_sample ($3)
# =========================================================
TASK_ID="${2:-${SLURM_ARRAY_TASK_ID:-6}}"
N_SAMPLE="${3:-}"

# =========================================================
# Run
# =========================================================
echo "======================================================================"
echo "GSA Step 2 Aggregate QC"
echo "Config argument: $CONFIG_ARG"
echo "Config file:     $CONFIG_PATH"
echo "R script:        $R_FULL_PATH_RUN"
echo "Task ID:         $TASK_ID"
echo "n_sample:        ${N_SAMPLE:-<not set>}"
echo "Node:            $(hostname)"
echo "Date:            $(date)"
echo "======================================================================"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  echo "ERROR: R script not found: $R_FULL_PATH_RUN"
  exit 1
fi

if [[ -n "$N_SAMPLE" ]]; then
  echo "Executing: Rscript \"$R_FULL_PATH_RUN\" \"$CONFIG_PATH\" \"$TASK_ID\" \"$N_SAMPLE\""
  Rscript "$R_FULL_PATH_RUN" "$CONFIG_PATH" "$TASK_ID" "$N_SAMPLE"
else
  echo "Executing: Rscript \"$R_FULL_PATH_RUN\" \"$CONFIG_PATH\" \"$TASK_ID\""
  echo "NOTE: n_sample not provided; R may require it for count QC."
  Rscript "$R_FULL_PATH_RUN" "$CONFIG_PATH" "$TASK_ID"
fi

EXIT_STATUS=$?
echo "GSA Step 2 Aggregate QC finished with exit status: $EXIT_STATUS"
exit $EXIT_STATUS
