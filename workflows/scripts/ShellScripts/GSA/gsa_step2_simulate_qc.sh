#!/bin/bash

####========================================================
#### GSA Step 2 Simulation QC
####========================================================
#
# USAGE:
#   sbatch --array=1-N workflows/scripts/ShellScripts/GSA/gsa_step2_simulate_qc.sh \
#       <config.yaml> [task_id] [n_sample]
#
# Arguments:
#   config.yaml  - Required: config basename, name.yaml, or path under workflows/configs/
#   task_id      - Optional: GSA method index (1-based; default: 6 = soboljansen)
#   n_sample     - Optional: expected sample count for filesystem QC
#
# SLURM_ARRAY_TASK_ID is used as partition_id (database partition QC).
#
# Examples:
#   sbatch --array=1-25 .../gsa_step2_simulate_qc.sh crop_yield_corn_all.yaml
#   sbatch --array=1-25 .../gsa_step2_simulate_qc.sh crop_yield_corn_C6.yaml 3 # different GSA method other than 6 (default)
#
####========================================================

#SBATCH -J gsaS2_sim_qc
#SBATCH -p rubel
##SBATCH --mem-per-cpu=10G
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --array=1
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null
#SBATCH --output=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/gsa_S2_sim_qc_stdout_%A-%a.log
#SBATCH --error=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/gsa_S2_sim_qc_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=ALL
#SBATCH --exclude=rubel-030

# =========================================================
# Project roots (absolute for SLURM spool compatibility)
# =========================================================
PROJECT_ROOT="/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"
R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/RScripts/GSA/gsa_step2_simulate_qc.R"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/crop_yield_all/GSA/gsa_step2_simulate_qc.R"
fi

# =========================================================
# Config (required $1); task_id default 6 (soboljansen); n_sample optional
# =========================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch --array=1-N $0 <config.yaml> [task_id] [n_sample]"
  echo ""
  echo "Examples:"
  echo "  sbatch --array=1-25 $0 crop_yield_corn_all.yaml"
  echo "  sbatch --array=1-25 $0 crop_yield_corn_C6.yaml 6"
  echo "  sbatch --array=1-4  $0 crop_yield_corn_all.yaml 6 66560"
  echo ""
  echo "Note: task_id defaults to 6 (soboljansen). Array task = partition_id."
  exit 1
fi
CONFIG_ARG="$1"
TASK_ID="${2:-6}"          # default soboljansen index
N_SAMPLE="${3:-}"

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
  echo "Usage: sbatch --array=1-N $0 <config.yaml> [task_id] [n_sample]"
  exit 1
fi

# partition_id from SLURM array (legacy behavior)
PARTITION_ID="${SLURM_ARRAY_TASK_ID:-1}"

# =========================================================
# Run
# =========================================================
echo "======================================================================"
echo "GSA Step 2 Simulation QC"
echo "Config argument: $CONFIG_ARG"
echo "Config file:     $CONFIG_PATH"
echo "R script:        $R_FULL_PATH_RUN"
echo "Partition ID:    $PARTITION_ID (from SLURM_ARRAY_TASK_ID)"
echo "Task ID:         $TASK_ID (default 6 = soboljansen)"
echo "n_sample:        ${N_SAMPLE:-<not set>}"
echo "Node:            $(hostname)"
echo "Date:            $(date)"
echo "======================================================================"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  echo "ERROR: R script not found: $R_FULL_PATH_RUN"
  exit 1
fi

# R expects: config_path, partition_id, task_id [, n_sample]
if [[ -n "$N_SAMPLE" ]]; then
  echo "Executing: Rscript \"$R_FULL_PATH_RUN\" \"$CONFIG_PATH\" \"$PARTITION_ID\" \"$TASK_ID\" \"$N_SAMPLE\""
  Rscript "$R_FULL_PATH_RUN" "$CONFIG_PATH" "$PARTITION_ID" "$TASK_ID" "$N_SAMPLE"
else
  echo "Executing: Rscript \"$R_FULL_PATH_RUN\" \"$CONFIG_PATH\" \"$PARTITION_ID\" \"$TASK_ID\""
  Rscript "$R_FULL_PATH_RUN" "$CONFIG_PATH" "$PARTITION_ID" "$TASK_ID"
fi

EXIT_STATUS=$?
echo "GSA Step 2 Simulation QC finished with exit status: $EXIT_STATUS"
exit $EXIT_STATUS
