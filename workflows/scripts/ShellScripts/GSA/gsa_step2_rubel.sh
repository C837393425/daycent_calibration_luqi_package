#!/usr/bin/env bash
#=======================================================================================
# Generic GSA Step 2 SLURM Submission Script - Individual Simulation Mode
#
# Each array task processes ONE Monte Carlo simulation (sim_id = SLURM_ARRAY_TASK_ID).
#
# USAGE:
#   sbatch --array=1-N gsa_step2_rubel.sh <config.yaml> [gsa_method] [extra args...]
#
# Arguments:
#   config.yaml  - Required: config basename, name.yaml, or path under workflows/configs/
#   gsa_method   - Optional: method name (default: soboljansen)
#   extra args   - Optional: e.g. --partition-id 3 for scaling mode
#
# Examples:
#   sbatch --array=1-66560 workflows/scripts/ShellScripts/GSA/gsa_step2_rubel.sh \
#       crop_yield_corn_all.yaml
#   sbatch --array=1-66560 workflows/scripts/ShellScripts/GSA/gsa_step2_rubel.sh \
#       crop_yield_corn_C6.yaml soboljansen
#   sbatch --array=1-66560 workflows/scripts/ShellScripts/GSA/gsa_step2_rubel.sh \
#       crop_yield_corn_all.yaml sobol
#   sbatch --array=1-100 workflows/scripts/ShellScripts/GSA/gsa_step2_rubel.sh \
#       crop_yield_corn_all.yaml --partition-id 1
#
# Author: Yi Yang
# Date: August 2026
#=======================================================================================

# IMPORTANT: You MUST specify --array=1-N where N matches the simulation count from GSA Step 1
#SBATCH --job-name=gsaS2
#SBATCH --ntasks=1
#SBATCH --mem-per-cpu=1GB
#SBATCH --exclude=rubel-030
#SBATCH --partition=rubel

##SBATCH --output=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/GSA_Step2_stdout_%A-%a.log
##SBATCH --error=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/GSA_Step2_stderr_%A-%a.log
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=BEGIN,END,FAIL

# =======================================================================================
# Project root (absolute for SLURM spool compatibility)
# =======================================================================================
PROJECT_ROOT="/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

# =======================================================================================
# Config (required $1); GSA method (optional, default soboljansen); extra flags
# =======================================================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch --array=1-N $0 <config.yaml> [gsa_method] [--partition-id N ...]"
  echo ""
  echo "Examples:"
  echo "  sbatch --array=1-66560 $0 crop_yield_corn_all.yaml"
  echo "  sbatch --array=1-66560 $0 crop_yield_corn_C6.yaml soboljansen"
  echo "  sbatch --array=1-66560 $0 crop_yield_corn_all.yaml sobol"
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

# Remaining args forwarded to R (e.g. --partition-id 3)
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

SIM_ID="${SLURM_ARRAY_TASK_ID:-}"
if [[ -z "$SIM_ID" ]]; then
  echo "ERROR: SLURM_ARRAY_TASK_ID is not set."
  echo "Submit with an array, e.g.:"
  echo "  sbatch --array=1-66560 $0 $CONFIG_ARG [$GSA_METHOD]"
  exit 1
fi

# Prefer shared RScripts tree
WRAPPER_SCRIPT="$PROJECT_ROOT/workflows/scripts/RScripts/GSA/gsa_step2_simulate.R"
if [[ ! -f "$WRAPPER_SCRIPT" ]]; then
  WRAPPER_SCRIPT="$PROJECT_ROOT/workflows/scripts/crop_yield_all/GSA/gsa_step2_simulate.R"
fi

# DayCent executable from YAML daycent.executable (not hard-coded binaries)
# Must match package default: file.path(lairice_root, config$daycent$executable)
DAYCENT_REL=$(awk '
  /^daycent:/ { in_sec=1; next }
  /^[a-zA-Z]/ && in_sec { in_sec=0 }
  in_sec && /^[[:space:]]*executable:/ {
    sub(/^[[:space:]]*executable:[[:space:]]*/, "")
    gsub(/"/, ""); sub(/#.*/, ""); sub(/[[:space:]]+$/, ""); print; exit
  }
' "$CONFIG_PATH")
if [[ -z "$DAYCENT_REL" ]]; then
  echo "ERROR: daycent.executable not found in config: $CONFIG_PATH"
  echo "Set daycent.executable in the YAML (see calibration_config_template.yaml)."
  exit 1
fi
if [[ "$DAYCENT_REL" == /* ]]; then
  DAYCENT_EXE="$DAYCENT_REL"
else
  DAYCENT_EXE="$PROJECT_ROOT/$DAYCENT_REL"
fi
if [[ ! -f "$DAYCENT_EXE" && ! -x "$DAYCENT_EXE" ]]; then
  echo "WARNING: DayCent executable from YAML not found at: $DAYCENT_EXE"
  echo "  (from daycent.executable: $DAYCENT_REL)"
fi

# Scratch directory from config (cluster.scratch_dir)
CONFIG_SCRATCH_DIR=$(grep "scratch_dir:" "$CONFIG_PATH" | grep -v "#" | head -1 | sed 's/.*scratch_dir:[[:space:]]*"\{0,1\}\([^"#]*\)"\{0,1\}.*/\1/' | tr -d ' ')
if [[ -z "$CONFIG_SCRATCH_DIR" ]]; then
  CONFIG_SCRATCH_DIR="/scratch/daycent_calibration"
fi
# Absolute scratch if relative
if [[ "$CONFIG_SCRATCH_DIR" != /* ]]; then
  CONFIG_SCRATCH_DIR="$PROJECT_ROOT/$CONFIG_SCRATCH_DIR"
fi
SCRATCH_DIR="${CONFIG_SCRATCH_DIR}/daycent_gsa_${SLURM_JOB_ID}_${SIM_ID}"

echo "=== GSA Step 2 Individual Simulation Started ==="
date
echo "Job ID: $SLURM_JOB_ID"
echo "Array Task ID (Simulation ID): $SIM_ID"
echo "Node: $SLURM_JOB_NODELIST"
echo "Config argument: $CONFIG_ARG"
echo "Configuration: $CONFIG_PATH"
echo "GSA Method: $GSA_METHOD (default soboljansen if not provided)"
echo "Root directory: $PROJECT_ROOT"
echo "Wrapper script: $WRAPPER_SCRIPT"
echo "DayCent executable (from YAML daycent.executable): $DAYCENT_EXE"
echo "Scratch directory: $SCRATCH_DIR"
echo "Working directory: $(pwd)"
if [[ ${#EXTRA_ARGS[@]} -gt 0 ]]; then
  echo "Extra arguments: ${EXTRA_ARGS[*]}"
fi

if [[ ! -f "$WRAPPER_SCRIPT" ]]; then
  echo "ERROR: R script not found: $WRAPPER_SCRIPT"
  exit 1
fi

echo ""
echo "=== Starting Individual GSA Step 2 Simulation ==="
echo "Command: Rscript --vanilla $WRAPPER_SCRIPT $CONFIG_PATH $GSA_METHOD $SIM_ID $DAYCENT_EXE $SCRATCH_DIR ${EXTRA_ARGS[*]}"
echo ""

# R expects: config, method, sim_id, [daycent_exe], [scratch_dir], [--partition-id N]
Rscript --vanilla \
  "$WRAPPER_SCRIPT" \
  "$CONFIG_PATH" \
  "$GSA_METHOD" \
  "$SIM_ID" \
  "$DAYCENT_EXE" \
  "$SCRATCH_DIR" \
  "${EXTRA_ARGS[@]}"

EXIT_STATUS=$?

echo ""
echo "=== GSA Step 2 Job Completed ==="
date
echo "Exit Status: $EXIT_STATUS"

exit $EXIT_STATUS
