#!/usr/bin/env bash
#=======================================================================================
# SIR Step 2 - Model Simulation (SLURM)
#
# Each array task runs ONE SIR Monte Carlo simulation (sim_id = SLURM_ARRAY_TASK_ID).
#
# USAGE:
#   sbatch --array=1-N sir_step2_rubel.sh <config.yaml> [extra args...]
#
# Arguments:
#   config.yaml  - Required: basename, name.yaml, or path under workflows/configs/
#   extra args   - Optional: e.g. --partition-id 3 for scaling mode
#
# Examples:
#   sbatch --array=1-100000 workflows/scripts/ShellScripts/SIR/sir_step2_rubel.sh \
#       crop_yield_corn_all.yaml
#   sbatch --array=1-250000 workflows/scripts/ShellScripts/SIR/sir_step2_rubel.sh \
#       crop_yield_corn_m2.yaml 
#
# R: sir_step2_simulate.R <config> <sim_id> [daycent_exe] [scratch_dir] [--partition-id N]
#
#=======================================================================================

#SBATCH --job-name=sirS2
#SBATCH --ntasks=1
#SBATCH --partition=rubel
#SBATCH --exclude=rubel-041,rubel-031
#SBATCH --mem-per-cpu=800

##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=BEGIN,END,FAIL

##SBATCH --output=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/SIR_Step2_stdout_%A-%a.log
##SBATCH --error=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/SIR_Step2_stderr_%A-%a.log
#SBATCH --output=/dev/null
#SBATCH --error=/dev/null

# =======================================================================================
# Project root (absolute for SLURM spool compatibility)
# =======================================================================================
PROJECT_ROOT="/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

# =======================================================================================
# Config (required $1); remaining args e.g. --partition-id N
# =======================================================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch --array=1-N $0 <config.yaml> [--partition-id N ...]"
  echo ""
  echo "Examples:"
  echo "  sbatch --array=1-100000 $0 crop_yield_corn_all.yaml"
  echo "  sbatch --array=1-250000 $0 crop_yield_corn_m2.yaml"
  echo "  sbatch --array=1-100    $0 crop_yield_corn_all.yaml --partition-id 1"
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

SIM_ID="${SLURM_ARRAY_TASK_ID:-}"
if [[ -z "$SIM_ID" ]]; then
  echo "ERROR: SLURM_ARRAY_TASK_ID is not set."
  echo "Submit with an array, e.g.:"
  echo "  sbatch --array=1-100000 $0 $CONFIG_ARG"
  exit 1
fi

START_ID=0
ACTUAL_SIM_ID=$((START_ID + SIM_ID))

SIR_STEP2_SCRIPT="$PROJECT_ROOT/workflows/scripts/RScripts/SIR/sir_step2_simulate.R"
if [[ ! -f "$SIR_STEP2_SCRIPT" ]]; then
  SIR_STEP2_SCRIPT="$PROJECT_ROOT/workflows/scripts/crop_yield_all/SIR/sir_step2_simulate.R"
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

# Scratch directory from config
CONFIG_SCRATCH_DIR=$(grep "scratch_dir:" "$CONFIG_PATH" | grep -v "#" | head -1 | sed 's/.*scratch_dir:[[:space:]]*"\{0,1\}\([^"#]*\)"\{0,1\}.*/\1/' | tr -d ' ')
if [[ -z "$CONFIG_SCRATCH_DIR" ]]; then
  CONFIG_SCRATCH_DIR="/scratch/daycent_calibration"
fi
if [[ "$CONFIG_SCRATCH_DIR" != /* ]]; then
  CONFIG_SCRATCH_DIR="$PROJECT_ROOT/$CONFIG_SCRATCH_DIR"
fi
SCRATCH_DIR="${CONFIG_SCRATCH_DIR}/daycent_sir_${SLURM_JOB_ID}_${SIM_ID}"

echo "======================================================================"
echo "SIR Step 2 - Model Simulation"
date
echo "Job ID: ${SLURM_JOB_ID}"
echo "Array Task ID: ${SIM_ID}"
echo "Actual Simulation ID: ${ACTUAL_SIM_ID}"
echo "Node: ${SLURM_NODELIST}"
echo "Config argument: ${CONFIG_ARG}"
echo "Config File: ${CONFIG_PATH}"
echo "R script: ${SIR_STEP2_SCRIPT}"
echo "DayCent executable (from YAML daycent.executable): ${DAYCENT_EXE}"
echo "Scratch Directory: ${SCRATCH_DIR}"
if [[ ${#EXTRA_ARGS[@]} -gt 0 ]]; then
  echo "Extra arguments: ${EXTRA_ARGS[*]}"
fi
echo "======================================================================"

if [[ ! -f "$SIR_STEP2_SCRIPT" ]]; then
  echo "ERROR: R script not found: $SIR_STEP2_SCRIPT"
  exit 1
fi

# R expects: config, sim_id, [daycent_exe], [scratch_dir], [--partition-id N]
Rscript --vanilla \
  "$SIR_STEP2_SCRIPT" \
  "$CONFIG_PATH" \
  "$SIM_ID" \
  "$DAYCENT_EXE" \
  "$SCRATCH_DIR" \
  "${EXTRA_ARGS[@]}"

EXIT_CODE=$?

echo "======================================================================"
echo "SIR Step 2 simulation completed with exit code: ${EXIT_CODE}"
echo "Simulation ID: ${ACTUAL_SIM_ID}"
date
echo "======================================================================"

exit ${EXIT_CODE}
