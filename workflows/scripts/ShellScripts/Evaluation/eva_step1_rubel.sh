#!/bin/bash
#=======================================================================================
# Evaluation Step 1 - RunFile and output-layout setup (SLURM)
#
# Creates Evaluation/RunFile.rds, mc_EVA_draw.rds, and either job-group folders
# or database result tables. Does not run DayCent.
#
# USAGE:
#   sbatch workflows/scripts/ShellScripts/Evaluation/eva_step1_rubel.sh <config.yaml> [date_stamp]
#
# Arguments:
#   config.yaml   - Required: basename, name.yaml, or path under workflows/configs/
#   date_stamp    - Optional: override project date_stamp (e.g. 30Jun2026)
#
# Examples:
#   sbatch workflows/scripts/ShellScripts/Evaluation/eva_step1_rubel.sh soil_organic_carbon.yaml
#   sbatch workflows/scripts/ShellScripts/Evaluation/eva_step1_rubel.sh soil_organic_carbon.yaml 30Jun2026
#
# Local test:
#   bash workflows/scripts/ShellScripts/Evaluation/eva_step1_rubel.sh soil_organic_carbon.yaml
#
#=======================================================================================

#SBATCH --job-name=evaS1
#SBATCH --ntasks=1
#SBATCH --partition=hipri

#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/EVA_Step1_stdout_%A.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/EVA_Step1_stderr_%A.log
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null

# =======================================================================================
# Project root (absolute for SLURM spool compatibility)
# =======================================================================================
PROJECT_ROOT="/data/rubelscratch/rubelogle/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

EVA_STEP1_SCRIPT="$PROJECT_ROOT/workflows/scripts/RScripts/Evaluation/eva_step1_setup.R"
if [[ ! -f "$EVA_STEP1_SCRIPT" ]]; then
  EVA_STEP1_SCRIPT="$PROJECT_ROOT/workflows/scripts/soil_organic_carbon/Evaluation/eva_step1_setup.R"
fi

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
  echo "  sbatch $0 soil_organic_carbon.yaml"
  echo "  sbatch $0 soil_organic_carbon.yaml 30Jun2026"
  exit 1
fi
CONFIG_ARG="$1"
DATE_STAMP_OVERRIDE="${2:-}"

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

if [[ ! -f "$EVA_STEP1_SCRIPT" ]]; then
  echo "ERROR: R script not found: $EVA_STEP1_SCRIPT"
  exit 1
fi

echo "======================================================================"
echo "Evaluation Step 1 - RunFile and output-layout setup"
echo "======================================================================"
date
echo "Config argument: $CONFIG_ARG"
echo "Config file:     $CONFIG_PATH"
echo "R script:        $EVA_STEP1_SCRIPT"
if [[ -n "$DATE_STAMP_OVERRIDE" ]]; then
  echo "Date stamp:      $DATE_STAMP_OVERRIDE (CLI override)"
else
  echo "Date stamp:      from config"
fi
echo "Node:            $(hostname)"
echo "======================================================================"

if [[ -n "$DATE_STAMP_OVERRIDE" ]]; then
  echo "Executing: Rscript --vanilla $EVA_STEP1_SCRIPT $CONFIG_PATH $DATE_STAMP_OVERRIDE"
  Rscript --vanilla "$EVA_STEP1_SCRIPT" "$CONFIG_PATH" "$DATE_STAMP_OVERRIDE"
else
  echo "Executing: Rscript --vanilla $EVA_STEP1_SCRIPT $CONFIG_PATH"
  Rscript --vanilla "$EVA_STEP1_SCRIPT" "$CONFIG_PATH"
fi

EXIT_STATUS=$?
echo "======================================================================"
date
echo "Evaluation Step 1 finished with exit status: $EXIT_STATUS"
echo "======================================================================"
exit $EXIT_STATUS
