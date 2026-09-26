#!/bin/bash

####========================================================
#### GSA Step 3 Likelihood QC
####========================================================
#
# USAGE:
#   sbatch workflows/scripts/ShellScripts/GSA/gsa_step3_likelihood_qc.sh \
#       <config.yaml> [gsa_method]
#
# Arguments:
#   config.yaml  - Required: config basename, name.yaml, or path under workflows/configs/
#   gsa_method   - Optional: method name (default: soboljansen)
#
# Examples:
#   sbatch .../gsa_step3_likelihood_qc.sh crop_yield_corn_all.yaml 
#   sbatch .../gsa_step3_likelihood_qc.sh crop_yield_corn_all.yaml sobol
#
# R script: gsa_step3_likelihood_qc.R <config> <gsa_method>
#   - Checks likelihood table vs mc_GSA_draw_<method>.rds counts
#
####========================================================

#SBATCH -J gsaS3_qc
#SBATCH -p rubel
#SBATCH --mem-per-cpu=10G
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --array=1
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null
#SBATCH --output=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/gsa_S3_likhd_qc_stdout_%A-%a.log
#SBATCH --error=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/gsa_S3_likhd_qc_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=ALL
#SBATCH --exclude=rubel-030

# =========================================================
# Project roots (absolute for SLURM spool compatibility)
# =========================================================
PROJECT_ROOT="/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"
R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/RScripts/GSA/gsa_step3_likelihood_qc.R"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  R_FULL_PATH_RUN="$PROJECT_ROOT/workflows/scripts/crop_yield_all/GSA/gsa_step3_likelihood_qc.R"
fi

# =========================================================
# Config (required $1); GSA method (optional $2, default soboljansen)
# =========================================================
if [[ -z "${1:-}" ]]; then
  echo "ERROR: YAML config argument is required."
  echo ""
  echo "Usage:"
  echo "  sbatch $0 <config.yaml> [gsa_method]"
  echo ""
  echo "Examples:"
  echo "  sbatch $0 crop_yield_corn_all.yaml"
  echo "  sbatch $0 crop_yield_corn_C6.yaml soboljansen"
  echo "  sbatch $0 crop_yield_corn_all.yaml sobol"
  exit 1
fi
CONFIG_ARG="$1"
GSA_METHOD="${2:-soboljansen}"

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
  echo "Usage: sbatch $0 <config.yaml> [gsa_method]"
  exit 1
fi

# =========================================================
# Run
# =========================================================
echo "======================================================================"
echo "GSA Step 3 Likelihood QC"
echo "Config argument: $CONFIG_ARG"
echo "Config file:     $CONFIG_PATH"
echo "R script:        $R_FULL_PATH_RUN"
echo "GSA Method:      $GSA_METHOD (default soboljansen if not provided)"
echo "Node:            $(hostname)"
echo "Date:            $(date)"
echo "======================================================================"

if [[ ! -f "$R_FULL_PATH_RUN" ]]; then
  echo "ERROR: R script not found: $R_FULL_PATH_RUN"
  exit 1
fi

# R expects: config_path, gsa_method
echo "Executing: Rscript \"$R_FULL_PATH_RUN\" \"$CONFIG_PATH\" \"$GSA_METHOD\""
Rscript "$R_FULL_PATH_RUN" "$CONFIG_PATH" "$GSA_METHOD"

EXIT_STATUS=$?
echo "GSA Step 3 Likelihood QC finished with exit status: $EXIT_STATUS"
exit $EXIT_STATUS
