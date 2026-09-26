#!/bin/bash

# =======================================================================================
#  PURPOSE:   SLURM script for GSA Step 1 using migrated R package and YAML config
#             All configuration is now in YAML file for maximum portability
#
#  AUTHOR:    Yi Yang
#             Migration from original GSA_Step1_MCdraw_SetUp_rubel.sh
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
# =======================================================================================
#
# USAGE:
#   sbatch workflows/scripts/ShellScripts/GSA/gsa_step1_rubel.sh <config.yaml>
#
# Examples (config lives under workflows/configs/ unless you pass a full path):
#   sbatch workflows/scripts/ShellScripts/GSA/gsa_step1_rubel.sh crop_yield_corn_all.yaml
#   sbatch workflows/scripts/ShellScripts/GSA/gsa_step1_rubel.sh crop_yield_corn_C6.yaml
#   sbatch workflows/scripts/ShellScripts/GSA/gsa_step1_rubel.sh soil_organic_carbon.yaml
#   sbatch workflows/scripts/ShellScripts/GSA/gsa_step1_rubel.sh crop_yield_corn_all
#       # .yaml is optional; basename without extension is accepted
#
# Local test (no SLURM array; default task_id=6):
#   bash workflows/scripts/ShellScripts/GSA/gsa_step1_rubel.sh crop_yield_corn_all.yaml
#
# =======================================================================================

# SLURM directives - these should match the YAML config
#SBATCH --array=6          # Array jobs for 8 GSA methods #1-8
#SBATCH -p rubel             # Partition (from yaml: slurm.partition)
#SBATCH --job-name=gsaS1  # Job name (from yaml: slurm.job_name)
#SBATCH --ntasks=1           # Tasks per job (from yaml: slurm.ntasks)
#SBATCH --output=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/GSA_Step1_stdout_%A-%a.log
#SBATCH --error=/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration/LogFiles/GSA_Step1_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu  # From yaml: slurm.mail_user
##SBATCH --mail-type=ALL      # From yaml: slurm.mail_type

# =======================================================================================
# CONFIGURATION - All paths now come from YAML
# =======================================================================================

# Hardcode project root for SLURM compatibility
# (SLURM spool can make relative paths unreliable)
PROJECT_ROOT="/nfs/ogle/DayCent_Model_Development/temp/test_tbd/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

# ---------------------------------------------------------------------------
# Config selection: REQUIRED first CLI arg (works with sbatch and bash)
#   sbatch gsa_step1_rubel.sh crop_yield_corn_all.yaml
#   sbatch gsa_step1_rubel.sh crop_yield_corn_C6.yaml
# SLURM passes any tokens after the script name as $1, $2, ...
# ---------------------------------------------------------------------------
if [[ -z "${1:-}" ]]; then
    echo "ERROR: YAML config argument is required."
    echo ""
    echo "Usage:"
    echo "  sbatch $0 <config.yaml>"
    echo "  bash   $0 <config.yaml>"
    echo ""
    echo "Examples:"
    echo "  sbatch $0 crop_yield_corn_all.yaml"
    echo "  sbatch $0 crop_yield_corn_C6.yaml"
    echo "  sbatch $0 soil_organic_carbon.yaml"
    exit 1
fi
CONFIG_ARG="$1"

resolve_config_path() {
    local arg="$1"
    local path=""

    # Absolute or existing path as given
    if [[ "$arg" == /* ]] && [[ -f "$arg" ]]; then
        path="$arg"
    elif [[ -f "$arg" ]]; then
        # Relative path that exists from the submit cwd
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
    echo "Usage: sbatch $0 <config.yaml>"
    exit 1
fi

echo "Using config: $CONFIG_PATH"

# Bash YAML parser functions (no Python dependencies)
parse_yaml_value() {
    local yaml_file="$1"
    local yaml_path="$2"

    # Convert dot notation to grep pattern (e.g., "paths.lairice_root" -> find "lairice_root:" under "paths:")
    if [[ "$yaml_path" == *"."* ]]; then
        local section="${yaml_path%.*}"
        local key="${yaml_path##*.}"
        # Find section, then find key within that section
        awk "
        /^${section}:/ { in_section=1; next }
        /^[a-zA-Z]/ && in_section { in_section=0 }
        in_section && /^[[:space:]]*${key}:/ {
            gsub(/^[[:space:]]*${key}:[[:space:]]*/, \"\")
            gsub(/\"/, \"\")
            gsub(/#.*/, \"\")
            gsub(/[[:space:]]*$/, \"\")
            print
            exit
        }" "$yaml_file"
    else
        # Simple key lookup
        grep "^[[:space:]]*${yaml_path}:" "$yaml_file" | sed 's/^[[:space:]]*'${yaml_path}':[[:space:]]*//' | sed 's/"//g' | sed 's/#.*//' | sed 's/[[:space:]]*$//'
    fi
}

# Parse YAML to get required paths using bash functions
LAIRICE_ROOT=$(parse_yaml_value "$CONFIG_PATH" "paths.lairice_root")
R_SCRIPT_REL=$(parse_yaml_value "$CONFIG_PATH" "scripts.gsa_step1")

# Prefer shared RScripts tree (same convention as other ShellScripts/* steps).
# Fall back to YAML scripts.gsa_step1 (legacy crop_yield_all / workflow paths),
# then crop_yield_all/GSA.
R_SCRIPT_FULL="$PROJECT_ROOT/workflows/scripts/RScripts/GSA/gsa_step1_setup.R"
if [[ ! -f "$R_SCRIPT_FULL" ]]; then
    if [[ -n "$R_SCRIPT_REL" ]]; then
        if [[ "$R_SCRIPT_REL" == /* ]]; then
            R_SCRIPT_FULL="$R_SCRIPT_REL"
        else
            R_SCRIPT_FULL="${LAIRICE_ROOT}/${R_SCRIPT_REL}"
        fi
    else
        R_SCRIPT_FULL=""
    fi
fi
if [[ -z "$R_SCRIPT_FULL" || ! -f "$R_SCRIPT_FULL" ]]; then
    R_SCRIPT_FULL="$PROJECT_ROOT/workflows/scripts/crop_yield_all/GSA/gsa_step1_setup.R"
fi

# Ensure LogFiles directory exists
LOG_DIR_REL=$(parse_yaml_value "$CONFIG_PATH" "slurm.log_dir")
if [[ -n "$LOG_DIR_REL" ]]; then
    LOG_DIR="$LAIRICE_ROOT/$LOG_DIR_REL"
else
    LOG_DIR="$PROJECT_ROOT/LogFiles"
fi
if [[ ! -d "$LOG_DIR" ]]; then
    echo "Creating log directory: $LOG_DIR"
    mkdir -p "$LOG_DIR"
fi

# =======================================================================================
# EXECUTION
# =======================================================================================

echo "======================================================================"
echo "GSA Step 1 - Monte Carlo Draw Setup (Migrated Version)"
echo "======================================================================"
echo "Config argument: $CONFIG_ARG"
echo "Config file: $CONFIG_PATH"
echo "LAIRICE_ROOT: $LAIRICE_ROOT"
echo "R Script: $R_SCRIPT_FULL"
echo "SLURM Array Task ID: ${SLURM_ARRAY_TASK_ID:-<not set>}"
echo "Working Directory: $(pwd)"
echo "Node: $(hostname)"
echo "Date: $(date)"
echo "======================================================================"

if [[ ! -f "$R_SCRIPT_FULL" ]]; then
    echo "ERROR: R script not found: $R_SCRIPT_FULL"
    echo "Set scripts.gsa_step1 in the YAML, or place gsa_step1_setup.R under RScripts/GSA."
    exit 1
fi

# Execute the R script with 2 arguments: config file and task ID
# Use default task_id if SLURM_ARRAY_TASK_ID is not set (for testing)
TASK_ID="${SLURM_ARRAY_TASK_ID:-6}"
echo "Executing: Rscript --vanilla \"$R_SCRIPT_FULL\" \"$CONFIG_PATH\" \"$TASK_ID\""
Rscript --vanilla "$R_SCRIPT_FULL" "$CONFIG_PATH" "$TASK_ID"

EXIT_STATUS=$?

echo "======================================================================"
echo "GSA Step 1 completed with exit status: $EXIT_STATUS"
echo "Date: $(date)"
echo "======================================================================"

exit $EXIT_STATUS
