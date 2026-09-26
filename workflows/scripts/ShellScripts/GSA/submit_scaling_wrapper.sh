#!/usr/bin/env bash
#=======================================================================================
# GSA Step 2 Scaling Mode Wrapper Script - Version 2 (Partition-Based Strategy)
#
# Submits one SLURM job per site partition; each job uses --array over parameter sets.
#
# USAGE:
#   bash submit_scaling_wrapper.sh [OPTIONS] <config.yaml>
#
# Options:
#   --partitions <SPEC>   Partition specification (default: all)
#                         Examples: "1", "1-5", "1,3,5", "all"
#   --samples <SPEC>      Sample/parameter specification (default: all)
#                         Examples: "1-10", "1,5,10", "all", "1-66560%750"
#   --method <NAME>       GSA method name (default: soboljansen)
#   --help                Show this help message
#
# Arguments:
#   config.yaml           Required: basename, name.yaml, or path under workflows/configs/
#
# Examples:
#   # All partitions × all samples, method soboljansen (default)
#   bash workflows/scripts/ShellScripts/GSA/submit_scaling_wrapper.sh crop_yield_corn_all.yaml
#
#   # Test: 2 partitions, first 10 samples
#   bash .../submit_scaling_wrapper.sh --partitions 1-2 --samples 1-10 crop_yield_corn_all.yaml
#
#   # Explicit method override
#   bash .../submit_scaling_wrapper.sh --method sobol --samples 1-100 crop_yield_corn_C6.yaml
#
#   # Throttle array
#   bash .../submit_scaling_wrapper.sh --partitions 1-3 --samples '1-3%750' crop_yield_corn_all.yaml
#
# Author: Yi Yang
# Date: October 2025
#=======================================================================================

# Default values
PARTITION_SPEC="all"
SAMPLE_SPEC="all"
GSA_METHOD="soboljansen"   # default method unless --method is given
CONFIG_ARG=""

# Parse command line options
while [[ $# -gt 0 ]]; do
    case $1 in
        --partitions)
            PARTITION_SPEC="$2"
            shift 2
            ;;
        --samples)
            SAMPLE_SPEC="$2"
            shift 2
            ;;
        --method)
            GSA_METHOD="$2"
            shift 2
            ;;
        --help)
            grep "^#" "$0" | grep -v "#!/usr/bin/env" | sed 's/^# \?//'
            exit 0
            ;;
        -*)
            echo "ERROR: Unknown option: $1"
            echo "Use --help for usage."
            exit 1
            ;;
        *)
            if [[ -z "$CONFIG_ARG" ]]; then
                CONFIG_ARG="$1"
            else
                echo "ERROR: Unexpected argument: $1"
                echo "Usage: bash $0 [OPTIONS] <config.yaml>"
                exit 1
            fi
            shift
            ;;
    esac
done

if [[ -z "$CONFIG_ARG" ]]; then
    echo "ERROR: YAML config argument is required."
    echo ""
    echo "Usage: bash $0 [OPTIONS] <config.yaml>"
    echo ""
    echo "Examples:"
    echo "  bash $0 crop_yield_corn_all.yaml"
    echo "  bash $0 --partitions 1-2 --samples 1-10 crop_yield_corn_C6.yaml"
    echo "  bash $0 --method sobol crop_yield_corn_all.yaml"
    exit 1
fi

# Use absolute paths
PROJECT_ROOT="/data/rubelscratch/rubelogle/daycent_calibration"
CONFIGS_DIR="$PROJECT_ROOT/workflows/configs"

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
    exit 1
fi

echo "=== GSA Step 2 Scaling Mode Wrapper (v2: Partition-Based Strategy) ==="
echo "Config argument: $CONFIG_ARG"
echo "Config: $CONFIG_PATH"
echo "GSA method: $GSA_METHOD (default soboljansen if not set via --method)"
echo "Partition spec: $PARTITION_SPEC"
echo "Sample spec: $SAMPLE_SPEC"
echo ""

# Read partition / MC configuration for selected method
echo "Reading partition configuration for method: $GSA_METHOD ..."
PARTITION_INFO=$(Rscript --vanilla --slave -e "
suppressMessages({
  .libPaths('$PROJECT_ROOT/rlib')
  library(yaml)
})
config <- yaml::read_yaml('$CONFIG_PATH')
gsa_method <- '$GSA_METHOD'

# Determine date stamp
if (identical(config\$project\$date_stamp, 'auto')) {
  date_stamp <- format(Sys.Date(), '%d%b%Y')
} else {
  date_stamp <- config\$project\$date_stamp
}

# Prefer output_base from config when present
output_base <- config\$paths\$output_base
if (is.null(output_base) || !nzchar(output_base)) {
  output_base <- file.path('results', config\$project\$name)
}
gsa_method_dir <- file.path('$PROJECT_ROOT', output_base, date_stamp, 'GSA', gsa_method)

if (!dir.exists(gsa_method_dir)) {
  # Fallback: results/<project>/<date>/GSA/<method>
  gsa_method_dir <- file.path('$PROJECT_ROOT', 'results', config\$project\$name, date_stamp, 'GSA', gsa_method)
}

if (!dir.exists(gsa_method_dir)) {
  cat('ERROR: GSA method directory not found:', gsa_method_dir, '\\n', file=stderr())
  quit(status = 1)
}

pa_file <- file.path(gsa_method_dir, 'point_assignments.rds')
if (!file.exists(pa_file)) {
  cat('ERROR: point_assignments.rds not found at', pa_file, '\\n', file=stderr())
  cat('Run GSA Step 1 with scaling mode enabled first.\\n', file=stderr())
  quit(status = 1)
}

pa <- readRDS(pa_file)
n_partitions <- max(pa\$job_id)

points_per_job <- config\$scaling\$points_per_job
if (is.null(points_per_job)) {
  points_per_job <- config\$step2_execution\$scaling_config\$points_per_job
}
if (is.null(points_per_job)) {
  points_per_job <- 200
}

mc_file <- file.path(gsa_method_dir, paste0('mc_GSA_draw_', gsa_method, '.rds'))
if (!file.exists(mc_file)) {
  cat('ERROR: MC draw file not found at', mc_file, '\\n', file=stderr())
  quit(status = 1)
}
mc_draw <- readRDS(mc_file)
n_parameter_sets <- nrow(mc_draw)

# Output: method n_partitions n_parameter_sets points_per_job
cat(gsa_method, n_partitions, n_parameter_sets, points_per_job, sep=' ')
" 2>&1)

# Check if detection succeeded
if [[ $? -ne 0 ]] || [[ -z "$PARTITION_INFO" ]] || [[ "$PARTITION_INFO" == ERROR* ]]; then
    echo "ERROR: Failed to read configuration"
    echo "$PARTITION_INFO"
    echo ""
    echo "Make sure GSA Step 1 has been run successfully with scaling mode enabled"
    echo "and that results exist under GSA/${GSA_METHOD}/ (point_assignments.rds, mc draws)."
    exit 1
fi

# Parse partition info
GSA_METHOD=$(echo "$PARTITION_INFO" | awk '{print $1}')
N_PARTITIONS_TOTAL=$(echo "$PARTITION_INFO" | awk '{print $2}')
N_PARAMETER_SETS_TOTAL=$(echo "$PARTITION_INFO" | awk '{print $3}')
POINTS_PER_JOB=$(echo "$PARTITION_INFO" | awk '{print $4}')

echo "GSA method: $GSA_METHOD"
echo "Total partitions available: $N_PARTITIONS_TOTAL (~$POINTS_PER_JOB points each)"
echo "Total parameter sets available: $N_PARAMETER_SETS_TOTAL"
echo ""

# Parse partition specification into array
PARTITIONS_TO_RUN=()
if [[ "$PARTITION_SPEC" == "all" ]]; then
    PARTITIONS_TO_RUN=($(seq 1 $N_PARTITIONS_TOTAL))
elif [[ "$PARTITION_SPEC" =~ ^[0-9]+-[0-9]+$ ]]; then
    # Range format (e.g., "1-5")
    START=$(echo "$PARTITION_SPEC" | cut -d'-' -f1)
    END=$(echo "$PARTITION_SPEC" | cut -d'-' -f2)
    if [ "$END" -gt "$N_PARTITIONS_TOTAL" ]; then
        echo "WARNING: Requested partition range $PARTITION_SPEC exceeds available partitions ($N_PARTITIONS_TOTAL)"
        echo "         Adjusting to 1-$N_PARTITIONS_TOTAL"
        END=$N_PARTITIONS_TOTAL
    fi
    PARTITIONS_TO_RUN=($(seq $START $END))
elif [[ "$PARTITION_SPEC" =~ , ]]; then
    # Comma-separated list (e.g., "1,3,5")
    IFS=',' read -ra PARTITIONS_TO_RUN <<< "$PARTITION_SPEC"
else
    # Single partition
    PARTITIONS_TO_RUN=("$PARTITION_SPEC")
fi

# Parse sample specification into SLURM array format
# Support throttle suffix: 1-100%50 -> array "1-100%50"
SAMPLE_THROTTLE=""
SAMPLE_CORE="$SAMPLE_SPEC"
if [[ "$SAMPLE_SPEC" == *%* ]]; then
    SAMPLE_CORE="${SAMPLE_SPEC%%\%*}"
    SAMPLE_THROTTLE="%${SAMPLE_SPEC#*%}"
fi

if [[ "$SAMPLE_CORE" == "all" ]]; then
    ARRAY_SPEC="1-${N_PARAMETER_SETS_TOTAL}${SAMPLE_THROTTLE}"
    N_SAMPLES=${N_PARAMETER_SETS_TOTAL}
elif [[ "$SAMPLE_CORE" =~ ^[0-9]+-[0-9]+$ ]]; then
    ARRAY_SPEC="${SAMPLE_CORE}${SAMPLE_THROTTLE}"
    START=$(echo "$SAMPLE_CORE" | cut -d'-' -f1)
    END=$(echo "$SAMPLE_CORE" | cut -d'-' -f2)
    N_SAMPLES=$((END - START + 1))
    if [ "$END" -gt "$N_PARAMETER_SETS_TOTAL" ]; then
        echo "WARNING: Requested sample range $SAMPLE_CORE exceeds available samples ($N_PARAMETER_SETS_TOTAL)"
    fi
elif [[ "$SAMPLE_CORE" =~ , ]]; then
    ARRAY_SPEC="${SAMPLE_CORE}${SAMPLE_THROTTLE}"
    N_SAMPLES=$(echo "$SAMPLE_CORE" | tr ',' '\n' | wc -l)
else
    ARRAY_SPEC="${SAMPLE_CORE}${SAMPLE_THROTTLE}"
    N_SAMPLES=1
fi

echo "Partitions to run: ${PARTITIONS_TO_RUN[@]} (${#PARTITIONS_TO_RUN[@]} total)"
echo "Samples per partition: $ARRAY_SPEC ($N_SAMPLES total)"
echo "Total simulations: $((${#PARTITIONS_TO_RUN[@]} * N_SAMPLES))"
echo ""
echo "Strategy: Submit ${#PARTITIONS_TO_RUN[@]} main jobs (one per partition)"
echo "          Each job processes $N_SAMPLES parameter sets using --array"
echo "          Total queue entries: ${#PARTITIONS_TO_RUN[@]}"
echo ""

# Path to shared Step 2 SLURM script (updated CLI: config [method] --partition-id N)
SLURM_SCRIPT="$PROJECT_ROOT/workflows/scripts/ShellScripts/GSA/gsa_step2_rubel.sh"
if [ ! -f "$SLURM_SCRIPT" ]; then
    SLURM_SCRIPT="$PROJECT_ROOT/workflows/scripts/crop_yield_all/GSA/gsa_step2_rubel.sh"
fi

if [ ! -f "$SLURM_SCRIPT" ]; then
    echo "ERROR: SLURM script not found: $SLURM_SCRIPT"
    exit 1
fi

# Track all job IDs
ALL_JOB_IDS=()

# Submit one job per selected partition, with array for selected parameter sets
echo "=== Submitting Jobs ==="
for partition in "${PARTITIONS_TO_RUN[@]}"; do
    echo -n "Partition $partition/${N_PARTITIONS_TOTAL}... "

    # New gsa_step2_rubel.sh: sbatch ... script.sh <config.yaml> [method] --partition-id N
    JOB_OUTPUT=$(sbatch --array=${ARRAY_SPEC} \
                        --job-name="gsa_p${partition}" \
                        --output=/dev/null \
                        --error=/dev/null \
                        "$SLURM_SCRIPT" "$CONFIG_PATH" "$GSA_METHOD" --partition-id "$partition" 2>&1)

    if [ $? -eq 0 ]; then
        JOB_ID=$(echo "$JOB_OUTPUT" | grep -oP 'Submitted batch job \K[0-9]+' || echo "$JOB_OUTPUT" | awk '/Submitted batch job/{print $NF}')
        ALL_JOB_IDS+=("$JOB_ID")
        echo "✓ Job $JOB_ID (array $ARRAY_SPEC)"
    else
        echo "✗ FAILED"
        echo "    Error: $JOB_OUTPUT"
    fi

    # Small delay to avoid overwhelming scheduler
    sleep 0.1
done

echo ""
echo "=== Submission Complete ==="
echo "Total jobs submitted: ${#ALL_JOB_IDS[@]} (one per partition)"
echo "Total array tasks: $((${#PARTITIONS_TO_RUN[@]} * N_SAMPLES))"
echo "Job IDs: ${ALL_JOB_IDS[@]}"
echo ""
echo "Monitor with:"
echo "  squeue -u \$USER | grep gsa_p"
if [ ${#ALL_JOB_IDS[@]} -gt 0 ]; then
    echo "  squeue -j $(echo ${ALL_JOB_IDS[@]} | tr ' ' ',')"
fi
echo ""
echo "After ALL jobs complete, run aggregation:"
AGG_SCRIPT_R="$PROJECT_ROOT/workflows/scripts/ShellScripts/GSA/gsa_step2_aggregate_rubel.sh"
if [[ "$SAMPLE_SPEC" == "all" ]] || [[ "$SAMPLE_CORE" == "all" ]]; then
    echo "  sbatch --array=1-${N_PARAMETER_SETS_TOTAL} $AGG_SCRIPT_R $CONFIG_ARG $GSA_METHOD"
else
    echo "  sbatch --array=${ARRAY_SPEC} $AGG_SCRIPT_R $CONFIG_ARG $GSA_METHOD"
    echo ""
    echo "  NOTE: Running partial samples ($SAMPLE_SPEC). For testing only."
fi
echo ""
