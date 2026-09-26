#!/usr/bin/env bash
#
# Submit one SLURM array task per row in crop_id_chist_mapping.csv
#
# Usage:
#   bash workflows/scripts/RScripts/Validatiaon/run_val_stap1_aggre_all.sh
#   bash workflows/scripts/RScripts/Validatiaon/run_val_stap1_aggre_all.sh --local
#   bash workflows/scripts/RScripts/Validatiaon/run_val_stap1_aggre_all.sh --local 3
#
# Options:
#   --local       Run rows sequentially on the current machine (no sbatch)
#   --local N     Run only CSV row N locally

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"
MAPPING_CSV="${SCRIPT_DIR}/crop_id_chist_mapping.csv"
WORKER_SH="${SCRIPT_DIR}/val_stap1_aggre_rubel.sh"
MODE="submit"
LOCAL_ROW=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --local)
      MODE="local"
      shift
      if [[ $# -gt 0 && "$1" =~ ^[0-9]+$ ]]; then
        LOCAL_ROW="$1"
        shift
      fi
      ;;
    *)
      echo "ERROR: Unknown argument: $1" >&2
      echo "Usage: $0 [--local [ROW_ID]]" >&2
      exit 1
      ;;
  esac
done

if [[ -n "${PROJECT_ROOT:-}" ]]; then
  REPO_ROOT="${PROJECT_ROOT}"
fi

cd "${REPO_ROOT}"

if [[ ! -f "${MAPPING_CSV}" ]]; then
  echo "ERROR: Mapping file not found: ${MAPPING_CSV}"
  exit 1
fi

if [[ ! -f "${WORKER_SH}" ]]; then
  echo "ERROR: Worker script not found: ${WORKER_SH}"
  exit 1
fi

N_ROWS="$(Rscript --vanilla -e "
  suppressPackageStartupMessages(library(readr))
  cat(nrow(read_csv('${MAPPING_CSV}', show_col_types = FALSE)))
")"

echo "========================================================================"
echo "Crop validation aggregation batch"
echo "========================================================================"
echo "Repo root   : ${REPO_ROOT}"
echo "Mapping CSV : ${MAPPING_CSV}"
echo "Rows        : ${N_ROWS}"
echo "Mode        : ${MODE}"
echo "Output      : ${REPO_ROOT}/results/crop_validation/"
echo "Logs        : ${REPO_ROOT}/LogFiles/val_stap1_aggre_*_%A-%a.log"
echo "------------------------------------------------------------------------"

if [[ "${MODE}" == "local" ]]; then
  failures=0
  total=0

  if [[ -n "${LOCAL_ROW}" ]]; then
    rows=("${LOCAL_ROW}")
  else
    rows=($(seq 1 "${N_ROWS}"))
  fi

  for row_id in "${rows[@]}"; do
    total=$((total + 1))
    echo "[local ${row_id}/${N_ROWS}]"
    if bash "${WORKER_SH}" "${row_id}"; then
      echo "  OK"
    else
      echo "  FAILED"
      failures=$((failures + 1))
    fi
    echo "------------------------------------------------------------------------"
  done

  echo "Completed: $((total - failures))/${total} succeeded, ${failures} failed"
  [[ "${failures}" -eq 0 ]]
fi

if ! command -v sbatch >/dev/null 2>&1; then
  echo "ERROR: sbatch not found. Use --local to run without SLURM." >&2
  exit 1
fi

mkdir -p "${REPO_ROOT}/LogFiles"

job_id="$(sbatch --parsable --array="1-${N_ROWS}" "${WORKER_SH}")"
echo "Submitted SLURM array job ${job_id} with ${N_ROWS} tasks (one per CSV row)"
echo "Monitor: squeue -j ${job_id}"
echo "========================================================================"
