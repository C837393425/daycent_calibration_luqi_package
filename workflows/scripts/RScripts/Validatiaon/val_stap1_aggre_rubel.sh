#!/usr/bin/env bash

####========================================================
#### Crop validation Step 1 aggregation (one CSV row per array task)
####========================================================

#SBATCH -J val_stap1_aggre
#SBATCH -p hipri
#SBATCH --mem-per-cpu=10G
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --array=1-21
#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/val_stap1_aggre_stdout_%A-%a.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/val_stap1_aggre_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=ALL
#SBATCH --exclude=rubel-030

set -euo pipefail

# Absolute paths required: SLURM runs a copy of this script from /var/spool/slurm/...
PROJECT_ROOT="${PROJECT_ROOT:-/data/rubelscratch/rubelogle/daycent_calibration}"
SCRIPT_DIR="${PROJECT_ROOT}/workflows/scripts/RScripts/Validatiaon"
MAPPING_CSV="${SCRIPT_DIR}/crop_id_chist_mapping.csv"
AGGRE_R="${SCRIPT_DIR}/val_stap1_aggre.R"

ROW_ID="${SLURM_ARRAY_TASK_ID:-${1:-}}"
if [[ -z "${ROW_ID}" ]]; then
  echo "ERROR: SLURM_ARRAY_TASK_ID or row index argument is required." >&2
  exit 1
fi

if [[ ! -f "${MAPPING_CSV}" ]]; then
  echo "ERROR: Mapping file not found: ${MAPPING_CSV}" >&2
  exit 1
fi

if [[ ! -f "${AGGRE_R}" ]]; then
  echo "ERROR: R script not found: ${AGGRE_R}" >&2
  exit 1
fi

cd "${PROJECT_ROOT}"

if command -v module >/dev/null 2>&1; then
  module load R >/dev/null 2>&1 || true
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found in PATH." >&2
  exit 1
fi

read -r id_chist crop crop_nass crop_mg state_level nass_level < <(Rscript --vanilla -e "
  suppressPackageStartupMessages(library(readr))
  mapping <- read_csv('${MAPPING_CSV}', show_col_types = FALSE)
  row_id <- as.integer('${ROW_ID}')
  if (is.na(row_id) || row_id < 1L || row_id > nrow(mapping)) {
    stop('Invalid row index ', row_id, ' for ', nrow(mapping), ' mapping rows')
  }
  row <- mapping[row_id, ]
  mg <- row\$crop_mg[[1]]
  if (is.na(mg)) {
    mg <- ''
  }
  cat(
    row\$id_chist[[1]], row\$crop[[1]], row\$crop_nass[[1]],
    mg, row\$state_level[[1]], row\$nass_level[[1]],
    sep = '\t'
  )
  cat('\n')
")

echo "Row ${ROW_ID}: crop=${crop} id_chist=${id_chist} crop_nass=${crop_nass} state_level=${state_level} crop_mg=${crop_mg:-<none>} nass_level=${nass_level}"

cmd=(Rscript --vanilla "${AGGRE_R}" "${id_chist}" "${crop}" "${crop_nass}" "${state_level}")
if [[ -n "${crop_mg}" ]]; then
  cmd+=("${crop_mg}")
fi

"${cmd[@]}"
