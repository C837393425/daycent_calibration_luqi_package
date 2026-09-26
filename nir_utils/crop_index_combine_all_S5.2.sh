#!/bin/bash

####========================================================
#### Crop Index Combine All Step 5.2 
####========================================================

#SBATCH -J crop_S5.2
#SBATCH -p hipri
#SBATCH --mem-per-cpu=10G
#SBATCH -n 1
#SBATCH -N 1
#SBATCH --array=1-2
##SBATCH --output=/dev/null
##SBATCH --error=/dev/null
#SBATCH --output=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/crop_index_S5.2_stdout_%A-%a.log
#SBATCH --error=/data/rubelscratch/rubelogle/daycent_calibration/LogFiles/crop_index_S5.2_stderr_%A-%a.log
##SBATCH --mail-user=luqi.jiaoemanuele@colostate.edu
##SBATCH --mail-type=ALL
#SBATCH --exclude=rubel-030

# =========================================================
# Paths
# =========================================================

DIR_PROJECT="/data/rubelscratch/rubelogle/daycent_calibration/nir_utils"
R_FULL_PATH_RUN="$DIR_PROJECT/crop_file_index_combine_all_S5.2.R"

# =========================================================
# Environment
# =========================================================
if command -v module >/dev/null 2>&1; then
  module load R >/dev/null 2>&1 || true
fi

if ! command -v Rscript >/dev/null 2>&1; then
  echo "ERROR: Rscript not found in PATH." >&2
  exit 1
fi

# =========================================================
# Run
# =========================================================
Rscript "$R_FULL_PATH_RUN" "$SLURM_ARRAY_TASK_ID"

exit
