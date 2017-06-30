#! /bin/bash
#SBATCH --time=00:20:00
#SBATCH --qos=short
#SBATCH --ntasks=1
#SBATCH --array=1-22%6
#SBATCH --output=ice_prep_%A_%a.out
#SBATCH --job-name=ice_prep
LANG=C
ulimit -s unlimited

cd /work/ollie/egowan/icesheet/icesheet/global
echo "SLURM_JOBID:         $SLURM_JOBID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
echo "SLURM_ARRAY_JOB_ID:  $SLURM_ARRAY_JOB_ID"

time_var=$(awk -v line_number=${SLURM_ARRAY_TASK_ID} '{if (NR == line_number) print $1}' times_to_calculate)

#### Suggestion for a more consistent way which should be less prone to errors. 
#mkdir run/${time_var}
#cp master_prepare_icesheet.sh run/${time_var}/
#sed 's/@time_var@/'${time_var}'/' master_run_parameters > run/${time_var}/run_parameters
#### End of suggestion

cd run/${time_var}
echo "Executing in $(pwd)"
srun prepare_icesheet.sh






