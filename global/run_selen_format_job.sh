#! /bin/bash
#SBATCH --time=00:20:00
#SBATCH --qos=short
#SBATCH --ntasks=1
#SBATCH --output=ice_combine.out
#SBATCH --job-name=ice_combine
LANG=C
ulimit -s unlimited

cd /work/ollie/egowan/icesheet/icesheet/global
echo "SLURM_JOBID:         $SLURM_JOBID"
echo "SLURM_ARRAY_TASK_ID: $SLURM_ARRAY_TASK_ID"
echo "SLURM_ARRAY_JOB_ID:  $SLURM_ARRAY_JOB_ID"


srun selen_format.sh


