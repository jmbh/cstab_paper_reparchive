#!/bin/bash
#SBATCH -N 1
#SBATCH -t 02:30:00

module load openmpi/gnu
module load R/3.3.1
module load eb
module load intel/2016b
module load fortran/intel
module load mkl

#export R_LIBS=$HOME/rpackages:$R_LIBS

cp -r "$HOME"/clusterSim "$TMPDIR"
cd "$TMPDIR"/clusterSim

echo $SLURM_ARRAY_TASK_ID

Rscript --vanilla simulation.R $SLURM_ARRAY_TASK_ID

cp -r ./*.RDS "$HOME"/clusterSim/output