#! /bin/bash

#SBATCH --job-name=bc_mcsim

# one task for threaded parallelization
#SBATCH --ntasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=20

# memory request
#SBATCH --mem=160gb

# two day time limit
#SBATCH --time=2-0

# load the appropriate R module
module load r/4.1.3

# Load the appropriate g++ module for stan
module load gcc/11.2.0

## run script
Rscript --vanilla backcheck_sim_cluster.R
