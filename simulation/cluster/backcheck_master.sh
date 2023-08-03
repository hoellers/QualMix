#! /bin/bash

# load the appropriate R module
module load r/4.1.3

# Load the appropriate g++ module for stan
module load gcc/11.2.0

# create directory for output
mkdir -p MC_Output

# create .csv file of covariance function parameters for simulations, capturing job ID for dependency
SETUPJOB=$(sbatch --job-name=bc_setup --ntasks=1 --time=1:00 --mem=100 --output=/dev/null --error=/dev/null --wrap="Rscript --vanilla  backcheck_setup.R")

# assign setup job ID to environment variable
SETUPID=`echo $SETUPJOB | cut -d " " -f 4`

# set Monte Carlo simulation parameters as environment variables
# export MCSIMS="100"
export CILEVEL=".95"

# run simulation for each set of Monte Carlo simulation parameters
for iter in {1..12}; do
    # print iteration message
    echo starting simulation $iter

    # export iteration number as envrionment variable for simulation
    export MCITER=$iter

    # start simulation after setup script has finished creating .csv
    sbatch --dependency=afterok:$SETUPID backcheck_subprocess.sh  
done

# quit script
exit
