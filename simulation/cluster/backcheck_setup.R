# Key Information --------------------------------------------------------------
# Title: Set Up Backchecking Simulation Parameters
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script generates a matrix of different backcheck simulation 
#          parameters for the Monte Carlo simulation and saves it to a csv for
#          access by each job spawned by the master job submission script
# ---------------------------------------------------------------------------- #

# Code -------------------------------------------------------------------------

## clear environment
rm(list = ls())

# create parameter grid
mc_params <- expand.grid(num_enum = c(35), 
                         backcheck_portion = c(0.05, 0.10, .15, .2),
                         beta_0 = c(.8, .9, .95))

## save to directory for access by jobs
write.csv(mc_params, 'mc_params.csv', row.names = F)

## exit script
quit(save = 'no')