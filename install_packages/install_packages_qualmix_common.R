# Key Information --------------------------------------------------------------
# Title: Installation Script for Common Packages 
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script installs all packages required for both the simulations
#          and the empirical application
# ---------------------------------------------------------------------------- #

# CRAN Packages ----------------------------------------------------------------
install.packages("tidyverse") # a set of data importing, wrangling, and 
# visualization packages
# tidyverse packages used: dplyr, ggplot2, purrr, haven, tidyr
install.packages("here") # used to standardize file paths call, in conjunction
# with R project file
install.packages("pROC") # for model evaluation
install.packages("philentropy") # for Jensen-Shannon Divergence
install.packages("gtools") # for inv.logit() function
install.packages("stringdist") # to calculate string distance metrics
install.packages("rstan") # to manipulate rstan objects
install.packages("Cairo") # for saving images

# Non-CRAN Packages ------------------------------------------------------------
install.packages("cmdstanr", 
                 repos = c("https://mc-stan.org/r-packages/",
                           getOption("repos")))
# will need to install a C++ toolchain and then cmdstan itself;
# see directions here: https://mc-stan.org/cmdstanr/articles/cmdstanr.html