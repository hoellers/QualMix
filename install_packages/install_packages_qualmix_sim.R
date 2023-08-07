# Key Information --------------------------------------------------------------
# Title: Installation Script for Simulation Part of Project
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script installs all extra packages required to replicate the 
#          simulation portion of the paper.
# ---------------------------------------------------------------------------- #

# CRAN Packages ----------------------------------------------------------------
install.packages("doParallel") # for parallel computing (more useful for cluster
# version than personal computer version)
install.packages("RColorBrewer") # for better colors with plots and figures
install.packages("xtable") # for making LaTeX tables from R data frames

# GitHub Packages --------------------------------------------------------------