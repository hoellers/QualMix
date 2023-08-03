# Key Information --------------------------------------------------------------
# Title: Removing Personal Identifiers from Data
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script cuts down the data objects to include only the
#          necessary variables to replicate the paper, as data is not fully
#          publicly available yet.
#          It also removes pids from the data files to maintain data 
#          confidentiality. 
# Note: Not runnable with data included in replication archive but included for
#       transparency reasons 
# ---------------------------------------------------------------------------- #

# Set Up -----------------------------------------------------------------------

# functions

`%nin%` <- Negate('%in%')

# packages
library(tidyverse)
library(haven)
library(here)

# set working directory
wd_path <- here::here() # assumes project is open
setwd(wd_path)

# Backcheck data ---------------------------------------------------------------

# read in data
vendor_end_long_bc <- read_dta("data/surveys/tad_endline_market_long_bc_clean.dta")
vendor_end_short_bc <- read_dta("data/surveys/tad_endline_market_short_bc_clean.dta")

## Remove potentially identifying information ####

# anonymize backcheckers
vendor_end_long_bc <- vendor_end_long_bc %>% 
  mutate(bcer = as.numeric(as.factor(bcer)))
vendor_end_short_bc <- vendor_end_short_bc %>% 
  mutate(bcer = as.numeric(as.factor(bcer)))

# remove variables with PID information for respondents and backcheckers
vendor_end_long_bc <- vendor_end_long_bc %>% 
  select(-c(deviceid:username), -bcer_txt, -c(pre_name:pre_phone), -key,
         -obs, #useful but unfortunately contains names
         -no_consent_why, #similar
         -incomplete_explain, #no values
         -formdef_version, #no variation
         -cases_users, #no variation
         -cases_id #duplicated column
  )
vendor_end_short_bc <- vendor_end_short_bc %>% 
  select(-c(deviceid:username), -bcer_txt, -c(pre_name:pre_phone), -key,
         -obs, #useful but unfortunately contains names
         -no_consent_why, #similar
         -incomplete_explain, #no values
         -formdef_version, #no variation
         -cases_users, #no variation
         -cases_id #duplicated column
)

# Original Data ----------------------------------------------------------------

# load data
load("data/surveys/vendor_end.RData")

# anonymize enumerators
vendor_end <- vendor_end %>% 
  mutate(enum = as.numeric(as.factor(enum)))

# find variables for backcheck
common_vars <- names(vendor_end_long_bc)[names(vendor_end_long_bc) %in% 
                                           names(vendor_end_short_bc)]
# retain only backcheck variables and other key variables for sim and analysis
vendor_end <- vendor_end %>% 
  select(resp_id, enum, market, district, any_of(common_vars),
         recent_receipt_7, receipt_shown)

# Save -------------------------------------------------------------------------
save(vendor_end, file = "data/surveys/vendor_end_nopid.RData")
save(vendor_end_long_bc, file =  "data/surveys/vendor_end_long_bc_nopid.RData")
save(vendor_end_short_bc, file =  "data/surveys/vendor_end_short_bc_nopid.RData")
