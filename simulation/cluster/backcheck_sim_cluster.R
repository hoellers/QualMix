# Key Information --------------------------------------------------------------
# Title: Backchecking Simulation
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script simulates 50 different sets of survey backchecks using
#          the same combination of parameters, fits a Stan model to each set, 
#          then performs preliminary analysis
# ---------------------------------------------------------------------------- #

# Setup ------------------------------------------------------------------------

# print script to identify in log
print(paste('Backcheck Monte Carlo Simulation', Sys.getenv('MCITER'),
            'Started', Sys.time()))

# clear environment
rm(list = ls())

# set seed for replication - this ensures that beta_E are same between all
# simulations
set.seed(3491)

# set working directory
wd_path <- 'set/to/working/directory/on/cluster'
setwd(wd_path)

# load required packages
library(tidyverse) # for easier manipulation of data structures
library(cmdstanr) #for fitting Stan models in R via CmdStan
library(rstan) # for working with Stan models in R
library(philentropy) # for Jensen-Shannon Divergence
library(stringdist) # for Jaro-Winkler string distance
library(gtools) # for inv.logit
library(doParallel) # parallel computing backend
registerDoParallel(as.numeric(Sys.getenv('SLURM_CPUS_PER_TASK')) %/% 4)

# source function scripts
source("convenience_functions.R") # for analysis
source("simulate_Ra_Rb.R") # for creating backchecks
source("typos.R")

# Monte Carlo Simulation Parameters --------------------------------------------

# credible interval level from environment variable
ci_level <- as.numeric(Sys.getenv('CILEVEL'))
ci_bounds <- c((1 - ci_level) / 2, 1 - (1 - ci_level) / 2)

# use iteration environment variable to get backcheck simulation parameters
params <- unlist(read.csv('mc_params.csv')[as.numeric(Sys.getenv('MCITER')), ])
print(params)

# number of iterations for sampler
it <- 1500

# variables in survey to use for matching
match_vars <- c("female", 
                "age",
                "education",
                "hh_income",
                "sell_freq",
                "stall_activity",
                "profit_lst_yr_gen",
                "fee2_always",
                "y")

# load survey used for simulating backchecks
load("backcheck_survey.RData")

# make sure variables type are correct
backcheck_survey$education <- as.ordered(backcheck_survey$education)
backcheck_survey$sell_freq <- as.ordered(backcheck_survey$sell_freq)
backcheck_survey$profit_lst_yr_gen <- as.ordered(backcheck_survey$profit_lst_yr_gen)

#turn household income into tens of thousands of kwacha
backcheck_survey$hh_income <- backcheck_survey$hh_income/10000

#determine simulation regression coefficients
options(contrasts = rep ("contr.treatment", 2)) #set contrasts to avoid poly
                                                #contrasts for ordered variables
#initialize y
backcheck_survey$y <- NA
#formula
lm_form <- y ~ fee2_always + female + age + hh_income +
  profit_lst_yr_gen
mm <- model.matrix(lm_form,
                   data = backcheck_survey)
betas_true <- c(1.45, #intercept
                -1.3, #fee2_always
                2.35, #female
                -.25, #age
                .67, #hh_income
                c(0.3, 1, 2, 3) #profit_lst_yr_gen
                ) 
names(betas_true) <- colnames(mm)

#make enum probs
enum_probs <- backcheck_survey %>% group_by(enum) %>% tally
enum_probs <- make_enum_probs(enum_ids = enum_probs$enum,
                              n = enum_probs$n,
                              smallest_group_size = 150,
                              num_enum = params["num_enum"],
                              enum_mean = logit(params["beta_0"]))

# Monte Carlo Simulation #------------------------------------------------------

#set seed that will make sure no correlation between first iterations of each
#simulation
set.seed(as.numeric(Sys.getenv('MCITER')))

results <- foreach(i = 1:50, .packages = c('rstan', 'gtools', 'stringdist',
                                           'tidyverse')) %dopar% {
  
  # set Stan options for each worker
  #rstan_options(auto_write = TRUE)
  options(mc.cores = 4)
                                              
  #simulate outcome for regression
  backcheck_survey$y <- as.vector(model.matrix(lm_form, 
                                               model.frame(~ ., backcheck_survey,
                                                          na.action=na.pass)) %*%
    betas_true +
    rnorm(nrow(backcheck_survey), 0, 5))
   
  #simulate backcheck
  bckchck <- simulate_Ra_Rb(survey = backcheck_survey, 
                            match_vars = match_vars,
                            max_var_change_por = .7,
                            var_scramble_por = .5,
                            change_prob_lb = .1,
                            backcheck = T,
                            backcheck_portion = params['backcheck_portion'],
                            enum_probs = enum_probs)
  
  end_col <- ncol(bckchck$Ra)
  agreements <- getGamma(bckchck$Ra[, 3:end_col], bckchck$Rb[, 3:end_col], 
                         varnames = match_vars,
                         stringdist.match = c(FALSE, FALSE, FALSE, FALSE, 
                                              FALSE, TRUE, FALSE, FALSE, FALSE),
                         numeric.match = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                                           TRUE, TRUE, TRUE),
                         partial.match = rep(TRUE, length(match_vars))) %>%
    to_multinomial()
  
  model_data <- list(N = nrow(agreements),
                       K = 3,
                       agreements = agreements,
                       alpha_1 = c(1, 2, 3),
                       alpha_0 = c(1, 2, 3),
                       mu_beta_0_p = logit(.5),
                       sigma_beta_0_p = .1,
                       E = length(unique(bckchck$Ra$enum)),
                       id_E = bckchck$Ra$enum)
  backcheck_model <- cmdstan_model('MM_SQ_backchecking.stan',
                                 stanc_options = list("auto-format"))
  
  backcheck_model <- backcheck_model$sample(
                        data = model_data,
                        iter_warmup = it/2,
                        iter_sampling = it/2,
                        chains = 4)
  backcheck_model <- rstan::read_stan_csv(backcheck_model$output_files())  

  #make sure original survey contains same respondents as bckchck$survey
  orig_surv <- backcheck_survey %>% filter(resp_id %in% bckchck$survey$resp_id)
  
  list(model = backcheck_model, bckchck = bckchck, 
       orig_surv = orig_surv) 
  
}

# print script to indicate completion of model fitting in log
print(paste('Backcheck Monte Carlo Simulation', Sys.getenv('MCITER'),
            'Model Fitting Completed', Sys.time()))

# Preliminary Analysis ---------------------------------------------------------

# model evaluation
mod_eval <- parallel::mclapply(results, eval_model_oos, mc.cores = 20)
mod_eval <- extract_sublists(mod_eval, c("auc_oos", "fdr_oos",
                                    "fnr_oos"))
mod_eval <- lapply(mod_eval, c)

# print script to identify progress in log
print(paste('Backcheck Monte Carlo Simulation', Sys.getenv('MCITER'),
            'Model Evaluation Completed', Sys.time()))

# quality estimation evaluation
qual_eval <- parallel::mclapply(results, eval_surv_qual, 
                                true_beta_0 = log(params["beta_0"]/(1- params["beta_0"])),
                                mc.cores = 20)
qual_eval <- extract_sublists(qual_eval, c("beta_0_bias",
                                           "surv_qual_bias_l",
                                    "surv_qual_bias_r",
                                    "beta_E_bias",
                                    "enum_qual_bias_l",
                                    "enum_qual_bias_r"),
                           simplify = c(T, T, T, F, F, F))
qual_eval1 <- lapply(qual_eval[c("beta_0_bias",
                                 "surv_qual_bias_l",
                              "surv_qual_bias_r")],
                     c)
qual_eval2 <- lapply(qual_eval[c("beta_E_bias","enum_qual_bias_l", 
                              "enum_qual_bias_r")],
                     function(x) do.call(cbind, x))
qual_eval <- c(qual_eval1, qual_eval2)

# print script to identify progress in log
print(paste('Backcheck Monte Carlo Simulation', Sys.getenv('MCITER'),
            'Quality Evaluation Completed', Sys.time()))

# post-quality estimation weighting evaluation
weight_eval <- parallel::mclapply(results, eval_weighting, 
                      mean_vars = c("age", "hh_income", "y", "fee2_always"),
                      mc.cores = 20)
weight_eval <- extract_sublists(weight_eval,
                                c("lm_uw", "lm_fe", "lm_r_bckchck",
                                  "lm_r", "lm_l", "lm_rl",
                                  "lm_drop_r", "lm_drop_l",
                                  "means_uw", "means_r_bckchck",
                                  "means_r", "means_l", "means_rl",
                                  "means_drop_r", "means_drop_l"),
                                simplify = F)
weight_eval <- lapply(weight_eval,
                      function(x) do.call(cbind, x))

# print script to identify progress in log
print(paste('Backcheck Monte Carlo Simulation', Sys.getenv('MCITER'),
            'Weighting Evaluation Completed', Sys.time()))

#calculate JSD
JSD <- parallel::mclapply(results, calc_JSD)
JSD <- list(JSD = do.call(c, JSD))

#save important objects
enum_probs <- extract_sublists(extract_sublists(results, "bckchck", 
                                                simplify = F), 
                               "enum_probs" , simplify = F)

orig_surv <- extract_sublists(results, "orig_surv" , simplify = F)

# save results of simulation to disk
save(mod_eval, qual_eval, weight_eval, JSD, enum_probs, orig_surv,
     file = paste0('MC_Output/results',
                            as.numeric(Sys.getenv('MCITER')), '.RData'))

# print script to identify in log
print(paste('Backcheck Monte Carlo Simulation', Sys.getenv('MCITER'),
            'Completed', Sys.time()))


# exit script
quit(save = 'no')
