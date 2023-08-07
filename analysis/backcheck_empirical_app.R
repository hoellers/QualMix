# Key Information --------------------------------------------------------------
# Title: Backchecking Application: Real World Example
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script performs the analysis of the real world application from
#          the paper and supplementary appendix.
# ---------------------------------------------------------------------------- #

# Setup ------------------------------------------------------------------------

# Working Directory
## **Can be changed before running analysis if not using R project** 
wd_path <- here::here()
setwd(wd_path)

#for replication purposes
set.seed(1)

#packages
library(tidyverse)
library(haven)
library(stringdist)
library(philentropy)
library(rstan)
library(gtools)
library(cmdstanr)
require(Cairo)

#source files
source("scripts/convenience_functions.R")

#load data
load("data/surveys/vendor_end_long_bc_nopid.RData")
load("data/surveys/vendor_end_short_bc_nopid.RData")
load("data/surveys/vendor_end_nopid.RData")

# create directory for saving figures
dir.create("figures")

# Formatting Data --------------------------------------------------------------

#figuring out common and unique variables between short and long backchecks
common_vars <- names(vendor_end_long_bc)[names(vendor_end_long_bc) %in% 
                                           names(vendor_end_short_bc)]
short_only_vars <- names(vendor_end_short_bc)[names(vendor_end_short_bc) %nin% 
                                           names(vendor_end_long_bc)]
long_only_vars <- names(vendor_end_long_bc)[names(vendor_end_long_bc) %nin% 
                                                names(vendor_end_short_bc)]

# binding together the short and long backchecks
vendor_end_bc <- bind_rows(vendor_end_long_bc, vendor_end_short_bc)


# find out which variables are unique to backchecks
backcheck_uniq <- names(vendor_end_bc)[names(vendor_end_bc) %nin% 
                                         names(vendor_end)]

# add original enumerator information to backcheck data
enum_info <- vendor_end[, c("resp_id", "enum")]
vendor_end_bc <- inner_join(vendor_end_bc,
                            enum_info,
                            by = c("resp_id" = "resp_id"))

# finding out number of obs per enumerator
num_enum_orig <- vendor_end %>% group_by(enum) %>% 
  summarize(num_pr_enum = n())
num_enum_bc <- vendor_end_bc %>% group_by(enum) %>% 
  summarize(num_in_bckchck = n())
num_enum <- full_join(num_enum_orig, num_enum_bc,
                      by = c("enum" = "enum"))
# add numeric id vector (needed for stan model)
# arrange by number in backcheck so that it makes it easy to drop enumerators
# with few observations later
num_enum <- arrange(num_enum, desc(num_in_bckchck))
num_enum$enum_id <- 1:nrow(num_enum)

# now add enum_id to backcheck data and original data
vendor_end <- vendor_end %>% full_join(num_enum[,c("enum", "enum_id")],
                                       by = c("enum" = "enum"))
vendor_end_bc <- vendor_end_bc %>% inner_join(num_enum[,c("enum", "enum_id")],
                                       by = c("enum" = "enum"))

# cut original data to only variables selected for backchecking both long and 
# short surveys
vendor_end_orig <- vendor_end %>% 
  filter(resp_id %in% vendor_end_bc$resp_id) %>% 
  select(enum_id, market, district, any_of(common_vars))

# arrange both by resp_id
vendor_end_orig <- vendor_end_orig %>% arrange(resp_id)
vendor_end_bc <- vendor_end_bc %>% arrange(resp_id)

# seeing which actual variables are found in the backcheck
names(vendor_end_orig)
backcheck_vars <- c("d8", "d12", "e3", "e7_b", "tc2", "ms10")

# checking variable types
vendor_end_orig %>% select(any_of(backcheck_vars))
vendor_end_bc %>% select(any_of(backcheck_vars))

# making labelled variables into factor variables
vendor_end_bc <- vendor_end_bc %>% mutate(across(where(labelled::is.labelled),
                                                 labelled::to_factor))

# checking whether levels of factors are the same
mapply(function(x, y) {
         if(length(levels(x)) != length(levels(x))) return(FALSE)
           
         all(levels(x) == levels(y))
         
       },
       vendor_end_bc %>% select(any_of(backcheck_vars)),
       vendor_end_orig %>% select(any_of(backcheck_vars))
)

# fixing nonsensical age in vendor_end_bc (assuming that enumerator
# entered birthyear instead, )
vendor_end_bc$d8[vendor_end_bc$d8 == 1975] <- 2018 - 1975

# Fixing tc2 issue
# turning Don't Know or Refused to Answer into NA
vendor_end_bc$tc2[vendor_end_bc$tc2 %in% c("Don't Know", "Refused to Answer")] <-
  NA
# making No into No Receipt Available
levels(vendor_end_bc$tc2)[1:3] <- "No Receipt"
levels(vendor_end_bc$tc2)[2] <- "Receipt Available"
# fix typo in tc2 in vendor_end_orig
levels(vendor_end_orig$tc2)[1] <- "No Receipt"

# make sure variables type and orderings are correct
# d12 (education) ## --> make ordered
vendor_end_orig$d12 <- factor(vendor_end_orig$d12, 
                              levels = levels(vendor_end_orig$d12)[c(2, 20, 3:19)],
                              ordered = TRUE)
vendor_end_bc$d12 <- factor(vendor_end_bc$d12, 
                              levels = levels(vendor_end_bc$d12)[c(2, 20, 3:19)],
                            ordered = TRUE)
# note: turning Refused to Answer into NA

# e3 (sell frequency) ## --> make ordered
vendor_end_orig$e3 <- factor(vendor_end_orig$e3,
                             levels = levels(vendor_end_orig$e3)[c(9:5, 3, 4, 2)],
                             ordered = TRUE)
vendor_end_bc$e3 <- factor(vendor_end_bc$e3,
                             levels = levels(vendor_end_bc$e3)[c(9:5, 3, 4, 2)],
                           ordered = TRUE)
# note: turning Refused to Answer into NA

# e7_b stall type ## --> make character because of sheer number of categories
vendor_end_orig$e7_b <- as.character(vendor_end_orig$e7_b)
vendor_end_bc$e7_b <- as.character(vendor_end_bc$e7_b)

# ms10 (satisfaction with dev) ## --> make ordered
vendor_end_orig$ms10 <- factor(vendor_end_orig$ms10,
                             levels = levels(vendor_end_orig$ms10)[c(5:2)],
                             ordered = TRUE)
vendor_end_bc$ms10 <- factor(vendor_end_bc$ms10,
                           levels = levels(vendor_end_bc$ms10)[c(5:2)],
                           ordered = TRUE)
# note: turning Refused to Answer into NA
         
# creating agreement vectors
agreements <- getGamma(vendor_end_orig %>% select(any_of(backcheck_vars)), 
                       vendor_end_bc %>% select(any_of(backcheck_vars)), 
                       varnames = backcheck_vars,
                       stringdist.match = c(FALSE, FALSE, FALSE,  
                                            TRUE, FALSE, FALSE),
                       numeric.match = c(TRUE, TRUE, TRUE, FALSE, FALSE,
                                         TRUE),
                       partial.match = rep(TRUE, length(backcheck_vars)))

# checking number of NAs
table(apply(agreements, 1, function(x) sum(is.na(x))))


# turn all NAs into mismatches/0s
agreements[is.na(agreements)] <- 0

# forming agreement summary vectors
Nu <- agreements %>% to_multinomial()

# gather data together for Stan model fitting
model_data <- list(N = nrow(Nu),
                   K = ncol(Nu),
                   agreements = Nu,
                   alpha_1 = c(1, 2, 3),
                   alpha_0 = c(1, 2, 3),
                   mu_beta_0_p = gtools::logit(.5),
                   sigma_beta_0_p = .1,
                   E = length(unique(vendor_end_orig$enum_id)),
                   id_E = vendor_end_orig$enum_id)

# Model Fitting ----------------------------------------------------------------

# compiling stan model
bc_mod <- cmdstan_model("stan_models/MM_SQ_backchecking.stan")

# sampling
# Note: you may get warnings about pi_k_0 not being a valid simplex. These are
#       safe to ignore if they disappear after the first few iterations.
bc_fit <- bc_mod$sample(
  data = model_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4, # may have to change depending on computer core counts
  iter_warmup = 1500,
  iter_sampling = 1500,
  refresh = 500
)

# convert to rstan object (for ease of use)
bc_stanfit <- rstan::read_stan_csv(bc_fit$output_files())

# 95% credible intervals
probs <- c(0.025, 0.5, 0.975)

# Main Paper -------------------------------------------------------------------

## Section 4.2 #### 

### Descriptive Information ####

# number of respondents and number of backchecks 
nrow(vendor_end)
nrow(vendor_end_bc)
nrow(vendor_end_bc)/nrow(vendor_end)

### Figure 2 #### 

# Figure of backcheck percentages
num_enum$percent_backcheck <- num_enum$num_in_bckchck/num_enum$num_pr_enum
num_enum$percent_backcheck
mean(num_enum$percent_backcheck, na.rm = T)

file_name <- paste0("figures/", "reinterviewed_percents",".pdf") 
cairo_pdf(filename = file_name, width = 8, height = 7)
ggplot(num_enum, aes(x = percent_backcheck)) + 
  geom_histogram() + xlim(c(0, .1)) +
  labs(y = "Count", x = "Percent of Resp. Reinterviewed") +
  theme_bw()
dev.off()

## Section 4.2.1 ####

### Figure 3 ####

#assess similarity of pi_k distributions
pi_k_1 <- as.data.frame(t(apply(rstan::extract(bc_stanfit, pars = "pi_k_1")$pi_k_1,
                  2,
                  quantile, probs = probs)))
pi_k_0 <- as.data.frame(t(apply(rstan::extract(bc_stanfit, pars = "pi_k_0")$pi_k_0,
        2,
        quantile, probs = probs)))
pi_k_1$Cat <- 0:2
pi_k_0$Cat <- 0:2

#combine for plotting
pi_k <- rbind(pi_k_0, pi_k_1)
pi_k$Distribution <- c(rep("Low-Quality", 3),
                       rep("High-Quality", 3))

# Pi_K plots (Figure 3)
file_name <- paste0("figures/", "pi_ks",".pdf") 
cairo_pdf(filename = file_name, width = 8, height = 7)
ggplot(pi_k, aes(y = `50%`, x = Cat, color = Distribution)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = .25,
                position = position_dodge(0.5)) +
  labs(x = "Agreement Categories", y = "Posterior Probability") +
  theme_bw() +
  scale_x_continuous(breaks = 0:2,
                     labels = c("Complete Disagreement", 
                                "Similar",
                                "Complete Agreement")) + 
  scale_color_grey()
dev.off()

### Jensen Shannon Distance ####

# Jensen-Shannon Distance
c(quantile(calc_JSD(list(model = bc_stanfit)),
           probs = c(0.025, 0.5, 0.975)), 
  mean = mean(calc_JSD(list(model = bc_stanfit))))

### Table 4 ####

#expected value (for Table 4)
pi_k[1:3]*6

### Calculating Enumerator Data Quality Estimates #####
post_match_prob <- t(rstan::extract(bc_stanfit, pars = "responsibilities")$responsibilities) %>% 
  as.data.frame()

# looking at posterior probability of a match
post_match_prob_summary <- as.data.frame(t(apply(post_match_prob,
                                                 1, quantile, probs = c(0.025, .5, .975))))
post_match_prob_summary <- cbind(post_match_prob_summary, Nu)

post_match_prob$enum_id <- vendor_end_orig$enum_id
post_match_prob_summary$enum_id <- vendor_end_orig$enum_id

post_match_prob_summary$match <- as.numeric(post_match_prob_summary$`50%` > .5)

# enumerator quality estimates
avg_post_match_prob_pr_enum_full <- post_match_prob %>%
  group_by(enum_id) %>%
  summarise(across(everything(), mean))
avg_post_match_prob_pr_enum <- avg_post_match_prob_pr_enum_full
avg_post_match_prob_pr_enum <- data.frame(enum_id = avg_post_match_prob_pr_enum$enum_id,
                                          t(apply(avg_post_match_prob_pr_enum[2:ncol(avg_post_match_prob_pr_enum)],
                                                  1,
                                                  quantile,
                                                  probs = probs)),
                                          sd = (apply(avg_post_match_prob_pr_enum[2:ncol(avg_post_match_prob_pr_enum)],
                                                      1,
                                                      sd
                                          )),
                                          mean = (apply(avg_post_match_prob_pr_enum[2:ncol(avg_post_match_prob_pr_enum)],
                                                        1,
                                                        mean
                                          )))
names(avg_post_match_prob_pr_enum)[2:4] <- c("Low", "Median", "High")
avg_post_match_prob_pr_enum$num_bckchck <-
  num_enum$num_in_bckchck[1:nrow(avg_post_match_prob_pr_enum)]


### Descriptive Information ####

#number of obs per each category
table(Nu[,1]) #complete disagreement
table(Nu[,2]) #similar
table(Nu[,3]) #complete agreement

#percent of backchecks all wrong
table(Nu[,1])[7]/nrow(vendor_end_bc)
for(i in 1:6){
  print(table(agreements[,i]))
}

# seeing how people in backcheck and original survey responded to tc2
table(vendor_end_orig$tc2)
table(vendor_end_bc$tc2)

# average of each category's count for each distribution
apply(Nu[post_match_prob_summary$match == 1,], 2, mean)
apply(Nu[post_match_prob_summary$match == 0,], 2, mean)

# percent of matches
mean(post_match_prob_summary$match)

### Figure 4 - Enumerator Data Quality ####
file_name <- paste0("figures/", "enum_qual_endline",".pdf") 
cairo_pdf(filename = file_name, width = 8, height = 7)
ggplot(avg_post_match_prob_pr_enum, 
       aes(y = Median, x = enum_id)) +
  geom_point() +
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  labs(x = "Enumerator", y = "Average Posterior Probability\nof Belonging to High-Quality Distribution") +
  theme_bw() +
  ylim(c(0,1))
dev.off()

### Survey Data Quality Estimate ####
surv_qual <- quantile(apply(post_match_prob, 2, mean), 
                      probs = c(0.025, 0.5, 0.975))

# Appendix Materials -----------------------------------------------------------

## Appendix A ####

### How Table 3 Was Filled: ####

# Last Name
## Melzer - Beier
1 - stringdist::stringdist("Beier", "Melzer", "jw", p = 0.1)
## Karlsen - Karls
1 - stringdist::stringdist("Karlsen", "Karls", "jw", p = 0.1)

# Monthly Income  (assumed 6 category ordered variable)
## [$250, $500) (2) - <$250 (1)
getPerMaxRange(1, 2, 6 - 1)
## <$250 (1) (1) - <$250 (1)
getPerMaxRange(1, 1, 6 - 1)

# Occupation
## Market Vendor vs Business Owner
### Considered "Similar" because of hypothetical substantive knowledge
## Market Vendor vs Tax Collector
### Considered "Complete Disagreement" because very different occupations

# Age
## 65 - 57
getPerMaxRange(65, 57, 86 - 18)

## 21 - 31
getPerMaxRange(21, 31, 86 - 18)

### Appendix A.1 Examples ####
# In this sections I pull out the information used to make the tables in
# Appendix A.1, which help clarify how agreement summary vectors were created
# for the real world application (Section 4.2)
agreements[c(18, 24, 44), ]
# observations 14, 18, 24 chosen because they showcase an observations with
# mostly matches (14), all NAs (aka backcheck failure; 18), and a mix of 
# disagreements, similars, and agreements (24)

# for table A1
# original survey responses
og_ex <- vendor_end_orig %>% 
  select(any_of(backcheck_vars)) %>% 
  slice(c(18, 24, 44)) %>% 
  as.data.frame()

# backcheck survey responses
bc_ex <- vendor_end_bc %>% 
  select(any_of(backcheck_vars)) %>% 
  slice(c(18, 24, 44)) %>% 
  as.data.frame()

# for table A2
getComparisons(vendor_end_orig %>% select(any_of(backcheck_vars)), 
               vendor_end_bc %>% select(any_of(backcheck_vars)), 
               varnames = backcheck_vars,
               stringdist.match = c(FALSE, FALSE, FALSE,  
                                    TRUE, FALSE, FALSE),
               numeric.match = c(TRUE, TRUE, TRUE, FALSE, FALSE,
                                 TRUE),
               partial.match = rep(TRUE, length(backcheck_vars)))[c(18, 24, 44),]

# Gammas, for table A3
agreements[c(18, 24, 44),]

# Nus for examples, for table A4
print(to_multinomial(agreements[c(18, 24, 44), ]))

## Appendix I - Results Dropping Failed Backchecks ####

# Re-Analysis Dropping All 0s

#figuring out which were all 6
vendor_end_bc$not_all_0 <- Nu[,1] != 6

Nu2 <- Nu[vendor_end_bc$not_all_0,]

num_enum <- full_join(num_enum,
                      vendor_end_bc %>% 
                        filter(not_all_0) %>% 
                        group_by(enum_id) %>% 
                        summarise(num_in_bckchck2 = n()))

num_enum$percent_not_all_0 <- num_enum$num_in_bckchck2/num_enum$num_in_bckchck

model_data2 <- list(N = nrow(Nu2),
                    K = ncol(Nu2),
                    agreements = Nu2,
                    alpha_1 = c(1, 2, 3),
                    alpha_0 = c(1, 2, 3),
                    mu_beta_0_p = gtools::logit(.5),
                    sigma_beta_0_p = .1,
                    E = length(unique(vendor_end_orig$enum_id)),
                    id_E = vendor_end_orig$enum_id[vendor_end_bc$not_all_0])

### Fitting Model ####

bc_fit2 <- bc_mod$sample(
  data = model_data2,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1500,
  iter_sampling = 1500,
  refresh = 500
)

bc_stanfit2 <- rstan::read_stan_csv(bc_fit2$output_files())

#assess similarity of pi_k distributions
pi_k_1_2 <- as.data.frame(t(apply(rstan::extract(bc_stanfit2, pars = "pi_k_1")$pi_k_1,
                                  2,
                                  quantile, probs = probs)))
pi_k_0_2 <- as.data.frame(t(apply(rstan::extract(bc_stanfit2, pars = "pi_k_0")$pi_k_0,
                                  2,
                                  quantile, probs = probs)))
pi_k_1_2$Cat <- 0:2
pi_k_0_2$Cat <- 0:2

#combine for plotting
pi_k2 <- rbind(pi_k_0_2, pi_k_1_2)
pi_k2$Distribution <- c(rep("Low-Quality", 3),
                        rep("High-Quality", 3))




#expected value
pi_k2[1:3]*6

#number of obs per each category
table(Nu2[,1]) #complete disagreement
table(Nu2[,2]) #similar
table(Nu2[,3]) #complete agreement

for(i in 1:6){
  print(table(agreements[,i]))
}

# Assessing Enumerator Quality ##
post_match_prob2 <- t(rstan::extract(bc_stanfit2, pars = "responsibilities")$responsibilities) %>% 
  as.data.frame()

# looking at postior probability of a match
post_match_prob_summary2 <- as.data.frame(t(apply(post_match_prob2,
                                                  1, quantile, probs = c(0.025, .5, .975))))
post_match_prob_summary2 <- cbind(post_match_prob_summary2, Nu2)

post_match_prob2$enum_id <- vendor_end_orig$enum_id[vendor_end_bc$not_all_0]
post_match_prob_summary2$enum_id <- vendor_end_orig$enum_id[vendor_end_bc$not_all_0]

post_match_prob_summary2$match <- as.numeric(post_match_prob_summary2$`50%` > .5)

# average of each category's count for each distribution
apply(Nu2[post_match_prob_summary2$match == 1,], 2, mean)
apply(Nu2[post_match_prob_summary2$match == 0,], 2, mean)


# percent of matches
mean(post_match_prob_summary2$match)

# enumerator quality estimates
avg_post_match_prob_pr_enum2 <- post_match_prob2 %>%
  group_by(enum_id) %>%
  summarise(across(everything(), mean))
avg_post_match_prob_pr_enum2 <- data.frame(enum_id = avg_post_match_prob_pr_enum2$enum_id,
                                           t(apply(avg_post_match_prob_pr_enum2[2:ncol(avg_post_match_prob_pr_enum2)],
                                                   1,
                                                   quantile,
                                                   probs = probs)),
                                           sd = (apply(avg_post_match_prob_pr_enum2[2:ncol(avg_post_match_prob_pr_enum2)],
                                                       1,
                                                       sd
                                           )))
names(avg_post_match_prob_pr_enum2)[2:4] <- c("Low", "Median", "High")

### Jensen-Shannon Distance ####
c(quantile(calc_JSD(list(model = bc_stanfit2)),
           probs = c(0.025, 0.5, 0.975)), 
  mean = mean(calc_JSD(list(model = bc_stanfit2))))

#### Survey Quality Estimate ####
surv_qual2 <- quantile(apply(post_match_prob2, 2, mean), 
                       probs = c(0.025, 0.5, 0.975))

### Figure H1 - Pi_K plots ####
file_name <- paste0("figures/", "pi_ks2",".pdf") 
cairo_pdf(filename = file_name, width = 8, height = 7)
ggplot(pi_k2, aes(y = `50%`, x = Cat, color = Distribution)) +
  geom_point(position = position_dodge(width = .5)) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = .25,
                position = position_dodge(0.5)) +
  labs(x = "Agreement Categories", y = "Posterior Probability") +
  theme_bw() +
  scale_x_continuous(breaks = 0:2,
                     labels = c("Complete Disagreement", 
                                "Similar",
                                "Complete Agreement")) + 
  scale_color_grey()
dev.off()

### Figure H2 - Histogram of Posterior Probability of High Quality ####
file_name <- paste0("figures/", "poster_match_prob2",".pdf") 
cairo_pdf(filename = file_name, width = 8, height = 7)
ggplot(post_match_prob_summary2, aes(x = `50%`)) + 
  geom_histogram() +
  labs(y = "Count", x = "Median of Posterior Probability of High Quality") +
  theme_bw()
dev.off()

### Figure H3 - Enumerator Data Quality  ####
file_name <- paste0("figures/", "enum_qual_endline2",".pdf") 
cairo_pdf(filename = file_name, width = 8, height = 7)
ggplot(avg_post_match_prob_pr_enum2, 
       aes(y = Median, x = enum_id)) +
  geom_point() +
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  labs(x = "Enumerator", y = "Average Posterior Probability\nof Belonging to High-Quality Distribution") +
  theme_bw() +
  ylim(c(0,1))
dev.off()

## Appendix J - Validation Exercise - Receipts And Enumerator Quality ####

# Results from this appendix are also mentioned in Section 4.2.1

#calculate percent receipt shown by enumerator
r7_enum <- vendor_end %>% group_by(enum, enum_id) %>% 
  summarise(overall_mean = mean(recent_receipt_7, na.rm = T),
            sum_r7 = sum(recent_receipt_7, na.rm = T),
            sum_receipt = sum(receipt_shown, na.rm = T),
            n = n())

#merge enum_ids with r7
r7_enum <- left_join(r7_enum, avg_post_match_prob_pr_enum, 
                     by = c("enum_id" = "enum_id"))

# arrange by enum_id to make later stan models easier to fit
r7_enum <- arrange(r7_enum, enum_id)

### Appendix J.1 Initial Modeling Attempt ####

# function to calculate alpha and beta for Beta distribution using mu and 
# variance of Beta
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

#calculating alpha and beta for Beta distributed latent X (we estimate the 
# model as if quality were estimated with error, to incorporate uncertainty about
# it into model)
r7_enum$est_alpha <- estBetaParams(r7_enum$mean,
                                   r7_enum$sd^2)$alpha
r7_enum$est_beta <- estBetaParams(r7_enum$mean,
                                   r7_enum$sd^2)$beta

# collect data for Stan model
r7_qual_data <- list(N = nrow(r7_enum[!is.na(r7_enum$Median),]),
                     success = r7_enum$sum_receipt[!is.na(r7_enum$Median)],
                     total = r7_enum$n[!is.na(r7_enum$Median)],
                     alpha = r7_enum$est_alpha[!is.na(r7_enum$Median)],
                     beta = r7_enum$est_beta[!is.na(r7_enum$Median)])

# compile stan model
r7_qual_mod <- cmdstan_model("stan_models/receipts_qual_model.stan")

# fit stan model
r7_qual_fit <- r7_qual_mod$sample(
  data = r7_qual_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1500,
  iter_sampling = 1500,
  refresh = 500,
  adapt_delta = .99999,
  max_treedepth = 20
)

r7_qual_fit$summary()
r7_qual_fit$cmdstan_diagnose()
# r7_qual_fit_stanfit <- rstan::read_stan_csv(r7_qual_fit$output_files())

# this model has too many divergent transitions; the issue is that it treats
# the predictor as a latent variable, with enumerator quality distributions
# acting as prior. Because quality is [0,1], I use a beta distribution. But 
# some of the quality estimates are 1, which makes the log-likelihood equal -Inf
# This causes divergent transitions

# Solution: fit many models with samples from posterior of enumerator quality


### Appendix J.2 Model Fitting - Second Attempt ####
set.seed(1)
# take 1000 random draws from posterior of post match probability per enumerator
samps <- sample(2:ncol(avg_post_match_prob_pr_enum_full), 1000)

# containers for draws
beta1_samples <- matrix(NA, nrow = 1000, ncol = 2000)
beta0_samples <- matrix(NA, nrow = 1000, ncol = 2000)

# compiling stan model
r7_qual_mod2 <- cmdstan_model("stan_models/receipts_qual_model_simple.stan")

for(i in 1:1000){
  it <- samps[i]
  
  # collect data for new Stan model
  r7_qual_data2 <- list(N = nrow(r7_enum[!is.na(r7_enum$Median),]),
                        x = avg_post_match_prob_pr_enum_full[[it]],
                        success = r7_enum$sum_receipt[!is.na(r7_enum$Median)],
                        total = r7_enum$n[!is.na(r7_enum$Median)])
  
  r7_qual_fit2 <- r7_qual_mod2$sample(
    data = r7_qual_data2,
    seed = 123,
    chains = 2,
    parallel_chains = 2,
    iter_warmup = 1000,
    iter_sampling = 1000,
    refresh = 500
  )
  
  #turn into rstan object to make it easier to handle
  r7_qual_fit_stanfit2 <- rstan::read_stan_csv(r7_qual_fit2$output_files())
  
  #extract parameters 
  pars <- rstan::extract(r7_qual_fit_stanfit2,
          pars = c("beta_0", "beta_1"))
  #put parameters into their containers
  beta0_samples[i,] <- pars$beta_0
  beta1_samples[i,] <- pars$beta_1
}

### Appendix J.3 - Results ####

# beta_0 and beta_1 summaries
beta0_summary <- c(mean = mean(c(beta0_samples)),
                   quantile(c(beta0_samples), probs = probs))
beta1_summary <- c(mean = mean(c(beta1_samples)),
                   quantile(c(beta1_samples), probs = probs))

#### Values Referenced in Section 4.2.1 ####

#posterior predicted probability of going from .5 to .75 and .75 to 1 in quality
ppp_.5 <- gtools::inv.logit(c(beta0_samples) + c(beta1_samples) * .5)
ppp_.75 <- gtools::inv.logit(c(beta0_samples) + c(beta1_samples) * .75)
ppp_1 <- gtools::inv.logit(c(beta0_samples) + c(beta1_samples) * 1)

diff_.75_.5_summary <- quantile(ppp_.75 - ppp_.5, probs = probs)
diff_1_.75_summary <- quantile(ppp_1 - ppp_.75, probs = probs)


#### Figure I1 #####

r7_qual_plot_data <- as.data.frame(rbind(diff_.75_.5_summary, diff_1_.75_summary))
r7_qual_plot_data$diff <- c("Quality Change: .5 -> .75",
                            "Quality Change: .75 -> 1")
names(r7_qual_plot_data)[1:3] <- c("Low", "Median", "High") 

r7_qual_plot <- ggplot(data = r7_qual_plot_data) +
  geom_point(aes(y = diff,
                 x = Median)) +
  geom_errorbarh(aes(xmax = High, xmin = Low, y = diff), height = .25) +
  theme_bw() + labs(x = "Change in Posterior Predicted Probability",
                    y = "")
ggsave("figures/r7_qual.pdf", r7_qual_plot, width = 8, height = 7,
       units = "in")

#### Figure I2 - Probability Curve ####

### quality range
qual_range <- seq(0.44, 1, by = .005)
### making container for posterior predicted probabilities
ppp <- matrix(ncol = length(beta0_samples),
              nrow = length(qual_range))
### calculating the posterior predicted probabilities
for(i in 1:nrow(ppp)){
  
  ppp[i, ] <- gtools::inv.logit(c(beta0_samples) + c(beta1_samples) * qual_range[i])
  
}
### summarizing for plotting
ppp_summary <- t(apply(ppp, 1, quantile, probs = probs))
### making dataframe
r7_qual_curve_data <- data.frame(ppp_summary, qual_range) 
names(r7_qual_curve_data)[1:3] <- c("Low", "Median", "High")

# Figure I2
r7_qual_curve <- ggplot(data = r7_qual_curve_data) +
  geom_point(aes(x = qual_range,
                 y = Median), size = .2) +
  geom_errorbar(aes(ymax = High, ymin = Low, x = qual_range)) +
  theme_bw() + labs(x = "Enumerator Data Quality",
                    y = "Posterior Predicted Probability")  +
  ylim(c(0, 0.5)) +
  scale_x_continuous(breaks = seq(0.5, 1, by = .1), limits =  c(0.44, 1.01)) +
  geom_rug(aes(x = Median), data = avg_post_match_prob_pr_enum, color = "#D55E00")
ggsave("figures/r7_qual_curve.pdf",
       r7_qual_curve, width = 8, height = 7,
       units = "in")


