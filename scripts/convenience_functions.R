# Key Information --------------------------------------------------------------
# Title: Convenience Functions
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script contains a series of functions that facilitate the 
#           use of the QualMix model and analysis of model results. It is 
#           sourced in all analysis scripts.
# Attribution: getGamma() and getComparisons are adapted from the getPatterns()
#              functions from the fastLink package (https://cran.r-project.org/package=fastLink).
# ---------------------------------------------------------------------------- #

# necessary packages for these functions:
#library(tidyverse)
#library(pROC)
#library(philentropy)
#library(gtools)
#library(stringdist)
# the functions below usually try to load these packages and will return an error 
# if they are not found

# function that negates %in% for convenience
'%nin%' <- Negate('%in%')

# function that transforms a series of identically distributed categorical
# variables into multinomial format
to_multinomial <- function(data){
  
  counts <- do.call("bind_rows", apply(data, 1, table))
  
  counts <- apply(counts, 2, as.vector)
  
  counts[is.na(counts)] <- 0
  
  counts <- counts[, order(colnames(counts))]
  
}


# function that takes a simulated data set and returns the proportion of matches
# per enumerator, along with the beta_E
get_per_matches <- function(data){
  
  output <- data %>% group_by(id_E) %>% summarise(per_match = mean(match),
                                                  probs = unique(probs),
                                                  betas_E = unique(alpha_E),
                                                  n = n())
  
  return(output)
  
}


# function that compares model results for beta_Es with true values and with 
# percent matches in actual data. Returns a named correlation vector, 3 plots,
# and a data frame
compare_betas <- function(data, stan_model){
  
  data <- get_per_matches(data)
  
  temp <-  rstan::extract(stan_model, pars = "beta_E")$beta_E %>% apply(2, mean)
  
  compare <- data.frame(data, model_mean = temp)
  
  plot_true_model <- ggplot(data = compare) + 
    geom_point(aes(x = betas_E, y = model_mean)) +
    labs(x = "Random Intercepts by Enumerator, from Simulation", 
         y = "Mean of Distribution of Random Intercepts by Enumerator from Model")
  
  plot_true_observed <- ggplot(data = compare) + 
    geom_point(aes(x = probs, y = per_match)) +
    labs(x = "True Probabiltiy of Match, per Enum, from Simulation", 
         y = "Percent of Matches Observed in Data")
  
  plot_observed_model <- ggplot(data = compare) + 
    geom_point(aes(x = per_match, y = model_mean)) +
    labs(x = "Percent of Matches Observed in Data", 
         y = "Mean of Distribution of Random Intercepts by Enumerator from Model")
  
  cor_vector <- c(true_model = cor(compare$betas_E, compare$model_mean),
                  true_observed = cor(compare$probs, compare$per_match),
                  observed_model = cor(compare$per_match, compare$model_mean))
  
  return(list(compared = compare,
         plots = list(plot_true_model = plot_true_model, 
                      plot_true_observed = plot_true_observed,
                      plot_observed_model = plot_observed_model),
         cor_vector = cor_vector))

}

# this function checks for correct matches/non-matches under different s values
check_matches <- function(matches, resp, start = .01, end = .99, detail = 100){
  correct_matches <- rep(NA, times = detail)
  S <- seq(from = start, to = end, length.out = detail)
  
  a <- 1
  for (s in S){
    correct_matches[a] <- mean(matches == as.integer(resp > s))                
    a <- a + 1
  }
  
  return(data.frame(s = S, correct_matches = correct_matches))
  
}

# function to calculate mean absolute error for random intercepts by enumerator
mean_abs_err <- function(estimates, true_vals, sigma){
  
  apply(estimates, 1, function(x){
    mean(abs(x - true_vals))
  })/sigma 
}

# function to calculate False Discovery Rate
FDR <- function(pred_prob, true_pos, s){
  
  pred_pos <- apply(pred_prob, 1, function(x) as.numeric(x > s))
  
  false_pos <- apply(pred_pos, 2, function(x) sum(x == 1 & true_pos == 0))
  
  false_pos/apply(pred_pos, 2, sum)
  
}

# function to calculate False Negative Rate
FNR <- function(pred_prob, true_pos, s){
  
  pred_pos <- apply(pred_prob, 1, function(x) as.numeric(x > s))
  
  false_neg <- apply(pred_pos, 2, function(x) sum(x == 0 & true_pos == 1))
  
  false_neg/sum(true_pos)
}

# function to calculate positive predictive value
PPV <- function(pred_prob, true_pos, s){
  
  pred_pos <- apply(pred_prob, 1, function(x) as.numeric(x > s))
  
  true_pred_pos <- apply(pred_pos, 2, function(x) sum(x == 1 & true_pos == 1))
  
  pred_pos_total <- apply(pred_pos, 2, function(x) sum(x == 1))
  
  true_pred_pos/pred_pos_total
}

#function to calculate negative predictive value
NPV <- function(pred_prob, true_pos, s){
  
  pred_pos <- apply(pred_prob, 1, function(x) as.numeric(x > s))
  
  true_pred_neg <- apply(pred_pos, 2, function(x) sum(x == 0 & true_pos == 0))
  
  pred_neg_total <- apply(pred_pos, 2, function(x) sum(x == 0))
  
  true_pred_neg/pred_neg_total
  
}

# function to calculate average responsibility per enumerator
resp_pr_enum <- function(stan_model, bckchck){
  resp <- as.data.frame(t(rstan::extract(stan_model, pars = "responsibilities")$responsibilities))
  
  resp$enum <- bckchck$Ra$enum
  
  resp <- resp %>% group_by(enum) %>% summarise_all(mean)
  
  return(resp)
}

# function to calculate predicted lambda per enumerator
pred_lambda_pr_enum <- function(stan_model, bckchck){
  beta_0 <- rstan::extract(stan_model, pars = "beta_0")$beta_0
  beta_E <- rstan::extract(stan_model, pars = "beta_E")$beta_E
  
  lambda <- matrix(NA, nrow(beta_E), ncol(beta_E)) 
  
  for (c in 1:ncol(beta_E)){
    lambda[, c] <- beta_0 + beta_E[, c]
  }
  
  lambda <- exp(lambda)/(1 + exp(lambda))
  
  return(lambda)
}

# function to indicate whether credible interval contains specified value
ci_contains <- function(samples, val, ci_bounds){

  samp_bounds <- t(apply(samples, 2, quantile, probs = ci_bounds))
  
  val >= samp_bounds[,1] & val <= samp_bounds[,2]
  
}


# function that gets credible intervals for important evaluation measures
# this function is the first processing step after simulations
process_backcheck <- function(results, ci_bounds){
  
  #add median in
  ci_bounds <- c(ci_bounds, .5)
  
  # pull out and combine within iterations
  evals <- c("fnr", "fdr", "ppv", "npv")
  
  combined_evals <- map(evals, function(y) {
      map(results, y)
    }) %>% map(function(z){
      do.call(bind_rows, z)
    })
  
  #then, for each element get credible intervals at each value of s
  combined_evals <- map(combined_evals, function(x){
    temp <- apply(x, 2, quantile, probs = ci_bounds, na.rm = T) %>%
      t %>% as.data.frame() %>% rownames_to_column()
    temp$mean <- apply(x, 2, mean, na.rm = T)
    names(temp) <- c("s", "low", "high", "median", "mean") 
    return(temp)
  }) 
  
  #rename dataframes
  names(combined_evals) <- evals
  
  # pull out mean absolute error
  mae <- map_depth(results, 1, "mae") %>% do.call(c,.)
  mae <- c(quantile(mae, ci_bounds, na.rm = T), mean(mae))
  names(mae) <- c("low", "high", "median", "mean")
  
  #add mae to combined_evals
  combined_evals[["mae"]] <- mae
  
  #pull out and combine correlations
  cors <- map_depth(results, 1, "cors") %>% do.call(bind_rows,.)
  
  combined_evals[["cors"]] <- cors
  
  #scrub out npr, ppv, fdr, npv, mae from results
  results <- map(results, function(x){
    x[names(combined_evals)] <- NULL
    return(x)})
  
  list(combined_evals = combined_evals, results = results)
}

# function to calculate percent of max range
# used for deciding agreement values
getPerMaxRange <- function(a, b, r_a = NULL, r_b = NULL, trim = .99){
  if(is.null(r_b) & !is.null(r_a)) r_b <- r_a

  if(length(a) > 1 | length(b) > 1){
    if(mean(a, na.rm = T) > 1.25 * median(a, na.rm = T) &&
       mean(b, na.rm = T) > 1.25 * median(b, na.rm = T)){
      tmp_a <- a[a < quantile(a, trim, na.rm = T)]
      tmp_b <- b[b < quantile(b, trim, na.rm = T)]
      r_a <- max(tmp_a, na.rm = T) - min(a, na.rm = T)
      r_b <- max(tmp_b, na.rm = T) - min(b, na.rm = T)  
       
    } else if (mean(a, na.rm = T) < 1.25 * median(a, na.rm = T) &&
               mean(b, na.rm = T) < 1.25 * median(b, na.rm = T)){ 
      
      tmp_a <- a[a > quantile(a, 1-trim, na.rm = T)]
      tmp_b <- b[b > quantile(b, 1-trim, na.rm = T)]
      r_a <- max(a, na.rm = T) - min(tmp_a, na.rm = T)
      r_b <- max(b, na.rm = T) - min(tmp_b, na.rm = T)  
      
      } else {
      r_a <- max(a, na.rm = T) - min(a, na.rm = T)
      r_b <- max(b, na.rm = T) - min(b, na.rm = T) 
    }
  }
  
  1 - abs(a - b)/max(r_a, r_b)
}

# function to get difference vectors gammas
# adapted from fastLink::getPatterns (https://cran.r-project.org/package=fastLink)
getGamma <- function (matchesA, matchesB, varnames, stringdist.match, numeric.match, 
                      partial.match, stringdist.method = "jw", cut.a = 0.94,
                      cut.p = 0.88, jw.weight = 0.1, cut.a.num = .94, 
                      cut.p.num = .88, ordered.lim = 8, cut.a.ord = 0,
                      cut.p.ord = 1) 
{

  if (any(class(matchesA) %in% c("tbl_df", "data.table"))) {
    matchesA <- as.data.frame(matchesA)
  }
  if (any(class(matchesB) %in% c("tbl_df", "data.table"))) {
    matchesB <- as.data.frame(matchesB)
  }
  if (!(stringdist.method %in% c("jw", "jaro", "lv"))) {
    stop("Invalid string distance method. Method should be one of 'jw', 'jaro', or 'lv'.")
  }
  if (stringdist.method == "jw" & !is.null(jw.weight)) {
    if (jw.weight < 0 | jw.weight > 0.25) {
      stop("Invalid value provided for jw.weight. Remember, jw.weight in [0, 0.25].")
    }
  }
  if (any(stringdist.match * numeric.match) == 1) {
    stop("There is a variable present in both 'numeric.match' and 'stringdist.match'. Please select only one matching metric for each variable.")
  }
  gammalist <- vector(mode = "list", length = length(varnames))
  namevec <- rep(NA, length(varnames))
  ord <- FALSE
  for (i in 1:length(gammalist)) {
    if(is.ordered(matchesA[, varnames[i]]) | is.ordered(matchesB[, 
                                                                 varnames[i]])) {
      if(any(length(levels(matchesA[, varnames[i]])) <= ordered.lim,
             length(levels(matchesB[, varnames[i]])) <= ordered.lim)){
        ord <- TRUE
      }
      matchesA[, varnames[i]] <- as.numeric(matchesA[, 
                                                       varnames[i]])
      matchesB[, varnames[i]] <- as.numeric(matchesB[, 
                                                       varnames[i]])
    }
    if (is.factor(matchesA[, varnames[i]]) | is.factor(matchesB[, 
                                                                varnames[i]])) {
      matchesA[, varnames[i]] <- as.character(matchesA[, 
                                                       varnames[i]])
      matchesB[, varnames[i]] <- as.character(matchesB[, 
                                                       varnames[i]])
    }
    if (stringdist.match[i]) {
      if (stringdist.method %in% c("jw", "jaro")) {
        if (stringdist.method == "jw") {
          p1 <- jw.weight
        } else {
          p1 <- NULL
        }
        tmp <- 1 - stringdist::stringdist(matchesA[, varnames[i]], 
                                          matchesB[, varnames[i]], "jw", p = p1)
      } else {
        t <- stringdist::stringdist(matchesA[, varnames[i]], matchesB[, 
                                                                      varnames[i]],
                                    method = stringdist.method)
        t.1 <- nchar(matchesA[, varnames[i]])
        t.2 <- nchar(matchesB[, varnames[i]])
        o <- ifelse(t.1 > t.2, t.1, t.2)
        tmp <- 1 - t * (1/o)
      }
      if (partial.match[i]) {
        gammalist[[i]] <- ifelse(tmp >= cut.a, 2, ifelse(tmp >= 
                                                           cut.p, 1, 0))
      } else {
        gammalist[[i]] <- ifelse(tmp >= cut.a, 2, 0)
      }
    } else if (numeric.match[i]) {
      if(ord){
        tmp <- abs(matchesA[, varnames[i]] - matchesB[, varnames[i]])
        if (partial.match[i]) {
          gammalist[[i]] <- ifelse(tmp <= cut.a.ord, 2, 
                                   ifelse(tmp <= cut.p.ord, 1, 0))
        }
        else {
          gammalist[[i]] <- ifelse(tmp <= cut.a.ord, 2, 
                                   0)
        }
      } else {
        tmp <- getPerMaxRange(matchesA[, varnames[i]],
                              matchesB[, varnames[i]])
        if (partial.match[i]) {
          gammalist[[i]] <- ifelse(tmp >= cut.a.num, 2, ifelse(tmp >= 
                                                                 cut.p.num, 1, 0))
        } else {
          gammalist[[i]] <- ifelse(tmp >= cut.a.num, 2, 0)
        }
      }
    } else {
      tmp <- matchesA[, varnames[i]] == matchesB[, varnames[i]]
      gammalist[[i]] <- ifelse(tmp == TRUE, 2, 0)
    }
    namevec[i] <- paste0("gamma.", i)
    ord <- FALSE
  }
  gammalist <- data.frame(do.call(cbind, gammalist))
  names(gammalist) <- namevec
  return(gammalist)
}

# function to get comparisons values for all variables
# adapted from fastLink::getPatterns (https://cran.r-project.org/package=fastLink)
getComparisons <- function (matchesA, matchesB, varnames, stringdist.match, numeric.match, 
                      partial.match, stringdist.method = "jw", cut.a = 0.94,
                      cut.p = 0.88, jw.weight = 0.1, cut.a.num = .94, 
                      cut.p.num = .88, ordered.lim = 8, cut.a.ord = 0,
                      cut.p.ord = 1) 
{
  
  if (any(class(matchesA) %in% c("tbl_df", "data.table"))) {
    matchesA <- as.data.frame(matchesA)
  }
  if (any(class(matchesB) %in% c("tbl_df", "data.table"))) {
    matchesB <- as.data.frame(matchesB)
  }
  if (!(stringdist.method %in% c("jw", "jaro", "lv"))) {
    stop("Invalid string distance method. Method should be one of 'jw', 'jaro', or 'lv'.")
  }
  if (stringdist.method == "jw" & !is.null(jw.weight)) {
    if (jw.weight < 0 | jw.weight > 0.25) {
      stop("Invalid value provided for jw.weight. Remember, jw.weight in [0, 0.25].")
    }
  }
  if (any(stringdist.match * numeric.match) == 1) {
    stop("There is a variable present in both 'numeric.match' and 'stringdist.match'. Please select only one matching metric for each variable.")
  }
  gammalist <- vector(mode = "list", length = length(varnames))
  namevec <- rep(NA, length(varnames))
  ord <- FALSE
  for (i in 1:length(gammalist)) {
    if(is.ordered(matchesA[, varnames[i]]) | is.ordered(matchesB[, 
                                                                 varnames[i]])) {
      if(any(length(levels(matchesA[, varnames[i]])) <= ordered.lim,
             length(levels(matchesB[, varnames[i]])) <= ordered.lim)){
        ord <- TRUE
      }
      matchesA[, varnames[i]] <- as.numeric(matchesA[, 
                                                     varnames[i]])
      matchesB[, varnames[i]] <- as.numeric(matchesB[, 
                                                     varnames[i]])
    }
    if (is.factor(matchesA[, varnames[i]]) | is.factor(matchesB[, 
                                                                varnames[i]])) {
      matchesA[, varnames[i]] <- as.character(matchesA[, 
                                                       varnames[i]])
      matchesB[, varnames[i]] <- as.character(matchesB[, 
                                                       varnames[i]])
    }
    if (stringdist.match[i]) {
      if (stringdist.method %in% c("jw", "jaro")) {
        if (stringdist.method == "jw") {
          p1 <- jw.weight
        } else {
          p1 <- NULL
        }
        tmp <- 1 - stringdist::stringdist(matchesA[, varnames[i]], 
                                          matchesB[, varnames[i]], "jw", p = p1)
      } else {
        t <- stringdist::stringdist(matchesA[, varnames[i]], matchesB[, 
                                                                      varnames[i]],
                                    method = stringdist.method)
        t.1 <- nchar(matchesA[, varnames[i]])
        t.2 <- nchar(matchesB[, varnames[i]])
        o <- ifelse(t.1 > t.2, t.1, t.2)
        tmp <- 1 - t * (1/o)
      }
      gammalist[[i]] <- tmp
    } else if (numeric.match[i]) {
      if(ord){
        tmp <- abs(matchesA[, varnames[i]] - matchesB[, varnames[i]])
        gammalist[[i]] <- tmp
      } else {
        tmp <- getPerMaxRange(matchesA[, varnames[i]],
                              matchesB[, varnames[i]])
        gammalist[[i]] <- tmp
      }
    } else {
      tmp <- matchesA[, varnames[i]] == matchesB[, varnames[i]]
      gammalist[[i]] <- ifelse(tmp == TRUE, 2, 0)
    }
    namevec[i] <- paste0("gamma.", i)
    ord <- FALSE
  }
  gammalist <- data.frame(do.call(cbind, gammalist))
  names(gammalist) <- varnames
  return(gammalist)
}

# S3 generic function that perturbs a variable slightly, doing different things 
# based on type
# used in simulations to create errors
perturb <- function(val, ...){
  UseMethod("perturb")
}

# default method, makes no changes 
perturb.default <- function(val){
  return(val)
}

# perturb method for factors
perturb.factor <- function(val){
    if(is.ordered(val)){
      levs <- levels(val)
      l <- length(levs)
      w <- which(levs == val)
      if(l < 3) {
        return(val)
      } else if(l > 3 & l < 8){
        if(w == l) {
          val <- levs[w - 1]
        } else if(w == 1) {
          val <- levs[w + 1]
        } else {
          val <- levs[w + sample(c(-1, 1), 1)]
        }
        return(val)
      } else {
        if(w == l) {
          val <- levs[w - sample(c(1, 2), 1)]
        } else if(w == 1) {
          val <- levs[w + sample(c(1, 2), 1)]
        } else if ((w - 1) == 1) {
          val <- levs[w + sample(c(-1, 1, 2), 1)]
        } else if ((w + 1) == l){
          val <- levs[w + sample(c(-2, -1, 1), 1)]
        } else {
          val <- levs[w + sample(c(-2, -1, 1, 2), 1)]
        }
        return(val)
      }
    }
    return(val)
} 

# perturn method for numerics
perturb.numeric <- function(val){
  
  val_sign <- sign(val)
  
  val <- abs(val)
  
  val <- format(val, scientific = FALSE) 
  
  u <- runif(1, max = 3)
  
  if(u >= 0 & u < 1){
    val <- transpos(val)
  } else if (u >= 1 & u < 2){
    val <- typo_num(val)
  } else {
    val <- deletion(val)
  }
  
  val <- as.numeric(val)
  
  if((u >= .2 & u <= .25) |
      (u >= 1.2 & u <= 1.25) |
      (u >= 2.2 & u <= 2.25)){
    val <- -1*val_sign*val
  } else {
    val <- val_sign*val
  }
  
  return(val)  
}

# perturb method for characters
perturb.character <- function(val){
  u <- runif(1, max = 3)
  
  if(u >= 0 & u < 1){
    val <- transpos(val)
  } else if (u >= 1 & u < 2){
    val <- typo(val)
  } else {
    val <- deletion(val)
  }
  return(val)  
}

#function to evaluate individual model results
# used in simulation analysis
eval_model_oos <- function(result){
  list2env(result, environment())
  
  rm(result)
  
  #CALCULATE OOS RESP#
  
  #identify oos obs
  oos_ids <- bckchck$survey$resp_id[bckchck$survey$resp_id %nin%
                                      bckchck$Ra$resp_id]
  
  #now select only these observations
  bckchck_surv_oos <- bckchck$survey[bckchck$survey$resp_id %in% oos_ids, ]
  orig_surv_oos <- orig_surv[orig_surv$resp_id %in% oos_ids,]
  
  #order by resp_id
  bckchck_surv_oos <- arrange(bckchck_surv_oos, resp_id)
  orig_surv_oos <- arrange(orig_surv_oos, resp_id)
  
  #merge in new enumerator information
  enum_info <- bckchck$enum_probs[, c("enum", "enum_old")]
  enum_info$enum_new <- enum_info$enum
  enum_info$enum <- NULL
  orig_surv_oos <- full_join(orig_surv_oos, 
                             enum_info,
                             by = c("enum" = "enum_old"))
  
  resp_oos <- data.frame(calc_resp(model, bckchck_surv_oos, orig_surv_oos))
  
  truth_oos <- bckchck$match_status[bckchck$match_status$resp_id %in%
                                      oos_ids,]$match
  
  #EVALUATE IDENTIFCATION OF "FAKE" OBS#
  
  auc_oos <- get_auc(resp_oos, truth_oos)
  fdr_oos <- FDR(t(as.matrix(resp_oos)), truth_oos, .5)
  fnr_oos <- FNR(t(as.matrix(resp_oos)), truth_oos, .5)
  
  list(auc_oos = auc_oos,
       fdr_oos = fdr_oos,
       fnr_oos = fnr_oos)
  
}

#function to evaluate how well model assess survey quality
# used in simulation analysis
eval_surv_qual <- function(result, true_beta_0){
  list2env(result, environment())
  rm(result)
  
  # Survey Quality (Beta0) Bias
  beta_0_bias <- rstan::extract(model, par = "beta_0")$beta_0 -
    true_beta_0
  beta_0_bias <- as.vector(beta_0_bias)
    
  # Survey Quality (Match Prob) Bias - beta_0 as quality #
  surv_qual_bias_l <- gtools::inv.logit(rstan::extract(model, par = "beta_0")$beta_0) -
    mean(bckchck$match_status$match)
  surv_qual_bias_l <- as.vector(surv_qual_bias_l)
  
  # Survey Quality (Match Prob) Bias - avg resp as quality #
  surv_qual_est_r <- rstan::extract(model, par = "responsibilities")$responsibilities 
  surv_qual_est_r <- apply(surv_qual_est_r, 1, mean)
  surv_qual_bias_r <- surv_qual_est_r - mean(bckchck$match_status$match)
  
  # Enum int Bias #
  #extract intercept estimates
  beta_E_est <- rstan::extract(model, par = "beta_E")$beta_E
  #calculate enumerator intercept bias
  beta_E_bias <- t(beta_E_est) - bckchck$enum_probs$beta_E
  
  # Enum Quality (Match Probab) Bias - lambda_e as quality #
  lambda_e_est <- pred_lambda_pr_enum(model, bckchck)
  enum_qual_bias_l <- t(lambda_e_est) - bckchck$enum_probs$match_prob
  
  # Enum Quality (Match Prob) Bias - avg resp as quality
  resp_e_est <- as.matrix(resp_pr_enum(model, bckchck)[, -1])
  enum_qual_bias_r <- resp_e_est - bckchck$enum_probs$match_prob
  
  list(beta_0_bias = beta_0_bias,
       surv_qual_bias_l = surv_qual_bias_l,
       surv_qual_bias_r = surv_qual_bias_r,
       beta_E_bias = beta_E_bias,
       enum_qual_bias_l = enum_qual_bias_l,
       enum_qual_bias_r = enum_qual_bias_r)
  
}

#function to calculate responsibilities (posterior probability of a high quality)
calc_resp <- function(model, data1, data2, enum_var = "enum_new"){
  
  end_col <- ncol(data1)
  
  #get agreement summary vectors for oos
  agreements <- getGamma(data1[,3:end_col], 
                         data2[,3:end_col], 
                         varnames = names(data1[, 3:end_col]),
                         stringdist.match = c(FALSE, FALSE, FALSE, FALSE, 
                                              FALSE, TRUE, FALSE, FALSE, FALSE),
                         numeric.match = c(FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                                           TRUE, TRUE, TRUE),
                         partial.match = rep(TRUE, end_col - 2)) %>%
    to_multinomial()
  
  #extract parameters
  beta_0 <- as.vector(rstan::extract(model, pars = "beta_0")$beta_0)
  beta_E <- rstan::extract(model, pars = "beta_E")$beta_E
  pi_k_1 <- rstan::extract(model, pars = "pi_k_1")$pi_k_1
  pi_k_0 <- rstan::extract(model, pars = "pi_k_0")$pi_k_0
  
  #calculate lambdas
  lambdas <- t(gtools::inv.logit(beta_0 + beta_E[, as.data.frame(data2)[,enum_var]]))
  
  resp <- matrix(NA, nrow = nrow(data2), ncol = length(beta_0))
  #calculate responsibilities
  for(n in 1:nrow(resp)){
    loglik_1 <- log(lambdas[n, ]) + colSums(agreements[n,] * t(log(pi_k_1))) 
    loglik_0 <- log(1 - lambdas[n, ]) + colSums(agreements[n,] * t(log(pi_k_0)))
    
    loglik <- log(exp(loglik_1) + exp(loglik_0))
    
    resp[n,] <- exp(loglik_1 - loglik)
  }
  
  resp
  
}

# function to get auc
get_auc <- function(pred, truth){
  
  purrr::map_dbl(pred, function(x){
    as.numeric(pROC::auc(pROC::roc(truth ~ x,
                                   quiet = TRUE)))
  })
  
}


#helper function that puts together sublists of a list with the same name
extract_sublists <- function(list, names, simplify = TRUE){
  #browser()
  if(length(simplify) > 1){
    if(length(simplify) != length(names)){
      stop("names and simplify must be same length")
    }
  } else {
    simplify <- rep(simplify, length(names))
  }
  
  if(length(names) > 1){
    out <- mapply(FUN = function(x, y) sapply(list, function(z) z[[x]], simplify = y),
                  names,
                  simplify,
                  SIMPLIFY = FALSE)
  } else {
    out <- sapply(list, function(x) x[[names]], simplify = simplify)
  }
  
  out
}

#function to test how well weighting works for a single iteration of simulation
# used in simulation
eval_weighting <- function(result, mean_vars){
  list2env(result, environment())
  rm(result)
  
  #helper function for eval_weighting()
  weighted_analysis <- function(wght_vec, it_num){
    
    #helper function to calculate bias of variable mean as percent of truth
    get_wghtd_mean_bias <-  function(x, w, subset = 1:length(w)){
      w[is.na(w)] <- 0
      var <- bckchck$survey[[x]]
      orig_var <- orig_surv[[x]]
      if(length(levels(bckchck$survey[[x]])) == 2){
        var <- bckchck$survey[[x]]
        var <- as.numeric(var) - 1
        orig_var <- as.numeric(orig_var) - 1
      } 
      (weighted.mean(var[subset], w[subset], na.rm = T) - mean(orig_var, na.rm = T))/
        mean(orig_var, na.rm = T)
    }
    
    #helper function to calculate bias of variable mean, non-weighted version
    #as percent of truth
    get_mean_bias <-  function(x){
      var <- bckchck$survey[[x]]
      orig_var <- orig_surv[[x]]
      if(length(levels(bckchck$survey[[x]])) == 2){
        var <- as.numeric(var) - 1
        orig_var <- as.numeric(orig_var) - 1
      }
      (mean(var, na.rm = T) - mean(orig_var, na.rm = T))/mean(orig_var, na.rm = T)
    }
    
    bckchck$survey$wght_vec <- unlist(wght_vec)
    
    #unweighted
    lm_uw <- (coef(lm(lm_form,
                     data = bckchck$survey)) - betas_true)/betas_true
    means_uw <- map_dbl(mean_vars, get_mean_bias)
    names(means_uw) <- mean_vars
    
    #unweighted but with enumerator fixed effects
    lm_fe <- (coef(lm(update.formula(lm_form, . ~ . + as.factor(enum)),
                     data = bckchck$survey))[1:length(betas_true)] -
      betas_true)/betas_true
    
    #weighted using responsibilities, but only include backcheck cases 
    lm_r_bckchck <- (coef(lm(lm_form,
                            data = bckchck$survey,
                            weights = wght_vec)) - betas_true)/betas_true
    means_r_bckchck <- map_dbl(mean_vars,
                               .f = get_wghtd_mean_bias,
                               w = bckchck$survey$wght_vec)
    names(means_r_bckchck) <- mean_vars
    
    #weighted using responsibilities, inserting average resp for non-backcheck
    #units
    #adjust weights to replace NAs with iteration resp_enum
    bckchck$survey$wght_vec[bckchck$match_status$backcheck == 0] <-
      unlist(resp_enum[bckchck$survey$enum[bckchck$match_status$backcheck == 0],
                       paste0("resp_enum_it", it_num)])
    lm_r <- (coef(lm(lm_form,
                    data = bckchck$survey,
                    weights = wght_vec)) - betas_true)/betas_true
    means_r <- map_dbl(mean_vars,
                       .f = get_wghtd_mean_bias,
                       w = bckchck$survey$wght_vec)
    names(means_r) <- mean_vars
    
    #drop all observations where we are not certain of quality using responsibilites
    lm_drop_r <- (coef(lm(lm_form,
                         data = bckchck$survey %>%
                           filter(wght_vec > .5))) - betas_true)/betas_true
    means_drop_r <- map_dbl(mean_vars,
                            .f = get_wghtd_mean_bias,
                            w = bckchck$survey$wght_vec,
                            subset = bckchck$survey$wght_vec > .5)
    names(means_drop_r) <- mean_vars
    
    #weighted using responsibilities, inserting estimated lambda_e for non-backcheck
    #units
    #adjusting weights to replace NAs with iteration lambda 
    bckchck$survey$wght_vec[bckchck$match_status$backcheck == 0] <-
      unlist(lambda_enum[bckchck$survey$enum[bckchck$match_status$backcheck == 0],
                         paste0("lambda_it", it_num)])
    lm_rl <- (coef(lm(lm_form,
                     data = bckchck$survey,
                     weights = wght_vec)) - betas_true)/betas_true
    means_rl <- map_dbl(mean_vars,
                        .f = get_wghtd_mean_bias,
                        w = bckchck$survey$wght_vec)
    names(means_rl) <- mean_vars
    
    #weighted using lambda_e
    bckchck$survey$wght_vec2 <- lambda_enum[bckchck$survey$enum, 
                                            paste0("lambda_it", it_num)]
    lm_l <- (coef(lm(lm_form,
                    data = bckchck$survey,
                    weights = wght_vec2)) - betas_true)/betas_true
    means_l <- map_dbl(mean_vars,
                       .f = get_wghtd_mean_bias,
                       w = bckchck$survey$wght_vec2)
    names(means_l) <- mean_vars
    
    #drop enumerators where we are not sure of quality
    lm_drop_l <- (coef(lm(lm_form,
                         data = bckchck$survey %>% 
                           filter(wght_vec2 > .75))) - betas_true)/betas_true
    means_drop_l <- map_dbl(mean_vars,
                            .f = get_wghtd_mean_bias,
                            w = bckchck$survey$wght_vec2,
                            subset = bckchck$survey$wght_vec2 > .75)
    names(means_drop_l) <- mean_vars
    
    list(lm_uw = lm_uw, lm_fe = lm_fe, lm_r_bckchck = lm_r_bckchck,
         lm_r = lm_r, lm_l = lm_l, lm_rl = lm_rl,
         lm_drop_r = lm_drop_r, lm_drop_l = lm_drop_l,
         means_uw = means_uw, means_r_bckchck = means_r_bckchck,
         means_r = means_r, means_l = means_l, means_rl = means_rl,
         means_drop_r = means_drop_r, means_drop_l = means_drop_l
    )
    
  }
  
  #get last column
  end_col <- ncol(bckchck$survey)
  
  #calculate weights
  # distribution of lambdas per enumerator
  lambda_enum <- data.frame(enum = 1:nrow(bckchck$enum_probs),
                            t(pred_lambda_pr_enum(model, bckchck)))
  names(lambda_enum)[2:ncol(lambda_enum)] <- paste0("lambda_it",
                                                    1:(ncol(lambda_enum)-1))
  
  # responsibilities per enumerator
  resp_enum <- resp_pr_enum(model, bckchck)
  names(resp_enum)[2:ncol(resp_enum)] <- paste0("resp_enum_it",
                                                1:(ncol(resp_enum)-1))
  
  # add responsibilities posteriors to bckchck$survey
  resp_full <- data.frame(resp_id = bckchck$Ra$resp_id,
                          t(rstan::extract(model,
                                           pars = "responsibilities")$responsibilities))
  names(resp_full)[2:ncol(resp_full)] <- paste0("respon_it",
                                                1:(ncol(resp_full)-1))
  bckchck$survey <- full_join(bckchck$survey, resp_full,
                              by = "resp_id") %>% 
    full_join(bckchck$match_status %>% select(resp_id, backcheck), by = "resp_id")
  rm(resp_full)
  
  wghtd <- map2(bckchck$survey %>% select(contains("respon")),
                1:(ncol(resp_enum) - 1),
                .f = weighted_analysis)
  
  out <- extract_sublists(wghtd,
                          c("lm_uw", "lm_fe", "lm_r_bckchck",
                            "lm_r", "lm_l", "lm_rl",
                            "lm_drop_r", "lm_drop_l",
                            "means_uw", "means_r_bckchck",
                            "means_r", "means_l", "means_rl",
                            "means_drop_r", "means_drop_l"),
                          simplify = TRUE)
  
  out
  
}

#function to calculate JSD
calc_JSD <- function(result){
  list2env(result, envir = environment())
  pi_k_0 <- rstan::extract(model, pars = c("pi_k_0"))$pi_k_0
  pi_k_1 <- rstan::extract(model, pars = c("pi_k_1"))$pi_k_1
  l <- ncol(pi_k_0)
  
  pi_k_01 <- cbind(pi_k_0, pi_k_1)
  #calculate JS Divergence
  JSDiv <- apply(pi_k_01, 1, function(x){
    philentropy::distance(rbind(x[1:l], x[(l+1):(2*l)]),
                          method = "jensen-shannon",
                          unit = "log2",
                          mute.message = T)
  })
  #take square root to create metric
  sqrt(JSDiv)
}

