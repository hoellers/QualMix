# Key Information --------------------------------------------------------------
# Title: Simulate R_a and R_b
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: These functions take a survey dataset and simulate response sets 
#          R_a and R_b as defined in the supplementary materials
# ---------------------------------------------------------------------------- #

# FUNCTIONS START --------------------------------------------------------------

# this function takes a survey dataset Rb and creates hypothetical set Ra from it
# where Rb is "correct" and R_a has "mistakes" given an overall match portion
# or a data frame with match portions by enumerator
create_Ra <- function(Rb, overall_match_portion = .9,
                         enum_probs = NULL,
                         incl_enum = T){
  
  if(incl_enum & ("enum" %nin% names(Rb))){
    stop("Enumerator ID var must be called 'enum'")
  }
  if("resp_id" %nin% names(Rb)){
    stop("Respondent ID var must be called resp_id")
  }
  
  #create container for match information
  match_status <- data.frame(resp_id = Rb$resp_id,
                             match = NA)
  
  #turn enum into id vector from 1 to length(unique(enum)) to help with Stan later
  if(incl_enum){
    if(is.character(Rb$enum) | is.factor(Rb$enum) | 
       is.numeric(Rb$enum) & min(Rb$enum) != 0 & 
       (length(unique(Rb$enum) > (max(Rb$enum) - min(Rb$enum))))){
      
      enum_key <- data.frame(enum_old = unique(Rb$enum),
                             enum_new = 1:length(unique(Rb$enum)))
      
      Rb$enum <- Rb[, "enum"] %>% 
        left_join(enum_key, by = c("enum" = "enum_old")) %>%
        pull(enum_new)
      
      #keep old enum id
      if(!is.null(enum_probs)){
        enum_probs <- enum_probs %>% full_join(enum_key,
                                               by = c("enum" = "enum_old"))
        enum_probs$enum_old <- enum_probs$enum
        enum_probs$enum <- enum_probs$enum_new
        enum_probs$enum_new <- NULL
        
        enum_probs <- arrange(enum_probs, enum)
      }
    }
    
    match_status$enum <- Rb$enum
    
    match_status <- match_status %>% full_join(enum_key, 
                                               by = c("enum" = "enum_new"))
  }
  
  #move enum, and resp_id to front so that they aren't replaced later
  if(incl_enum){
    Rb <- Rb %>% select(enum, resp_id, everything())
  } else {
    Rb <- Rb %>% select(resp_id, everything())
  }

  #initialize R_a
  Ra <- Rb  
  
  #select matches and non-matches
  if(is.null(enum_probs)){
    match1 <- rep(1, nrow(Rb) * overall_match_portion)
    match0 <- rep(0, nrow(Rb) - length(match1))
    match_status$match <- sample(c(match1, match0))
    
  } else {
  
    match_IDs <- map2(enum_probs$enum, enum_probs$match_prob, function(.x, .y){
      temp <- Rb$resp_id[Rb$enum == .x]
      return(sample(temp, round(.y * length(temp))))
    }
    ) %>% flatten_chr
    
    match_status$match <- as.integer(Rb$resp_id %in% match_IDs)
  }
  
  #replace non-matches with "wrong" observations selected from matches
  # (results in duplicates)
  ##first, select correct number of non-match ids
  non_match_num <- nrow(Rb) - sum(match_status$match)
  
  non_matches <- Rb %>% 
    filter(match_status$match == 1) %>% 
    slice_sample(n = non_match_num)
  # save original IDs for obs used to create non-matches
  orig_nm_IDs <- non_matches %>% pull(resp_id)
  if(incl_enum){
    non_matches <- non_matches %>% select(-resp_id, -enum)
  } else {
    non_matches <- non_matches %>% select(-resp_id)
  }
  
  # put "incorrect" observations into R_a
  if (incl_enum) {s <- 3} else {s <- 2} 
  Ra[match_status$match == 0, c(s:ncol(Ra))] <- non_matches
  
  if(!is.null(enum_probs)){
    #calculating actual match percentages per enumerator
    enum_probs <- full_join(enum_probs, match_status %>% group_by(enum) %>% 
      summarise(actual_match_per = mean(match)), by = "enum")
    #tracking number of respondents per enumerator
    enum_probs <- full_join(enum_probs, Rb %>% group_by(enum) %>%
      summarise(num_pr_enum = n()), by = "enum")
  }
  
  list(Ra = Ra,
       Rb = Rb,
       orig_nm_IDs = orig_nm_IDs,
       match_status = match_status,
       enum_probs = enum_probs,
       s = s)
}


# this function induces artificial similarities and differences into Ra,
# so that we cannot just perfectly identify matches by lack of errors and
# non-matches by all errors
induce_diff_sim <- function(RaRb, 
                            max_var_change_por = .4,
                            var_scramble_por = .5,
                            change_prob_lb = .25,
                            skip_nonmatch = T){
  #browser()
  if(max_var_change_por > 1 | max_var_change_por < 0){
    stop(sprintf("max_var_change_por must <= 1 and >= 0: you chose %s",
                 max_var_change_por))
  }
  
  if(var_scramble_por > 1 | var_scramble_por < 0){
    stop(sprintf("var_scramble_por must <= 1 and >= 0: you chose %s",
                 var_scramble_por))
  }
  
  #unpack list passed from create_Ra to environment
  list2env(RaRb, environment())
  rm(RaRb)
  
  #find out maximum possible number of vars to change
  tot_vars_change <- round(max_var_change_por * (ncol(Ra) - (s - 1)))
  
  if(!is.null(enum_probs)){
    # probability of error for matches is inverse of prob of match
    # so enum of lower quality will have higher number of errors than 
    # enum of relatively higher quality
    prob_1 <- 1 - enum_probs$match_prob[match_status$enum[match_status$match == 1]]
    prob_1[prob_1 < change_prob_lb] <- change_prob_lb
    
    match_status$num_vars_change[match_status$match == 1] <- 
      rbinom(sum(match_status$match == 1),
             tot_vars_change,
             prob = prob_1)
    
    # probability of error for nonmatches is same as prob of match (with some adj)
    # so enum of lower quality will have lower number of random "corrections" than 
    # enum of relatively higher quality
    match_status$num_vars_change[match_status$match == 0] <- 
      rbinom(sum(match_status$match == 0),
             tot_vars_change,
             prob = 1 - prob_1)
    
  } else {
    #decide how many vars to change for each obs, not stratified by enum
    match_status$num_vars_change <- sample(0:tot_vars_change,
                                           nrow(Ra),
                                           replace = T,
                                           prob = (tot_vars_change+1):1)
  }
  
  match_status$num_vars_scramble <- ceiling(var_scramble_por *
                                            match_status$num_vars_change)
  
  match_status$num_vars_perturb <- match_status$num_vars_change -
    match_status$num_vars_scramble

  #create container for storing scrambled variable names
  match_status$vars_scrambled <- vector("character", nrow(match_status))
  match_status$vars_perturbed <- vector("character", nrow(match_status))
  
  #save IDs of non-match observations for easy access
  non_match_IDs <- match_status$resp_id[match_status$match == 0]
  
  #scrambling variables
  for (n in 1:nrow(Ra)){
    #if no vars to be scrambled, skip to next
    if(match_status$num_vars_change[n] == 0) next
    
    #determine which vars to change
    cho_vars <- names(Ra)[sample(s:(ncol(Rb)), match_status$num_vars_change[n])]
    
    if(match_status$match[n] == 1){
      #determine which vars to scramble and which vars to
      #scramble
      cho_vars_s <- sample(cho_vars, match_status$num_vars_scramble[n])
      #perturb
      cho_vars_p <- cho_vars[cho_vars %nin% cho_vars_s]
      
      if(!is_empty(cho_vars_s)){
        #sample "wrong" match from original survey and pull in answers from there
        Ra[n, cho_vars_s] <- Rb[sample((1:nrow(Rb))[Rb$resp_id %nin%
                                                      c(non_match_IDs, orig_nm_IDs)],
                                       1), cho_vars_s]
      }

      if(!is_empty(cho_vars_p)){
        #perturb original answers slightly
        for(v in cho_vars_p){
          if(is.na(Ra[n, v])) next
          Ra[n, v] <- perturb(Ra[[v]][n])
        }
      }
      
      #track variables scrambled and perturbed
      match_status$vars_scrambled[n] <- toString(cho_vars_s)
      match_status$vars_perturbed[n] <- toString(cho_vars_p)
      
    } else if(match_status$match[n] == 0){
      if(skip_nonmatch){
        match_status$vars_scrambled[n] <- ""
        match_status$vars_perturbed[n] <- ""
        next
      } 
      
      #pull correct values back in, to simulate accidental agreements within 
      #non-matches
      Ra[n, cho_vars] <- Rb[n, cho_vars]
      
      #track variables scrambled
      match_status$vars_scrambled[n] <- toString(cho_vars)
      match_status$vars_perturbed[n] <- ""
    }
    

  }
  
  list(Rb = Rb, Ra = Ra, 
       match_status = match_status,
       orig_nm_IDs = orig_nm_IDs,
       non_match_IDs = non_match_IDs,
       s = s,
       enum_probs = enum_probs,
       max_var_change_por = max_var_change_por,
       var_scramble_por = var_scramble_por,
       change_prob_lb = change_prob_lb)

}

# third function
# takes a survey data set, creates mistakes in a version of it
# then creates Ra and Rb (which may contain a subset of variables);
# if backcheck is TRUE, samples
# certain percentage of respondents from each enumerator
simulate_Ra_Rb <- function(survey, 
                           match_vars = NULL, 
                           enumerator_var = NULL,
                           resp_id_var = NULL,
                           complete_cases = TRUE,
                           incl_enum = T,
                           overall_match_portion = .9,
                           max_var_change_por = .4,
                           var_scramble_por = .5,
                           change_prob_lb = .25,
                           skip_nonmatch = T,
                           enum_probs = NULL,
                           backcheck = FALSE,
                           backcheck_portion = .1,
                           seed = NULL){
  
  require(tidyverse)

  #set seed if not NULL
  if(!is.null(seed)) set.seed(seed)
  
  # if enumerator and respondent variables are not called enum and resp_id
  if(!is.null(enumerator_var) && (enumerator_var != "enum")){
    survey$enum <- pull(survey, enumerator_var)
    #remove original
    survey[, enumerator_var] <- NULL
  }
  
  if(!is.null(resp_id_var) && resp_id_var != "resp_id"){
    survey$resp_id <- pull(survey, resp_id_var)
    #remove original
    survey[, resp_id_var] <- NULL
  }
  
  #if requested, drop NAs before backchecks are created
  if(complete_cases){
    survey <- survey %>% tidyr::drop_na()
  }
  
  # cut down to only enumerators included in enum_probs
  if(!is.null(enum_probs)){
    survey <- survey %>% filter(enum %in% enum_probs$enum)
  }
  
  #create Ra and Rb set
  RaRb <- create_Ra(survey,
                    overall_match_portion = overall_match_portion, 
                    enum_probs = enum_probs,
                    incl_enum = incl_enum) %>% 
    induce_diff_sim(max_var_change_por = max_var_change_por,
                    var_scramble_por = var_scramble_por,
                    change_prob_lb = change_prob_lb,
                    skip_nonmatch = skip_nonmatch)
  
  RaRb$backcheck <- backcheck
  
  if(backcheck){
    #save "original" survey
    RaRb$survey <- RaRb$Ra
    
    if(is.null(enum_probs)){
      #sample proportion of respondents
      RaRb$Ra <- RaRb$Ra %>%
        slice_sample(prop = backcheck_portion)
    } else {
      #sample proportion of respondents per enumerator
      RaRb$Ra <- RaRb$Ra %>% group_by(enum) %>%
        slice_sample(prop = backcheck_portion) %>% ungroup()
    }
    
    RaRb$Rb <- RaRb$Rb %>% filter(resp_id %in% RaRb$Ra$resp_id)
    
    RaRb$match_status$backcheck <- ifelse(RaRb$match_status$resp_id %in%
                                            RaRb$Ra$resp_id,
                                     1,
                                     0)
    
    # make sure all data frames are in same order
    RaRb$match_status <- arrange(RaRb$match_status, resp_id) 
    RaRb$Rb <- arrange(RaRb$Rb, resp_id)
    RaRb$Ra <- arrange(RaRb$Ra, resp_id)
    RaRb$survey <- arrange(RaRb$survey, resp_id)
    
    bckchck_enum <- RaRb$match_status %>% 
      filter(backcheck == 1) %>% 
      group_by(enum) %>% 
      summarise(backcheck_match_per = mean(match),
                num_in_bckchck = n())
    RaRb$enum_probs <- full_join(RaRb$enum_probs, bckchck_enum,
                                 by = "enum")
  } else {
    # make sure all data frames are in same order
    RaRb$match_status <- arrange(RaRb$match_status, resp_id) 
    RaRb$Rb <- arrange(RaRb$Rb, resp_id)
    RaRb$Ra <- arrange(RaRb$Ra, resp_id)
    RaRb$survey <- arrange(RaRb$survey, resp_id)
  }
  
  #cut down to vars to be used for comparison, if requested
  if(!is.null(match_vars)){
    if(incl_enum){
      RaRb$Ra <- RaRb$Ra %>% select(resp_id, enum, one_of(match_vars))
      RaRb$Rb <- RaRb$Rb %>% select(resp_id, enum, one_of(match_vars))
    } else {
      RaRb$Ra <- RaRb$Ra %>% select(resp_id, one_of(match_vars))
      RaRb$Rb <- RaRb$Rb %>% select(resp_id, one_of(match_vars))
    }
  }
  
  
  return(RaRb)
  
}

# function that makes the enum_probs data frame used in the simulate_Ra_Rb
# functions if probability of match is stratified by enumerator.
make_enum_probs <- function(enum_ids,
                            n,
                            num_enum = "all",
                            enum_mean = log(.9/(1-.9)),
                            overall_match_sigma = 1,
                            smallest_group_size = 10){
  
  enum_probs <- data.frame(enum = enum_ids,
                           n = n,
                           match_prob = NA,
                           beta_E = NA)
  
  if(!is.null(smallest_group_size)){
    enum_probs <- enum_probs %>% 
      filter(n > smallest_group_size)
  }
  
  if(num_enum != "all"){
    if(nrow(enum_probs) < num_enum){
      warning("nrow(enum) probs < num_enum, all enum will be selected")
    } else{
      take <- base::sample(enum_probs$enum, num_enum) 
      enum_probs <- enum_probs %>% filter(enum %in% take)
    }
  }
  
  #decide on match vs non-match within enumerators using framework from model 
  #first, get different match probabilities
  enum_probs$beta_E <- rnorm(enum_probs$enum, 0, overall_match_sigma)
  enum_probs$match_prob <- gtools::inv.logit(x = enum_mean +
                                        enum_probs$beta_E)
  enum_probs
}
