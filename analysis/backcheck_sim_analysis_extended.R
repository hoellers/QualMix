# Key Information --------------------------------------------------------------
# Title: Backchecking Demonstration: Analysis of Backchecking Simulation
# Project: A Mixture Model Approach to Assessing Measurement Error in Surveys 
#          Using Reinterviews
# Purpose: This script does additional processing of data from the simulation
#          and produces figures for the main paper and supplementary appendix
# ---------------------------------------------------------------------------- #

# Setup ------------------------------------------------------------------------

# Working Directory
## **Can be changed before running analysis if not using R project** 
wd_path <- here::here()
setwd(wd_path)

# load required packages
library(tidyverse)
library(Cairo)
library(RColorBrewer)

# functions
source("scripts/convenience_functions.R")

# load parameter combinations
mc_params <- read.csv('data/mc_params.csv')

#specify quantiles
probs <- c(0.025, 0.5, 0.975)

lm_names <- c("lm_uw", "lm_fe", "lm_r_bckchck",
              "lm_r", "lm_l", "lm_rl",
              "lm_drop_r", "lm_drop_l")

means_names <- c("means_uw", "means_r_bckchck",
                 "means_r", "means_l", "means_rl",
                 "means_drop_r", "means_drop_l")

# create figures directory (if it doesn't exist already)
dir.create("figures")

## Additional Functions ####

# function that facilitates using latex code with the xtable package
sanitize_allow_latex <- function (str, type = "latex") 
{
  if (type == "latex") {
    result <- str
    result <- gsub("\\\\", "SANITIZE.BACKSLASH", 
                   result)
    #result <- gsub("$", "\\$", result, fixed = TRUE)
    result <- gsub(">", "$>$", result, fixed = TRUE)
    result <- gsub("<", "$<$", result, fixed = TRUE)
    result <- gsub("|", "$|$", result, fixed = TRUE)
    #result <- gsub("{", "\\{", result, fixed = TRUE)
    #result <- gsub("}", "\\}", result, fixed = TRUE)
    result <- gsub("%", "\\%", result, fixed = TRUE)
    result <- gsub("&", "\\&", result, fixed = TRUE)
    result <- gsub("_", "\\_", result, fixed = TRUE)
    result <- gsub("#", "\\#", result, fixed = TRUE)
    #result <- gsub("^", "\\verb|^|", result, 
    #    fixed = TRUE)
    result <- gsub("~", "\\~{}", result, fixed = TRUE)
    result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", 
                   result, fixed = TRUE)
    result <- gsub("$\\backslash$footnote", "\\footnote",
                   result, fixed = TRUE)
    result <- gsub("$\\backslash$textbf", "\\textbf",
                   result, fixed = TRUE)
    result <- gsub("$\\backslash$protect", "\\protect",
                   result, fixed = TRUE)  
    result <- gsub("$\\backslash$$\\backslash$", "\\\\",
                   result, fixed = TRUE)
    result <- gsub("$\\backslash$makecell", "\\makecell",
                   result, fixed = TRUE)
    result <- gsub("$\\backslash$beta\\_", "\\beta_",
                   result, fixed = TRUE) 
    return(result)
  }
  else {
    result <- str
    result <- gsub("&", "&amp;", result, fixed = TRUE)
    result <- gsub(">", "&gt;", result, fixed = TRUE)
    result <- gsub("<", "&lt;", result, fixed = TRUE)
    return(result)
  }
}

# function that returns the quantile for each evaluation measure as a data frame
# for a single results file and returns them all as one list
process_results <- function(file, probs){
  load(file)
  results <- list(mod_eval, qual_eval, weight_eval, JSD)
  
  get_bounds <- function(x, probs){

    if(is.null(nrow(x))){
      bounded <- as.data.frame(t(c(quantile(x, probs = probs),
                                   "mean" = mean(x),
                                   "var" = var(x)
                                   )
                                 )) 
    } else {
      bounded <- as.data.frame(t(apply(x, 1, function(y){

        c(quantile(y, probs = probs), "mean" = mean(y),
          "var" = var(y))}
        )))
      bounded$name <- rownames(bounded)
      rownames(bounded) <- NULL
    }
    bounded
  }
  
  results <- lapply(results, function(x) { lapply(x, get_bounds, probs = probs)})
  results <- do.call(c, results)
  results
}

#function to bind together results
bind_results <- function(results_all, id_vec){
  temp_fun1 <- function(x, y) {
    x$iter_params <- y
    x
  }
  
  temp_fun2 <- function(x, y){
    mapply(temp_fun1, x = x, y = y, SIMPLIFY = FALSE) 
  }
  
  results_all <- lapply(results_all,
                        temp_fun2,
                        y = id_vec)
  
  lapply(results_all, function(x){ do.call(rbind, x)})
}

#function to plot analyses where there is one row for each parameter set
plot_onerows <- function(df, title, ylim = NULL, invisible = T,
                         folder = "figures/", save = T,
                         font_size = 11){
  require(Cairo)
  
  getwd()
  
  file_name <- paste0(folder, gsub(" ", "", title),".png") 
  
  fig <- ggplot(df, aes(y = `50%`, x = iter_params)) + geom_point() +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = .5) +
    labs(x = '', y = '', title = title) +
    coord_cartesian(ylim = ylim) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 270, size = font_size, vjust = .5),
          axis.text = element_text(size = font_size),
          legend.position = 'top',
          legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'))
  if(!invisible) print(fig)
  
  if(save) {
    ggsave(file_name, fig, width = 8, height = 7.5, units = "in")
  }
  
  return(fig)
  
}

# function to plot analyses where there is more than one row for each parameter
# set
plot_multirows <- function(df, title, ylim = NULL, invisible = T,
                           folder = "figures/", save = T,
                           font_size = 11){
  require(Cairo)
  
  getwd()
  
  file_name <- paste0(folder, gsub(" ", "", title),".png") 
  
  fig <- ggplot(df, aes(y = mean, x = iter_params, color = name)) +
    geom_point() +
    geom_line(aes(group = name)) +
    labs(x = '', y = '', title = title) +
    coord_cartesian(ylim = ylim) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 270, size = font_size, vjust = .5),
          axis.text = element_text(size = font_size),
          legend.position = 'none',
          legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm'))
  
  if(length(unique(df$name)) > 12){
    coul <- colorRampPalette(brewer.pal(12, "Paired"))(length(unique(df$name)))
    fig <- fig + scale_colour_manual(values=coul)
  } else {
    fig <- fig + scale_colour_brewer(palette = "Paired")
  }
  
  scale_colour_brewer(palette = "Paired")
  
  if(!invisible) print(fig)
  
  if(save) {
    ggsave(file_name, fig, width = 8, height = 7.5, units = "in")
  }
  
  return(fig)
  
}


plot_weighted_types <- function(df, title, var, ylim = NULL, invisible = T,
                          folder = "figures/", save = T){
  
  require(Cairo)
  
  df <- df[df$name %in% var,]
  
  getwd()
  
  file_name <- paste0(folder, gsub(" ", "", title),".png") 
  
  fig <- ggplot(df, aes(y = mean, x = iter_params, color = type)) +
    geom_point() +
    #geom_line(aes(group = iter_params)) +
    labs(x = '', y = '', title = title) +
    coord_cartesian(ylim = ylim) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 270, size = 7.5, vjust = .5),
          legend.position = 'right',
          legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm')) +
    scale_colour_brewer(palette = "Paired")
  if(!invisible) print(fig)
  
  if(save) {
    ggsave(file_name, fig, width = 8, height = 7, units = "in")
  }
  
  return(fig)
  
}

plot_weighted_vars <- function(df, title, type, ylim = NULL, invisible = T,
                          folder = "figures/", save = T){
  
  require(Cairo)
  
  df <- df[df$type %in% type,]
  
  getwd()
  
  file_name <- paste0(folder, gsub(" ", "", title),".png") 
  
  fig <- ggplot(df, aes(y = mean, x = iter_params, color = name)) +
    geom_point() +
    #geom_line(aes(group = iter_params)) +
    labs(x = '', y = '', title = title) +
    coord_cartesian(ylim = ylim) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 270, size = 7.5, vjust = .5),
          legend.position = 'right',
          legend.margin=margin(t = 0, r = 0, b = 0, l = 0, unit = 'mm')) +
    scale_colour_brewer(palette = "Set3")
  if(!invisible | save) print(fig)
  
  if(save) {
    ggsave(file_name, fig, width = 8, height = 7, units = "in")
  }
  
  return(fig)
  
}

# Processing -------------------------------------------------------------------

# initiall processing of results
mc_params$iter_params <- apply(mc_params, 1, function(x){
  paste(paste0("%R=", x['backcheck_portion']),
        paste0("logit\u207B\u00B9\u2009(\u03B2\u2080)", "=", x['beta_0']),
        sep = "; ")
})
#note: \u207B = superscipt minus; \u00B9 = superscript 1
#\u2009 = thinspace (to avoid overlap), \u03B2 is Greek beta;
#\u2080 = subscript 0
mc_params$iter_params <- factor(mc_params$iter_params, 
                                levels = mc_params$iter_params)
# create vector of file paths to simulation results stored in MC_Output folder
files <- list.files(path = "data/MC_Output/",
                    pattern = "results*")
# reordered this list so that it is in numerical order by simulation number, 
# not alphabetical order
ord <- str_split(str_split(files, "results", simplify = T)[,2], ".RData", simplify = T)[,1]
ord <- data.frame(file = files, ord_num = as.numeric(ord))
ord <- ord %>% arrange(ord_num)

# process all files
results_all <- lapply(paste0("data/MC_Output/" ,ord$file), process_results, probs = probs)
# this results in a list with 12 elements, where each element corresponds to one
# simulation (set of parameters)

# now we put all subresults into their own lists, grouping together all types of
# analyses
results_all <- extract_sublists(results_all, names(results_all[[1]]),
                                simplify = FALSE)
results_all <- bind_results(results_all, mc_params$iter_params)

# process the linear regression (part of simulation) results
lm_all <- mapply(function(x, y){
  x$type <- y
  x
  },
  results_all[lm_names],
  lm_names,
  SIMPLIFY = FALSE)
lm_all <- do.call(rbind, lm_all)

# process mean calculation (part of simulation) results, not used in paper
means_all <- mapply(function(x, y){
  x$type <- y
  x
  },
  results_all[means_names],
  means_names,
  SIMPLIFY = FALSE)
means_all <- do.call(rbind, means_all)

# Load 1 result object for extracting simulation set up
load("data/MC_Output/results1.RData")

# Main Paper -------------------------------------------------------------------

## Section 4.1.2 ####

### JSD #### 
results_all$JSD

### Figure 1 ####

# Survey Quality Bias - Using Mean Post. Prob. of High Quality (Figure 2)
plot_onerows(results_all$surv_qual_bias_r, "Survey Quality Error") 

### Table 3 ####

# get summary statistics of enumerator quality biases
enum_qual_bias_r_sum <- results_all$enum_qual_bias_r %>%
  group_by(iter_params) %>% 
  summarize(Mean = mean(mean),
            SD = sd(mean),
            Min = min(mean),
            Max = max(mean))
# fix iter_params column for inclusion in document
enum_qual_bias_r_sum$iter_params <- paste0("%R=", 
                                           rep(c(0.05, 0.1, 0.15, 0.2), 3),
                                           "; logit$^{-1}(\\beta_0)$ =",
                                           rep(c(0.8, 0.9, 0.95), each = 4))
names(enum_qual_bias_r_sum)[1] <- "Simulation Parameters" 
# produces LaTeX code for table 3
xtable::print.xtable(xtable::xtable(enum_qual_bias_r_sum,
                                    label = "tab:enum_qual",
                                    header = F,
                                    caption = "Enumerator Quality Bias Summarized by Simulation Parameters. Calculated across enumerator-specific biases.",
                                    align = c("l", "p{0.35\\textwidth}",
                                              "p{0.1\\textwidth}",
                                              "p{0.1\\textwidth}",
                                              "p{0.1\\textwidth}"
                                              ,"p{0.1\\textwidth}"),
                                    digits = 4),
                     comment = F, include.rownames = F, booktabs = T,
                     hline.after = c(-1, 0, 1:nrow(enum_qual_bias_r_sum)),
                     sanitize.text.function = sanitize_allow_latex,                       
                     caption.placement = "bottom",
                     table.placement = "!htbp")

### Descriptive Statistics ####


# maximum bias of enumerator quality estimates
results_all$enum_qual_bias_r$mean[which.max(results_all$enum_qual_bias_r$mean)]

#minimum bias of enumerator quality estimates
results_all$enum_qual_bias_r$mean[which.min(results_all$enum_qual_bias_r$mean)]

# Appendix Materials -----------------------------------------------------------

## Appendix D.1 ####

## JSD ##

plot_onerows(results_all$JSD, "JSD")

## Appendix F ####

#how many observations per enumerator (enumerators chosen for each parameter set)
#are the same because of how seed is set.
# (footnote 7 Appendix F)
enum_probs[[1]]$num_pr_enum[order(enum_probs[[1]]$num_pr_enum)]
#total observations
sum(enum_probs[[1]]$num_pr_enum[order(enum_probs[[1]]$num_pr_enum)])

## Appendix G ####

### Difference in maximum and minimum AUCs ####
results_all$auc_oos[,"50%"][which.max(results_all$auc_oos[,"50%"])] -
  results_all$auc_oos[,"50%"][which.min(results_all$auc_oos[,"50%"])]

### AUC (Figure G1) ####
plot_onerows(results_all$auc_oos, "AUC (OOS)")

### FNR (Figure G2 - A) ####
plot_onerows(results_all$fnr_oos, "FNR (OOS)")

### FDR (Figure G2 - A) ####
plot_onerows(results_all$fdr_oos, "FDR (OOS)")  

## Appendix H ####


### (Figure H1) ####

# Enumerator Quality Bias - Using Mean Post. Prob. of High Quality Per Enumerator
g1 <- plot_multirows(results_all$enum_qual_bias_r,
               "Enumerator Quality Bias - Using Mean Post. Prob. of High Quality",
               save = F) 

#### Identifying Negative Biases ####
trouble_enum <- unique(results_all$enum_qual_bias_r[results_all$enum_qual_bias_r$mean < -0.05, "name"])
trouble_enum <- as.numeric(trouble_enum)
enum_probs[[1]] %>% filter(enum %in% trouble_enum) %>% select(enum, match_prob)
(enum_probs[[1]] %>% arrange(beta_E))$enum

# getting values for text annotations
enum1 <- results_all$enum_qual_bias_r$mean[results_all$enum_qual_bias_r$name == "1"][7]
enum25 <- results_all$enum_qual_bias_r$mean[results_all$enum_qual_bias_r$name == "25"][7]
enum6 <- results_all$enum_qual_bias_r$mean[results_all$enum_qual_bias_r$name == "6"][7]

# adding enumerator labels to plot
g1 <- g1 + annotate(geom = "text", x = "%R=0.15; logit⁻¹ (β₀)=0.9",
                    y = enum1 - 0.005, label = "1") +
  annotate(geom = "text", x = "%R=0.15; logit⁻¹ (β₀)=0.9",
           y = enum25 - 0.005, label = "25") +
  annotate(geom = "text", x = "%R=0.15; logit⁻¹ (β₀)=0.9",
           y = enum6 + 0.005, label = "6")
ggsave(paste0("figures/", gsub(" ", "", "Enumerator Quality Bias - Using Mean Post. Prob. of High Quality"),
              ".png"),
       g1, width = 8, height = 7, units = "in")