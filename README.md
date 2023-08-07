# Introducing QualMix

This README page serves to introduce the replication archive for “A
Mixture Model Approach to Assessing Measurement Error in Surveys Using
Reinterviews,” accepted for publication in the *Journal of Survey
Statistics and Methodology* (*JSSAM*).

See [Applying QualMix to Your Own Work](#sec-example) for a brief
demonstration of applying the method proposed in the analysis to your
own data. If you do use this approach, I only request that you please
cite the paper. You are welcome to adapt the code I wrote for thisp
roject, but please note the GPL3 license. I am working on a R package
that will implement this method more efficiently.

If you encounter any issues or have any questions, please do not
hesitate to let me know! Please see [File
Descriptions](#sec-file-descriptions) below for a description of all the
files contained in this repository.

# Required Packages

Please see `install_packages_qualmix_app.R`,
`install_packages_qualmix_common.R`, and
`install_packages_qualmix_app.R` in the `install_packages/` folder for
installation scripts for all required packages for the simulation
analysis and application parts of the analysis.

R and package versions used for analysis in paper:

- R: 4.3.0
- Tidyverse 2.0.0 packages:
  - `{dplyr_1.1.2}`
  - `{ggplot2_3.4.2}`
  - `{haven_2.5.2}`
  - `{purr_1.0.1}`
  - `{tidyr_1.3.0}`
  - `{tibble_3.2.1}`
- `{here_1.0.1}`
- `{pROC_1.18.0}`
- `{philentropy_0.7.0}`
- `{gtools_3.9.4}`
- `{stringdist_0.9.10}`
- `{rstan_2.21.8}`
- `{Cairo_1.6-0}`
- `{cmdstanr_0.5.3}`
  - `cmdstan`: 2.32.1
- `{labelled_2.11.0}`
- `{doParallel_1.0.17}`
- `{RColorBrewer_1.1-3}`
- `{xtable_1.8-4}`

# Replication

<div>

> **Backchecking = Reinterviewing**
>
> In my native discipline, the process of reinterviewing is often
> referred to as “backchecking.” During the work for this project and
> initial drafts, the paper retained this terminology. Because *JSSAM*
> is a general survey methodology journal, however, in consultation with
> the editors, I chose to change to “reinterviewing” throughout the
> paper, as “backcheck” is not frequently used in the survey methodology
> literature. As the code for this project was written before this
> change, the file and object names in the scripts still use the word
> backcheck (or variations thereof). Whenever you see “backcheck,” you
> can insert “reinterview.”

</div>

## Paper Results Replication

To replicate the analysis presented in the main paper and supplementary
appendix, please follow these steps:

1.  Clone this repository to your computer.
2.  Download the `MC_Output` folder
    [here](https://www.dropbox.com/sh/1j8rj3jfbd0mkbo/AABwxaanbp_YlMkGG9jZ6hnUa?dl=0),
    extract the files to the `data/MC_Output` folder. Please note that
    these are the simulation results. Together they total over 2 GBs in
    size (there are 12 files; each file is over 200 MBs) and are not
    stored on GitHub due to GitHub’s file size limitations.
3.  Open the `QualMix.rproj` file in RStudio.
4.  Run the following scripts:
    - `analysis/backcheck_sim_analysis_extended.R` - this replicates the
      simulation analysis results.
    - `analysis/backcheck_empirical_app.R` - this replicates the
      analysis for the empirical application part of the paper.

    The scripts can be run independently of one another. Whichever one
    is run first will create a a `figures/` folder that holds the
    figures found in the paper and supplementary materials.

Note: To replicate these results, no compilation of STAN models is
necessary, although you will still need to have `cmdstan` and
`{cmdstanr}` installed. If you **do** want to recompile the Stan models
from scratch, please delete the three `.exe` files in the `stan_models/`
folder before following step 4 above.

## Full Project Replication

This repo contains all the files necessary to fully recreate the project
results from scratch, including rerunning the simulations from scratch.
To do so, you have two options:

1.  Use a high performance computing cluster with a SLURM job scheduler.
2.  Use a personal computer.

Option 2 may take considerably longer.

If you want to recompile the Stan models from scratch, please delete the
three `.exe` files in the `stan_models/` folder before following the
steps below.

### On Cluster with a SLURM Job Scheduler:

1.  Clone the repository to your computer.
2.  Copy the following files from your computer to your working
    directory on the cluster:
    - `scripts/simulate_Ra_Rb.R`
    - `scripts/typos.R`
    - `scripts/convenience_functions.R`
    - `simulation/cluster/backcheck_sim_cluster.R`
    - `simulation/cluster/backcheck_setup.R`
    - `simulation/cluster/backcheck_master.sh`,
    - `simulation/cluster/backcheck_subprocess.sh`
    - `stan_models/MM_SQ_backchecking.stan`
    - `stan_models/MM_SQ_backchecking.exe` (only if you don’t want to
      recompile) Stan model).
3.  Change the working directory in `backcheck_sim_cluster.R` (`wd_path`
    object in line 24)
4.  Run command `sbatch backcheck_master.sh` from the command line
    interface on cluster. This will create a folder called `MC_Output/`
    in the working directory. It will also spawn 12 other jobs, each
    running the simulation and preliminary analysis for 1 of the 12
    simulation parameter combinations. After all 12 jobs have completed,
    this folder will contain RData objects `results1.RData` to
    `results12.RData`
5.  Copy the 12 `results*.RData` files from the cluster into the
    `data/MC_Output` folder in the version of the repo on your personal
    computer.
6.  Open the `QualMix.rproj` file in RStudio.
7.  Run the following scripts:
    - `analysis/backcheck_sim_analysis_extended.R` - this replicates the
      simulation analysis results.
    - `analysis/backcheck_empirical_app.R` - this replicates the
      analysis for the empirical application part of the paper.

    The scripts can be run independently of one another. Whichever one
    is run first will create a a `figures/` folder that holds the
    figures found in the paper and supplementary materials.

<div>

> **Note**
>
> The simulation was run on the Longleaf HPC cluster at UNC-Chapel Hill.
> Running it on a different cluster with different architecture may lead
> to slightly different results.

</div>

### On a Personal Computer

Please note that this is NOT RUN by me. I have only done limited testing
on `simulation/personal/backcheck_sim_personal.R` and so this approach
may require a bit more troubleshooting.

1.  Clone the repository to your computer.
2.  Open the `QualMix.rproj` file in RStudio.
3.  Run the script `simulation/personal/backcheck_sim_personal.R`
    - Please note that this can take a considerable amount of time. It
      can be parallelized across simulations by uncommenting line 41
      (although this will only lead to performance improvements on Linux
      and Apple machines); it is not parallelized across simulation
      parameter combinations.
4.  Run the following scripts:
    - `analysis/backcheck_sim_analysis_extended.R` - this replicates the
      simulation analysis results.
    - `analysis/backcheck_empirical_app.R` - this replicates the
      analysis for the empirical application part of the paper.

    The scripts can be run independently of one another. Whichever one
    is run first will create a a `figures/` folder that holds the
    figures found in the paper and supplementary materials.

# Applying QualMix to Your Own Work

With the files in this repository, it is straightforward to apply the
model to your own reinterviewing/backchecking data.

This section demonstrates a sample application with a subset of the data
used for the empirical application in the paper. It is important to
first source the `convenience_functions.R` script.

``` r
# necessary helper functions
source(here::here("scripts/convenience_functions.R"))
```

## Loading Data

For this brief example application we will work with only the
respondents to the long version of the survey (20% of the overall
sample, ~5% of which were reinterviewed – see Appendix K of the
supplementary materials for more information).

``` r
# load required packages
library(tidyverse)
library(haven)
library(stringdist)
library(philentropy)
library(rstan)
library(gtools)
library(cmdstanr)

# load original data (R_a)
load(here::here("data/surveys/vendor_end_nopid.RData"))

# load backcheck data (R_b)
load(here::here("data/surveys/vendor_end_long_bc_nopid.RData"))
```

Because we are limited by the reinterview data, we will have 158
observations in this example analysis.

There are a few data processing steps necessary, but they are omitted
here to save space. To see them, you can look at the underlying `.qmd`
file or look at lines 42-160 of `analysis/backcheck_empirical_app.R`.

It is important for the creating of the agreement vectors (the
$\gamma$’s) and the agreement summary vectors (the $\nu$’s) that the
variables in both data sets are the same type (numeric, character,
factor, etc), and that factors that have an ordering are explicitly
turned into ordered factors, as ordered factors are treated differently
from unordered factors.

     [1] "resp_id"          "enum"             "market"           "district"        
     [5] "duration"         "consent"          "d8"               "d9"              
     [9] "d12"              "d12_other"        "e3"               "e7_b"            
    [13] "e7_b_other"       "tc2"              "ms10"             "complete"        
    [17] "submissiondate"   "starttime"        "endtime"          "recent_receipt_7"
    [21] "receipt_shown"    "enum_id"         

    # A tibble: 158 × 6
          d8 d12        e3              e7_b                             tc2   ms10 
       <dbl> <fct>      <fct>           <fct>                            <fct> <fct>
     1    32 Standard 8 Every Day       Retail - Hardware                Rece… Some…
     2    42 Standard 8 4-6 days a week Retail - Hardware                Rece… Some…
     3    64 JCE/Form 2 4-6 days a week Other                            Rece… Some…
     4    45 Standard 8 4-6 days a week Retail - Agricultural produce (… Rece… Very…
     5    35 Form 1     4-6 days a week Retail - Hardware                No R… Some…
     6    35 Standard 5 Every Day       Retail - Groceries               Rece… Some…
     7    37 Standard 8 4-6 days a week Retail - Animal produce(meat,fi… No R… Some…
     8    28 Form 3     1-3 days a week Retail - Stationery/Printing     No R… Some…
     9    70 Standard 8 1-3 days a week Retail - Bags/Plastic Bags/Sacks Rece… Very…
    10    23 Standard 8 1-3 days a week Retail - Clothes/shoes           Rece… Some…
    # ℹ 148 more rows

    # A tibble: 158 × 6
          d8 d12             e3                   e7_b             tc2      ms10    
       <dbl> <dbl+lbl>       <dbl+lbl>            <dbl+lbl>        <dbl+lb> <dbl+lb>
     1    32  8 [Standard 8]  1 [Every Day]        6 [Retail - Ha…  1 [Yes]  2 [Som…
     2    42  8 [Standard 8]  3 [4-6 days a week]  6 [Retail - Ha…  1 [Yes]  2 [Som…
     3    64 10 [JCE/Form 2]  3 [4-6 days a week] 15 [Retail - Sa…  1 [Yes]  3 [Som…
     4    45  8 [Standard 8]  3 [4-6 days a week] 45 [Retail - Ba…  0 [No]   2 [Som…
     5    35  9 [Form 1]      1 [Every Day]        6 [Retail - Ha…  1 [Yes]  1 [Ver…
     6    35  5 [Standard 5]  3 [4-6 days a week]  1 [Retail - Gr…  1 [Yes]  2 [Som…
     7    37  9 [Form 1]      3 [4-6 days a week]  4 [Retail - An…  0 [No]   2 [Som…
     8    28 11 [Form 3]      1 [Every Day]        1 [Retail - Gr…  0 [No]   2 [Som…
     9    NA NA              NA                   NA               NA       NA      
    10    24  5 [Standard 5]  1 [Every Day]        8 [Retail - Cl…  1 [Yes]  2 [Som…
    # ℹ 148 more rows

       d8   d12    e3  e7_b   tc2  ms10 
     TRUE  TRUE  TRUE  TRUE FALSE  TRUE 

## Creating Agreement-Summary Vectors

The next step is to formally compare the original and reinterview data.

The `getGamma()` function does this for us. The first two arguments are
$\mathbf{R_a}$ and $\mathbf{R_b}$ as data frames. For simplicity’s sake,
both of these should contain only the variables that will be compared.

This function is adapted from the `getPatterns()` function from the
[`{fastLink}` package.](https://cran.r-project.org/package=fastLink).

There are other key arguments (with default values):

- `varnames`: string vector of variable names to compare
- `stringdist.match`: a logical vector of the length of `varnames` that
  specifies for which variables in `varnames` the string distance will
  be used for comparison (should be string variables)
- `numeric.match`: a logical vector of the length of `varnames` that
  specifies for which variables in `varnames` the percent max range
  should be used
- `partial.match`: a logical vector of the length of `varnames` that
  specifies which variable comparisons should allow for partial matches
- `stringdist.method`: the string distance method to use for comparing
  strings. See the `fastLink::getPatterns()` helpfile for more
  information. Default is `"jw"` for Jaro-Winkler
- `cut.a`: a numeric between 0 and 1 that marks the lower bound for a
  full string-distance match. Default is 0.94
- `cut.p`: a numeric between 0 and 1 that marks the lower bound for a
  partial string-distance match. Default is 0.88
- `jw.weigh`: weight parameter for the importance of the first
  characters of a string. Only applicable for the Jaro-Winkler string
  distance. Default is 0.10
- `cut.a.num`: a numeric between 0 and 1 that marks the lower bound for
  a full numeric match. Default is 0.94
- `cut.p.num`: a numeric between 0 and 1 that marks the lower bound for
  a partial numeric match. Default is 0.88
- `ordered.lim`: a positive integer that marks the upper bound for when
  an ordered factor variable is treated as an ordered factor variable
  versus a numeric variable. Default is 8
- `cut.a.ord`: a positive integer that marks the lower bound for a full
  ordered match. Default is 0
- `cut.p.ord`: a positive integer marks the lower bound for a partial
  ordered match. Default is 1

``` r
# specifying backchecking variables
backcheck_vars <- c("d8", "d12", "e3", "e7_b", "tc2", "ms10")

# getting agreement Matrix
agreements <- getGamma(vendor_end_orig %>% select(any_of(backcheck_vars)), 
                       vendor_end_long_bc %>% select(any_of(backcheck_vars)), 
                       varnames = backcheck_vars,
                       stringdist.match = c(FALSE, FALSE, FALSE,  
                                            TRUE, FALSE, FALSE),
                       numeric.match = c(TRUE, TRUE, TRUE, FALSE, FALSE,
                                         TRUE),
                       partial.match = rep(TRUE, length(backcheck_vars)))
# looking at first six agreement vectors
head(agreements)
```

      gamma.1 gamma.2 gamma.3 gamma.4 gamma.5 gamma.6
    1       2       2       2       2       2       2
    2       2       2       2       2       2       1
    3       2       2       2       0       2       2
    4       2       2       2       0       0       1
    5       2       2       1       2       0       1
    6       2       2       1       2       2       2

We use the `to_multinomial()` function to turn agreement vectors into
agreement- summary vectors, which become the inputs to the QualMix
model. For this application we turn `NA`’s into complete disagreements.

``` r
# turn all NAs into complete disagreements (0s)
agreements[is.na(agreements)] <- 0

# forming agreement summary vectors
Nu <- agreements %>% to_multinomial()

# printing first six agreement summary vectors
head(Nu)
```

         0 1 2
    [1,] 0 0 6
    [2,] 0 1 5
    [3,] 1 0 5
    [4,] 2 1 3
    [5,] 1 2 3
    [6,] 0 1 5

<div>

> **`getComparisons()`**
>
> The `getComparisons()` function, which takes the same arguments as
> `getGamma()`, allows users to print out the underlying comparison
> values used to determine complete and partial agreement or complete
> disagreement.
>
> ``` r
> # getting underlying comparison values
> comps <- getComparisons(vendor_end_orig %>% select(any_of(backcheck_vars)), 
>                         vendor_end_long_bc %>% select(any_of(backcheck_vars)), 
>                         varnames = backcheck_vars,
>                         stringdist.match = c(FALSE, FALSE, FALSE,  
>                                              TRUE, FALSE, FALSE),
>                         numeric.match = c(TRUE, TRUE, TRUE, FALSE, FALSE,
>                                           TRUE),
>                         partial.match = rep(TRUE, length(backcheck_vars)))
>
> # printing first 6 comparisons
> head(comps)
> ```
>
>       d8 d12 e3      e7_b tc2 ms10
>     1  1   1  0 1.0000000   2    0
>     2  1   1  0 1.0000000   2    1
>     3  1   1  0 0.3215054   2    0
>     4  1   1  0 0.7644360   0    1
>     5  1   1  1 1.0000000   0    1
>     6  1   1  1 1.0000000   2    0

</div>

Please see Appendix A of the Supplementary Materials for more discussion
of the comparisons and the impact of the decisions researchers must make
when implementing them.

## Fitting Model

<div>

> **`{cmdstanr}`**
>
> The `{cmdstanr}` package is used to fit the model. Please see
> [here](https://mc-stan.org/cmdstanr/index.html) for more information
> on this package. Please note that it uses the R6 OOP system (see
> [Chapter 14](https://adv-r.hadley.nz/r6.html) of *Advanced R, Second
> Edition* by Hadley Wickham for more information on this system).

</div>

To fit the model, we must first gather the data into a list, the format
required by Stan. Please note that we must specify priors; the ones
below are the ones used in the paper. Please see Appendix F.1 of the
Supplementary Materials for the full model specification.

``` r
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
```

We then compile the Stan model (if we have the pre-compiled `.exe` file,
the model is not actual compiled, but this *step is still required*).
Next, we fit the model. Finally, we convert the `cmdstanr` object to an
`rstan` object, as `{rstan}` has useful functions for extracting
parameters.

Please note that the `show_messages` and `show_exceptions` arguments for
the `$sample()` method are set to `FALSE`. This is to prevent a lot of
output from being included in this README file. However, if applying
this model to your own work, you should **keep these set to `TRUE`** to
avoid silencing very helpful diagnostic output.

``` r
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
  refresh = 500,
  show_messages = FALSE,
  show_exceptions = FALSE
)

# convert to rstan object (for ease of use)
bc_stanfit <- rstan::read_stan_csv(bc_fit$output_files())
```

## Analysis

We can treat `bc_stanfit` as a regular `rstan` object.

### Summarizing Distributions

We can plot the probability of seeing each of agreement values for the
two estimated components of the mixture. We can use the
`get_pi_k_1_probs_summary()` and `get_pi_k_0_probs_summary()`
convenience functions to help with this.

``` r
# specify 95% credible intervals
probs <- c(0.025, 0.5, 0.975)

# create data frames for each of the parameters 
pi_k_1 <- get_pi_k_1_probs_summary(bc_stanfit, probs)
pi_k_0 <- get_pi_k_0_probs_summary(bc_stanfit, probs)

#combine for plotting
pi_k <- rbind(pi_k_0, pi_k_1)

# Pi_K plots (like figure 3 in paper)
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
```

![](README_files/figure-commonmark/pi_ks-1.png)

## Jensen-Shannon Distance

We can use the `get_JSD_summary()` convenience function to get a quick
summary of the Jensen-Shannon Distance (JSD) for the two estimated
distributions.

``` r
get_JSD_summary(bc_stanfit, probs)
```

         2.5%       50%     97.5%      mean 
    0.6533192 0.7123503 0.7615031 0.7111271 

## Estimating Measurement Error

As explained in the paper, we can estimate the level of measurement
error in a survey based on the reinterview data by first estimating the
posterior probability that each observation chosen for the reinterview
is high quality (HQ) or not. We can extract the samples from this
distribution using the `get_post_prob_HQ()` helper function.

``` r
post_prob_HQ <- get_post_prob_HQ(bc_stanfit)
```

### Overall Survey Quality

We can then use the `get_surv_qual_summary()` function to get a summary
of the distribution of the overall survey quality (see Section 3 of the
paper for information on how this quantity is defined.)

``` r
get_surv_qual_summary(post_prob_HQ, probs = probs)
```

         2.5%       50%     97.5% 
    0.8144216 0.8262240 0.8393996 

### Enumerator Data Quality

We can also estimate the level of measurement error associated with each
of the enumerators who helped field the survey, if we have this
information. To do this we use the convenience function
`get_post_enum_qual()`, which extracts the samples from the joint
posterior enumerator quality distribution – note that we must provide
the output of `get_post_prob_HQ()` and the enumerator IDs. Next, we use
`get_post_enum_qual_summary`, which summarizes this distribution within
enumerators.

``` r
post_enum_qual_summary <- get_post_enum_qual(post_prob_HQ, 
                                             vendor_end_orig$enum_id) %>% 
  get_post_enum_qual_summary(probs)

# first six enumerators
head(post_enum_qual_summary)
```

      enum_id       Low    Median      High           sd      mean
    1       1 0.8388140 0.8453426 0.8461702 2.138440e-03 0.8446290
    2       2 0.8967524 0.9077564 0.9090867 3.883371e-03 0.9064978
    3       3 0.9998587 0.9999935 0.9999999 5.819644e-05 0.9999777
    4       4 0.8534200 0.8568774 0.8572109 1.190697e-03 0.8565065
    5       5 0.8261595 0.8327681 0.8333908 2.600823e-03 0.8319963
    6       6 0.8333409 0.8333648 0.8334886 8.334336e-05 0.8333776

#### Plotting Enumerator Data Quality

We can easily plot enumerator data quality using the output of
`get_post_enum_qual_summary()`.

``` r
ggplot(post_enum_qual_summary, 
       aes(y = Median, x = enum_id)) +
  geom_point() +
  geom_errorbar(aes(ymin = Low, ymax = High)) +
  labs(x = "Enumerator", y = "Average Posterior Probability\nof Belonging to High-Quality Distribution") +
  theme_bw() +
  ylim(c(0,1))
```

![](README_files/figure-commonmark/plot_enum_qual-1.png)

## Extensions

Please note that if you want to apply one of the various model
extensions to the QualMix model discussed in the supplementary
materials, you will need to modify and then re-compile the Stan code
found in `stan_models/MM_SQ_backchecking.stan`.

It is also possible to apply this model to different kinds of data, such
as panel data, with the goal of estimating the probability that
individuals interviewed over waves are actually the same. However, there
is currently no example of this ready.

# File Descriptions

This file breakdown follows the repo structure and list folders and
files in alphabetical order:

- `analysis/`
  - `backcheck_sim_analysis_extended.R`: script that performs analysis
    and makes figures for the simulation presented in the paper and
    appendix
  - `backcheck_empirical_app.R`: script that performs analysis and makes
    figures for the empirical application presented in the paper and
    appendix.
- `data/`
  - `MC_Output/`
    - `README.md`: Instructions for copying and pasting simulation
      results data into this folder.
    - unzip the `MC_Output/` folder from Dropbox into this folder
  - `surveys/`
    - `backcheck_survey.RData`: cleaned version of
      `vendor_end_nopid.RData` used for simulations.
    - `vendor_end_long_bc_nopid.RData`: RData file containing the
      backcheck data for the *long* version of the survey
    - `vendor_end_nopid.RData`: RData file containing the original data
      for the empirical application
    - `vendor_end_short_bc_nopid.RData`: RData file containing the
      backcheck data for the *short* version of the survey.
  - `mc_params.csv`: csv file containing simulation parameters created
    by `backcheck_setup.R` when replicating the simulation on a cluster
    or by `backcheck_sim_personal` when replicating the simulation on a
    personal computer. Included here to make it replicators are not
    using a cluster
- `install_packages/`
  - `install_packages_qualmix_app.R`: script that installs all packages
    necessary for the empirical application portion of the project
  - `install_packages_qualmix_common.R`: script that installs all
    packages necessary for both simulation and empirical application
    portion of the project.
  - `install_packages_qualmix_app.R`: script that installs all packages
    necessary for the empirical application portion of the project
- `scripts/`
  - `convenience_functions.R`: functions to help with analysis,
    including `getGamma()` and `getComparison()`, which help create
    agreement-summary vectors.
  - `remove_data_pid`: script that removes personal identifying
    information from data sets used in project; included for
    transparency reasons, but **not** runnable with data in `data/`
    folder (from which PID has already been removed)
  - `simulate_Ra_Rb.R`: functions to help with simulation of original
    data-backcheck dataset
  - `typos.R`: functions to help with simulation (adding typos to mimic
    data entry errors)
- `simulation/`
  - `cluster/`
    - `backcheck_master.sh`: SLURM job submission script for simulation
      (spawns 12 other job submissions)
    - `backcheck_setup.R`: script that creates a data frame with
      simulation parameters to make them accessible to all jobs
    - `backcheck_sim_cluster.R`: script performing simulation and
      preliminary analysis for a specific combination of simulation
      parameters intended to be run on a computing cluster using a SLURM
      job scheduler
    - `backcheck_subprocess.sh`: SLURM job submission script used for
      each combination of simulation parameters (called by
      `backcheck_master.sh`)
  - `personal/`
    - `backcheck_sim_personal.R`: a NOT RUN version of the simulation
      script that should work (slowly) on a personal computer
- `stan_models/`
  - `MM_SQ_backchecking.exe`: pre-compiled version of the QualMix model
  - `MM_SQ_backchecking.stan`: underlying Stan code for QualMix model
  - `receipts_qual_model.exe`: pre-compiled version of the poorly
    performing receipts validation model
  - `receipts_qual_model.stan`: underlying Stan code for poorly
    performing receipt model
  - `receipts_qual_model_simple.exe`: pre-compiled version of receipt
    model used in paper
  - `receipts_qual_model_simple.stan`: underlying Stan code for receipt
    model used in paper
- `QualMix.rproj`: R project file associated with repo
- `README.md`: this repo description and help guide
- `README.qmd`: file that creates this repo description and help guide.
