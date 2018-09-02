########################################################################################
# State Level Poll Modelling - SRGG Original model 
# Author: Joshua J Bon
# Date: 20180130
# Notes: 
# - Revision for JRSS
#
# - SRGG: Shirani-Mehr, H., Rothschild, D., Goel, S., & Gelman, A. (2018). 
#   Disentangling bias and variance in election polls. 
#   Journal of the American Statistical Association, 1-23
#
########################################################################################

#### Directory ####

  setwd("~/Dropbox/Research/Papers/JRSS_election/code/undecided-voters-us-pres-elections/")

####

#### Libraries ####

  library(rstan)
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  library(shinystan)
  library(stringr)
  library(dplyr)

####

#### Load Data ####

  poll_data <- readRDS("data/us-pres-state-polling-2004-2016.rds")
  vote_data <- readRDS("data/us-pres-state-voting-2004-2016.rds")
  
####

#### Set up stan model ####

  stan_dat_list <- list(
    N = nrow(poll_data),
    S = nrow(vote_data),
    poll = with(poll_data, Rep_poll/(Rep_poll + Dem_poll)),
    vote = with(vote_data, Rep_vote/(Rep_vote + Dem_vote)),
    sample_size = poll_data$sample_size,
    t = as.numeric(with(poll_data, end_days_to_election/35)), # standardised to ~month (should be between 0 and 1)
    state_year_id = poll_data$state_year_id
  )
  
  # quick check of lengths
  sapply(stan_dat_list, length)
  
####

#### SRGG original model ####  

  resStan <- stan(file = "stan_models/SRGG-model.stan", data = stan_dat_list,
                  chains = 4, iter = 150,
                  control = list(adapt_delta = 0.95)
  )
  
  
  #saveRDS(list(stan_fit = resStan, stan_data = stan_dat_list, vote_data = vote_data, poll_data = poll_data), file = "fitted-models/SRGG-fit.rds")
  
####
  
#### Inspect ####
  
  launch_shinystan(resStan)
  
  print(resStan, pars = c("alpha","mu_alpha","sig_alpha"))
  
  print(resStan, pars = c("beta","mu_beta","sig_beta"))
  
  pairs(resStan, pars = c("mu_alpha","sig_alpha","mu_beta","sig_beta","sig_tau"))
  
####
