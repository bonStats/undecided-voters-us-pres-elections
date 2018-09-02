########################################################################################
# State Level Poll Modelling - SRGG Extended Model - Proportionate allocation
# Author: Joshua J Bon
# Notes: 
# - To appear in JRSS-A article by Bon, Ballard & Baffour
#
# - arXiv: https://arxiv.org/abs/1703.09430
#
# - SRGG: Shirani-Mehr, H., Rothschild, D., Goel, S., & Gelman, A. (2018). 
#   Disentangling bias and variance in election polls. 
#   Journal of the American Statistical Association, 1-23
#
############################################################################################

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
    N_u = sum(!is.na(poll_data$Undecided)),
    S = nrow(vote_data),
    Y = length(unique(poll_data$year_id)),
    RMY = length(unique(poll_data$rmargin_year_id)), #for rmargin_year_id - number of levels for undecided bias effect
    H = length(unique(poll_data$pollster_id)),
    poll = with(poll_data, Rep_poll/(Rep_poll + Dem_poll)),
    und = with(poll_data %>% filter(!is.na(Undecided)), Undecided/(Rep_poll + Dem_poll + Undecided)),
    vote = with(vote_data, Rep_vote/(Rep_vote + Dem_vote)),
    t = as.numeric(with(poll_data, end_days_to_election/35)), # standardised to ~month (should be between 0 and 1)
    sample_size = poll_data$sample_size,
    rmargin_year_id = poll_data$rmargin_year_id,
    state_year_id = poll_data$state_year_id,
    year_id = poll_data$year_id,
    house_id = poll_data$pollster_id,
    sy_to_y_id = vote_data$year_id
  )
  
  # quick check of lengths
  sapply(stan_dat_list, length)
  
####

#### Run ext_prop Stan ####
  
  resStan <- stan(file = "stan_models/extended-SRGG-model.stan", data = stan_dat_list,
                  chains = 4, iter = 15000,
                  control = list(adapt_delta = 0.95)
                  )
  
  # saveRDS(list(stan_fit = resStan, stan_data = stan_dat_list, vote_data = vote_data, poll_data = poll_data), file = "fitted-models/extended-SRGG-prop-fit.rds")
  
#### 

  
#### Inspect ####
  
  launch_shinystan(resStan)
  
  print(resStan, pars = c("alpha","mu_alpha","sig_alpha"))
  
  print(resStan, pars = c("beta","mu_beta","sig_beta"))
  
  print(resStan, pars = c("theta","b_theta","sig_theta"))
  
  print(resStan, pars = c("tau","sig_tau"))
  
  print(resStan, pars = "state_year_bias")
  
  pairs(resStan, pars = c("mu_alpha","sig_alpha","mu_beta","sig_beta","mu_gamm","sig_gamm","sig_tau"))
  
####
