############################################################################################
# Posterior Calculations - Undecided voters and polling bias in US presidential elections
# Author: Joshua J Bon
# Notes: 
# - To appear in JRSS-A article by Bon, Ballard & Baffour
#
# - arXiv: https://arxiv.org/abs/1703.09430
#
# - Models: 
#   - Original SRGG model for 2004, 2008, 2012, 2016 US presidential elections
#   - Extended SRGG with bias away from proportional allocation of undecided voters
#   - Extended SRGG with bias away from even allocation of undecided voters
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

  library(stringr)
  library(plyr)
  library(dplyr)
  
  library(rv)
  library(gtools)
  
  library(ggplot2)
  library(scales)

####

#### Load Models ####
  
  # list of models (as environments)
  models <- list()
  
  models$prop <- readRDS("fitted-models/extended-SRGG-prop-fit.rds")
  models$even <- readRDS("fitted-models/extended-SRGG-even-fit.rds")
  models$SRGG <- readRDS("fitted-models/SRGG-fit.rds")
  
  # set number of simulations for RV package
  setnsims(length(rstan::extract(models$SRGG$stan_fit, "lp__")[[1]]))
  getnsims()
  
  if(any(diff(sapply(models, function(l){length(extract(l$stan_fit, "lp__")[[1]])})) != 0)){
    warning("Simulations from stan objects loaded are not equal in length")
  }

####
  
#### Extra rv functins ####
  
  ilogit.rv <- function(x) rvmapply(FUN = inv.logit, x)
  logit.rv <- function(x) rvmapply(FUN = logit, x)
  
  rv_sm <- function(rr, cnames = NULL){
    xx <- rbind(
      rvmean(rr) %>% round(digits = 1),
      rvsd(rr) %>% round(digits = 2)
    )
    
    dimnames(xx) <- list(c("mean", "sd"), 1:ncol(xx))
    
    if(!is.null(cnames)) colnames(xx) <- cnames
    
    return(xx)
  }
  
####

#### Original model - Bias summaries ####
  
  models$SRGG <- within(models$SRGG,{
    
    postrv <- as.rv(stan_fit)
    syid <- stan_data$state_year_id

    p_i <- ilogit.rv(
      logit(stan_data$vote[syid]) + postrv$alpha[syid] + postrv$beta[syid] * stan_data$t
    )
    
    p_i_e <- ilogit.rv(
      logit(stan_data$vote[syid]) + postrv$alpha[syid]
    )
    
    sig_i <- sqrt(
      (p_i * (1 - p_i) / stan_data$sample_size) + postrv$tau_sq[syid]
    )
    
    SRS_i <- sqrt(
      (p_i * (1 - p_i) / stan_data$sample_size)
    )
    
    N_races <- stan_data$S
    Y <- stan_data$Y
    
    b_r <- rv(N_races)
    b_r_e <- rv(N_races)
    sd_r <- rv(N_races)
    SRS_r <- rv(N_races)
    
    for(i in 1:N_races){
      b_r[i]   <- mean(p_i[i == stan_data$state_year_id]) - stan_data$vote[i]
      b_r_e[i] <- mean(p_i_e[i == stan_data$state_year_id]) - stan_data$vote[i]
      sd_r[i]  <-  mean(sig_i[i == stan_data$state_year_id])
      SRS_r[i]  <-  mean(SRS_i[i == stan_data$state_year_id]) 
    }
    
    # overall summaries (mean abs)
    mabs_b_r <- mean(abs(b_r))
    mabs_b_r_e <- mean(abs(b_r_e))
    m_sd_r <- mean(sd_r)
    m_SRS_r <- mean(SRS_r)
    
    yearly_mabs_b_r <- rv(Y)
    yearly_mabs_b_r_e <-rv(Y)
    yearly_m_sd_r <-rv(Y)
    yearly_m_SRS_r <-rv(Y)
    
    # summaries by year
    for(i in 1:4){
      yearly_mabs_b_r[i] <- mean(abs(b_r[i == vote_data$year_id]))
      yearly_mabs_b_r_e[i] <- mean(abs(b_r_e[i == vote_data$year_id]))
      yearly_m_sd_r[i] <- mean(sd_r[i == vote_data$year_id])
      yearly_m_SRS_r[i] <- mean(SRS_r[i == vote_data$year_id])
    }
    
  })
  
  # remove stan fit (and rv as already saved), and unwanted rvs
  # and save summary rvs
  remove_items <- c("postrv","stan_fit","i","stan_data","SRS_i","sig_i","p_i","p_i_e")
  models$SRGG[remove_items] <- NULL

  saveRDS(models$SRGG, file = "fitted-models/SRGG-summary-rvs.rds")
  
####
  
#### Extended model - Proportional (or even) Und Alloc - Bias summaries ####
  # See Appendix B for more details
  
  for(mdn in c("prop","even")){
    
    models[[mn]] <- within(models[[mn]],{
      
      postrv <- as.rv(stan_fit)
      syid <- stan_data$state_year_id

      # update gamm (if gamm * 10 is in stan model)
      postrv$gamm <- postrv$gamm * 10
    
      # Include undecided bias (if applicable)
      p_i <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid] + 
          postrv$beta[syid] * stan_data$t + 
          postrv$kappa[stan_data$house_id] - 
          postrv$gamm[stan_data$rmargin_year_id] * postrv$alpha_und[syid]
      )
      
      p_i_e <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid] + 
          postrv$kappa[stan_data$house_id] -
          postrv$gamm[stan_data$rmargin_year_id] * postrv$alpha_und[syid]
      )
      
      p_i_u <- ilogit.rv(
        logit(stan_data$vote[syid]) - 
          postrv$gamm[stan_data$rmargin_year_id] * postrv$alpha_und[syid]
      ) 
      
      p_i_h <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$kappa[stan_data$house_id] 
      )
      
      # Exclude undecideds
      p_i_0u <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid] + 
          postrv$beta[syid] * stan_data$t + 
          postrv$kappa[stan_data$house_id]
      )
      
      p_i_e_0u <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid] + 
          postrv$kappa[stan_data$house_id]
      )
      
      # Exclude house effects
      p_i_0h <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid] + 
          postrv$beta[syid] * stan_data$t - 
          postrv$gamm[stan_data$rmargin_year_id] * postrv$alpha_und[syid]
        
      )
      
      p_i_e_0h <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid] -
          postrv$gamm[stan_data$rmargin_year_id] * postrv$alpha_und[syid]
        
      )
      
      # Exclude undecided bias and house effects
      p_i_0uh <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid] + 
          postrv$beta[syid] * stan_data$t
        
      )
      
      p_i_e_0uh <- ilogit.rv(
        logit(stan_data$vote[syid]) + 
          postrv$alpha[syid]
        
      )
      
      # Variance calcs
      sig_i <- sqrt(
        (p_i * (1 - p_i) / stan_data$sample_size) + postrv$tau_sq[syid]
      )
      
      SRS_i <- sqrt(
        (p_i * (1 - p_i) / stan_data$sample_size)
      )
      
      # constants
      N_races <- stan_data$S
      Y <- stan_data$Y
      
      # Setup bias 
      b_r <- rv(N_races)
      b_r_e <- rv(N_races)
      b_r_u <- rv(N_races)
      b_r_h <- rv(N_races)
      
      b_r_0u <- rv(N_races)
      b_r_e_0u <- rv(N_races)
      b_r_0h <- rv(N_races)
      b_r_e_0h <- rv(N_races)
      b_r_0uh <- rv(N_races)
      b_r_e_0uh <- rv(N_races)
      
      und_level <- rv(N_races)
      
      sd_r <- rv(N_races)
      
      SRS_r <- rv(N_races)
      
      for(i in 1:N_races){
        b_r[i]   <- mean(p_i[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_e[i] <- mean(p_i_e[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_u[i] <- mean(p_i_u[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_h[i] <- mean(p_i_h[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_0u[i] <- mean(p_i_0u[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_e_0u[i] <- mean(p_i_e_0u[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_0h[i] <- mean(p_i_0h[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_e_0h[i] <- mean(p_i_e_0h[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_0uh[i] <-  mean(p_i_e_0uh[i == stan_data$state_year_id]) - stan_data$vote[i]
        b_r_e_0uh[i] <-  mean(p_i_e_0uh[i == stan_data$state_year_id]) - stan_data$vote[i]
        
        sd_r[i]  <-  mean(sig_i[i == stan_data$state_year_id])
        SRS_r[i] <-  mean(SRS_i[i == stan_data$state_year_id])
      }
      
      # overall summaries (mean abs)
      mabs_b_r <- mean(abs(b_r))
      mabs_b_r_e <- mean(abs(b_r_e))
      mabs_b_r_u <- mean(abs(b_r_u))
      mabs_b_r_h <- mean(abs(b_r_h))
      mabs_b_r_0u <- mean(abs(b_r_0u))
      mabs_b_r_e_0u <- mean(abs(b_r_e_0u))
      mabs_b_r_0h <- mean(abs(b_r_0h))
      mabs_b_r_e_0h <-mean(abs(b_r_e_0h))
      mabs_b_r_0uh <-mean(abs(b_r_0uh))
      mabs_b_r_e_0uh <-mean(abs(b_r_e_0uh))
      
      m_sd_r <- mean(sd_r)
      m_SRS_r <- mean(SRS_r)
      
      yearly_mabs_b_r <- rv(Y)
      yearly_mabs_b_r_e <-rv(Y)
      yearly_mabs_b_r_u <- rv(Y)
      yearly_mabs_b_r_h <- rv(Y)
      yearly_mabs_b_r_0u <- rv(Y)
      yearly_mabs_b_r_e_0u <- rv(Y)
      yearly_mabs_b_r_0h <- rv(Y)
      yearly_mabs_b_r_e_0h <- rv(Y)
      yearly_mabs_b_r_0uh <- rv(Y)
      yearly_mabs_b_r_e_0uh <- rv(Y)
      
      yearly_m_sd_r <-rv(Y)
      yearly_m_SRS_r <-rv(Y)
      
      # summaries by year
      for(i in 1:4){
        yearly_mabs_b_r[i] <- mean(abs(b_r[i == vote_data$year_id]))
        yearly_mabs_b_r_e[i] <- mean(abs(b_r_e[i == vote_data$year_id]))
        yearly_mabs_b_r_u[i] <- mean(abs(b_r_u[i == vote_data$year_id]))
        yearly_mabs_b_r_h[i] <- mean(abs(b_r_h[i == vote_data$year_id]))
        yearly_mabs_b_r_0u[i] <- mean(abs(b_r_0u[i == vote_data$year_id]))
        yearly_mabs_b_r_e_0u[i] <- mean(abs(b_r_e_0u[i == vote_data$year_id]))
        yearly_mabs_b_r_0h[i]    <- mean(abs(b_r_0h[i == vote_data$year_id]))
        yearly_mabs_b_r_e_0h[i]  <- mean(abs(b_r_e_0h[i == vote_data$year_id]))
        yearly_mabs_b_r_0uh[i]   <- mean(abs(b_r_0uh[i == vote_data$year_id]))
        yearly_mabs_b_r_e_0uh[i] <- mean(abs(b_r_e_0uh[i == vote_data$year_id]))
        
        yearly_m_sd_r[i] <- mean(sd_r[i == vote_data$year_id])
        yearly_m_SRS_r[i] <- mean(SRS_r[i == vote_data$year_id])
      }
      
      # pollster bias summary
      N_pollsters <- stan_data$H
      b_h <- rv(N_pollsters)
      hid <- stan_data$house_id
  
      for(i in 1:N_pollsters){
        which_polls <- i == hid
        which_votes <- stan_data$state_year_id[which_polls]
        b_h[i] <- mean(p_i_h[which_polls] - stan_data$vote[which_votes])
      }
      
      y_id_ord <- vote_data %>% select(year,year_id) %>% 
        unique() %>% arrange(year_id) %>% select(year) %>% collect() %>% .[[1]]
      
    })
    
  }
  
  saveRDS(models$SRGG, file = "fitted-models/SRGG-summary-rvs.rds")
  saveRDS(models$prop, file = "fitted-models/prop-summary-rvs.rds")
  saveRDS(models$even, file = "fitted-models/even-summary-rvs.rds")

####
