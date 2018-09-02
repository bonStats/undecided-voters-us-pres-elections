 data {
  
   // Data ordered so that all obs with full information are first, followed by those with missing undecideds
  
   int<lower=1> N;                          //  Number of obs total

   int<lower=1> S;                          //  Number of states-year elections (+ DC)

   real<lower=0, upper=1> poll[N];          //  individual polls, Rep/(Rep + Dem)
   
   real<lower=0, upper=1> vote[S];          //  state vote outcome for each poll
   
   vector<lower=1>[N] sample_size;          // poll sample size
   
   vector<lower=0, upper=1>[N] t;           // days to election, standardised between 1 and 0
   
   int<lower=1, upper=S> state_year_id[N];  //  identifies state and year

   
 } 
 
transformed data {
  vector[S] logit_vote;
  for (i in 1:S){
    logit_vote[i] = logit(vote[i]);
  }
}
 
 parameters {
      
  // Polling Model
    // Hyper-parameters
      // bias - constant
      real mu_alpha;
      real<lower=0> sig_alpha;
      
      // bias - time-varying
      real mu_beta;
      real<lower=0> sig_beta;
      
      
      // excess variance
      real<lower=0> sig_tau;
    
    // Scaled Parameters
      vector[S] alpha_sc;
      vector[S]  beta_sc;
      vector<lower=0>[S] tau_sq_sc;
      
    
 } 
 
 transformed parameters {
   
  // Polling Model
   // Parameters
     vector[S] alpha;                           // bias in each state
     vector[S] beta;
     vector<lower=0>[S] tau_sq;     
     
  // Polling Model
    // Parameters - Definitions (due to scaling)
     // alpha ~ normal(mu_alpha,sig_alpha)
     alpha = mu_alpha + sig_alpha * alpha_sc;
     
     // beta ~ normal(mu_beta,sig_beta)
     beta = mu_beta + sig_beta * beta_sc;
     
     // tau ~ normal(0,sig_tau)
     tau_sq = sig_tau * tau_sq_sc;
 }
 
 model {
  
  vector[N] logit_p;
  vector[N] p;
   
  // Polling Model
    // Hyper-priors
      // bias - constant
      mu_alpha ~ normal(0,0.2);
      sig_alpha ~ normal(0,0.2) T[0,]; // half normal due to bounds

      // bias - time-varying
      mu_beta ~ normal(0,0.2);
      sig_beta ~ normal(0,0.2) T[0,]; // half normal due to bounds

      // non-sampling error
      sig_tau ~ normal(0,0.05) T[0,]; // half normal due to bounds
      
    // Priors
      // bias - constant
      //alpha ~ normal(mu_alpha,sig_alpha);
      alpha_sc ~ normal(0,1);
      
      // bias - time-varying
      //beta ~ normal(mu_beta,sig_beta);
      beta_sc ~ normal(0,1);
      
      // non-sampling error
      //tau ~ normal(0,sig_tau);
      for(i in 1:S){
        tau_sq_sc[i] ~ normal(0,1) T[0,];
      }
      
  // Model for Polling Data
    for(i in 1:N){
      logit_p[i] = logit_vote[state_year_id[i]] + alpha[state_year_id[i]] +  beta[state_year_id[i]] * t[i];
       
      p[i] = inv_logit(logit_p[i]);
       
      poll[i] ~ normal(p[i], sqrt(p[i] * (1 - p[i]) / sample_size[i] + tau_sq[state_year_id[i]]));
   }
    
}
