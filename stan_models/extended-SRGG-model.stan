 data {
  
   // Data ordered so that all obs with full information are first, followed by those with missing undecideds
  
   int<lower=1> N;                          //  Number of obs total
   int<lower=1> N_u;                        //  Number of obs with undecided info... N_u <= N
   int<lower=1> S;                          //  Number of states (+ DC)
   int<lower=1> Y;                          //  Number of years
   int<lower=1> RMY;                        //  Number of margin-year categories
   int<lower=1> H;                        //  Number of polling houses
   
   real<lower=0, upper=1> poll[N];          //  individual polls, Rep/(Rep + Dem)
   vector<lower=0, upper=1>[N_u] und;                         //  undecided from each poll
   
   real<lower=0, upper=1> vote[S];          //  state vote outcome for each poll
   
   vector<lower=1>[N] sample_size;          // poll sample size
   
   vector<lower=0, upper=1>[N] t;           // days to election, standardised between 1 and 0
   
   int<lower=1, upper=S> state_year_id[N];  //  identifies state and year
   
   int<lower=1, upper=Y> year_id[N];        //  identifies year
   
   int<lower=1, upper=RMY> rmargin_year_id[N];        //  identifies margin-year categories
   
   int<lower=1, upper=H> house_id[N];        //  identifies house categories (polling agent)
   
    int<lower=1, upper=Y> sy_to_y_id[S];        // identifies year from state_year_id
   
 } 
 
transformed data {
  vector[S] logit_vote;
  for (i in 1:S){
    logit_vote[i] = logit(vote[i]);
  }
}
 
 parameters {

  // Undecided Model   
    // Hyper-parameters
      // Undecided model - state mean
      //real mu_alpha_und;
      real<lower=0> sig_alpha_und;
      
      // Undecided model - state time-varying
      real mu_beta_und;
      real<lower=0> sig_beta_und;
      
      // Undecided model - state sd
      real<lower=0> sig_tau_und;
    
    // Scaled Parameters
      vector[S] alpha_und_sc;
      vector[S] beta_und_sc;
      vector<lower=0>[S] tau_sq_und_sc;
      
      // Undecided model - year mean
      vector[Y] phi_und_sc;

    // Parameters
    
      // Undecided model - year mean
      //vector[Y] phi_und;
      
      // Undecided model - year sd
      //vector<lower=0>[Y] eta_sq_und;
      
  // Polling Model
    // Hyper-parameters
      // bias - constant
      real mu_alpha;
      real<lower=0> sig_alpha;
      
      // bias - time-varying
      real mu_beta;
      real<lower=0> sig_beta;
      
      // bias - house
      real mu_kappa;
      real<lower=0> sig_kappa;
      
      // non-sampling error
      real<lower=0> sig_tau;
    
    // Scaled Parameters
      vector[S] alpha_sc;
      vector[S]  beta_sc;
      vector[H-1]  kappa_sc; // 0_None has no group
      vector<lower=0>[S] tau_sq_sc;
      
    vector[RMY] gamm;                            // bias in allocation of undecideds
    
 } 
 
 transformed parameters {
   
  // Undecided Model 
   // Parameters
     vector[S] alpha_und;
     vector[S] beta_und;
     vector<lower=0>[S] tau_sq_und;
     
     // Undecided model - year mean
      vector[Y] phi_und;
     
  // Polling Model
   // Parameters
     vector[S] alpha;                           // bias in each state
     vector[S] beta;
     vector[H] kappa;
     vector[H-1] kappa0;
     vector<lower=0>[S] tau_sq; 
     
  // Undecided Model
   // Parameters - Definitions (due to scaling)
   
    // Undecided model - year mean
     phi_und = 0.04 + 0.01 * phi_und_sc;
   
     // alpha_und ~ normal(mu_alpha_und,sig_alpha_und)
     for(i in 1:S){
       alpha_und[i] = phi_und[sy_to_y_id[i]] + sig_alpha_und * alpha_und_sc[i];
     }
     
     // beta_und ~ normal(mu_beta_und,sig_beta_und)
     beta_und = mu_beta_und + sig_beta_und * beta_und_sc;
     
     // tau_und ~ normal(0,sig_tau_und)
     tau_sq_und = sig_tau_und * tau_sq_und_sc;
    
     
  // Polling Model
    // Parameters - Definitions (due to scaling)
     // alpha ~ normal(mu_alpha,sig_alpha)
     alpha = mu_alpha + sig_alpha * alpha_sc;
     
     // beta ~ normal(mu_beta,sig_beta)
     beta = mu_beta + sig_beta * beta_sc;
     
    // kappa ~ normal(mu_kappa,sig_kappa)
     kappa0 = mu_kappa + sig_kappa * kappa_sc;
     kappa[1] = 0;
     kappa[2:H] = kappa0;
     
     // tau ~ normal(0,sig_tau)
     tau_sq = sig_tau * tau_sq_sc;
 }
 
 model {
  
  vector[N] logit_p;
  vector[N] p;
  
  // Undecided Model
    // Hyper-priors
    
      // Undecided model - state mean
      //mu_alpha_und ~ normal(0,0.02);
      //mu_alpha_und ~ beta(1.5,28.5);
      sig_alpha_und ~ normal(0,0.02) T[0,]; // half normal due to bounds
      
      // Undecided model - state time-varying
      mu_beta_und ~ normal(0,0.02);
      sig_beta_und ~ normal(0,0.02) T[0,]; // half normal due to bounds
    
      // Undecided model - state sd
      sig_tau_und ~ normal(0,0.01) T[0,]; // half normal due to bounds
    
     // Priors
     
      // Scaled parameter priors
      //alpha ~ normal(mu_alpha_und,sig_alpha_und);
      alpha_und_sc ~ normal(0,1);
      //beta_und ~ normal(mu_beta_und,sig_beta_und);
      beta_und_sc ~ normal(0,1);
      //tau_und ~ normal(0,sig_tau_und);
      for(j in 1:S){
        tau_sq_und_sc[j] ~ normal(0,1) T[0,];
      }
      
      // Year mean
      phi_und_sc ~ normal(0,1); // 0.04 mean, 0.01 sd added in transformation
    
      // Year sd
      // for(y in 1:Y){
      //   eta_sq_und[y] ~ normal(0,0.01) T[0,];
      // }
   
   // Model for undecided data
   for(i in 1:N_u){
     und[i] ~ normal(alpha_und[state_year_id[i]] + beta_und[state_year_id[i]] * t[i],
     sqrt(tau_sq_und[state_year_id[i]])
     );
   } 
   
  // Polling Model
    // Hyper-priors
      // bias - constant
      mu_alpha ~ normal(0,0.2);
      sig_alpha ~ normal(0,0.2) T[0,]; // half normal due to bounds
      //sig_alpha ~ exponential(1/0.02);
      
      // bias - time-varying
      mu_beta ~ normal(0,0.2);
      sig_beta ~ normal(0,0.2) T[0,]; // half normal due to bounds
      //sig_beta ~ exponential(1/0.02);
      
      // bias - house
      mu_kappa ~ normal(0,0.2);
      //sig_kappa ~ normal(0,0.2) T[0,]; // half normal due to bounds
      sig_kappa ~ exponential(1/0.05); 

      
      // shrinkage prior on gamm
      gamm ~ double_exponential(0, 0.05); // 
      //gamm ~ normal(0, 0.2);
    
      // non-sampling error
      sig_tau ~ normal(0,0.05) T[0,]; // half normal due to bounds
      
    // Priors
      // bias - constant
      //alpha ~ normal(mu_alpha,sig_alpha);
      alpha_sc ~ normal(0,1);
      
      // bias - time-varying
      //beta ~ normal(mu_beta,sig_beta);
      beta_sc ~ normal(0,1);
      
      // bias - house
      //kappa ~ normal(mu_kappa,sig_kappa);
      kappa_sc ~ normal(0,1);
      
      // non-sampling error
      //tau ~ normal(0,sig_tau);
      for(i in 1:S){
        tau_sq_sc[i] ~ normal(0,1) T[0,];
      }
      
  // Model for Polling Data
     for(i in 1:N){
       logit_p[i] = logit_vote[state_year_id[i]] + 
       alpha[state_year_id[i]] + 
       beta[state_year_id[i]] * t[i] +
       kappa[house_id[i]] - 
       gamm[rmargin_year_id[i]] * (10 * alpha_und[state_year_id[i]]);
       p[i] = inv_logit(logit_p[i]);
     poll[i] ~ 
      normal(p[i],
      sqrt(p[i] * (1 - p[i]) / sample_size[i] + tau_sq[state_year_id[i]]));
   }
    
}

// generated quantities {
//   vector[N] poll_bias;
//   vector[N] poll_est;
//   vector[N] poll_sd;
//   
//   for(i in 1:N){
//     poll_bias[i] = 
//         alpha[state_year_id[i]] + beta[state_year_id[i]] * t[i] 
//              - gamm[rmargin_year_id[i]] * (phi_und[year_id[i]] + alpha_und[state_year_id[i]]) + kappa[house_id[i]];
//     
//     poll_est[i] = vote[state_year_id[i]] + poll_bias[i];
//     
//     poll_sd[i] =     
//         sqrt(vote[state_year_id[i]] * (1 - vote[state_year_id[i]]) / sample_size[i]) + 
//         tau[state_year_id[i]];
//    }
//   
// }
