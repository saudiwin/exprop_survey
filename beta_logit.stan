//
// Ordinal beta regression model for analying experimental outcomes
// with proportion and degenerate responses (i.e. 0 and 1)
// Models 0/1 as ordered categories above/below (0,1) 
// Robert Kubinec
// New York University Abu Dhabi
data {
  int<lower=0> N_prop; // number of proportion observations (0,1)
  int<lower=0> N_degen; // number of 0/1 observations
  int X; // number predictors
  vector[N_prop] outcome_prop; // Y in (0,1)
  int outcome_degen[N_degen]; // Y in {0,1}
  matrix[N_prop,X] covar_prop; // covariate X for proportion outcome
  matrix[N_degen,X] covar_degen; // covariate X for degenerate (0,1) outcome
}
parameters {
  real alpha1; // intercept for ordered model
  real alpha2; // intercept for beta regression
  vector[X] X_beta; // common predictor
  ordered[2] cutpoints; // cutpoints on ordered (latent) variable
  real<lower=0> kappa; // scale parameter for beta regression
}
model {
  
  // vague priors
  alpha1 ~ normal(0,5);
  alpha2 ~ normal(0,5);
  X_beta ~ normal(0,5);
  kappa ~ exponential(1);
  cutpoints[2] - cutpoints[1] ~ normal(0,3);
  
  // need separate counters for logit (0/1) and beta regression
  
  for(n in 1:N_degen) {
    if(outcome_degen[n]==0) {
      // Pr(Y==0)
      target += log(1 - inv_logit(alpha1 + covar_degen[n,]*X_beta - cutpoints[1]));
    } else {
      //Pr(Y==1)
      target += log(inv_logit(alpha1 + covar_degen[n,]*X_beta - cutpoints[2]));
    }
  }
  
  for(n in 1:N_prop) {
    // Pr(Y in (0,1))
    target += log(inv_logit(alpha1 + covar_prop[n,]*X_beta - cutpoints[1]) - inv_logit(alpha1 + covar_prop[n,]*X_beta - cutpoints[2]));
    // Pr(Y==x where x in (0,1))
    target += beta_proportion_lpdf(outcome_prop[n]|inv_logit(alpha2 + covar_prop[n,]*X_beta),kappa);
  }
  
}

generated quantities {
  
  int regen_degen[N_degen+N_prop]; // determines which model we get
  vector[N_prop+N_degen] regen_all; // final (combined) outcome
  vector[3] theta; // different options for distribution
  
  // first do degenerate outcomes 
  // note: these could be *re-generated* as beta/propotions
  for(n in 1:N_degen) {
    theta[1] = 1 - inv_logit(alpha1 + covar_degen[n,]*X_beta - cutpoints[1]);
    theta[2] = inv_logit(alpha1 + covar_degen[n,]*X_beta - cutpoints[1]) - inv_logit(alpha1 + covar_degen[n,]*X_beta - cutpoints[2]);
    theta[3] = inv_logit(alpha1 + covar_degen[n,]*X_beta - cutpoints[2]);
    
    // draw an outcome
    //theta is a simplex through manual logit scaling
    
    regen_degen[n] = categorical_rng(theta);
    
    if(regen_degen[n]==1) {
      regen_all[n] = 0;
    } else if(regen_degen[n]==3) {
      regen_all[n] = 1;
    } else {
      // did not occur in original data but could re-occur probabilistically
      regen_all[n] = beta_proportion_rng(inv_logit(alpha2 + covar_degen[n,]*X_beta),kappa);
    }
    
  }
  
  // now do originally proportional outcomes
  // can be re-generated as 0s or 1s
  
  for(n in 1:N_prop) {
    theta[1] = 1 - inv_logit(alpha1 + covar_prop[n,]*X_beta - cutpoints[1]);
    theta[2] = inv_logit(alpha1 + covar_prop[n,]*X_beta - cutpoints[1]) - inv_logit(alpha1 + covar_prop[n,]*X_beta - cutpoints[2]);
    theta[3] = inv_logit(alpha1 + covar_prop[n,]*X_beta - cutpoints[2]);
    
    // draw an outcome
    //theta is a simplex through manual logit scaling
    
    regen_degen[n+N_degen] = categorical_rng(theta);
    
    if(regen_degen[n+N_degen]==1) {
      regen_all[n+N_degen] = 0;
    } else if(regen_degen[n+N_degen]==3) {
      regen_all[n+N_degen] = 1;
    } else {
      // did not occur in original data but could re-occur probabilistically
      regen_all[n+N_degen] = beta_proportion_rng(inv_logit(alpha2 + covar_prop[n,]*X_beta),kappa);
    }
    
  }
   
  
}

