data {
  int n;
  int k; // number of columns
  matrix[n,k] x; 
  vector<lower=0, upper=1>[n] y;
}
transformed data {
  int<lower=0, upper=1> is_discrete[n];
  int<lower=-1, upper=1> y_discrete[n];

  // create indicator for whether y is discrete 
  // and an integer value to pass to bernoulli_lpmf for discrete y
  for (i in 1:n) {
    if (y[i] == 0) {
      is_discrete[i] = 1;
      y_discrete[i] = 0;
    } else if (y[i] == 1) {
      is_discrete[i] = 1;
      y_discrete[i] = 1;
    } else {
      is_discrete[i] = 0;
      // hack to ensure that throws error if passed to bernoulli_lpmf
      y_discrete[i] = -1;
    }
  }
}
parameters {
  vector[k] coef_a;
  vector[k] coef_g;
  vector[k] coef_m;
  vector[k] coef_p;
  vector[4] alpha;
}
transformed parameters {
  vector<lower=0, upper=1>[n] psi;
  vector<lower=0, upper=1>[n] gamma;
  vector[n] mu;
  vector<lower=0>[n] phi;
  vector<lower=0>[n] p;
  vector<lower=0>[n] q;

  for (i in 1:n) {
    psi[i] = inv_logit(alpha[1] + x[i,]*coef_a);
    gamma[i] = inv_logit(alpha[2] + x[i,]*coef_g);
    mu[i] = inv_logit(alpha[3] + x[i,]*coef_m);
    phi[i] = exp(alpha[4] + x[i,]*coef_p);
    p[i] = mu[i] * phi[i];
    q[i] = phi[i] - mu[i] * phi[i];
  }
}
model {
  coef_a ~ normal(0, 1);
  coef_g ~ normal(0, 1);
  coef_m ~ normal(0, 1);
  coef_p ~ normal(0, 1);
  alpha ~ normal(0,1);
  
  // is_discrete ~ bernoulli(psi); 
  // for (i in 1:n) {
  //   if (is_discrete[i] == 1) {
  //     y_discrete[i] ~ bernoulli(gamma[i]);
  //   } else {
  //     y[i] ~ beta(p[i], q[i]);
  //   }
  // }
  
  for (i in 1:n) {
    if (y[i] == 0) {
      target += log(psi[i]) + log1m(gamma[i]);
    } else if (y[i] == 1) {
      target += log(psi[i]) + log(gamma[i]);
    } else {
      target += log1m(psi[i]) + beta_lpdf(y[i] | p[i], q[i]);
    }
  }
}
generated quantities {
  vector[n] zoib_log;
  vector[n] zoib_regen;
  vector[n] is_discrete_regen;
  
  for (i in 1:n) {
    if (y[i] == 0) {
      zoib_log[i] = log(psi[i]) + log1m(gamma[i]);
    } else if (y[i] == 1) {
      zoib_log[i] = log(psi[i]) + log(gamma[i]);
    } else {
      zoib_log[i] = log1m(psi[i]) + beta_lpdf(y[i] | p[i], q[i]);
    }
  }
  
  for(i in 1:n) {
    
    is_discrete_regen[i] = bernoulli_rng(psi[i]);
    
    if(is_discrete_regen[i]==0) {
      zoib_regen[i] = beta_rng(p[i], q[i]);
    } else {
      zoib_regen[i] = bernoulli_rng(gamma[i]);
    }
    
  }
  
  
  
  zoib_log += bernoulli_lpmf(is_discrete|psi);
}
