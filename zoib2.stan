data {
  int n;
  vector[n] x; 
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
  vector[2] coef_a;
  vector[2] coef_g;
  vector[2] coef_m;
  vector[2] coef_p;
}
transformed parameters {
  vector<lower=0, upper=1>[n] alpha;
  vector<lower=0, upper=1>[n] gamma;
  vector[n] mu;
  vector<lower=0>[n] phi;
  vector<lower=0>[n] p;
  vector<lower=0>[n] q;

  for (i in 1:n) {
    alpha[i] = inv_logit(coef_a[1] + coef_m[2] * x[i]);
    gamma[i] = inv_logit(coef_g[1] + coef_m[2] * x[i]);
    mu[i] = inv_logit(coef_m[1] + coef_m[2] * x[i]);
    phi[i] = exp(coef_p[1] + coef_p[2] * x[i]);
    p[i] = mu[i] * phi[i];
    q[i] = phi[i] - mu[i] * phi[i];
  }
}
model {
  coef_a ~ normal(0, 1);
  coef_g ~ normal(0, 1);
  coef_m ~ normal(0, 1);
  coef_p ~ normal(0, 1);
  is_discrete ~ bernoulli(alpha); 
  for (i in 1:n) {
    if (is_discrete[i] == 1) {
      y_discrete[i] ~ bernoulli(gamma[i]);
    } else {
      y[i] ~ beta(p[i], q[i]);
    }
  }
}