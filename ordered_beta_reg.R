# Robert Kubinec
# New York University Abu Dhabi
# January 23, 2020
# let's try to simulate a new distribution for ordered beta regression

require(rstan)
require(bayesplot)

N <- 1000

# on the logit scale, determines threshold above which we get degenerate (0/1) outcomes
cutpoints <- c(-2,2) 

X <- runif(N,min=-1,max=1)
X_beta <- 2.5
eta <- X*X_beta

# ancillary parameter of beta distribution
kappa <- 2
# intercept for ordered model
alpha1 <- -0.3 
# intercept for beta regression
alpha2 <- 0.2

# predictor for ordered model
mu1 <- alpha1 + eta
# predictor for beta regression
mu2 <- alpha2 + eta

# probabilities for three possible categories (0, proportion, 1)
low <- 1-plogis(mu2 - cutpoints[1])
middle <- plogis(mu2-cutpoints[1]) - plogis(mu2-cutpoints[2])
high <- plogis(mu2 - cutpoints[2])

# we'll assume the same eta was used to generate outcomes

out_beta <- rbeta(N,plogis(mu1) * phi, (1 - plogis(mu1)) * phi) 

# now determine which one we get for each observation
outcomes <- sapply(1:N, function(i) {
  sample(1:3,size=1,prob=c(low[i],middle[i],high[i]))
})

# now combine binary (0/1) with proportion (beta)

final_out <- sapply(1:length(outcomes),function(i) {
  if(outcomes[i]==1) {
    return(0)
  } else if(outcomes[i]==2) {
    return(out_beta[i])
  } else {
    return(1)
  }
})

# now we need a Stan file

beta_logit <- stan_model("beta_logit.stan")

to_bl <- list(N_degen=sum(final_out %in% c(0,1)),
              N_prop=sum(final_out>0 & final_out<1),
              X=1,
              outcome_prop=final_out[final_out>0 & final_out<1],
              outcome_degen=final_out[final_out %in% c(0,1)],
              covar_prop=as.matrix(X[final_out>0 & final_out<1]),
              covar_degen=as.matrix(X[final_out %in% c(0,1)]))

fit_model <- sampling(beta_logit,data=to_bl,chains=2,cores=2,iter=1000)

# regenerate data and see how we do capturing it

yrep <- extract(fit_model,"regen_all")[[1]]

# use only a sample of draws

yrep <- yrep[sample(1:nrow(yrep),size=100),]

ppc_dens_overlay(y=final_out,yrep=yrep) + ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

# looks pretty good

# try with Stan betareg where we pre-transform outcome

final_out_scale <- (final_out * (length(final_out) - 1) + .5) / length(final_out)

