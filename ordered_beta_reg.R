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
alpha1 <- 0 
# intercept for beta regression
alpha2 <- 0

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

to_sample <- 400

indices_degen <- sample(1:length(X[final_out %in% c(0,1)]),size=to_sample/2)
indices_prop <- sample(1:length(X[final_out>0 & final_out<1]),size=to_sample/2)

to_bl <- list(N_degen=sum(final_out %in% c(0,1)),
              N_prop=sum(final_out>0 & final_out<1),
              X=1,
              outcome_prop=final_out[final_out>0 & final_out<1],
              outcome_degen=final_out[final_out %in% c(0,1)],
              covar_prop=as.matrix(X[final_out>0 & final_out<1]),
              covar_degen=as.matrix(X[final_out %in% c(0,1)]),
              N_pred_degen=to_sample/2,
              N_pred_prop=to_sample/2,
              indices_degen=indices_degen,
              indices_prop=indices_prop)

fit_model <- sampling(beta_logit,data=to_bl,chains=2,cores=2,iter=1000)

# regenerate data and see how we do capturing it

yrep <- extract(fit_model,"regen_all")[[1]]

# use only a sample of draws

#yrep <- yrep[sample(1:nrow(yrep),size=100),]

final_out_prop <- final_out[final_out>0 & final_out<1][indices_prop]
final_out_degen <- final_out[final_out %in% c(0,1)][indices_degen]

ppc_ecdf_overlay(y=c(final_out_degen,final_out_prop),yrep=yrep) + 
  ggtitle("Empirical Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

ppc_dens_overlay(y=c(final_out_degen,final_out_prop),yrep=yrep) + 
  ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

# looks pretty good

# try with rstanarm betareg where we pre-transform outcome

require(rstanarm)

final_out_scale <- (final_out * (length(final_out) - 1) + .5) / length(final_out)

betareg_fit <- stan_betareg(formula = outcome~X,data=tibble(outcome=final_out_scale,
                                                            X=X),chains=2,cores=2,iter=1000)

summary(betareg_fit)

# X_beta has lost 1/3 its value from original (i.e., effect has been compressed)

# see the distortion re: the original values

betareg_post <- posterior_predict(betareg_fit,draws=100)

ppc_dens_overlay(y=final_out,yrep=betareg_post) + ggtitle("Posterior Predictive Distribution for Rstanarm Beta Regression",subtitle="N=1000, Transformed to (0,1) Scale")

# Tends to over/under predict middle and outliers (i.e. 0/1)

# now try the ZOIB

zoib_model <- stan_model("zoib.stan")

zoib_fit <- sampling(zoib_model,data=list(n=length(final_out),
                                          y=final_out,
                                          x=X),chains=2,cores=2,iter=1000)

# this is X_beta
print(zoib_fit,pars="coef_m")
# this one is quite close to what I had, slightly under
# inflated coefs are off so hard to interpret as the same
# i.e. no 1 effect of X

# what if we try to force one effect of X?
# modified stan code with 1 effect for X

zoib_model2 <- stan_model("zoib2.stan")

zoib_fit2 <- sampling(zoib_model2,data=list(n=length(final_out),
                                          y=final_out,
                                          x=X),chains=2,cores=2,iter=1000)

# this is X_beta
print(zoib_fit2,pars="coef_m")

# once we have a constrained coef_m, also downgrades effect of coef_m (not clear why)
# intercept is still correct too

print(zoib_fit2,pars="coef_g")
print(zoib_fit2,pars="coef_a")

# how well does the zoib fit

# zoib_mydata <- sampling(zoib_model,data=list(y=c(combine_prop$outcome_norm,combine_degen$outcome),
#                                              x=),cores=2,chains=2,iter=1000,init=0)

