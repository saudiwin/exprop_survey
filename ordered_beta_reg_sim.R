# Robert Kubinec
# New York University Abu Dhabi
# January 23, 2020
# let's try to simulate a new distribution for ordered beta regression

require(rstan)
require(bayesplot)
require(dplyr)

N <- 1000

X <- runif(N,-2,2)

# on the logit scale, determines threshold above which we get degenerate (0/1) outcomes
cutpoints <- c(-2,2) 

X_beta <- 2.5
eta <- X*X_beta

# ancillary parameter of beta distribution
kappa <- 2

# predictor for ordered model
mu1 <- eta
# predictor for beta regression
mu2 <- eta

# probabilities for three possible categories (0, proportion, 1)
low <- 1-plogis(mu2 - cutpoints[1])
middle <- plogis(mu2-cutpoints[1]) - plogis(mu2-cutpoints[2])
high <- plogis(mu2 - cutpoints[2])

# we'll assume the same eta was used to generate outcomes

out_beta <- rbeta(N,plogis(mu1) * kappa, (1 - plogis(mu1)) * kappa) 

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
              N_pred_degen=sum(final_out %in% c(0,1)),
              N_pred_prop=sum(final_out>0 & final_out<1),
              indices_degen=1:(sum(final_out %in% c(0,1))),
              indices_prop=1:(sum(final_out>0 & final_out<1)),
              run_gen=1)

fit_model <- sampling(beta_logit,data=to_bl,chains=2,cores=2,iter=1000)

# regenerate data and see how we do capturing it

yrep_ordbeta <- extract(fit_model,"regen_all")[[1]]

# use only a sample of draws

# final_out_prop <- final_out[final_out>0 & final_out<1][indices_prop]
# final_out_degen <- final_out[final_out %in% c(0,1)][indices_degen]

final_out_prop <- final_out[final_out>0 & final_out<1]
final_out_degen <- final_out[final_out %in% c(0,1)]

ppc_ecdf_overlay(y=c(final_out_degen,final_out_prop),yrep=yrep_ordbeta) + 
  ggtitle("Empirical Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

# ppc_dens_overlay(y=c(final_out_degen,final_out_prop),yrep=yrep) + 
#   ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

# looks pretty good

# try with rstanarm betareg where we pre-transform outcome

require(rstanarm)

final_out_scale <- (final_out * (length(final_out) - 1) + .5) / length(final_out)

betareg_fit <- stan_betareg(formula = outcome~X,data=tibble(outcome=final_out_scale,
                                                            X=X),chains=2,cores=2,iter=1000)

summary(betareg_fit)

# X_beta has lost 1/3 its value from original (i.e., effect has been compressed)

# see the distortion re: the original values

yrep_betareg <- posterior_predict(betareg_fit,draws=100)

ppc_dens_overlay(y=final_out,yrep=yrep_betareg) + ggtitle("Posterior Predictive Distribution for Rstanarm Beta Regression",subtitle="N=1000, Transformed to (0,1) Scale")

ppc_dens_overlay(y=final_out_scale,yrep=yrep_betareg) + ggtitle("Posterior Predictive Distribution for Rstanarm Beta Regression",subtitle="N=1000, Transformed to (0,1) Scale")

# Tends to over/under predict middle and outliers (i.e. 0/1)

# now try the ZOIB

zoib_model <- stan_model("zoib.stan")
x <- as.matrix(X)
zoib_fit <- sampling(zoib_model,data=list(n=length(final_out),
                                          y=final_out,
                                          k=ncol(x),
                                          x=x,
                                          run_gen=1),chains=2,cores=2,iter=1000)

# this is X_beta
print(zoib_fit,pars="coef_m")
yrep_zoib <- extract(zoib_fit,"zoib_regen")[[1]]
ppc_dens_overlay(y=final_out,yrep=yrep_zoib) + 
  ggtitle("Empirical Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

# this one is quite close to what I had, slightly under
# inflated coefs are off so hard to interpret as the same
# i.e. no 1 effect of X

# what if we try to force one effect of X?
# modified stan code with 1 effect for X

# zoib_model2 <- stan_model("zoib2.stan")
# 
# zoib_fit2 <- sampling(zoib_model2,data=list(n=length(final_out),
#                                           y=final_out,
#                                           x=X),chains=2,cores=2,iter=1000)
# 
# # this is X_beta
# print(zoib_fit2,pars="coef_m")
# 
# # once we have a constrained coef_m, also downgrades effect of coef_m (not clear why)
# # intercept is still correct too
# 
# print(zoib_fit2,pars="coef_g")
# print(zoib_fit2,pars="coef_a")

# how well does the zoib fit

# zoib_mydata <- sampling(zoib_model,data=list(y=c(combine_prop$outcome_norm,combine_degen$outcome),
#                                              x=),cores=2,chains=2,iter=1000,init=0)


# Compare Distributions ---------------------------------------------------

require(loo)

loo_ordbeta <- loo(fit_model,"ord_log")
loo_betareg <- loo(betareg_fit)

loo_zoib <- loo(zoib_fit,"zoib_log")

loo_compare(loo_ordbeta,loo_zoib)

# let's do some simulations

N_rep <- 2000

simul_data <- tibble(N=round(runif(N_rep,100,2000),0),
                     X_beta=runif(N_rep,-2,2),
                     kappa=runif(N_rep,0.5,4),
                     cutpoints1=runif(N_rep,-5,-1)) %>% 
              mutate(cutpoints2=cutpoints1+runif(N_rep,0.5,5))

all_simul_data <- parallel::mclapply(1:10, function(i) {
  this_data <- slice(simul_data,i)
  cat(file = "simul_status.txt",paste0("Now on row ",i),append = T)
  
  N <- this_data$N
  
  X <- as.numeric(runif(N,0,1)>.5)
  
  X_beta <- this_data$X_beta
  eta <- X*X_beta
  
  # ancillary parameter of beta distribution
  kappa <- this_data$kappa
  
  # predictor for ordered model
  mu1 <- eta
  # predictor for beta regression
  mu2 <- eta
  
  cutpoints <- c(this_data$cutpoints1,this_data$cutpoints2)
  
  # probabilities for three possible categories (0, proportion, 1)
  low <- 1-plogis(mu2 - cutpoints[1])
  middle <- plogis(mu2-cutpoints[1]) - plogis(mu2-cutpoints[2])
  high <- plogis(mu2 - cutpoints[2])
  
  # we'll assume the same eta was used to generate outcomes
  
  out_beta <- rbeta(N,plogis(mu1) * kappa, (1 - plogis(mu1)) * kappa) 
  
  print(i)
  
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
  
  # now fit ordinal beta
  
  to_bl <- list(N_degen=sum(final_out %in% c(0,1)),
                N_prop=sum(final_out>0 & final_out<1),
                X=1,
                outcome_prop=final_out[final_out>0 & final_out<1],
                outcome_degen=final_out[final_out %in% c(0,1)],
                covar_prop=as.matrix(X[final_out>0 & final_out<1]),
                covar_degen=as.matrix(X[final_out %in% c(0,1)]),
                N_pred_degen=sum(final_out %in% c(0,1)),
                N_pred_prop=sum(final_out>0 & final_out<1),
                indices_degen=1:(sum(final_out %in% c(0,1))),
                indices_prop=1:(sum(final_out>0 & final_out<1)),
                run_gen=1)
  
  fit_model <- sampling(beta_logit,data=to_bl,chains=1,cores=1,iter=1000,pars=c("regen_all","X_beta","ord_log"))
  
  
  x <- as.matrix(X)
  zoib_fit <- sampling(zoib_model,data=list(n=length(final_out),
                                            y=final_out,
                                            k=ncol(x),
                                            x=x,
                                            run_gen=1),chains=1,cores=1,iter=1000,pars=c("coef_m","zoib_regen","zoib_log"))
  
  final_out_scale <- (final_out * (length(final_out) - 1) + .5) / length(final_out)
  
  betareg_fit <- stan_betareg(formula = outcome~X,data=tibble(outcome=final_out_scale,
                                                              X=X),chains=1,cores=1,iter=1000)
  
  yrep_betareg <- posterior_predict(betareg_fit,draws=100)
  
  # do a second one with just non0/non1 data
  
  betareg_fit2 <- stan_betareg(formula = outcome~X,data=tibble(outcome=final_out[final_out>0 & final_out<1],
                                                               X=X[final_out>0 & final_out<1]),chains=1,cores=1,iter=1000)
  
  yrep_betareg2 <- posterior_predict(betareg_fit2,draws=100)
  
  # now return the full data frame
  
  X_beta_ord <- as.matrix(fit_model,"X_beta")
  X_beta_zoib <- as.matrix(zoib_fit,"coef_m")
  X_beta_reg <- as.matrix(betareg_fit,pars="X")
  X_beta_reg2 <- as.matrix(betareg_fit2,pars="X")
  
  # calculate rmse
  
  rmse_ord <- sqrt(mean(apply(as.matrix(fit_model,"regen_all"),1,function(c) { (c - c(final_out[final_out %in% c(0,1)],final_out[final_out>0 & final_out<1]))^2 })))
  rmse_zoib <-sqrt( mean(apply(as.matrix(zoib_fit,"zoib_regen"),1,function(c) { (c - final_out)^2 })))
  rmse_betareg <- sqrt(mean(apply(yrep_betareg,1,function(c) { (c - final_out)^2 })))
  rmse_betareg2 <- sqrt(mean(apply(yrep_betareg2,1,function(c) { (c - final_out[final_out>0 & final_out<1])^2 })))
  
  # calculate loo
  
  loo_ordbeta <- loo(fit_model,"ord_log")
  loo_betareg <- loo(betareg_fit)
  loo_betareg2 <- loo(betareg_fit2)
  loo_zoib <- loo(zoib_fit,"zoib_log")
  
  comp_loo <- loo_compare(loo_ordbeta,loo_zoib)
  
  win_loo <- row.names(comp_loo)[1]=="model1"
  
  bind_cols(purrr::map_dfr(seq_len(4), ~this_data),bind_rows(tibble(model="Ordinal Beta Regression",
                   med_est=median(X_beta_ord),
                   high=quantile(X_beta_ord,.95),
                   low=quantile(X_beta_ord,.05),
                   sd=sd(X_beta_ord),
                   loo_val=loo_ordbeta$estimates[1,1],
                   win_loo=win_loo,
                   win_loo_se=comp_loo[2,2],
                   rmse=rmse_ord),
            tibble(model="ZOIB",
                   med_est=median(X_beta_zoib),
                   high=quantile(X_beta_zoib,.95),
                   low=quantile(X_beta_zoib,.05),
                   sd=sd(X_beta_zoib),
                   loo_val=loo_zoib$estimates[1,1],
                   win_loo=win_loo,
                   win_loo_se=comp_loo[2,2],
                   rmse=rmse_zoib),
            tibble(model="Beta Regression - Transformed",
                   med_est=median(X_beta_reg),
                   high=quantile(X_beta_reg,.95),
                   low=quantile(X_beta_reg,.05),
                   sd=sd(X_beta_reg),
                   loo_val=loo_betareg$estimates[1,1],
                   rmse=rmse_betareg),
            tibble(model="Beta Regression - (0,1)",
                   med_est=median(X_beta_reg2),
                   high=quantile(X_beta_reg2,.95),
                   low=quantile(X_beta_reg2,.05),
                   sd=sd(X_beta_reg2),
                   loo_val=loo_betareg2$estimates[1,1],
                   rmse=rmse_betareg2)))
  
  
  
},mc.cores=8)

simul_data_final <- bind_rows(all_simul_data)
