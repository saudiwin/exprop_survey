# Do Power Calculation for Expropriation Survey
# Robert Kubinec
# August 5th, 2019


require(dplyr)
require(ggplot2)
require(tidyr)
require(purrr)

# we can use a funciton that allows us to parameterize the 
# Beta distribution in terms of mean and variance
# See this answer from stack overflow:
# https://stats.stackexchange.com/a/12239

estBetaParams <- function(mu, var) {
alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
beta <- alpha * (1 / mu - 1)
return(params = list(alpha = alpha, beta = beta))
}

check1 <- rbeta(1000,
                estBetaParams(0.2,0.1)$alpha,
                estBetaParams(0.2,0.1)$beta)

summary(check1)
var(check1)

# works correctly
# now need to parameterize the beta regression
# equals marginal probability increase of roughly 10%
true_eff <- .5

# intercept = make outcome somewhat rare
int <- -1

# calculate alpha/beta parameters given this mean and variance of 0.1

beta_par1 <- estBetaParams(mu=plogis(true_eff+int),
                          0.1)
beta_par2 <- estBetaParams(mu=plogis(int),
                           0.1)
treat <- .5>runif(1000)
out_treat <- rbeta(n=1000,beta_par1$alpha,
                   beta_par1$beta)
out_control <- rbeta(n=1000,beta_par2$alpha,
                     beta_par2$beta)
outcome <- ifelse(treat,out_treat,
                  out_control)

# make sure we can recover: test R package betareg

require(betareg)

beta_fit <- betareg(outcome ~ treat)
summary(beta_fit)

# pretty close if not perfect, as we should expect

# vary sample size

sample_size <- seq(10,3000,by=50)

over_samples <- lapply(sample_size,function(s) {
  subj <- 1:s
  
  treat <- as.numeric(.5>runif(s))
  
  print(paste0("Now on sample size ",s))
  
  these_pars <- lapply(subj,function(this_subj) {
    mu <- plogis(int + true_eff*treat[this_subj])
    # 1/2 maximum dispersion conditional on mu
    to_beta <- estBetaParams(mu=mu,
                               var = (mu*(1-mu))/2)
    # draw s samples
    out_beta <- rbeta(n=1000,to_beta$alpha,
                        to_beta$beta)
    # return data frame
  out_d <- tibble(size=s,
             alpha=to_beta$alpha,
             beta=to_beta$beta,
             treat=treat[this_subj],
             outcome=out_beta,
             subj=this_subj,
             iter=1:1000)
  }) %>% bind_rows
}) %>% bind_rows

# now calculate models / true values

to_split <- split(over_samples,list(over_samples$size,
                                    over_samples$iter))

saveRDS(over_samples,"over_samples.rds")

rm(over_samples)

over_models <- lapply(to_split, function(this_data) {
  out_mod <- betareg(outcome ~ treat,
                     data=this_data)
  
  all_coefs <- coef(out_mod)
  
  all_sum <- summary(out_mod)
  
  print(paste0("Now on sample ",this_data$size[1]," and iter ",
               this_data$iter[1]))
  
  tibble(treat_est=all_coefs['treat'],
                treat_p=all_sum$coefficients$mean["treat","Pr(>|z|)"],
                true_treat=true_eff,
                power=treat_p<0.05,
                s_err=sign(all_coefs["treat"])==1,
                size=this_data$size[1],
                iter=this_data$iter[1]) %>% 
           mutate(m_err=treat_est/true_treat) %>% 
    return(.)
  
}) %>% bind_rows

rm(to_split)

saveRDS(over_models,"over_models.rds")

# plot data / power curves                      

require(binom) # for confidence intervals on proportions

to_plot <- over_models %>% 
  group_by(size) %>% 
  summarize(avg_power=mean(power),
            high_power=binom.bayes(x=sum(power),
                                   n=length(power))$upper,
            low_power=binom.bayes(x=sum(power),
                                  n=length(power))$lower)

power_pt <- to_plot$size[which(to_plot$avg_power>.8)[1]]

to_plot %>% 
  ggplot(aes(y=avg_power,x=size)) +
  geom_ribbon(aes(ymin=low_power,
                  ymax=high_power),
              alpha=0.5,fill="blue") +
  geom_hline(yintercept=.8,linetype=2) +
  geom_vline(aes(xintercept=size[which(avg_power>.8)[1]]),
             linetype=3) +
  geom_line() +
  theme(panel.grid=element_blank(),
        panel.background = element_blank()) +
  annotate(geom="text",x=1500,
           y=.83,label="80% Power") +
  xlab("Sample Size") +
  ylab("Average Power") +
  scale_y_continuous(labels=scales::percent_format()) +
  scale_x_continuous(breaks=c(power_pt,500,1000,2000,3000))

ggsave("power_plot.png")
