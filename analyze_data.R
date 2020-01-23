
# required packages
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(betareg)
require(readr)
require(lubridate)

# change path to reflect most recent data from Qualtrics
qual_data <- read_csv("data/Expropriation+Survey_August+12,+2019_08.33.csv") %>% 
  slice(-c(1:2)) %>% 
  filter(consent_fixed=="Agree" | consent_random=="Agree") %>% 
  mutate(EndDate=ymd_hms(EndDate)) %>% 
  filter(EndDate>ymd_hms("2020-01-01 12:00:00")) %>% 
  mutate(outcome1=coalesce(experiment1_desk_1,exp1_short_1),
         outcome2=coalesce(experiment2_desk_1,exp2_short_1),
         outcome3=coalesce(experiment3_desk_1,exp3_short_1),
         outcome4=coalesce(experiment4_desk_1,exp4_short_1),
         short_long=case_when(!is.na(experiment1_desk_1)~"Long",
                              !is.na(exp1_short_1)~"Short",
                              TRUE~NA_character_),
         Duration=scale(as.numeric(`Duration (in seconds)`)))

# need to run a model predicting outcome with treatments
# using betareg
# transform outcome & recode data from survey

recode_currency <- function(col) {
  
  col <- sapply(col, function(w) {

    if(is.na(w)) {
      return(NA)
    }
    
    if(grepl(x=w,pattern="M")) {
      as.numeric(str_remove_all(w,"[M,'k$-]"))*1000000
    } else if(grepl(x=w,pattern="k")) {
      as.numeric(str_remove_all(w,"[M,'k$-]"))*100000
    } else {
      as.numeric(str_remove_all(w,"[M,'k$-]"))
    }
  })
  
  col
  
}

exp1 <- select(qual_data,outcome="outcome1",
               matches("[a-z][A|C|E|G]",ignore.case=F),
               position,short_long,ResponseId,Duration,country_resp="country") %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=1) %>% 
  select(-RecipientEmail,
         -DistributionChannel,
         -IncentiveAmount)


exp2 <- select(qual_data,outcome="outcome2",
               matches("[a-z][B|D|F|H]",ignore.case=F),
               position,short_long,ResponseId,Duration,country_resp="country") %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=2) %>% 
  select(-StartDate,
         -EndDate,
         -RecordedDate,
         -RecipientFirstName)


exp1 <- gather(exp1,key = treatment,value=value,-outcome,-experiment,-Duration,
               -position,-ResponseId,-short_long,-country_resp) %>% 
  mutate(profile=str_extract(treatment,"[A-Z]"),
         treatment=str_remove(treatment,"[A-Z]"))
exp2 <- gather(exp2,key = treatment,value=value,-outcome,-experiment,-position,-Duration,
               -ResponseId,-short_long,-country_resp) %>% 
  mutate(profile=str_extract(treatment,"[A-Z]"),
         treatment=str_remove(treatment,"[A-Z]"))

names(exp1) <- str_replace(names(exp1),"(?<=[a-z])[CEG]",
                           "A")

names(exp2) <- str_replace(names(exp2),"(?<=[a-z])[DFH]",
                           "B")

combined <- bind_rows(exp1,
                      exp2)

combined <- combined %>% 
  mutate(outcome=as.numeric(outcome),
         outcome=ifelse(experiment==1,100-outcome,outcome),
         outcome_norm=(outcome - min(outcome,na.rm = T))/(max(outcome,na.rm=T) - 
                                                       min(outcome,na.rm = T)),
         outcome_prop=(outcome_norm* (sum(!is.na(outcome_norm))-1) + 0.5)/sum(!is.na(outcome_norm)))

# make it wide

combined <- spread(combined,key = "treatment",value="value")

combined <- mutate(combined,
                   connections=recode(connections,
                            `El propietario es ex oficial de policía`="own_police",
                            `El propietario es miembro del partido político del presidente`="own_pres_party",
                            `El propietario es sobrino del primer ministro`="own_pm_nephew",
                            `El propietario es un ex miembro del parlamento`="own_mp",
                            `El propietario es un general retirado de las Fuerzas Armadas`="own_gen_army",
                            `El propietario no tiene interés en la política`="control",
                            `Un funcionario público de rango intermedio está en la Junta Directiva de la empresa`="board_med_crat",
                            `Un hijo del presidente forma parte de la Junta Directiva de la empresa`="board_son_pres",
                            `Un miembro del parlamento está en la Junta Directiva de la empresa`="board_mp",
                            `Un ministro está en la Junta Directiva de la empresa`="board_minister",
                            `El propietario es ex compañero de clase del presidente`="own_pres_classmate",
                            `Una hija del presidente forma parte de la Junta Directiva de la empresa`="board_daughter_pres",
                            `El propietario está casado con un hijo del presidente`="own_pres_son_inlaw",
                            `La propietaria es sobrina del primer ministro`="own_pm_niece",
                            `La propietaria está casado con una hija del presidente`="own_pres_daughter_inlaw",
                            `Владелец - бывший генерал`="own_gen_army",
                            `Владелец - бывший офицер полиции`="own_police",
                            `Владелец - бывший член парламента`="own_mp",
                            `Владелец - зять президента`="own_pres_son_inlaw",
                            `Владелец - одноклассник президента`="own_pres_classmate",
                            `Владелец - племянник премьер министра`="own_pm_nephew",
                            `Владелец - племянница премьер министра`="own_pm_niece",
                            `Владелец - член политической партии президента`="own_pres_party",
                            `Владелица - невестка президента`="own_pres_daughter_inlaw",
                            `Дочь президента входит в совет директоров компании`="board_daughter_pres",
                            `Министр входит в совет директоров компании`="board_minister",
                            `Сын президента входит в совет директоров компании`="board_son_pres",
                            `У владелеца нет интереса к политике`="control",
                            `Чиновник среднего уровня входит в совет директоров компании`="board_med_crat",
                            `Член парламента входит в совет директоров компании`="board_mp",
                            `المالك  جنرال سابق في الجيش`="own_gen_army",
                            `المالك  زميل دراسة سابق للرئيس`="own_pres_classmate",
                            `المالك  ضابط سابق في الشرطة`="own_police",
                            `المالك عضو سابق في البرلمان`="own_mp",
                            `المالك عضو في الحزب السياسي الذي ينتمي له الرئيس`="own_pres_party",
                            `المالك غير مهتم بالسياسية`="control",
                            `المالك متزوج من ابنة الرئيس`="own_pres_son_inlaw",
                            `المالك هو إبن أخ رئيس الوزراء`="own_pm_nephew",
                            `المالكة هي بنت أخ رئيس الوزراء`="own_pm_niece",
                            `يتضمن مجلس إدارة الشركة إبن الرئيس`="board_son_pres",
                            `المالكة متزوجة من ابن الرئيس`="own_pres_daughter_inlaw",
                            `يتضمن مجلس إدارة الشركة إبنة الرئيس`="board_daughter_pres",
                            `يتضمن مجلس إدارة الشركة بيروقراطي متوسط المستوى`="board_med_crat",
                            `يتضمن مجلس إدارة الشركة رئيس الوزارة`="board_minister",
                            `يتضمن مجلس إدارة الشركة عضو من البرلمان`="board_mp",
                            `Mid-level bureaucrat is on board of company`="board_med_crat",
                            `Owner has no interest in politics`="control",
                            `Owner is a niece of the prime minister`="own_pm_niece",
                            `Owner is member of the President's political party`="own_pres_party",
                            `President's daughter is on the board of the company`="board_daughter_pres",
                            `Owner is former general in the military`="own_gen_army",
                            `President's son is on the board of the company`="board_son_pres"),
         connections=forcats::fct_relevel(factor(connections),"control"),
         perf=recode(perf,`Покрывает расходы`="break_even",
                     `Теряет немного денег`="lose_some",
                     `Теряет много денег`="lose_much",
                     `Algo rentable`="gain_some",
                     `Немного прибыльная`="gain_some",
                     `Очень прибыльная`="gain_much",
                     `Perder mucho dinero`="lose_much",
                     `Cubrir los gastos`="break_even",
                     `Muy rentable`="gain_much",
                     `Perder algo de dinero`="lose_some",
                     `خسارة الكثير من المال`="lose_much",
                     `خسارة بعض المال`="lose_some",
                     `مربحة إلى حد ما`="gain_some",
                     `التعادل (لا يوجد ربح أو خسارة)`="break_even",
                     `مربح للغاية`="gain_much",
                     `Losing A Lot of Money`="lose_much",
                     `Break Even`="break_even",
                     `Somewhat Profitable`="lose_some")) %>% 
  mutate_at(c("employees","years"),as.numeric)

# current model only uses continuous treatments as discrete 
# treatments need to be combined across languages
all_treat_fit <- betareg(outcome_prop ~ connections,
                         data = combined)

all_treat_fit_lm <- lm(outcome ~ connections + employees + years + short_long + Duration,
                         data = combined)

summary(all_treat_fit_lm)

# break apart by country

output <- lapply(unique(combined$country_resp), function(c) {
  print(c)
  print(summary(lm(outcome ~ connections + employees + years + short_long,
     data = filter(combined,country_resp==c))))
})

# just short

all_treat_short <- lm(outcome ~ connections + employees + years + perf,
                       data = filter(combined,short_long=="Short"))

summary(all_treat_short)

output <- lapply(unique(combined$country_resp), function(c) {
  print(c)
  print(summary(lm(outcome ~ connections + employees + years,
                   data = filter(combined,country_resp==c,short_long=="Short"))))
})

# just long

all_treat_long <- lm(outcome ~ connections + employees + years,
                      data = filter(combined,short_long=="Long"))

summary(all_treat_long)

output <- lapply(unique(combined$country_resp), function(c) {
  print(c)
  print(summary(lm(outcome ~ connections + employees + years,
                   data = filter(combined,country_resp==c,short_long=="Long"))))
})

# check gender effects

combined$connections_gender <- if_else(combined$connections %in% c("board_daughter_pres","own_pm_niece","own_pres_daughter_inlaw"),
                                       "Female",if_else(combined$connections %in% c("board_son_pres","own_pm_nephew","own_pres_son_inlaw"),
                                                        "Male","Neutral"))
gender_lm <- lm(outcome ~ connections_gender + employees + years +  short_long,
                       data = combined)
summary(gender_lm)

output <- lapply(unique(combined$country_resp), function(c) {
  print(c)
  print(summary(lm(outcome ~ connections_gender + employees + years,
                   data = filter(combined,country_resp==c))))
})

# look at just managers

all_treat_fit_lm <- lm(outcome ~ connections + employees + years + short_long,
                       data = filter(combined,position=="Manager"))

summary(all_treat_fit_lm)


# implement a Stan model

require(rstan)

beta_logit_stan <- stan_model("beta_logit.stan")

combine_prop <- filter(combined,outcome_norm>0,outcome_norm<1)
combine_degen <- filter(combined,outcome==0|outcome==100) %>% 
  mutate(outcome=ifelse(outcome>0,1,outcome))

treat_matrix_prop <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_prop)
treat_matrix_degen <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_degen)

to_stan_data <- list(N_prop=nrow(treat_matrix_prop),
                     N_degen=nrow(treat_matrix_degen),
                     X=ncol(treat_matrix_prop)-1,
                     outcome_prop=combine_prop$outcome_norm[as.numeric(row.names(treat_matrix_prop))],
                     outcome_degen=combine_degen$outcome[as.numeric(row.names(treat_matrix_degen))],
                     covar_degen=treat_matrix_degen[,-1],
                     covar_prop=treat_matrix_prop[,-1])

est_beta_logit <- sampling(beta_logit_stan,data=to_stan_data,cores=2,chains=2,iter=1000)

# look at it for just Egypt

combine_prop <- filter(combined,outcome_norm>0,outcome_norm<1,country_resp=="Egypt")
combine_degen <- filter(combined,outcome==0|outcome==100,country_resp=="Egypt") %>% 
  mutate(outcome=ifelse(outcome>0,1,outcome))

treat_matrix_prop <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_prop)
treat_matrix_degen <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_degen)

to_stan_data <- list(N_prop=nrow(treat_matrix_prop),
                     N_degen=nrow(treat_matrix_degen),
                     X=ncol(treat_matrix_prop)-1,
                     outcome_prop=combine_prop$outcome_norm[as.numeric(row.names(treat_matrix_prop))],
                     outcome_degen=combine_degen$outcome[as.numeric(row.names(treat_matrix_degen))],
                     covar_degen=treat_matrix_degen[,-1],
                     covar_prop=treat_matrix_prop[,-1])

est_beta_logit_egypt <- sampling(beta_logit_stan,data=to_stan_data,cores=2,chains=2,iter=1000,init=0)

# now just Ukraine

combine_prop <- filter(combined,outcome_norm>0,outcome_norm<1,country_resp=="Ukraine")
combine_degen <- filter(combined,outcome==0|outcome==100,country_resp=="Ukraine") %>% 
  mutate(outcome=ifelse(outcome>0,1,outcome))

treat_matrix_prop <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_prop)
treat_matrix_degen <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_degen)

to_stan_data <- list(N_prop=nrow(treat_matrix_prop),
                     N_degen=nrow(treat_matrix_degen),
                     X=ncol(treat_matrix_prop)-1,
                     outcome_prop=combine_prop$outcome_norm[as.numeric(row.names(treat_matrix_prop))],
                     outcome_degen=combine_degen$outcome[as.numeric(row.names(treat_matrix_degen))],
                     covar_degen=treat_matrix_degen[,-1],
                     covar_prop=treat_matrix_prop[,-1])

est_beta_logit_ukraine <- sampling(beta_logit_stan,data=to_stan_data,cores=2,chains=2,iter=1000,init=0)

# let's try with a correction for missing data


beta_logit_stan_miss <- stan_model("beta_logit_missing.stan")

combine_prop <- filter(combined,outcome_norm>0,outcome_norm<1)
combine_degen <- filter(combined,outcome==0|outcome==100) %>% 
  mutate(outcome=ifelse(outcome>0,1,outcome))

treat_matrix_prop <- model.matrix(outcome ~ connections + employees + years + short_long+Duration,data=combine_prop)
treat_matrix_miss <- treat_matrix_prop[,"Duration",drop=F]
treat_matrix_prop <- treat_matrix_prop[,!(colnames(treat_matrix_prop) %in% colnames(treat_matrix_miss)),drop=F]
treat_matrix_degen <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_degen)

to_stan_data <- list(N_prop=nrow(treat_matrix_prop),
                     N_degen=nrow(treat_matrix_degen),
                     X=ncol(treat_matrix_prop)-1,
                     X_miss=ncol(treat_matrix_miss),
                     outcome_prop=combine_prop$outcome_norm[as.numeric(row.names(treat_matrix_prop))],
                     outcome_degen=combine_degen$outcome[as.numeric(row.names(treat_matrix_degen))],
                     covar_degen=treat_matrix_degen[,-1],
                     covar_prop=treat_matrix_prop[,-1],
                     covar_miss=treat_matrix_miss)

est_beta_logit_miss <- sampling(beta_logit_stan_miss,data=to_stan_data,cores=2,chains=2,iter=1000,init=0)

# let's try to simulate a new distribution for ordered beta regression

cutpoints <- c(-2,2) 
# on the logit scale

N <- 1000

X <- runif(N,min=-1,max=1)
eta <- X*2.5

# ancillary parameter of beta distribution
phi <- 2

alpha <- -0.2
gamma <- c(1.8, 0.4)
mu1 <- alpha + eta

low <- 1-plogis(mu1 - cutpoints[1])
middle <- plogis(mu1-cutpoints[1]) - plogis(mu1-cutpoints[2])
high <- plogis(mu1 - cutpoints[2])

# we'll assum ethe same eta was used to generate outcomes

out_beta <- rbeta(N,plogis(mu1) * phi, (1 - plogis(mu1)) * phi) 

outcomes <- sapply(1:N, function(i) {
  sample(1:3,size=1,prob=c(low[i],middle[i],high[i]))
})
# now pull full density
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

require(bayesplot)

yrep <- extract(fit_model,"regen_all")[[1]]

# use only a sample of draws

yrep <- yrep[sample(1:nrow(yrep),size=100),]

ppc_dens_overlay(y=final_out,yrep=yrep) + ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

# looks pretty good

        