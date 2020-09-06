
# required packages
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(betareg)
require(readr)
require(lubridate)
require(brms)
require(rstanarm)
require(forcats)

# change path to reflect most recent data from Qualtrics
qual_data <- read_csv("data/Expropriation+Survey_February+8,+2020_04.29.csv") %>% 
  slice(-c(1:2)) %>% 
  filter(consent_fixed=="Agree" | consent_random=="Agree") %>% 
  mutate(EndDate=ymd_hms(EndDate)) %>% 
  #filter(EndDate>ymd_hms("2020-01-22 12:00:00")) %>% 
  mutate(outcome1=coalesce(experiment1_desk_1,exp1_short_1),
         outcome2=coalesce(experiment2_desk_1,exp2_short_1),
         outcome3=coalesce(experiment3_desk_1,exp3_short_1),
         outcome4=coalesce(experiment4_desk_1,exp4_short_1),
         short_long=case_when(!is.na(experiment1_desk_1)~"Long",
                              !is.na(exp1_short_1)~"Short",
                              TRUE~NA_character_),
         Duration=scale(as.numeric(`Duration (in seconds)`)))


# we have double the number of responses that we "should" have given the number of FB completions
# suggests duplicate responses
# remove by checking cell phone #s and IP addresses

qual_data <- group_by(qual_data,IPAddress) %>% 
  arrange(EndDate) %>% 
  slice(1)

# remove ones with missing ad IDs (sign of shared link)

qual_data <- filter(qual_data,!is.na(adid))

# remove any where it took less than 5 minutes to finish the survey (should take longer than that)

qual_data <-  mutate(qual_data,`Duration (in seconds)`=as.numeric(`Duration (in seconds)`)) %>% 
                       filter(`Duration (in seconds)`>300)

# check for cell phone dupes

jotform1 <- read_csv("data/Venezuela-Egypt-Ukraine-Mobile-Credit (1).csv") %>% 
  mutate(cell_num=coalesce(`Введите ваш номер телефона (без впереди идущих нолей):`,
                           `Introduzca su número de teléfono (sin código de país):`))
jotform2 <- read_csv("data/Clone-of-Clone-of-Venezuela-Egypt-Ukraine-Mobile-Credit.csv")
names(jotform2)[6] <- "arab_num"
jotform2 <- jotform2 %>% 
  mutate(cell_num=coalesce(`Введите ваш номер телефона (без впереди идущих нолей):`,
           `Introduzca su número de teléfono (sin código de país):`,
           arab_num))

all_jot <- bind_rows(jotform1,jotform2) %>% select(RandomNum=`Respondent ID`,cell_num,
                                                   resp_date=`Submission Date`) %>% 
  mutate(resp_date=ymd_hms(resp_date)) %>% 
  group_by(cell_num) %>% 
  mutate(n_resp=length(unique(RandomNum))) %>% 
  arrange(resp_date) %>% 
  slice(-1)
  
# get rid of these dupes

qual_data <- anti_join(qual_data,all_jot,by="RandomNum") %>% 
  ungroup

# let's test for difference between exp 1 and exp 2

clicks_any <- !is.na(qual_data$clicks1) | !is.na(qual_data$clicks2) | !is.na(qual_data$clicks3) | !is.na(qual_data$clicks4)
qual_data <- qual_data %>% 
  mutate_at(vars(matches("click")), function(c,clicks_any) {
    c <- ifelse(clicks_any,coalesce(as.numeric(c),0),c)
  },clicks_any) %>% 
    mutate(clicks_comb = as.numeric(clicks1) + as.numeric(clicks2) + 
                      as.numeric(clicks3) + as.numeric(clicks4))

t.test(formula=clicks_comb~short_long,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00"),clicks_comb<50))
kruskal.test(formula=clicks_comb~short_long,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00")))
wilcox.test(formula=clicks_comb~short_long,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00")),paired=F)
summary(glm(clicks_comb~mobile+short_long+position,data=filter(qual_data,EndDate>ymd_hms("2020-01-22 12:00:00"),clicks_comb<50),
            family="poisson"))

qual_data %>% 
  ggplot(aes(x=clicks_comb)) +
  geom_histogram() +
  facet_wrap(~short_long) +
  theme_minimal()

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
               matches("[a-z][A|C|E|G]",ignore.case=F),clicksA="clicks1",clicksC="clicks2",clicksE="clicks3",
               clicksG="clicks4",
               position,short_long,ResponseId,Duration,country_resp="country") %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=1) %>% 
  select(-RecipientEmail,
         -DistributionChannel,
         -IncentiveAmount)


exp2 <- select(qual_data,outcome="outcome2",
               matches("[a-z][B|D|F|H]",ignore.case=F),clicksB="clicks1",clicksD="clicks2",
               clicksF="clicks3",clicksH="clicks4",
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

# replace missing click values with mean of respondent

combined <- mutate(combined, clicks=as.numeric(clicks))

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
                            `President's son is on the board of the company`="board_son_pres",
                            `Owner is former member of parliament`="own_mp",
                            `Owner is a nephew of the prime minister`="own_pm_nephew",
                            `Owner is former officer in the police`="own_police",
                            `Owner is former classmate of the President`="own_pres_classmate",
                            `Owner is married to the President's son`="own_pres_daughter_inlaw",
                            `Head of ministry is on board of company`="board_minister",
                            `Member of parliament is on board of company`="board_mp",
                            `Owner is married to the President's daughter`="own_pres_son_inlaw"),
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
                     `Somewhat Profitable`="lose_some",
                     `Losing Some Money`="lose_some",
                     `Very Profitable`="gain_much"),
         country=fct_collapse(country,Germany=c("Alemania","Germany","Германия"),
                              Egypt=c("Egypt","مصر"),
                              Ukraine=c("Украина","Ukraine"),
                              Saudi=c("Arabia Saudita","Saudi Arabia","Саудовская Аравия"),
                              Brazil=c("Brasil","Brazil","Бразилия"),
                              China=c("China","Китай"),
                              Korea=c("Corea del Sur","Южная Корея"),
                              USA=c("United States","Estados Unidos","США"),
                              Japan=c("Japan","Япония"),
                              Russia=c("Rusia","Россия"),
                              Venezuela="Venezuela")) %>% 
  mutate_at(c("employees","years"),as.numeric) %>% 
  mutate(employees=scale(employees),
         years=scale(years),
         clicks=scale(clicks))

# need to figure out what to do with profit/revenue/etc



# remove 50s, 51s and 63s (possible missing values)

combined_clean <- group_by(combined,ResponseId) %>% 
  mutate(all50=(!(all(outcome==50) || all(outcome==63) || all(outcome==51) || all(outcome==52) || all(outcome==37)))) %>% 
  filter(all50,!is.na(outcome))

# only ones where we measured clicks

combined_clean_clicks <- filter(combined_clean,!is.na(clicks))

require(rstanarm)

all_treat_fit_stanlm <- rstanarm::stan_lm(outcome_norm ~ connections + employees + years,
                       data = combined_clean,
                       prior=NULL)

yrep_lm <- posterior_predict(all_treat_fit_stanlm)

#ppc_dens_overlay(all_treat_fit_stanlm$model$outcome,yrep_lm)

rmse_lm <- apply(yrep_lm,1,function(c) sqrt(mean((c-all_treat_fit_stanlm$model$outcome)^2)))

all_treat_fit_stanlm

all_treat_fit_stanlm_clicks <- rstanarm::stan_lm(outcome_norm ~ connections + employees + years,
                                          data = combined_clean_clicks,
                                          prior=NULL)

yrep_lm_clicks <- posterior_predict(all_treat_fit_stanlm_clicks)

#ppc_dens_overlay(all_treat_fit_stanlm$model$outcome,yrep_lm)

rmse_lm_clicks <- apply(yrep_lm_clicks,1,function(c) sqrt(mean((c-all_treat_fit_stanlm_clicks$model$outcome_norm)^2)))

all_treat_fit_stanlm_clicks

stan_plot(all_treat_fit_stanlm_clicks,pars=c("(Intercept)","log-posterior","R2","mean_PPD","sigma","log-fit_ratio"),include=F)

# ordered beta regression

# scale variables 

combine_prop <- filter(combined_clean,outcome_norm>0,outcome_norm<1)
combine_degen <- filter(combined_clean,outcome==0|outcome==100) %>% 
  mutate(outcome=ifelse(outcome>0,1,outcome))

treat_matrix_prop <- model.matrix(outcome ~ connected*employees,data=combine_prop)
treat_matrix_degen <- model.matrix(outcome ~ connected*employees,data=combine_degen)

to_sample <- nrow(treat_matrix_prop)

# indices_degen <- sample(1:nrow(treat_matrix_degen),size=to_sample/2)
# indices_prop <- sample(1:nrow(treat_matrix_prop),size=to_sample/2)

to_stan_data <- list(N_prop=nrow(treat_matrix_prop),
                     N_degen=nrow(treat_matrix_degen),
                     X=ncol(treat_matrix_prop)-1,
                     outcome_prop=combine_prop$outcome_norm[as.numeric(row.names(treat_matrix_prop))],
                     outcome_degen=combine_degen$outcome[as.numeric(row.names(treat_matrix_degen))],
                     covar_degen=treat_matrix_degen[,-1],
                     covar_prop=treat_matrix_prop[,-1],
                     N_pred_degen=nrow(treat_matrix_degen),
                     N_pred_prop=nrow(treat_matrix_prop),
                     indices_degen=1:nrow(treat_matrix_degen),
                     indices_prop=1:nrow(treat_matrix_prop),
                     run_gen=1)

ord_beta_mydata <- sampling(beta_logit_stan,data=to_stan_data,cores=2,chains=2,iter=1000,init=0)

yrep <- as.matrix(ord_beta_mydata,pars="regen_all")
loo_ord <- loo(ord_beta_mydata,"ord_log")

# rmse

rmse_ord <- apply(yrep,1,function(c) sqrt(mean((c-c(to_stan_data$outcome_degen,to_stan_data$outcome_prop))^2)))
sd_ord <- apply(yrep,1,sd)
kurt_ord <- apply(yrep,1,moments::kurtosis)
#ppc_dens_overlay(y=c(to_stan_data$outcome_degen,to_stan_data$outcome_prop),yrep=yrep) + ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

#ppc_ecdf_overlay(y=c(to_stan_data$outcome_degen,to_stan_data$outcome_prop),yrep=yrep) + ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

rbeta_mean <- function(N,mu,phi) {
  rbeta(N, mu * phi, (1 - mu) * phi)
}

# get marginal effects

eps <- 1e-7
setstep <- function(x) {
  x + (max(abs(x), 1, na.rm = TRUE) * sqrt(eps)) - x
}

# functions for doing marginal effects

predict_ordbeta <- function(cutpoints=NULL,X=NULL,X_beta=NULL,
                            combined_out=T) {
  
  # we'll assume the same eta was used to generate outcomes
  eta <- X %*% as.matrix(X_beta)
  
  # probabilities for three possible categories (0, proportion, 1)
  low <- 1-plogis(eta - cutpoints[1])
  middle <- plogis(eta-cutpoints[1]) - plogis(eta-cutpoints[2])
  high <- plogis(eta - cutpoints[2])
  
  # check for whether combined outcome or single outcome
  
  if(combined_out) {
    low*0 + middle*plogis(eta) + high*1
  } else {
    list(pr_zero=low,
         pr_proportion=middle,
         proportion_value=plogis(eta),
         pr_one=high)
  }
  
}

cutpoints_est <- as.matrix(ord_beta_mydata,"cutpoints")
X_beta_ord <- as.matrix(ord_beta_mydata,"X_beta")

# iterate over columns to get marginal effects

mat_data <- rbind(treat_matrix_degen,treat_matrix_prop)[,-1]

all_vars <- lapply(1:ncol(mat_data),function(c) {
  
  if(all(mat_data[!is.na(mat_data[,c]),c] %in% c(0,1))) {
    pred_data_high <- mat_data
    
    pred_data_high[,c] <- 0
    
    pred_data_low <- mat_data
    
    pred_data_low[,c] <- 1
  } else {
    pred_data_high <- mat_data
    
    pred_data_high[,c] <- pred_data_high[,c] + setstep(pred_data_high[,c])
    
    pred_data_low <- mat_data
    
    pred_data_low[,c] <- pred_data_low[,c] - setstep(pred_data_low[,c])
  }
  
  
  
  margin_ord <- sapply(1:nrow(X_beta_ord), function(i,this_col) {
    y0 <- predict_ordbeta(cutpoints=cutpoints_est[i,],
                          X=pred_data_low,
                          X_beta=X_beta_ord[i,])
    
    y1 <- predict_ordbeta(cutpoints=cutpoints_est[i,],
                          X=pred_data_high,
                          X_beta=X_beta_ord[i,])
    
    marg_eff <- (y1-y0)/(pred_data_high[,this_col]-pred_data_low[,this_col])
    
    mean(marg_eff)
  },c)
  
  tibble(marg=margin_ord,variable=colnames(mat_data)[c])
}) %>% bind_rows

# plot the marginal effects

all_vars %>% 
  group_by(variable) %>% 
  mutate(marg=marg*100) %>% 
  summarize(med_est=median(marg),
            high=quantile(marg,.95),
            low=quantile(marg,.05)) %>% 
  ggplot(aes(y=med_est,x=reorder(variable,med_est))) +
  geom_pointrange(aes(ymin=low,ymax=high)) +
  theme_minimal() +
  geom_hline(yintercept = 0,linetype=2) +
  scale_y_continuous(labels=scales::dollar_format()) +
  coord_flip()

# evaluate effect of connected treatment conditional on number of employees

con_x_employ <- as.matrix(ord_beta_mydata,"X_beta[3]")
employ <- as.matrix(ord_beta_mydata,"X_beta[2]")
con_beta <- as.matrix(ord_beta_mydata,"X_beta[1]")
alpha <- as.matrix(ord_beta_mydata,"alpha")

# evaluate over employees

over_employ_cont <- sapply(seq(min(combined$employees,na.rm=T),max(combined$employees,na.rm=T),length.out=100), function(em) {
  plogis(employ*em)
}) %>% 
  as_tibble %>% 
  mutate(iter=1:n(),type="Control") %>% 
  gather(employ_num,estimate,-iter,-type)

over_employ_treat <- sapply(seq(min(combined$employees,na.rm=T),max(combined$employees,na.rm=T),length.out=100), function(em) {
  plogis(con_x_employ*em + con_beta + employ*em)
}) %>% 
  as_tibble %>% 
  mutate(iter=1:n(),type="Treatment") %>% 
  gather(employ_num,estimate,-iter,-type)

bind_rows(over_employ_cont,
          over_employ_treat)  %>% 
  group_by(type,employ_num) %>% 
  summarize(med_est=median(estimate),
            high_est=quantile(estimate,.95),
            low_est=quantile(estimate,.05)) %>% 
  mutate(employ_num=as.numeric(stringr::str_extract(employ_num,"[0-9]+"))) %>% 
  ggplot(aes(y=med_est,x=employ_num)) +
  geom_ribbon(aes(ymin=low_est,
                  ymax=high_est,fill=type),alpha=0.5) +
  geom_line(linetype=2,aes(color=type))

# just do connected vs. unconnected


# Phi regression -----------------------------------------------

beta_logit_phireg <- stan_model("beta_logit_phireg.stan")

# need to make a sum_clicks variable

combined <- group_by(combined,ResponseId) %>% 
  mutate(sum_clicks=sum(clicks))

combine_prop <- filter(combined,outcome_norm>0,outcome_norm<1,
                       !(is.na(connections)|is.na(employees)|is.na(years)|is.na(short_long)|is.na(clicks)|is.na(Duration)))
combine_degen <- filter(combined,outcome==0|outcome==100) %>% 
  mutate(outcome=ifelse(outcome>0,1,outcome)) %>% 
  filter(!(is.na(connections)|is.na(employees)|is.na(years)|is.na(short_long)|is.na(clicks)|is.na(Duration)))

treat_matrix_prop <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_prop)
treat_matrix_degen <- model.matrix(outcome ~ connections + employees + years + short_long,data=combine_degen)
treat_matrix_phi <- model.matrix(outcome ~  poly(clicks,2) + poly(Duration,2) + position,data=combine_prop)
treat_matrix_phi_degen <- model.matrix(outcome ~ poly(clicks,2) + poly(Duration,2) + position,data=combine_degen)
to_sample <- nrow(treat_matrix_prop)

to_stan_data <- list(N_prop=nrow(treat_matrix_prop),
                     N_degen=nrow(treat_matrix_degen),
                     X=ncol(treat_matrix_prop)-1,
                     X_phi=ncol(treat_matrix_phi)-1,
                     outcome_prop=combine_prop$outcome_norm[as.numeric(row.names(treat_matrix_prop))],
                     outcome_degen=combine_degen$outcome[as.numeric(row.names(treat_matrix_degen))],
                     covar_degen=treat_matrix_degen[,-1],
                     covar_prop=treat_matrix_prop[,-1],
                     covar_prop_phi=treat_matrix_phi[,-1],
                     covar_degen_phi=treat_matrix_phi_degen[,-1],
                     N_pred_degen=nrow(treat_matrix_degen),
                     N_pred_prop=nrow(treat_matrix_prop),
                     indices_degen=1:nrow(treat_matrix_degen),
                     indices_prop=1:nrow(treat_matrix_prop),
                     run_gen=1)

ord_beta_con <- sampling(beta_logit_phireg,data=to_stan_data,cores=2,chains=2,iter=1000,init=0)

#stan_plot(ord_beta_con,"X_beta")
#stan_plot(ord_beta_con,c("alpha_phi","X_beta_phi"))

yrep_con <- as.matrix(ord_beta_con,pars="regen_all")

# rmse

rmse_ord_phi <- apply(yrep_con,1,function(c) sqrt(mean((c-c(to_stan_data$outcome_degen,to_stan_data$outcome_prop))^2)))
kurt_ord_phi <- apply(yrep_con,1,moments::kurtosis)
#ppc_dens_overlay(y=c(to_stan_data$outcome_degen,to_stan_data$outcome_prop),yrep=yrep) + ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")
#ppc_ecdf_overlay(y=c(to_stan_data$outcome_degen,to_stan_data$outcome_prop),yrep=yrep) + ggtitle("Posterior Predictive Distribution for Ordinal Beta Regression",subtitle="N=1000")

# compare to OLS rmse

tibble(rmse_ord_phi=rmse_ord_phi[1:1000]*100,rmse_lm=rmse_lm[1:1000],
       rmse_ord=rmse_ord[1:1000]*100) %>% 
  gather(key="type",value="rmse") %>% 
  ggplot(aes(x=rmse)) +
  geom_density(aes(fill=type),alpha=0.5) +
  geom_vline(aes(xintercept=mean(rmse),linetype=type)) +
  theme_minimal()

cutpoints_est <- as.matrix(ord_beta_con,"cutpoints")
X_beta_ord <- as.matrix(ord_beta_con,"X_beta")

# make a plot with cutpoints overlain on empirical density

tibble(outcome=c(to_stan_data$outcome_degen,to_stan_data$outcome_prop),
       cut1=mean(plogis(cutpoints_est[,1])),
       cut2=mean(plogis(cutpoints_est[,2]))) %>% 
  ggplot(aes(x=outcome)) +
  geom_histogram() +
  theme_minimal() +
  geom_vline(aes(xintercept=unique(cut1))) +
  geom_vline(aes(xintercept=unique(cut2)))

# iterate over columns to get marginal effects

mat_data <- rbind(treat_matrix_degen,treat_matrix_prop)[,-1]

all_vars_con <- lapply(1:ncol(mat_data),function(c) {
  
  if(all(mat_data[!is.na(mat_data[,c]),c] %in% c(0,1))) {
    pred_data_high <- mat_data
    
    pred_data_high[,c] <- 0
    
    pred_data_low <- mat_data
    
    pred_data_low[,c] <- 1
  } else {
    pred_data_high <- mat_data
    
    pred_data_high[,c] <- pred_data_high[,c] + setstep(pred_data_high[,c])
    
    pred_data_low <- mat_data
    
    pred_data_low[,c] <- pred_data_low[,c] - setstep(pred_data_low[,c])
  }
  
  
  
  margin_ord <- sapply(1:nrow(X_beta_ord), function(i,this_col) {
    y0 <- predict_ordbeta(cutpoints=cutpoints_est[i,],
                          X=pred_data_low,
                          X_beta=X_beta_ord[i,])
    
    y1 <- predict_ordbeta(cutpoints=cutpoints_est[i,],
                          X=pred_data_high,
                          X_beta=X_beta_ord[i,])
    
    marg_eff <- (y1-y0)/(pred_data_high[,this_col]-pred_data_low[,this_col])
    
    mean(marg_eff)
  },c)
  
  tibble(marg=margin_ord,variable=colnames(mat_data)[c])
}) %>% bind_rows

# plot the marginal effects

bind_rows(list(just_mean=all_vars,
               with_phi=all_vars_con),.id="model") %>% 
  group_by(variable,model) %>% 
  mutate(marg=marg*100) %>% 
  summarize(med_est=median(marg),
            high=quantile(marg,.95),
            low=quantile(marg,.05)) %>% 
  ggplot(aes(y=med_est,x=reorder(variable,med_est))) +
  geom_pointrange(aes(ymin=low,ymax=high,colour=model,shape=model),
                  position=position_dodge(width=.5)) +
  theme_minimal() +
  geom_hline(yintercept = 0,linetype=2) +
  scale_y_continuous(labels=scales::dollar_format()) +
  coord_flip()


loo_phi <- loo(ord_beta_con,"ord_log")
loo_mu <- loo(ord_beta_mydata,"ord_log")

loo::loo_compare(loo_mu,loo_phi)
