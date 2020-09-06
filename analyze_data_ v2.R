
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
require(bayesplot)
require(inflection)
require(mirt)
require(texreg)

# auxiliary code

source("define_ord_betareg.R")

source("prepare_data.R")

# look at some correlates of political connections

# let's make some factor scores for political connections
# need to recode these variables

all_types <- unique(unlist(str_split(unique(qual_data$pol_connect1_1),pattern=",")))
all_types <- all_types[!is.na(all_types)]
# all_types <- sort(factor(all_types,levels=c("Former Low-ranking Bureaucrat",
#                                        "Current Low-ranking Bureaucrat",
#                                        "Former High-ranking Bureaucrat",
#                                        "Current High-ranking Bureaucrat",
#                                        "Former Member of Parliament",
#                                        "Current Member of Parliament")))
#all_type_num <- as.numeric(all_types)
over_types <- lapply(all_types, function(a) {
  out_tib <- tibble(!!a :=rowSums(cbind(
          as.numeric(grepl(x=qual_data$pol_connect1_1,pattern=a)),
          as.numeric(grepl(x=qual_data$pol_connect1_2,pattern=a)),
          as.numeric(grepl(x=qual_data$pol_connect1_3,pattern=a)),
          as.numeric(grepl(x=qual_data$pol_connect1_4,pattern=a)))))
}) %>% bind_cols

names(over_types) <- c("cur_high_beau",
                       "for_low_beau",
                       "for_high_beau",
                       "for_mem_parl",
                       "cur_low_beau",
                       "cur_mem_parl")

pol_score <- mirt(over_types,
                  model=1,
                  itemtype="Rasch")

# put pol score back in the data frame

qual_data$pol_score <- as.numeric(fscores(pol_score))

# table of pol scores by performance

str_wrap_factor <- function(x, ...) {
  levels(x) <- str_wrap(levels(x), ...)
  x
}

qual_data <- mutate(qual_data,perf_1=factor(perf_1, levels=c("More than 20% loss",
                                                             "Between 10 and 20% loss",
                                                             "Between 10 and 5% loss",
                                                             "Between 5 and 0% loss",
                                                             "Broke even",
                                                             "Between 0 and 5% profit margin",
                                                             "Between 5 and 10% profit margin",
                                                             "Between 10 and 20% profit margin",
                                                             "More than 20% profit margin")))


qual_data %>% 
  filter(!is.na(perf_1)) %>% 
  ggplot(aes(y=pol_score,x=str_wrap_factor(perf_1,5))) +
  stat_summary(fun.data=mean_cl_boot) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(size=7)) +
  ylab("Political Connections Score")

ggsave("pol_con_perf.png")

# let's do a heat map of pol connections 1-10 and pol efficiency 1-10

# make a quantitative version of firm performance

qual_data <- mutate(qual_data,perf_quant=as.numeric(as.character(factor(perf_1,
                                                                        labels=c(-30,
                                                                                 -15,
                                                                                 -7.5,
                                                                                 -2.5,
                                                                                 0,
                                                                                 2.5,
                                                                                 7.5,
                                                                                 15,
                                                                                 30)))))

qual_data %>% 
  mutate(pol_con_1=as.numeric(pol_con_1),
         pol_eff_1=as.numeric(pol_eff_1)) %>% 
  filter(!is.na(pol_con_1),!is.na(pol_eff_1)) %>% 
  group_by(pol_con_1,pol_eff_1) %>% 
  mutate(count_pol=n()) %>% 
  ggplot(aes(y=pol_con_1,x=pol_eff_1)) +
  geom_count() +
  theme(panel.background = element_blank()) +
  xlab("Political Connections Level") +
  ylab("Political Efficacy Level") +
  guides(size=guide_legend(title="N"))

ggsave("pol_con_eff.png")

# figure out categories of companies

qual_data <- mutate(qual_data,
                    pol_con_1=as.numeric(pol_con_1),
                    pol_eff_1=as.numeric(pol_eff_1),
                    firm_pol_cat=case_when(pol_con_1<6 & pol_eff_1<6~"Non-Connected/Hurt",
                                           pol_con_1>5 & pol_eff_1<6~"Connected/Hurt",
                                           pol_con_1<6 & pol_eff_1>5~"Non-Connected/Helped",
                                           pol_con_1>5 & pol_eff_1>5~"Connected/Helped"))

# see how performance breaks down by these categories

prop.table(table(qual_data$perf_1,qual_data$firm_pol_cat),margin=2)

qual_data %>% 
  group_by(perf_1,firm_pol_cat) %>% 
  count %>% 
  filter(!is.na(firm_pol_cat),!is.na(perf_1)) %>% 
  group_by(firm_pol_cat) %>% 
  mutate(n=n/sum(n)) %>% 
  ggplot(aes(x=perf_1,y=n)) +
  geom_col() +
  facet_wrap(~firm_pol_cat,ncol=1) +
  scale_y_continuous(labels=scales::percent,breaks=c(0,0.1,0.2)) +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        axis.text.x = element_text(size=7,angle=90)) +
  xlab("")  + ylab("")

ggsave("con_perf.png")

# datasets:

# qual_data - original survey data
# combined_clean - all data excluding duplicates, fast responses
# combined_clean_clicks - data with information on number of clicks on the sliders (much more recent)

# fit basic OLS model

lm_fit <- lm(outcome_norm ~ connections + employees + years + own + sector + country,
                       data = combined_clean)

summary(lm_fit)

lm_fit <- lm(outcome_norm ~ gender*perf + employees + years + own,
             data = combined_clean)

summary(lm_fit)

# ordered beta regression

# scale variables 

# base treatment

if(run_model) {
  base_con <- brm(outcome_norm ~ 0 + Intercept + connected + employees + years + own + country + sector + assets,
                  family=ord_beta_reg,
                  data=combined_clean,
                  stanvars=stanvars,
                  prior=priors,
                  iter=1000,
                  chains=1,cores=1)
  
  saveRDS(base_con,"data/base.rds")
} else {
  base_con <- readRDS("data/base.rds")
}

# all treatments

if(run_model) {
  all_con <- brm(outcome_norm ~ 0 + Intercept + connections + employees + years + own + country + sector + assets,
                  family=ord_beta_reg,
                  data=combined_clean,
                  stanvars=stanvars,
                  prior=priors,
                  iter=1000,
                  chains=1,cores=1)
  
  saveRDS(all_con,"data/all.rds")
} else {
  all_con <- readRDS("data/all.rds")
}

# connected X country

if(run_model) {
  country_con <- brm(outcome_norm ~ 0 + Intercept + connected*country_resp + employees + years + own + country + sector + assets,
                  family=ord_beta_reg,
                  data=combined_clean,
                  stanvars=stanvars,
                  prior=priors,
                  iter=1000,
                  chains=1,cores=1)
  
  saveRDS(country_con,"data/base_country.rds")
} else {
  country_con <- readRDS("data/base_country.rds")
}

# connected X pol_help 

if(run_model) {
  pol_int_con <- brm(outcome_norm ~ 0 + Intercept + connected*pol_eff_1 + employees + years + own + country + sector + assets,
                     family=ord_beta_reg,
                     data=combined_clean,
                     stanvars=stanvars,
                     prior=priors,
                     iter=1000,
                     chains=1,cores=1)
  
  saveRDS(pol_int_con,"data/pol_int_con.rds")
} else {
  pol_int_con <- readRDS("data/pol_int_con.rds")
}

# connected X connected

if(run_model) {
  pol_con_con <- brm(outcome_norm ~ 0 + Intercept + connected*pol_con_1 + employees + years + own + country + sector + assets,
                     family=ord_beta_reg,
                     data=combined_clean,
                     stanvars=stanvars,
                     prior=priors,
                     iter=1000,
                     chains=1,cores=1)
  
  saveRDS(pol_con_con,"data/pol_con_con.rds")
} else {
  pol_con_con <- readRDS("data/pol_con_con.rds")
}

# connected X profits

if(run_model) {
  con_profit <- brm(outcome_norm ~ 0 + Intercept + connected*pol_con_1 + employees + years + own + country + sector + assets,
                     family=ord_beta_reg,
                     data=combined_clean,
                     stanvars=stanvars,
                     prior=priors,
                     iter=1000,
                     chains=1,cores=1)
  
  saveRDS(con_profit,"data/con_profit.rds")
} else {
  con_profit <- readRDS("data/con_profit.rds")
}

# gender
if(run_model) {
  gender <- brm(outcome_norm ~ 0 + Intercept + gender + employees + years + own + Duration + position,
                family=ord_beta_reg,
                data=combined_clean,
                stanvars=stanvars,
                prior=priors,
                iter=1000,
                chains=1,cores=1)
  
  saveRDS(gender ,"data/gender1.rds")
} else {
  gender <- readRDS("data/gender1.rds")
}


# gender by country
# nothing to see here

if(run_model) {
  gender_country <- brm(outcome_norm ~ 0 + Intercept + gender*country_resp + employees + years + own + Duration + position,
                        family=ord_beta_reg,
                        data=combined_clean,
                        stanvars=stanvars,
                        prior=priors,
                        iter=1000,
                        chains=1,cores=1)
  
  saveRDS(gender_country ,"data/gender_country.rds")
} else {
  gender_country <- readRDS("data/gender_country.rds")
}


# gender by performance

if(run_model) {
  gender_combined_perf_conn <- brm(outcome_norm ~ 0 + Intercept + gender*mo(combined_perf) + employees + years + own + Duration + position,
                                   family=ord_beta_reg,
                                   data=combined_clean,
                                   stanvars=stanvars,
                                   prior=priors,
                                   iter=1000,
                                   chains=1,cores=1)
  
  saveRDS(gender_perf ,"data/gender_perf.rds")
} else {
  gender_perf <- readRDS("data/gender_perf.rds")
}


# output to latex for inclusion in paper

texreg(all_con,file="all_con.tex",booktabs = T,
       bold=0.05,
       dcolumn = T,caption="ATEs for All Political Connection Treatments",scalebox=.5,
       custom.coef.names=c("Intercept",
                           "Daughter of President",
                           "Mid-level Bureaucrat on Board",
                           "Cabinet Minister on Board",
                           "MP on Board",
                           "Son of President on Board",
                           "Owner is Army General",
                           "Owner is an MP",
                           "Owner is Nephew of PM",
                           "Owner is Niece of PM",
                           "Owner is Police Officer",
                           "Owner is President's Classmate",
                           "Owner is President's Daughter-in-law",
                           "Owner belongs to President's Party",
                           "Owner is President's Son in Law",
                           "No. Employees",
                           "Years Operation",
                           "Private",
                           "SOE",
                           "Germany",
                           "Saudi Arabia",
                           "China",
                           "Korea",
                           "Egypt",
                           "USA",
                           "Japan",
                           "Russia",
                           "Ukraine",
                           "Venezuela",
                           "Construction",
                           "Energy",
                           "Financials",
                           "Manufacturing",
                           "Retail",
                           "Telecom",
                           "Assets"))

int_con_names <- c("Politically Connected",
                   "No. Employees",
                   "Years in Operation",
                   "Private",
                   "SOE",
                   "Germany",
                   "Saudi",
                   "China",
                   "Korea",
                   "Egypt",
                   "USA",
                   "Japan",
                   "Russia",
                   "Ukraine",
                   "Venezuela",
                   "Construction",
                   "Energy",
                   "Financials",
                   "Manufacturing",
                   "Retail",
                   "Telecom",
                   "Assets",
                   "Respondent:Ukraine",
                   "Respondent:Venezuela",
                   "ConnectedXResp. Ukraine",
                   "ConnectedXResp. Venezuela")

texreg(list(base_con,country_con,pol_int_con,pol_con_con), 
       file="int_con.tex",scalebox=0.5,
       bold=0.05,
       booktabs=T,dcolumn=T,caption="ATEs for Connection Treatment and Interactions")

texreg(list(gender,gender_country,gender_perf),booktabs=T,dcolumn=T,
       file="gender.tex",
       bold=0.05,
       scalebox=0.5,
       caption="ATEs for Gendered Political Connections")