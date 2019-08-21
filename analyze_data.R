
# required packages
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(betareg)
require(readr)

# change path to reflect most recent data from Qualtrics
qual_data <- read_csv("data/Expropriation+Survey_August+12,+2019_08.33.csv") %>% 
  slice(-c(1:2))

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

exp1 <- select(qual_data,outcome="experiment1_desk_1",
               matches("[a-z][A|B]",ignore.case=F),
               position) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=1)

exp2 <- select(qual_data,outcome="experiment2_desk_1",
               matches("[a-z][C|D]",ignore.case=F),
               position) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=2)

names(exp2) <- str_replace(names(exp2),"(?<=[a-z])C",
                           "A")

names(exp2) <- str_replace(names(exp2),"(?<=[a-z])D",
                           "B")

exp3 <- select(qual_data,outcome="experiment3_desk_1",
               matches("[a-z][E|F]",ignore.case=F),
               position) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=3)

names(exp3) <- str_replace(names(exp3),"(?<=[a-z])E",
                           "A")

names(exp3) <- str_replace(names(exp3),"(?<=[a-z])F",
                           "B")

exp4 <- select(qual_data,outcome="experiment4_desk_1",
               matches("[a-z][G|H]",ignore.case=F),
               position) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=4)

names(exp4) <- str_replace(names(exp4),"(?<=[a-z])G",
                           "A")

names(exp4) <- str_replace(names(exp4),"(?<=[a-z])H",
                           "B")

names(exp4) <- str_remove(names(exp4),"(?<=[a-z])[GH]")

combined <- bind_rows(exp1,
                      exp2,
                      exp3,
                      exp4)

combined$outcome <- as.numeric(combined$outcome)

combinedA <- select(combined,experiment,matches("experiment|outcome|A\\b"),
                    position) %>% 
  mutate(type="A")
combinedB <- select(combined,experiment,matches("experiment|outcome|B\\b"),
                    position) %>% 
  mutate(type="B")

# transform outcome for B, then recombine

combinedB$outcome <- 100 - combinedB$outcome 

names(combinedB) <- str_remove(names(combinedB),"B\\b")
names(combinedA) <- str_remove(names(combinedA),"A\\b")
combined <- bind_rows(combinedA,combinedB) %>% 
  mutate(outcome=(outcome - min(outcome,na.rm = T))/(max(outcome,na.rm=T) - 
                                                       min(outcome,na.rm = T)),
         outcome=(outcome * (sum(!is.na(outcome))-1) + 0.5)/sum(!is.na(outcome)),
         tfp=as.numeric(tfp),
         years=as.numeric(years),
         employees=as.numeric(employees)) 

# current model only uses continuous treatments as discrete 
# treatments need to be combined across languages
all_treat_fit <- betareg(outcome ~ profit + I(profit^2),
                         data = combined)

summary(all_treat_fit)

# by experiment

mods <- lapply(1:max(combined$experiment), function(i) {
  out_mod <- betareg(outcome ~ profit + tfp + 
            assets + years + employees,
          data = filter(combined,experiment==i))
  
  return(out_mod)
})

lapply(mods,summary)

require(rstanarm)

all_treat_fit_bayes <- stan_betareg(outcome ~ profit,
                                    data = combined,
                                    link="logit",
                                    chains=2,cores=2,
                                    algorithm="sampling")

summary(all_treat_fit_bayes,digits=4)

# without TFP

no_tfp <- stan_betareg(outcome ~ profit + assets + years + employees,
                                    data = combined,
                                    link="logit",
                                    chains=2,cores=2,
                                    algorithm="sampling")

