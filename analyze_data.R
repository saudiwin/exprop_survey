
# required packages
require(dplyr)
require(tidyr)
require(stringr)
require(ggplot2)
require(betareg)
require(readr)
require(lubridate)

# change path to reflect most recent data from Qualtrics
qual_data <- read_csv("data/Expropriation+Survey_January+14,+2020_03.25.csv") %>% 
  slice(-c(1:2)) %>% 
  mutate(EndDate=ymd_hms(EndDate)) %>% 
  filter(EndDate>ymd_hms("2020-01-01 12:00:00")) %>% 
  mutate(outcome1=coalesce(experiment1_desk_1,exp1_short_1),
         outcome2=coalesce(experiment2_desk_1,exp2_short_1),
         outcome3=coalesce(experiment3_desk_1,exp3_short_1),
         outcome4=coalesce(experiment4_desk_1,exp4_short_1),
         short_long=case_when(!is.na(experiment1_desk_1)~"Long",
                              !is.na(exp1_short_1)~"Short",
                              TRUE~NA_character_))

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
               matches("[a-z][A|B]",ignore.case=F),
               position,short_long) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=1)


exp2 <- select(qual_data,outcome="outcome2",
               matches("[a-z][C|D]",ignore.case=F),
               position,short_long) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=2)

names(exp2) <- str_replace(names(exp2),"(?<=[a-z])C",
                           "A")

names(exp2) <- str_replace(names(exp2),"(?<=[a-z])D",
                           "B")

exp3 <- select(qual_data,outcome="outcome3",
               matches("[a-z][E|F]",ignore.case=F),
               position,short_long) %>% 
  mutate_at(vars(matches("assets|profit")),
            recode_currency) %>% 
  mutate(experiment=3)

names(exp3) <- str_replace(names(exp3),"(?<=[a-z])E",
                           "A")

names(exp3) <- str_replace(names(exp3),"(?<=[a-z])F",
                           "B")

exp4 <- select(qual_data,outcome="outcome4",
               matches("[a-z][G|H]",ignore.case=F),
               position,short_long) %>% 
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
                    position,short_long) %>% 
  mutate(type="A",
         outcome=na_if(outcome, c(101)))
combinedB <- select(combined,experiment,matches("experiment|outcome|B\\b"),
                    position,short_long) %>% 
  mutate(type="B",
         outcome=na_if(outcome, c(101)))

# transform outcome for B, then recombine

combinedB$outcome <- 100 - combinedB$outcome 

names(combinedB) <- str_remove(names(combinedB),"B\\b")
names(combinedA) <- str_remove(names(combinedA),"A\\b")
combined <- bind_rows(combinedA,combinedB) %>% 
  mutate(outcome_norm=(outcome - min(outcome,na.rm = T))/(max(outcome,na.rm=T) - 
                                                       min(outcome,na.rm = T)),
         outcome_prop=(outcome_norm* (sum(!is.na(outcome_norm))-1) + 0.5)/sum(!is.na(outcome_norm)),
         tfp=as.numeric(tfp),
         years=as.numeric(years),
         sales=as.numeric(recode_currency(sales)),
         employees=as.numeric(employees),
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
                            `Член парламента входит в совет директоров компании`="board_mp"),
         connections=forcats::fct_relevel(factor(connections),"control")) 

# current model only uses continuous treatments as discrete 
# treatments need to be combined across languages
all_treat_fit <- betareg(outcome_prop ~ connections,
                         data = combined)

all_treat_fit_lm <- lm(outcome ~ connections + employees + years + short_long,
                         data = combined)

summary(all_treat_fit_lm)

# check gender effects

combined$connections_gender <- if_else(combined$connections %in% c("board_daughter_pres","own_pm_niece","own_pres_daughter_inlaw"),
                                       "Female",if_else(combined$connections %in% c("board_son_pres","own_pm_nephew","own_pres_son_inlaw"),
                                                        "Male","Neutral"))
gender_lm <- lm(outcome ~ connections_gender + employees + years +  short_long,
                       data = combined)
summary(gender_lm)
