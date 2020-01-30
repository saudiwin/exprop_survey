# Robert Kubinec script for Algeria survey
# Use this script to send SMS to respondents, and also remove duplicate responses

require(readxl)
require(stringr)
require(dplyr)
require(tidyr)
require(hms)
require(lubridate)
require(googlesheets)
require(forcats)
require(readr)

# remove all mobile credits not in survey data
jotform <- read_csv("data/Venezuela-Egypt-Ukraine-Mobile-Credit.csv")

names(jotform) <- c("Submission Date",
                    "Respondent ID",
                    "Network",
                    "Number",
                    "Number2")

to_sms <- jotform %>% 
  mutate(Number=coalesce(Number,Number2),
         Country=case_when(Network %in% c("Kyivstar",
                                          "Lifecell",
                                          "Yezzz!",
                                          "3Mob",
                                          "PeopleNet")~"ukraine",
                            Network %in% c("Movilnet",
                                            "Movistar")~"venezuela",
                            Network %in% c("Orange",
                                            "Etisalat",
                                            "Mobinil",
                                           "Vodafone")~"egypt"),
         just_credit=case_when(Country=="ukraine"~"27 UAH",
                               Country=="venezuela"~"60000 VES",
                               Country=="egypt" & Network %in% c("Etisalat",
                                                                 "Vodafone")~"15 LE",
                               Country=="egypt"~"60 LE"),
         Time=`Submission Date`,
         small_int=as.numeric(str_extract(just_credit,"[0-9]+")),
         Number=str_remove_all(Number,pattern = " "),
         Number=if_else(Country=="venezuela",paste0(substr(Number,1,3),
                                                    "-",
                                                    substr(Number,4,nchar(Number))),
                        Number),
         area_code=recode(Country,algeria='+213',tunisia='+216',egypt='+20',morocco='+212',
                          jordan='+962',Tunisie='+216',
                          ukraine="+380",
                          venezuela="+58"),
         Network=recode(Network,
                        Movistar="Prepaid Movistar mobile top up",
                        Movilnet="Prepaid Movilnet mobile top up",
                        Digitel="Prepaid Digitel mobile top up"))

# check for multiple submissions

if(nrow(distinct(to_sms,`Respondent ID`)) != nrow(to_sms)) {
  print("Duplicate responses detected.")
  to_sms <- group_by(to_sms,`Respondent ID`) %>% 
    mutate(n_response=n()) %>% 
    group_by(`Respondent ID`,Number) %>% 
    mutate(n_nums=n()) %>% 
    ungroup %>% 
    arrange(`Respondent ID`,Time)
  
  # keep the first submission
  
  to_sms <- group_by(to_sms,`Respondent ID`) %>% 
    slice(1)
  
  # check again for duplicates
  
  if(nrow(distinct(to_sms,`Respondent ID`)) == nrow(to_sms)) {
    print("Duplicates solved")
  } else {
    stop("Duplicates remain in file. Need to fix first.")
  }
} 

# check for multiple numbers

# check for multiple submissions

if(length(unique(to_sms$`Respondent ID`)) != length(unique(to_sms$Number))) {
  print("Duplicate numbers detected.")
  to_sms <- group_by(to_sms,Number) %>% 
    mutate(n_nums=n()) %>% 
    ungroup %>% 
    arrange(Number,Time)
  
  # keep the first submission
  
  to_sms <- group_by(to_sms,Number) %>% 
    slice(1)
  
  # check again for duplicates
  
  if(length(unique(to_sms$`Respondent ID`)) == length(unique(to_sms$Number))) {
    print("Duplicate numbers solved")
  } else {
    stop("Duplicate numbers remain in file. Need to fix first.")
  }
} 


