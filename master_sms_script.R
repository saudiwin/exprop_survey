# given list of sms, send credits

require(dplyr)
require(httr)
require(RSQLite)
require(readr)
require(stringr)

survey <- read_csv('data/Expropriation+Survey_August+21,+2019_08.25.csv')

source('prepare_sms_data.R')
source('send_credits_cysend.R')

  # import existing billing data to make sure we haven't sent one to anyone here yet

billing_data <- read_csv('data/CY.SEND V6.9.5 Billing history.csv') %>% 
  mutate(Number=str_extract(Description,'(?<=(\\+213|\\+20|\\+216|\\+380|\\+58))[0-9]+')) %>% 
  filter(!is.na(Number))

not_sent <- filter(to_sms,!(Number %in% billing_data$Number))

# need to remove leading zero

not_sent$Number <- ifelse(substr(not_sent$Number,1,1)=="0",
                          substr(not_sent$Number,2,nchar(not_sent$Number)),
                          not_sent$Number)

# need to replace egypt 15 with 20

not_sent$small_int <- ifelse(not_sent$Country=="egypt" & not_sent$Network=="Vodafone",20,not_sent$small_int)

if(!all(unique(not_sent$Network) %in% unique(these_prod$name))) {
  stop(paste0("Not all networks match. In particular, ",unique(not_sent$Network)[!(unique(not_sent$Network) %in% unique(these_prod$name))]," doesn't match."))
}


try(file.remove('credit_status.txt'))
over_sms <- lapply(not_sent$`Respondent ID`,function(i) {
  cat(paste0('Now on Respondent ID ',i),file = 'credit_status.txt',append = T)
  this_row <- filter(not_sent,`Respondent ID`==i)
  
  # send credit
  
  outcome <- send_sms(country = this_row$Country,
           number=paste(this_row$area_code,this_row$Number,sep='-'),
           network=this_row$Network,
           amount=this_row$small_int)
           
  return(outcome)
})

# save result 

saveRDS(object = bind_rows(over_sms),
        paste0('data/credits_sent_',as.character(as.Date(lubridate::now()))))
# 
# cat(paste0('credits/credits_sent_',as.character(as.Date(lubridate::now()))),file = 'current_sms_file.txt',
#      append=F)
# 
# source('update_paid_table.R')
# 
# # check for ones we missed 
# 
# check_history <- readr::read_csv('credits/CY.SEND V6.9.2.06  Recharge history.csv')
# 
# missed <- bind_rows(over_sms) %>% 
#   mutate(Number=stringr::str_replace_all(number,'[+-]','')) %>% 
#            filter(!(as.numeric(Number) %in% check_history$`Recharge account`))
# 
# missed_orig <- filter(to_sms,Number %in% missed$Number)
# 
# # save edited to_sms file 
# 
# check_history <- mutate(check_history,
#                         Number=as.character(`Recharge account`),
#                         Number=str_replace(Number,"20|216",''))
# 
# to_sms_final <- mutate(to_sms,
#                        Paid=if_else(Number %in% check_history$Number,'Paid',Paid),
#                        Paid=if_else(Number %in% missed$Number,'Attempt But Fail',Paid))
# 
# saveRDS(to_sms_final,paste0('data/to_sms_final_',as.Date(lubridate::now()),'.rds'))
# 
# read_sms <- paste0('data/to_sms_final_',as.Date(lubridate::now()),'.rds')
# source('update_gs_sheets.R')                       
# 
#          