# get full list of credits from CY.SEND

require(httr)
require(dplyr)

this_func <- "get_product_countries"
this_user <- "XEHSMruHVKNsc7SpTVD542KztksQnd7b"
this_pass <- "BZT5zcwCU6jv1GDnfm8zSxUs4FUw2RPhg6n77psTeLzJ1aFKMrXXNmuhnXj2EyfS"
api_url <- 'https://honeypot.cysend.ch/merchant_api/4.5'
all_count <- POST(api_url,
                  body=list(`function`=this_func,
                            format='json',
                            username=this_user,
                            hash=digest::digest(paste(this_func,
                                                      'json',
                                                      this_user,
                                                      this_pass,sep = '|'),
                                                algo='md5',
                                                serialize = F)))

all_count_cont <- try(jsonlite::fromJSON(content(all_count, "text"), simplifyVector = FALSE))

all_count_cont <- lapply(all_count_cont$response$countries,as_data_frame) %>% 
  bind_rows

this_func <- "get_products"

all_prod <- POST(api_url,
                 body=list(`function`=this_func,
                           format='json',
                           range='yes',
                           username=this_user,
                           hash=digest::digest(paste(this_func,
                                                     'json',
                                                     'yes',
                                                     this_user,
                                                     this_pass,sep = '|'),
                                               algo='md5',
                                               serialize = F)))

all_prod_cont <- jsonlite::fromJSON(content(all_prod, "text"), simplifyVector = FALSE) 
all_prod_cont <- lapply(all_prod_cont$response$products, function(x) {
  x[[7]] <- NULL
  as_data_frame(x)
}) %>% 
  bind_rows %>% 
  mutate(product_id=id) %>% 
  select(-id)

# repeat for non-range products

all_prod_nor <- POST(api_url,
                 body=list(`function`=this_func,
                           format='json',
                           range='no',
                           username=this_user,
                           hash=digest::digest(paste(this_func,
                                                     'json',
                                                     'no',
                                                     this_user,
                                                     this_pass,sep = '|'),
                                               algo='md5',
                                               serialize = F)))

all_prod_cont_nor <- jsonlite::fromJSON(content(all_prod_nor, "text"), simplifyVector = FALSE) 
all_prod_cont_nor <- lapply(all_prod_cont_nor$response$products, function(x) {
  x[[7]] <- NULL
  as_data_frame(x)
}) %>% 
  bind_rows %>% 
  mutate(product_id=id) %>% 
  select(-id)

# combine

all_prod_cont <- bind_rows(all_prod_cont,all_prod_cont_nor)

# products for EGYPT

these_prod <- all_count_cont %>% 
  mutate(country_name=name) %>% 
  select(-name) %>% 
  left_join(all_prod_cont,by=c(id='country_id'))

# product values 

this_func <- "get_product_values"

prod_val <- POST(api_url,
                 body=list(`function`=this_func,
                           username=this_user,
                           format='json',
                           hash=digest::digest(paste(this_func,
                                                     this_user,
                                                     'json',
                                                     this_pass,sep = '|'),
                                               algo='md5',
                                               serialize = F)))

prod_val_cont <- jsonlite::fromJSON(content(prod_val, "text"), simplifyVector = FALSE) 
prod_val_cont <- lapply(prod_val_cont$response$values,as_data_frame) %>% 
  bind_rows %>% 
  mutate(product_id=as.character(product_id))

# add in specific product values

these_prod <- left_join(these_prod,
                        prod_val_cont,
                        by='product_id') %>% 
  mutate(country_name=tolower(country_name))

send_sms <- function(country,number,network,amount,prod_list=these_prod) {
 
  # check
  
  # get product value
  if(network=="Prepaid Movistar mobile top up" || country=="ukraine") {
    # missing product values for Movistar
    this_prod <- prod_list %>% filter(country_name==country,
                                      name==network)
    this_prod$product_value <- amount
  } else {
    this_prod <- prod_list %>% filter(country_name==country,
                                      name==network,
                                      product_value==amount)
  }
  
  if(nrow(this_prod)==0) {
    # check and see if amount is an issue
    this_prod <- mutate(prod_list,name=tolower(name)) %>% 
      filter(country_name==country,
             name==network) %>% 
      filter(product_value==min(product_value))
    if(nrow(this_prod)==0) {
      stop(paste0('Number ',number, 'in country ',country,', the data doesnt match anything in the product list'))
    }
  }
  this_tid <- as.character(round(as.numeric(Sys.time())))
  transfer <- POST(api_url,
                   body=list(`function`='instant_transfer',
                             beneficiary_account=number,
                             product=as.integer(this_prod$product_id),
                             value=this_prod$product_value,
                             tid=this_tid,
                             sms_receipt='no',
                             username=this_user,
                             format='json',
                             hash=digest::digest(paste('instant_transfer',
                                                       number,
                                                       as.integer(this_prod$product_id),
                                                       this_prod$product_value,
                                                       this_tid,
                                                       'no',
                                                       this_user,
                                                       'json',
                                                       this_pass,sep = '|'),
                                                 algo='md5',
                                                 serialize = F)))
  
  transfer_cont <- try(jsonlite::fromJSON(content(transfer, "text"), simplifyVector = FALSE))
  
  if('try-error' %in% class(transfer_cont)) {
    warning(paste0("JSON parse failed for number ",number))
    print(content(transfer,"text"))
    return(number)
  }
  
  if(transfer_cont$response$status=='PROCESSING') {
    # check for whether transfer has gone through. wait 10s
    Sys.sleep(10)
    transfer_check <- POST(api_url,
                           body=list(`function`='transfer_status',
                                     tid=this_tid,
                                     cysend_tid=transfer_cont$response$cysend_tid,
                                     username=this_user,
                                     format='json',
                                     hash=digest::digest(paste('transfer_status',
                                                               this_tid,
                                                               transfer_cont$response$cysend_tid,
                                                               this_user,
                                                               'json',
                                                               this_pass,sep = '|'),
                                                         algo='md5',
                                                         serialize = F)))
    transfer_check_cont <- jsonlite::fromJSON(content(transfer_check, "text"), simplifyVector = FALSE)
    # if not done, wait another 15s
    if(transfer_check_cont$response$status!='OK') {
      Sys.sleep(15)
      transfer_check <- POST(api_url,
                             body=list(`function`='transfer_status',
                                       tid=this_tid,
                                       cysend_tid=transfer_cont$response$cysend_tid,
                                       username=this_user,
                                       format='json',
                                       hash=digest::digest(paste('transfer_status',
                                                                 this_tid,
                                                                 transfer_cont$response$cysend_tid,
                                                                 this_user,
                                                                 'json',
                                                                 this_pass,sep = '|'),
                                                           algo='md5',
                                                           serialize = F)))
      transfer_check_cont <- jsonlite::fromJSON(content(transfer_check, "text"), simplifyVector = FALSE)
      #if still not done, fail
      if(transfer_check_cont$response$status!='OK') warning(paste0('Transfer for number ',number,' still not done.'))
    }
  } else {
    warning(paste0('Transfer for number ',number,' failed.'))
  }
  
  # number should have processed, return data frame with results
  
  return(data_frame(number=number,
                    amount=amount,
                    product_id=this_prod$product_id,
                    tid=this_tid))
}
