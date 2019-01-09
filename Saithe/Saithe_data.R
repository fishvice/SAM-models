library(stockassessment)
library(tidyverse)

tmp_fun <- function(x){
  
  x %>% 
    mutate_all(~ifelse(is.na(.),-1,.)) %>%  
    filter(Year <= 2018)->x
  
  x %>% 
    select(-Year) %>% 
    as.matrix() -> mat
  
  rownames(mat) <- x$Year
  
  mat
}


## Catch at age 

cn <- 
  read_csv('https://data.hafro.is/assmt/2018/saithe/catage.csv') %>% 
  tmp_fun() 


## Catch weight at age 
cw <- read_csv('https://data.hafro.is/assmt/2018/saithe/catch_weights.csv')%>% 
  filter(Year<2018)%>%
  tmp_fun() %>% 
  (function(x) x/1000)


## Read the spring survey data
smb <- read_csv('https://data.hafro.is/assmt/2018/saithe/smb.csv')  %>% 
  select(Year,  `3`,   `4`,   `5`,   `6`,   `7`,   `8`,   `9`,  `10`)%>%  # removed age 2 to see if it prevents the crash
  tmp_fun()
## set the time window of the survey
attributes(smb)$time <- c(0.15,0.2)


## Stock weights
sw <- read_csv('https://data.hafro.is/assmt/2018/saithe/catch_weights.csv') %>% 
  tmp_fun() 

## Maturity at age
# mo <- read_csv('https://data.hafro.is/assmt/2018/saithe/maturity.csv')  %>% 
#   tmp_fun() %>% 
#   cbind(`1`=0,`2`=0, `3`=0,.,`10`=1,`11`=1, `12`=1)
mo <- read_csv('https://data.hafro.is/assmt/2018/saithe/maturity.csv')  %>% 
  tmp_fun() %>% 
  cbind(`3`=0,.,`10`=1,`11`=1, `12`=1)
