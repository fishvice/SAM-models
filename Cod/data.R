library(stockassessment)
library(tidyverse)

## let's read in cod

tmp_fun <- function(x){
  
  x %>% 
    mutate_all(~ifelse(is.na(.),-1,.)) %>%  
    filter(Year < 2018)->x
  
  x %>% 
    select(-Year) %>% 
    as.matrix() -> mat
  
  rownames(mat) <- x$Year
  
  mat
}


## Catch at age 
cn <- 
  read_csv('https://data.hafro.is/assmt/2018/cod/catage.csv') %>% 
  tmp_fun() %>% 
  ## append missing ages with nominal catches
  cbind(`1`=0.001,`2`=0.001,.)

## Catch weight at age 
cw <- read_csv('https://data.hafro.is/assmt/2018/cod/catch_weights.csv')%>% 
  tmp_fun() %>% 
  cbind(`1`=0,`2`=0,.) %>% 
  (function(x) x/1000)


## Read the spring survey data
smb <- read_csv('https://data.hafro.is/assmt/2018/cod/smb.csv')  %>% 
  tmp_fun()
## set the time window of the survey
attributes(smb)$time <- c(0.15,0.2)

## Autumn survey
smh <- read_csv('https://data.hafro.is/assmt/2018/cod/smh.csv')  %>% 
  tmp_fun()
attributes(smh)$time <- c(0.7,0.8)


## Stock weights
sw <- read_csv('https://data.hafro.is/assmt/2018/cod/smb_weights.csv') %>% 
  tmp_fun() %>% 
  ## add missing ages and years using catch weights
  (function(x){rbind(cw[1:30,],
                     cbind(x,cw[-c(1:30),-c(1:9)]))
  })


## Maturity at age
mo <- read_csv('https://data.hafro.is/assmt/2018/cod/maturity.csv')  %>% 
  tmp_fun() %>% 
  cbind(`1`=0,`2`=0,.)
