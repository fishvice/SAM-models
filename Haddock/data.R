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
  read_csv('https://data.hafro.is/assmt/2018/haddock/catage.csv') %>% 
  tmp_fun() %>% 
  ## append missing ages with nominal catches
  cbind(`1`=0.001,.)

## Catch weight at age 
cw <- read_csv('https://data.hafro.is/assmt/2018/haddock/catch_weights.csv')%>% 
  tmp_fun() %>% 
  cbind(`1`=0,.) %>% 
  (function(x) x/1000)


## Read the spring survey data
smb <- read_csv('https://data.hafro.is/assmt/2018/haddock/smb.csv')  %>% 
  tmp_fun()
## set the time window of the survey
attributes(smb)$time <- c(0.15,0.2)

## Autumn survey
smh <- read_csv('https://data.hafro.is/assmt/2018/haddock/smh.csv')  %>% 
  tmp_fun()
attributes(smh)$time <- c(0.7,0.8)


## Stock weights
sw <- read_csv('https://data.hafro.is/assmt/2018/haddock/wstock.csv') %>% 
  tmp_fun() %>% 
  cbind(`1`=0,.) %>% 
  (function(x) x/1000)


## Maturity at age
mo <- read_csv('https://data.hafro.is/assmt/2018/haddock/maturity.csv')  %>% 
  tmp_fun() %>% 
  cbind(`1`=0,.)


## Landing frequency
lf <- array(1,dim= dim(cn))
dimnames(lf) <- dimnames(cn)

## Proportion F before spawning
pf <- array(0,dim= dim(cn))
dimnames(pf) <- dimnames(cn)

## Proportion M before spawning
pm <- array(0,dim= dim(cn))
dimnames(pm) <- dimnames(cn)

## Natural mortality 

nm <- array(0.2,dim= dim(cn))
dimnames(nm) <- dimnames(cn)


dat <- setup.sam.data(surveys=list(smb=smb,smh=smh),
                      residual.fleet=cn, 
                      prop.mature=mo, 
                      stock.mean.weight=sw, 
                      catch.mean.weight=cw, 
                      dis.mean.weight=cw, 
                      land.mean.weight=cw,
                      prop.f=pf, 
                      prop.m=pm, 
                      natural.mortality=nm, 
                      land.frac=lf)


## set default settings
conf <- defcon(dat)

conf$keyVarObs[which(conf$keyVarObs==2)] <- c(4,rep(5,7)) #1st age group (age 2) different from rest in autumn survey for variances in estimate
conf$keyVarObs[which(conf$keyVarObs==1)] <- c(2,rep(3,9)) #same here but for spring survey
conf$keyVarObs[which(conf$keyVarObs==0)] <- c(0,0,rep(1,10)) # age 1 and 2 different for commercial
# splitting by 0,1,2,3,4,5 delimits 5 unique variances being estimated across commercial and surveys

## define model parameters
par <- defpar(dat,conf)

## Fit a model with SAM
sam_fit <- sam.fit(dat,conf,par) 

ssbplot(sam_fit)
fbarplot(sam_fit)
recplot(sam_fit)
catchplot(sam_fit)
res <- residuals(sam_fit)
plot(res)
resp <- procres(sam_fit)
plot(resp)
retro <- retro(sam_fit,year=10)
plot(retro)


