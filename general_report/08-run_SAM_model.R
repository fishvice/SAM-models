library(stockassessment)
#library(tidyverse)

## Get the data

##source('data.R')

## Additional input data

## Landing frequency
lf <- array(1,dim= dim(cn_s))
dimnames(lf) <- dimnames(cn_s)

## Proportion F before spawning
pf <- array(0,dim= dim(cn_s))
dimnames(pf) <- dimnames(cn_s)

## Proportion M before spawning
pm <- array(0,dim= dim(cn_s))
dimnames(pm) <- dimnames(cn_s)

## Natural mortality 

nm <- array(0.1,dim= dim(cn_s))
dimnames(nm) <- dimnames(cn_s)


## Prepare input to the SAM call

dat <- setup.sam.data(surveys=list(smb=smb_b_s,smh=smh_b_s), #should this be biomass or numbers?
                      residual.fleet=cn_s*1000, 
                      prop.mature=mat_s, 
                      stock.mean.weight=smb_sw_s, 
                      catch.mean.weight=cw_s, 
                      dis.mean.weight=cw_s, 
                      land.mean.weight=cw_s,
                      prop.f=pf, 
                      prop.m=pm, 
                      natural.mortality=nm, 
                      land.frac=lf)


## set default settings
conf <- defcon(dat)
ll <- length(conf$keyVarObs[which(conf$keyVarObs==2)])

if(Species == 2){
  
  conf$keyVarObs[which(conf$keyVarObs==2)] <- c(4,rep(5,7)) #1st age group (age 2) different from rest in autumn survey for variances in estimate
  conf$keyVarObs[which(conf$keyVarObs==1)] <- c(2,rep(3,9)) #same here but for spring survey
  conf$keyVarObs[which(conf$keyVarObs==0)] <- c(0,0,rep(1,10)) # age 1 and 2 different for commercial
  # splitting by 0,1,2,3,4,5 delimits 5 unique variances being estimated across commercial and surveys
  
}

if(Species == 9) {
conf$keyVarObs[which(conf$keyVarObs==2)] <- c(4,4,rep(5,ll-2)) #1st 2 age groups (age 1 - 2) different from rest in autumn survey for variances in estimate
conf$keyVarObs[which(conf$keyVarObs==1)] <- c(2,rep(3,ll-2)) #same here but for spring survey
conf$keyVarObs[which(conf$keyVarObs==0)] <- c(0,0,0,0,rep(1,ll-4)) # age 1 and 2 different for commercial
# splitting by 0,1,2,3,4,5 delimits 5 unique variances being estimated across commercial and surveys
}

## define model parameters
par <- defpar(dat,conf)

## Fit a model with SAM
fit <- sam.fit(dat,conf,par) 


ssbplot(fit)
fbarplot(fit)
recplot(fit)
catchplot(fit)
res <- residuals(fit)
plot(res)
resp <- procres(fit)
plot(resp)
retro <- retro(fit,year=10)
plot(retro)



