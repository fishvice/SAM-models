library(stockassessment)
library(tidyverse)

## Get the data

source('data.R')

## Additional input data

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


## Prepare input to the SAM call

dat <- setup.sam.data(surveys=list(smb=smb,smh=smh),
                      residual.fleet=cn*1000, 
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

## define model parameters
par <- defpar(dat,conf)

## Fit a model with SAM
fit <- sam.fit(dat,conf,par) 


