source('Saithe_data.R')

## Additional input data

## Landing frequency
lf <- array(1,dim= dim(cn))
dimnames(lf) <- dimnames(cn)

## Proportion F before spawning
pf <- array(0,dim= dim(sw))
dimnames(pf) <- dimnames(sw)

## Proportion M before spawning
pm <- array(0,dim= dim(sw))
dimnames(pm) <- dimnames(sw)

## Natural mortality 

nm <- array(0.2,dim= dim(sw))
dimnames(nm) <- dimnames(sw)


## Prepare input to the SAM call

dat <- setup.sam.data(survey=smb,
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
conf$fbarRange <- c(4,10)
conf$corFlag <- 1 # was 2
conf$keyLogFpar
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
# [1,]   -1   -1   -1   -1   -1   -1   -1   -1   -1
# [2,]    0    1    2    3    4    5    6    6   -1


## define model parameters
par <- defpar(dat,conf)
par$logFpar <- rep(0,7)

## Fit a model with SAM
fit <- sam.fit(dat,conf,par) 





### SAM figures

res <- residuals(fit)

resp <- procres(fit)

retro <- retro(fit,year=10)

#lo <- leaveout(fit)

# same order as first figure in the advice pdf

par(mfrow=c(2,2))

catchplot(fit)

recplot(fit)

fbarplot(fit)

ssbplot(fit)



# Residual plot
plot(res)

plot(resp)

# Retro
plot(retro)

# With or without each survey
#plot(lo)

