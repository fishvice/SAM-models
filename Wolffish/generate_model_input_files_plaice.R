assyear <- 2018
attach("data/plaicecno.rda")

res <- expand.grid(list(year=1985:(assyear-1), age=1:16))
res <- plyr::join(res, plaicecno[,c("year","age","cno","cwt")])

i <- plaicecno$year %in% 2005:(assyear - 1)
meanwt <- plyr::ddply(plaicecno[i,],"age",plyr::summarize,meancwt=mean(cwt,na.rm=T))
res <- plyr::join(res,meanwt)
i <- is.na(res$cno)
res$cno[i] <- -1
i <- is.na(res$cwt)
res$cwt[i] <- res$meancwt[i]
# Do not really need age 1 and 2 but convenient to have if program needs it.
i <- res$age %in% 1
res$cwt[i] <- 16
res$cno[i] <- 0
i <- res$age %in% 2
res$cwt[i] <- 80
res$cno[i] <- 0
i <- res$year %in% c(1986,1987,1990) & res$age %in% 1:2
res$cno[i] <- -1

# Use cwt as stock weights.

# Predict matwt
#attach("/net/hafkaldi/export/u2/reikn/Splus5/SMB/.RData")
load("/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Stations.rdata")
library(fjolst)
i <- STODVAR.all$ar %in% 2000:(assyear - 1)
dat <- lesa.kvarnir(STODVAR.all[i,"synis.id"],23,c("kyn","kynthroski"))
dat <- dat[!is.na(dat$aldur) & !is.na(dat$kynthroski),]
dat$kynth <- ifelse(dat$kynthroski > 1,1,0)
mat <- plyr::ddply(dat,"aldur",plyr::summarize,mat=mean(kynth))
names(mat)[1] <- "age"

res <-  plyr::join(res,mat)

# use cwt for swt and ssbwt
res$swt <- res$ssbwt <- res$cwt
res <- res[,c("year","age","cno","cwt","swt","mat","ssbwt")]
names(res)[1] <- "#year" # to get the comment

res$mat[is.na(res$mat)] <- 0
write.table(res,file="ass/sep/catchandstockdata.dat",row.names=F,col.name=T,quote=F)

dat <- all.kv[all.kv$tegund==23,]


# Tékka hvort lengdar þyngdarsambandið er í lagi.
x <- apply.shrink(dat$oslaegt,dat$lengd,median,na.rm=T,names=c("lengd","osl"))
x$predosl <- LWCOEFF[["23"]][1]*x$lengd^LWCOEFF[["23"]][2]
x <- x[x$lengd > 25,]

# Hægvaxta miðað við afla.

dat  <- dat[!is.na(dat$oslaegt) & !is.na(dat$aldur),]
tapply(dat$oslaegt,dat$aldur,median,na.rm=T)


# ------------------------------------------------------------------------------
# Rallvísitölur

load("data/plaiceind.rda")
dat <- geo::apply.shrink(plaiceind$fj,plaiceind$year,sum,names=c("year","totfj"))
dat <- dat[dat$totfj > 0,]
dat <- plaiceind[plaiceind$year %in% dat$year,]
dat <- dat[,c("year","age","fj")]
names(dat)[1] <- "#year"
write.table(dat,file="ass/sep/surveydata.dat",row.names=F,col.names=T,sep="\t",quote=F)
