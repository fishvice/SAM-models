#library(plyr)
library(fjolst)
library(Logbooks)
# The old way
#attach("/net/hafkaldi/export/u2/reikn/Tac/2016/02/HBFunctions/.RData")
#attach("/net/hafkaldi/export/u2/reikn/Splus5/GETALK/.RData")
# The intermediate way - "soon" to be replaced by fishvise/pax
# devtools::install_github("fishvice/husky")
library(husky)

gear1 <- c("v2","v5","v6")
gear <- gearlist[gearlist$geartext %in% gear1,]

cond.23 <- c(0.0094,3.0780)

# Gá hvað er til af gögnum.
dat <- stodvar[stodvar$synaflokkur %in% c(1,2,4,8),]
dat1 <- lesa.kvarnir(dat$synis.id,23)
dat <- calcgear(dat, gearlist)
dat <- dat[dat$vf %in% gear1,]
dat1 <- fjolst:::join(dat1,dat,"synis.id")
dat1 <- dat1[!is.na(dat1$aldur),]
dat1 <- dat1[dat1$ar > 1990,]
x <- table(dat1$ar,dat1$vf)
#x

# Byrja á CNO

TEG <- 23
ALDUR <- 3:16
LE <- c(seq(24.5,54.5,by=5),64.5)
commcatch <- landedcatch[landedcatch$fteg==TEG & (landedcatch$veidisvaedi %in% "I" | is.na(landedcatch$veidisvaedi)),]
totcatch <-  apply.shrink(commcatch$magn,commcatch$ar,sum,names=c("year","catch"))

commcatch$veidarfaeri <- commcatch$veidarf # Fyrir calcgeawr
commcatch <- calcgear(commcatch, gearlist)
commcatch <- commcatch[commcatch$vf %in% gear1,]
commcatch$reg <- "s1" # eitt svæði
commcatch$time <- ifelse(commcatch$man > 5,"t2","t1")
commcatch <- plyr::ddply(commcatch,c("ar","time","reg","vf"),plyr::summarize,catch=sum(magn/1000))



result <- NULL
for(year in c(1985,1988:1989,1991:2017)) {
  print(year)
  styear <- lesa.stodvar(ar = year, veidarfaeri = unique(gear$veidarfaeri))
  styear <- inside.reg.bcbreytt(styear,option=2) # Sýni upp í landi fylgja með.
  styear <- styear[styear$area %in% 1:10,]
  styear <- styear[styear$synaflokkur %in% c(1,2,4,8),]
  styear <- calcgear(styear, gearlist)  # Breyta veiðarfærum, sameina botnvörpukóða o.s.frv.
  kvyear <- lesa.kvarnir(styear$synis.id, TEG, c("kyn", "kynthroski","slaegt","oslaegt"),oracle=F)  # Allar kvarnir ársins.
  kvyear <- kvyear[!is.na(kvyear$aldur),  ]
  kvyear$fjoldi <- rep(1,nrow(kvyear))
  leyear <- lesa.lengdir(styear$synis.id, TEG)
  styear <- styear[!is.na(match(styear$synis.id,c(kvyear$synis.id,leyear$synis.id))),]

  styear$reg <- rep("s1",nrow(styear))  # Bara eitt svæði
  styear$time <- ifelse(styear$man > 5,"t2","t1")

  styear$index <- paste(styear$vf,styear$reg,styear$time,sep="")
  styearkv <- styear[!is.na(match(styear$synis.id,kvyear$synis.id)),]
  print(table(styearkv$reg,styearkv$time,styearkv$vf))




   # Búa til kvarnir og lengdir með minna vægi
   kvallyearlesswt <- kvyear;kvallyearlesswt$fjoldi <- rep(0.001,nrow(kvallyearlesswt))
   leallyearlesswt <- leyear;leallyearlesswt$fjoldi <- 0.001*leallyearlesswt$fjoldi

  cnoplaice <- list()




# Fyrri hluti árs, allar kvarnir þar hafa vægið 0.01 í samsull
  tmpst  <- styear[styear$reg %in% "s1" & styear$time %in% "t1",]
  tmpkv <- kvyear[!is.na(match(kvyear$synis.id,tmpst$synis.id)),]
  if(nrow(tmpkv) > 0) tmpkv$fjoldi <- 0.01



  cat("dr janmai")

  afli <- sum(commcatch$catch[commcatch$ar==year & commcatch$vf %in% "v5" & commcatch$time %in% "t1" & commcatch$reg %in% "s1"])
  st <- styear[styear$vf %in% "v5" & styear$reg %in% "s1" & styear$time %in% "t1",]
  kv <- kvyear[!is.na(match(kvyear$synis.id,st$synis.id)),]
  kv <- rbind(kv,kvallyearlesswt,tmpkv)
  le <- leyear[!is.na(match(leyear$synis.id,st$synis.id)),]
  le <- rbind(le,leallyearlesswt)
  cond <- cond.23
  keys <- MakeAlk(kv,TEG,kynth=F,lengd=LE,aldur=ALDUR,Stodvar=st,FilterAldurLengd=F)
  tmp3 <- MakeLdist(TEG,lengd=LE,Stodvar=st,lengdir=le,lengd.thyngd.data=cond,talid=F,afli=afli)
  cnoplaice$v5s1t1 <- Calc.fj(keys,tmp3)


  cat("bv janmai")

  afli <- sum(commcatch$catch[commcatch$ar==year & commcatch$vf %in% "v6" & commcatch$time %in% "t1" & commcatch$reg %in% "s1"])
  st <- styear[styear$vf %in% "v6" & styear$reg %in% "s1" & styear$time %in% "t1",]
  kv <- kvyear[!is.na(match(kvyear$synis.id,st$synis.id)),]
  kv <- rbind(kv,kvallyearlesswt,tmpkv)
  le <- leyear[!is.na(match(leyear$synis.id,st$synis.id)),]
  le <- rbind(le,leallyearlesswt)
  cond <- cond.23
  keys <- MakeAlk(kv,TEG,kynth=F,lengd=LE,aldur=ALDUR,Stodvar=st,FilterAldurLengd=F)
  tmp3 <- MakeLdist(TEG,lengd=LE,Stodvar=st,lengdir=le,lengd.thyngd.data=cond,talid=F,afli=afli)
  cnoplaice$v6s1t1 <- Calc.fj(keys,tmp3)


  cat("net allt ár")
  afli <- sum(commcatch$catch[commcatch$ar==year & commcatch$vf %in% "v2" & commcatch$reg %in% "s1"])
  st <- styear[styear$vf %in% "v2" & styear$reg %in% "s1" ,]
  kv <- kvyear[!is.na(match(kvyear$synis.id,st$synis.id)),]
  kv <- rbind(kv,kvallyearlesswt,tmpkv)
  le <- leyear[!is.na(match(leyear$synis.id,st$synis.id)),]
  le <- rbind(le,leallyearlesswt)
  cond <- cond.23
  keys <- MakeAlk(kv,TEG,kynth=F,lengd=LE,aldur=ALDUR,Stodvar=st,FilterAldurLengd=F)
  tmp3 <- MakeLdist(TEG,lengd=LE,Stodvar=st,lengdir=le,lengd.thyngd.data=cond,talid=F,afli=afli)
  cnoplaice$v2s1t0 <- Calc.fj(keys,tmp3)

# Síðari hluti árs.

# Fyrri hluti árs, allar kvarnir þar hafa vægið 0.01 í samsull
  tmpst  <- styear[styear$reg %in% "s1" & styear$time %in% "t2",]
  tmpkv <- kvyear[!is.na(match(kvyear$synis.id,tmpst$synis.id)),]
  if(nrow(tmpkv) > 0) tmpkv$fjoldi <- 0.01



  cat("dr jundes")

  afli <- sum(commcatch$catch[commcatch$ar==year & commcatch$vf %in% "v5" & commcatch$time %in% "t2" & commcatch$reg %in% "s1"])
  st <- styear[styear$vf %in% "v5" & styear$reg %in% "s1" & styear$time %in% "t2",]
  kv <- kvyear[!is.na(match(kvyear$synis.id,st$synis.id)),]
  kv <- rbind(kv,kvallyearlesswt,tmpkv)
  le <- leyear[!is.na(match(leyear$synis.id,st$synis.id)),]
  le <- rbind(le,leallyearlesswt)
  cond <- cond.23
  keys <- MakeAlk(kv,TEG,kynth=F,lengd=LE,aldur=ALDUR,Stodvar=st,FilterAldurLengd=F)
  tmp3 <- MakeLdist(TEG,lengd=LE,Stodvar=st,lengdir=le,lengd.thyngd.data=cond,talid=F,afli=afli)
  cnoplaice$v5s1t2 <- Calc.fj(keys,tmp3)


  cat("bv jundes")

  afli <- sum(commcatch$catch[commcatch$ar==year & commcatch$vf %in% "v6" & commcatch$time %in% "t2" & commcatch$reg %in% "s2"])
  st <- styear[styear$vf %in% "v6" & styear$reg %in% "s1" & styear$time %in% "t2",]
  kv <- kvyear[!is.na(match(kvyear$synis.id,st$synis.id)),]
  kv <- rbind(kv,kvallyearlesswt,tmpkv)
  le <- leyear[!is.na(match(leyear$synis.id,st$synis.id)),]
  le <- rbind(le,leallyearlesswt)
  cond <- cond.23
  keys <- MakeAlk(kv,TEG,kynth=F,lengd=LE,aldur=ALDUR,Stodvar=st,FilterAldurLengd=F)
  tmp3 <- MakeLdist(TEG,lengd=LE,Stodvar=st,lengdir=le,lengd.thyngd.data=cond,talid=F,afli=afli)
  cnoplaice$v6s1t2 <- Calc.fj(keys,tmp3)






  totcnoplaice <- cnoplaice[[1]]$FjPerAldur
  for(i in 2:length(cnoplaice)) totcnoplaice <- totcnoplaice+cnoplaice[[i]]$FjPerAldur

  tmp <- cnoplaice[[1]]
  for(i in 2:length(cnoplaice)) tmp <- husky:::rbind.alk1(tmp, cnoplaice[[i]])
  wtyear <- tmp$BiomassPerAldur/apply(tmp$FjPerAldur,2,sum)
  plyearcno <- data.frame(age=ALDUR,fj=c(totcnoplaice),wt=c(wtyear),bio=c(tmp$BiomassPerAldur))

  # Skala með hlutfalli heildarafla og þeirra sella sem voru teknar með. c.a 1.2% hækkun
  rat <- totcatch$catch[totcatch$year==year]/sum(plyearcno$bio)
  print(rat)

  plyearcno$fj <- plyearcno$fj*rat
  plyearcno$bio <- round(plyearcno$bio*rat/1000,1)
  plyearcno$fj <- round(plyearcno$fj,1)
  plyearcno$wt <- round(plyearcno$wt)
  names(plyearcno) <- c("age","cno","cwt","ctons")
  plyearcno$prosentno <- plyearcno$cno/sum(plyearcno$cno)*100
  plyearcno$prosentwt <- plyearcno$ctons/sum(plyearcno$ctons)*100
  plyearcno$year <- rep(year,nrow(plyearcno))
  result <- rbind(result,plyearcno)
}

result$yearclass <- result$year - result$age
plaicecno <- result
save(plaicecno, file = "data/plaicecno.rda")



