library(mar)
library(fjolst)
library(husky)
tyr <- 2017
mar <- connect_mar()

husky::gearlist %>% 
  dbWriteTable(mar,'husky_gearlist',.,overwrite =TRUE)

foreigncatch <- 0 

cond.02 <- 
  expand.grid(gear = c("v1", "v2", "v3", "v5", "v6", "v9"), 
              time = c("t1", "t2"), 
              reg = 's1',#c("s1", "s2"),
              stringsAsFactors = FALSE) %>% 
  unite(vst,c(gear,reg,time),sep = '',remove = FALSE) %>% 
  mutate(condition = 0.00885, power = 3.02857)

TEG <- 2
ALDUR <- 2:14
LE <- c(seq(22.5,77.5,by=5),87.5)

st <- 
  lesa_stodvar(mar) %>% 
  mutate(GRIDCELL = 10*reitur + smareitur) %>% 
  filter(ar == tyr) %>% 
  inner_join(tbl(mar,'husky_gearlist')) %>% 
  rename(vf = geartext) %>% 
  inner_join(tbl(mar,'reitmapping')) %>% 
  rename(area = DIVISION) %>% 
  filter(area %in% 101:108) %>% 
  mutate(reg = 's1',
         time = ifelse(man > 5,'t2','t1')) %>% 
  mutate(index = concat(concat(vf,reg),time))

#st2016 <- st2016[st2016$synaflokkur %in% c(1,2,4,8),]
#st2016 <- inside.reg.bcbreytt(st2016,option=2)

#st2016 <- geo::inside.reg.bc(st2016)
#st2016 <- st2016[st2016$area %in% 1:10,]
#st2016 <- husky::calcgear(st2016)

minl <- 
  data.frame(aldur=1:14,minl=15+2.5*(1:14)) %>% 
  dbWriteTable(mar,'minl_lookup',.,overwrite=TRUE)

kv <- 
  lesa_kvarnir(mar) %>% 
  filter(tegund == TEG) %>% 
  semi_join(st) %>% 
  select(synis_id,lengd,aldur,kyn,kynthroski,slaegt,oslaegt) %>% 
  filter(!is.na(aldur)) %>% 
  left_join(tbl(mar,'minl_lookup')) %>% 
  filter(lengd>minl) %>% 
  mutate(fjoldi =1) 

#kv2016all <- fjolst::lesa.kvarnir(st2016$synis.id, TEG, c("kyn", "kynthroski","slaegt","oslaegt"),oracle=F)
#kv2016 <- kv2016all[!is.na(kv2016all$aldur),  ]
#minl <- data.frame(aldur=1:14,minl=15+2.5*(1:14))
#kv2016 <- fjolst:::join(kv2016,minl,"aldur")

#i <- kv2016$lengd > kv2016$minl
#kv2016 <- kv2016[i,] # villur
# kv2016$fjoldi <- rep(1,nrow(kv2016))

le <- 
  lesa_lengdir(mar) %>% 
  semi_join(st) %>% 
  filter(tegund == TEG) 

nu <- 
  lesa_numer(mar) %>% 
  semi_join(st) %>% 
  filter(tegund == TEG) 

#le2016 <- fjolst::lesa.lengdir(st2016$synis.id, TEG)
#nu2016 <- fjolst::lesa.numer(st2016$synis.id, TEG)
#st2016$reg <- rep("s1",nrow(st2016)) 
#st2016$time <- rep("t1",nrow(st2016))
#i <- st2016$man > 5;
#st2016$time[i] <- "t2"
#st2016$index <- paste(st2016$vf,st2016$reg,st2016$time,sep="")
#st2016.kvarnad <- st2016[!is.na(match(st2016$synis.id,kv2016all$synis.id)),]
#st2016kv <- st2016[!is.na(match(st2016$synis.id,kv2016$synis.id)),]
#print(table(st2016kv$reg,st2016kv$time,st2016kv$vf))
#print(table(st2016$reg,st2016$time,st2016$vf))


# Færeysk og norsk skip í löndunum nota ekki
#  utlskip <- skipaskra[skipaskra$einkst %in% c("FO","NO"),]
# Lina
landings <- 
  lods_oslaegt(mar) %>% 
  inner_join(tbl(mar,'husky_gearlist')) %>% 
  rename(vf = geartext) %>% 
  filter(#veidarfaeri == 1,
    fteg == TEG,
    ar == tyr,
    veidisvaedi == 'I') %>% 
  group_by(vf) %>% 
  summarise(landings = sum(magn_oslaegt)) 

catch <- 
  afli_stofn(mar) %>% 
  inner_join(afli_afli(mar)) %>% 
  filter(tegund == TEG, ar == tyr) %>% 
  mutate(GRIDCELL = 10*reitur + smareitur) %>% 
  inner_join(tbl(mar,'husky_gearlist'),
             by =c('veidarf'='veidarfaeri')) %>% 
  rename(vf = geartext) %>% 
  inner_join(tbl(mar,'reitmapping')) %>% 
  rename(area = DIVISION) %>% 
  filter(area %in% 101:108) %>% 
  mutate(reg = 's1',
         time = ifelse(man > 5,'t2','t1')) %>% 
  mutate(index = concat(concat(vf,reg),time)) 

sc <- 
  catch %>% 
  group_by(vf) %>% 
  summarise(catch = sum(afli)) %>% 
  left_join(landings)

commcatch <- 
  catch %>% 
  left_join(sc) %>% 
  mutate(afli = afli*landings/catch) %>% 
  group_by(index,vf,time,reg) %>% 
  summarise(catch = sum(afli,na.rm=TRUE)) %>% 
  collect(n=Inf) %>% 
  unite(index,c(vf,reg,time),sep = '',remove = FALSE) 

totcatch <- 
  landings <- 
  lods_oslaegt(mar) %>% 
  filter(#veidarfaeri == 1,
    fteg == TEG,
    ar == tyr,
    veidisvaedi == 'I') %>% 
  summarise(landings = sum(magn_oslaegt,na.rm=TRUE)) %>% 
  collect(n=Inf) %>% 
  .$landings

st_c <- st %>% collect(n=Inf)   
kv_c <- kv %>% collect(n=Inf)
le_c <- le %>% collect(n=Inf)  

## total age measurements have a really small weight
tmpkv2016all <- 
  kv_c %>% 
  mutate(fjoldi = 0.001)

tmp2016 <- 
  st_c %>% 
  filter(synis_id %in% c(le_c$synis_id,kv_c$synis_id))


## Southern area has a small weight


calc_all <- function(ind){
    if(str_sub(ind,start=6) == '1'){
        per <- 1:5
    } else {
        per <- 6:12
    }

    tmp2016a  <- 
        tmp2016 %>% 
        filter(area %in% 102:105,
               man %in% per)
    tmpkv2016a <- 
        kv_c %>% 
        semi_join(tmp2016a) %>% 
        mutate(fjoldi = 0.01)
    



    tmpafli <- 
    commcatch %>% 
    ungroup() %>% 
    filter(index == ind) %>% 
    mutate(catch = catch/1000) %>% 
    .$catch
  
  st_tmp <- 
    tmp2016 %>% 
    filter(index == ind)
  
  kv_tmp <- 
    kv_c %>% 
    semi_join(st_tmp) %>% 
    mutate(fjoldi = 1) %>% 
    bind_rows(tmpkv2016all) %>% 
    bind_rows(tmpkv2016a) %>% 
    rename(synis.id=synis_id)
  
  le_tmp <- 
    le_c %>% 
    semi_join(st_tmp) %>% 
    rename(synis.id = synis_id)
  
 tmp2016 <- 
    tmp2016 %>% 
    rename(synis.id = synis_id)
  
  cond <- 
    cond.02 %>% 
    #filter(vst == "v1s1t1") %>% 
    filter(vst == ind) %>% 
    select(condition,power) %>% 
    unlist()
  
  
  keys <- MakeAlk(kv_tmp,TEG,kynth=F,
                  lengd=LE,aldur=ALDUR,
                  Stodvar=tmp2016,
                  FilterAldurLengd=FALSE)
  tmp3 <- MakeLdist(TEG,lengd=LE,
                    Stodvar=tmp2016,
                    lengdir=le_tmp,
                    lengd.thyngd.data=cond,
                    talid=F,afli=tmpafli)
  #fj2016allirsynaflokkar1reg$v1s1t1 <- Calc.fj(keys,tmp3)
  return(Calc.fj(keys,tmp3))
}

fj2016allirsynaflokkar1reg <- 
  cond.02$vst %>% 
  set_names(.,.) %>% 
  purrr::map(calc_all)




#tmp2016a  <- tmp2016[!is.na(match(tmp2016$reg,"s1")) & !is.na(match(tmp2016$man,6:12)),]
#tmpkv2016a <- kv2016[!is.na(match(kv2016$synis.id,tmp2016a$synis.id)),]
#tmpkv2016a$fjoldi <- rep(0.01,nrow(tmpkv2016a))

totfj2016allirsynaflokkar1reg <- 
  fj2016allirsynaflokkar1reg %>% 
  purrr::map('FjPerAldur') %>% 
  purrr::map(unlist) %>% 
  purrr::map(t) %>% 
  purrr::map(as.data.frame) %>%
  purrr::map(~mutate(.,age = rownames(.))) %>% 
  bind_rows(.id = 'part') %>% 
  as_data_frame() %>% 
  group_by(age) %>% 
  summarise(total = sum(V1)) %>% 
  mutate(age = as.numeric(age)) %>% 
  arrange(age)


totfj2016allirsynaflokkar1reg <- fj2016allirsynaflokkar1reg[[1]]$FjPerAldur        
for(i in 2:length(fj2016allirsynaflokkar1reg)) 
   totfj2016allirsynaflokkar1reg <- totfj2016allirsynaflokkar1reg+fj2016allirsynaflokkar1reg[[i]]$FjPerAldur  

tmp <- fj2016allirsynaflokkar1reg[[1]] 
for(i in 2:length(fj2016allirsynaflokkar1reg))
  tmp <- husky:::rbind.alk1(tmp, fj2016allirsynaflokkar1reg[[i]]) 
wt2016 <- tmp$BiomassPerAldur/apply(tmp$FjPerAldur,2,sum)
catch2016allirsynaflokkar1reg <- 
  data.frame(age=ALDUR,
             fj=c(totfj2016allirsynaflokkar1reg),
             wt=c(wt2016),
             bio=c(tmp$BiomassPerAldur))

# Skala með hlutfalli heildarafla og þeirra sella sem voru teknar með. c.a 1.2% hækkun
rat <- totcatch/sum(catch2016allirsynaflokkar1reg$bio)
catch2016allirsynaflokkar1reg$fj <- 
  catch2016allirsynaflokkar1reg$fj*rat/1000
catch2016allirsynaflokkar1reg$bio <- 
  round(catch2016allirsynaflokkar1reg$bio*rat/1000,1)
#catch2016allirsynaflokkar1reg$fj <- 
#  round(catch2016allirsynaflokkar1reg$fj,3)
#catch2016allirsynaflokkar1reg$wt <- 
#  round(catch2016allirsynaflokkar1reg$wt)
names(catch2016allirsynaflokkar1reg) <- c("age","cno","cwt","ctons")
catch2016allirsynaflokkar1reg$prosentno <- 
  catch2016allirsynaflokkar1reg$cno/sum(catch2016allirsynaflokkar1reg$cno)*100
catch2016allirsynaflokkar1reg$prosentwt <- 
  catch2016allirsynaflokkar1reg$ctons/sum(catch2016allirsynaflokkar1reg$ctons)*100



catch <- rep(0,length(fj2016allirsynaflokkar1reg)) 
for(i in 1:length(fj2016allirsynaflokkar1reg)) 
  catch[[i]] <- sum(fj2016allirsynaflokkar1reg[[i]]$BiomassPerAldur)
catch <- data.frame(index=names(fj2016allirsynaflokkar1reg),catch=round(catch/1000))

# I don't think this is used because st2016kv and st2016.kvarnad are commented out earlier
#x <- geo::apply.shrink(rep(1,nrow(st2016kv)),st2016kv$index,sum,names=c("index","fj.aged"))
#x1 <- geo::apply.shrink(rep(1,nrow(st2016.kvarnad)),st2016.kvarnad$index,sum,names=c("index","fj.othsamp"))
#catch <- fjolst:::join(catch,x,"index")
#catch <- fjolst:::join(catch,x1,"index")
#catch$index <- as.character(catch$index)


tmp <- read.table("/home/hoski/Tac2016/02/Ass/Adapt/PrognosisBaediroll/resultsbyyearandage",header=T)
tmp <- tmp[tmp$year==2016 & tmp$age > 1,c("age","CalcCno","CatchWts")]
names(tmp) <- c("age","predcno","predwts")
tmp$predcno <- tmp$predcno
tmp$predpercno <- round(tmp$predcno/sum(tmp$predcno)*100,1)
catch2016allirsynaflokkar1reg <- fjolst:::join( catch2016allirsynaflokkar1reg,tmp,"age")
tmp <- round(catch2016allirsynaflokkar1reg,2)

save(list=c("catch2016allirsynaflokkar1reg","fj2016allirsynaflokkar1reg"),file="cno2016allirsynaflokkar.rdata")
