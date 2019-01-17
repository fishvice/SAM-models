
load(paste0(tyr, "_", Species,"_catch_at_age.rdata"))

create_previous_years<-FALSE #TRUE if model is run for first time and previous years data needs to be filled in

if(create_previous_years){
  
  if (file.exists("first_run")){
    stop('first_run folder already exists. Are you sure you want to rewrite the first run? This 
         may destroy any documents of previous year assessments because
         data taken from queries can change over time. If you REALLY want to, 
         then delete this folder first.')
  } else {
    dir.create(file.path(getwd(), "first_run"))
  }
  
  tyr_current <- tyr
  catch_by_age_all <- catch_by_age
  
  for(i in (tyr_current - 1):1985){
    tyr <- i
    source('03-catch_at_age')
    catch_by_age_all <- bind_rows(catch_by_age_all, catch_by_age) 
  }
  
  cn <-
    catch_by_age_all %>% 
    select(age, year, ctons) %>% 
    spread(key = age, value = ctons) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    as.matrix()
  row.names(cn)<-cn[,1]
  write.csv(cn[,-1], 'first_run/catage.csv')
  
  cw <-
    catch_by_age_all %>% 
    select(age, year, cwt) %>% 
    spread(key = age, value = cwt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    as.matrix()
  row.names(cw)<-cw[,1]
  write.csv(cw[,-1], 'first_run/catch_weights.csv')

  smb <-
    catch_by_age_all %>% 
    select(age, year, s2_sno) %>% 
    spread(key = age, value = s2_sno) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    as.matrix()
  row.names(smb)<-smb[,1]
  write.csv(smb[,-1], 'first_run/smb.csv')

  smh <-
    catch_by_age_all %>% 
    select(age, year, s3_sno) %>% 
    spread(key = age, value = s3_sno) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    as.matrix()
  row.names(smh)<-smh[,1]
  write.csv(smh[,-1], 'first_run/smh.csv')

  sw <-
    catch_by_age_all %>% 
    select(age, year, s2_swt) %>% 
    spread(key = age, value = s2_swt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    as.matrix()
  row.names(sw)<-sw[,1]
  write.csv(sw[,-1], 'first_run/stock_weights.csv')
  
  #ADD MATURITY HERE
  
  tyr <- tyr_current
}

#NOW READ IN FILES AND ADD THIS YEAR'S DATA AFTER WARNING CHECK TO MAKE SURE YEARS LINE UP
## Catch at age 
cn <- 
  read_csv('https://data.hafro.is/assmt/2018/cod/catage.csv') %>% 
  tmp_fun() %>% 
  ## append missing ages with nominal catches
  cbind(`1`=0.001,`2`=0.001,.)


## Catch weight at age - are these individual or total?
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
