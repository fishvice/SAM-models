
load(paste0(res_dir, '/', 'catch_at_age_rdata', '/', tyr, "_", Species,"_catch_at_age.rdata"))

create_previous_years<-TRUE #TRUE if model is run for first time and previous years data needs to be filled in

if(create_previous_years){
  
  tyr_current <- tyr
  catch_by_age_all <- catch_by_age
  
  #This will take a while
  for(i in (tyr_current-1):1950){
    tyr <- i
    print(paste0('beginning ', tyr))
    source('../general_report/03-catch_at_age.R')
    catch_by_age_all <- bind_rows(catch_by_age_all, catch_by_age) 
  } 
  
  cn <-
    catch_by_age_all %>% 
    select(age, year, ctons) %>% 
    spread(key = age, value = ctons) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(cn, paste0(res_dir,'/','catage.csv'), row.names = F)
  
  cw <-
    catch_by_age_all %>% 
    select(age, year, cwt) %>% 
    spread(key = age, value = cwt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(cw, paste0(res_dir,'/','catch_weights.csv'), row.names = F)

  smb_n <-
    catch_by_age_all %>% 
    select(age, year, s2_sno) %>% 
    spread(key = age, value = s2_sno) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smb_n, paste0(res_dir,'/','smb_n.csv'), row.names = F)

  smb_b <-
    catch_by_age_all %>% 
    select(age, year, s2_sbio) %>% 
    spread(key = age, value = s2_sbio) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smb_b, paste0(res_dir,'/','smb_b.csv'), row.names = F)
  
  smh_n <-
    catch_by_age_all %>% 
    select(age, year, s3_sno) %>% 
    spread(key = age, value = s3_sno) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smh_n, paste0(res_dir,'/','smh_n.csv'), row.names = F)

  smh_b <-
    catch_by_age_all %>% 
    select(age, year, s3_sbio) %>% 
    spread(key = age, value = s3_sbio) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smh_b, paste0(res_dir,'/','smh_b.csv'), row.names = F)
  
  smb_sw <-
    catch_by_age_all %>% 
    select(age, year, s2_swt) %>% 
    spread(key = age, value = s2_swt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smb_sw, paste0(res_dir,'/','smb_stock_weights.csv'), row.names = F)

  smh_sw <-
    catch_by_age_all %>% 
    select(age, year, s3_swt) %>% 
    spread(key = age, value = s3_swt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smh_sw, paste0(res_dir,'/','smh_stock_weights.csv'), row.names = F)
  
  #mat created from all data sources
  mat <-
    catch_by_age_all %>% 
    select(age, year, mat) %>% 
    spread(key = age, value = mat) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(mat, paste0(res_dir,'/','maturity.csv'), row.names = F)
  
  tyr <- tyr_current
  
} else {


## Catch at age 
cn <- 
  #read_csv('https://data.hafro.is/assmt/2018/cod/catage.csv') %>% 
  read_csv(paste0(res_dir,'/','catage.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, cno) %>% 
              spread(key = age, value = cno) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
              ) %>%  
  tmp_fun() #%>% #will put cbind options as argument to tmp_fun
  #arrange(year) %>% 
  #data.frame(., row.names = 1) %>% 
  #set_names(., c(as.character(catch_by_age$age %>% unique)))
  
  
  #appending to be replaced with function
  #tmp_fun() %>% 
  ## append missing ages with nominal catches
  #cbind(`1`=0.001,`2`=0.001,.) 


## Catch weight at age - are these individual or total?
cw <- 
  read_csv(paste0(res_dir,'/','catch_weights.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, cw) %>% 
              spread(key = age, value = cw) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) %>%  
  tmp_fun() %>% #will put cbind options as argument to tmp_fun
  #arrange(year) %>% 
  #data.frame(., row.names = 1) %>% 
  #set_names(., c(as.character(catch_by_age$age %>% unique)))
#read_csv('https://data.hafro.is/assmt/2018/cod/catch_weights.csv')%>% 
  #tmp_fun() %>% 
  #cbind(`1`=0,`2`=0,.) %>% 
  (function(x) x/1000)


## Read the spring survey data
smb <- 
  read_csv(paste0(res_dir,'/','smb.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s2_sno) %>% 
              spread(key = age, value = s2_sno) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) %>%  
  #arrange(year) %>% 
  #data.frame(., row.names = 1) %>% 
  #set_names(., c(as.character(catch_by_age$age %>% unique)))
#read_csv('https://data.hafro.is/assmt/2018/cod/smb.csv')  %>% 
  tmp_fun() #will put cbind options as argument to tmp_fun
## set the time window of the survey
attributes(smb)$time <- c(0.15,0.2)

## Autumn survey
smh <- 
  read_csv(paste0(res_dir,'/','smh.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s3_sno) %>% 
              spread(key = age, value = s3_sno) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) %>%  
  #arrange(year) %>% 
  #data.frame(., row.names = 1) %>% 
  #set_names(., c(as.character(catch_by_age$age %>% unique)))

  #read_csv('https://data.hafro.is/assmt/2018/cod/smh.csv')  %>% 
  tmp_fun() #will put cbind options as argument to tmp_fun
attributes(smh)$time <- c(0.7,0.8)



## Stock weights from spring survey
sw <- 
  read_csv(paste0(res_dir,'/','stock_weights.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s2_swt) %>% 
              spread(key = age, value = s2_swt) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) %>%  
  #arrange(year) %>% 
  #data.frame(., row.names = 1) %>% 
  #set_names(., c(as.character(catch_by_age$age %>% unique)))

#read_csv('https://data.hafro.is/assmt/2018/cod/smb_weights.csv') %>% 
  tmp_fun() #%>%  will put below as argument to tmp_fun
  ## add missing ages and years using catch weights
  #(function(x){rbind(cw[1:30,],
  #                   cbind(x,cw[-c(1:30),-c(1:9)]))
  #})


## Maturity at age
mo <- 
  read_csv(paste0(res_dir,'/','maturity.csv')) %>% 
  #read_csv('https://data.hafro.is/assmt/2018/cod/maturity.csv')  %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, mat) %>% 
              spread(key = age, value = mat) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) %>%  
  tmp_fun() #%>% #will put cbind options as argument to tmp_fun
  #cbind(`1`=0,`2`=0,.)
}