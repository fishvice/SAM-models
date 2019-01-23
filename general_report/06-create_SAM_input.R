
load(paste0(yr_dir, '/', 'catch_at_age_rdata', '/', tyr, "_", Species,"_catch_at_age.rdata"))

create_previous_years<-TRUE #TRUE if model is run for first time and previous years data needs to be filled in

if(create_previous_years){
  
  dir.create(paste0(as.numeric(yr_dir)-1))
  tyr_current <- tyr
  catch_by_age_current <- catch_by_age
  
  #initialize with first previous year
  tyr <- tyr - 1
  print(paste0('beginning ', tyr))
  source('../general_report/03-catch_at_age.R')
  catch_by_age_all <- catch_by_age
  
  #This will take a while
  for(i in (tyr_current-2):1982){
    tyr <- i
    print(paste0('beginning ', tyr))
    source('../general_report/03-catch_at_age.R')
    catch_by_age_all <- bind_rows(catch_by_age_all, catch_by_age) 
  } 
  
  cn <-
    catch_by_age_all %>% 
    select(age, year, ctons) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = ctons) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(cn, paste0(paste0(as.numeric(yr_dir)-1),'/','catage.csv'), row.names = F)
  
  cw <-
    catch_by_age_all %>% 
    select(age, year, cwt) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = cwt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(cw, paste0(paste0(as.numeric(yr_dir)-1),'/','catch_weights.csv'), row.names = F)

  smb_n <-
    catch_by_age_all %>% 
    select(age, year, s2_sno) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = s2_sno) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smb_n, paste0(paste0(as.numeric(yr_dir)-1),'/','smb_n.csv'), row.names = F)

  smb_b <-
    catch_by_age_all %>% 
    select(age, year, s2_sbio) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = s2_sbio) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smb_b, paste0(paste0(as.numeric(yr_dir)-1),'/','smb_b.csv'), row.names = F)
  
  smh_n <-
    catch_by_age_all %>% 
    select(age, year, s3_sno) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = s3_sno) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smh_n, paste0(paste0(as.numeric(yr_dir)-1),'/','smh_n.csv'), row.names = F)

  smh_b <-
    catch_by_age_all %>% 
    select(age, year, s3_sbio) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = s3_sbio) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smh_b, paste0(paste0(as.numeric(yr_dir)-1),'/','smh_b.csv'), row.names = F)
  
  smb_sw <-
    catch_by_age_all %>% 
    select(age, year, s2_swt) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = s2_swt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smb_sw, paste0(paste0(as.numeric(yr_dir)-1),'/','smb_stock_weights.csv'), row.names = F)

  smh_sw <-
    catch_by_age_all %>% 
    select(age, year, s3_swt) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = s3_swt) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(smh_sw, paste0(paste0(as.numeric(yr_dir)-1),'/','smh_stock_weights.csv'), row.names = F)
  
  #mat created from all data sources
  mat <-
    catch_by_age_all %>% 
    select(age, year, mat) %>% #.[-c(1:13),] %>% 
    spread(key = age, value = mat) %>% 
    mutate_all(~ifelse(is.na(.), -1, .)) %>% 
    rename(Year = year)
  write.csv(mat, paste0(paste0(as.numeric(yr_dir)-1),'/','maturity.csv'), row.names = F)
  
  tyr <- tyr_current
  catch_by_age <- catch_by_age_current
  
  #why isn't this working?
  dbWriteTable(mar, name = paste0(Species, "_previous_years"), value = catch_by_age_all )
  
  
} 
  
## Catch at age 
cn <- 
  #read_csv('https://data.hafro.is/assmt/2018/cod/catage.csv') %>% 
  read_csv(paste0(yr_dir,'/','catage.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, cno) %>% 
              spread(key = age, value = cno) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
              ) 



## Catch weight at age
cw <- 
  read_csv(paste0(yr_dir,'/','catch_weights.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, cwt) %>% 
              spread(key = age, value = cwt) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) 

## Read the spring survey numbers
smb_n <- 
  read_csv(paste0(yr_dir,'/','smb_n.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s2_sno) %>% 
              spread(key = age, value = s2_sno) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) 

## Read the spring survey biomass

smb_b <- 
  read_csv(paste0(yr_dir,'/','smb_b.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s2_sbio) %>% 
              spread(key = age, value = s2_sbio) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) 



## Autumn survey numbers
smh_n <- 
  read_csv(paste0(yr_dir,'/','smh_n.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s3_sno) %>% 
              spread(key = age, value = s3_sno) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) 

## Autumn survey biomass
smh_b <- 
  read_csv(paste0(yr_dir,'/','smh_b.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s3_sbio) %>% 
              spread(key = age, value = s3_sbio) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) 


## Stock weights from spring survey
smb_sw <- 
  read_csv(paste0(yr_dir,'/','smb_stock_weights.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s2_swt) %>% 
              spread(key = age, value = s2_swt) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  )
  
  ## Stock weights from autumn survey
smh_sw <- 
  read_csv(paste0(yr_dir,'/','smh_stock_weights.csv')) %>% 
  #set_names(., c('year', as.character(catch_by_age$age %>% unique))) %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, s3_swt) %>% 
              spread(key = age, value = s3_swt) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) 


## Maturity at age
mat <- 
  read_csv(paste0(yr_dir,'/','maturity.csv')) %>% 
  #read_csv('https://data.hafro.is/assmt/2018/cod/maturity.csv')  %>% 
  bind_rows(catch_by_age %>% 
              select(age, Year = year, mat) %>% 
              spread(key = age, value = mat) %>% 
              mutate_all(~ifelse(is.na(.), -1, .))  
  ) 

