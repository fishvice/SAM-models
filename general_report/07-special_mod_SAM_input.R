#This script is for including species-specific modifications to SAM input that was created
#with 04-create_SAM_input

cn     <- read_csv(paste0(yr_dir,'/','catage.csv'))
cw     <- read_csv(paste0(yr_dir,'/','catch_weights.csv'))
if(30 %in% Index_Synaflokkur[[Species]]){
  smb_n  <- read_csv(paste0(yr_dir,'/','smb_n.csv'))
  smb_b  <- read_csv(paste0(yr_dir,'/','smb_b.csv'))
  smb_sw <- read_csv(paste0(yr_dir,'/','smb_stock_weights.csv'))
  smb_len<- read_csv(paste0(yr_dir,'/','smb_len.csv'))
}
if(35 %in% Index_Synaflokkur[[Species]]){
  smh_n  <- read_csv(paste0(yr_dir,'/','smh_n.csv'))
  smh_b  <- read_csv(paste0(yr_dir,'/','smh_b.csv'))
  smh_sw <- read_csv(paste0(yr_dir,'/','smh_stock_weights.csv'))
  smh_len    <- read_csv(paste0(yr_dir,'/','smh_len.csv'))
}
mat    <- read_csv(paste0(yr_dir,'/','maturity.csv'))

#cn, cw, mat, sw need to start from the beginning of the model (I think - based on haddock ex.)
#smb and smh can have missing age columns or -1 for missing year rows.

clean_weights <- function(x){
  #In words: if the 'weight at age' x is greater than 1.96*sd + mean of x (over years) after the minimum and maximum values have been removed, 
  #(OR conversely less than -1.96*sd + mean of x after min and max have been removed), 
  #OR it's a missing value (== -1),
  #then replace with the mean, after min and max have been removed
  #NOTE that when there are multiple min/max matches, only the first is removed
  neg1s <- x!= -1
    
  min_ind <- which.min(x[neg1s])
  max_ind <- which.max(x[neg1s])
  if(max(x) != tyr){x[neg1s][c(min_ind,max_ind)] <- -1
    
  x <- ifelse((x > mean(x[neg1s][-c(min_ind, max_ind)]) + 1.96*sd(x[neg1s][-c(min_ind, max_ind)])) |
              (x < mean(x[neg1s][-c(min_ind, max_ind)]) - 1.96*sd(x[neg1s][-c(min_ind, max_ind)])) |
              (x == -1),
         mean(x[neg1s][-c(min_ind, max_ind)]),
         x)
  } else {x}
  
}

#Convert all to SAM format

format_SAM <- function(x, 
                       fin_year = tyr + 1, 
                       first_year = 1900,
                       smb = FALSE, 
                       smh = FALSE, 
                       add_ages = NULL){
  x <-
    x %>% 
    #mutate_all(~ifelse(is.na(.),-1,.)) %>%  
    filter(Year < fin_year & Year >= first_year)
  
  mat <-
    x %>% 
    select(-Year) %>% 
    as.matrix()
  
  rownames(mat) <- x$Year
  
  if(smb){attributes(mat)$time <- c(0.15,0.2)}
  if(smh){attributes(mat)$time <- c(0.7,0.8)}
  if(!is.null(add_ages)){
    mat <- cbind(as_data_frame(add_ages, mat)) # for adding missing columns of age data
  }
  
  mat
}



if(Species==2){ #needs work to be like wolffish
  
  
  # cw <-
  #   cw %>% 
  #   (function(x) x/1000)

  cw <-
    cw %>% 
    mutate(`1` = ifelse(`1` > `2`, -1, `1`),
           `2` = ifelse(`2` > `3`, -1, `2`)) %>% #converting obviously wrong weights to NAs
    purrr::map(function(x) clean_weights(x)) %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    mutate(Year = Year*1000) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year)) 
  

  smb_sw <-
    smb_sw %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = Year*1000) %>% 
    ## add missing ages and years using catch weights
    (function(x){bind_rows(cw[cw$Year %in% c(1982:1984),],
                           x[!(x$Year %in% c(1982:1984)),])}) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year)) %>% 
    arrange(Year)
  
  
  smh_sw <-
    smh_sw %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year=Year*1000) %>% 
    (function(x){bind_rows(smb_sw[smb_sw$Year %in% c(1982:1995,2011),],
                           x[!(x$Year %in% c(1982:1995,2011)),])}) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year)) %>% 
    arrange(Year)
  
  
  
  
  # smb_sw <-
  #   smb_sw %>% 
  #   (function(x) x/1000) %>% 
  # ## add missing ages and years using catch weights
  #   (function(x){bind_rows(cw[1:3,],
  #                      x[-c(1:3),])})
  # smh_sw <-
  #   smh_sw %>% 
  #   (function(x) x/1000) %>% 
  #   (function(x){bind_rows(smb_sw[1:14,],
  #                        x[-c(1:14),])})
  mat <- 
    mat %>% 
    mutate_all(~ifelse(. == -1, 1, .)) #%>% 
    # mutate('11' = ifelse('11' < 0.9, 1, '11'),
    #        '12' = ifelse('12' < 0.9, 1, '12'),
    #        '13' = ifelse('13' < 0.9, 1, '13')
    #        )
  
  mat <- 
    mat %>% 
    mutate(`1` = ifelse(`1` == -1, 0, `1`),
           `2` = ifelse(`2` == -1, 0, `2`))
#           `3` = ifelse(`3` == -1, 0, `3`),
#           `4` = ifelse(`4` == -1, 0, `4`)) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) 
  
}

if(Species==9){
  
  #looks like ALK is not so good prior to 1988... going to use ALK from 1988 to fill in cn and cw data
  #length dist missting from 1986 and 1984; questionable from other years prior to 1988.
  #would be nice to have ALKs outputted from 05-catch_at_age script

  #weight data are weird for wolffish. some unreasonable values coming - cleaning with clean_weights function

  smb_n <-
    smb_n %>% 
    filter(Year > 1984) #%>% 
    #select(-c(`1`)) #should age 1 be excluded? Does it matter?
  
  smb_b <-
    smb_b %>% 
    filter(Year > 1984) #%>% 
    #select(-c(`1`)) #should age 1 be excluded? Does it matter?
  
  smh_n <-
    smh_n %>% 
    filter(Year > 1995) #%>% 
    #select(-c(`1`)) #should age 1 be excluded? Does it matter?
  
  smh_b <-
    smh_b %>% 
    filter(Year > 1995) #%>% 
    #select(-c(`1`)) #should age 1 be excluded? Does it matter?
    
  smh_n[smh_n$Year==2011,2:24] <- smh_b[smh_b$Year==2011,2:24] <- rep(-1,23)
  
  cw <-
    cw %>% 
    mutate(`1` = ifelse(`1` > `2`, -1, `1`),
           `2` = ifelse(`2` > `3`, -1, `2`)) %>% #converting obviously wrong weights to NAs
    purrr::map(function(x) clean_weights(x)) %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year*1000)) 
  
  
  smb_sw <-
    smb_sw %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = Year*1000) %>% 
    ## add missing ages and years using catch weights
    (function(x){bind_rows(cw[cw$Year %in% c(1982:1984),],
                           x[!(x$Year %in% c(1982:1984)),])}) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year)) %>% 
    arrange(Year)
    

  smh_sw <-
    smh_sw %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = Year*1000) %>% 
    (function(x){bind_rows(smb_sw[smb_sw$Year %in% c(1982:1995,2011),],
                           x[!(x$Year %in% c(1982:1995,2011)),])}) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year)) %>% 
    arrange(Year)
    
  mat <- 
    mat %>% 
    mutate(`1` = ifelse(`1` == -1, 0, `1`),
           `2` = ifelse(`2` == -1, 0, `2`),
           `3` = ifelse(`3` == -1, 0, `3`),
           `4` = ifelse(`4` == -1, 0, `4`),
           `23` = ifelse(`23` == -1, `22`, `23`)) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) 
    
    # mutate(`21` = ifelse(`21` == -1, 1, `21`),
    #        `22` = ifelse(`22` == -1, 1, `22`),
    #        `23` = ifelse(`23` == -1, 1, `23`))
  
  mat[mat$Year==1982,2:24] <- mat[mat$Year==1983,2:24] <- mat[mat$Year==1984,2:24] <- mat[mat$Year==1985,2:24]

  #Ásgeir says earlier than 2002 is unreliable maturity data; replacing these with 2002
  mat[mat$Year==1982,2:24] <- mat[mat$Year==1983,2:24] <- mat[mat$Year==1984,2:24] <- mat[mat$Year==1985,2:24] <- mat[mat$Year==1986,2:24] <-
    mat[mat$Year==1987,2:24] <- mat[mat$Year==1988,2:24] <- mat[mat$Year==1989,2:24] <- mat[mat$Year==1990,2:24] <- mat[mat$Year==1991,2:24] <-
    mat[mat$Year==1992,2:24] <- mat[mat$Year==1993,2:24] <- mat[mat$Year==1994,2:24] <- mat[mat$Year==1995,2:24] <- mat[mat$Year==1996,2:24] <-
    mat[mat$Year==1997,2:24] <- mat[mat$Year==1998,2:24] <- mat[mat$Year==1999,2:24] <- mat[mat$Year==2000,2:24] <- mat[mat$Year==2001,2:24] <-
    mat[mat$Year==2002,2:24]
  
  #Maturity data is squirrely in general. Perhaps better to fix maturity ogive and base maturity on length distribution
  mat2 <- mat
  len <- ifelse(is.na(smh_len[,2:24]), cbind(smb_len[,3:24], rep(NA, dim(smb_len)[1])), smh_len[,2:24]) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) 
  
  mat2 <- 
    len %>% 
    purrr::map(function(x) (1-1/(1 + exp(0.44*(x - 63))))) %>%
    bind_cols(x)
  
  cn_s <- format_SAM(cn, first_year = 1988) #format_SAM(., add_ages = list(`1`=0.001,`2`=0.001)) # could do this here or above
  cw_s <- format_SAM(cw, first_year = 1988) #format_SAM(., add_ages = list(`1`=0,`2`=0)) #could do this here or above
  smb_n_s <- format_SAM(smb_n, first_year = 1988, smb = TRUE)
  smb_b_s <- format_SAM(smb_b, first_year = 1988, smb = TRUE)
  smh_n_s <- format_SAM(smh_n, first_year = 1988, smh = TRUE)
  smh_b_s <- format_SAM(smh_b, first_year = 1988, smh = TRUE)
  smb_sw_s <- format_SAM(smb_sw, first_year = 1988) #these do not need timing changes right?
  smh_sw_s <- format_SAM(smh_sw, first_year = 1988)
  mat_s <- format_SAM(mat, first_year = 1988)
  
  
  ## weight length relationship
  lw.constants <- 
    mfdb_dplyr_sample(mdb) %>% 
    filter(species == defaults$species,
           sampling_type == 'IGFS',
           !is.na(weight),
           length > 0) %>% 
    select(length,weight) %>% 
    collect(n=Inf) %>% 
    lm(log(weight/1e3)~log(length),.) %>% 
    broom::tidy() %>% 
    select(estimate)
  ## transport back to right dimension
  lw.constants$estimate[1] <- exp(lw.constants$estimate[1])
  
  
  
  
  ## initial guess for the maturity ogive:
  mat.l50 <- 
    mfdb_dplyr_sample(mdb) %>% 
    filter(species == defaults$species,
           sampling_type == 'AUT',
           sex == 'F',
           !is.na(maturity_stage)) %>% 
    select(length,maturity_stage) %>% 
    group_by(length,maturity_stage) %>% 
    dplyr::summarise(n=n()) %>% 
    group_by(length) %>% 
    dplyr::mutate(p=n/sum(n, na.rm = T)) %>% 
    ungroup() %>% 
    filter(maturity_stage=='2',p>0.50,length>25) %>% 
    dplyr::summarise(l50=min(length)) %>% 
    collect(n=Inf)
  
  
}

if(Species==19){
  
  cn <- 
    cn %>% 
    purrr::map(function(x) ifelse(x == -1, x, x*1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year/1000))
  
  smh_n <-
    smh_n %>% 
    filter(Year > 1995) #%>% #not sure why earlier data not working right now...
  #select(-c(`1`)) #should age 1 be excluded? Does it matter?
  
  smh_b <-
    smh_b %>% 
    filter(Year > 1995) #%>% 
  #select(-c(`1`)) #should age 1 be excluded? Does it matter?
  nages <- tbl(mar,'age_minlength') %>% filter(species==19) %>% collect(n=Inf) %>% dim(.) %>% unlist %>% .[1]
  smh_n[smh_n$Year==2011,2:(nages+1)] <- smh_b[smh_b$Year==2011,2:(nages+1)] <- rep(-1,nages) #only partial data from cancelled survey
  

  
  cw <-
    cw %>% 
    mutate(`1` = ifelse(`1` >= `2`, -1, `1`),
           `2` = ifelse(`2` >= `3`, -1, `2`)) %>% #converting obviously wrong weights to NAs. But makes judgment that smaller is the wrong one.
    purrr::map(function(x) clean_weights(x)) %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year*1000)) #careful that years are not changed at this step
  

  smh_sw <-
    smh_sw %>% 
    purrr::map(function(x) ifelse(x == -1, x, x/1000)) %>% 
    bind_cols(.) %>% 
    mutate(Year = round(Year*1000)) %>% 
    (function(x){bind_rows(cw[cw$Year %in% c(1982:1999,2011),],
                           x[!(x$Year %in% c(1982:1999,2011)),])}) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) %>% 
    arrange(Year)
  
  mat <- 
    mat %>% 
    mutate(`17` = ifelse(`17` == -1, `16`, `17`),
           `18` = ifelse(`18` == -1, `17`, `18`),
           `19` = ifelse(`19` == -1, `18`, `19`),
           `20` = ifelse(`20` == -1, `19`, `20`),
           `21` = ifelse(`21` == -1, `20`, `21`),
           `22` = ifelse(`22` == -1, `21`, `22`),
           `23` = ifelse(`23` == -1, `22`, `23`),
           `24` = ifelse(`24` == -1, `23`, `24`),
           `25` = ifelse(`25` == -1, `24`, `25`)) %>% 
    purrr::map(function(x) clean_weights(x)) %>% 
    bind_cols(.) 
  


  cn_s <- format_SAM(cn) #format_SAM(., add_ages = list(`1`=0.001,`2`=0.001)) # could do this here or above
  cw_s <- format_SAM(cw, first_year = 1985) #format_SAM(., add_ages = list(`1`=0,`2`=0)) #could do this here or above
#  smb_n_s <- format_SAM(smb_n, first_year = 1985, smb = TRUE)
#  smb_b_s <- format_SAM(smb_b, first_year = 1985, smb = TRUE)
  smh_n_s <- format_SAM(smh_n[-c(1:4),], first_year = 1985, smh = TRUE)
  smh_b_s <- format_SAM(smh_b[-c(1:4),], first_year = 1985, smh = TRUE)
#  smb_sw_s <- format_SAM(smb_sw, first_year = 1985) #these do not need timing changes right?
  smh_sw_s <- format_SAM(smh_sw, first_year = 1985)
  mat_s <- format_SAM(mat, first_year = 1985)
  
}


