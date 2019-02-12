#This script is for including species-specific modifications to SAM input that was created
#with 04-create_SAM_input

cn     <- read_csv(paste0(yr_dir,'/','catage.csv'))
cw     <- read_csv(paste0(yr_dir,'/','catch_weights.csv'))
smb_n  <- read_csv(paste0(yr_dir,'/','smb_n.csv'))
smb_b  <- read_csv(paste0(yr_dir,'/','smb_b.csv'))
smh_n  <- read_csv(paste0(yr_dir,'/','smh_n.csv'))
smh_b  <- read_csv(paste0(yr_dir,'/','smh_b.csv'))
smb_sw <- read_csv(paste0(yr_dir,'/','smb_stock_weights.csv'))
smh_sw <- read_csv(paste0(yr_dir,'/','smh_stock_weights.csv'))
mat    <- read_csv(paste0(yr_dir,'/','maturity.csv'))

if(Species==2){

  cw <-
    cw %>% 
    (function(x) x/1000)

  smb_sw <-
    smb_sw %>% 
    (function(x) x/1000) %>% 
  ## add missing ages and years using catch weights
    (function(x){bind_rows(cw[1:3,],
                       x[-c(1:3),])})
  smh_sw <-
    smh_sw %>% 
    (function(x) x/1000) %>% 
    (function(x){bind_rows(smb_sw[1:14,],
                         x[-c(1:14),])})
  mat <- 
    mat %>% 
    mutate_all(~ifelse(. == -1, 1, .)) %>% 
    mutate('11' = ifelse('11' < 0.9, 1, '11'),
           '12' = ifelse('12' < 0.9, 1, '12'),
           '13' = ifelse('13' < 0.9, 1, '13'))
  
  
}

if(Species==9){
  
  #looks like ALK is not so good prior to 1988... going to use ALK from 1988 to fill in cn and cw data
  #its possible that length distributions may also not be good prior to 1988 but will see that later
  #under smb and smb_sw, 1990 data need to be filled in by 1989 data (ALK only? length too?)
  #or can 1990 survey data be missing? Probably just an ALK problem.
  #under smh and smh_sw, smb ALK need to be used for most years. (If length data also missing then skip but I think they should be available)
  #would be nice to have ALKs outputted from 05-catch_at_age script

  cw <-
    cw %>% 
    (function(x) x/1000)
  
  smb_sw <-
    smb_sw %>% 
    (function(x) x/1000) %>% 
    ## add missing ages and years using catch weights
    (function(x){bind_rows(cw[1:3,],
                           x[-c(1:3),])})
  smh_sw <-
    smh_sw %>% 
    (function(x) x/1000) %>% 
    (function(x){bind_rows(smb_sw[1:14,],
                           x[-c(1:14),])})
  mat <- 
    mat %>% 
    mutate_all(~ifelse(. == -1, 1, .)) %>% 
    mutate('11' = ifelse('11' < 0.9, 1, '11'),
           '12' = ifelse('12' < 0.9, 1, '12'),
           '13' = ifelse('13' < 0.9, 1, '13'))
  
  
}

#Convert all to SAM format

format_SAM <- function(x, 
                       fin_year = tyr + 1, 
                       smb = FALSE, 
                       smh = FALSE, 
                       add_ages = NULL){
  x <-
    x %>% 
    mutate_all(~ifelse(is.na(.),-1,.)) %>%  
    filter(Year < fin_year)
  
  mat <-
    x %>% 
    select(-Year) %>% 
    as.matrix()
  
  rownames(mat) <- x$Year
  
  if(smb){attributes(mat)$time <- c(0.15,0.2)}
  if(smh){attributes(mat)$time <- c(0.7,0.8)}
  if(!is.null(add_ages)){
    mat <- cbind(as_data_frame(add_ages, mat))
  }
  
  mat
}


cn <- format_SAM(cn) #format_SAM(., add_ages = list(`1`=0.001,`2`=0.001)) # could do this here or above
cw <- format_SAM(cw) #format_SAM(., add_ages = list(`1`=0,`2`=0)) #could do this here or above
smb_n <- format_SAM(smb_n, smb = TRUE)
smb_b <- format_SAM(smb_b, smb = TRUE)
smh_n <- format_SAM(smh_n, smh = TRUE)
smh_b <- format_SAM(smh_b, smh = TRUE)
smb_sw <- format_SAM(smb_sw)
smh_sw <- format_SAM(smh_sw)
mat <- format_SAM(mat)
