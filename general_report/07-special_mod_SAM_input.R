#This script is for including species-specific modifications to SAM input that was created
#with 04-create_SAM_input

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
