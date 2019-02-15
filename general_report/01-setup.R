# devtools::install_github('mareframe/mfdb',ref='6.x')
#devtools::install_github("fishfollower/SAM/stockassessment")
#devtools::install_github("fishvice/husky", dependencies = FALSE) # A temporary measure
#devtools::install_github("fishvice/pax", dependencies = FALSE)
library(tidyverse)
library(fjolst)    
library(pax)
library(mar)
library(knitr)
library(kableExtra)
library(patchwork)
library(forcats)
library(purrrlyr)
library(husky)
library(ROracle)
library(mfdb)
library(broom)
library(tidyverse)
library(mar)
library(purrrlyr)
library(dbplyr)
library(patchwork)

mar <- connect_mar()

recreate_helper_tables <- FALSE #helper tables should only need to be created once

# ------------------------------------------------------------------------------
# Variable stuff

sp_name <- 
  lesa_tegundir(mar) %>% 
  filter(tegund==Species) %>% 
  select(enskt_heiti) %>% collect() %>% 
  unlist

#set up directories
setwd('/home/pamela/Documents/Hafro/fishvice/SAM-models')
sp_dir<-paste0(Species, ' - ', sp_name)
dir.create(file.path(getwd(), sp_dir))
setwd(file.path(getwd(), sp_dir)) #should work for windows?
yr_dir<-as.character(tyr)
dir.create(file.path(getwd(), yr_dir))

#Set species-specific parameters (done for haddock so far)

#Basic dimensions for ALK 
#age and min length possible for that age
data.frame(age=1:14,minlength=15+2.5*(1:14), species=2) %>%
  bind_rows(data.frame(age = 1:23, minlength = 5+2*(1:23), species=9)) %>% 
  dbWriteTable(mar,'age_minlength',.,overwrite=TRUE)

ldist_bins <- NULL
ldist_bins[[2]] <- c(seq(22.5,77.5,by=5),87.5)
ldist_bins[[9]] <- c(seq(10, 100,by=5),120)

ALKyr_catch <- ALKyr_surv <- LEyr_catch <- LEyr_surv <-1900 #placeholder reference year to fill in good ALK or length data for catch at age or surv
use_alk_catch <- use_alk_surv <- use_le_catch <- use_le_surv <- FALSE



Index_Synaflokkur <- Index_Tognumer <- NULL
Index_Synaflokkur[[1]] <- Index_Synaflokkur[[2]] <- Index_Synaflokkur[[9]] <- c(30,35)         #for including which surveys to make indices from
Index_Tognumer[[1]] <- Index_Tognumer[[2]] <- Index_Tognumer[[9]] <- list(1:39, 1:75)       #and corresponding townumbers - ARE THESE RIGHT?

Length.min <- Length.max <- NULL
Length.min[[1]] <- Length.min[[2]] <- Length.min[[9]] <- 5                   # Minimum length for indices calculation
Length.max[[1]] <- Length.max[[2]] <- Length.max[[9]] <- 500                 # Maximum length for indices calculation

#for script 03-indices4plot.R
#cutoffs for indices in the 4 corners of the 4-plot
#this is coded for looping over multiple species
cutoffs_list<-NULL
cutoffs_list[[2]]<-list(total = c( 5,500), juv = c(0,30), B40 = c(40,500), B60=c(60,500)) #haddock
cutoffs_list[[6]]<-list(total = c(10,500), juv = c(0,40), B40 = c(40,500), B80=c(80,500)) #ling
cutoffs_list[[8]]<-list(total = c( 5,500), juv = c(0,30), B40 = c(40,500), B60=c(60,500)) #tusk
cutoffs_list[[9]]<-list(total = c( 5,500), juv = c(0,40), B60 = c(60,500), B79=c(79,500)) #wolffish
cutoffs<-cutoffs_list[[Species]] 


# ------------------------------------------------------------------------------
# Constants
std.cv        <- 1             # The cv if only one station in a strata - where is this used?
std.towlength <- 4             # Standard tow length for SMB is 4 nautical miles
std.width     <- 17 / 1852     # Standard sweep width in nautical miles

min.towlength <- 2             # Minimum "acceptable" towlength
max.towlength <- 8             # Maximum "acceptable" towlength


if(recreate_helper_tables){ #these should be stable every year


#these only need to be run once then can be commented out
dbRemoveTable(mar,'husky_gearlist')
#dbRemoveTable(mar,'reitmapping_original')
dbRemoveTable(mar,'OLDSTRATAAREA')
dbRemoveTable(mar,'NEWSTRATAAREA')
dbRemoveTable(mar,'SMBSTATIONSSTRATA')
dbRemoveTable(mar,'SMHSTATIONSSTRATA')

# ------------------------------------------------------------------------------
###--------COMPILE STRATA-----###

###---- Old strata ----###
attach("/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/OldStratas/.RData")
d <- 
  data_frame(oldstrata = attributes(STRATAS)$names %>% as.integer(),
             name = attributes(STRATAS)$names,
             area = attributes(STRATAS)$area,
             rall.area = attributes(STRATAS)$rall.area)
dbWriteTable(mar, name = "OLDSTRATAAREA", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/OldStratas/.RData")
# sql: grant select on oldstrataarea to h_fiskar_skoda

###---- New strata ----###
attach("/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/NewStratas/.RData")
d <- 
  data_frame(newstrata = 1:length(attributes(STRATAS)$names),
             name = attributes(STRATAS)$names,
             area = attributes(STRATAS)$area,
             rall.area = attributes(STRATAS)$rall.area)
dbWriteTable(mar, name = "NEWSTRATAAREA", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/NewStratas/.RData")
# sql: grant select on newstrataarea to h_fiskar_skoda

###---- SMB ----###
attach("/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/SMB/Stations.rdata")
d <- 
  STODVAR.all %>%
  select(synis_id = synis.id, oldstrata, newstrata)
dbWriteTable(mar, name = "SMBSTATIONSSTRATA", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/SMB/Stations.rdata")
# sql: grant select on smbstationsstrata to h_fiskar_skoda


#NOTE: In 2018 the stations were fixed to a strata, thus the above is since then replaced with the following:
#Above needs STODVAR.all updated every year
#below may be cleaner
#
# INDEX_STRATA <-
#   newstratas %>% 
#   full_join(oldstratas) %>% 
#   separate(index, c("reitur", "tognumer"), 3, marvert = TRUE) %>% 
#   mutate(veidarfaeri = 73)
# dbWriteTable(mar, name = "SMB_INDEX_STRATA", value = SMB_INDEX_STRATA, overwrite = TRUE)

#attach('/net/hafkaldi.hafro.is/export/u2/reikn/Splus5/HAUSTRALLNewStrata/.RData', pos=3)


###---- SMH ----###

attach("/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/SMH/stations.rdata")
d <- 
  haustrall.all.st %>%
  select(synis_id = synis.id, newstrata) %>% 
  mutate(newstrata = as.integer(newstrata))
dbWriteTable(mar, name = "SMHSTATIONSSTRATA", value = d, overwrite = FALSE)
detach("file:/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/SMH/stations.rdata")
# sql: grant select on smhstationsstrata to h_fiskar_skoda


###---- Area definitions ----###

#If an error with 'data' missing comes up, restart R session
reitmapping_original <- read.table(
  system.file("demo-data", "reitmapping.tsv", package="mfdb"),
  header=TRUE,
  as.is=TRUE) %>% 
  tbl_df() %>% 
  mutate(id = 1:n(),
         lat = geo::sr2d(GRIDCELL)$lat,
         lon = geo::sr2d(GRIDCELL)$lon) %>% 
  purrrlyr::by_row(safely(function(x) geo::srA(x$GRIDCELL),otherwise=NA)) %>% 
  unnest(size=.out %>% map('result')) %>% 
  select(-.out) %>% 
  na.omit()

#for some reason this doesn't work on mine unless I have a clean session with minimal packages loaded (tidyverse, mfdb, mar)
#but this table needs to be written once if later scripts will work
#dbWriteTable(mar,'reitmapping_original',as.data.frame(reitmapping_original),overwrite=TRUE)


###---- Gear definitions ----###

husky::gearlist %>% 
  dbWriteTable(mar,'husky_gearlist',.,overwrite =TRUE)

}



#####-------FUNCTIONS for Indices created with 01-length-based_indices.R-------#####
#Calculate CVs

calc_cv <- function(m, s, area, n) {
  
  Mean = sum(m * area) / sum(area)
  Sum = sum(m * area)
  tmpsum = sum(m[!is.na(s)] * area[!is.na(s)])
  Calc.sdev = sqrt(sum(s[!is.na(s)]^2 * area[!is.na(s)]^2/  n[!is.na(s)])   / sum(area[!is.na(s)])^2)
  Sdev = Calc.sdev * Sum/tmpsum
  cv = Sdev/Mean
  
  return(cv)
}


# B. Summarise abundance and biomass by station
#global variables used:
#Species = Species, Length.min = Length.min, Length.max = Length.max,
#std.cv = std.cv, std.towlength = std.towlength, min.towlength = min.towlength, 
#max.towlength = max.towlength, Index_Synaflokkur = Index_Synaflokkur, Index_Tognumer = Index_Tognumer

#lower bound inclusive
calc_index <- function(mar, 
                       total_biomass_length_ranges = c(5,500),
                       other_length_ranges = list(juv = c(5,30))){
  
  length_ranges <- c(list(total = total_biomass_length_ranges), other_length_ranges)
  
  #create index table for later
  # ind.tmp <-
  #   names(length_ranges) %>% 
  #   as.list() %>% 
  #   set_names(.,.) %>% 
  #   purrr::map(.,function(x){
  #     y <-
  #       tbl(mar, paste0('raw_index_calc_',Species)) %>%
  #       select(lengd) %>%
  #       distinct %>% 
  #       mutate(ind.tmp = ifelse(lengd >= length_ranges[[x]][1] & lengd < length_ranges[[x]][2], 1, 0)) %>% 
  #       collect(n=Inf)
  #     #      names(y)[2] <- x
  #      return(y)
  #  }) #%>% 
  #Reduce(function(...) merge(..., by='lengd', all.x=TRUE), .)
  by.strata.all <-
    
    #This part is skipped because 01-length-based_indices.R produces values that are
    #summable up through the level of strata - already adjusted for area and # tows, etc.
    # 8. summarise by station ----------------------------------------------------
  
  names(length_ranges) %>% 
    as.list() %>% 
    set_names(.,.) %>% 
    purrr::map(function(x){
      
      by.station <- 
        
        tbl(mar, paste0('raw_index_calc_',Species)) %>% 
        filter(lengd >= length_ranges[[x]][1] & lengd < length_ranges[[x]][2]) %>%
        # NOTE: here is the first step where statistics by length is dropped
        #       some (minor) recoding above would be needed if one were to take things
        #       forward by each length class
        group_by(synis_id, synaflokkur,reitur, smareitur, tognumer, veidarfaeri, tegund, ar) %>% 
        summarise(N = sum(N, na.rm = TRUE)/1000, #bring to same scale as reports
                  B = sum(B, na.rm = TRUE)/1000,
                  ml = mean(lengd, na.rm = TRUE)) %>% 
        # Zero stations - THOUGHT THIS STEP SHOULD BE REDUNDANT
        mutate(N = nvl(N,0),
               B = nvl(B,0)) 
      
      
      
      # ------------------------------------------------------------------------------
      # C. Calculate mean and standard deviation of abundance and biomass of stations
      #    within each strata and raise estimates by the area of the strata
      
      by.strata <-
        
        #tbl(mar, paste0('raw_index_calc_',Species)) %>% 
        by.station %>% 
        # 9. filter stations ---------------------------------------------------------  
      filter((synaflokkur == Index_Synaflokkur[[Species]][1] & tognumer %in% Index_Tognumer[[Species]][[1]]) | 
               (synaflokkur == Index_Synaflokkur[[Species]][2] & tognumer %in% Index_Tognumer[[Species]][[2]])) %>%  #IS THIS NECESSARY? 
        
        # 10. summarise by strata ----------------------------------------------------
      # 10.a  Get the strata for each station - already done when calculating raw_index_calc table
      left_join(tbl(mar, "SMBSTATIONSSTRATA") %>%
                  select(synis_id, strata =newstrata)) %>%
        left_join(tbl(mar, "SMHSTATIONSSTRATA") %>%
                    select(synis_id, strata2 =newstrata)) %>%
        mutate(strata = nvl(strata,strata2)) %>% 
        select(-strata2) %>% 
        left_join(tbl(mar, "NEWSTRATAAREA") %>%
                    select(strata = newstrata, area = rall.area) %>%
                    #  area is above is in km2, here convert nm2
                    mutate(area = area / 1.852^2)) %>% 
        mutate(n = N,
               b = B) %>% #probably unnecessary
        group_by(tegund, ar, strata, synaflokkur, area) %>% 
        # 10.b group by year and strata and calculate number of stations, mean and sd
        # THIS IS VERY CONFUSING HERE... taking the mean results in different answers depending on evenness of fish catch. Summing seems more appropriate, 
        # then not sure how standard deviation among stations relates to the final standard devation. I believe what's
        # going on is that the sd per station is based on the total biomass, and just applied to any size range,
        # since the size range indices in Höski's code were a side calculation using cum sum.
         summarise(sN  = n(),   # number of stations within strata
                   n_s = sum(N, na.rm = TRUE), # for scaling up length_dependent portions of the biomass
                   n_m  = mean(N, na.rm = TRUE), # number of fish
                   n_d  = ifelse(n() == 1, sum(N, na.rm = TRUE) * std.cv, sd(N)),
                   b_s  = sum(B, na.rm = TRUE), # for scaling up length_dependent portions of the biomass
                   b_m  = mean(B, na.rm = TRUE), # biomass of fish
                   b_d  = ifelse(n() == 1, sum(B, na.rm = TRUE) * std.cv, sd(B)),
                   ml = ifelse(sum(N, na.rm = TRUE)==0, 0, sum(ml*N, na.rm = TRUE) / sum(N, na.rm = TRUE) )  # mean length of fish  
        ) %>% 
        #raise by strata
        mutate(N     = n_m  * area / std.towlength, #this should be absent or also by tow width 
               B     = b_m  * area / std.towlength) %>% 
        mutate(index = x) %>% 
        collect(n = Inf)
      #%>% 
      #above only seems to work for total or juvenile biomass for wolffish - patchiness problem?
      #BELOW WAS PROBABLY THE PROBLEM - NOT IN EINAR'S CODE
      #divide by number of tows (?)
      #mutate(N = N/n_distinct(synis_id),
      #       B = B/n_distinct(synis_id)) 
    }) %>% 
    bind_rows(.)
      
      
      # ------------------------------------------------------------------------------
      # D. Summarise data by year
      
      
      by.year <- 
        by.strata.all %>% 
        select(-c(n_m, n_d, b_m, b_d, N, B)) %>% #inappropriately calculated for anything but total indices
        left_join(by.strata.all %>% 
                    filter(index == 'total') %>% 
                    select(tegund, ar, strata, synaflokkur, N, B, n_s, b_s, n_m, b_m, n_d, b_d) %>% #this step replaces with mean and standard deviations calculated from total indices
                    rename(n_s_tot = n_s, b_s_tot = b_s)) %>% 
        mutate(N_rat = ifelse(n_s_tot==0, 1, n_s/n_s_tot), 
               N = N*N_rat,# here, N from total index (already scaled up to strata based on n_m) are adjusted by the proportion attributable to a certain length index
               B_rat = ifelse(b_s_tot==0, 1, b_s/b_s_tot),
               B= B*B_rat) %>% # now N, B are length-based index dependent, but n_m, b_m, n_d, b_d, sN are based on total index
        #subset by length range
        # left_join(tbl(mar, paste0('raw_index_calc_',Species)) %>%
        #             select(lengd) %>%
        #             distinct %>% 
        #             mutate(ind.tmp = ifelse(lengd >= length_ranges[[x]][1] & lengd < length_ranges[[x]][2], 1, 0))) %>%
        # ----------------------------------------------------------------------------
      # Up to now we only have been operating within Oracle. I.e. sql-scripts via R.
      # Have to collect here because of calc_cv function in the year aggregate step
      # TODO: Fix that, do internally in Oracle
      #collect(n = Inf) %>% 
        # ----------------------------------------------------------------------------
      # some data fall outside strata, drop them - needs some double checking of code
      drop_na() %>% 
        # subset by index groups------------------------------------------------
      #left_join(ind.tmp[[x]]) %>% 
      #  filter(ind.tmp==1) %>% 
      # 11. summarise by year ------------------------------------------------------
      group_by(tegund, synaflokkur, ar, index) %>%
        summarise(n = sum(N, na.rm = TRUE), #based on specific index
                  # A la Höski
                  n.cv = calc_cv(n_m, n_d, area, sN), # based on total index, except sN which is based on specific index
                  b = sum(B, na.rm = TRUE), #based on specific index
                  # A la Höski
                  b.cv = calc_cv(b_m, b_d, area, sN), # based on total index, except sN which is based on specific index
                  ml = ifelse(sum(N, na.rm = TRUE)==0, 0, sum(ml*N, na.rm = TRUE)/sum(N, na.rm=TRUE))) %>%
        ungroup()
  
  return(by.year)
  #if(type=='by.year'){return(by.year)} else {return(by.strata)} 
}

#test <-calc_index(mar)

