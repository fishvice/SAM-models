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

recreate_helper_tables <- TRUE #helper tables should only need to be created once - not species specific. 
#However, as currently implemented they depend on synis_id so may need to run every year...

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
data.frame(age=1:14,minlength=15+2.5*(1:14), maxlength = 200, species=2) %>%
  bind_rows(data.frame(age = 1:23, minlength = 5+2*(1:23), maxlength = c(20, 35, rep(200, 23-2)),species=9)) %>% 
  bind_rows(data.frame(age = 1:30, minlength = c(5+1.5*(1:20), rep(35, 10)), maxlength = c(25, 30, 35, 40, 45, rep(60, 30-5)),species=19)) %>% 
  dbWriteTable(mar,'age_minlength',.,overwrite=TRUE)

ldist_bins <- NULL
ldist_bins[[2]] <- c(seq(22.5,77.5,by=5),87.5)
ldist_bins[[9]] <- c(seq(5, 100,by=5),120)
ldist_bins[[19]] <- c(seq(10, 58,by=4),60)

ALKyr_catch <- ALKyr_surv <- LEyr_catch <- LEyr_surv <-1900 #placeholder reference year to fill in good ALK or length data for catch at age or surv
use_alk_catch <- use_alk_surv <- use_le_catch <- use_le_surv <- FALSE

Index_Synaflokkur <- Index_Tognumer <- species_strata_depth_list <- species_strata_region_list <- species_winsor_list <- NULL

#assign species with highest tegund first to create lists properly
species_strata_region_list[[19]] <- c('N', 'S', 'W', 'E', 'NE', 'NW', 'SE', 'SW')
species_strata_depth_list[[19]] <- c('deep')
species_winsor_list[[19]] <- c('q95') #this will replace only the upper 5% with the 95% quantile value
species_strata_depth_list[[1]] <- species_strata_depth_list[[2]] <- species_strata_depth_list[[9]] <- 'all'
species_strata_region_list[[1]] <- species_strata_region_list[[2]] <- species_strata_region_list[[9]] <- 'all'
species_winsor_list[[1]] <- species_winsor_list[[2]] <- species_winsor_list[[9]] <- ''

Index_Synaflokkur[[19]] <- c(35,35)
Index_Tognumer[[19]] <- list(1:72, 1:72)#It was 72 in Muggur's code
Index_Synaflokkur[[1]] <- Index_Synaflokkur[[2]] <- Index_Synaflokkur[[9]] <-  c(30,35)         #for including which surveys to make indices from
Index_Tognumer[[1]] <- Index_Tognumer[[2]] <- Index_Tognumer[[9]] <- list(1:39, 1:75)       #and corresponding townumbers - ARE THESE RIGHT?
  
Length.min <- Length.max <- NULL
Length.max[[19]] <- 60
Length.min[[1]] <- Length.min[[2]] <- Length.min[[9]] <- Length.min[[19]] <- 5                  # Minimum length for indices calculation
Length.max[[1]] <- Length.max[[2]] <- Length.max[[9]] <- 500                 # Maximum length for indices calculation

#for script 03-indices4plot.R
#cutoffs for indices in the 4 corners of the 4-plot
#this is coded for looping over multiple species
cutoffs_list<-NULL
cutoffs_list[[19]]<-list(total = c( 10,60), juv = c(10,20), B25 = c(25,60), B79=c(35,60)) #gss
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
#dbRemoveTable(mar,'husky_gearlist')
#dbRemoveTable(mar,'reitmapping_original')
#dbRemoveTable(mar,'OLDSTRATAAREA')
#dbRemoveTable(mar,'NEWSTRATAAREA')
#dbRemoveTable(mar,'SMBSTATIONSSTRATA')
#dbRemoveTable(mar,'SMHSTATIONSSTRATA')
#dbRemoveTable(mar,'SMHSTATIONSSTRATA_19')

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
dbWriteTable(mar, name = "SMHSTATIONSSTRATA", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/SMH/stations.rdata")
# sql: grant select on smhstationsstrata to h_fiskar_skoda

###---- SMH for GSS----###

attach("/net/hafkaldi.hafro.is/export/u2/reikn/Tac/2018/19-GSS/Rwork/STRATAS.RData")
#attach("/net/hafkaldi.hafro.is/export/u2/reikn/Tac/2018/19-GSS/Rwork/gulllaxNewStr.RData")

  
  ### Size of stratas 1:41
#  attributes(STRATAS)$rall.area

d400<-c(1,6,4,7,9,10,11,15,16,17,19,22,20,27)
shallow<-c(35,40,29,34,25,24,26,28,23,3,2,8,5,39,12,33,32,31,14,30)
total<-c(d400, shallow)
d400<-data.frame(strata=names(STRATAS[d400]),nr=d400)
shallow<-data.frame(strata=names(STRATAS[shallow]),nr=shallow)
total<-data.frame(strata=names(STRATAS[total]),nr=total)
NWs<-c(35,40,29,34)
NWs<-data.frame(strata=names(STRATAS[NWs]),nr=NWs)
Ws<-c(25,24,26,28,23)
Ws<-data.frame(strata=names(STRATAS[Ws]),nr=Ws)
SWs<-c(3,2)
SWs<-data.frame(strata=names(STRATAS[SWs]),nr=SWs)
SEs<-c(8,5)
SEs<-data.frame(strata=names(STRATAS[SEs]),nr=SEs)
Es<-c(39,12)
Es<-data.frame(strata=names(STRATAS[Es]),nr=Es)
Ns<-c(33,32,31,14,30)
Ns<-data.frame(strata=names(STRATAS[Ns]),nr=Ns)
#Deep sub-area
NWd<-c(17,19)
NWd<-data.frame(strata=names(STRATAS[NWd]),nr=NWd)
Wd<-c(22,20,27)
Wd<-data.frame(strata=names(STRATAS[Wd]),nr=Wd)
Sd<-c(1,6)
Sd<-data.frame(strata=names(STRATAS[Sd]),nr=Sd)
SEd<-c(4,7,9)
SEd<-data.frame(strata=names(STRATAS[SEd]),nr=SEd)
NEd<-c(10,11,15,16)
NEd<-data.frame(strata=names(STRATAS[NEd]),nr=NEd)
mugg.smh<-list(d400=d400, shallow=shallow, NWs=NWs, Ws=Ws, SWs=SWs, SEs=SEs, Es=Es,Ns=Ns,
               NWd=NWd, Wd=Wd, Sd=Sd, SEd=SEd, NEd=NEd, total=total)

mugg.ralllist<-sort(mugg.smh$total$nr)

smh.st.init <- 
  lesa_stodvar(mar) %>% 
  filter(synaflokkur==35, tognumer <= 72, ar >=2000, !is.na(synis_id)) %>% 
  select(synis_id, leidangur, ar, man, dags, kastad_n_breidd, kastad_v_lengd, hift_n_breidd, hift_v_lengd, toglengd, reitur, tognumer, veidarfaeri) %>% 
  dplyr::rename(synis.id = synis_id) %>% 
  collect(n=Inf) %>% 
  mutate(lat = (kastad_n_breidd + hift_n_breidd)/2, lon = (kastad_v_lengd + hift_v_lengd)/2) %>% 
  select(-c(kastad_n_breidd, hift_n_breidd, kastad_v_lengd, hift_v_lengd)) %>% 
  unite('station', reitur, tognumer) 
#stodvar[stodvar$synaflokkur==35 & stodvar$tognumer <=72 & stodvar$ar>=2000,]
#smh.st<-smh.st[,c('synis.id','leidangur','ar','man',
#                  'dags','lat','lon', 'toglengd')]
#smh.st<-smh.st[!is.na(smh.st$synis.id),]


smh.st.pj<-
  husky::inside.strata(smh.st.init, mugg.ralllist) %>%
  filter(newstrata>0) %>% 
  mutate(strata_depth = ifelse(newstrata %in% d400$nr, 'deep', 
                               ifelse(newstrata %in% shallow$nr, 'shallow', NA))) %>% 
  mutate(strata_region = ifelse(newstrata %in% c(SEs$nr, SEd$nr), 'SE', 
                                ifelse(newstrata %in% c(SWs$nr), 'SW',
                                       ifelse(newstrata %in% c(Ws$nr, Wd$nr), 'W',
                                              ifelse(newstrata %in% c(Es$nr), 'E',
                                                     ifelse(newstrata %in% c(Ns$nr), 'N',
                                                            ifelse(newstrata %in% c(NEd$nr), 'NE',
                                                                   ifelse(newstrata %in% c(NWs$nr, NWd$nr), 'NW', 
                                                                          ifelse(newstrata %in% c(Sd$nr), 'S', NA)))))))))

#from Calc.index 
sfile <- smh.st.pj %>% rename(strata = newstrata)
strata.list <- mugg.ralllist
areas <- attributes(STRATAS)$rall.area
for(i in 1:length(strata.list)) {
  areas[strata.list[[i[1]]]] <- sum(areas[strata.list[[
    i]]])
  j <- !is.na(match(sfile$strata, strata.list[[i]]))
  if(any(j))
    sfile$strata[j] <- strata.list[[i]][1]
}

smh.st.pj2<-
  sfile %>% 
  left_join(tibble(strata = attributes(STRATAS)$name, area = areas[1:64])) %>% 
  left_join(lesa_numer(mar) %>% filter(tegund==19) %>%  select(synis.id = synis_id, fj_talid) %>% collect(n=Inf)) %>%
  mutate(fjoldi = ifelse(is.na(fj_talid),0, fj_talid))
  #filter(!is.na(fjoldi))
  

d <-
  smh.st.pj2 %>% 
  left_join(smh.st.pj2 %>%
              group_by(ar) %>% 
              summarise(q95 = quantile(fjoldi, 0.95, na.rm = TRUE),
                        q05 = quantile(fjoldi, 0.05, na.rm =TRUE)
                        )) %>% 
  group_by(ar) %>% 
  mutate(winsor = ifelse(fjoldi >= q95, 'q95',
                         ifelse(fjoldi <= q05, 'q05','mid')),
         winsor_val = ifelse(winsor=='q95', q95/fjoldi,
                             ifelse(winsor=='q05'& fjoldi==0, 1, 
                                    ifelse(winsor=='q05' & fjoldi > 1, q05/fjoldi, 
                                           ifelse(winsor=='mid', 1, NA))))) %>% 
  select(synis_id = synis.id, station, strata, strata_depth, strata_region, area, winsor, winsor_val) %>% 
  mutate(newstrata = as.integer(strata))


dbWriteTable(mar, name = "SMHSTATIONSSTRATA_19", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi.hafro.is/export/u2/reikn/Tac/2018/19-GSS/Rwork/STRATAS.RData")
# sql: grant select on smhstationsstrata to h_fiskar_skoda


###---- Area definitions ----###

#If an error with 'data' missing comes up, restart R session
try(reitmapping_original <- read.table(
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
  na.omit(), silent = T)

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
# test synis_id 468908 for GSS
# 
#lower bound inclusive
calc_index <- function(mar, 
                       total_biomass_length_ranges = c(5,500),
                       other_length_ranges = list(juv = c(5,30)),
                       species_strata_depth = species_strata_depth_list[[Species]],
                       species_strata_region = species_strata_region_list[[Species]],
                       species_winsor = species_winsor_list[[Species]]){
  
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
    
  
    # 8. summarise by station ----------------------------------------------------
    # sum over length ranges
  
  names(length_ranges) %>% 
    as.list() %>% 
    set_names(.,.) %>% 
    purrr::map(function(x){
      
      by.station <- 
        
        tbl(mar, paste0('raw_index_calc_',Species)) %>% 
        filter((lengd >= length_ranges[[x]][1] & lengd < length_ranges[[x]][2]) | lengd==0) %>% #0 lengths indicate 0 counts at stations
        # NOTE: here is the first step where statistics by length is dropped
        #       some (minor) recoding above would be needed if one were to take things
        #       forward by each length class
        group_by(synis_id, synaflokkur,reitur, smareitur, tognumer, veidarfaeri, tegund, ar) %>% 
        # Zero stations - THOUGHT THIS STEP SHOULD BE REDUNDANT
        mutate(N = nvl(N,0), 
               B = nvl(B,0)) %>% 
        dplyr::summarise(N = sum(N, na.rm = TRUE)/1000, #bring to same scale as reports
                         B = sum(B, na.rm = TRUE)/1000,
                         ml = mean(lengd, na.rm = TRUE)) 
      
      
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
        left_join(tbl(mar, "SMHSTATIONSSTRATA_19") %>%
                    select(synis_id, strata_19 = newstrata, area_19 = area, strata_depth_19 = strata_depth, strata_region_19 = strata_region, winsor_19=winsor, winsor_val_19 = winsor_val) %>% 
                    mutate(area_19 = area_19 / 1.852^2)) %>% #hafe seen 1.854 other places
        filter(!(is.na(strata_19) & tegund==19)) %>% 
        mutate(strata = ifelse(tegund==19, strata_19, strata),
               area = ifelse(tegund==19, area_19, area),
               strata_depth = ifelse(tegund==19, strata_depth_19, 'all'),
               strata_region = ifelse(tegund==19, strata_region_19, 'all'),
               winsor = ifelse(tegund==19, winsor_19, 'none'),
               winsor_val = ifelse(tegund==19, winsor_val_19, 1),
               N = ifelse(winsor %in% species_winsor, N*winsor_val, N),
               B = ifelse(winsor %in% species_winsor, B*winsor_val, B)) %>%
    filter(strata_depth %in% species_strata_depth, 
               strata_region %in% species_strata_region) %>% 
        mutate(n = N,
               b = B) %>% #probably unnecessary
        group_by(tegund, ar, strata, synaflokkur, area) %>% 
        # 10.b group by year and strata and calculate number of stations, mean and sd
        # THIS IS VERY CONFUSING HERE... taking the mean results in different answers depending on evenness of fish catch. Summing seems more appropriate, 
        # then not sure how standard deviation among stations relates to the final standard devation. I believe what's
        # going on is that the sd per station is based on the total biomass, and just applied to any size range,
        # since the size range indices in Höski's code were a side calculation using cum sum.
         dplyr::summarise(sN  = n(),   # number of stations within strata
                   n_s = sum(N, na.rm = TRUE), # for scaling up length_dependent portions of the biomass
                   n_m  = mean(N, na.rm = TRUE), # number of fish
                   n_d  = ifelse(n() == 1, sum(N, na.rm = TRUE) * std.cv, sd(N)),
                   b_s  = sum(B, na.rm = TRUE), # for scaling up length_dependent portions of the biomass
                   b_m  = mean(B, na.rm = TRUE), # biomass of fish
                   b_d  = ifelse(n() == 1, sum(B, na.rm = TRUE) * std.cv, sd(B)),
                   ml = ifelse(sum(N, na.rm = TRUE)==0, 0, sum(ml*N, na.rm = TRUE) / sum(N, na.rm = TRUE) )  # mean length of fish  
        ) %>% 
        #raise by strata
        mutate(N     = n_m  * area , 
               B     = b_m  * area ) %>% 
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
        dplyr::summarise(n = sum(N, na.rm = TRUE), #based on specific index
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

