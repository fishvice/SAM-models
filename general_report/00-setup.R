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

tyr <- 2018
Species <- 2                    # Select a species
sp_name <- 
  lesa_tegundir(mar) %>% 
  filter(tegund==Species) %>% 
  select(enskt_heiti) %>% collect() %>% 
  unlist


#categories for making ALK - should be made into a table like age_minlength
AGE <- 2:14
LEN <- c(seq(22.5,77.5,by=5),87.5)


setwd('/home/pamela/Documents/Hafro/fishvice/SAM-models')
sp_dir<-paste0(Species, ' - ', sp_name)
dir.create(file.path(getwd(), sp_dir))
setwd(file.path(getwd(), sp_dir)) #should work for windows?
yr_dir<-as.character(tyr)
dir.create(file.path(getwd(), yr_dir))

Index_Synaflokkur <- c(30,35)         #for indices
Index_Tognumer <- list(1:39, 1:75)       #ARE THESE RIGHT?

Length.min <- 5                   # Minimum length for indices calculation
Length.max <- 500                 # Maximum length for indices calculation

#for script 03-indices4plot.R
#cutoffs for indices in the 4 corners of the 4-plot
#this is coded for looping over multiple species
cutoffs_list<-NULL
cutoffs_list[[2]]<-list(total = c( 5,500), juv = c(0,30), B40 = c(40,500), B60=c(60,500)) #haddock
cutoffs_list[[6]]<-list(total = c(10,500), juv = c(0,40), B40 = c(40,500), B80=c(80,500)) #ling
cutoffs_list[[8]]<-list(total = c( 5,500), juv = c(0,30), B40 = c(40,500), B60=c(60,500)) #tusk
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

#Catch tables need to be rerun every year
# ------------------------------------------------------------------------------
###----TOTAL CATCH ----###

#NOT WORKING not sure why - but not necessary -replaced with fiskifelag_oslaegt()
#Get old catch data
# bind_rows(
#   list.files('/net/hafkaldi.hafro.is/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = '^[0-9]+',full.names = TRUE) %>% 
#     map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep='\t')) %>% 
#     bind_rows() %>% 
#     rename_(.dots=stats::setNames(colnames(.),c('vf',	'skip',	'teg',	'ar',	'man',	'hofn',	'magn'))) %>% 
#     mutate(magn=as.numeric(magn)),
#   list.files('/net/hafkaldi.hafro.is/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = 'ready',full.names = TRUE) %>% 
#     map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep='\t')) %>% 
#     bind_rows() %>% 
#     rename_(.dots=stats::setNames(colnames(.),c(	'ar','hofn',	'man',	'vf',	'teg', 'magn'))),
#   list.files('/net/hafkaldi.hafro.is/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = 'afli.[0-9]+$',full.names = TRUE) %>% 
#     map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep=';')) %>% 
#     bind_rows()%>% 
#     rename_(.dots=stats::setNames(colnames(.),c(	'ar','hofn',	'man',	'vf',	'teg', 'magn')))) %>%
#   filter(!(ar==1991&is.na(skip))) %>% 
#   mutate(veidisvaedi='I') %>% 
#   rename(veidarfaeri=vf,skip_nr=skip,magn_oslaegt=magn,fteg=teg) %>% 
#   dbWriteTable(mar,'landed_catch_pre94',.)
#attach("/net/hafkaldi.hafro.is/export/u2/reikn/R/SurveyWork/SMB/Stations.rdata")

#add fiskifelag_oslaegt code here later if necessary

dbRemoveTable(mar,paste0(sp_name,'_catch'))
mar::afli_afli(mar) %>% 
  dplyr::filter(tegund == Species) %>%
  dplyr::left_join(afli_afli(mar) %>% 
                     dplyr::group_by(visir) %>% 
                     dplyr::summarise(total=sum(afli,na.rm = TRUE))) %>% 
  dplyr::inner_join(mar::afli_stofn(mar) %>% 
                      mutate(gridcell = reitur*10+smareitur)) %>% 
  dplyr::left_join(mar::afli_toga(mar) %>% 
                     dplyr::select(visir,togtimi)) %>% 
  dplyr::left_join(mar::afli_lineha(mar) %>%
                     dplyr::mutate(hooks = onglar*bjod, nr_net = dregin) %>% 
                     dplyr::select(visir,hooks, nr_net)) %>% 
  dplyr::left_join(tbl(mar,'gear_mapping'),by=c('veidarf'='veidarfaeri')) %>% 
  dplyr::select(id=visir,towtime=togtimi,gear,vessel_nr=skipnr,year=ar,month=man,
                lat=breidd,lon=lengd,gridcell,depth=dypi,catch=afli,total,hooks,nr_net) %>% 
  dplyr::mutate(depth = 1.83*depth) %>% #still necessary?
  compute(name=paste0(sp_name,'_catch'),temporary=FALSE)


