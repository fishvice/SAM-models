## db setup 
## stuff to load for the first time..
library(mar)
library(knitr)
library(kableExtra)
library(patchwork)
library(forcats)
mar <- connect_mar()
tyr <- 2018


#load('/net/hafkaldi/export/home/haf/einarhj/r/Pakkar/landr/data/lices.rda')
#dbWriteTable(mar,'lices',lices)

bind_rows(
  list.files('/net/hafkaldi/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = '^[0-9]+',full.names = TRUE) %>% 
    map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep='\t')) %>% 
    bind_rows() %>% 
    rename_(.dots=stats::setNames(colnames(.),c('vf',	'skip',	'teg',	'ar',	'man',	'hofn',	'magn'))) %>% 
    mutate(magn=as.numeric(magn)),
  list.files('/net/hafkaldi/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = 'ready',full.names = TRUE) %>% 
    map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep='\t')) %>% 
    bind_rows() %>% 
    rename_(.dots=stats::setNames(colnames(.),c(	'ar','hofn',	'man',	'vf',	'teg', 'magn'))),
  list.files('/net/hafkaldi/export/u2/reikn/R/Pakkar/Logbooks/Olddata',pattern = 'afli.[0-9]+$',full.names = TRUE) %>% 
    map(~read.table(.,skip=2,stringsAsFactors = FALSE,sep=';')) %>% 
    bind_rows()%>% 
    rename_(.dots=stats::setNames(colnames(.),c(	'ar','hofn',	'man',	'vf',	'teg', 'magn')))) %>%
  filter(!(ar==1991&is.na(skip))) %>% 
  mutate(veidisvaedi='I') %>% 
  rename(veidarfaeri=vf,skip_nr=skip,magn_oslaegt=magn,fteg=teg) %>% 
  dbWriteTable(mar,'landed_catch_pre94',.)

dbRemoveTable(mar,'had_catch')
mar::afli_afli(mar) %>% 
  dplyr::filter(tegund == 2) %>%
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
  dplyr::mutate(depth = 1.83*depth) %>% 
  compute(name='had_catch',temporary=FALSE)

## indices
attach("/net/hafkaldi/export/u2/reikn/R/SurveyWork/OldStratas/.RData")
d <- 
  data_frame(oldstrata = attributes(STRATAS)$names %>% as.integer(),
             name = attributes(STRATAS)$names,
             area = attributes(STRATAS)$area,
             rall.area = attributes(STRATAS)$rall.area)
dbWriteTable(mar, name = "OLDSTRATAAREA", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi/export/u2/reikn/R/SurveyWork/OldStratas/.RData")
# sql: grant select on oldstrataarea to h_fiskar_skoda

## New strata

attach("/net/hafkaldi/export/u2/reikn/R/SurveyWork/NewStratas/.RData")
d <- 
  data_frame(newstrata = 1:length(attributes(STRATAS)$names),
             name = attributes(STRATAS)$names,
             area = attributes(STRATAS)$area,
             rall.area = attributes(STRATAS)$rall.area)
dbWriteTable(mar, name = "NEWSTRATAAREA", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi/export/u2/reikn/R/SurveyWork/NewStratas/.RData")
# sql: grant select on newstrataarea to h_fiskar_skoda

## SMB

attach("/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Stations.rdata")
d <- 
  STODVAR.all %>%
  select(synis_id = synis.id, oldstrata, newstrata)
dbWriteTable(mar, name = "SMBSTATIONSSTRATA", value = d, overwrite = TRUE)
detach("file:/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Stations.rdata")
# sql: grant select on smbstationsstrata to h_fiskar_skoda

#NOTE: In 2018 the stations were fixed to a strata, thus the above is since then replaced with the following:
  
# INDEX_STRATA <-
#   newstratas %>% 
#   full_join(oldstratas) %>% 
#   separate(index, c("reitur", "tognumer"), 3, marvert = TRUE) %>% 
#   mutate(veidarfaeri = 73)
# dbWriteTable(mar, name = "SMB_INDEX_STRATA", value = SMB_INDEX_STRATA, overwrite = TRUE)

## SMH

attach("/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMH/stations.rdata")
d <- 
  haustrall.all.st %>%
  select(synis_id = synis.id, newstrata) %>% 
  mutate(newstrata = as.integer(newstrata))
dbWriteTable(mar, name = "SMHSTATIONSSTRATA", value = d, overwrite = FALSE)
detach("file:/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMH/stations.rdata")
# sql: grant select on smhstationsstrata to h_fiskar_skoda

## Calculate indices

# ------------------------------------------------------------------------------
# Constants
std.cv        <- 1             # The cv if only one station in a strata
std.towlength <- 4             # Standard tow length for SMB is 4 nautical miles
std.width     <- 17 / 1852     # Standard sweep width in nautical miles

min.towlength <- 2             # Minimum "acceptable" towlength
max.towlength <- 8             # Maximum "acceptable" towlength

# ------------------------------------------------------------------------------
# Variable stuff
Synaflokkur <- c(30,35)
Tognumer <- c(1:39, NA)
Species <- 2                      # Select a species
Length.min <- 5                   # Minimum length for indices calculation
Length.max <- 500                 # Maximum length for indices calculation

# ------------------------------------------------------------------------------
# Calculations

# A. Data gathering ------------------------------------------------------------

by.length <-
  # 1. get survey stations -----------------------------------------------------
lesa_stodvar(con) %>%
  filter(synaflokkur %in% Synaflokkur) %>%
  mutate(index = reitur * 100 + tognumer) %>%
  select(synis_id, ar, index, reitur, smareitur, tognumer, veidarfaeri, toglengd,synaflokkur) %>%
  mutate(gridcell = reitur*10 + smareitur) %>% 
  # 2. get length data ---------------------------------------------------------
left_join(lesa_lengdir(con) %>%
            filter(tegund %in% Species,
                   lengd >= Length.min,
                   lengd < Length.max) %>%
            group_by(synis_id, tegund, lengd) %>%
            summarise(fjoldi = sum(fjoldi, na.rm = TRUE)),
          by = "synis_id") %>%
  ungroup() %>%
  # 0. A temporary fix, for zero stations --------------------------------------
#     TODO: Find a more permanent solution so scripts works for more than
#           one species (via group_by( ..., tegund))
mutate(tegund = if_else(is.na(tegund), Species, tegund),
       lengd  = if_else(is.na(lengd), Length.min, lengd),
       fjoldi = if_else(is.na(fjoldi), 0, fjoldi)) %>%
  
  # 3. get count data ----------------------------------------------------------
#    Note: If only counted at station this code approach fails
skala_med_toldum() %>% 
  
  mutate(fjoldi = fjoldi_alls/1e3) %>%   
  # 4. scale by counted --------------------------------------------------------
#mutate(N = fjoldi_alls / 1e3) %>%   # units of thousand

# 5. trim towlength ----------------------------------------------------------
mar:::skala_med_toglengd() %>%
  # 6.a standardize by towlength ------------------------------------------------
#mutate(N = fjoldi / toglengd * std.towlength) %>%      # standardize to per 4 miles
# 6.b standardize to area swept
#     this does not make much sense here because we already have this above
#     need to pass this function further down in the code path
mutate(N = fjoldi / if_else(veidarfaeri == 78, 1.25 * std.width, std.width)) %>%
  # 7. calculate_biomass from numbers, length and a and b ----------------------
# 7.a get the length weight coefficients
left_join(tbl_mar(con, "ops$einarhj.lwcoeff"),
          by = "tegund") %>%
  # 7.b use Newton's law if lwcoefficient for species not specified
  mutate(a = ifelse(is.na(a), 0.01, a),
         b = ifelse(is.na(b), 3.00, b),
         B  = ifelse(is.na(N), 0, N) * a * lengd^b / 1e3) %>%
  select(-a, -b) %>% 
  left_join(tbl(con, "SMBSTATIONSSTRATA") %>%
              select(synis_id, strata =newstrata)) %>%
  left_join(tbl(con, "SMHSTATIONSSTRATA") %>%
              select(synis_id, strata2 =newstrata)) %>%
  mutate(strata = nvl(strata,strata2)) %>% 
  select(-strata2) %>% 
  left_join(tbl(con, "NEWSTRATAAREA") %>%
              select(strata = newstrata, area = rall.area) %>%
              #  area is above is in km2, here convert nm2
              mutate(area = area / 1.852^2)) %>%
  mutate(n = N,
         b = B) %>% 
  mutate(N     = N  * area / std.towlength,
         B     = B  * area / std.towlength) %>% 
  group_by(tegund,ar,strata) %>% 
  mutate(N = N/n_distinct(synis_id),
         B = B/n_distinct(synis_id)) %>% 
  # 8. Cumulative calculation
  #     Note: This step is a precursor for calculating things via
  #           abundance less than and biomass greater than
  arrange(tegund, synis_id, lengd) %>%
  group_by(tegund, synis_id) %>%
  mutate(cN = cumsum(N),
         cB = sum(B, na.rm = TRUE) - cumsum(B) + B) %>%
  ungroup() %>% 
  compute(name='raw_index_calc',temporary =FALSE, overwrite =TRUE)

