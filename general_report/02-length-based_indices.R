
#Catch and length index tables need to be rerun every year - used in figures


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

try(dbRemoveTable(mar,paste0(sp_name,'_catch')), silent=TRUE)
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


# ------------------------------------------------------------------------------
###--------LENGTH_BASED INDICES-----###


# A. Data gathering ------------------------------------------------------------
try(dbRemoveTable(mar,paste0('raw_index_calc_',Species)), silent=TRUE)

by.length <-
  # 1. get survey stations -----------------------------------------------------
lesa_stodvar(mar) %>%
  filter(synaflokkur %in% Index_Synaflokkur[[Species]]) %>%
  mutate(index = reitur * 100 + tognumer) %>%
  select(synis_id, ar, index, reitur, smareitur, tognumer, veidarfaeri, toglengd,synaflokkur) %>%
  #select(synis_id, synaflokkur, ar, reitur, smareitur, tognumer, veidarfaeri, toglengd) %>%
  mutate(gridcell = reitur*10 + smareitur) %>% 
  # 2. get length data ---------------------------------------------------------
left_join(lesa_lengdir(mar) %>%
            filter(tegund %in% Species,
                   lengd >= Length.min[[Species]],
                   lengd < Length.max[[Species]]) %>%
            group_by(synis_id, tegund, lengd) %>%
            summarise(fjoldi = sum(fjoldi, na.rm = TRUE)),
          by = "synis_id") %>%
  ungroup() %>%
  # 0. A temporary fix, for zero stations --------------------------------------
#     TODO: Find a more permanent solution so scripts works for more than
#           one species (via group_by( ..., tegund))
mutate(tegund = if_else(is.na(tegund), Species, tegund),
       lengd  = if_else(is.na(lengd), Length.min[[Species]], lengd), 
       fjoldi = if_else(is.na(fjoldi), 0, fjoldi)) %>%
  
  # 3. get count data ----------------------------------------------------------
#    Note: If only counted at station this code approach fails
skala_med_toldum() %>% 
  
  # 4. scale by counted --------------------------------------------------------
mutate(fjoldi = fjoldi/1e3) %>%   
  #mutate(N = fjoldi / 1e3) %>%   # units of thousand
  
  # 5. trim towlength and ----------------------------------------------------------

# 6.a standardize by towlength ------------------------------------------------
# this is done within the above skala_med_toglengd
#mutate(N = fjoldi / toglengd * std.towlength) %>%      # standardize to per 4 miles
mar:::skala_med_toglengd(., min_towlength = min.towlength, max_towlength = max.towlength, std_towlength = std.towlength) %>%
  
  # 6.b standardize to area swept
  #     this does not make much sense here because we already have this above
  #     need to pass this function further down in the code path
  #     
  mutate(N = fjoldi / if_else(veidarfaeri == 78 , 1.25 * std.width, std.width)) %>%
  
  # 7. calculate_biomass from numbers, length and a and b ----------------------
# 7.a get the length weight coefficients
left_join(tbl_mar(mar, "ops$einarhj.lwcoeff"),
          by = "tegund") %>%
  
  # 7.b use Newton's law if lwcoefficient for species not specified
  mutate(a = ifelse(is.na(a), 0.01, a),
         b = ifelse(is.na(b), 3.00, b),
         B  = ifelse(is.na(N), 0, N) * a * lengd^b / 1e3) %>%
  select(-a, -b) %>% 
  
  
  #Below not really necessary
  # 8. Cumulative calculation
  #     Note: This step is a precursor for calculating things via
  #           abundance less than and biomass greater than
  #arrange(tegund, synis_id, desc(lengd)) %>% #indices will are calculated as greater than or equal to, so need to reverse lengd order
  #group_by(tegund, synis_id) %>%
  #mutate(cN = cumsum(N),
  #       cB = sum(B, na.rm = TRUE) - cumsum(B) + B) %>% #this is for plotting indices at certain length cutoffs in 03-indices4plot
  ungroup() %>% 
  compute(name=paste0('raw_index_calc_',Species),temporary =FALSE, overwrite =TRUE)


#tbl(mar, paste0('raw_index_calc_',Species))




