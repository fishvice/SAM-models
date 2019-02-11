#foreigncatch <- 0 ???

#Note this script is needed to generate ALK for a single year (tyr). When setting up a model for the first time,
#it will be necessary to run this for all years, then each year thereafter just the final year can be run.

# all_areas is all possible areas, should be replaced within species section if necessary
all_areas <- tbl(mar, 'reitmapping_original') %>% select(DIVISION) %>% distinct %>% collect(n=Inf) %>% unlist

# ------------------------------------------------------------------------------
###----VARIABLE SETTINGS FOR PRODUCING ALK----###
#these are inputs to the function below used to create keys within each defined grouping

if(Species==2){
  #parameterized for haddock
  
comm_synaflokkur_group <- list('s1')
mat_codes <- c(2:100) #IS THIS RIGHT?
mat_surv <- 30
surv_ind <- c('t1s2r1g1vsurv', 't2s3r1g1vsurv') #corresponds with spring and autumn surveys,
#above should correspond with below group helper tables
# global_areas is the full region for consideration for the species
global_areas <- 101:108 #for haddock

#Aggregations needed by which ALK will be specific
month_group <-data.frame(month = 1:12, month_group = c(rep('t1',5),rep('t2',7)))
GRIDCELL_group <-data.frame(GRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, GRIDCELL_group = 'g1')
#area_group <- data.frame(area = all_areas) %>% mutate(area_group = ifelse(area %in% 102:105, 'r1', 'r2'))
area_group <- data.frame(area = all_areas) %>% mutate(area_group =  'r1')
#strata_group <- data.frame(strata = NA, strata_group = NA)#- to be implemented later
#above currently not implemented because strata not joined with catch synis_id
#but would be nice to do this in the future
vf_group <- data.frame(vf = c('vsurv',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist), 
                      vf_group = c('vsurv',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist))
#best to always keep s2 refer to synaflokkur 30 and s3 to 35; s1 refer to everything else
synaflokkur_group <- data.frame(synaflokkur = lesa_stodvar(mar) %>% filter(ar == tyr) %>% select(synaflokkur) %>% distinct %>% collect(n=Inf)) %>% 
                      mutate(synaflokkur_group = ifelse(synaflokkur == 30, 's2',
                                                        ifelse(synaflokkur == 35, 's3', 's1')))
#NOT IMPLEMENTED YET - because saw an attempt based on a subset synaflokkur = c(1,2,4,8)
#strata_group <- #NOT IMPLEMENTED YET - because it may be handy at some point to match catches to strata?


#Condition matrix to translate from weights. This needs to be as specific as the groups above are defined to.
cond_group<-expand.grid(condition = tbl_mar(mar, "ops$einarhj.lwcoeff") %>% filter(tegund==Species) %>% select(a) %>% collect %>%  unlist, 
                      power = tbl_mar(mar, "ops$einarhj.lwcoeff") %>% filter(tegund==Species) %>% select(b) %>% collect %>%  unlist, 
                      month_group = c('t1','t2'), 
                      GRIDCELL_group = 'g1', 
#                      area_group = c('r1','r2'), 
                      area_group = c('r1'), 
                      vf_group = unique(vf_group$vf_group),
                      synaflokkur_group = unique(synaflokkur_group$synaflokkur_group)) %>% 
  filter((!(synaflokkur_group %in% unlist(comm_synaflokkur_group)) & vf_group == 'vsurv') | (synaflokkur_group %in% unlist(comm_synaflokkur_group) & vf_group != 'vsurv') )

#month_group and synaflokkur_group not included in ref_group because time must be same as ind under consideration
ref_group <- list(GRIDCELL_group = unique(GRIDCELL_group$GRIDCELL_group),
                  area_group = 'r1',
                  areas = global,
#                  area_group = c('r1','r2'),
                  vf_group = unique(vf_group$vf_group))
}

if(Species==9){

  comm_synaflokkur_group <- list('s1')

  mat_codes <- c(2:100) #IS THIS RIGHT?
  
  mat_surv <- c(30,35) #IS THIS RIGHT?

  surv_ind <- c('t1s2r1g1vsurv', 't2s3r1g1vsurv') #corresponds with spring and autumn surveys,

  global_areas <- 101:108
  
  month_group <-data.frame(month = 1:12, month_group = c(rep('t1',6),rep('t2',6)))
  
  GRIDCELL_group <-data.frame(GRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, GRIDCELL_group = 'g1')
  
  area_group <- data.frame(area = all_areas) %>% mutate(area_group =  'r1')
  
  vf_group <- data.frame(vf = c('vsurv',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist), 
                         vf_group = c('vsurv',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist))
  
  synaflokkur_group <- data.frame(synaflokkur = lesa_stodvar(mar) %>% filter(ar == tyr) %>% select(synaflokkur) %>% distinct %>% collect(n=Inf)) %>% 
    mutate(synaflokkur_group = ifelse(synaflokkur == 30, 's2',
                                      ifelse(synaflokkur == 35, 's3', 's1')))
  
  cond_group<-expand.grid(condition = tbl_mar(mar, "ops$einarhj.lwcoeff") %>% filter(tegund==Species) %>% select(a) %>% collect %>%  unlist, 
                          power = tbl_mar(mar, "ops$einarhj.lwcoeff") %>% filter(tegund==Species) %>% select(b) %>% collect %>%  unlist, 
                          month_group = c('t1','t2'), 
                          GRIDCELL_group = 'g1', 
                          area_group = c('r1'), 
                          vf_group = unique(vf_group$vf_group),
                          synaflokkur_group = unique(synaflokkur_group$synaflokkur_group)) %>% 
    filter((!(synaflokkur_group %in% unlist(comm_synaflokkur_group)) & vf_group == 'vsurv') | (synaflokkur_group %in% unlist(comm_synaflokkur_group) & vf_group != 'vsurv') )

  ref_group <- list(GRIDCELL_group = unique(GRIDCELL_group$GRIDCELL_group),
                    area_group = 'r1',
                    areas = global_areas,
                    vf_group = unique(vf_group$vf_group))
}

# ------------------------------------------------------------------------------
###----Gather catch, age, length data----###

# all station information - only commercial to be comparable to Höski's code 
st <- 
  lesa_stodvar(mar) %>% 
  mutate(GRIDCELL = 10*reitur + smareitur) %>% 
  filter(ar == tyr) %>% 
  #inner_join(tbl(mar,'husky_gearlist')) %>% #AT THIS STEP SURVEY DATA ARE REMOVED???
  left_join(tbl(mar,'husky_gearlist')) %>%  
  rename(vf = geartext) %>% 
  mutate(vf = ifelse(synaflokkur %in% Index_Synaflokkur[[Species]], 'vsurv', vf)) %>%
  #filter(vf != 'vsurv') %>% #this will make it closer to Höski's code but at the expense of the following being generalized to both commercial and survey
  inner_join(tbl(mar,'reitmapping_original')) %>% 
  rename(area = DIVISION) %>% 
  filter(area %in% global_areas) %>% 
  rename(synis.id=synis_id)


# all otolith info that can be matched with st
kv <- 
  lesa_kvarnir(mar) %>% 
  filter(tegund == Species) %>% 
  rename(synis.id = synis_id) %>% 
  semi_join(st) %>% 
  select(synis.id,lengd,aldur,kyn,kynthroski,slaegt,oslaegt) %>% 
  filter(!is.na(aldur)) %>% 
  left_join( tbl(mar,'age_minlength') %>%
               filter(species==Species) %>% 
               mutate(aldur = age)) %>% 
  filter(lengd>minlength) %>% 
  mutate(fjoldi = 0.001) #initial weighted contribution to forming the ALK - 
                        #using all age measurements have a really small weight as a baseline

#all length info that can be matched with st
le <- 
  lesa_lengdir(mar) %>% 
  rename(synis.id = synis_id) %>% 
  semi_join(st) %>% 
  filter(tegund == Species) 

#all number info tha can be matched with st
nu <- 
  lesa_numer(mar) %>% 
  rename(synis.id = synis_id) %>% 
  semi_join(st) %>% 
  filter(tegund == Species) 

#landings_caa data by gear
landings_caa <- 
  lods_oslaegt(mar) %>% 
  filter(ar > 1993) %>% 
  full_join(fiskifelag_oslaegt(mar) %>%  
            filter(ar < 1994) %>% 
            mutate(veidisvaedi='I')) %>%  
  inner_join(tbl(mar,'husky_gearlist')) %>% 
  rename(vf = geartext) %>% 
  filter(#veidarfaeri == 1,
    fteg == Species,
    ar == tyr,
    veidisvaedi == 'I') %>% 
  group_by(vf) %>% 
  summarise(landings_caa = sum(magn_oslaegt)) 

#catch data....
catch <- 
  afli_stofn(mar) %>% 
  inner_join(afli_afli(mar)) %>% 
  filter(tegund == Species, ar == tyr) %>% 
  mutate(GRIDCELL = 10*reitur + smareitur) %>% 
  inner_join(tbl(mar,'husky_gearlist'),
             by =c('veidarf'='veidarfaeri')) %>% 
  rename(vf = geartext) %>% 
  inner_join(tbl(mar,'reitmapping')) %>% 
  rename(area = DIVISION) %>% 
  filter(area %in% global_areas) #%>% 

#.... by gear and compared with landings_caa
sc <- 
  catch %>% 
  group_by(vf) %>% 
  summarise(catch = sum(afli)) %>% 
  left_join(landings_caa)

#.... corrected by the discrepancy between landings_caa and catch specific to gear types...
commcatch <- 
  catch %>% 
  left_join(sc) %>% 
  mutate(afli = afli*landings_caa/catch) %>% #discrepancy correction
  mutate(synaflokkur_group = 's1') %>% # 'commercial' synaflokkur - helps with later groupings - 
  group_by(vf, man) %>% 
  filter(!is.na(afli)) %>% 
  mutate(catch = afli) %>% 
  collect(n=Inf) #%>% 

#... and summarized by vf, time, reg, for viewing purposes
commcatch_groupings<-
  commcatch %>% 
  rename(month = man) %>% 
  left_join(month_group) %>% 
  left_join(GRIDCELL_group) %>% 
  left_join(area_group) %>% 
  left_join(vf_group) %>% 
  filter(!is.na(month_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA', !is.na(vf_group)) %>%       
  ungroup() %>% 
  unite(index,c(month_group,synaflokkur_group,area_group,GRIDCELL_group, vf_group),sep = '',remove = FALSE) %>% 
  group_by(index) %>% 
  summarise(catch = sum(catch,na.rm=TRUE))
  
#total landings for that year
totcatch <- 
  lods_oslaegt(mar) %>% 
  filter(ar > 1993) %>% 
  full_join(fiskifelag_oslaegt(mar) %>%  
              filter(ar < 1994) %>% 
              mutate(veidisvaedi='I')) %>%  
  filter(fteg == Species,
    ar == tyr,
    veidisvaedi == 'I') %>% 
  summarise(landings_caa = sum(magn_oslaegt,na.rm=TRUE)) %>% 
  collect(n=Inf) %>% 
  .$landings_caa

#passed globally into function
st_c <- st %>% collect(n=Inf)   
kv_c <- kv %>% collect(n=Inf)
le_c <- le %>% collect(n=Inf)  


# ------------------------------------------------------------------------------
###----Define calc_all function to use data defined above----###


#age_minlength argument needs to be updated with appropriate ages and the minimum lengths used to clean the data
#group arguments need to be updated with the aggregations within which ALKs should be made
#putting NA in the group column excludes the corresponding data
calc_all <- function(ind, 
                     comm_synaflokkur_group = comm_synaflokkur_group,
                     surv_synaflokkur = Index_Synaflokkur[[Species]],
                     #ind refers to the name of one of many specific combos of man/GRIDCELL/area/vf combinations that are made by later arguments
 ###---These '_group' arguments define groupings by which each ALK and distribution should be split
                     month_group = data.frame(month = 1:12, 
                                            month_group = 't1'), 
                     GRIDCELL_group = data.frame(GRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, 
                                                 GRIDCELL_group = 'g1'),
                     area_group = data.frame(area = tbl(mar, 'reitmapping_original') %>% select(DIVISION) %>% distinct %>% collect(n=Inf) %>% unlist, 
                                             area_group = 'r1'),
                     synaflokkur_group = data.frame(synaflokkur = st %>% select(synaflokkur) %>% distinct %>% collect(n=Inf)) %>% mutate(synaflokkur_group = ifelse(synaflokkur == 30, 's2', ifelse(synaflokkur==35, 's3', 's1'))),
                     #strata_group = data.frame(strata = NA, strata_group = NA), 
                     #above currently not implemented but would be nice to do this in the future
                    vf_group = data.frame(vf = c('vsurv',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist), 
                                           vf_group = c('vsurv',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist)),
 ###----cond_group defines the length-weight relationship within each grouping, so must be available for all groupings
                    cond_group = expand.grid(condition = 0.01, 
                                             power = 3, 
                                             month_group = unique(month_group$month_group), 
                                             GRIDCELL_group = unique(GRIDCELL_group$GRIDCELL_group), 
                                             area_group = unique(area_group$area_group), 
                                             vf_group = unique(vf_group$vf_group),
                                             synaflokkur_group = unique(synaflokkur_group$synaflokkur_group)) %>% 
                    #synaflokkur s2, if based on synaflokkur 30 and s3 for 35, should be exclusive of vf_group 'vsurv' to keep survey and catch data separate
                    filter((!(synaflokkur_group %in% unlist(comm_synaflokkur_group)) & vf_group == 'vsurv') | (synaflokkur_group %in% unlist(comm_synaflokkur_group) & vf_group != 'vsurv') ),
 ###---ref_group should be a named list that defines which grouping should be the 'reference'. 
        #It defines the reference group used that is the best source of data for replacing data whe there are minimal data
        #month_group and synaflokkur_group not included in ref_group because time must be same as ind under consideration
                  ref_group = list(GRIDCELL_group = unique(GRIDCELL_group$GRIDCELL_group),
                                area_group = unique(area_group),
                                areas = all_areas,
                                vf_group = unique(vf_group$vf_group)),
 ###---these arguments are needed for creating ALK
                     age_minlength = tbl(mar,'age_minlength') %>% filter(species==Species) %>% collect(n=Inf) ,
                     ALK_wts = c(0.001, 0.01, 1)
                      ){

  if(purrr:::map( list(month_group, GRIDCELL_group, area_group, vf_group, cond_group), 
         function(x){!is.data.frame(x)} ) %>% 
     unlist %>% 
     any){print('Warning: group arguments must be data frames with 2 columns, first column named by what precedes the underscore in the argument name, second column with the whole argument name (e.g., month_group = data.frame(month = NA, month_group = NA))')}

  if(any(nchar(na.omit(unique(c(as.character(month_group$month_group), as.character(synaflokkur_group$synaflokkur_group)))))>2)){
    print('Warning: month_group and synaflokkur_group names must be no longer than 2 characters')
  }              

  ###---- Begin with 'global' data defined external to function:
  #apply ALK weight to the all-data scenario
  kv_c<-
    kv_c %>% 
    mutate(fjoldi = ifelse(comm_synaflokkur_group %>% 
                             purrr::map(function(x) grepl(x,ind)) %>% unlist %>% any,
                           ALK_wts[1],
                           0)) #kv_c defined as commercial data, gives a weight of 0 if computing for survey index
  

  ###---- Create reference group that will fill in data when not available (has a low weight which becomes more important when fewer data are available for a group)---###
  #note this could be done outside the function, but may be handy inside for automation
  
  #reference stations - timing and synaflokkur always match
    st_ref <- 
      st_c %>% 
      rename(month = man) %>% 
      left_join(month_group) %>% 
      left_join(GRIDCELL_group) %>% 
      left_join(area_group) %>% 
      left_join(vf_group) %>% 
      left_join(synaflokkur_group) %>%
      ungroup() %>% 
      unite(index,c(month_group,synaflokkur_group,area_group,GRIDCELL_group,vf_group),sep = '',remove = FALSE) %>% 
      filter(!is.na(month_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA' | !is.na(vf_group), !is.na(synaflokkur_group),
             month_group == str_sub(ind,start=1,end=2), synaflokkur_group == str_sub(ind,start=3,end=4), GRIDCELL_group %in% ref_group$GRIDCELL_group, 
             area_group %in% ref_group$area_group, area %in% ref_group$areas, vf_group %in% ref_group$vf_group)
  
    if(dim(st_ref)[1] < 10){print(paste0('Warning: n of reference group for ', ind, ' is < 10'))} 
    
    #age data for the reference group matching reference stations
    kv_ref <- 
        kv_c %>% 
        semi_join(st_ref) %>% 
        mutate(fjoldi = ALK_wts[2]) #increase weighting from base for reference group

    
    ###---- Gather ind-specific data ---###
   #These will be used to create ALK and distributions. 
    
    #catch for the ind grouping - used as a weight when combining catch data
    afli_ind <- 
      commcatch %>% 
      rename(month = man) %>% 
      left_join(month_group) %>% 
      left_join(GRIDCELL_group) %>% 
      left_join(area_group) %>% 
      left_join(vf_group) %>% 
      ungroup() %>% 
      unite(index,c(month_group,synaflokkur_group,area_group,GRIDCELL_group,vf_group),sep = '',remove = FALSE) %>% 
      filter(!is.na(month_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA' | !is.na(vf_group), !is.na(synaflokkur_group)) %>% 
      filter(index == ind) %>% 
      summarise(catch = sum(catch,na.rm=TRUE)) %>% 
      mutate(catch = catch/1000) %>% 
      .$catch
  
  #stations for the ind groupings
  
   st_ind <- 
     st_c %>% 
     rename(month = man) %>% 
     left_join(month_group) %>% 
     left_join(GRIDCELL_group) %>% 
     left_join(area_group) %>% 
     left_join(vf_group) %>% 
     mutate(vf = ifelse(synaflokkur %in% surv_synaflokkur, 'vsurv', vf)) %>% #
     left_join(synaflokkur_group) %>%
     filter(!is.na(month_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA' | !is.na(vf_group), !is.na(synaflokkur_group)) %>%       
     ungroup() %>% 
     unite(index,c(month_group,synaflokkur_group,area_group,GRIDCELL_group,vf_group),sep = '',remove = FALSE) %>%  
     filter(index == ind)
    
  #otolith data for ind-based subset - this has repeat entries with differen weights (fjoldi) for the ALK
  kv_ind <- 
    kv_c %>% 
    semi_join(st_ind) %>% 
    mutate(fjoldi = ALK_wts[3]) %>%  #ind subset data gets the highest weight
    bind_rows(kv_c) %>%  #lowest weight corresponds to all possible data
    bind_rows(kv_ref)       #next highest weight corresponds to 'reference' group
  #the ALK_wts specificaiton should be chosen carefully depending on how much data are actually available and how
  #spread they are among groups
  
  #length data for the ind-based subset
  le_ind <- 
    le_c %>% 
    semi_join(st_ind) 
  
  #cond data for the ind-based subset
  cond_ind <- 
   cond_group %>% 
   unite(index,c(month_group,synaflokkur_group,area_group,GRIDCELL_group,vf_group),sep = '',remove = FALSE) %>% 
   filter(index == ind) %>% 
   select(condition,power) %>% 
   unlist()
  
  #create keys and output
  if(kv_ind$lengd %>% unique %>% length < 4){
    # for years when there are no data, this just fill in NAs
    medalle <- (ldist_bins[[Species]][-length(ldist_bins[[Species]])] + ldist_bins[[Species]][-1])/2
    n <- length(medalle)
    n1 <- length(age_minlength$age)
    tmp.data <- data.frame(aldur = rep(age_minlength$age[1], n), lengd = medalle, 
                           Kynth = rep(0, n), kyn = rep(1, n), fjoldi = rep(0, n))
    tmp.data <- rbind(tmp.data, data.frame(aldur = age_minlength$age, lengd = rep(medalle[1], 
                                                                      n1), Kynth = rep(1, n1), kyn = rep(2, n1), fjoldi = rep(0, 
                                                                                                                              n1)))
    tmp.data$lenfl <- cut(tmp.data$lengd, breaks = ldist_bins[[Species]])
    OTH.TOT <- tapply(tmp.data$fjoldi, list(tmp.data$lenfl, tmp.data$aldur), 
                      sum)
    OTH.TOT[,] <- NA
    
    keys <- list(ALK.TOT = OTH.TOT)

  } else {
  keys <- MakeAlk(kv_ind,Species,kynth=F,
                  lengd=ldist_bins[[Species]],aldur=age_minlength$age,
                  Stodvar=st_c,
                  FilterAldurLengd=FALSE)
  }
  ###distributions formed differently based on from catch or surveys
  ###These will need to be modified if distributions should be 'kyn' or 'kynth'-based
  
  print(ind)
  if(comm_synaflokkur_group %>% 
                      purrr::map(function(x) grepl(x,ind)) %>% unlist %>% any)
    {
    distributions <- MakeLdist(Species,
                               lengd=ldist_bins[[Species]],
                               Stodvar=st_c,
                               lengdir=le_ind,
                               lengd.thyngd.data=cond_ind,
                               talid=F,afli=afli_ind)
    } else {
      lims <- data.frame(l = ldist_bins[[Species]][-length(ldist_bins[[Species]])],
                       u = ldist_bins[[Species]][-1])   
      distributions <- list()
              
      #skip years of no survey data  
#      if(((tyr < 1996 | tyr==2011) & (35 == (synaflokkur_group %>% 
#                                            filter(synaflokkur_group==str_sub(ind,start = 3, end = 4)) %>%
#                                            select(synaflokkur) %>%
#                                            unlist)) %>% any) |
#         (tyr < 1985  & (30 == (synaflokkur_group %>% 
#                                            filter(synaflokkur_group==str_sub(ind,start = 3, end = 4)) %>%
#                                            select(synaflokkur) %>%
#                                            unlist)) %>% any) 
#         ){
#     Below should be more general but cover the above subsets. The point is to skip any combos
#     where there are no data so that calc_index is avoided.
      if(!((synaflokkur_group %>% 
            filter(synaflokkur_group==str_sub(ind,start = 3, end = 4)) %>%
            select(synaflokkur) %>%
            unlist) %in% (st_ind %>% select(synaflokkur) %>% distinct) %>% any)){
          
          dist.tmp <- list()
          dist.tmp$n <- dist.tmp$b <- dist.tmp$ml <- rep(NA,  length(ldist_bins[[Species]])-1)
        
        } else {
        
          try(dist.tmp <-
                as.list(lims$l) %>% 
                set_names(.,.) %>% 
                purrr::map(function(x) x <- as.vector(unlist(lims[lims$l==x,]))) %>% 
                calc_index(mar, length_ranges = .) %>% 
                filter(ar == tyr,
                       synaflokkur == synaflokkur_group %>% 
                         filter(synaflokkur_group == str_sub(ind,start = 3, end = 4)) %>% 
                         select(synaflokkur) %>% 
                         unlist
                ) %>% 
                right_join(lims %>% 
                             mutate(index = as.character(l)) %>% 
                             select(index)), silent = T)
        }
          
        if(length(dist.tmp$n)==0){ # likely due to no data
          dist.tmp <- list()
          dist.tmp$n <- dist.tmp$b <- dist.tmp$ml <- rep(NA, length(ldist_bins[[Species]])-1)
        } 
          
        distributions$LDIST.ALLS <- dist.tmp$n 
        distributions$MEANWT.ALLS <- dist.tmp$b / dist.tmp$n * 1000
        distributions$MEANLE <- dist.tmp$ml
        
    }
  
  #fj2016allirsynaflokkar1reg$v1s1t1 <- Calc.fj(keys,tmp3)
  return(Calc.fj(keys,distributions))
}

# ------------------------------------------------------------------------------
###----Use calc_all function with aggregations defined above----###

  


#fjallirsynaflokkar1reg <- 
dist_and_keys <-
  commcatch_groupings %>%
  select(index) %>% 
  bind_rows(data.frame(index = surv_ind)) %>% 
  unlist() %>% 
  as.list() %>% 
  set_names(.,.) %>% 
  purrr::map(calc_all, 
             comm_synaflokkur_group = comm_synaflokkur_group,
             surv_synaflokkur = Index_Synaflokkur[[Species]],
             month_group = month_group, 
             GRIDCELL_group = GRIDCELL_group, 
             area_group = area_group, 
             vf_group = vf_group, 
             synaflokkur_group = synaflokkur_group, 
             cond_group = cond_group, 
             ref_group = ref_group,
             age_minlength = tbl(mar,'age_minlength') %>% filter(species==Species) %>% collect(n=Inf) ,
             ALK_wts = c(0.001, 0.01, 1))

catch_by_age <- 
# total_FjPerAldur <-
#  fjallirsynaflokkar1reg %>% 
  dist_and_keys %>% 
  keep(grepl(comm_synaflokkur_group[[1]], names(.))) %>% 
  purrr::map(function(x) {
    as.list(x) %>% 
      keep(names(x) %in% c('FjPerAldur', 'BiomassPerAldur')) %>%
      bind_rows()
    }) %>% 
#  purrr::map(unlist) %>% 
#  purrr::map(t) %>% 
#  purrr::map(as.data.frame) %>%
  purrr::map(~mutate(.,age = rownames(.))) %>% 
  bind_rows(.id = 'part') %>% 
  as_data_frame() %>% 
  group_by(age) %>% 
  summarise(numbers = sum(FjPerAldur), biomass = sum(BiomassPerAldur)) %>% 
  ungroup() %>% 
  mutate(age = as.numeric(age), 
         cwt = biomass/numbers, 
         year = tyr,
         rat = totcatch/sum(biomass),
         cno = numbers*rat/1000,
         ctons = round(biomass*rat/1000,1),
         prosentno = cno/sum(cno)*100,
         prosentwt = ctons/sum(ctons)*100
         ) %>% 
  arrange(age) %>% 
  left_join(dist_and_keys %>% 
              keep(!grepl(comm_synaflokkur_group[[1]], names(.))) %>% 
              purrr::map(function(x) {
                as.list(x) %>% 
                  keep(names(x) %in% c('FjPerAldur', 'BiomassPerAldur','WtPerAldur')) %>% 
                  bind_rows()
                }) %>% 
              purrr::map(~mutate(.,age = rownames(.))) %>% 
              bind_rows(.id = 'part') %>%
              mutate(part = str_sub(part, 3,4)) %>% 
              gather(key = 'type', value = 'amount', -c(age, part)) %>%
              mutate(type = ifelse(grepl('Fj', type), 'sno', ifelse(grepl('Wt', type), 'swt', 'sbio'))) %>% 
              unite(index, part,type, sep = '_') %>% 
              spread(key = 'index', value = 'amount') %>% 
              as_data_frame() %>% 
              mutate(age = as.numeric(age)))

#will be renamed after going through later code to make sure I can recognize it

mat <-
  lesa_kvarnir(mar) %>% 
  filter(tegund == Species, !is.na(kynthroski), !is.na(aldur)) %>% 
  semi_join(lesa_stodvar(mar) %>% 
            filter(ar == tyr)  %>%
            filter(synaflokkur %in% mat_surv)) %>% 
  mutate(Mature = ifelse(kynthroski %in% mat_codes, 'Mat', 'Imm')) %>% 
  group_by(aldur,Mature) %>% 
  summarise(m_n = n()) %>% 
  left_join(kv %>% 
              filter(!is.na(kynthroski), !is.na(aldur)) %>% 
              group_by(aldur) %>% 
              summarise(tot_n = n())) %>% 
  mutate(prop = m_n/tot_n) %>% 
  arrange(aldur, Mature) %>% 
  filter(Mature == 'Mat') %>% 
  select(age = aldur, mat = prop) %>% 
  collect(n=Inf) %>% 
  full_join(data_frame(age = catch_by_age$age)) %>%
  filter(age %in% catch_by_age$age) %>% 
  arrange(age) 

catch_by_age<-
  catch_by_age %>% 
  left_join(mat %>% 
            left_join(data.frame(age = catch_by_age$age, 
                                 mat_sub = c(0,unlist(mat[,'mat'])[-length(unlist(mat[,'mat']))]))) %>% 
            mutate(mat = ifelse(is.na(mat), mat_sub, mat)) #fills in mat at age based on next youngest age if no data available))
  ) %>% 
  select(-mat_sub)

#Doesn't this repeat?
# totfjallirsynaflokkar1reg <- fjallirsynaflokkar1reg[[1]]$FjPerAldur        
# for(i in 2:length(fjallirsynaflokkar1reg)) 
#    totfjallirsynaflokkar1reg <- totfjallirsynaflokkar1reg+fjallirsynaflokkar1reg[[i]]$FjPerAldur  

#no longer necessary
#tmp <- fjallirsynaflokkar1reg[[1]] 
#tmp <- dist_and_keys[[1]]
#for(i in 2:length(dist_and_keys))
#for(i in 2:length(dist_and_keys))
#    tmp <- husky:::rbind.alk1(tmp, fjallirsynaflokkar1reg[[i]]) 
#    tmp <- husky:::rbind.alk1(tmp, dist_and_keys[[i]]) 
#wt_by_age <- tmp$BiomassPerAldur/apply(tmp$FjPerAldur,2,sum)

#no longer necessary
#catchallirsynaflokkar1reg <- 
# catch_by_age <-
#   data.frame(age= tbl(mar,paste0('age_minlength_',Species)) %>% collect(n=Inf) %>% select(age) %>% unlist,
#              numbers=c(total_catch_numbers_by_age),
#              weight=c(wt_by_age),
#              biomass=c(tmp$BiomassPerAldur),
#              year = tyr) %>% 
#   select(-numbers.age)

#no longer necessary - done above
# Something important I can't remember
# Skala með hlutfalli heildarafla og þeirra sella sem voru teknar með. c.a 1.2% hækkun
#rat <- totcatch/sum(catchallirsynaflokkar1reg$bio)
#rat <- totcatch/sum(catch_by_age$biomass)
#catchallirsynaflokkar1reg$fj <- 
#  catch_by_age$numbers <-
#  catchallirsynaflokkar1reg$fj*rat/1000
#  catch_by_age$numbers*rat/1000
#  catchallirsynaflokkar1reg$bio <-
#  catch_by_age$biomass <-
#  round(catchallirsynaflokkar1reg$bio*rat/1000,1)
#  round(catch_by_age$biomass*rat/1000,1)
  #catch2016allirsynaflokkar1reg$fj <- 
#  round(catch2016allirsynaflokkar1reg$fj,3)
#catch2016allirsynaflokkar1reg$wt <- 
#  round(catch2016allirsynaflokkar1reg$wt)
#names(catch_by_age) <- c("age","cno","cwt","ctons","years")
#names(catchallirsynaflokkar1reg) <- c("age","cno","cwt","ctons")
#catchallirsynaflokkar1reg$prosentno <- 
#catch_by_age$prosentno <-
#  catch_by_age$cno/sum(catch_by_age$cno)*100
#catchallirsynaflokkar1reg$cno/sum(catch2016allirsynaflokkar1reg$cno)*100
#catchallirsynaflokkar1reg$prosentwt <- 
#catch_by_age$prosentwt <-
#  catch_by_age$ctons/sum(catch_by_age$ctons)*100
#  catchallirsynaflokkar1reg$ctons/sum(catch2016allirsynaflokkar1reg$ctons)*100


#Not sure why this is done - probably to double check that catch levels are accurate? 
#I suppose those with catch_res as 0s but actual catch > 0 have no age-length data with them
#so do not contribute to the catch distribution formation

#catch <- rep(0,length(fj2016allirsynaflokkar1reg)) 
catch_results <- rep(0,length(dist_and_keys %>% 
                                         keep(grepl(comm_synaflokkur_group[[1]], names(.))))) 
# for(i in 1:length(fj2016allirsynaflokkar1reg)) 
#   catch[[i]] <- sum(fj2016allirsynaflokkar1reg[[i]]$BiomassPerAldur)
# catch <- data.frame(index=names(fj2016allirsynaflokkar1reg),catch=round(catch/1000))
for(i in 1:length(dist_and_keys %>% 
                  keep(grepl(comm_synaflokkur_group[[1]], names(.))))) 
  catch_results[[i]] <- sum(dist_and_keys %>% 
                              keep(grepl(comm_synaflokkur_group[[1]], names(.))) %>% 
                              .[[i]] %>% 
                              as.list() %>% 
                              .$BiomassPerAldur)
catch_results_by_index <- data.frame(index=names(dist_and_keys %>% keep(grepl(comm_synaflokkur_group[[1]], names(.)))),catch_res=round(catch_results/1000))


commcatch_groupings %>% 
  mutate(catch = catch/1000) %>% 
  left_join(catch_results_by_index)

#I don't think below is used because st2016kv and st2016.karnad are commented out earlier
#x <- geo::apply.shrink(rep(1,nrow(st2016kv)),st2016kv$index,sum,names=c("index","fj.aged"))
#x1 <- geo::apply.shrink(rep(1,nrow(st2016.kvarnad)),st2016.kvarnad$index,sum,names=c("index","fj.othsamp"))
#catch <- fjolst:::join(catch,x,"index")
#catch <- fjolst:::join(catch,x1,"index")
#catch$index <- as.character(catch$index)

#Not sure what this isused for either so saving for later - perhaps to make output
# tmp <- read.table("/home/hoski/Tac2016/02/Ass/Adapt/PrognosisBaediroll/resultsbyyearandage",header=T)
# tmp <- tmp[tmp$year==2016 & tmp$age > 1,c("age","CalcCno","CatchWts")]
# names(tmp) <- c("age","predcno","predwts")
# tmp$predcno <- tmp$predcno
# tmp$predpercno <- round(tmp$predcno/sum(tmp$predcno)*100,1)
# catchallirsynaflokkar1reg <- fjolst:::join( catchallirsynaflokkar1reg,tmp,"age")
# tmp <- round(catch2016allirsynaflokkar1reg,2)

  
dir.create(paste0(yr_dir, '/', 'catch_at_age_rdata'))
#save(list=c("catchallirsynaflokkar1reg","fjallirsynaflokkar1reg"),file="cnoallirsynaflokkar.rdata")
save(list=c("catch_by_age","dist_and_keys"),file=paste0(yr_dir, '/', 'catch_at_age_rdata', '/', tyr, "_", Species,"_catch_at_age.rdata"))

