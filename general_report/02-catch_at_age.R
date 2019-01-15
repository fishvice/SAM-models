#foreigncatch <- 0 ???

#Note this script is needed to generate ALK for a single year (tyr). When setting up a model for the first time,
#it will be necessary to run this for all years, then each year thereafter just the final year can be run.

# ------------------------------------------------------------------------------
###----VARIABLE SETTINGS FOR PRODUCING ALK----###
#these are inputs to the function below used to create keys within each defined grouping

# all_areas is all possible areas, 
# global_areas is the full region for consideration for the species
global_areas <- 101:108 #for haddock
all_areas <- tbl(mar, 'reitmapping_original') %>% select(DIVISION) %>% distinct %>% collect(n=Inf) %>% unlist

#Basic information for ALK - age and min length possible for that age
data.frame(age=1:14,minlength=15+2.5*(1:14)) %>% 
  dbWriteTable(mar,paste0('age_minlength_',Species),.,overwrite=TRUE)

#done for haddock
#Aggregations needed by which ALK will be specific
month_group <-data.frame(month = 1:12, month_group = c(rep('t1',5),rep('t2',7)))
GRIDCELL_group <-data.frame(GRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, GRIDCELL_group = 'g1')
area_group <- data.frame(area = all_areas) %>% mutate(area_group = ifelse(area %in% 102:105, 'r1', 'r2'))
#strata_group <- data.frame(strata = NA, strata_group = NA)#- to be implemented later
#above currently not implemented because strata not joined with catch synis_id
#but would be nice to do this in the future
vf_group <- data.frame(vf = c('v3035',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist), 
                      vf_group = c('v3035',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist))
#best to always keep s1 refer to synaflokkur 30 and 35; s2 refer to everything else
synaflokkur_group <- data.frame(synaflokkur = lesa_stodvar(mar) %>% filter(ar == tyr) %>% select(synaflokkur) %>% distinct %>% collect(n=Inf)) %>% 
                      mutate(synaflokkur_group = ifelse(synaflokkur %in% c(30,35), 's1', 's2'))
#NOT IMPLEMENTED YET - because saw an attempt based on a subset synaflokkur = c(1,2,4,8)
#strata_group <- #NOT IMPLEMENTED YET - because it may be handy at some point to match catches to strata?

#Condition matrix to translate from weights. This needs to be as specific as the groups above are defined to.
cond_group<-expand.grid(condition = 0.00885, 
                      power = 3.02857, 
                      month_group = c('t1','t2'), 
                      GRIDCELL_group = 'g1', 
                      area_group = c('r1','r2'), 
                      vf_group = unique(vf_group$vf_group),
                      synaflokkur_group = unique(synaflokkur_group$synaflokkur_group)) %>% 
  filter((synaflokkur_group == 's1' & vf_group == 'v3035') | (synaflokkur_group != 's1' & vf_group != 'v3035') )

#month_group and synaflokkur_group not included in ref_group because time must be same as ind under consideration
ref_group <- list(GRIDCELL_group = unique(GRIDCELL_group$GRIDCELL_group),
                  area_group = 'r1',
                  vf_group = unique(vf_group$vf_group))

# ------------------------------------------------------------------------------
###----Gather catch, age, length data----###

# all station information 
st <- 
  lesa_stodvar(mar) %>% 
  mutate(GRIDCELL = 10*reitur + smareitur) %>% 
  filter(ar == tyr) %>% 
  #inner_join(tbl(mar,'husky_gearlist')) %>% #AT THIS STEP SURVEY DATA ARE REMOVED???
  left_join(tbl(mar,'husky_gearlist')) %>%  
  rename(vf = geartext) %>% 
  mutate(vf = ifelse(synaflokkur %in% c(30,35), 'v3035', vf)) %>% 
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

#landings data by gear
landings <- 
  lods_oslaegt(mar) %>% 
  inner_join(tbl(mar,'husky_gearlist')) %>% 
  rename(vf = geartext) %>% 
  filter(#veidarfaeri == 1,
    fteg == Species,
    ar == tyr,
    veidisvaedi == 'I') %>% 
  group_by(vf) %>% 
  summarise(landings = sum(magn_oslaegt)) 

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

#.... by gear and compared with landings
sc <- 
  catch %>% 
  group_by(vf) %>% 
  summarise(catch = sum(afli)) %>% 
  left_join(landings)

#.... corrected by the discrepancy between landings and catch specific to gear types...
commcatch <- 
  catch %>% 
  left_join(sc) %>% 
  mutate(afli = afli*landings/catch) %>% #discrepancy correction
  mutate(synaflokkur_group = 's2') %>% # 'commercial' synaflokkur - helps with later groupings - 
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
  
#total landings
totcatch <- 
  landings <- 
  lods_oslaegt(mar) %>% 
  filter(fteg == Species,
    ar == tyr,
    veidisvaedi == 'I') %>% 
  summarise(landings = sum(magn_oslaegt,na.rm=TRUE)) %>% 
  collect(n=Inf) %>% 
  .$landings

#passed globally into function
st_c <- st %>% collect(n=Inf)   
kv_c <- kv %>% collect(n=Inf)
le_c <- le %>% collect(n=Inf)  


# ------------------------------------------------------------------------------
###----Define calc_all function to use data defined above----###


#should weights be added to some or all data frames?
#age_minlength argument needs to be updated with appropriate ages and the minimum lengths used to clean the data
#group arguments need to be updated with the aggregations within which ALKs should be made
#putting NA in the group column excludes the corresponding data
calc_all <- function(ind, 
                     #ind refers to the name of one of many specific combos of man/GRIDCELL/area/vf combinations that are made by later arguments
 ###---These '_group' arguments define groupings by which each ALK and distribution should be split
                     month_group = data.frame(month = 1:12, 
                                            month_group = 't1'), 
                     GRIDCELL_group = data.frame(GRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, 
                                                 GRIDCELL_group = 'g1'),
                     area_group = data.frame(area = tbl(mar, 'reitmapping_original') %>% select(DIVISION) %>% distinct %>% collect(n=Inf) %>% unlist, 
                                             area_group = 'r1'),
                     synaflokkur_group = data.frame(synaflokkur = st %>% select(synaflokkur) %>% distinct %>% collect(n=Inf)) %>% mutate(synaflokkur_group = ifelse(synaflokkur %in% c(30,35), 's1', 's2')),
                     #strata_group = data.frame(strata = NA, strata_group = NA), 
                     #above currently not implemented but would be nice to do this in the future
                    vf_group = data.frame(vf = c('v3035',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist), 
                                           vf_group = c('v3035',tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist)),
 ###----cond_group defines the length-weight relationship within each grouping, so must be available for all groupings
                    cond_group = expand.grid(condition = 0, 
                                             power = 3, 
                                             month_group = unique(month_group$month_group), 
                                             GRIDCELL_group = unique(GRIDCELL_group$GRIDCELL_group), 
                                             area_group = unique(area_group$area_group), 
                                             vf_group = unique(vf_group$vf_group),
                                             synaflokkur_group = unique(synaflokkur_group$synaflokkur_group)) %>% 
                    #synaflokkur s1, if based on synaflokkur 30 and 35, should be exclusive of vf_group 'v3035' to keep survey and catch data separate
                    filter((synaflokkur_group == 's1' & vf_group == 'v3035') | (synaflokkur_group != 's1' & vf_group != 'v3035') ),
 ###---ref_group should be a named list that defines which grouping should be the 'reference'. 
        #It defines the reference group used that is the best source of data for replacing data whe there are minimal data
        #month_group and synaflokkur_group not included in ref_group because time must be same as ind under consideration
                  ref_group = list(GRIDCELL_group = unique(GRIDCELL_group$GRIDCELL_group),
                                area_group = unique(area_group),
                                vf_group = unique(vf_group$vf_group)),
 ###---these arguments are needed for creating ALK
                     age_minlength = tbl(mar,paste0('age_minlength_',Species)) %>% collect(n=Inf) ,
                     ALK_wts = c(0.001, 0.01, 1)
                      ){

  if(purrr:::map( list(month_group, GRIDCELL_group, area_group, vf_group, cond_group), 
         function(x){!is.data.frame(x)} ) %>% 
     unlist %>% 
     any){print('Warning: group arguments must be data frames with 2 columns, first column named by what precedes the underscore in the argument name, second column with the whole argument name (e.g., month_group = data.frame(month = NA, month_group = NA))')}

  if(any(nchar(unique(c(as.character(month_group$month_group), as.character(synaflokkur_group$synaflokkur_group))))>2)){
    print('Warning: month_group and synaflokkur_group names must be no longer than 2 characters')
  }              

  ###---- Begin with 'global' data defined external to function:
  #apply ALK weight to the all-data scenario
  kv_c<-
    kv_c %>% 
    mutate(fjoldi = ALK_wts[1])

  ###---- Create reference group that will fill in data when not available (has a low weight which becomes more important when fewer data are available for a group)---###
  #note this could be done outside the function, but may be handy inside for automation
  
  #reference stations
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
             area_group %in% ref_group$area_group, vf_group %in% ref_group$vf_group)
  
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
     mutate(vf = ifelse(synaflokkur %in% c(30,35), 'v3035', vf)) %>% 
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
  keys <- MakeAlk(kv_ind,Species,kynth=F,
                  lengd=age_minlength$minlength,aldur=age_minlength$age,
                  Stodvar=st_c,
                  FilterAldurLengd=FALSE)
  ###START HERE TO MODIFY HOW LENGTH DISTRIBUTIONS FORMED FROM SURVEY VERSUS CATCH
  distributions <- MakeLdist(Species,lengd=age_minlength$minlength,
                    Stodvar=st_c,
                    lengdir=le_ind,
                    lengd.thyngd.data=cond_ind,
                    talid=F,afli=afli_ind)
  #fj2016allirsynaflokkar1reg$v1s1t1 <- Calc.fj(keys,tmp3)
  return(Calc.fj(keys,distributions))
}


# ------------------------------------------------------------------------------
###----Use calc_all function with aggregations defined above----###

#will be renamed after going through later code to make sure I can recognize it

#fjallirsynaflokkar1reg <- 
dist_and_keys <-
  commcatch_groupings %>%
  select(index) %>% 
  unlist() %>% 
  as.list() %>% 
  set_names(.,.) %>% 
  purrr::map(calc_all, 
             month_group = month_group, 
             GRIDCELL_group = GRIDCELL_group, 
             area_group = area_group, 
             vf_group = vf_group, 
             synaflokkur_group = synaflokkur_group, 
             cond_group = cond_group, 
             ref_group = ref_group)

totfjallirsynaflokkar1reg <- 
# total_FjPerAldur <-
  fjallirsynaflokkar1reg %>% 
  purrr::map('FjPerAldur') %>% 
  purrr::map(unlist) %>% 
  purrr::map(t) %>% 
  purrr::map(as.data.frame) %>%
  purrr::map(~mutate(.,age = rownames(.))) %>% 
  bind_rows(.id = 'part') %>% 
  as_data_frame() %>% 
  group_by(age) %>% 
  summarise(total = sum(V1)) %>% 
  mutate(age = as.numeric(age)) %>% 
  arrange(age)

#Doesn't this repeat?
# totfjallirsynaflokkar1reg <- fjallirsynaflokkar1reg[[1]]$FjPerAldur        
# for(i in 2:length(fjallirsynaflokkar1reg)) 
#    totfjallirsynaflokkar1reg <- totfjallirsynaflokkar1reg+fjallirsynaflokkar1reg[[i]]$FjPerAldur  

tmp <- fjallirsynaflokkar1reg[[1]] 
#tmp <- dist_and_keys[[1]]
for(i in 2:length(fjallirsynaflokkar1reg))
#for(i in 2:length(dist_and_keys))
    tmp <- husky:::rbind.alk1(tmp, fjallirsynaflokkar1reg[[i]]) 
#    tmp <- husky:::rbind.alk1(tmp, dist_and_keys[[i]]) 
wt <- tmp$BiomassPerAldur/apply(tmp$FjPerAldur,2,sum)

catchallirsynaflokkar1reg <- 
#catch_by_age <-
  data.frame(age= tbl(mar,paste0('age_minlength_',Species)) %>% collect(n=Inf) %>% select(age) %>% unlist,
             fj=c(totfjallirsynaflokkar1reg),
             wt=c(wt),
             bio=c(tmp$BiomassPerAldur))

# Something important I can't remember
# Skala með hlutfalli heildarafla og þeirra sella sem voru teknar með. c.a 1.2% hækkun
rat <- totcatch/sum(catchallirsynaflokkar1reg$bio)
catchallirsynaflokkar1reg$fj <- 
  #catch_by_age$fj <-
  catchallirsynaflokkar1reg$fj*rat/1000
catchallirsynaflokkar1reg$bio <-
  #catch_by_age$bio <-
  round(catchallirsynaflokkar1reg$bio*rat/1000,1)
#catch2016allirsynaflokkar1reg$fj <- 
#  round(catch2016allirsynaflokkar1reg$fj,3)
#catch2016allirsynaflokkar1reg$wt <- 
#  round(catch2016allirsynaflokkar1reg$wt)
names(catchallirsynaflokkar1reg) <- c("age","cno","cwt","ctons")
#names(catch_by_age) <- c("age","cno","cwt","ctons")
catchallirsynaflokkar1reg$prosentno <- 
#catch_by_age$prosentno <-
  catchallirsynaflokkar1reg$cno/sum(catch2016allirsynaflokkar1reg$cno)*100
catchallirsynaflokkar1reg$prosentwt <- 
#catch_by_age$prosentwt <-
  catchallirsynaflokkar1reg$ctons/sum(catch2016allirsynaflokkar1reg$ctons)*100



catch <- rep(0,length(fj2016allirsynaflokkar1reg)) 
#catch_res <- rep(0,length(dist_and_keys)) 
for(i in 1:length(fj2016allirsynaflokkar1reg)) 
  catch[[i]] <- sum(fj2016allirsynaflokkar1reg[[i]]$BiomassPerAldur)
catch <- data.frame(index=names(fj2016allirsynaflokkar1reg),catch=round(catch/1000))
#for(i in 1:length(dist_and_keys)) 
#  catch_res[[i]] <- sum(dist_and_keys[[i]]$BiomassPerAldur)
#catch_res <- data.frame(index=names(dist_and_keys),catch_res=round(catch_res/1000))

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

save(list=c("catchallirsynaflokkar1reg","fjallirsynaflokkar1reg"),file="cnoallirsynaflokkar.rdata")
#save(list=c("catch_by_age","dist_and_keys"),file=paste0(Species,"_catch_at_age.rdata"))
