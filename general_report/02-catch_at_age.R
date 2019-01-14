#foreigncatch <- 0 ???

#Note this script is needed to generate ALK for a single year (tyr). When setting up a model for the first time,
#it will be necessary to run this for all years, then each year thereafter just the final year can be run.

# ------------------------------------------------------------------------------
###----VARIABLE SETTINGS FOR PRODUCING ALK----###
#these are inputs to the function below used to create keys within each defined grouping

# all_areas is all possible areas, 
# focal_areas is the full region for consideration for the species
focal_areas <- 101:108 #for haddock
all_areas <- tbl(mar, 'reitmapping_original') %>% select(DIVISION) %>% distinct %>% collect(n=Inf) %>% unlist

#Basic information for ALK - age and min length possible for that age
data.frame(age=1:14,minlength=15+2.5*(1:14)) %>% 
  dbWriteTable(mar,paste0('age_minlength_',Species),.,overwrite=TRUE)

#Aggregations needed by which ALK will be specific
month_group <-data.frame(man = 1:12, man_group = 't1')
GRIDCELL_group <-data.frame(GRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, GRIDCELL_group = 'g1')
area_group <- data.frame(area = all_areas, area_group = 'r1')
#strata_group <- data.frame(strata = NA, strata_group = NA)#- to be implemented later
#above currently not implemented because strata not joined with catch synis_id
#but would be nice to do this in the future
vf_group <- data.frame(vf = tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist, 
                      vf_group = tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist)
#synaflokkur_group <- #NOT IMPLEMENTED YET - because saw an attempt based on a subset synaflokkur = c(1,2,4,8)
#strata_group <- #NOT IMPLEMENTED YET - because it may be handy at some point to match catches to strata?

#Condition matrix to translate from weights. This needs to be as specific as the groups above are defined to.
cond_mat<-data.frame(condition = 0.00885, power = 3.02857, man_group = 'm1', GRIDCELL_group = 'g1', area_group = 'a1', vf_group = vf_group$vf_group)


# ------------------------------------------------------------------------------
###----Gather catch, age, length data----###

# all station information 
st <- 
  lesa_stodvar(mar) %>% 
  mutate(GRIDCELL = 10*reitur + smareitur) %>% 
  filter(ar == tyr) %>% 
  inner_join(tbl(mar,'husky_gearlist')) %>% 
  rename(vf = geartext) %>% 
  inner_join(tbl(mar,'reitmapping_original')) %>% 
  rename(area = DIVISION) %>% 
  filter(area %in% focal_areas) %>% 
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
  filter(area %in% focal_areas) #%>% 

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
  group_by(vf, man) %>% 
  filter(!is.na(afli)) %>% 
  mutate(catch = afli) %>% 
  collect(n=Inf) #%>% 

#... and summarized by vf, time, reg, for viewing purposes
commcatch_groupings<-
  commcatch %>% 
  left_join(month_group) %>% 
  left_join(GRIDCELL_group) %>% 
  left_join(area_group) %>% 
  left_join(vf_group) %>% 
  filter(!is.na(man_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA') %>%       
  ungroup() %>% 
  unite(index,c(vf_group,area_group,man_group,GRIDCELL_group),sep = '',remove = FALSE) %>% 
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

st_c <- st %>% collect(n=Inf)   
kv_c <- kv %>% collect(n=Inf)
le_c <- le %>% collect(n=Inf)  


#this should be redundant
# tmp <- 
#   st_c %>% 
#   filter(synis.id %in% c(le_c$synis.id,kv_c$synis.id))
## Southern area has a small weight

# ------------------------------------------------------------------------------
###----Define calc_all function to use data defined above----###


#should weights be added to some or all data frames?
#age_minlength argument needs to be updated with appropriate ages and the minimum lengths used to clean the data
#group arguments need to be updated with the aggregations within which ALKs should be made
#putting NA in the group column excludes the corresponding data
calc_all <- function(ind, 
                     #ind refers to the name of one of many specific combos of man/GRIDCELL/area/vf combinations that are made by later arguments
                     age_minlength = tbl(mar,paste0('age_minlength_',Species)) %>% collect(n=Inf) ,
                     month_group = data.frame(man = 1:12, 
                                            man_group = 't1'), 
                     GRIDCELL_group = data.frame(GRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, 
                                                 GRIDCELL_group = 'g1'),
                     area_group = data.frame(area = tbl(mar, 'reitmapping_original') %>% select(DIVISION) %>% distinct %>% collect(n=Inf) %>% unlist, 
                                             area_group = 'r1'),
                     #synaflokkur_group = data.frame(synaflokkur = NA, synaflokkur_group),
                     #strata_group = data.frame(strata = NA, strata_group = NA), 
                     #above currently not implemented but would be nice to do this in the future
                     vf_group = data.frame(vf = tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist, 
                                           vf_group = tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist),
                     cond_group = data.frame(condition = 0.00885, 
                                             power = 3.02857, 
                                             man_group = 't1', 
                                             GRIDCELL_group = 'g1', 
                                             area_group = 'r1', 
                                             vf_group = vf_group$vf_group),
                     #above defines the length-weight relationship within each grouping
                     ref_group = list(),
                     ALK_wts = c(0.001, 0.01, 1)
                     #above defines the reference group used that is the best source of data for replacing data whe there are minimal data
                      ){

  if(purrr:::map( list(month_group, GRIDCELL_group, area_group, vf_group, cond_group), 
         function(x){!is.data.frame(x)} ) %>% 
     unlist %>% 
     any){print('Warning: group arguments must be data frames with 2 columns, first column named by what precedes the underscore in the argument name, second column with the whole argument name (e.g., man_group = data.frame(man = NA, man_group = NA))')}
      
    #for haddock, the ref argument should be ref = list(area = 102:105, man = something) 
    # tmpa  <- 
    #     tmp %>% 
    #     filter(area %in% 102:105, 
    #            man %in% per)
  
  #perhaps there's a better way to do this bit below
  ref <- list(man = 1:12, 
                   GRIDCELLGRIDCELL = tbl(mar, 'reitmapping_original') %>% select(GRIDCELL) %>% distinct %>% collect(n=Inf) %>% unlist, 
                   area = tbl(mar, 'reitmapping_original') %>% select(DIVISION) %>% distinct %>% collect(n=Inf) %>% unlist, 
                   vf = tbl(mar, 'husky_gearlist') %>% select(geartext) %>% distinct %>% collect(n=Inf) %>% unlist)

  #convert from default all possibilities to a narrower range for the reference group definition
  if(length(ref_group) > 0){
    for(i in 1:length(ref_group)){
      ref[[names(ref_group)[i]]] <- ref_group[[i]]
    }
  }
 
  #apply ALK weight to the all-data scenario
  kv_c<-
    kv_c %>% 
    mutate(fjoldi = ALK_wts[1])

  ###---- Create reference group that will fill in data when not available (has a low weight which becomes more important when fewer data are available for a group)---###
  #note this could be done outside the function, but may be handy inside for automation
  
  #reference stations
    st_ref <- 
      st_c %>% 
      left_join(month_group) %>% 
      left_join(GRIDCELL_group) %>% 
      left_join(area_group) %>% 
      left_join(vf_group) %>% 
      filter(!is.na(man_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA',
             man_group %in% ref$man_group, GRIDCELL_group %in% ref$GRIDCELL_group, 
             area_group %in% ref$area_group, vf %in%vf_group) %>%       
      ungroup() %>% 
      unite(index,c(vf_group,area_group,man_group,GRIDCELL_group),sep = '',remove = FALSE) 
    
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
      left_join(month_group) %>% 
      left_join(GRIDCELL_group) %>% 
      left_join(area_group) %>% 
      left_join(vf_group) %>% 
      filter(!is.na(man_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA') %>% 
      ungroup() %>% 
      unite(index,c(vf_group,area_group,man_group,GRIDCELL_group),sep = '',remove = FALSE) %>% 
      filter(index == ind) %>% 
      summarise(catch = sum(catch,na.rm=TRUE)) %>% 
      mutate(catch = catch/1000) %>% 
      .$catch
  
  #stations for the ind grouping
  #To make code more general I switched st_ref to tmp in the following subsetting - this is to avoid the need for
  #the current ind-based subset to also be a subset of the reference group
  
  # st_tmp <- 
  #   st_ref %>% 
  #   filter(index == ind)
  
   st_ind <- 
     st_c %>% 
     left_join(month_group) %>% 
     left_join(GRIDCELL_group) %>% 
     left_join(area_group) %>% 
     left_join(vf_group) %>% 
     filter(!is.na(man_group), !is.na(GRIDCELL_group), !is.na(area_group), vf_group != 'NA') %>%       
     ungroup() %>% 
     unite(index,c(vf_group,area_group,man_group,GRIDCELL_group),sep = '',remove = FALSE) %>%  
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
   unite(index,c(vf_group,area_group,man_group,GRIDCELL_group),sep = '',remove = FALSE) %>% 
   filter(index == ind) %>% 
   select(condition,power) %>% 
   unlist()
  
  #create keys and output
  keys <- MakeAlk(kv_ind,Species,kynth=F,
                  lengd=age_minlength$minlength,aldur=age_minlength$age,
                  Stodvar=st_c,
                  FilterAldurLengd=FALSE)
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

fjallirsynaflokkar1reg <- 
#dist_and_keys <-
  commcatch_groupings %>%
  select(index) %>% 
  unlist() %>% 
  as.list() %>% 
  set_names(.,.) %>% 
  purrr::map(calc_all)

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
