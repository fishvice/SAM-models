library(patchwork)


#####-------Traditional indices-------#####
#Husky's results - will compare with indices created here in next section
## huskyverse
if(Sys.info()[['sysname']] == 'Windows'){
  load('//HAFKALDI/u2/reikn/R/SurveyWork/SMB/Allaggrsmbindex.rdata')
  load('//HAFKALDI/u2/reikn/R/SurveyWork/SMH/Allaggrsmhindex.rdata')
  
} else {
  load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Allaggrsmbindex.rdata')
  load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMH/Allaggrsmhindex.rdata')
}



smb.index <- 
  Allaggrsmbindex %>% 
  filter(svaedi == 'Heild', 
         species == Species,
         fixed == 1) %>% 
  mutate(fj.minni = fj.minni/1000,
         fj.minni.u = fj.minni*(1+cv.fj.minni),
         fj.minni.l = fj.minni*(1-cv.fj.minni),
         bio.staerri = bio.staerri/1e3,
         bio.u = bio.staerri*(1+cv.bio.staerri),
         bio.l = bio.staerri*(1-cv.bio.staerri)) %>% 
  as_data_frame()

smh.index <- 
  Allaggrsmhindex %>% 
  filter(svaedi == 'Heild', 
         species == Species,
         fixed == 0,
         skipShallow == 0) %>% 
  mutate(fj.minni = fj.minni/1000,
         fj.minni.u = fj.minni*(1+cv.fj.minni),
         fj.minni.l = fj.minni*(1-cv.fj.minni),
         bio.staerri = bio.staerri/1e3,
         bio.u = bio.staerri*(1+cv.bio.staerri),
         bio.l = bio.staerri*(1-cv.bio.staerri)) %>% 
  as_data_frame()


## total biomass plot

total_bio_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs$total[1]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y='Total biomass') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == cutoffs[1]),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs$total[1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l))
## biomass > 40 cm
b40_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs$B40[1]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y=paste0('Biomass > ',cutoffs$B40[1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs$B40[1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## biomass > 60

b60_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs$B60[1]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Biomass > ',cutoffs$B60[1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs$B60[1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## abundance < 30

a30_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs$juv[2]) %>% 
  ggplot(aes(ar,fj.minni)) + 
  geom_ribbon(aes(ymin=fj.minni.l,ymax=fj.minni.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Abundance < ',cutoffs$juv[2])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs$juv[2]), #was 40
                  aes(ar,fj.minni,ymax=fj.minni.u,ymin=fj.minni.l)) 

## 4-plot

four_plot_traditional <- 
  total_bio_plot + 
  b40_plot + 
  b60_plot + 
  a30_plot + 
  plot_layout(ncol=2)

#####-------Indices created with 01-length-based_indices.R-------#####
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
#max.towlength = max.towlength, Synaflokkur = Synaflokkur, Tognumer = Tognumer

calc_index <- function(mar, length_ranges = list(total = c(5,500), juv = c(5,30)), type = 'by.year'){

  #create index table for later
  ind <-
    names(length_ranges) %>% 
    as.list() %>% 
    set_names(.,.) %>% 
    purrr::map(.,function(x){
      y <-
        tbl(mar, paste0('raw_index_calc_',Species)) %>%
        select(lengd) %>%
        distinct %>% 
        mutate(ind = ifelse(lengd >= length_ranges[[x]][1] & lengd < length_ranges[[x]][2], 1, 0)) %>% 
        collect(n=Inf)
#      names(y)[2] <- x
#      return(y)
    }) #%>% 
    #Reduce(function(...) merge(..., by='lengd', all.x=TRUE), .)

  #This part is skipped because 01-length-based_indices.R produces values that are
  #summable up through the level of strata - already adjusted for area and # tows, etc.
  # 8. summarise by station ----------------------------------------------------
  
#  by.station <- 
  
#  tbl(mar, paste0('raw_index_calc_',Species)) %>% 
  # NOTE: here is the first step where statistics by length is dropped
#       some (minor) recoding above would be needed if one were to take things
#       forward by each length class
#group_by(synis_id, synaflokkur, strata, reitur, smareitur, tognumer, veidarfaeri, tegund, ar,lengd,area) %>% 
#  summarise(N = sum(N, na.rm = TRUE),
#            B = sum(B, na.rm = TRUE)) %>% 
  # Zero stations - THOUGHT THIS STEP SHOULD BE REDUNDANT
#  mutate(N = nvl(N,0),
#         B = nvl(B,0)) 

# ------------------------------------------------------------------------------
# C. Calculate mean and standard deviation of abundance and biomass of stations
#    within each strata and raise estimates by the area of the strata

by.strata <-
  
  tbl(mar, paste0('raw_index_calc_',Species)) %>% 
  
  # 9. filter stations ---------------------------------------------------------  
  filter((synaflokkur == Synaflokkur[1] & tognumer %in% Tognumer[[1]]) | 
           (synaflokkur == Synaflokkur[2] & tognumer %in% Tognumer[[2]])) %>%  #IS THIS NECESSARY? 
  
  # 10. summarise by strata ----------------------------------------------------
    # 10.a  Get the strata for each station - already done when calculating raw_index_calc table
    # 10.b group by year and strata and calculate number of stations, mean and sd
  group_by(tegund, ar, strata, synaflokkur, lengd, area) %>% 
  summarise(sN  = n(),   # number of stations within strata
            n_m  = sum(N, na.rm = TRUE),
            n_d  = ifelse(n() == 1, sum(N, na.rm = TRUE) * std.cv, sd(N)),
            b_m  = sum(B, na.rm = TRUE),
            b_d  = ifelse(n() == 1, sum(B, na.rm = TRUE) * std.cv, sd(B)))  

  
  
# ------------------------------------------------------------------------------
# D. Summarise data by year


by.year <- 
  names(ind) %>% 
  as.list() %>% 
  set_names(.,.) %>% 
  purrr::map(function(x){
    by.strata %>% 
  # ----------------------------------------------------------------------------
# Up to now we only have been operating within Oracle. I.e. sql-scripts via R.
# Have to collect here because of calc_cv function in the year aggregate step
# TODO: Fix that, do internally in Oracle
    collect(n = Inf) %>% 
    # ----------------------------------------------------------------------------
    # some data fall outside strata, drop them - needs some double checking of code
    drop_na() %>% 
    # subset by index groups------------------------------------------------
    left_join(ind[[x]]) %>% 
    filter(ind==1) %>% 
      # 11. summarise by year ------------------------------------------------------
    group_by(tegund, synaflokkur, ar) %>%
      summarise(n = sum(n_m, na.rm = TRUE),
            # A la Höski
            n.cv = calc_cv(n_m, n_d, area, sN),
            b = sum(b_m, na.rm = TRUE),
            # A la Höski
            b.cv = calc_cv(b_m, b_d, area, sN)) %>%
      mutate(index = x) %>% 
      ungroup()
    }) %>% 
  bind_rows(.)

if(type=='by.year'){return(by.year)} else {return(by.strata)} 
}


smb.index <- 
  calc_index(mar, length_ranges = cutoffs) %>% 
  filter(synaflokkur == 30) %>% 
  mutate(fj.minni = n,
         fj.minni.u = n*(1+n.cv),
         fj.minni.l = n*(1-n.cv),
         bio.staerri = b,
         bio.u = b*(1+b.cv),
         bio.l = b*(1-b.cv)) 

smh.index <- 
  calc_index(mar) %>% 
  filter(synaflokkur == 35) %>% 
  mutate(fj.minni = n,
         fj.minni.u = n*(1+n.cv),
         fj.minni.l = n*(1-n.cv),
         bio.staerri =b,
         bio.u = b*(1+b.cv),
         bio.l = b*(1-b.cv)) 


## total biomass plot

total_bio_plot <- 
  smb.index %>% 
  filter(index == 'total') %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y='Total biomass') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == cutoffs[1]),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index %>% 
                    filter(index == 'total'),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l))
## biomass > 40 cm
b40_plot <- 
  smb.index %>% 
  filter(index == 'B40') %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y=paste0('Biomass > ',cutoffs$B40[1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(index == 'B40'),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## biomass > 60

b60_plot <- 
  smb.index %>% 
  filter(index == 'B60') %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Biomass > ',cutoffs$B60[1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(index == 'B60'),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## abundance < 30

a30_plot <- 
  smb.index %>% 
  filter(index == 'juv') %>% 
  ggplot(aes(ar,fj.minni)) + 
  geom_ribbon(aes(ymin=fj.minni.l,ymax=fj.minni.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Abundance < ',cutoffs$juv[[2]])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(index == 'juv'), #was 40
                  aes(ar,fj.minni,ymax=fj.minni.u,ymin=fj.minni.l)) 

## 4-plot

four_plot <- 
  total_bio_plot + 
  b40_plot + 
  b60_plot + 
  a30_plot + 
  plot_layout(ncol=2)