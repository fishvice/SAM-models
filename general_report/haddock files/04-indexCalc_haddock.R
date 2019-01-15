library(tidyverse)
library(mar)
con <- connect_mar()

#' Calculate overall cv from stratfied summary statistics
#' 
#' @param m Mean value within strata
#' @param s Standard deviation within strata
#' @param area The area (e.g. survey strata area)
#' @param n number of samples within strata
#'
#' @export
#'
calc_cv <- function(m, s, area, n) {
  
  Mean = sum(m * area) / sum(area)
  Sum = sum(m * area)
  tmpsum = sum(m[!is.na(s)] * area[!is.na(s)])
  Calc.sdev = sqrt(sum(s[!is.na(s)]^2 * area[!is.na(s)]^2/  n[!is.na(s)])   / sum(area[!is.na(s)])^2)
  Sdev = Calc.sdev * Sum/tmpsum
  cv = Sdev/Mean
  
  return(cv)
}


Species <- 2                   # Select a species
Length.min <- 5                   # Minimum length for indices calculation
Length.max <- 500                 # Maximum length for indices calculation

std.cv        <- 1             # The cv if only one station in a strata
std.towlength <- 4             # Standard tow length for SMB is 4 nautical miles
std.width     <- 17 / 1852     # Standard sweep width in nautical miles

min.towlength <- 2             # Minimum "acceptable" towlength
max.towlength <- 8             # Maximum "acceptable" towlength

Synaflokkur <- 30
Tognumer <- c(1:39, NA)


calc_index <- function(con, Species = 1, Length.min = 5, Length.max = 500,
                       std.cv = 1, std.towlength = 4, min.towlength, max.towlength,Synaflokkur = 30,
                       Tognumer = c(1:30,NA)){
  
  # ------------------------------------------------------------------------------
  # A. Data gathering
  
  by.length <-  
    # 1. get survey stations -----------------------------------------------------
  lesa_stodvar(con) %>% 
    filter(synaflokkur %in% Synaflokkur) %>% 
    #mutate(index = reitur * 100 + tognumer) %>% 
    select(synis_id, synaflokkur, ar,  reitur, smareitur, tognumer, veidarfaeri, toglengd) %>% 
    
    # 2. get length data ---------------------------------------------------------
  left_join(lesa_lengdir(con) %>% 
              filter(tegund %in% Species,
                     lengd >= Length.min,
                     lengd < Length.max) %>% 
              group_by(synis_id, tegund, lengd) %>% 
              summarise(fjoldi = sum(fjoldi, na.rm = TRUE))) %>% 
    ungroup()  %>% 
    
    # 0. A temporary fix, for zero stations --------------------------------------
  #     TODO: Find a more permanent solution so scripts works for more than
  #           one species (via group_by( ..., tegund))
  mutate(tegund = nvl(tegund, Species),
         lengd  = nvl(lengd, Length.min),
         fjoldi = nvl(fjoldi, 0)) %>% 
    # 3. get count data ----------------------------------------------------------
  skala_med_toldum() %>% 
    
    mutate(fjoldi = fjoldi_alls/1e3) %>%   
    # 4. scale by counted --------------------------------------------------------
  #mutate(N = fjoldi_alls / 1e3) %>%   # units of thousand
  
  # 5. trim towlength ----------------------------------------------------------
  mar:::skala_med_toglengd() %>% #mutate(toglengd = if_else(toglengd > max.towlength, max.towlength, toglengd),
    #toglengd = if_else(toglengd < min.towlength, min.towlength, toglengd)) %>% 
    rename(N = fjoldi) %>% 

  mutate(N = N / if_else(veidarfaeri == 78, 1.25 * std.width, std.width)) %>% 
    
    # 7. calculate_biomass from numbers, length and a and b ----------------------
  # 7.a get the length weight coefficients
  left_join(tbl_mar(con, "ops$einarhj.lwcoeff")) %>% 
    # 7.b use Newton's law if lwcoefficient for species not specified
    mutate(a = nvl(a,0.01),
           b = nvl(b,3),
           B  =nvl(N,0) * a * lengd^b / 1e3) %>% 
    
    ## this part is a bit messy
    left_join(tbl(con, "SMBSTATIONSSTRATA") %>%
                mutate(synaflokkur = 30) %>% 
                select(synis_id, synaflokkur,strata = newstrata)) %>% 
    left_join(tbl(con,'SMHSTATIONSSTRATA') %>% 
                mutate(synaflokkur = 35) %>% 
                select(synis_id,synaflokkur, strata2 = newstrata),
              by = c("synis_id", "synaflokkur")) %>%
    left_join(tbl(con, "NEWSTRATAAREA") %>% 
                select(strata = newstrata, area2 = rall.area,name2=name) %>% 
                #  area is above is in km2, here convert nm2
                mutate(area = area2 / 1.852^2),
              by = 'strata') %>%
#    left_join(tbl(con, "OLDSTRATAAREA") %>% 
#                select(strata = oldstrata, area = rall.area,name) %>% 
#                #  area is above is in km2, here convert nm2
#                mutate(area = area / 1.852^2),
#              by = 'strata') %>%
#    mutate(strata = nvl(strata,strata2),
#           area = nvl(area,area2),
#           name = nvl(name,name2)) %>%
#    select(-c(strata2,area2,name2)) %>% 
    # 11.b do the strata raising
    mutate(N     = N  * nvl(area,0) / std.towlength,
           B     = B  * nvl(area,0) / std.towlength) %>% 
  group_by(ar,strata) %>% 
    mutate(N = N/n(),
           B = B/n()) %>% 
    collect(n=Inf)
  
  # ------------------------------------------------------------------------------
  # B. Summarise abundance and biomass by station
  
  
  by.station <- 
    
    by.length %>% 
    
    # 8. summarise by station ----------------------------------------------------
  # NOTE: here is the first step where statistics by length is dropped
  #       some (minor) recoding above would be needed if one were to take things
  #       forward by each length class
  group_by(synis_id, synaflokkur, reitur, smareitur, tognumer, veidarfaeri, tegund, ar,lengd) %>% 
    summarise(N = sum(N, na.rm = TRUE),
              B = sum(B, na.rm = TRUE)) %>% 
    # Zero stations - THOUGHT THIS STEP SHOULD BE REDUNDANT
    mutate(N = nvl(N,0),
           B = nvl(B,0)) 
  
  # ------------------------------------------------------------------------------
  # C. Calculate mean and standard deviation of abundance and biomass of stations
  #    within each strata and raise estimates by the area of the strata
  
  by.strata <-
    
    by.station %>% 
    
    # 9. filter stations ---------------------------------------------------------  
  filter(tognumer %in% Tognumer) %>% 
    
    
    # 10. summarise by strata ----------------------------------------------------
  # 10.a  Get the strata for each station
  left_join(tbl(con, "SMBSTATIONSSTRATA") %>%
              mutate(synaflokkur = 30) %>% 
              select(synis_id, synaflokkur,strata = oldstrata)) %>% 
    left_join(tbl(con,'SMHSTATIONSSTRATA') %>% 
                mutate(synaflokkur = 35) %>% 
                select(synis_id,synaflokkur, strata2 = newstrata),
              by = c("synis_id", "synaflokkur")) %>%
    mutate(strata = nvl(strata,strata2)) %>% 
    # 10.b group by year and strata and calculate number of stations, mean and sd
    group_by(tegund, ar, strata,synaflokkur,lengd) %>% 
    summarise(sN  = n(),   # number of stations within strata
              n_m  = mean(N, na.rm = TRUE),
              n_d  = ifelse(n() == 1, mean(N, na.rm = TRUE) * std.cv, sd(N)),
              b_m  = mean(B, na.rm = TRUE),
              b_d  = ifelse(n() == 1, mean(B, na.rm = TRUE) * std.cv, sd(B))) %>% 
    
    # 11. raise to strata area ---------------------------------------------------
  # 11.a get area of the strata
  left_join(tbl(con, "NEWSTRATAAREA") %>% 
              select(strata = newstrata, area = rall.area,name) %>% 
              #  area is above is in km2, here convert nm2
              mutate(area = area / 1.852^2)) %>%
    
    # 11.b do the strata raising
    mutate(n     = n_m  * nvl(area,0) / std.towlength,
           b     = b_m  * nvl(area,0) / std.towlength) 
  
  # ------------------------------------------------------------------------------
  # D. Summarise data by year
  
  by.year <- 
    
    by.strata %>% 
    
    
    # ----------------------------------------------------------------------------
  # Up to now we only have been operating within Oracle. I.e. sql-scripts via R.
  # Have to collect here because of calc_cv function in the year aggregate step
  # TODO: Fix that, do internally in Oracle
  collect(n = Inf) %>% 
    
    
    # ----------------------------------------------------------------------------
  # some data fall outside strata, drop them - needs some double checking of code
  drop_na() %>% 
    
    # 11. summarise by year ------------------------------------------------------
  group_by(tegund, synaflokkur, ar,lengd) %>%
    summarise(n = sum(n, na.rm = TRUE),
              # A la Höski
              n.cv = calc_cv(n_m, n_d, area, sN),
              b = sum(b, na.rm = TRUE),
              # A la Höski
              b.cv = calc_cv(b_m, b_d, area, sN)) %>% 
    ungroup()
  
  return(by.year)
}

calc_index(mar,Species = 2,Synaflokkur = 30) -> tmp
calc_index(mar,Species = 2,Synaflokkur = 35,Tognumer = 1:75)->tmp2


age_length_key <- 
  lesa_stodvar(mar) %>% 
  filter(synaflokkur==30) %>% 
  select(synis_id,ar) %>% 
  inner_join(lesa_kvarnir(mar) %>% 
               filter(tegund==1,!is.na(aldur))) %>% 
  mutate(lg = floor((lengd - 2.5)/5)*5 + 2.5) %>% 
  select(synis_id,ar,lengd,aldur,lg) %>% 
  group_by(ar,aldur,lg) %>% 
  summarise(n=n()) %>% 
  group_by(ar,lg) %>% 
  mutate(p=n/sum(n)) %>% 
  collect(n=Inf)

by.year %>% 
  mutate(lg = floor((lengd - 2.5)/5)*5 + 2.5) %>% 
  left_join(age_length_key %>% select(-n)) %>% 
  group_by(ar,aldur) %>% 
  summarise(n.age = sum(n*p,na.rm=TRUE),
            b.age = sum(b*p,na.rm=TRUE)) %>% 
  filter(ar==2017)

