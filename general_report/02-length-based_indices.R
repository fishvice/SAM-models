

# ------------------------------------------------------------------------------
###--------LENGTH_BASED INDICES-----###


# A. Data gathering ------------------------------------------------------------
dbRemoveTable(mar,paste0('raw_index_calc_',Species))

by.length <-
  # 1. get survey stations -----------------------------------------------------
lesa_stodvar(mar) %>%
  filter(synaflokkur %in% Index_Synaflokkur[[Species]]) %>%
  mutate(index = reitur * 100 + tognumer) %>%
  select(synis_id, ar, index, reitur, smareitur, tognumer, veidarfaeri, toglengd,synaflokkur) %>%
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
  
  # 5. trim towlength ----------------------------------------------------------
mar:::skala_med_toglengd(., min_towlength = min.towlength, max_towlength = max.towlength, std_towlength = std.towlength) %>%
  
  # 6.a standardize by towlength ------------------------------------------------
#mutate(N = fjoldi / toglengd * std.towlength) %>%      # standardize to per 4 miles

# 6.b standardize to area swept
#     this does not make much sense here because we already have this above
#     need to pass this function further down in the code path
mutate(N = fjoldi / if_else(veidarfaeri == 78, 1.25 * std.width, std.width)) %>%
  
  # 7. calculate_biomass from numbers, length and a and b ----------------------
# 7.a get the length weight coefficients
left_join(tbl_mar(mar, "ops$einarhj.lwcoeff"),
          by = "tegund") %>%
  
  # 7.b use Newton's law if lwcoefficient for species not specified
  mutate(a = ifelse(is.na(a), 0.01, a),
         b = ifelse(is.na(b), 3.00, b),
         B  = ifelse(is.na(N), 0, N) * a * lengd^b / 1e3) %>%
  select(-a, -b) %>% 
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
         b = B) %>% 
  #raise by strata
  mutate(N     = N  * area / std.towlength,
         B     = B  * area / std.towlength) %>% 
  #divide by number of tows (?)
  group_by(tegund,ar,strata) %>% 
  mutate(N = N/n_distinct(synis_id),
         B = B/n_distinct(synis_id)) %>% 
  
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


tbl(mar, paste0('raw_index_calc_',Species))


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
#max.towlength = max.towlength, Index_Synaflokkur = Index_Synaflokkur, Index_Tognumer = Index_Tognumer

#lower bound inclusive
calc_index <- function(mar, 
                       length_ranges = list(total = c(5,500), juv = c(5,30)), 
                       type = 'by.year'){
  
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
  filter((synaflokkur == Index_Synaflokkur[[Species]][1] & tognumer %in% Index_Tognumer[[Species]][[1]]) | 
           (synaflokkur == Index_Synaflokkur[[Species]][2] & tognumer %in% Index_Tognumer[[Species]][[2]])) %>%  #IS THIS NECESSARY? 
    
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
    names(length_ranges) %>% 
    as.list() %>% 
    set_names(.,.) %>% 
    purrr::map(function(x){
      by.strata %>% 
        #subset by length range
        # left_join(tbl(mar, paste0('raw_index_calc_',Species)) %>%
        #             select(lengd) %>%
        #             distinct %>% 
        #             mutate(ind.tmp = ifelse(lengd >= length_ranges[[x]][1] & lengd < length_ranges[[x]][2], 1, 0))) %>%
        filter(lengd >= length_ranges[[x]][1] & lengd < length_ranges[[x]][2]) %>%
        # ----------------------------------------------------------------------------
      # Up to now we only have been operating within Oracle. I.e. sql-scripts via R.
      # Have to collect here because of calc_cv function in the year aggregate step
      # TODO: Fix that, do internally in Oracle
      collect(n = Inf) %>% 
        # ----------------------------------------------------------------------------
      # some data fall outside strata, drop them - needs some double checking of code
      drop_na() %>% 
        # subset by index groups------------------------------------------------
      #left_join(ind.tmp[[x]]) %>% 
      #  filter(ind.tmp==1) %>% 
        # 11. summarise by year ------------------------------------------------------
      group_by(tegund, synaflokkur, ar) %>%
      summarise(n = sum(n_m, na.rm = TRUE),
                  # A la Höski
                  n.cv = calc_cv(n_m, n_d, area, sN),
                  b = sum(b_m, na.rm = TRUE),
                  # A la Höski
                  b.cv = calc_cv(b_m, b_d, area, sN),
                  ml = mean(lengd, na.rm = TRUE)) %>%
      mutate(index = x) %>% 
      ungroup()
    }) %>% 
    bind_rows(.)
  
  if(type=='by.year'){return(by.year)} else {return(by.strata)} 
}

#calc_index(mar)
