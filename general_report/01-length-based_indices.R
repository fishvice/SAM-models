

# ------------------------------------------------------------------------------
###--------LENGTH_BASED INDICES-----###


# A. Data gathering ------------------------------------------------------------
dbRemoveTable(mar,paste0('raw_index_calc_',Species))

by.length <-
  # 1. get survey stations -----------------------------------------------------
lesa_stodvar(mar) %>%
  filter(synaflokkur %in% Synaflokkur) %>%
  mutate(index = reitur * 100 + tognumer) %>%
  select(synis_id, ar, index, reitur, smareitur, tognumer, veidarfaeri, toglengd,synaflokkur) %>%
  mutate(gridcell = reitur*10 + smareitur) %>% 
  # 2. get length data ---------------------------------------------------------
left_join(lesa_lengdir(mar) %>%
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
