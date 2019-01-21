library(patchwork)
library(knitr)
library(kableExtra)
library(gridExtra)
#devtools::install_github('thomasp85/patchwork')
library(patchwork)
library(forcats)
library(viridis)
#devtools::install_github('einarhjorleifsson/gisland')
library(gisland)

####-------Comparison of indices in 4plot-------#####

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



smb.index.tr <- 
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

smh.index.tr <- 
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

total_bio_plot.tr <- 
  smb.index.tr %>% 
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
b40_plot.tr <- 
  smb.index.tr %>% 
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

b60_plot.tr <- 
  smb.index.tr %>% 
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

a30_plot.tr <- 
  smb.index.tr %>% 
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

four_plot.tr <- 
  total_bio_plot.tr + 
  b40_plot.tr + 
  b60_plot.tr + 
  a30_plot.tr + 
  plot_layout(ncol=2)

four_plot <- 
  total_bio_plot + 
  b40_plot + 
  b60_plot + 
  a30_plot + 
  plot_layout(ncol=2)

######------Need to add plots here that demonstrate data availability and spatial coverage
#to aid in identifying groupings by strata, area, gridcell, gear, and synaflokkur in 03-catch-at-age

## landings data by country  
landings <- 
  tbl(mar,'lices') %>% 
  filter(#descr %in%  c('Va','5_A'),
    trim(sare) == '5',
    div == 'a',
    species == sp_name) %>%  #this may need to be modified depending on what name is in lices
  rename(country = country2) %>% 
  filter(!(country == 'Iceland' & year>1981)) %>% 
  group_by(year,country) %>% 
  summarise(c = sum(landings,na.rm = TRUE)) %>% 
  collect(n=Inf) %>% 
  bind_rows(tbl(mar,'landed_catch_pre94') %>% 
              filter(fteg == Species) %>% 
              group_by(ar) %>% 
              summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3) %>% 
              mutate(country='Iceland') %>% 
              collect(n=Inf) %>% 
              rename(year=ar)) %>%  
  bind_rows(lods_oslaegt(mar) %>% 
              filter(fteg == Species,veidisvaedi == 'I',ar>1993) %>% 
              left_join(lesa_skipaskra(mar)) %>% 
              mutate(country = ifelse(nvl(flokkur,0) != -4,'Iceland',einkst)) %>% 
              group_by(ar,country) %>% 
              summarise(c=sum(magn_oslaegt,na.rm = TRUE)/1e3)%>% 
              collect(n=Inf) %>% 
              rename(year=ar)) %>% 
  arrange(desc(country)) %>%
  filter(year < tyr,c>0) %>% 
  mutate(country = forcats::fct_recode(country,Norway = 'NO',
                                       `Faroe Islands` = 'FO',
                                       Belgium = 'BE', 
                                       Greenland = "GR",
                                       Greenland = 'GL',
                                       Germany = 'DE',
                                       Russia = 'RU'),
         country = ifelse(grepl('UK',country)|country=='GB','UK',country)) %>% 
  group_by(year,country) %>% 
  summarise(c=sum(c))

## catch history plot
landings.plot <- 
  landings %>% 
  mutate(country = ifelse(ifelse(is.na(country),' ',country)=='Iceland','Iceland','Other nations')) %>%
  group_by(year,country) %>% 
  summarise(c=sum(c)) %>% 
  arrange(desc(country)) %>%
  ggplot(aes(year,c/1e3,fill=country)) + 
  geom_bar(stat='identity') + 
  theme_bw() + 
  labs(y = 'Landings (in kt)', x = 'Year', fill = '') + 
  theme(legend.background = element_blank(),
        legend.position = c(0.15,0.75)) + 
  scale_fill_manual(values=c('lightblue','darkblue'))

## landings by nation
landings_by_country <- 
  landings %>% 
  filter(year>1978) %>% 
  mutate(c = round(c)) %>%
  spread(country,c,fill = '')

## number of boats and landings by gear 
nb_lnd_by_yr <- 
  lods_oslaegt(mar) %>% 
  filter(fteg == Species,veidisvaedi == 'I',ar>1992,ar<tyr,magn_oslaegt>100) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% c('LLN','BMT','DSE'),gear, 'Other')) %>% 
  group_by(ar,gear) %>% 
  summarise(c = round(sum(magn_oslaegt)/1e3),
            nb = n_distinct(skip_nr)) %>% 
  collect(n=Inf) %>% 
  mutate(gear = forcats::fct_recode(gear,
                                    Longlines='LLN',
                                    `Bottom trawl`='BMT',
                                    `Danish seine`='DSE')) %>% 
  gather(col,val,-c(ar,gear)) %>% 
  unite(col,c(col,gear),sep = '_') %>%
  spread(col,val) %>% 
  ungroup() %>% 
  mutate(`Total catch` = rowSums(.[grep('c_',names(.))],na.rm = TRUE)) %>% 
  select(Year=ar,starts_with('nb_'),starts_with('c_'),`Total catch`) %>% 
  select(-nb_Other) %>% 
  set_names(.,gsub('c_|nb_','',names(.))) 

## plot landings in tons by gear and year
lnd_by_gear <- 
  lods_oslaegt(mar) %>% 
  filter(fteg == Species,veidisvaedi == 'I',ar>1992,ar < tyr) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% c('LLN','BMT','DSE'),gear, 'Other')) %>% 
  group_by(ar,gear) %>% 
  summarise(c = sum(magn_oslaegt,na.rm=TRUE)/1e3) %>% 
  collect(n=Inf) %>% 
  mutate(gear = forcats::fct_recode(gear,
                                    Longlines='LLN',
                                    `Bottom trawl`='BMT',
                                    `Danish seine`='DSE')) %>% 
  arrange(gear) 


lnd_by_gear_top <- 
  lnd_by_gear %>% 
  ggplot(aes(ar,c,fill=gear)) + 
  geom_bar(stat = 'identity') + 
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(angle=90),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="top") +
  labs(y="Catches (kt)",fill='') +
  scale_fill_brewer(palette='YlOrRd',direction = -1)

lnd_by_gear_bottom <- 
  lnd_by_gear %>% 
  ggplot(aes(ar,c,fill=gear)) + 
  geom_bar(stat = 'identity',position = "fill") + 
  theme_light()+
  theme(axis.text.y=element_text(angle=90),
        legend.position=" ")+
  labs(y="Proportion (of catches)",x="Year") +
  scale_fill_brewer(palette='YlOrRd',direction = -1)

## depth by gear and year
depth_dat <- 
  tbl(mar,paste0(sp_name,'_catch')) %>% #DOH! Forgot where this is created
  filter(year > 2000, year < tyr) %>% 
  mutate(depth_class = ifelse(depth < 101,"4",
                              ifelse(depth < 201,"3",
                                     ifelse(depth < 301, "2","1")))) %>% 
  mutate(depth_class = nvl(depth_class,"1")) %>% 
  filter(gear %in% c('LLN','DSE','BMT')) %>% 
  group_by(year,depth_class) %>% 
  summarise(c=sum(catch, na.rm = TRUE)/1e6) %>% 
  collect(n=Inf) %>% 
  arrange(depth_class)

depth_plot <- 
  depth_dat %>%  
  ggplot(aes(year,c,fill=depth_class)) + geom_bar(stat='identity') +
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(angle=90),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="top") +
  labs(y="Catches (kt)") + 
  scale_fill_manual(values = c("#225EA8","#41B6C4","#7FCDBB","#C7E9B4"),
                    name="Total catch \n by depth (m)",
                    breaks=c("4", "3", "2", "1"),
                    labels=c("1-100",  "101-200", "201-300", ">300"))

depth_fill_plot <- 
  depth_dat %>% 
  ggplot(aes(year,c,fill=depth_class)) + 
  geom_bar(stat='identity', position = 'fill') +
  theme_light()+
  theme(axis.text.y=element_text(angle=90),
        legend.position=" ")+
  labs(y="Proportion (of catches)",x="Year") +
  scale_fill_manual(values = c("#225EA8","#41B6C4","#7FCDBB","#C7E9B4"),
                    name="Total catch \n by depth (m)",
                    breaks=c("4", "3", "2", "1"),
                    labels=c("1-100",  "101-200", "201-300", ">300"))


#THIS SHOWS AREA GROUPINGS USED IN 03-catch_at_age
## Catches by gadget division
region.map <- function(d){
  d %>% 
    mutate(DIVISION = nvl(DIVISION,0)) %>% 
    mutate(region = ifelse(DIVISION %in% c(101),'W',
                           ifelse(DIVISION %in% c(102),'NW',
                                  ifelse(DIVISION %in% c(103,104,105),'NE',
                                         ifelse(DIVISION %in% c(107,106),'SE',
                                                ifelse(DIVISION %in% c(108),'SW','Other'))))))
}

region_labels <- 
  list(data_frame(x=-25,y=64,label='W'),
       data_frame(x=-25,y=66,label='NW'),
       data_frame(x=-14,y=66,label='NE'),
       data_frame(x=-22.5,y=63.5,label='SW'),
       data_frame(x=-15,y=63.7,label='SE')) %>% 
  bind_rows()

region.plot <- 
  tbl(mar,'reitmapping') %>% 
  region.map() %>% 
  collect(n=Inf) %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  #mutate(region = as.factor(region)) %>% 
  filter(region!='Other') %>% 
  ggplot() + 
  geom_raster(aes(lon,lat,fill=region)) + 
  coord_quickmap() + 
  geom_polygon(data=gisland::biceland,aes(long,lat,group=group),col='black',fill='khaki') + 
  theme_light() +
  theme(legend.position = 'none', 
        axis.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'),
        panel.spacing = unit(0,'lines')) + 
  geom_label(data=region_labels,aes(x,y,label=label),col='black',size=3) +
  labs(y='',x='') +
  scale_fill_brewer(palette='YlOrRd') 

catch_by_area <- 
  tbl(mar,paste0(sp_name,'_catch')) %>%
  filter(year>1992,year<tyr) %>% 
  left_join(tbl(mar,'reitmapping') %>% 
              rename(gridcell = GRIDCELL) %>% 
              select(-c(id,lat,lon,size))) %>%
  region.map() %>% 
  group_by(year,region) %>% 
  summarise(c=sum(catch)/1e6) %>% 
  collect(n=Inf) %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  arrange(region) %>% 
  ggplot(aes(year,c,fill=region)) + 
  geom_bar(stat='identity') + 
  theme_light()+
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(angle=90),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'top') +
  labs(y="Catches (kt)",fill='') +
  scale_fill_brewer(palette='YlOrRd') +
  
  tbl(mar,paste0(sp_name,'_catch')) %>%
  filter(year>1992,year<tyr) %>% 
  left_join(tbl(mar,'reitmapping') %>% 
              rename(gridcell = GRIDCELL) %>% 
              select(-c(id,lat,lon,size))) %>%
  region.map() %>% 
  group_by(year,region) %>% 
  summarise(c=sum(catch)) %>% 
  collect(n=Inf) %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  arrange(region) %>% 
  ggplot(aes(year,c,fill=region)) + 
  geom_bar(stat='identity',position = 'fill') + 
  theme_light() +
  theme(axis.text.y=element_text(angle=90),
        legend.position=" ")+
  labs(y="Proportion (of catches)",x="Year") +
  scale_fill_brewer(palette='YlOrRd',drop=FALSE) +
  plot_layout(ncol=1)


## spatial distribution of catches
catch_dist_plot <- 
  tbl(mar,paste0(sp_name,'_catch')) %>% 
  filter(year>1989, year < tyr) %>% 
  #filter(year %in% c(1990,1995,2000,2005,2010,2016)) %>% 
  mar:::encode_zchords(dx=0.125,dy=0.0625) %>% 
  #  select(-sq) %>% 
  #  unite(sq,x,y,sep=':') %>% 
  group_by(year,sq) %>% 
  dplyr::summarise(catch=sum(catch)/(1e3*45.25277*1.852^2),
                   effort=sum(towtime)) %>% 
  collect(n=Inf) %>% 
  separate(sq,c('lon','lat'),sep=':') %>% 
  mutate(lon = gsub(',','.',lon) %>% as.numeric(),
         lat = gsub(',','.',lat)%>% as.numeric()) %>% 
  filter(!is.na(catch)) %>% 
  #dplyr::mutate(catch = cut(catch/1e3, 
  #                          breaks = c(-1,0.25,1,15,40,60,10000000),
  #                          labels = c('< 0.25 kt','< 1 kt',
  #                                     '< 15 kt','< 40 kt','< 60 kt','< 1000 kt'))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_raster(aes(lon, lat, fill = catch),interpolate = TRUE) + 
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(greenland),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(faroes),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  geom_label(x=-18,y=65,aes(label=year),data=data_frame(year=1990:(tyr-1))) +
  
  #  geom_polygon(data=geo::eyjar, aes(lon, lat),col = 'black', fill = 'gray70',size = 0.3) +
  #  geom_polygon(data=geo::faeroes, aes(lon, lat),col = 'black', fill = 'gray70')+
  
  geom_path(aes(lon, lat),data=geo::gbdypi.100,col='grey',size = 0.3) +
  #  geom_path(data=gbdypi.800,lty=2,size = 0.3) +
  #  geom_polygon(alpha=0.5,data=gbdypi.100,fill='gray90') +
  #  geom_path(data=gbdypi.200,lty=2,size = 0.3) + #alpha=0.5,fill='gray90',
  geom_path(aes(lon, lat),data=geo::gbdypi.500,col='grey',size = 0.3) +
  geom_path(aes(lon, lat),data=geo::gbdypi.1000,col='grey',size = 0.3) +
  
  #  coord_quickmap( xlim=c(-75,-0),ylim=c(60,85)) +
  #  scale_x_continuous(name = NULL, breaks = NULL) +
  #  scale_y_continuous(name = NULL, breaks = NULL) +
  labs(fill = "Catch (t)/nm2") +
  scale_fill_viridis_c(option = 'viridis',
                       #direction = -1,
                       breaks = c(0.0001,0.01,1),
                       trans='log') +
  #  scale_fill_gradient2(low = "yellow", mid = "red",
  #                       high = "black", midpoint = )+
  theme_bw()+
  coord_quickmap(xlim = c(-30, -10),ylim = c(63, 68))+
  
  #geom_path(data=fortify(ego),aes(long,lat,group=group))+
  facet_wrap(~year,ncol=4)+ 
  theme(#axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.background = element_rect(fill = "white"),
    #legend.position = 'none',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size= unit(0.5, 'cm'),
    #legend.key = element_rect(colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.margin = unit(0,'cm'),
    plot.margin = unit(c(0,0,0,0),'cm'),
    strip.background = element_blank(),
    strip.text.x = element_blank())

## position of samples from the catches
sampling_pos <- 
  lesa_stodvar(mar) %>% 
  filter(ar == (tyr -1),synaflokkur %in% c(1,2,8)) %>% 
  inner_join(lesa_kvarnir(mar) %>% filter(tegund == 2)) %>%
  left_join(tbl(mar,'gear_mapping')) %>% 
  filter(gear %in% c('BMT','LLN','DSE')) %>% 
  select(lat=kastad_n_breidd,lon=kastad_v_lengd,year = ar,gear) %>% 
  distinct() %>% 
  collect(n=Inf)


sampling_pos_plot <- 
  tbl(mar,paste0(sp_name,'_catch')) %>% 
  filter(year == (tyr -1), gear %in% c('BMT','LLN','DSE')) %>% 
  #filter(year %in% c(1990,1995,2000,2005,2010,2016)) %>% 
  mar:::encode_zchords(dx=0.125/2,dy=0.0625/2) %>% 
  #  select(-sq) %>% 
  #  unite(sq,x,y,sep=':') %>% 
  group_by(gear,sq) %>% 
  dplyr::summarise(catch=sum(catch)/(1e3*45.25277*1.852^2),
                   effort=sum(towtime)) %>% 
  collect(n=Inf) %>% 
  separate(sq,c('lon','lat'),sep=':') %>% 
  mutate(lon = gsub(',','.',lon) %>% as.numeric(),
         lat = gsub(',','.',lat)%>% as.numeric()) %>% 
  filter(!is.na(catch)) %>% 
  #dplyr::mutate(catch = cut(catch/1e3, 
  #                          breaks = c(-1,15,40,60,10000000),
  #                          labels = c('< 0.25 kt','< 1 kt',
  #                                     '< 15 kt','< 40 kt','< 60 kt','< 1000 kt'))) %>% 
  ungroup() %>% 
  ggplot() +
  geom_raster(aes(lon, lat, fill = catch),interpolate = TRUE) + 
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(greenland),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  #  geom_polygon(data= fortify(faroes),  aes(long, lat,group=group), fill = 'gray70',col='black') +
  geom_label(x=-18,y=65,aes(label=gear),data=sampling_pos %>% select(gear) %>% distinct()) +
  
  #  geom_polygon(data=geo::eyjar, aes(lon, lat),col = 'black', fill = 'gray70',size = 0.3) +
  #  geom_polygon(data=geo::faeroes, aes(lon, lat),col = 'black', fill = 'gray70')+
  
  geom_path(aes(lon, lat),data=geo::gbdypi.100,col='grey',size = 0.3) +
  #  geom_path(data=gbdypi.800,lty=2,size = 0.3) +
  #  geom_polygon(alpha=0.5,data=gbdypi.100,fill='gray90') +
  #  geom_path(data=gbdypi.200,lty=2,size = 0.3) + #alpha=0.5,fill='gray90',
  geom_path(aes(lon, lat),data=geo::gbdypi.500,col='grey',size = 0.3) +
  geom_path(aes(lon, lat),data=geo::gbdypi.1000,col='grey',size = 0.3) +
  
  geom_point(aes(lon,lat), data= sampling_pos, pch = 8) +
  
  #  coord_quickmap( xlim=c(-75,-0),ylim=c(60,85)) +
  #  scale_x_continuous(name = NULL, breaks = NULL) +
  #  scale_y_continuous(name = NULL, breaks = NULL) +
  labs(fill = "Catch (t) / nm2") +
  scale_fill_viridis_c(option = 'viridis',
                       #direction =,
                       trans='log',
                       breaks = c(0.0001, 0.01,1)) +
  #  scale_fill_gradient2(low = "yellow", mid = "red",
  #                       high = "black", midpoint = )+
  theme_bw()+
  coord_quickmap(xlim = c(-30, -10),ylim = c(63, 68))+
  
  #geom_path(data=fortify(ego),aes(long,lat,group=group))+
  facet_wrap(~gear,ncol=2)+ 
  theme(#axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.background = element_rect(fill = "white"),
    legend.position = c(0.75,0.2),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size= unit(0.5, 'cm'),
    #legend.key = element_rect(colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.margin = unit(0,'cm'),
    plot.margin = unit(c(0,0,0,0),'cm'),
    strip.background = element_blank(),
    strip.text.x = element_blank())

