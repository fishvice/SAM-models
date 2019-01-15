

survey_location_plot <- 
  lesa_stodvar(mar) %>% 
  filter(synaflokkur==30,ar>1983) %>% 
  inner_join(lesa_lengdir(mar) %>% 
               filter(tegund==2) %>% 
               skala_med_toldum()) %>% 
  dplyr::rename(lat=kastad_n_breidd,lon=kastad_v_lengd) %>% 
  dplyr::group_by(synis_id,lat,lon,ar) %>% 
  dplyr::summarise(bio = sum(abs(fjoldi)*0.001*abs(lengd)^3/abs(nvl(tog_lengd,4)),na.rm = TRUE)/1e3) %>% 
  dplyr::ungroup() %>% 
  collect(n=Inf) %>% 
  #filter(ar==2018) %>% 
  ggplot() +
  geom_point(aes(lon, lat, size = bio),col='red',shape = 1) + 
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray70',col='black') +
  
  #geom_polygon(data = fortify(greenland),  aes(long, lat,group=group), fill = 'gray70') +
  #geom_polygon(data = fortify(faroes),  aes(long, lat,group=group), fill = 'gray70') +
  
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
  labs(size = "Ton/nm") +
  #     scale_fill_brewer(palette='YlOrRd') +
  #  scale_fill_gradient2(low = "yellow", mid = "red",
  #                       high = "black", midpoint = )+
  #geom_label(x=-2,y=69,aes(label=ar)) +
  #   geom_text(data=data.frame(ar=2011),label='No survey',
  #              col='red',x=-20,y=65) +
  #  geom_text(data=data.frame(year=1996:1997),label='No survey',
  #            col='red',x=-31,y=66.5,angle=45) +
  #  coord_quickmap(xlim = c(-40, 0),ylim = c(60, 70))+
  coord_quickmap( xlim=c(-30,-10),ylim=c(63,68)) +
  scale_size_area()+
  theme_light()+
  #geom_path(data=fortify(ego),aes(long,lat,group=group))+
  facet_wrap(~ar)+ 
  theme(#axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.background = element_rect(fill = "white"),
    legend.position = 'none',#c(0.8, 0.2),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size= unit(0.5, 'cm'),
    #legend.key = element_rect(colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(0,'cm'),
    plot.margin = unit(c(0,0,0,0),'cm'),
    strip.background = element_blank(),
    strip.text = element_blank()) + 
  geom_label(data=data_frame(ar=1985:tyr, label=ar),
             aes(label=label),x=-20,y=65,size=3)




agfs_location_plot <- 
  lesa_stodvar(mar) %>% 
  filter(synaflokkur==35,ar>1983) %>% 
  inner_join(lesa_lengdir(mar) %>% 
               filter(tegund==2) %>% 
               skala_med_toldum()) %>% 
  dplyr::rename(lat=kastad_n_breidd,lon=kastad_v_lengd) %>% 
  dplyr::group_by(synis_id,lat,lon,ar) %>% 
  dplyr::summarise(bio = sum(abs(fjoldi)*0.001*abs(lengd)^3/abs(nvl(tog_lengd,4)),na.rm = TRUE)/1e3) %>% 
  dplyr::ungroup() %>% 
  collect(n=Inf) %>% 
  #filter(ar==2018) %>% 
  ggplot() +
  geom_point(aes(lon, lat, size = bio),alpha=0.2,col='red') + 
  geom_polygon(data = geo::bisland, aes(lon, lat), fill = 'gray70',col='black') +
  
  #geom_polygon(data = fortify(greenland),  aes(long, lat,group=group), fill = 'gray70') +
  #geom_polygon(data = fortify(faroes),  aes(long, lat,group=group), fill = 'gray70') +
  
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
  labs(size = "Ton/nm") +
  #     scale_fill_brewer(palette='YlOrRd') +
  #  scale_fill_gradient2(low = "yellow", mid = "red",
  #                       high = "black", midpoint = )+
  #geom_label(x=-2,y=69,aes(label=ar)) +
  #   geom_text(data=data.frame(ar=2011),label='No survey',
  #              col='red',x=-20,y=65) +
  #  geom_text(data=data.frame(year=1996:1997),label='No survey',
  #            col='red',x=-31,y=66.5,angle=45) +
  #  coord_quickmap(xlim = c(-40, 0),ylim = c(60, 70))+
  coord_quickmap( xlim=c(-30,-10),ylim=c(63,68)) +
  scale_size_area(max_size = 10)+
  theme_light()+
  #geom_path(data=fortify(ego),aes(long,lat,group=group))+
  facet_wrap(~ar)+ 
  theme(#axis.text = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.background = element_rect(fill = "white"),
    legend.position = 'none',#c(0.8, 0.2),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.key.size= unit(0.5, 'cm'),
    #legend.key = element_rect(colour = NA),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.spacing = unit(0,'cm'),
    plot.margin = unit(c(0,0,0,0),'cm'),
    strip.background = element_blank(),
    strip.text = element_blank()) + 
  geom_label(data=data_frame(ar=1995:(tyr-1), label=ar),
             aes(label=label),x=-20,y=65,size=3)







survey_ldist_plot <- 
  tbl(mar,'raw_index_calc') %>% 
  filter((synaflokkur == 30 & tognumer %in% c(1:39, NA)) | (synaflokkur == 35 & tognumer < 75)) %>% 
  mutate(ar = ifelse(synaflokkur == 35,ar+1,ar)) %>% 
  group_by(ar,lengd,synaflokkur) %>% 
  summarise(N=sum(N,na.rm = TRUE)/1e3) %>% 
  collect(n=Inf) %>% 
  ungroup() %>% 
  ggplot(aes(lengd,N,lty=as.factor(synaflokkur))) + 
  geom_line()+ 
  facet_wrap(~ar,scale='free_y') +
  theme_light() + 
  theme(legend.position = 'none') +
  labs(y='Abundance',x='Length')

## survey index by area
survey_by_area <- 
  tbl(mar,'raw_index_calc') %>%
  rename(year=ar) %>%
  filter((synaflokkur == 30 & tognumer %in% c(1:39, NA))) %>% 
  mutate(synaflokkur = ifelse(synaflokkur == 30, ' Spring survey','Autumn survey')) %>% 
  #filter(year>1992,year<tyr) %>% 
  left_join(tbl(mar,'reitmapping') %>% 
              rename(gridcell = GRIDCELL) %>% 
              select(-c(id,lat,lon,size))) %>%
  region.map() %>% 
  group_by(year,region,synaflokkur) %>% 
  summarise(c=sum(B)/1e3) %>% 
  collect(n=Inf) %>% 
  ungroup() %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  arrange(region) %>% 
  ggplot(aes(year,c,fill=region)) + 
  geom_bar(stat='identity') + 
  theme_bw()+
  #facet_wrap(~synaflokkur) +
  theme(axis.title.x=element_blank(),
        axis.text.y=element_text(angle=90),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'top',
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0,vjust = 0),
        plot.margin = margin(0,0,0,0,'cm')) +
  labs(y="Survey index",fill='') +
  scale_fill_brewer(palette='YlOrRd',direction = -1) +
  
  tbl(mar,'raw_index_calc') %>%
  rename(year=ar) %>%
  filter((synaflokkur == 30 & tognumer %in% c(1:39, NA))) %>% # | (synaflokkur == 35 & tognumer < 75)) %>%  
  mutate(synaflokkur = ifelse(synaflokkur == 30, ' Spring survey','Autumn survey')) %>%
  #filter(year>1992,year<tyr) %>% 
  left_join(tbl(mar,'reitmapping') %>% 
              rename(gridcell = GRIDCELL) %>% 
              select(-c(id,lat,lon,size))) %>%
  region.map() %>% 
  group_by(year,region,synaflokkur) %>% 
  summarise(c=sum(B)/1e3) %>% 
  collect(n=Inf) %>% 
  ungroup() %>% 
  mutate(region = forcats::fct_relevel(region,'W','NW','NE','SE','SW','Other')) %>% 
  arrange(region) %>% 
  ggplot(aes(year,c,fill=region)) + 
  geom_bar(stat='identity',position = 'fill') + 
  theme_light() +
  theme(axis.text.y=element_text(angle=90),
        legend.position=" ",
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin = margin(0,0,0,0,'cm'))+
#  facet_wrap(~synaflokkur) +
  labs(y="Proportion (of index)",x="Year") +
  scale_fill_brewer(palette='YlOrRd',drop=FALSE,direction = -1) +
  plot_layout(ncol=1)

