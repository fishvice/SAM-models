library(patchwork)

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

######------Need to add plots here that demonstrate data availability and spatial coverage
#to aid in identifying groupings by strata, area, gridcell, gear, and synaflokkur in 03-catch-at-age