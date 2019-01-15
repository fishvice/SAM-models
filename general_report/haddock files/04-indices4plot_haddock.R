library(tidyverse)
library(patchwork)

## huskyverse
if(Sys.info()[['sysname']] == 'Windows'){
  load('//HAFKALDI/u2/reikn/R/SurveyWork/SMB/Allaggrsmbindex.rdata')
  load('//HAFKALDI/u2/reikn/R/SurveyWork/SMH/Allaggrsmhindex.rdata')
  
} else {
  load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMB/Allaggrsmbindex.rdata')
  load('/net/hafkaldi/export/u2/reikn/R/SurveyWork/SMH/Allaggrsmhindex.rdata')
}

marteg <- 2
cutoffs_list<-NULL
cutoffs_list[[2]]<-c(5,30,40,60)

cutoffs<-cutoffs_list[[marteg]]

smb.index <- 
  Allaggrsmbindex %>% 
  filter(svaedi == 'Heild', 
         species == marteg,
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
         species == marteg,
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
  filter(lengd == cutoffs[1]) %>% 
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
                    filter(lengd == cutoffs[1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l))
## biomass > 40 cm
b40_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs[3]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y=paste0('Biomass > ',cutoffs[3])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs[3]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## biomass > 60

b60_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs[4]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Biomass > ',cutoffs[4])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs[4]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## abundance < 30

a30_plot <- 
  smb.index %>% 
  filter(lengd == cutoffs[2]) %>% 
  ggplot(aes(ar,fj.minni)) + 
  geom_ribbon(aes(ymin=fj.minni.l,ymax=fj.minni.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Abundance < ',cutoffs[2])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(lengd == cutoffs[2]), #was 40
                  aes(ar,fj.minni,ymax=fj.minni.u,ymin=fj.minni.l)) 

## 4-plot

four_plot <- 
  total_bio_plot + 
  b40_plot + 
  b60_plot + 
  a30_plot + 
  plot_layout(ncol=2)
