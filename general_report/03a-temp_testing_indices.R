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
#Höski's results - will compare with indices created here in next section
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
         fixed == 0) %>% 
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
  filter(lengd == cutoffs[[1]][1]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y='Total biomass') +
  expand_limits(y=0) +
  #  geom_point(data=smh.index %>% 
  #               filter(lengd == cutoffs[1]),
  #             aes(ar,bio.staerri)) +
  geom_pointrange(data=smh.index.tr %>% 
                    filter(lengd == cutoffs[[1]][1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l))
## biomass > 40 cm
b40_plot.tr <- 
  smb.index.tr %>% 
  filter(lengd == cutoffs[[3]][1]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y=paste0('Biomass > ',cutoffs[[3]][1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index.tr %>% 
                    filter(lengd == cutoffs[[3]][1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## biomass > 60

b60_plot.tr <- 
  smb.index.tr %>% 
  filter(lengd == cutoffs[[4]][1]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Biomass > ',cutoffs[[4]][1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index.tr %>% 
                    filter(lengd == cutoffs[[4]][1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## abundance < 30

a30_plot.tr <- 
  smb.index.tr %>% 
  filter(lengd == cutoffs[[2]][2]) %>% 
  ggplot(aes(ar,fj.minni)) + 
  geom_ribbon(aes(ymin=fj.minni.l,ymax=fj.minni.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Abundance < ',cutoffs[[2]][2])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index.tr %>% 
                    filter(lengd == cutoffs[[2]][2]), #was 40
                  aes(ar,fj.minni,ymax=fj.minni.u,ymin=fj.minni.l)) 




smb.index <- 
  calc_index(mar, 
             total_biomass_length_ranges = cutoffs[[1]],
             other_length_ranges = cutoffs[c(-1)]) %>% 
  filter(synaflokkur == 30) %>% 
  mutate(fj.minni = n,
         fj.minni.u = n*(1+n.cv),
         fj.minni.l = n*(1-n.cv),
         bio.staerri = b,
         bio.u = b*(1+b.cv),
         bio.l = b*(1-b.cv)) 

smh.index <- 
  calc_index(mar,  
             total_biomass_length_ranges = cutoffs[[1]],
             other_length_ranges = cutoffs[c(-1)]) %>% 
  filter(synaflokkur == 35) %>% 
  mutate(fj.minni = n,
         fj.minni.u = n*(1+n.cv),
         fj.minni.l = n*(1-n.cv),
         bio.staerri =b,
         bio.u = b*(1+b.cv),
         bio.l = b*(1-b.cv)) 


## total biomass plot

total_bio_plot <- 
  smh.index %>% 
  filter(index == names(cutoffs)[1]) %>% 
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
                    filter(index == names(cutoffs)[1]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l))
## biomass > 40 cm
b40_plot <- 
  smb.index %>% 
  filter(index == names(cutoffs)[3]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='',y=paste0('Biomass > ',cutoffs[[3]][1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(index == names(cutoffs)[3]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## biomass > 60

b60_plot <- 
  smb.index %>% 
  filter(index == names(cutoffs)[4]) %>% 
  ggplot(aes(ar,bio.staerri)) + 
  geom_ribbon(aes(ymin=bio.l,ymax=bio.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Biomass > ',cutoffs[[4]][1])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(index == names(cutoffs)[4]),
                  aes(ar,bio.staerri,ymax=bio.u,ymin=bio.l)) 
## abundance < 30

a30_plot <- 
  smb.index %>% 
  filter(index == names(cutoffs)[2]) %>% 
  ggplot(aes(ar,fj.minni)) + 
  geom_ribbon(aes(ymin=fj.minni.l,ymax=fj.minni.u),fill='grey') + 
  geom_line() + 
  theme_light() +
  labs(x='Year',y=paste0('Abundance < ',cutoffs[[2]][2])) +
  expand_limits(y=0) +
  geom_pointrange(data=smh.index %>% 
                    filter(index == names(cutoffs)[2]), #was 40
                  aes(ar,fj.minni,ymax=fj.minni.u,ymin=fj.minni.l)) 



smb.index.tr %>% 
  filter(lengd == cutoffs[[1]][1]) %>% 
  select(ar, bio.staerri) %>% 
  left_join(smb.index %>% 
              filter(index == names(cutoffs)[1]) %>% 
              select(ar, b) %>% 
              mutate(b = b)) %>% 
  ggplot(aes(ar, bio.staerri)) + geom_line() + geom_line(aes(ar, b/4), col = 2, linetype = 2)

smh.index.tr %>% 
  filter(lengd == cutoffs[[1]][1]) %>% 
  select(ar, bio.staerri) %>% 
  left_join(smh.index %>% 
              filter(index == names(cutoffs)[1]) %>% 
              mutate(b = b) %>% #for cod only
              select(ar, b) 
              ) %>% 
  ggplot(aes(ar, bio.staerri)) + geom_line() + geom_line(aes(ar, b), col = 2, linetype = 2)


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

#There are some very minor differences in cv at early years. Otherwise looks good for haddock .
#CVs are quite a bit smaller - could it be differences in the sN? This should probably be tracked
#like the sums are and used in the calc.cv function

four_plot
four_plot.tr

total_plot <- 
  total_bio_plot.tr + 
  total_bio_plot + 
  plot_layout(ncol=1)

b40_plot <- 
  b40_plot.tr + 
  b40_plot + 
  plot_layout(ncol=1)

smb.index.tr %>% 
  filter(lengd == cutoffs[[1]][1]) %>% 
  select(ar, bio.staerri) %>% 
  left_join(smb.index %>% 
              filter(index == names(cutoffs)[1]) %>% 
              select(ar, b) %>% 
              mutate(b = b)) %>% 
  ggplot(aes(ar, bio.staerri)) + geom_line() + geom_line(aes(ar, b), col = 2, linetype = 2)

smh.index.tr %>% 
  filter(lengd == cutoffs[[3]][1]) %>% 
  select(ar, bio.staerri) %>% 
  left_join(smh.index %>% 
              filter(index == names(cutoffs)[3]) %>% 
              mutate(b = b) %>% #for cod only
              select(ar, b) 
  ) %>% 
  ggplot(aes(ar, bio.staerri)) + geom_line() + geom_line(aes(ar, b), col = 2, linetype = 2)


b60_plot <-
  b60_plot.tr + 
  b60_plot + 
  plot_layout(ncol=1)

a30_plot <-
  a30_plot.tr + 
  a30_plot + 
  plot_layout(ncol=1)
