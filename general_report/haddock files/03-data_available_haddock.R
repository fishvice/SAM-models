
## length samples from commercial samples
sampling_lengths <- 
  lesa_stodvar(mar) %>% 
  filter(ar < tyr, ar>1999,synaflokkur %in% c(1,2,8)) %>% 
  inner_join(lesa_lengdir(mar) %>% filter(tegund == 2)) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% 
                         c(' ','TRP','HLN','VAR','NPT','PGT','SHT','PSE'),'Other',gear)) %>% 
  group_by(ar,gear) %>% 
  summarise(n = sum(fjoldi,na.rm=TRUE),
            samples = n_distinct(synis_id)) %>% 
  #filter(gear %in% c('BMT','LLN','DSE')) %>% 
  select(year = ar,gear,n,samples) %>% 
  collect(n=Inf) %>% 
  left_join(mfdb::gear %>% rename(gear=name) %>% 
              mutate(gear = as.character(gear),
                     description = as.character(description)) %>% 
              select(gear,description)) %>% 
  mutate(gear = ifelse(is.na(description),'Other',
                       tools::toTitleCase(description))) %>%
  unite(n,c(n,samples),sep='/') %>% 
  select(-description) %>% 
  spread(gear,n, fill= '') 


## age samples from commercial samples
sampling_ages <- 
  lesa_stodvar(mar) %>% 
  filter(ar < tyr, ar>1999,synaflokkur %in% c(1,2,8)) %>% 
  inner_join(lesa_kvarnir(mar) %>% filter(tegund == 2)) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% 
                         c(' ','TRP','HLN','VAR','NPT','PGT','SHT','PSE'),'Other',gear)) %>% 
  group_by(ar,gear) %>% 
  summarise(n = n(),
            samples = n_distinct(synis_id)) %>% 
  #filter(gear %in% c('BMT','LLN','DSE')) %>% 
  select(year = ar,gear,n,samples) %>% 
  collect(n=Inf) %>% 
  left_join(mfdb::gear %>% rename(gear=name) %>% 
              mutate(gear = as.character(gear),
                     description = as.character(description)) %>% 
              select(gear,description)) %>% 
  mutate(gear = ifelse(is.na(description),'Other',tools::toTitleCase(description))) %>% 
  unite(n,c(n,samples),sep='/') %>% 
  select(-description) %>% 
  spread(gear,n, fill= 0) 

## length distributions from commercial samples
tyr12 <- tyr - 15
tyr1 <- tyr - 1 
comm_ldist <- 
  lesa_stodvar(mar) %>% 
  filter(synaflokkur %in% c(1,2,8),
         ar %in% tyr12:tyr1) %>% 
  inner_join(lesa_lengdir(mar)) %>% 
  filter(tegund == 2) %>% 
  left_join(tbl(mar,'gear_mapping')) %>% 
  mutate(gear = ifelse(nvl(gear,' ') %in% 
                         c(' ','TRP','HLN','VAR','NPT','PGT','SHT','PSE'),'Other',gear)) %>% 
  group_by(ar,gear,lengd) %>% 
  summarise(n = sum(fjoldi,na.rm=TRUE)) %>% 
  #filter(gear %in% c('BMT','LLN','DSE')) %>% 
  select(year = ar,gear,length=lengd,n) %>% 
  group_by(year,gear) %>% 
  mutate(p=n/sum(n)) %>% 
  collect(n=Inf)

comm_ldist_plot <- 
  comm_ldist %>% 
  filter(length < 100, gear %in% c('BMT','LLN','DSE')) %>% 
  ungroup() %>% 
  left_join(mfdb::gear %>% rename(gear=name) %>% 
              mutate(gear = as.character(gear),
                     description = as.character(description)) %>% 
              select(gear,description)) %>% 
  mutate(gear = ifelse(is.na(description),'Other',tools::toTitleCase(description))) %>% 
  select(-description) %>% 
  ggplot() + 
  geom_line(aes(length,100*p,col=gear)) +
  geom_line(aes(length,100*p),lty=2, 
            data = comm_ldist %>% 
              filter(length < 100) %>% 
              group_by(year,length) %>% 
              summarise(n=sum(n,na.rm=TRUE)) %>% 
              group_by(year) %>% 
              mutate(p=n/sum(n,na.rm=TRUE)) %>% 
              group_by(length) %>% 
              summarise(p=mean(p,na.rm=TRUE)) %>% 
              ungroup()) + 
  xlim(c(25,100)) +
  facet_wrap(~year,ncol=3)  +
  theme_bw()+
  labs(y='Proportion (%)',x='Length (in cm)',col='') +
  theme(strip.background = element_blank(),strip.text=element_blank(),
        legend.position = 'top') +
  geom_label(data=data_frame(year=tyr12:tyr1),fill='white',label.size=0.2,
             aes(label=year,group=1),x=-Inf,y=Inf,size=3, vjust =
               1.1,hjust=-0.1)

