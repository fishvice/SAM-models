rby <- 
  read_delim('Ass/Adapt/PrognosisBaediroll/resultsbyyear',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))
rbyage <- 
  read_delim('Ass/Adapt/PrognosisBaediroll/resultsbyyearandage',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))
rbage <- 
  read_delim('Ass/Adapt/PrognosisBaediroll/resultsbyage',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))

rby_old <- 
  read_delim('../../2017/02/Ass/Adapt/PrognosisBaediroll/resultsbyyear',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))
rbyage_old <- 
  read_delim('../../2017/02/Ass/Adapt/PrognosisBaediroll/resultsbyyearandage',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))


  



rby_haust <- 
  read_delim('Ass/Adapt/PrognosisHaustrall/resultsbyyear',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))
rbyage_haust <- 
  read_delim('Ass/Adapt/PrognosisHaustrall/resultsbyyearandage',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))


rby_vor <- 
  read_delim('Ass/Adapt/PrognosisMarsrall/resultsbyyear',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))
rbyage_vor <- 
  read_delim('Ass/Adapt/PrognosisMarsrall/resultsbyyearandage',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))

## find old assessments
library(icesSAG)
library(tidyverse)
library(icesSAG)

options(icesSAG.use_token = FALSE)

tmp <- 
  getListStocks(0) %>% 
  as_data_frame()

retro <- 
  tmp %>% 
  filter(StockKeyLabel %in% c('had-iceg','had.27.5a'),
         AssessmentYear > 2011) %>%
  select(AssessmentKey) %>% 
  split(.$AssessmentKey) %>% 
  map(~getSummaryTable(.$AssessmentKey)) %>% 
  map(~bind_rows(.) %>% 
        select(-c(stockSizeUnits,fishingPressureUnits))) %>% 
  bind_rows() %>% 
  as_data_frame()


## model diagnostics
model_bubble_plot <- 
  rbyage %>% 
  filter(age < 11, year < tyr) %>% 
  mutate(#`Catch in numbers` = ifelse(ObsCno==-1,NA,ObsCno)-CalcCno,
         `Spring survey in numbers` = ifelse(ObsSurveyNr==-1,NA,ObsSurveyNr) - CalcSurveyNr,
         `Autumn survey in numbers` = ifelse(ObsSurveyNr2==-1,NA,ObsSurveyNr2) - CalcSurveyNr2) %>% 
  select(year,age, contains('in numbers')) %>% 
  gather(source,delta,-c(year,age)) %>%
  group_by(source,age) %>% 
  mutate(sign = ifelse(delta>0,1,0),
         delta = abs(delta)/sd(delta,na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(year,age,size=delta,col = as.factor(sign))) + 
  geom_point( shape=1) + 
  facet_wrap(~source) +
  scale_color_manual(values = c('red','blue'))+
  scale_size_area(max_size = 10) + 
  theme_bw() +
  theme(legend.position = 'none',
        strip.background = element_blank()) + 
  labs(y='Age',x='Year')


## model output

model_bio_plot <- 
  rby %>% 
  mutate(`B45cm+`= lag(HCRRefbio,1)) %>% 
  select(year,SSB,`B45cm+`) %>% 
  gather(type,val,-c(year)) %>% 
  ggplot(aes(year,val,col=type)) + geom_line() + 
  geom_vline(xintercept = tyr,lty=2,col='gray') +
  geom_hline(yintercept = 45) + 
  geom_hline(yintercept = 59,lty=2) + 
  geom_label(data=data_frame(x=tyr + 2,y=45,label='Blim = 49 kt'),aes(x,y,label=label),col='black') +
  geom_label(data=data_frame(x=tyr + 2,y=59,label='Bpa = 59 kt'),aes(x,y,label=label),col='black') +
  theme_light() + 
  labs(x='Year',y='SSB/B45cm+',col='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.2,0.2))

model_catch_plot <- 
  rby %>% 
  select(year,obscatch,calccatch) %>%
  mutate(val = ifelse(year < tyr, obscatch, calccatch)) %>% 
  ggplot(aes(year,val)) + geom_line() + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='Year',y='Catch (in kt)') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.1,0.1))

model_F_plot <- 
  rby %>% 
  mutate(HR = ifelse(year < tyr, obscatch, calccatch)/lag(HCRRefbio,1)) %>% 
  select(year,`F4-7`,HR) %>% 
  gather(type,val,-c(year)) %>% 
  ggplot(aes(year,val,col=type)) + geom_line() + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='',y='F4-7/HR',col='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.2,0.2))
  
model_rec_plot <- 
  rby %>% 
  ggplot(aes(year,N2/1e3)) + geom_line() + 
  geom_vline(xintercept = tyr+1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='',y='Recruitment (in millions)',col='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.1,0.1))

ices_plot_1 <- 
  model_catch_plot + 
  model_rec_plot +  plot_layout(ncol=2)
ices_plot_2 <-   
  model_F_plot + 
  model_bio_plot + plot_layout(ncol = 2)

## retro plot 

model_retro_plot <- 
  retro %>% 
  ggplot(aes(Year,SSB/1e3,lty=as.factor(AssessmentYear)))+ geom_line() +
  geom_line(aes(year,SSB),data=rby %>% filter(year<=tyr),lty=1,col='red') +
  theme_light() + 
  labs(x='',y='SSB (in kt)',lty='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = 'none') +
  
  retro %>% 
  ggplot(aes(Year,recruitment/1e6,lty=as.factor(AssessmentYear)))+ geom_line() +
  geom_line(aes(year,N2/1e6),data=rby %>% filter(year<=tyr),lty=1,col='red') +
  theme_light() + 
  labs(x='',y='Recruitment (in millions)',lty='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = 'none') +
  
  retro %>% 
  ggplot(aes(Year,F,lty=as.factor(AssessmentYear)))+ geom_line() +
  geom_line(aes(year,`F4-7`),data=rby %>% filter(year<tyr),lty=1,col='red') +
  theme_light() + 
  labs(x='Year',y='F (4-7)',lty='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = 'none')+
  plot_layout(ncol=2)

## Alternative model runs (i.e. leave one-out runs)


## model output


model_l1_bio_plot <- 
  rby %>%
  mutate(run = ' Full') %>% 
  bind_rows(rby_haust %>% mutate(run = 'Autumn'),
            rby_vor %>% mutate(run = 'Spring')) %>% 
  group_by(run) %>% 
  mutate(`B45cm+`= lag(HCRRefbio,1)) %>% 
  ungroup() %>% 
  select(year,SSB,`B45cm+`,run) %>% 
  gather(type,val,-c(year,run)) %>% 
  ggplot(aes(year,val,col=type,lty=run)) + geom_line() + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='Year',y='SSB/B45cm+',col='',lty='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.1,0.2))

model_l1_catch_plot <- 
  rby %>%
  mutate(run = ' Full') %>% 
  bind_rows(rby_haust %>% mutate(run = 'Autumn'),
            rby_vor %>% mutate(run = 'Spring')) %>% 
  select(year,obscatch,calccatch,run) %>%
  mutate(val = ifelse(year < tyr, obscatch, calccatch)) %>% 
  ggplot(aes(year,val,lty=run)) + geom_line() + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='Year',y='Catch (in kt)',lty='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.1,0.1))

model_l1_F_plot <- 
  rby %>%
  mutate(run = ' Full') %>% 
  bind_rows(rby_haust %>% mutate(run = 'Autumn'),
            rby_vor %>% mutate(run = 'Spring')) %>% 
  group_by(run) %>% 
  mutate(HR = ifelse(year < tyr, obscatch, calccatch)/lag(HCRRefbio,1)) %>% 
  ungroup() %>% 
  select(year,`F4-7`,HR,run) %>% 
  gather(type,val,-c(year,run)) %>% 
  ggplot(aes(year,val,col=type,lty=run)) + geom_line() + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='',y='F4-7/HR',col='',lty='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.2,0.2))

model_l1_rec_plot <- 
  rby %>%
  mutate(run = ' Full') %>% 
  bind_rows(rby_haust %>% mutate(run = 'Autumn'),
            rby_vor %>% mutate(run = 'Spring')) %>%
  ggplot(aes(year,N2/1e3,lty=run)) + geom_line() + 
  geom_vline(xintercept = tyr-1,lty=2,col='gray') + 
  theme_light() + 
  labs(x='',y='Recruitment (in millions)',col='',lty='') + 
  expand_limits(y=0) + 
  theme(legend.background = element_blank(),
        legend.position = c(0.1,0.8))

model_l1_plot <-
  model_l1_F_plot + theme(legend.position = 'top') +
  model_l1_bio_plot + theme(legend.position = 'top') +
  #model_l1_catch_plot + 
  
  plot_layout(ncol=1)

## comparison of predicted and realised weights
pred_result_plot <- 
  rbyage %>% 
  mutate(run = tyr) %>% 
  bind_rows(rbyage_old %>% mutate(run = tyr-1)) %>% 
  filter(year == tyr -1) %>% 
  select(age,run,`Catch in numbers`=CalcCno,
         `Catch weights`=CatchWts,
         `Stock in numbers`=N) %>%
  left_join(rbyage %>% 
              mutate(run = tyr) %>% 
              bind_rows(rbyage_old %>% 
                          mutate(run = tyr-1)) %>% 
              filter(year == tyr) %>% 
              select(run,age,`Mean weight at age in stock`=StockWts)) %>% 
  mutate(`Catch in biomass at age` = `Catch weights`*`Catch in numbers`/1e3,
         `Biomass at age` = `Stock in numbers`*`Mean weight at age in stock`/1e3) %>% 
  gather(type,val,-c(age,run)) %>% 
  ggplot(aes(age,val,fill=as.factor(run))) + 
  geom_bar(stat = 'identity',position = 'dodge') + 
  facet_wrap(~type,scale='free_y') +
  theme_bw()+
  theme(strip.background = element_blank()) +
  scale_fill_manual(values = c('red','blue')) +
  labs(fill = '',y='',x='Age')

model_agg_fit_plot <- 
  rby %>% 
  select(matches('^year|urvey')) %>% 
  gather(data,value,-year) %>% 
  filter(value!=-1) %>% 
  mutate(type=gsub('(Calc|Obs)[A-Za-z0-9]+','\\1',data),
         data=ifelse(grepl('2',data),'Autumn','Spring')) %>% 
  spread(type,value) %>%
  mutate(Obs = ifelse(year==2011&data=='Autumn',NA,Obs)) %>% 
  filter(!is.na(Obs)) %>% 
  ggplot(aes(year,Obs,col=data)) + geom_point() + 
  geom_line(aes(y=Calc))+ theme_classic() +
  labs(x='Year',y='Biomass index',col='') +
  theme(legend.position = c(0.2,0.8))

model_fit_plot <- 
rbyage %>% 
  select(year,age,Obs_Spring=ObsSurveyNr,Obs_Autumn=ObsSurveyNr2,
         pred_Spring=CalcSurveyNr,pred_Autumn=CalcSurveyNr2) %>% 
  mutate_all(~ifelse(.==-1| .==0,NA,.)) %>% 
  gather(type,value,-c(year,age)) %>% 
  separate(type,c("type","time")) %>%
  mutate(value = ifelse(time=='Spring'&year<1985,NA,
                        ifelse(time=='Autumn'&(!(year %in% c(1996:(tyr-1)))),NA,value)),
         value = ifelse(time=='Autumn'&year==2011,NA,value)) %>%
  filter(!is.na(value),
         year <=tyr) %>%
  ggplot(aes(year,value,lty=type)) + geom_line() + 
  facet_grid(age~time,scale='free') + 
  theme_bw() + 
  theme(strip.background = element_blank(),
        legend.position = 'none') +
  labs(y='Abundance index',x='Year')


model_xy_plot <- 
  rbyage %>% 
  select(year,age,Obs_spring=ObsSurveyNr,Obs_autumn=ObsSurveyNr2,
         pred_spring=CalcSurveyNr,pred_autumn=CalcSurveyNr2) %>% 
  mutate_all(~ifelse(.==-1| .==0,NA,.)) %>% 
  gather(type,value,-c(year,age)) %>% 
  separate(type,c("type","time")) %>%
  mutate(value = ifelse(time=='spring'&year<1985,NA,
                        ifelse(time=='autumn'&(!(year %in% c(1996:(tyr-1)))),NA,value))) %>% 
  filter(!is.na(value),
         year <=tyr) %>%
  spread(type,value) %>% 
  ggplot(aes(Obs,pred,label=year)) + geom_text() + 
  facet_wrap(~age+time,scale='free') + 
  geom_abline(slope=1,intercept=0,lty=2) +
  theme_bw() + 
  theme(strip.background = element_blank(),
        legend.position = 'none')

  
selection_plot <- 
  rbyage %>% 
  group_by(year) %>% 
  filter(F!=-1,age<9)%>% 
  mutate(s=F/max(F)) %>% 
  ggplot(aes(year,s,lty=as.factor(age),group=age)) + geom_line() + 
  geom_label(aes(label=age)) +
  theme_bw() +
  theme(legend.position = 'none') +
  labs(x='Year',y='Selection at age')

survey_sigma_plot <- 
  rbage %>% 
  select(age,Spring=surveysigma,Autumn = SurveySigma2) %>% 
  gather(survey,sigma,-age) %>% 
  filter(sigma != -1) %>% 
  ggplot(aes(age,sigma,lty = survey)) + geom_line() + 
  theme_bw() + 
  theme(legend.position = c(0.2,0.8),
        legend.background = element_blank()) + 
  labs(x='Age',y='CV',lty='')



survey_q_plot <- 
  rbage %>% 
  select(age,Spring=SurveylnQ,Autumn = surveylnQ2) %>% 
  gather(survey,q,-age) %>% 
  filter(q != -1) %>%
  mutate(q=exp(q)) %>% 
  ggplot(aes(age,q,lty = survey)) + geom_line()+ 
  theme_bw() + 
  theme(legend.position = c(0.8,0.2),
        legend.background = element_blank()) + 
  labs(x='Age',y='Catchability',lty='')
