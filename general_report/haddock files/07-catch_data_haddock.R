library(GGally)
rby <- 
  read_delim('Ass/Adapt/PrognosisBaediroll/resultsbyyear',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))
rbyage <- 
  read_delim('Ass/Adapt/PrognosisBaediroll/resultsbyyearandage',delim = '\t') %>% 
  set_names(.,str_trim(names(.)))


scale_fill_crayola <- function(n = 100, ...) {
  
  # taken from RColorBrewer::brewer.pal(12, "Paired")
  pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
           "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
           "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
  pal <- rep(pal, n)
  ggplot2::scale_fill_manual(values = pal, ...)
  
}


scale_col_crayola <- function(n = 100, ...) {
  
  # taken from RColorBrewer::brewer.pal(12, "Paired")
  pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
           "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00",
           "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
  pal <- rep(pal, n)
  ggplot2::scale_color_manual(values = pal, ...)
  
}

## tables 
catage_table <- 
  rbyage %>% 
  filter(year < tyr,ObsCno > 0,age < 11) %>% 
  select(year,age,ObsCno) %>% 
  mutate(ObsCno = ObsCno/1e3) %>% 
  spread(age,ObsCno,fill=0)

catwt_table <- 
  rbyage %>% 
  filter(year < tyr, CatchWts> 0,age < 11) %>% 
  select(year,age,CatchWts) %>% 
  mutate(CatchWts = CatchWts) %>% 
  spread(age,CatchWts,fill=0)

igfs_wt_table <- 
  rbyage %>% 
  filter(year < tyr, StockWts> 0,age < 11) %>% 
  select(year,age,StockWts) %>% 
  mutate(StockWts = StockWts) %>% 
  spread(age,StockWts,fill=0)

igfs_mat_table <- 
  rbyage %>% 
  filter(year < tyr, StockSexmat> 0,age < 12) %>% 
  select(year,age,StockSexmat) %>% 
  mutate(StockSexmat = StockSexmat) %>% 
  spread(age,StockSexmat,fill=0)

igfs_age_table <- 
  rbyage %>% 
  filter(ObsSurveyNr > 0,age < 11) %>% 
  select(year,age,ObsSurveyNr) %>% 
  spread(age,ObsSurveyNr,fill=0)


agfs_age_table <- 
  rbyage %>% 
  filter(ObsSurveyNr2 > 0,age < 11) %>% 
  select(year,age,ObsSurveyNr2) %>% 
  spread(age,ObsSurveyNr2,fill=0)




catage_bar_plot <- 
  rbyage %>% 
  filter(year < tyr) %>% 
  mutate(yc = as.factor(year - age)) %>% 
  ggplot(aes(age,ObsCno/1e3,fill=yc)) + 
  geom_bar(stat='identity') + 
  facet_wrap(~year,scale='free_y',dir='v',strip.position = 'left') +
  theme_light() + 
  labs(x='Age',y='C@age (in thous)') +
  scale_fill_crayola() + 
  theme(legend.position = 'none')


catage_bubble_plot <- 
  rbyage %>% 
  filter(year < tyr,ObsCno>=0,age < 11) %>% 
  mutate(yc = as.factor(year - age)) %>% 
  ggplot(aes(year,age,size=abs(ObsCno/1e3),col=yc)) + 
  geom_point() + 
  scale_size_area(max_size = 10) +
  scale_col_crayola() + 
  theme_light() + 
  labs(x='Year',y='Age') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45)) +
  # ylim(c(1,10)) + 
  scale_y_continuous(breaks = 1:10) + 
  scale_x_continuous(breaks = 1979:(tyr-1))

## catch weight plot 
catwt_plot <- 
  rbyage %>% 
  group_by(age) %>% 
  dplyr::mutate(m=mean(CatchWts),
                pred = ifelse(year<tyr,0,1)) %>% 
  filter(age>1,age < 11) %>% 
  ggplot(aes(year,CatchWts/1e3)) + #geom_point() + 
  geom_segment(aes(yend=m/1e3,xend=year,col=as.factor(pred))) + 
  scale_color_manual(values=c('blue','red')) +   
  facet_wrap(~age,scale='free_y') + 
  theme_light() + 
  labs(x='Year',y='Mean weight (kg)') + 
  theme(legend.position = 'none')


## maturity plot from IGFS
igfs_mat_plot <- 
  rbyage %>% 
  select(year,age,mat=StockSexmat) %>% 
  filter(age>1,age<11) %>% 
  group_by(age) %>% 
  dplyr::mutate(m=mean(mat),
                pred = ifelse(year<tyr,0,1)) %>% 
  ggplot(aes(year,m)) + #geom_point() + 
  geom_segment(aes(yend=mat,xend=year,col=as.factor(pred))) + 
  scale_color_manual(values=c('blue','red')) +   
  facet_wrap(~age,scale='free_y') + 
  theme_light() + 
  labs(x='Year',y='(%) mature') + 
  theme(legend.position = 'none')
        
  
## stock weights from IGFS
igfs_wt_plot <- 
  rbyage %>% 
  group_by(age) %>% 
  dplyr::mutate(m=mean(StockWts),
                pred = ifelse(year<tyr,0,1)) %>% 
  filter(age>1,age < 11) %>% 
  ggplot(aes(year,StockWts/1e3)) + #geom_point() + 
  geom_segment(aes(yend=m/1e3,xend=year,col=as.factor(pred))) + 
  scale_color_manual(values=c('blue','red')) +   
  facet_wrap(~age,scale='free_y') + 
  theme_light() + 
  labs(x='Year',y='Mean weight (kg)') + 
  theme(legend.position = 'none')

## CPUE plot

cpue_plot <- 
  tbl(mar,'had_catch') %>% 
  filter(gear %in% c('BMT','LLN','GIL','DSE'),year>1984,year < tyr) %>% 
  mutate(effort = nvl(towtime/60,nvl(hooks/1000,nvl(nr_net,1)))) %>%
  mutate(effort = ifelse(gear == 'DSE',1,effort)) %>% 
  mutate(cpue = ifelse(effort>0,catch/effort,0)) %>% 
  filter(cpue>0) %>% 
    group_by(year,gear) %>% 
  summarise(cpue = mean(cpue)) %>% 
  collect(n=Inf) %>% 
  bind_rows(tbl(mar,'had_catch') %>% 
              filter(gear %in% c('BMT','LLN','GIL','DSE'),year>1984,
                     catch/total > 0.5, year < tyr) %>% 
              mutate(effort = nvl(towtime/60,nvl(hooks/1000,nvl(nr_net,1)))) %>%
              mutate(effort = ifelse(gear == 'DSE',1,effort)) %>% 
              mutate(cpue = ifelse(effort>0,catch/effort,0)) %>% 
              filter(cpue>0) %>% 
              group_by(year,gear) %>% 
              summarise(cpue = mean(cpue)) %>% 
              collect(n=Inf), 
            .id = 'Prop') %>% 
  ungroup() %>% 
  ggplot(aes(year,cpue,lty=Prop)) + 
  geom_line() + 
  scale_linetype_manual(labels = c('All records','> 50% of catch'),values = 1:2) +
  facet_wrap(~gear,scale='free_y') + 
  theme_bw() + 
  labs(y='Catch per unit effort',x='Year',lty='') +
  theme(legend.position = c(0.1,0.8),
        legend.background = element_blank(),
        strip.background = element_blank())


survey_bubble_plot <- 
  rbyage %>% 
  select(year,age,Spring=ObsSurveyNr,Autumn=ObsSurveyNr2) %>% 
  gather(survey,index,-c(year,age)) %>% 
  filter(year <= tyr,index>=0,age < 11) %>% 
  mutate(yc = as.factor(year - age)) %>% 
  ggplot(aes(year,age,size=abs(index/1e3),col=yc)) + 
  geom_point() + 
  scale_size_area(max_size = 10) +
  scale_col_crayola() + 
  theme_bw() + 
  facet_wrap(~survey) + 
  labs(x='Year',y='Age') +
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45),
        strip.background = element_blank()) +
  # ylim(c(1,10)) + 
  scale_y_continuous(breaks = 1:10) + 
  scale_x_continuous(breaks = 1979:(tyr-1))


igfs_consistency_plot <- 
  rbyage %>% 
  select(year,age,index=ObsSurveyNr) %>% 
  filter(year <= tyr, index >=0) %>% 
  bind_rows(data_frame(year=1974:1984)) %>% 
  spread(age,index) %>% 
  select(-c(year,`<NA>`)) %>% 
  mutate(`1`= lead(`1`,1),
         `2`= lead(`2`,2),
         `3`= lead(`3`,3),
         `4`= lead(`4`,4),
         `5`= lead(`5`,5),
         `6`= lead(`6`,6),
         `7`= lead(`7`,7),
         `8`= lead(`8`,8),
         `9`= lead(`9`,9),
         `10`= lead(`10`,10),
         `11`= lead(`11`,11)) %>% 
  ggpairs()

agfs_consistency_plot <- 
  rbyage %>% 
  select(year,age,index=ObsSurveyNr2) %>% 
  filter(year <= tyr, index >=0) %>% 
  bind_rows(data_frame(year=1974:1984)) %>% 
  spread(age,index) %>% 
  select(-c(year,`<NA>`)) %>% 
  mutate(`1`= lead(`1`,1),
         `2`= lead(`2`,2),
         `3`= lead(`3`,3),
         `4`= lead(`4`,4),
         `5`= lead(`5`,5),
         `6`= lead(`6`,6),
         `7`= lead(`7`,7),
         `8`= lead(`8`,8),
         `9`= lead(`9`,9)) %>% 
  ggpairs()
   

between_survey_consistency_plot <- 
  rbyage %>% 
  select(year,age,Spring=ObsSurveyNr, 
         Autumn=ObsSurveyNr2) %>%
  filter(year <= tyr,age< 10) %>%
  mutate_all(~ifelse(.==-1,NA,.)) %>% 
  ggplot(aes(Spring,Autumn)) + geom_point() +
  facet_wrap(~age,scale='free')  + 
  theme_bw() + 
  theme(strip.background = element_blank()) + 
  geom_text(data =  rbyage %>% 
              mutate_all(~ifelse(.==-1,NA,.)) %>% 
              select(year,age,Spring=ObsSurveyNr, 
                     Autumn=ObsSurveyNr2) %>% 
              filter(year <= tyr,age< 10) %>% 
              group_by(age) %>% 
              summarise(corr = cor(Spring,Autumn,use = "pairwise.complete.obs"),
                        Autumn = 0.9*max(Autumn,na.rm = TRUE),
                        Spring = 0.2*max(Spring,na.rm = TRUE)) %>% 
              mutate(label = paste('rho:',round(corr,2))),
            aes(label = label))


test_plot <- 
rbyage %>% 
  select(year,age,index=ObsSurveyNr) %>% 
  mutate_all(~ifelse(.==-1| .==0,NA,.)) %>% 
  filter(year %in% 1985:tyr,age < 11) %>% 
  #bind_rows(data_frame(year=1974:1984)) %>% 
  mutate(yc = year - age) %>% 
  group_by(yc) %>% 
  arrange(yc,age) %>% 
  mutate(d= log(lead(index,1)/index)) %>% 
  ggplot(aes(year,d+age,group=age)) + geom_line() + 
  theme_minimal()
  
  

