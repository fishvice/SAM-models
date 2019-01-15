library(readxl)
tac_hist <- read_excel('data/TAC.xlsx')


## transfers
transfer_table <- 
  mar:::kvoti_stada_summarised(mar) %>%
  filter(fteg==2) %>% 
  collect(n=Inf) %>% 
  mutate(timabil = ifelse(str_sub(timabil,1,1) %in% "9",
                          paste0(1900+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)),
                          paste0(2000+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)))) %>% 
  arrange(fteg, timabil) %>% 
  ungroup() %>% 
  mutate(diff=varanlegt - afli,
         diffp=diff/varanlegt,
         diff =sprintf("%s (%.1f %%)",diff,100*diffp),
         stada = sprintf("%s (%.1f %%)",stada,100*stada/kvoti)) %>% 
  select(Period=timabil,TAC=varanlegt, Catch=afli,Diff=diff,TACtrans = kvoti,Diff_trans=stada) 


transfer_plot <- 
  mar:::kvoti_stada_summarised(mar) %>%
  filter(fteg==2) %>% 
  collect(n=Inf) %>% 
  mutate(timabil = ifelse(str_sub(timabil,1,1) %in% "9",
                          paste0(1900+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)),
                          paste0(2000+as.integer(str_sub(timabil,1,2)),"/",str_sub(timabil,3)))) %>% 
  arrange(fteg, timabil) %>% 
  head(-1) %>% 
  ungroup() %>% 
  mutate(m_ara = n_ar,
         m_p = 100*m_ara/varanlegt,
         til_p = 100*tilf/varanlegt,
         m_ara = m_ara/1e3,
         tilf = tilf/1e3) %>% 
  select(timabil,tilf,m_ara,
         m_p,til_p) %>% 
  gather(col,value,-timabil) %>%
  mutate(col=ifelse(col=='m_ara','Between years',
                    ifelse(col=='m_p','Between years (%)',
                           ifelse(col=='tilf','Between species',
                                  'Between species (%)')))) %>% 
  ggplot(aes(timabil,value)) + geom_bar(stat='identity') + 
  facet_wrap(~col,ncol=2,scale='free_y') + 
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x='Quota period',y='Transfers (in t)')
