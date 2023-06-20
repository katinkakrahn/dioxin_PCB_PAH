congeners <- merge(feedstock, biochar, 
                   c("feedstock", "pollutant_class", "pollutant","unit", "source")) %>% 
  filter(pollutant_class != "PAH") %>% 
  select(feedstock, pollutant_class, sample_ID.y, pollutant, source, type.x, type.y, LOQ_corr.x, LOQ_corr.y) %>% 
  mutate(congeners_f = ifelse(LOQ_corr.x == FALSE,1,0),
         congeners_bc = ifelse(LOQ_corr.y == FALSE,1,0)) %>% 
  group_by(sample_ID.y, pollutant_class) %>% 
  summarise(sum_congeners_f = sum(congeners_f),
            sum_congeners_bc = sum(congeners_bc)) %>% 
  mutate(perc_cong = (100-(sum_congeners_bc/sum_congeners_f*100)))
write_xlsx(congeners, "processed_data/congeners.xlsx")

congeners_avg <- congeners %>% 
  group_by(pollutant_class) %>% 
  summarise(mean_perc_cong = mean(perc_cong),
            sd_perc_cong = sd(perc_cong))
write_xlsx(congeners_avg, "processed_data/congeners_avg.xlsx")

congeners_poll <- merge(feedstock, biochar, 
                        c("feedstock", "pollutant_class", "pollutant","unit", "source")) %>% 
  filter(pollutant_class != "PAH") %>% 
  select(feedstock, pollutant_class, sample_ID.y, pollutant, source, type.x, type.y, LOQ_corr.x, LOQ_corr.y) %>% 
  mutate(congeners_f = ifelse(LOQ_corr.x == FALSE,1,0),
         congeners_bc = ifelse(LOQ_corr.y == FALSE,1,0)) %>% 
  group_by(pollutant, pollutant_class) %>% 
  summarise(sum_congeners_f = sum(congeners_f),
            sum_congeners_bc = sum(congeners_bc)) %>% 
  mutate(perc_degraded = (100-(sum_congeners_bc/sum_congeners_f*100)))

#PAHs ----
congeners_PAH <- merge(feedstock, biochar, 
                       c("feedstock", "pollutant_class", "pollutant","unit", "source")) %>% 
  filter(pollutant_class == "PAH") %>% 
  select(feedstock, pollutant_class, sample_ID.y, pollutant, source, type.x, type.y, LOQ_corr.x, LOQ_corr.y) %>% 
  mutate(congeners_f = ifelse(LOQ_corr.x == FALSE,1,0),
         congeners_bc = ifelse(LOQ_corr.y == FALSE,1,0)) %>% 
  group_by(sample_ID.y, pollutant_class) %>% 
  summarise(sum_congeners_f = sum(congeners_f),
            sum_congeners_bc = sum(congeners_bc)) %>% 
  mutate(perc_cong = (100-(sum_congeners_bc/sum_congeners_f*100)))

congeners_avg_PAH <- congeners %>% 
  group_by(pollutant_class) %>% 
  summarise(mean_perc_cong = mean(perc_cong),
            sd_perc_cong = sd(perc_cong))

congeners_poll_PAH <- merge(feedstock, biochar, 
                            c("feedstock", "pollutant_class", "pollutant","unit", "source")) %>% 
  filter(pollutant_class == "PAH") %>% 
  select(feedstock, pollutant_class, sample_ID.y, pollutant, source, type.x, type.y, LOQ_corr.x, LOQ_corr.y) %>% 
  mutate(congeners_f = ifelse(LOQ_corr.x == FALSE,1,0),
         congeners_bc = ifelse(LOQ_corr.y == FALSE,1,0)) %>% 
  group_by(pollutant, pollutant_class) %>% 
  summarise(sum_congeners_f = sum(congeners_f),
            sum_congeners_bc = sum(congeners_bc)) %>% 
  mutate(perc_degraded = (100-(sum_congeners_bc/sum_congeners_f*100)))
