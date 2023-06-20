rings <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx", 13)
chlorines <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx", 14)

EF_rings <- merge(EF, rings, all = TRUE)
EF_rings_cl <- merge(EF_rings, chlorines, all = TRUE)

EF_rings %>% 
  filter(pollutant_class == "PAH",
         conc_blank_corr < 2e+5) %>% 
  ggplot(aes(x = conc_blank_corr, y = rings, group = phase)) +
  geom_point(aes(color = phase, shape = carcinogenic),
             size = 4,
             alpha = 0.7)

PAH_particles <- EF_rings %>% 
  filter(pollutant_class == "PAH") %>% 
  group_by(phase, carcinogenic) %>% 
  summarise(mean_pollutant = mean(conc_blank_corr),
            sd_pollutant = sd(conc_blank_corr),
            RSD = sd_pollutant*100/mean_pollutant) %>% 
  pivot_wider(names_from = carcinogenic,
              values_from = c(mean_pollutant, sd_pollutant, RSD)) %>% 
  mutate(perc_carc = mean_pollutant_TRUE/(mean_pollutant_FALSE+mean_pollutant_TRUE)*100,
         RSD = sqrt(RSD_TRUE^2+(RSD_FALSE+RSD_TRUE)^2))
  
  ggplot(aes(x = phase, y = conc_blank_corr, fill = carcinogenic)) +
  geom_bar(position=position_dodge(), 
           stat="identity")
  
  
ggplot(aes(x = feedstock, y = sum_pollutant, 
           group = temperature, 
           fill = sample_ID_common)) +
  geom_bar(position=position_dodge(), 
           stat="identity")
  
  mutate(temperature = factor(temperature, 
                              levels = temperature_order),
         feedstock = factor(feedstock,
                            levels = feedstock_order)) %>% 
  ggplot() +
  geom_point(aes(x = feedstock, y = conc_blank_corr),
             group = rings)

  EF_rings_cl %>% 
    filter(pollutant_class != "PAH") %>% 
    ggplot(aes(x = conc_blank_corr, y = chlorines, group = phase)) +
    geom_point(aes(color = phase))
  
  

  filter <- FB_carc_summary %>% 
  filter(pollutant_class == "PAH",
         type == "biochar",
         sum_pollutant < 1)
  
  
 cl_corr_gas <- EF_rings_cl %>% 
    select(pollutant, sample_ID_common, pollutant_class, feedstock, 
           temperature, phase, phase2, conc_blank_corr, abbreviation, rings, chlorines) %>% 
   pivot_wider(names_from = phase,
               values_from = conc_blank_corr) %>% 
   filter(pollutant_class == "dioxin") %>% 
   group_by(chlorines) %>% 
   summarise(sum_cl_gas = sum(gas),
             sum_cl_particle = sum(particle)) %>% 
   mutate(frac_cl_gas = sum_cl_gas/(sum_cl_gas+sum_cl_particle))
  
  
 cl_corr_feedstock <- merge(feedstock_biochar_means, chlorines, all = TRUE) %>% 
   filter(type == "feedstock") %>% 
   select(pollutant, sample_ID_common, pollutant_class, feedstock, 
          temperature, mean_conc, sd_conc, abbreviation, chlorines) %>% 
   filter(pollutant_class == "dioxin") %>% 
   group_by(chlorines) %>% 
   summarise(sum_cl = sum(mean_conc)) %>% 
   pivot_wider(names_from = chlorines,
               values_from = sum_cl)
 
  
  
  
  
  
  
  
  
  
 dioxins_gas <- gas_particle %>% 
  filter(pollutant_class == "dioxin")
    
  

  oil <- data_summary_GP_together %>% 
    filter(phase2 == "oil")
write_xlsx(oil, "processed_data/oil.xlsx")  


biochar_summary %>% 
  lm(temperature~sum_pollutant,.) %>% 
  summary()




congeners <- merge(feedstock_means, biochar_means, 
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

