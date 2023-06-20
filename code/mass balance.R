# Mass balance PCB and dioxin ----
FB <- merge(feedstock_summary, biochar_summary, by = c("feedstock", "pollutant_class"), all = TRUE)
FBG <- read_xlsx("processed_data/FBG.xlsx") %>% 
  filter(pollutant_class != "PAH") %>% 
  rename(sum_pollutant_F = sum_pollutant.x,
         sum_pollutant_BC = sum_pollutant.y,
         sum_pollutant_gas = sum_pollutant)

perc_red <- FBG %>% 
  group_by(sample_ID.y, pollutant_class) %>% 
  summarise(perc_red_BC = (1-sum_pollutant_BC/sum_pollutant_F)*100,
            perc_in_BC = (sum_pollutant_BC/sum_pollutant_F)*100,
            perc_in_gas = (sum_pollutant_gas/sum_pollutant_F)*100) %>% 
  drop_na(sample_ID.y)

write_xlsx(perc_red, "processed_data/percent_reduced.xlsx")
