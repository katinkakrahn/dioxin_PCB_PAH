# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)

# data sets ----
feedstock <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",1) %>% 
  select(-sample_name)
biochar <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",2)
oil <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",3)
PUF <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",4)
XAD <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",5) %>% 
  select(-c(comment,conc_div))
GFF <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",6) %>% 
  select(-c(comment,conc_div))
yield_and_gas <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",7)
TEF <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",8)
limits <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",9)
carcinogens <- read_xlsx("raw_data/PAH_PCDDF_PCB_data_KMK_090223.xlsx",10)
Cl <- read_xlsx("raw_data/Cl_data.xlsx") %>% 
  group_by(feedstock, unit_Cl) %>% 
  summarise(mean_Cl = mean(Cl),
            sd_Cl = sd(Cl)) %>% 
  filter(!(feedstock %in% c("DWSS", "blank")))

# merge ----
data1 <- merge(feedstock, biochar, all = TRUE)
data2 <- merge(data1, oil, all = TRUE)
data3 <- merge(data2, PUF, all = TRUE)
data4 <- merge(data3, XAD, all = TRUE)
data5 <- merge(data4, GFF, all = TRUE)
data6 <- merge(data5, TEF, all.x = TRUE)
all_data <- merge(data6, yield_and_gas, all = TRUE) %>% 
  mutate(TEQ = TEF*conc_blank_corr,
         conc_yield_corrected = ifelse(phase2 == "gas", yield*as.numeric(conc_blank_corr_unit), 
                                       yield*as.numeric(conc_corr)),
         conc_corr_all = ifelse(phase2 == "gas", as.numeric(conc_blank_corr_unit), as.numeric(conc_corr)))

#feedstock ----
feedstock <- all_data %>% 
  filter(type == "feedstock")

feedstock_means <- feedstock %>% 
  filter(LOQ_corr==FALSE) %>% 
  group_by(sample_ID, sample_ID_common, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, LOQ_corr, TEF, TEF_order, unit, source, yield) %>% 
  summarise(mean_conc = mean(as.numeric(conc)),
            sd_conc = sd(as.numeric(conc)),
            mean_TEQ = mean(as.numeric(conc)*TEF),
            sd_TEQ = sd(as.numeric(conc)*TEF),
            n = n())

feedstock_summary <- feedstock_means %>% 
  group_by(sample_ID, sample_ID_common, pollutant_class, unit, temperature, feedstock, source, type, yield) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sd_sum_pollutant = sqrt(sum(sd_conc^2)),
            sum_TEQ = sum(mean_TEQ),
            sd_sum_TEQ = sqrt(sum(sd_TEQ^2)))

feedstock_LOQ <- feedstock %>% 
  filter(LOQ_corr==TRUE) %>% 
  unique()

feedstock_means_LOQ <- merge(feedstock_means, feedstock_LOQ, all = TRUE) %>% 
  mutate(conc_comb = coalesce(as.character(mean_conc), conc))

feedstock_percent_total <- merge(feedstock_means_LOQ, feedstock_summary, all = TRUE) %>% 
  mutate(perc_comp = ifelse(LOQ_corr == FALSE, mean_conc/sum_pollutant*100, NA))

# biochar ----
biochar <- all_data %>% 
  filter(type == "biochar")

biochar_means <- biochar %>% 
  filter(LOQ_corr==FALSE) %>% 
  group_by(sample_ID, sample_ID_common, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, LOQ_corr, TEF, TEF_order, unit, 
           source, yield, phase) %>% 
  summarise(mean_conc = mean(as.numeric(conc_corr)),
            sd_conc = sd(as.numeric(conc_corr)),
            mean_TEQ = mean(as.numeric(conc_corr)*TEF),
            sd_TEQ = sd(as.numeric(conc_corr)*TEF),
            n = n())

biochar_summary <- biochar_means %>% 
  group_by(sample_ID,  sample_ID_common, pollutant_class, unit, temperature, feedstock, source, 
           type, yield, phase) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sd_sum_pollutant = sqrt(sum(sd_conc^2)),
            sum_TEQ = sum(mean_TEQ),
            sd_sum_TEQ = sqrt(sum(sd_TEQ^2)))

biochar_LOQ <- biochar %>% 
  filter(LOQ_corr==TRUE) %>% 
  unique()

biochar_means_LOQ <- merge(biochar_means, biochar_LOQ, all = TRUE) %>% 
  mutate(conc_comb = coalesce(as.character(mean_conc), conc_corr))

biochar_percent_total <- merge(biochar_means_LOQ, biochar_summary, all = TRUE) %>% 
  mutate(perc_comp = ifelse(LOQ_corr == FALSE, mean_conc/sum_pollutant*100, NA))

# oil ----
oil_means <- all_data %>% 
  filter(type == "oil") %>% 
  group_by(sample_ID,  sample_ID_common, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, TEF, TEF_order, unit, source, yield, phase) %>% 
  summarise(mean_conc = mean(as.numeric(conc_corr)),
            sd_conc = sd(as.numeric(conc_corr)),
            mean_TEQ = mean(as.numeric(conc_corr)*TEF),
            sd_TEQ = sd(as.numeric(conc_corr)*TEF),
            n = n())
write_xlsx(oil_means, "processed_data/oil_means.xlsx")

oil_summary <- oil_means %>% 
  group_by(sample_ID,  sample_ID_common, pollutant_class, unit, 
           temperature, feedstock, source, type, yield, phase) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sd_sum_pollutant = sqrt(sum(sd_conc^2)),
            sum_TEQ = sum(mean_TEQ),
            sd_sum_TEQ = sqrt(sum(sd_TEQ^2)))

# Gas and particle ----
gas_particle <- all_data %>% 
  filter(phase2 == "gas")

gas_particle_summary <- gas_particle %>% 
  group_by(sample_ID,  sample_ID_common, pollutant_class, phase, unit, type, temperature, feedstock, source,
           V_gas_m3, V_gas_m3kg, V_gas_m3kg_propane, V_gas_m3kg_feedstock, yield) %>% 
  summarise(sum_pollutant = sum(conc_blank_corr),
            sum_TEQ = (sum(TEQ)))

gas_particle_percent <- gas_particle_summary %>% 
  pivot_wider(names_from = phase,
              values_from = c(sum_pollutant, sum_TEQ)) %>% 
  mutate(percent_particle = sum_pollutant_particle/(sum_pollutant_particle+sum_pollutant_gas)*100,
         percent_gas = 100-percent_particle)

write_xlsx(gas_particle_percent, "processed_data/gas_particle_percent.xlsx")

gas_particle_total_summary <- gas_particle %>% 
  group_by(sample_ID, phase2, pollutant_class, unit, temperature, feedstock, 
           V_gas_m3, V_gas_m3kg, V_gas_m3kg_propane, V_gas_m3kg_feedstock, yield) %>% 
  summarise(sum_pollutant = sum(conc_blank_corr),
            sum_TEQ = (sum(TEQ)))

write_xlsx(gas_particle_total_summary, "processed_data/gas_particle_total_summary.xlsx")

