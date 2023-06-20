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

# merge ----
data1 <- merge(feedstock, biochar, all = TRUE)
data2 <- merge(data1, oil, all = TRUE)
data3 <- merge(data2, PUF, all = TRUE)
data4 <- merge(data3, XAD, all = TRUE)
data5 <- merge(data4, GFF, all = TRUE)
data6 <- merge(data5, TEF, all.x = TRUE)
all_data <- merge(data6, yield_and_gas, by = "sample_ID_common", all = TRUE) %>% 
  mutate(TEQ = TEF*conc_blank_corr)

#feedstock ----
feedstock <- all_data %>% 
  filter(type == "feedstock")

feedstock_means_RE <- feedstock %>% 
  group_by(sample_ID, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, carcinogenic, LOQ_corr, TEF, TEF_order, unit, source) %>% 
  summarise(mean_conc = mean(as.numeric(conc_LOQ_value)),
            sd_conc = sd(as.numeric(conc_LOQ_value)),
            n = n()) %>% 
  mutate(TEQ = mean_conc*TEF,
         sd_TEQ = sd_conc*TEF)

feedstock_summary_RE <- feedstock_means_RE %>% 
  group_by(sample_ID, pollutant_class, unit, temperature, feedstock, source, type) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sum_TEQ = sum(TEQ))

# biochar ----
biochar <- all_data %>% 
  filter(type == "biochar")

biochar_means_RE <- biochar %>% 
  group_by(sample_ID, pollutant, pollutant_class, feedstock, type, temperature,
           abbreviation, LOQ_corr, TEF, unit, source, yield_biochar) %>% 
  summarise(mean_conc = mean(as.numeric(conc_LOQ_value)),
            sd_conc = sd(as.numeric(conc_LOQ_value)),
            n = n()) %>% 
  mutate(TEQ = mean_conc*TEF,
         sd_TEQ = sd_conc*TEF)

biochar_summary_RE <- biochar_means_RE %>% 
  group_by(sample_ID, pollutant_class, unit, temperature, feedstock, source, type, yield_biochar) %>% 
  summarise(sum_pollutant = sum(as.numeric(mean_conc)),
            sum_TEQ = sum(TEQ))

# Removal efficiency ----
RE_summary <- merge(feedstock_summary_RE, biochar_summary_RE, 
                    c("feedstock", "pollutant_class", "unit", "source")) %>% 
  filter(pollutant_class != "PAH") %>% 
  mutate(RE = 100-(sum_pollutant.y*yield_biochar/sum_pollutant.x),
         RE_TEQ = 100-(sum_TEQ.y*yield_biochar/sum_TEQ.x)) %>% 
  select(feedstock, pollutant_class, temperature.y, RE, RE_TEQ) %>% 
  pivot_wider(names_from = pollutant_class,
              values_from = c(RE, RE_TEQ))

write_xlsx(RE_summary, "processed_data/RE_summary.xlsx")
