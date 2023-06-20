# Library ----
library(readxl)
library(writexl)
library(tidyverse)

# raw data processing ----
biochar_PAH <- read_xlsx("raw_data/Biochar_PAH.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc") %>% 
  write_xlsx("processed_data/biochar_PAH.xlsx")

biochar_dioxin <- read_xlsx("raw_data/Biochar_dioxin.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc") %>% 
  write_xlsx("processed_data/biochar_dioxin.xlsx")

biochar_PCB <- read_xlsx("raw_data/Biochar_PCB.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc") %>% 
  write_xlsx("processed_data/biochar_PCB.xlsx")

oil_PAH <- read_xlsx("raw_data/oil_PAH.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc") %>% 
  write_xlsx("processed_data/oil_PAH.xlsx")

PUF_samples <- read_xlsx("raw_data/PUF_samples.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc") %>% 
  write_xlsx("processed_data/PUF_samples.xlsx")

PUF2 <- read_xlsx("raw_data/PUF2.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_wider(names_from = "sample_ID",
              values_from = "conc") 

write_xlsx(PUF2, "processed_data/PUF2.xlsx")

PUF_merge_blank <- read_xlsx("raw_data/PUF_merge_blank.xlsx")

PUF_merge <- left_join(PUF_merge_blank, PUF2, all = TRUE) %>% 
  drop_na(conc)

write_xlsx(PUF_merge, "processed_data/PUF_merge.xlsx")

XAD_samples <- read_xlsx("raw_data/XAD_samples.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc")

write_xlsx(XAD_samples, "processed_data/XAD_samples.xlsx")

XAD_blanks <- read_xlsx("raw_data/XAD_blanks.xlsx")

XAD_merge <- left_join(XAD_samples, XAD_blanks, all = TRUE) %>% 
  drop_na(conc)

write_xlsx(XAD_merge, "processed_data/XAD_merge.xlsx")

GFF_samples <- read_xlsx("raw_data/GFF_samples.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc") %>% 
  drop_na(conc)

write_xlsx(GFF_samples, "processed_data/GFF_samples.xlsx")

GFF_blanks <- read_xlsx("raw_data/GFF_blanks.xlsx")

GFF_merge <- left_join(GFF_samples, GFF_blanks, all = TRUE) 

write_xlsx(GFF_merge, "processed_data/GFF_merge.xlsx")

emission_per_weight <- read_xlsx("raw_data/emission_per_weight.xlsx") %>% 
  mutate_all(as.character) %>% 
  pivot_longer(-Samplename,
               names_to = "pollutant",
               values_to = "conc") %>% 
  write_xlsx("processed_data/emission_per_weight.xlsx")
