# Library ----
library(readxl)
library(writexl)
library(tidyverse)
library(ggforce)
library(ggh4x)
library(latex2exp)
library(matrixStats)
library(RColorBrewer)

# Data sets ----
# see data_wrangling.R for data sets

# merge data ---
BO <- merge(biochar_summary, oil_summary, all = TRUE)

EF_PAH_summary <- gas_particle_summary %>%
  filter(pollutant_class == "PAH") %>% 
  mutate(emission_conc = sum_pollutant/V_gas_m3,
         EF_mgkg = emission_conc*V_gas_m3kg_feedstock/1000000)

GP <- EF_PAH_summary %>% 
  group_by(sample_ID,  sample_ID_common, pollutant_class, phase, temperature, feedstock, source, type) %>% 
  summarise(unit = "mg/kg",
            sum_pollutant = EF_mgkg)

BOGP <- merge(BO, GP, all = TRUE) %>% 
  filter(pollutant_class == "PAH") %>% 
  select(-yield_biochar)

BOGP_yield <- merge(BOGP, yield_and_gas, by = "sample_ID_common", all = TRUE) %>% 
  drop_na(sample_ID) %>% 
  select(-sum_TEQ) %>% 
  pivot_wider(names_from = phase,
              values_from = sum_pollutant) %>% 
  mutate(bc_yield = biochar*yield_biochar,
         oil_yield = oil*yield_oil,
         gas_yield = gas*yield_gas,
         particle_yield = particle*yield_gas) %>% 
  select(sample_ID_common, pollutant_class, temperature, feedstock, source, type,
         bc_yield, oil_yield, gas_yield, particle_yield) %>% 
  mutate(type2 = case_when(
    type == "GFF" ~ "particle",
    type == "PUF" ~ "gas",
    type == "XAD"~ "gas",
    TRUE ~ type
  )) %>% 
  select(-type) %>% 
  filter(feedstock %in% c("CWC", "DSS-1"))

BOGP_biochar <- BOGP_yield %>% 
  drop_na(bc_yield) %>% 
  select(-c("oil_yield", "gas_yield", "particle_yield")) %>% 
  write_xlsx("processed_data/BOGP/BOGP_biochar.xlsx")

BOGP_oil <- BOGP_yield %>% 
  drop_na(oil_yield) %>% 
  select(-c("bc_yield", "gas_yield", "particle_yield")) %>% 
  group_by(sample_ID_common, pollutant_class, temperature, feedstock, source) %>% 
  summarise(oil_yield = sum(oil_yield)) %>% 
  write_xlsx("processed_data/BOGP/BOGP_oil.xlsx")

BOGP_gas <- BOGP_yield %>% 
  drop_na(gas_yield) %>% 
  select(-c("oil_yield", "bc_yield", "particle_yield")) %>% 
  write_xlsx("processed_data/BOGP/BOGP_gas.xlsx")

BOGP_particle <- BOGP_yield %>% 
  drop_na(particle_yield) %>% 
  select(-c("oil_yield", "gas_yield", "bc_yield")) %>% 
  write_xlsx("processed_data/BOGP/BOGP_particle.xlsx")

BOGP_pivot <- left_join(BOGP_biochar, BOGP_oil, by = c("sample_ID_common", "temperature", "feedstock"))
BOGP_pivot <- merge(BOGP_pivot, BOGP_gas, by = c("sample_ID_common", "temperature", "feedstock"), all = TRUE)
BOGP_pivot <- merge(BOGP_pivot, BOGP_particle, by = c("sample_ID_common", "temperature", "feedstock"), all = TRUE) %>% 
  select(sample_ID_common, temperature, feedstock, bc_yield,oil_yield,gas_yield,particle_yield)

BOGP_total_PAH <- BOGP_pivot %>% 
  mutate(sum_PAH = bc_yield+oil_yield+gas_yield+particle_yield)

# EBC limits ----
EBC <- biochar_summary %>% 
  filter(pollutant_class == "PAH",
         sum_pollutant < 12)

cum_feedstock_PAH <- biochar_summary %>% 
  filter(pollutant_class == "PAH") %>% 
  group_by() %>% 
  summarise(mean = mean(sum_pollutant),
            total = sum(sum_pollutant),
            median = median(sum_pollutant))